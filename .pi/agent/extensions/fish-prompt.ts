import { isAbsolute, relative, resolve, sep } from "node:path";
import {
	CustomEditor,
	type ExtensionAPI,
	type ExtensionContext,
	type KeybindingsManager,
} from "@earendil-works/pi-coding-agent";
import type { Usage } from "@earendil-works/pi-ai";
import { truncateToWidth, visibleWidth, type EditorTheme, type TUI, type TuiMouseEvent } from "@earendil-works/pi-tui";

const CYAN = "#8BE9FD";
const ORANGE = "#FFB86C";
const WHITE = "#FFFFFF";
const RESET = "\x1b[0m";

const color = (hex: string, text: string) => {
	const parts = hex.match(/[\da-f]{2}/gi);
	if (!parts || parts.length !== 3) return text;
	const [r, g, b] = parts.map((part) => Number.parseInt(part, 16));
	return `\x1b[38;2;${r};${g};${b}m${text}${RESET}`;
};

const formatTokens = (count: number) => {
	if (count < 1_000) return `${count}`;
	if (count < 10_000) return `${(count / 1_000).toFixed(1)}k`;
	if (count < 1_000_000) return `${Math.round(count / 1_000)}k`;
	return count < 10_000_000 ? `${(count / 1_000_000).toFixed(1)}M` : `${Math.round(count / 1_000_000)}M`;
};

const formatCwd = (cwd: string) => {
	const home = process.env.HOME || process.env.USERPROFILE;
	if (!home) return cwd;
	const rel = relative(resolve(home), resolve(cwd));
	return rel === "" || (rel !== ".." && !rel.startsWith(`..${sep}`) && !isAbsolute(rel)) ? `~${rel ? `${sep}${rel}` : ""}` : cwd;
};

const sanitize = (text: string) => text.replace(/[\r\n\t]/g, " ").replace(/ +/g, " ").trim();

const addUsage = (totals: Usage, usage: Usage) => {
	totals.input += usage.input;
	totals.output += usage.output;
	totals.cacheRead += usage.cacheRead;
	totals.cacheWrite += usage.cacheWrite;
	totals.cost.total += usage.cost.total;
};

let gitGlyph = "λ";

class PromptEditor extends CustomEditor {
	constructor(tui: TUI, theme: EditorTheme, keybindings: KeybindingsManager) {
		super(tui, theme, keybindings);
	}

	render(width: number): string[] {
		if (width <= 2) return super.render(width);
		const prefix = `${color(ORANGE, gitGlyph)} `;
		return super.render(width - 2).map((line, index) => `${index === 1 ? prefix : "  "}${line}`);
	}

	handleMouse(event: TuiMouseEvent) {
		return super.handleMouse({ ...event, x: Math.max(0, event.x - 2), width: Math.max(1, event.width - 2) });
	}
}

export default function (pi: ExtensionAPI) {
	let activeTui: TUI | undefined;

	const refreshGitGlyph = async (ctx: ExtensionContext) => {
		const result = await pi.exec("git", ["diff", "--quiet", "--"], { cwd: ctx.cwd }).catch(() => undefined);
		gitGlyph = result?.code === 1 ? "Δ" : "λ";
		activeTui?.requestRender();
	};

	pi.on("agent_start", (_event, ctx) => void refreshGitGlyph(ctx));
	pi.on("agent_end", (_event, ctx) => void refreshGitGlyph(ctx));

	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;

		void refreshGitGlyph(ctx);
		ctx.ui.setFooter((tui, theme, data) => {
			activeTui = tui;
			const unsubscribe = data.onBranchChange(() => tui.requestRender());
			return {
				dispose: unsubscribe,
				invalidate() {},
				render(width: number) {
					const totals: Usage = { input: 0, output: 0, cacheRead: 0, cacheWrite: 0, cost: { total: 0 } } as Usage;
					let latestCacheHit: number | undefined;
					for (const entry of ctx.sessionManager.getEntries()) {
						let usage: Usage | undefined;
						if (entry.type === "usage") usage = entry.usage;
						else if (entry.type === "message" && entry.message.role === "assistant") {
							usage = entry.message.usage;
							const latestPromptTokens = usage.input + usage.cacheRead + usage.cacheWrite;
							latestCacheHit = latestPromptTokens ? (usage.cacheRead / latestPromptTokens) * 100 : undefined;
						} else if (entry.type === "message" && entry.message.role === "toolResult") usage = entry.message.usage;
						else if ((entry.type === "branch_summary" || entry.type === "compaction") && entry.usage) usage = entry.usage;
						if (usage) addUsage(totals, usage);
					}

					const branch = data.getGitBranch();
					const sessionName = ctx.sessionManager.getSessionName();
					const cwd = color(CYAN, formatCwd(ctx.sessionManager.getCwd()));
					const branchText = branch ? ` ${color(WHITE, `(${branch})`)}` : "";
					const sessionText = sessionName ? theme.fg("dim", ` • ${sessionName}`) : "";
					const pwdLine = truncateToWidth(`${cwd}${branchText}${sessionText}`, width, theme.fg("dim", "..."));

					const stats = [];
					if (totals.input) stats.push(`↑${formatTokens(totals.input)}`);
					if (totals.output) stats.push(`↓${formatTokens(totals.output)}`);
					if (totals.cacheRead) stats.push(`R${formatTokens(totals.cacheRead)}`);
					if (totals.cacheWrite) stats.push(`W${formatTokens(totals.cacheWrite)}`);
					if ((totals.cacheRead || totals.cacheWrite) && latestCacheHit !== undefined) stats.push(`CH${latestCacheHit.toFixed(1)}%`);
					if (totals.cost.total) stats.push(`$${totals.cost.total.toFixed(3)}`);
					const context = ctx.getContextUsage();
					const contextWindow = context?.contextWindow ?? ctx.model?.contextWindow ?? 0;
					const contextPercent = context?.percent === null ? "?" : (context?.percent ?? 0).toFixed(1);
					stats.push(`${contextPercent}%/${formatTokens(contextWindow)}`);

					let left = stats.join(" ");
					const model = ctx.model?.id || "no-model";
					const thinking = ctx.model?.reasoning ? ` • ${ctx.thinkingLevel || "off"}` : "";
					const right = `${model}${thinking}`;
					if (visibleWidth(left) + 2 + visibleWidth(right) > width) left = truncateToWidth(left, width, "...");
					const pad = " ".repeat(Math.max(1, width - visibleWidth(left) - visibleWidth(right)));
					const lines = [pwdLine, theme.fg("dim", `${left}${pad}${right}`)];

					const statuses = Array.from(data.getExtensionStatuses().values()).map((text) => sanitize(String(text))).join(" ");
					if (statuses) lines.push(truncateToWidth(statuses, width, theme.fg("dim", "...")));
					return lines;
				},
			};
		});
		ctx.ui.setEditorComponent((tui, theme, keybindings) => {
			activeTui = tui;
			return new PromptEditor(tui, theme, keybindings);
		});
	});
}
