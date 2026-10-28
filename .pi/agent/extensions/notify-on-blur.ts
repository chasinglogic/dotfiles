import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import { execFile } from "node:child_process";
import { promisify } from "node:util";

const run = promisify(execFile);

const FOCUS_IN = "\x1b[I";
const FOCUS_OUT = "\x1b[O";

function oscNotify(title: string, body: string): void {
	const clean = (value: string) => value.replace(/[\x00-\x1f\x7f;]/g, " ").trim();
	process.stdout.write(`\x1b]777;notify;${clean(title)};${clean(body)}\x07`);
}

export default function (pi: ExtensionAPI) {
	let focused = true;
	let listening = false;
	let tail = "";
	let notificationId: number | undefined;
	let notificationTimer: ReturnType<typeof setTimeout> | undefined;
	let generation = 0;

	const close = (id: number) => {
		void run("gdbus", ["call", "--session", "--dest", "org.freedesktop.Notifications",
			"--object-path", "/org/freedesktop/Notifications",
			"--method", "org.freedesktop.Notifications.CloseNotification", String(id)]).catch(() => {});
	};
	const dismiss = () => {
		generation++;
		if (notificationTimer) clearTimeout(notificationTimer);
		notificationTimer = undefined;
		if (notificationId !== undefined) close(notificationId);
		notificationId = undefined;
	};

	const onData = (data: Buffer | string) => {
		const chunk = tail + data.toString("utf8");
		for (const match of chunk.matchAll(/\x1b\[(?:I|O)/g)) {
			focused = match[0] === FOCUS_IN;
			if (focused) dismiss();
		}
		tail = chunk.slice(-2);
	};

	pi.on("session_start", async (_event, ctx) => {
		if (ctx.mode !== "tui" || listening) return;
		listening = true;
		process.stdout.write("\x1b[?1004h");
		process.stdin.on("data", onData);
	});

	pi.on("session_shutdown", async () => {
		if (!listening) return;
		listening = false;
		dismiss();
		process.stdin.off("data", onData);
		process.stdout.write("\x1b[?1004l");
	});

	pi.on("agent_settled", async () => {
		if (focused || !listening) return;
		if (process.platform !== "linux") {
			oscNotify("Pi", "Agent turn finished");
			return;
		}

		// OSC 777 cannot retract notifications; Linux notification IDs can.
		dismiss();
		const current = generation;
		try {
			const { stdout } = await run("notify-send", ["-p", "-a", "Pi", "Pi", "Agent turn finished"]);
			const id = Number(stdout.trim());
			if (!Number.isSafeInteger(id) || id <= 0) throw new Error("Invalid notification ID");
			if (current !== generation) close(id);
			else {
				notificationId = id;
				notificationTimer = setTimeout(dismiss, 120_000);
			}
		} catch {
			// shortcut: OSC fallback cannot be dismissed, revisit if a terminal close protocol is available.
			if (current === generation) oscNotify("Pi", "Agent turn finished");
		}
	});
}
