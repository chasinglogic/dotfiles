import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

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

	const onData = (data: Buffer | string) => {
		const chunk = tail + data.toString("utf8");
		for (const match of chunk.matchAll(/\x1b\[(?:I|O)/g)) {
			focused = match[0] === FOCUS_IN;
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
		process.stdin.off("data", onData);
		process.stdout.write("\x1b[?1004l");
	});

	pi.on("agent_settled", async () => {
		if (!focused) oscNotify("Pi", "Agent turn finished");
	});
}
