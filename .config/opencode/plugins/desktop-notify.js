import { execFile } from "node:child_process"
import { promisify } from "node:util"

const execFileAsync = promisify(execFile)

const MIN_NOTIFY_GAP_MS = 3000

function nowMs() {
  return Date.now()
}

async function getRepoRoot() {
  try {
    const { stdout } = await execFileAsync("git", ["rev-parse", "--show-toplevel"])
    return stdout.trim()
  } catch {
    return process.cwd()
  }
}

async function sendDesktopNotification(title, body) {
  const platform = process.platform

  if (platform === "darwin") {
    await execFileAsync("osascript", [
      "-e",
      `display notification ${JSON.stringify(body)} with title ${JSON.stringify(title)}`,
    ])
    return
  }

  if (platform === "linux") {
    await execFileAsync("notify-send", [title, body, "--app-name=opencode"])
    return
  }

  process.stdout.write("\u0007")
}

export const DesktopNotifyPlugin = async ({ client }) => {
  let lastNotifiedAt = 0
  const repoRoot = await getRepoRoot()

  async function notify(title, body) {
    const now = nowMs()
    if (now - lastNotifiedAt < MIN_NOTIFY_GAP_MS) return

    lastNotifiedAt = now
    try {
      await sendDesktopNotification(title, `${repoRoot}\n${body}`)
    } catch (error) {
      await client.app.log({
        body: {
          service: "desktop-notify",
          level: "warn",
          message: "Failed to send desktop notification",
          extra: {
            error: String(error),
          },
        },
      })
    }
  }

  return {
    event: async ({ event }) => {
      if (event.type === "session.idle") {
        await notify("OpenCode", "Agent finished and is waiting.")
        return
      }

      if (event.type === "permission.asked") {
        await notify("OpenCode", "Approval needed for a requested action.")
        return
      }

      if (event.type === "tui.prompt.append") {
        await notify("OpenCode", "Input requested in the current session.")
      }
    },
  }
}
