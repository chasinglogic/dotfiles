import { execFile } from "node:child_process"
import path from "node:path"
import { promisify } from "node:util"

const execFileAsync = promisify(execFile)

const MIN_NOTIFY_GAP_MS = 3000
const ESC = "\u001b"
const BEL = "\u0007"

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

function wrapForTmux(sequence) {
  if (!process.env.TMUX) return sequence

  return `${ESC}Ptmux;${sequence.split(ESC).join(`${ESC}${ESC}`)}${ESC}\\`
}

async function sendDesktopNotification(title, body) {
  const message = `${title}: ${body}`.replace(/[\r\n]+/g, " ")
  const sequence = `${ESC}]9;${message}${BEL}`
  process.stdout.write(wrapForTmux(sequence))
}

export const DesktopNotifyPlugin = async ({ client }) => {
  let lastNotifiedAt = 0
  const repoName = path.basename(await getRepoRoot())

  async function notify(title, body) {
    const now = nowMs()
    if (now - lastNotifiedAt < MIN_NOTIFY_GAP_MS) return

    lastNotifiedAt = now
    try {
      await sendDesktopNotification(title, `${repoName}\n${body}`)
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
