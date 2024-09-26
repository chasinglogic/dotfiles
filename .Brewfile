# vi: ft=ruby

brew "siderolabs/tap/talosctl"
brew "kubectl"
brew "awscli"
brew "fd"
brew "ripgrep"
brew "fzf"
brew "mise"
brew "neovim"
brew "helm"
brew "helmfile"
brew "just"
brew "shfmt"
brew "shellcheck"
brew "golang-migrate"
brew "golangci-lint"
brew "carapace"
brew "duckdb"

if OS.mac?
  tap "mongodb/brew"

  brew "mongodb-community"
  brew "bash"
  brew "grep"
  brew "gnu-tar"
  brew "coreutils"
  brew "mysql@8.4"
  brew "postgresql"

  if not File.exist?("/Applications/Google Chrome.app")
    cask "google-chrome"
  end

  cask "firefox"
  cask "rectangle"
  cask "wezterm"
  cask "notion"
  cask "docker"
  cask "mongodb-compass"
end
