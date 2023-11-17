{ config, pkgs, ... }:

let
  osSpecificPackages = if pkgs.stdenv.isLinux then
    with pkgs; [
      pkgs.thunderbird
      pkgs.strace
      pkgs.ltrace
      pkgs.firefox
      pkgs.xclip
      pkgs.wl-clipboard
      pkgs.gnome.gnome-boxes
      pkgs.gnome.gnome-calendar
      pkgs.gnome.gnome-weather
      pkgs.gnome.gnome-clocks
    ]
  else
    [ ];
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username =
    if pkgs.stdenv.isLinux then "chasinglogic" else "mathewrobinson";
  home.homeDirectory = if pkgs.stdenv.isLinux then
    "/home/chasinglogic"
  else
    "/Users/mathewrobinson";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs;
    [
      ansible
      aspell
      aspellDicts.en
      atuin
      clojure
      clojure-lsp
      cmake
      curl
      docker
      emacs
      gcc
      git
      gnumake
      graalvm-ce
      htop
      hugo
      indent
      libtool
      mktemp
      neil
      nerdfonts
      ninja
      nixops_unstable
      nodejs
      pandoc
      podman
      ripgrep
      ruby
      rustup
      tmux
      tree-sitter
      twine
      yamllint
      pulumi

      (python3.withPackages (ps: with ps; [ requests python-lsp-server ]))
    ] ++ osSpecificPackages;

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # You can also manage environment variables but you will have to manually
  # source
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/mathewrobinson/etc/profile.d/hm-session-vars.sh
  #
  # if you don't want to manage your shell through Home Manager.
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
