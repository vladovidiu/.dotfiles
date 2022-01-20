{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "vladovidiu";
  home.homeDirectory = "/home/vladovidiu";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  home.packages = with pkgs; [
    # cli tools
    tree
    pstree
    fzf

    # terminal
    starship

    # formatters and linters
    shellcheck
    haskellPackages.nixfmt
    shfmt
    html-tidy
    nodePackages.stylelint
    stylua

    # tools
    graphviz
  ];
}
