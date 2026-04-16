#!/usr/bin/env zsh

set -e

DOTFILES_ROOT="$(cd "$(dirname "$0")" && pwd)"

# setup spacemacs
if [[ ! -d ~/.emacs.d ]]; then
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

# setup submodules
git -C "$DOTFILES_ROOT" submodule init
git -C "$DOTFILES_ROOT" submodule update

# symlink bin folder
mkdir -p ~/bin
ln -sf "$DOTFILES_ROOT/bin/"* ~/bin/

# stow packages
cd "$DOTFILES_ROOT"
stow zshenv
stow zprofile
stow zshrc
stow p10k-zsh
stow tmux
stow wezterm
stow config
stow spacemacs
