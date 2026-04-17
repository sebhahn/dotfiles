#!/usr/bin/env zsh

set -e

DOTFILES_ROOT="$(cd "$(dirname "$0")" && pwd)"

# setup spacemacs
if [[ ! -d ~/.emacs.d ]]; then
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
fi

# setup ranger
if [[ ! -d ~/.local/lib/ranger ]]; then
    git clone https://github.com/ranger/ranger.git ~/.local/lib/ranger
fi

# setup submodules
git -C "$DOTFILES_ROOT" submodule init
git -C "$DOTFILES_ROOT" submodule update

# symlink bin folder
mkdir -p ~/bin
ln -sf "$DOTFILES_ROOT/bin/"* ~/bin/

# install nvim and nvm via ansible
ansible-playbook "$DOTFILES_ROOT/playbooks/install_nvm.yml"
ansible-playbook "$DOTFILES_ROOT/playbooks/install_nvim.yml"

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
stow misc
