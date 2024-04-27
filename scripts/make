# Define variables
DOTFILES_ROOT := $(shell pwd)
CONFIG_PATH := $(HOME)/.config
BIN_PATH := $(HOME)/bin

# List of files to symlink
DOTFILES := \
    .spacemacs.d.symlink \
    .zshenv.symlink \
    .zshrc.symlink \
    .tmux.conf.symlink \
    .mbsyncrc.symlink \
    .latexmkrc.symlink \
    .p10k.zsh.symlink

CONFIG_FILES := $(addprefix .config/,$(wildcard .config/*))
BIN_FILES := $(wildcard bin/*)
RANGER_PY := ranger/ranger.py

# Makefile targets
.PHONY: setup-dotfiles setup-config setup-bin setup-submodules

all: setup-dotfiles setup-config setup-bin setup-submodules

setup-dotfiles:
    @echo "Setting up dotfiles..."
    $(foreach dotfile,$(DOTFILES), ln -sf $(DOTFILES_ROOT)/$(dotfile) $(HOME)/$(dotfile);)

setup-config:
    @echo "Setting up config directory..."
    mkdir -p $(CONFIG_PATH)
    ln -sf $(CONFIG_FILES) $(HOME)/.config

setup-bin:
    @echo "Setting up bin directory..."
    mkdir -p $(BIN_PATH)
    ln -sf $(BIN_FILES) $(BIN_PATH)
    ln -sf $(DOTFILES_ROOT)/$(RANGER_PY) $(BIN_PATH)/ranger

setup-submodules:
    @echo "Setting up submodules..."
    cd $(DOTFILES_ROOT) && git submodule update --init --recursive
    ln -sf $(DOTFILES_ROOT)/.pyenv $(HOME)/.pyenv
    cd $(DOTFILES_ROOT)/.pyenv && git submodule update --init --recursive
    ln -sf $(DOTFILES_ROOT)/ohmyzsh $(HOME)/.oh-my-zsh
    cd $(DOTFILES_ROOT)/ohmyzsh && git submodule update --init --recursive
    git clone https://github.com/syl20bnr/spacemacs $(HOME)/.emacs.d
    ln -sf $(DOTFILES_ROOT)/.spacemacs.d.symlink/snippets/* $(HOME)/.emacs.d/private/snippets
    cd $(HOME)/.emacs.d && git checkout develop && git submodule update --init --recursive
