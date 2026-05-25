# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH=$HOME/.ohmyzsh
ZSH_THEME="powerlevel10k/powerlevel10k"
DEFAULT_USER="shahn"
DISABLE_AUTO_UPDATE="true"
HIST_STAMPS="yyyy-mm-dd"

plugins=(git zsh-syntax-highlighting fzf uv zoxide)

if [[ -n "$INSIDE_EMACS" ]]; then
  DISABLE_LS_COLORS="true"
else
  export TERM="xterm-256color"
fi

source $ZSH/oh-my-zsh.sh

eval "$(pyenv init - zsh)"

# smart history search after typing and pressing up-down keys
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\eOA' up-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\eOB' down-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search

# bind C-j and C-k to up-down in history
bindkey '^k' up-history
bindkey '^j' down-history

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

alias zs="source ~/.zshrc"
alias df='df -h'
alias du='du -h -d 2'
alias grep='grep --color=auto'
alias lsg='ls | grep'
alias lt='eza --sort=size --long'
alias countf='find . -type f | wc -l'
alias bell="echo -n $'\x7'"
alias getpass="openssl rand -base64 20"
alias ping='ping -c 5'
alias www='python3 -m http.server 8000'
alias fd='fd --exclude "$HOME/shares"'
alias update='sudo apt-get update && sudo apt-get upgrade -y'
alias cpr='rsync --progress -avz --ignore-existing'
alias tree='tree -C --dirsfirst'
alias weather='function _weather() { curl "wttr.in/$1?m";}; _weather'
alias encrypt='function _encrypt() { openssl enc -aes-256-cbc -salt -in $1 -out $2; }; _encrypt'
alias decrypt='function _decrypt() { openssl enc -d -aes-256-cbc -in $1 -out $2; }; _decrypt'
alias avg_fs='find ./ -type f -ls | awk "{sum += \$7; n++;} END {print sum/n;}"'
alias tn='tmux new -s'
alias ta="tmux attach -t"
alias tl='tmux list-sessions'

export FZF_DEFAULT_COMMAND="fd --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_ALT_C_COMMAND="fd --type=d --hidden --strip-cwd-prefix --exclude .git"
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"

# Use fd (https://github.com/sharkdp/fd) for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
_fzf_compgen_path() {
  fd --hidden --exclude .git . "$1"
}

_fzf_compgen_dir() {
  fd --type=d --hidden --exclude .git . "$1"
}

_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    cd)           fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
    export|unset) fzf --preview "eval 'echo $'{}"         "$@" ;;
    ssh)          fzf --preview 'dig {}'                   "$@" ;;
    *)            fzf --preview "bat -n --color=always --line-range :500 {}" "$@" ;;
  esac
}

locatefzf() {
    if [ -z "$1" ]; then
        echo "Usage: locatefzf <search-term>"
        return 1
    fi

    locate --existing "$1" | fzf --preview 'bat {}' | xargs xdg-open
}

fg="#CBE0F0"
bg="#011628"
bg_highlight="#143652"
purple="#B388FF"
blue="#06BCE4"
cyan="#2CF9ED"

export FZF_DEFAULT_OPTS="--color=fg:${fg},bg:${bg},hl:${purple},fg+:${fg},bg+:${bg_highlight},hl+:${purple},info:${blue},prompt:${cyan},pointer:${cyan},marker:${cyan},spinner:${cyan},header:${cyan}"

alias ls="eza --color=always --long --git --icons=always"
alias rgf='rg --files | rg'

tvr() {
  local file
  file=$(tv) && [ -n "$file" ] && ranger --selectfile="$file"
}

export NVM_DIR="$HOME/.nvm"
_load_nvm() {
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}
(( $+commands[node] )) || _load_nvm
nvm() { unfunction nvm node npm npx; _load_nvm; nvm "$@"; }
node() { unfunction nvm node npm npx; _load_nvm; node "$@"; }
npm() { unfunction nvm node npm npx; _load_nvm; npm "$@"; }
npx() { unfunction nvm node npm npx; _load_nvm; npx "$@"; }

muf() {
    local query="${*:-date:2w..}"
    mu find "$query" \
        --fields "d f s l" \
        --sortfield=date \
        --reverse \
        2>/dev/null \
    | awk '{
        split($4,t,":")
        m=(index("JanFebMarAprMayJunJulAugSepOctNovDec",$2)+2)/3
        printf "%s-%02d-%02d %s:%s  ", $5, m, $3, t[1], t[2]
        for(i=6;i<=NF;i++) printf "%s%s",(i>6?" ":""),$i
        print ""
    }' \
    | fzf \
        --no-sort \
        --prompt "mu> " \
        --preview 'mu view {-1}' \
        --preview-window 'right:55%:wrap' \
        --bind 'enter:execute(mu view {-1} | less)'
}

[[ ! -f ~/.init_startup ]] || source ~/.init_startup
