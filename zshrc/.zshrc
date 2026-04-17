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

plugins=(extract git wd rsync zsh-syntax-highlighting fzf uv zoxide)

# set full color terminal
if [[ -n "$INSIDE_EMACS" ]]; then
  # export TERM=
  DISABLE_LS_COLORS="true"
else
  export TERM="xterm-256color"
fi

source $ZSH/oh-my-zsh.sh

eval "$(pyenv init - zsh)"

# set default editor
export EDITOR=nvim
export VISUAL=nvim
export ALTERNATE_EDITOR=""

# smart history search after typing and pressing up-down keys
autoload -Uz up-line-or-beginning-search
autoload -Uz down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '\eOA' up-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
bindkey '\eOB' down-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search

alias zs="source ~/.zshrc"

# bind C-j and C-k to up-down in history
bindkey '^k' up-history
bindkey '^j' down-history

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# show human friendly numbers and colors
alias df='df -h'
alias du='du -h -d 2'

alias grep='grep --color=auto'

alias lsg='ls | grep'
alias lt='eza --sort=size --long'
alias countf='find . -type f | wc -l'

# does that ring a bell?
alias bell="echo -n $'\x7'"

alias getpass="openssl rand -base64 20"
alias ping='ping -c 5'
alias www='python3 -m http.server 8000'

alias fd='fd --exclude "$HOME/share"'

alias update='sudo apt-get update && sudo apt-get upgrade -y'
alias cpr='rsync --progress -avz --ignore-existing'
alias tree='tree -C --dirsfirst'
alias weather='function _weather() { curl "wttr.in/$1?m";}; _weather'
alias encrypt='function _encrypt() { openssl enc -aes-256-cbc -salt -in $1 -out $2; }; _encrypt'
alias decrypt='function _decrypt() { openssl enc -d -aes-256-cbc -in $1 -out $2; }; _decrypt'
alias avg_fs='find ./ -type f -ls | awk "{sum += \$7; n++;} END {print sum/n;}"'

# create new tmux session
alias tn='tmux new -s'

# attach to tmux session
alias ta="tmux attach -t"

# list all ongoing tmux sessions
alias tl='tmux list-sessions'


export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=00:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.wim=01;31:*.swm=01;31:*.dwm=01;31:*.esd=01;31:*.avif=01;35:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:*~=00;90:*#=00;90:*.bak=00;90:*.crdownload=00;90:*.dpkg-dist=00;90:*.dpkg-new=00;90:*.dpkg-old=00;90:*.dpkg-tmp=00;90:*.old=00;90:*.orig=00;90:*.part=00;90:*.rej=00;90:*.rpmnew=00;90:*.rpmorig=00;90:*.rpmsave=00;90:*.swp=00;90:*.tmp=00;90:*.ucf-dist=00;90:*.ucf-new=00;90:*.ucf-old=00;90:'
[[ ! -f ~/.init_startup ]] || source ~/.init_startup

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

# Use fd to generate the list for directory completion
_fzf_compgen_dir() {
  fd --type=d --hidden --exclude .git . "$1"
}

# Advanced customization of fzf options via _fzf_comprun function
# - The first argument to the function is the name of the command.
# - You should make sure to pass the rest of the arguments to fzf.
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

    locate --existing "$1" | fzf --preview 'cat {}' | xargs xdg-open
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

function y() {
	  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	  yazi "$@" --cwd-file="$tmp"
	  IFS= read -r -d '' cwd < "$tmp"
	  [ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd"
	  rm -f -- "$tmp"
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
