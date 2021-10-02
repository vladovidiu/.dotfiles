export ZSH="/home/vladovidiu/.oh-my-zsh"

# ZSH_THEME='wedisagree'
source $HOME/.zplug/init.zsh

zplug 'zplug/zplug', hook-build:'zplug --self-manage'
zplug 'zdharma/fast-syntax-highlighting', depth:1
zplug 'zsh-users/zsh-autosuggestions', depth:1
zplug 'zsh-users/zsh-completions', depth:1

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

#=======================
#       Plugins        #
#=======================
plugins=(
    git
    colored-man-pages
    colorize
    python
    pip
    zsh-autosuggestions
    fast-syntax-highlighting
)

fpath=("$HOME/.zfunctions" $fpath)
fpath+=("$PWD/functions")

source $ZSH/oh-my-zsh.sh
source $HOME/.cargo/env

export GOPATH=$HOME/go
export GOBIN=$HOME/go/bin
export DOOMPATH=$HOME/emacs-configs/doom-emacs/bin
export LUAROCKS_BIN=$HOME/.luarocks/bin
export PATH=$PATH:$GOPATH/bin:/usr/local/go/bin:$HOME/.bin:$HOME/.local/bin:$DOOMPATH:$LUAROCKS_BIN
export VISUAL="emacsclient -c -a emacs"
export EDITOR="emacsclient -c -a emacs"
export SHELL=zsh
export VIMRC=$HOME/.vimrc
export NVIMRC=$HOME/.config/nvim/init.vim
export MAKEFLAGS="-j$(nproc)"
export DOTFILES="$HOME/.dotfiles"
export LANG=en_GB.UTF-8
export LC_ALL=en_GB.UTF-8
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive
export TERM=screen-256color

export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!.git/*"'

zplug load

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

fh() {
    print -z $( ([ -n "$ZSH_NAME" ] && fc -l 1 || history) | fzf +s --tac | sed -r 's/ *[0-9]*\*? *//' | sed -r 's/\\/\\\\/g' )
}

fkill() {
    local pid
    pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

if [ -e /home/vladovidiu/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vladovidiu/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

eval "$(starship init zsh)"

alias la="exa -al --git"
alias cat="bat"
alias git_prune="git fetch -p && git branch -vv | awk '/: gone]/{print $1}' | xargs git branch -d"
alias sudo="doas"

# NNN
export NNN_PLUG='o:fzopen;c:fcd;j:jump;p:preview-tui;t:preview-tabbed;i:imgview;v:vidthumb;d:dragdrop;'
export NNN_FIFO='/tmp/nnn.fifo'
HR="04" DIR="04" EXE="00" REG="00" HARDLINK="00" SYMLINK="06" MISSING="00" ORPHAN="01" FIFO="0F" SOCK="0F" OTHER="02"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"

# Guix
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
GUIX_PROFILE="/home/vladovidiu/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"

GUIX_PROFILE="/home/vladovidiu/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
