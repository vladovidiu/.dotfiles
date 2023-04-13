if status is-interactive
    # Commands to run in interactive sessions can go here
    /usr/bin/rbenv init - fish | source
end

set -gx fish_greeting # disable fish greeting

set -U EDITOR nvim # 'neovim/neovim' text editor 

set -Ux MAKEFLAGS "-J"(nproc)

set -U FZF_CTRL_R_OPTS "--border-label=' History ' --prompt=' '"
set -U FZF_DEFAULT_COMMAND "fd -H -E '.git'"
set -U FZF_DEFAULT_OPTS "--reverse --no-info --prompt=' ' --pointer='' --marker=' ' --ansi --color gutter:-1,bg+:-1,header:4,separator:0,info:0,label:4,border:4,prompt:7,pointer:5,query:7,prompt:7"
set -U FZF_TMUX_OPTS "-p --no-info --ansi --color gutter:-1,bg+:-1,header:4,separator:0,info:0,label:4,border:4,prompt:7,pointer:5,query:7,prompt:7"

set -U GOPATH (go env GOPATH) # https://golang.google.cn/

fish_add_path $GOPATH/bin

alias nvim-lazy="NVIM_APPNAME=LazyVim nvim"
alias nvim-chad="NVIM_APPNAME=NvChad nvim"
alias nvim-astro="NVIM_APPNAME=AstroNvim nvim"

starship init fish | source
zoxide init fish | source
