#!/usr/bin/env bash
selected=$(cat ~/.config/tmux/.cht-command ~/.config/tmux/.cht-languages | fzf)
if [[ -z $selected ]]; then
    exit 0
fi
read -p "query: " query

if grep -qs "$selected" ~/.config/tmux/.cht-languages; then
    query=$(echo "$query" | tr ' ' '+')
    tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl -s cht.sh/$selected/$query | less -R"
else
    tmux neww bash -c "curl -s cht.sh/$selected~$query | less -R"
fi
