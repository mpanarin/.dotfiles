# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/share/fzf* ]]; then
  export PATH="$PATH:/usr/share/fzf"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/share/fzf/completion.bash" 2> /dev/null

# Key bindings
# ------------
source "/usr/share/fzf/key-bindings.bash"

