# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

for _texlive_bin in "$HOME"/.local/texlive/*/bin/x86_64-linux(N/); do
    PATH="$_texlive_bin:$PATH"
done
unset _texlive_bin

export GIT_INTERNAL_GETTEXT_TEST_FALLBACKS=1

export LANGUAGE="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
