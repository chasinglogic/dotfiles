##############
# ENV setup. #
##############

export SHELLNAME=$(basename $SHELL)

source_if_exists $HOME/.${SHELLNAME}rc.local
# Load the profile if it's not loaded yet. My .profile is idempotent so it can
# be sourced multiple times safely without causing weird environments.
source_if_exists $HOME/.profile
source_if_exists $HOME/.fzf.${SHELLNAME}
source_if_exists $HOME/.local/bin/virtualenvwrapper.sh

### Virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON="$(which python3)"
source_if_exists /usr/local/bin/virtualenvwrapper.sh
source_if_exists /usr/share/virtualenvwrapper/virtualenvwrapper.sh
source_if_exists $HOME/.local/bin/virtualenvwrapper.sh

source_if_exists $HOME/.prompt.$SHELLNAME
