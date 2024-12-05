# A colon-separated list of values controlling how commands are saved on the
# history list.  If the list  of  values  includes ignorespace,  lines  which
# begin  with  a space character are not saved in the history list.  A value of
# ignoredups causes lines matching the previous history entry to not be saved.
# A value of ignoreboth is  shorthand  for  ignorespace  and  ig‐ noredups.  A
# value of erasedups causes all previous lines matching the current line to be
# removed from the history list be‐ fore that line is saved.  Any value not in
# the above list is ignored.  If HISTCONTROL is unset, or does not include a
# valid value,  all  lines  read by the shell parser are saved on the history
# list, subject to the value of HISTIGNORE.  The second and subsequent lines of
# a multi-line compound command are not tested, and are added to the history
# regardless of the  value of HISTCONTROL.
export HISTCONTROL=ignoredups:erasedups

# The number of commands to remember in the command  history  (see
# HISTORY  below).   If  the value is 0, commands are not saved in
# the history list.  Numeric values less than zero result in every
# command  being  saved  on  the history list (there is no limit).
# The shell sets the  default  value  to  500  after  reading  any
# startup files.
export HISTSIZE=100000

# The maximum number of lines contained in the history file.  When
# this variable is assigned a value, the  history  file  is  trun‐
# cated,  if  necessary,  to  contain  no more than that number of
# lines by removing the oldest entries.  The history file is  also
# truncated  to this size after writing it when a shell exits.  If
# the value is 0, the history file  is  truncated  to  zero  size.
# Non-numeric  values  and  numeric  values less than zero inhibit
# truncation.  The shell sets the default value to  the  value  of
# HISTSIZE after reading any startup files.
export HISTFILESIZE=100000

# If the histappend shell option is enabled (see the description of shopt under
# SHELL BUILTIN COMMANDS below), the lines are  appended to the history file,
# otherwise the history file is overwritten.
shopt -s histappend

# Read the contents of $HISTFILE and insert them in to the current
# running session history. this will raise the history counter by the
# amount of lines in $HISTFILE. Note that the line count of $HISTFILE is
# not necessarily $HISTFILESIZE.
_bash_history_sync() {
	builtin history -a
	HISTFILESIZE=$HISTSIZE
	builtin history -c
	builtin history -r
}

# The history() function overrides the builtin history to make sure that
# the history is synchronised before it is displayed. This is necessary
# for the history expansion by number.
history() {
	_bash_history_sync
	builtin history "$@"
}

# If  set,  the pattern ** used in a pathname expansion context will match all
# files and zero or more directories and subdirectories.  If the pattern is
# followed by a /, only directories and subdirectories match.
shopt -s globstar

# If  the  extglob  shell option is enabled using the shopt builtin, several extended pattern matching operators are recognized.  In
# the following description, a pattern-list is a list of one or more patterns separated by a |.  Composite patterns  may  be  formed
# using one or more of the following sub-patterns:
#
#        ?(pattern-list)
#               Matches zero or one occurrence of the given patterns
#        *(pattern-list)
#               Matches zero or more occurrences of the given patterns
#        +(pattern-list)
#               Matches one or more occurrences of the given patterns
#        @(pattern-list)
#               Matches one of the given patterns
#        !(pattern-list)
#               Matches anything except one of the given patterns
#
# Complicated  extended  pattern  matching  against  long strings is slow, especially when the patterns contain alternations and the
# strings contain multiple matches.  Using separate matches against shorter strings, or using arrays of strings instead of a  single
# long string, may be faster.
shopt -s extglob

# If set, bash replaces directory names with the results of word expansion when
# performing filename completion.  This changes the contents of the readline
# editing buffer.  If not set, bash attempts to preserve what the user typed.
#
# Note: this is enabled by default on most distributions I think but might as
# well make sure it's always on.
shopt -s direxpand

# If set, bash attempts spelling correction on directory names during word
# completion if the directory name initially supplied does not exist.
#
# Note: this only works on paths with at least one level of dir, i.e. `cd
# mispelled` won't autocorrect but `cd mispelled/somedir` will.
shopt -s dirspell

# autocd  If  set,  a  command name that is the name of a directory is executed
# as if it were the argument to the cd command. This option is only used by
# interactive shells.
shopt -s autocd

source "$HOME/.profile"
source "$HOME/.functions.sh"

# MacOS sucks.
source_if_exists /opt/homebrew/etc/profile.d/bash_completion.sh
source_if_exists /usr/share/bash-completion/bash_completion
source_if_exists /etc/bash_completion

source_if_exists "$HOME/.prompt.bash"

source_if_exists "$HOME/.config/fzf/shell/key-bindings.bash"
source_if_exists "$HOME/.config/fzf/shell/completion.bash"
source_if_exists "$HOME/.config/fzf/fzf.bash"

for compfile in "$HOME/.local/share/bash-completions"/*; do
	source "$compfile"
done

if [ -n "$(find_executable mise)" ]; then
	eval "$(mise activate bash)"
fi
export PATH="/Users/mathewrobinson/.config/herd-lite/bin:$PATH"
export PHP_INI_SCAN_DIR="/Users/mathewrobinson/.config/herd-lite/bin:$PHP_INI_SCAN_DIR"
