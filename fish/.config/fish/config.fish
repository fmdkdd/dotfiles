# Source /etc/profile and /etc/profile.d files
# Get environments by executing dash (a minimalist bash).
# Skip PWD variable (cannot be set in fish).
# Change PATH to fish syntax (space-separated)
# Use `set -x` to set them (environment variables).
env -i HOME=$HOME dash -l -c printenv | sed -e '/^PWD=/d;/PATH/s/:/ /g;s/=/ /;s/^/set -x /' | source

set PATH $PATH ~/.node_modules/bin
#set PATH ~/.cabal/bin $PATH
set PATH ~/.local/bin $PATH

# Remove greeting
set fish_greeting

abbr -a r ranger
abbr -a pacup sudo pacman -Syu
abbr -a pacs sudo pacman -S
abbr -a pacss sudo pacman -Ss
abbr -a startx ssh-agent startx -- -ardelay 200 -arinterval 30

#set NO_AT_BRIDGE 1 # Fix for accessibility bus warning when launching evince, sunvox ...
