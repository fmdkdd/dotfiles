# Source /etc/profile and /etc/profile.d files
# Get environments by executing dash (a minimalist bash).
# Skip PWD variable (cannot be set in fish).
# Change PATH to fish syntax (space-separated)
# Use `set -x` to set them (environment variables).
env -i HOME=$HOME dash -l -c printenv | sed -e '/^PWD=/d;/PATH/s/:/ /g;s/=/ /;s/^/set -x /' | source

set PATH $PATH ~/.node_modules/bin
set PATH ~/.yarn/bin $PATH
#set PATH ~/.cabal/bin $PATH
set PATH ~/.local/bin $PATH

# Allow bundler to run sudo-less
if type -q ruby
   set GEM_HOME (ruby -e 'print Gem.user_dir')
   set PATH $GEM_HOME/bin $PATH
end

# Remove greeting
set fish_greeting

abbr -a r ranger
abbr -a pacup sudo pacman -Syu
abbr -a pacs sudo pacman -S
abbr -a pacss sudo pacman -Ss
abbr -a startx ssh-agent startx -- -ardelay 200 -arinterval 30
abbr -a n ninja

#set NO_AT_BRIDGE 1 # Fix for accessibility bus warning when launching evince, sunvox ...

if test -d /opt/android-sdk
    set ANDROID_HOME /opt/android-sdk
end
