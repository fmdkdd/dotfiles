# Source /etc/profile and /etc/profile.d files
env -i HOME=$HOME dash -l -c printenv | sed -e '/PATH/s/:/ /g;s/=/ /;s/^/set -x /' | source

set PATH $PATH ~/.node_modules/bin 

#set PATH ~/.cabal/bin $PATH
#set NO_AT_BRIDGE 1 # Fix for accessibility bus warning when launching evince, sunvox ...
