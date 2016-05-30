# Source /etc/profile and /etc/profile.d files
# Get environments by executing dash (a minimalist bash).
# Skip PWD variable (cannot be set in fish).
# Change PATH to fish syntax (space-separated)
# Use `set -x` to set them (environment variables).
env -i HOME=$HOME dash -l -c printenv | sed -e '/^PWD=/d;/PATH/s/:/ /g;s/=/ /;s/^/set -x /' | source

set PATH $PATH ~/.cargo/bin ~/.node_modules/bin
#set PATH ~/.cabal/bin $PATH

# Remove greeting
set fish_greeting

abbr -a r ranger
abbr -a pacup sudo pacman -Syu
abbr -a pacs sudo pacman -S
abbr -a pacss sudo pacman -Ss
abbr -a startx startx -- -ardelay 200 -arinterval 30
abbr -a mountusb sudo mount -o uid=fmdkdd,gid=fmdkdd /dev/ /mnt/usb
abbr -a umountusb sudo umount /mnt/usb

#set NO_AT_BRIDGE 1 # Fix for accessibility bus warning when launching evince, sunvox ...
