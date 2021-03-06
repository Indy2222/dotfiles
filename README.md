Post Installation Setup
=======================

Peform the following steps after bare
[installation](https://wiki.archlinux.org/index.php/installation_guide)
of Arch Linux.

Network
-------

```bash
systemctl enable --now systemd-networkd
systemctl enable --now systemd-resolved
# To make non GNU-libc applications (like kubectl written in Golang) work.
ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
```

Modify `/etc/systemd/network/20-wired.network` to:

```
[Match]
Name=ETHERNET_DEVICE

[Network]
DHCP=ipv4

[DHCP]
RouteMetric=10
```

To setup Wi-fi:

```bash
pacman -S iwd
systemctl enable --now iwd
```

Modify `/etc/systemd/network/25-wireless.network` to:

```
[Match]
Name=WIFI_DEVICE

[Network]
DHCP=ipv4

[DHCP]
RouteMetric=20
```

Modify `/etc/iwd/main.conf` to:

```
[General]
dns_resolve_method=systemd
```

Use `iwctl` to connect to a Wi-fi network.

For `eduroam` create file `/var/lib/iwd/eduroam.8021x` with the following
content (don't forget to replace the password). Set permissions to `600` to the
file.

```
[Security]
EAP-Method=PEAP
EAP-Identity=indrama1@fel.cvut.cz
EAP-PEAP-CACert=/etc/ssl/certs/DigiCert_Assured_ID_Root_CA.pem
EAP-PEAP-Phase2-Method=MSCHAPV2
EAP-PEAP-Phase2-Identity=indrama1@fel.cvut.cz
EAP-PEAP-Phase2-Password=<place-password-here>

[Settings]
Autoconnect=true
```

Use Avahi for zero-configuration network (service discovery, mDNS, etc.):

```bash
pacman -S avahi nss-mdns
systemctl enable --now avahi-daemon
```

Add `mdns_minimal [NOTFOUND=return]` to `hosts` in `/etc/nsswitch.conf` before
`resolve` and `dns`. See https://wiki.archlinux.org/index.php/Avahi

NTP
---

```bash
systemctl enable --now systemd-timesyncd
```

Create User
-----------

```bash
useradd indy
passwd indy
mkdir /home/indy
chown -R indy:indy /home/indy
pacman -S sudo
# And add indy to sudo users
visudo
```

Install KDE
-----------

```bash
pacman -S xf86-video-intel
pacman -S sddm plasma-meta
systemctl enable sddm
```

Setup Under My User
===================

All of the following commands work if executed under my user.

Useful Software
---------------

```bash
# Note that gcc is needed for Emacs to work properly. See
# https://wiki.archlinux.org/index.php/Emacs#Emacs_fails_to_start_with_the_error_message_'Undefined_color:_%22WINDOW_FOREGROUND%22'
sudo pacman -S firefox konsole xclip gcc htop jq ripgrep ktorrent dolphin \
    okular vlc spectacle rsync make python-sphinx ark korganizer bat wget exa \
    unzip gzip kuickshow
```

SSH
---

```bash
sudo pacman -S ssh
ssh-keygen -t rsa -b 4096 -C "martin.indra@mgn.cz"
ssh-add ~/.ssh/id_rsa
```

Add the SSH key to GitHub and GitLab.


Git
---

```bash
sudo pacman -S git git-lfs git-crypt
```

Pass
----

```bash
sudo pacman -S pass
git clone git@github.com:Indy2222/pass.git ~/.password-store
```

Dotfiles
--------

Clone this repository:

```bash
git clone git@github.com:Indy2222/dotfiles ~/dotfiles
```

Git Config
----------

```bash
ln -s ~/dotfiles/git/config ~/.gitconfig
ln -s ~/dotfiles/git/gitignore ~/.gitignore_global
```

GPG
---

```bash
ln -s ~/dotfiles/gpg/gpg-agent.conf ~/.gnupg/gpg-agent.conf
killall gpg-agent
# private.txt is placed on a secret device
gpg --import --armor private.txt
```

KBD Conf
--------

```bash
sudo cp ~/dotfiles/90-custom-kbd.conf /etc/X11/xorg.conf.d/90-custom-kbd.conf
sudo chown root:root /etc/X11/xorg.conf.d/90-custom-kbd.conf
```

ZSH
---

```bash
sudo pacman -S zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
rm ~/.zshrc
ln -s ~/dotfiles/zsh/zshrc ~/.zshrc
ln -s ~/dotfiles/zsh ~/zsh
```

Theme:

```bash
ln -s ~/dotfiles/zsh/minimal.zsh-theme ~/.oh-my-zsh/custom/themes/
```

Plugins:

```bash
cd ~/.oh-my-zsh/custom/plugins
# syntax highlighting
git clone https://github.com/zdharma/fast-syntax-highlighting.git
git clone https://github.com/zsh-users/zsh-autosuggestions
```

Tmux
----

```bash
sudo pacman -S tmux
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
```

Terminal
--------

```bash
sudo pacman -S alacritty
mkdir -p ~/.config/alacritty
ln -s ~/dotfiles/alacritty.yml ~/.config/alacritty/alacritty.yml
```

Emacs
-----

```bash
sudo pacman -S emacs aspell aspell-en aspell-cs spell-it
mkdir ~/.emacs.d
mkdir ~/notes
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el
systemctl --user enable --now emacs
```

Offlineimap
-----------

```bash
sudo pacman -S offlineimap
ln -s ~/dotfiles/offlineimaprc ~/.offlineimaprc
mkdir ~/mail
mkdir ~/mail/mgn
mkdir ~/mail/dm
mkdir ~/mail/fel
```

Neomutt
-------

```bash
sudo pacman -S neomutt w3m urlscan

ln -s ~/dotfiles/mutt/muttrc ~/.muttrc
mkdir ~/.mutt
ln -s ~/dotfiles/mutt/mailcap ~/.mutt/mailcap
ln -s ~/dotfiles/mutt/datamole ~/.mutt/datamole
ln -s ~/dotfiles/mutt/mgn ~/.mutt/mgn
ln -s ~/dotfiles/mutt/fel ~/.mutt/fel
ln -s ~/dotfiles/mutt/toptal ~/.mutt/toptal
ln -s ~/dotfiles/mutt/datamole_signature.txt ~/.mutt/datamole_signature.txt
ln -s ~/dotfiles/mutt/mgn_signature.txt ~/.mutt/mgn_signature.txt
ln -s ~/dotfiles/mutt/fel_signature.txt ~/.mutt/fel_signature.txt
ln -s ~/dotfiles/mutt/toptal_signature.txt ~/.mutt/toptal_signature.txt
ln -s ~/dotfiles/mutt/theme ~/.mutt/theme
mkdir ~/.mutt/datamole_header_cache
mkdir ~/.mutt/mgn_header_cache
mkdir ~/.mutt/fel_header_cache
mkdir ~/.mutt/toptal_header_cache
touch ~/.mutt/aliases
mkdir ~/.mutt/account.datamole
mkdir ~/.mutt/account.mgn
mkdir ~/.mutt/account.fel
mkdir ~/.mutt/account.toptal
```

Firefox-Plasma Integration
--------------------------

Install https://addons.mozilla.org/en-US/firefox/addon/plasma-integration/

```bash
sudo pacman -S plasma-browser-integration
```

Rust
----

```bash
sudo pacman -S rust rust-racer
```

Python
------

```bash
sudo pacman -S python-pip python-poetry
# These makes Emacs Elpy more powerful
sudo pacman -S python-isort flake8 yapf python-rope python-jedi
```

.NET
----

```bash
sudo pacman -S dotnet-sdk
```

Docker
------

```bash
sudo pacman -S docker
sudo usermod -G docker indy
systemctl enable --now docker
```

Kubernetes
----------

```bash
sudo pacman -S kubectl minikube virtualbox linux-headers
```

Google Cloud SDK
----------------

Follow instructions from https://cloud.google.com/sdk/docs/quickstart-linux.

Microsoft Azure
---------------

Follow instruction from
https://docs.microsoft.com/en-us/cli/azure/install-azure-cli-apt?view=azure-cli-latest

TeX
---

```bash
sudo pacman -S texlive-most
```

Tor
---

```bash
sudo pacman -S tor
systemctl enable --now tor
```

In Preferences > General > Network Settings > Settings…, select Manual proxy
configuration and enter SOCKS host localhost with port 9050 (SOCKS v5). To
channel all DNS requests through TOR's socks proxy, also select Proxy DNS when
using SOCKS v5.

Printing
--------

```bash
sudo pacman -S cups
systemctl enable org.cups.cupsd.socket
```

More Network
------------

```bash
sudo pacman -S vnstat
systemctl enable --now vnstat
```
