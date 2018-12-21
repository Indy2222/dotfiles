Setup
=====

First install and setup Git:

```bash
sudo apt-get update
sudo apt-get install git
```

then clone this repository:

```bash
git clone git@github.com:Indy2222/dotfiles ~/dotfiles
```

directory structure I use:

```bash
mkdir ~/downloads
```

other "necessary" software from Ubuntu repository:

```bash
sudo apt-get install aptitude gcc make mu4e xclip htop silversearcher-ag \
    curl qalculate jq w3m offlineimap
```

ZSH
---

On client:

```bash
sudo apt-get install fonts-powerline
```

On server:

```bash
sudo apt-get install zsh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
rm ~/.zshrc
ln -s ~/dotfiles/zsh/zshrc ~/.zshrc
ln -s ~/dotfiles/zsh/spaceknow ~/.zsh_spaceknow
sudo chsh -s /usr/bin/zsh indy
```

Theme:

```bash
mkdir ~/.oh-my-zsh/custom/themes
ln -s ~/dotfiles/zsh/minimal.zsh-theme ~/.oh-my-zsh/custom/themes/
```

Plugins:

```bash
cd ~/.oh-my-zsh/custom/plugins
# syntax highlighting
git clone https://github.com/zdharma/fast-syntax-highlighting.git
git clone https://github.com/zsh-users/zsh-autosuggestions
```

Emacs
-----

Installation prerequisites(works for v25.2):

```bash
sudo apt-get install libxml2-dev libtinfo-dev libjpeg-dev libtiff-dev \
    libgif-dev libpng-dev gnutls-bin
```

configuration:

```bash
mkdir ~/.emacs.d
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el
```

start Emacs server on startup:

```bash
mkdir -p ~/.config/systemd/user/
ln -s /home/indy/dotfiles/emacs.service ~/.config/systemd/user/emacs.service
systemctl --user daemon-reload
systemctl --user enable emacs.service
systemctl --user start emacs.service
```

Python
------

```bash
sudo apt-get install python-pip
# these makes Emacs Elpy more powerful
sudo pip install rope importmagic yapf flake8 isort
```

Tmux
----

```bash
ln -s ~/dotfiles/tmux.conf ~/.tmux.conf
```

Git
---

```bash
ln -s ~/dotfiles/git/config ~/.gitconfig
ln -s ~/dotfiles/git/gitignore ~/.gitignore_global
```

Docker
------

```bash
sudo apt-get install docker.io
sudo sudo usermod -G docker $USER
sudo exec sudo su -l $USER
```

Google Cloud SDK
----------------

Follow instructions from https://cloud.google.com/sdk/docs/quickstart-linux.

The Silver Searcher
-------------------

```bash
ln -s ~/dotfiles/agignore ~/.agignore
```

Rust
----

```bash
curl https://sh.rustup.rs -sSf | sh
cargo install racer
rustup component add rust-src rustfmt-preview
```

GPG
---

```bash
ln -s ~/dotfiles/gpg/gpg-agent.conf ~/.gnupg/gpg-agent.conf
```

Firefox
--------

Install [Tree Style Taps](https://addons.mozilla.org/en-US/firefox/addon/tree-style-tab/).


```bash
mkdir ~/.mozilla/firefox/<profile-name>.default/chrome
ln -s ~/dotfiles/firefox/userChrome.css \
    ~/.mozilla/firefox/<profile-name>.default/chrome/userChrome.css
```

Offlineimap
-----------

```bash
ln -s ~/dotfiles/offlineimaprc ~/.offlineimaprc
mkdir ~/mail
mkdir ~/mail/mgn
mkdir ~/mail/sk
mkdir ~/mail/fel
```

Mutt
----

Create GPG encrypted file `~/.mutt/sk.gpg` and `~/.mutt/mgn.gpg` with
`$imap_pass` and `$smtp_pass` variables.

```bash
sudo pip3 install urlscan
git clone https://github.com/neomutt/neomutt
cd neomutt
./configure --disable-doc --ssl --gnutls --gpgme --lmdb --gpgme

ln -s ~/dotfiles/mutt/muttrc ~/.muttrc
mkdir ~/.mutt
ln -s ~/dotfiles/mutt/mailcap ~/.mutt/mailcap
ln -s ~/dotfiles/mutt/sk ~/.mutt/sk
ln -s ~/dotfiles/mutt/mgn ~/.mutt/mgn
ln -s ~/dotfiles/mutt/fel ~/.mutt/fel
ln -s ~/dotfiles/mutt/sk_signature.txt ~/.mutt/sk_signature.txt
ln -s ~/dotfiles/mutt/mgn_signature.txt ~/.mutt/mgn_signature.txt
ln -s ~/dotfiles/mutt/fel_signature.txt ~/.mutt/fel_signature.txt
ln -s ~/dotfiles/mutt/theme ~/.mutt/theme
mkdir ~/.mutt/sk_header_cache
mkdir ~/.mutt/mgn_header_cache
touch ~/.mutt/aliases
mkdir ~/.mutt/account.sk
mkdir ~/.mutt/account.mgn
```
