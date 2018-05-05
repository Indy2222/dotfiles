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
sudo apt-get install aptitude gcc make mu4e xclip htop silversearcher-ag curl
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
sudo apt-get install libxml2-dev libtinfo-dev
```

configuration:

```bash
mkdir ~/.emacs.d
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el
# this is necessary to share clipboard via X11Forwarding
mkdir ~/.emacs.d/lisp/
cd ~/.emacs.d/lisp/
wget https://raw.githubusercontent.com/emacsmirror/xclip/master/xclip.el
```

start Emacs server on startup:

```bash
sudo ln -s /home/indy/dotfiles/emacs@.service /etc/systemd/system/emacs@.service
sudo systemctl enable emacs@indy.service
sudo systemctl start emacs@indy.service
```

Python
------

```bash
sudo apt-get install python-pip
# these makes Emacs Elpy more powerful
sudo pip install rope importmagic yapf flake8
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
