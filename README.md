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
sudo apt-get install aptitude gcc make mu4e xclip htop silversearcher-ag
```


Bash
----

```bash
rm ~/.bashrc # if there is such file
ln -s ~/dotfiles/bash/bashrc ~/.bashrc
ln -s ~/dotfiles/bash/spaceknow ~/.bash_spaceknow
source ~/.bashrc
```

GnuPG
-----

Import GNU GPG keyring:

```bash
cd ~/downloads
wget https://ftp.gnu.org/gnu/gnu-keyring.gpg
gpg --import gnu-keyring.gpg
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
touch ~/.emacs.d/custom.el
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
git config --global user.email "indra@spaceknow.com"
git config --global user.name "Martin Indra"
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
