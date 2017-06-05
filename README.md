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
sudo apt-get install aptitude gcc make mu4e xclip
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
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el
# this is necessary to share clipboard via X11Forwarding
cd ~/.emacs.d/lisp/
wget https://raw.githubusercontent.com/emacsmirror/xclip/master/xclip.el
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
