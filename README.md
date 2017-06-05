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

Bash
----

```bash
rm ~/.bashrc # if there is such file
ln -s ~/dotfiles/bash/bashrc ~/.bashrc
ln -s ~/dotfiles/bash/spaceknow ~/.bash_spaceknow
source ~/.bashrc
```

Emacs
-----

```bash
ln -s ~/dotfiles/emacs/init.el ~/.emacs.d/init.el
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
