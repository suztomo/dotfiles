ln -s $HOME/dotfiles/.emacs.el $HOME/.emacs.el
ln -s $HOME/dotfiles/.screenrc $HOME/.screenrc
ln -s $HOME/dotfiles/.zshrc $HOME/.zshrc
ln -s $HOME/dotfiles/.gitignore $HOME/.gitignore
ln -s $HOME/dotfiles/.gdbinit $HOME/.gdbinit
IFS=:
GIT=false
for d in $PATH
do test -x $d/git && GIT=true
done
if $GIT
then
  echo "git found!"
  git config --global user.name "suztomo"
  git config --global user.email "suztomo+github@gmail.com"
  git config --global core.excludesfile "$HOME/.gitignore"
else echo "no git"
fi

