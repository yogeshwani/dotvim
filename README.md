dotvim
======

Vim config



cd ~/.vim
mkdir ~/.vim/bundle
git submodule add http://github.com/tpope/vim-fugitive.git bundle/fugitive
git add .
git commit -m "Install Fugitive.vim bundle as a submodule."

cd ~
git clone http://github.com/davidpine/dotvim.git ~/.vim
ln -s ~/.vim/vimrc ~/.vimrc
ln -s ~/.vim/gvimrc ~/.gvimrc
cd ~/.vim
git submodule init
git submodule update


cd ~/.vim/bundle/fugitive
git pull origin master

git submodule foreach git pull origin master

