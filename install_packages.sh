echo "Bootstrapping straight.el and installing packages…"
emacs --batch -l ~/.emacs.d/init.el \
      --eval "(straight-pull-all)" \
      --eval "(straight-rebuild-all)"
