#!/bin/sh

set -euC


# ----------------------------------------- #

SRCDIR="/Vrac/2I008-1819"

# Set up PATH and proxy settings
install_env_file () {
  if [ ! -e "$HOME/$1" ]; then
    touch "$HOME/$1"
  fi
  cat >> "$HOME/$1" <<EOF

# update PATH for 2I008 + set proxy settings
. $SRCDIR/source_me
EOF
}

# ----------------------------------------- #

reload_required=0

if [ ! -e "$HOME/.opam" ]; then
  ln -s "$SRCDIR/.opam" "$HOME/.opam"
  echo "=> Symlinked .opam"
else
  echo "=> .opam exists already → skip"
fi

if [ ! -e "$HOME/.ocamlinit" ]; then
  ln -s "$SRCDIR/.ocamlinit" "$HOME/.ocamlinit"
  echo "=> Symlinked .ocamlinit"
else
  echo "=> .ocamlinit exists already → skip"
fi

if ! command -v opam >/dev/null ; then
  install_env_file .bashrc
  echo "=> .bashrc updated"
  install_env_file .profile
  echo "=> .profile updated"
  reload_required=1
else
  echo "=> PATH already up to date → skip"
fi

if [ "$reload_required" = "1" ]; then
  echo "=>"
  echo "=> Please reopen your terminal or source ~/.bashrc"
  echo "=>"
fi
