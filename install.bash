#!/bin/bash
set -o nounset
set -o errexit

usage()
{
	echo "Usage: $0 [install | remove]" 1>&2
	exit 1
}

if [[ $# -ne 1 ]]; then
	usage
fi

readonly SCRIPT_NAME=$(basename "$0")
readonly INSTALL_DIR=~/.emacs.d

if [[ $1 == install && ! -d $INSTALL_DIR ]]; then
    set -o xtrace
    mkdir -p "$INSTALL_DIR"
    set +o xtrace
fi

for FILE in init.el src; do
	DEST=$INSTALL_DIR/$FILE
	SOURCE=$(pwd -P)/$FILE
	case $1 in
		install)
			set -o xtrace
			ln --backup=existing \
			    --interactive \
			    --symbolic \
			    --no-target-directory \
			    "$SOURCE" "$DEST"
			set +o xtrace
			;;
		remove)
			if [[ -L "$DEST" && $(readlink "$DEST") == "$SOURCE" ]]; then
				set -o xtrace
				rm "$DEST"
				set +o xtrace
			fi
			;;
		*)
			usage
			;;
	esac
done
