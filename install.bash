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

if [[ -d $INSTALL_DIR ]]; then
	mkdir -p "$INSTALL_DIR"
fi

for FILE in init.el src; do
	DEST=$INSTALL_DIR/$FILE
	SOURCE=$(pwd -P)/$FILE
	case $1 in
		install)
			SKIP_FILE=
			if [[ -e $DEST || -L $DEST ]]; then
				VALID=
				until [[ $VALID ]]; do
					read -p "File \`$DEST' exists. ([b]ackup, [o]verwrite, [s]kip, [q]uit)? " CHOICE
					VALID=yes
					case $CHOICE in
						b | backup)
							set -o xtrace
							mv -i "$DEST" "$DEST.old"
							set +o xtrace
							break
							;;
						o | overwrite)
							break
							;;
						s | skip)
							SKIP_FILE=yes
							break
							;;
						q | quit)
							exit
							;;
						*)
							VALID=
							;;
					esac
				done
			fi
			
			if [[ $SKIP_FILE ]]; then
				continue
			fi
			
			set -o xtrace
			ln -sf "$SOURCE" "$INSTALL_DIR"
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
