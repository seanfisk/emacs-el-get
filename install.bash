#!/bin/bash
set -o nounset
set -o errexit

readonly INSTALL_DIR="$HOME/.emacs.d"
readonly IGNORE_REGEX='(^\.git(ignore|modules)?|install.bash)|\.markdown$'

if [[ -d "$INSTALL_DIR" ]]; then
    echo "\`$INSTALL_DIR' exists. Please remove it before continuing." 1>&2
    exit 1
fi
mkdir "$INSTALL_DIR"

for FILE in $(ls -A | grep -iEv "$IGNORE_REGEX"); do
	DEST="$INSTALL_DIR/$FILE"
	SOURCE="$(pwd -P)/$FILE"
	SKIP_FILE=
	if [[ -e $DEST || -L $DEST ]]; then
		VALID=
		until [[ $VALID ]]; do
			read -p "File \`$DEST' exists. ([b]ackup, [o]verwrite, [s]kip, [q]uit)? " CHOICE
			VALID=yes
			case "$CHOICE" in
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
done
