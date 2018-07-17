#!/bin/bash

TAGS=$(git tag)

set -e

STARTER_PACK_DIR="starter-pack/"
CONFIG_FILE="$STARTER_PACK_DIR/game-runner-config.json"

for tag in $TAGS; do
    echo "=========="
    echo "WORKING ON: $tag"
    echo "=========="
    BOT_DIR="$tag"
    echo emacs --batch --no-init -Q $CONFIG_FILE --eval "(progn (goto-char (point-min)) (search-forward \"working-binaries/\") (zap-to-char 1 ?\") (insert \"$BOT_DIR\\\"\") (save-buffer))"
    emacs --batch --no-init -Q $CONFIG_FILE --eval "(progn (goto-char (point-min)) (search-forward \"working-binaries/\") (zap-to-char 1 ?\") (insert \"$BOT_DIR\\\"\") (save-buffer))"
    echo "cd starter-pack/ && java -jar tower-defence-runner-2.0.1.jar"
    cd starter-pack/ && java -jar tower-defence-runner-2.0.1.jar && cd ..
done
