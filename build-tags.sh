#!/bin/bash

TAGS=$(git tag)

set -e

for tag in $TAGS; do
    echo "=========="
    echo "WORKING ON: $tag"
    echo "=========="
    echo git reset --hard $tag
    git reset --hard $tag
    echo stack install --local-bin-path bin
    stack install --local-bin-path bin
    BASE_DIR="working-binaries/$tag/"
    BIN_DIR="$BASE_DIR/bin"
    echo mkdir -p $BIN_DIR
    mkdir -p $BIN_DIR
    echo cp bin/EntelectChallenge2018-exe $BIN_DIR
    cp bin/EntelectChallenge2018-exe $BIN_DIR
    echo 'sed -e "s/Quiescent/$tag/" bot.json > $BASE_DIR/bot.json'
    sed -e "s/Quiescent/$tag/" bot.json > $BASE_DIR/bot.json
    echo
done

git checkout origin/master
