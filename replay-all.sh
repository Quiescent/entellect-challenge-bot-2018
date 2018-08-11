#!/bin/bash

BASE_DIRECTORY=$1
GAMES=$(ls $1)

for GAME in $GAMES; do
    PLAYER_1=$(ls "$1/$GAME/Round 001" | sed -e "s/[AB] - //g" | head -1)
    PLAYER_2=$(ls "$1/$GAME/Round 001" | sed -e "s/[AB] - //g" | tail -1)
    ./bin/TestEngine-exe $PLAYER_1 $PLAYER_2 "$1/$GAME/"
done
