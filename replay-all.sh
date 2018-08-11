#!/bin/bash

BASE_DIRECTORY=$1
GAMES=$(ls $1/starter-pack/tower-defence-matches/)

for GAME in GAMES; do
    PLAYER_1=$(ls "$GAME/Round 001" | sed -e "s/[AB] - //g" | head -1)
    PLAYER_2=$(ls "$GAME/Round 001" | sed -e "s/[AB] - //g" | tail -1)
    ./bin/TestEngine-exe $PLAYER_1 $PLAYER_2 $GAME
done
