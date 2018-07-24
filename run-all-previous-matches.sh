#!/bin/bash

for folder in $(ls starter-pack/tower-defence-matches/); do
    ./bin/TestEngine-exe Quiescent James /home/edward/wip/entelect-challenge-2018/starter-pack/tower-defence-matches/$folder/
done
