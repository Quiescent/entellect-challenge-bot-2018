#!/bin/bash

set -e

START_COMMIT=$1
END_COMMIT=$2
COMMITS=$(git rev-list $START_COMMIT $END_COMMIT)

for commit in $COMMITS; do
    echo "===================="
    echo "Working On: $commit"
    echo "===================="
    echo "git reset --hard $commit"
    git reset --hard $commit
    echo "sed -i 's/maxSearchTime = 1800000000/maxSearchTime = 60000000000/g' src/SearchSpace.hs"
    sed -i 's/maxSearchTime = 1800000000/maxSearchTime = 60000000000/g' src/SearchSpace.hs
    echo "stack --nix --profile install --local-bin-path bin"
    stack --nix --profile install --local-bin-path bin
    echo "git checkout src/SearchSpace.hs"
    git checkout src/SearchSpace.hs
    echo "./bin/EntelectChallenge2018-exe +RTS -p -RTS"
    ./bin/EntelectChallenge2018-exe +RTS -p -RTS
    PROF_DIR=profiling-$commit
    echo "mkdir $PROF_DIR"
    mkdir $PROF_DIR
    echo "cp EntelectChallenge2018-exe.prof $PROF_DIR"
    cp EntelectChallenge2018-exe.prof $PROF_DIR
done

echo "git reset --hard origin/master"
git reset --hard origin/master
