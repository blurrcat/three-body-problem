#!/bin/sh
set -x
git add -f dist
git commit -m "temp commit for deploy $CIRCLE_SHA1"
git push dokku -f deploy:master
git reset HEAD~1
