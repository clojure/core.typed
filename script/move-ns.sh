#!/bin/sh

git grep -l $1 | xargs sed -i '' "s/$1/$2/g"
