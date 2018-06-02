#!/bin/bash

set -x

BRANCH=master

# Remove any previous stopify versions
rm -rf node_modules/{stopify,stopify-continuations}

# Clone the stopify repo
git clone https://github.com/plasma-umass/Stopify.git --branch $BRANCH stopify

cd stopify
# Build the stopify version
yarn install && yarn run build

# copy the stopify version
cd ..
cp -R stopify/stopify-continuations node_modules/
cp -R stopify/stopify node_modules/

rm -rf stopify
