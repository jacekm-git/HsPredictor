
[![Build Status](https://travis-ci.org/jacekm-git/HsPredictor.svg)](https://travis-ci.org/jacekm-git/HsPredictor)
[![Build Status](https://circleci.com/gh/jacekm-git/HsPredictor.svg?style=shield&circle-token=c492e3e9a1137d25e5814078a2e1a8abf54c1467)](https://circleci.com/gh/jacekm-git/HsPredictor)
[![Coverage Status](https://coveralls.io/repos/jacekm-git/HsPredictor/badge.svg?branch=master&service=github)](https://coveralls.io/github/jacekm-git/HsPredictor?branch=master)
[![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.en.html)
# About 
Predicting outcome of football matches.
# Warning 
HsPredictor is at very early stage of development.
# Dependencies 
* docker

# Installation 
```sh
user$ sh debian_sandbox
docker-root$ cabal sandbox init
docker-root$ cabal install --only-dependencies --enable-tests
docker-root$ cabal test
docker-root$ cabal build
docker-root$ exit
user$ cd dist/build/HsPredictor-exec
user$ ./HsPredictor-exec
```
