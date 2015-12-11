[![Build Status](https://travis-ci.org/jacekm-git/HsPredictor.svg)](https://travis-ci.org/jacekm-git/HsPredictor)
[![Coverage Status](https://coveralls.io/repos/jacekm-git/HsPredictor/badge.svg?branch=master&service=github)](https://coveralls.io/github/jacekm-git/HsPredictor?branch=master)
[![License](https://img.shields.io/badge/License-GPL--3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.en.html)
* About *
Clone of BetBoy.
* Warning *
HsPredictor is at very early stage of development.
* Dependencies *
- docker
* Installation *
```
user$ docker pull jacekxp2/hspredictor:latest
user$ sh debian_sandbox
docker-root$ cabal sandbox init
docker-root$ cabal cabal install --only-dependencies --enable-tests
docker-root$ cabal test
docker-root$ cabal build
docker-root$ exit
user$ cd dist/build/HsPredictor-exec
user$ ./HsPredictor-exec
```