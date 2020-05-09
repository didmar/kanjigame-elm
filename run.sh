#!/bin/bash

./node_modules/elm-live/bin/elm-live.js -h 0.0.0.0 -s src/index.html -- src/Main.elm --output=elm.js
