#!/bin/bash

./node_modules/elm-live/bin/elm-live.js -h 0.0.0.0 -d public -u -- src/Main.elm --output=public/elm.js
