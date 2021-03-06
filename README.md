Kanji game
============

An educational game to practice kanji-based Japanese words.

How to install (Linux)
-----------------------

The easiest way is to use Docker and docker-compose:
```bash
docker-compose up -d --build
```
Then go to http://127.0.0.1:8000 and start playing !


Otherwise you can build locally on Debian/Ubuntu (tested on Ubuntu 18.04):

[Install Elm 0.19 first](https://guide.elm-lang.org/install/elm.html)
```sh
curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip elm.gz
chmod +x elm
sudo mv elm /usr/local/bin/
```

Install NPM:
```sh
sudo apt install npm
```

Install [elm-live](https://github.com/wking-io/elm-live) dev server locally:
```sh
# Requires Node 10.x
curl -sL https://deb.nodesource.com/setup_10.x | sudo -E bash -
sudo apt install nodejs

npm install --save-dev elm-live
```

Run the following script to start the server on http://localhost:8000
```sh
./run.sh
```
