FROM debian:buster


RUN apt-get update && apt-get install -y curl npm

# Need NodeJS 10 for elm-live
RUN curl -sL https://deb.nodesource.com/setup_10.x | bash -
RUN apt-get update && apt-get install -y nodejs

# Install Elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/

RUN npm install --save-dev elm-live


ADD elm.json .
ADD public public
ADD src src
ADD run.sh .


ENTRYPOINT ["./run.sh"]
