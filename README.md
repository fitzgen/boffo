# Boffo

[![Build Status](https://travis-ci.org/fitzgen/boffo.png?branch=master)](https://travis-ci.org/fitzgen/boffo)

Boffo is a turn based gaming platform.

## Developing

### Getting set up

Copy the example settings.

    $ cp example_boffo_settings.hrl boffo_settings.hrl
    $ export BOFFO_SETTINGS=`pwd`/boffo_settings.hrl

Edit the settings to fit in with your dev environment, and then run `make`.

    $ make

### Running tests

    $ make test
