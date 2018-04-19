# timeout


[![Build Status](https://travis-ci.org/philwade/timeout.svg?branch=master)](https://travis-ci.org/philwade/timeout)

This is a tool for managing host files for productivity.

## install

You can get the latest version from the [releases page](https://github.com/philwade/timeout/releases) or install via homebrew:

```bash
$ brew update
$ brew install philwade/tools/timeout
```

## usage

Checkout the [website](http://timeout.philwade.org) for usage or:
```bash
$ timeout help
```

## development

To build/run:

    $ stack build
    $ stack exec timeout

Build watch:

    $ stack build --test --pedantic --fast --file-watch
