# Building

Developers can use this `stack` command, which will trigger a re-build and run the tests every time a file in the project is modified:

    $ stack build --test --ghc-options -Wall --file-watch
