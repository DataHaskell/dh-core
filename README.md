# DataHaskell/dh-core

[![Build Status](https://travis-ci.org/DataHaskell/dh-core.png)](https://travis-ci.org/DataHaskell/dh-core)
<a href="https://gitter.im/DataHaskell/Lobby" target="_blank"><img src="https://badges.gitter.im/Join%20Chat.svg"></a>

DataHaskell core project monorepo


## Aims

This project aims to provide a native, end-to-end data science toolkit in Haskell. To achieve this, many types of experience are valuable; engineers, scientists, programmers, visualization experts, data journalists are all welcome to join the discussions and contribute.
Not only this should be a working piece of software, but it should be intuitive and pleasant to use.
All contributions, big or small, are very welcome and will be acknowledged.

## Architecture

One single repository allows us to experiment with interfaces and move code around much more freely than many single-purpose repositories. Also, it makes it more convenient to track and visualize progress.

This is the directory structure of the project; the main project lives in the `dh-core` subdirectory:

    dh-core/
      dh-core/              
      dh-core-accelerate/
      ....


## Contributed packages

A number of authors and maintainers agreed to move ownership of their repositories under the `dh-core` umbrella. In some cases, these packages were already published on Hackage and cannot simply disappear from there, nor can this new line of development break downstream packages.

For this reason, contributed packages will appear as subdirectories to the main `dh-core` project, and will need to retain their original .cabal file.  

The `stack` tool can take care of multi-package projects; its `packages` stanza in the `stack.yaml` file has only its directory as a default, but can contain a list of paths to other Cabal projects; e.g. in our case it could look like:

    packages:
    - .
    - analyze/
    - datasets/

Packages that are listed on Hackage already must be added here as distinct sub-directories. Once the migration is complete (PRs merged etc.), add the project to this table :


| Package | Description | Original author(s) | First version after merge |
| --- | --- | --- | --- | 
| [`analyze`](https://hackage.haskell.org/package/analyze) | Data analysis and manipulation library | [Eric Conlon](https://github.com/ejconlon) | 0.2.0 | 
| [`datasets`](https://hackage.haskell.org/package/datasets) | A collection of ready-to-use datasets | [Tom Nielsen](https://github.com/glutamate) | 0.2.6 | 
| [`dense-linear-algebra`](https://hackage.haskell.org/package/dense-linear-algebra) | Fast, native dense linear algebra primitives | [Brian O'Sullivan](https://github.com/bos), [Alexey Khudyakov](https://github.com/Shimuuar) | 0.1.0 (a) | 

(a) : To be updated

NB: Remember to bump version numbers and change web links accordingly when moving in contributed packages.





## Contributing

1. Open an issue (https://github.com/DataHaskell/dh-core/issues) with a description of what you want to work on (if it's not already open)
2. Assign or add yourself to the issue contributors
3. Pull from `dh-core:master`, start a git branch, add code 
4. Add tests 
5. Update the changelog, describing briefly your changes and their possible effects
6.

* If you're working on a contributed package (see next section), increase the version number in the Cabal file accordingly

* If you bumped version numbers, make sure these are updated accordingly in the Travis CI .yaml file

7. Send a pull request with your branch, referencing the issue
8. `dh-core` admins : merge only _after_ another admin has reviewed and approved the PR


### GHC and Stackage compatibility

Tested against :

- Stackage nightly-2019-02-27 (GHC 8.6.3)



## Development information and guidelines

### Dependencies

We use the [`stack`](https://docs.haskellstack.org/en/stable/README/) build tool.

Some systems /might/ need binaries and headers for these additional libraries:

* zlib
* curl

(however if you're unsure, first try building with your current configuration).

Nix users should set `nix.enable` to `true` in the `dh-core/dh-core/stack.yaml` file.


### Building instructions

In the `dh-core/dh-core` subdirectory, run 

    $ stack build

and this will re-build the main project and the contributed packages.

While developing this `stack` command can come in handy : it will trigger a re-build and run the tests every time a file in the project is modified:

    $ stack build --test --ghc-options -Wall --file-watch

## Testing

Example : 

    $ stack test core:doctest core:spec

The `<project>:<test_suite>` pairs determine which tests will be run. 


## Continuous Integration (TravisCI)

Travis builds `dh-core` and its hosted projects every time a commit is pushed to Github. 
Currently the `dh-core/.travis.yml` script uses the following command to install the GHC compiler, build the project and subprojects with `stack`, run the tests and build the Haddock documentation HTMLs:

    - stack $ARGS --no-terminal --install-ghc test core:spec core:doctest dense-linear-algebra:spec --haddock


## Visualizing the dependency tree of a package

`stack` can produce a .dot file with the dependency graph of a Haskell project, which can then be rendered by the `dot` tool (from the [`graphviz`](https://graphviz.gitlab.io/_pages/Download/Download_source.html) suite).
For example, in the following command the output of `stack dot` will be piped into `dot`, which will produce a SVG file called `deps.svg`:

`stack dot --external --no-include-base --prune rts,ghc-prim,ghc-boot-th,template-haskell,transformers,containers,deepseq,bytestring,time,primitive,vector,text,hashable | dot -Tsvg > deps.svg`




