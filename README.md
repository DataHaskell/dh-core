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

## Building

We use the [`stack`](https://docs.haskellstack.org/en/stable/README/) build tool; in the `dh-core` subdirectory, run 

    $ stack build

and this will re-build the main project and the contributed packages.

More detailed developer information can be found in the wiki : https://github.com/DataHaskell/dh-core/wiki/Development 

### GHC and Stackage compatibility


Tested against :

- Stackage nightly-2019-02-27 (GHC 8.6.3)


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



### Contributed packages

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
| `analyze` | Data analysis and manipulation library | [Eric Conlon](https://github.com/ejconlon) | 0.2.0 | 
| `datasets` | A collection of ready-to-use datasets | [Tom Nielsen](https://github.com/glutamate) | 0.2.6 | 
| `dense-linear-algebra` | Fast, native dense linear algebra primitives | [Brian O'Sullivan](https://github.com/bos), [Alexey Khudyakov](https://github.com/Shimuuar) | 0.1.0 (a) | 

(a) : To be updated

NB: Remember to bump version numbers and change web links accordingly when moving in contributed packages.



