# DataHaskell/dh-core

[![Build Status](https://travis-ci.org/DataHaskell/dh-core.png)](https://travis-ci.org/DataHaskell/dh-core)

DataHaskell core project monorepo


## Aims

This project aims to provide a native, end-to-end data science toolkit in Haskell. To achieve this, many types of experience are valuable; engineers, scientists, programmers, visualization experts, data journalists are all welcome to join the discussions and contribute.
Not only this should be a working piece of software, but it should be intuitive and pleasant to use.
All contributions, big or small, are very welcome and will be acknowledged.

## Architecture

One single repository allows us to experiment with interfaces and move code around much more freely than many single-purpose repositories. Also, it makes it more convenient to track and visualize progress.

A number of authors and maintainers agreed to move ownership of their repositories under the `dh-core` umbrella. In some cases, these packages were already published on Hackage and cannot simply disappear from there, nor can this new line of development break downstream packages.

For this reason, contributed packages will appear as subdirectories to the main `dh-core` project, and will need to retain their original .cabal file.  

We use the [`stack`](https://docs.haskellstack.org/en/stable/README/) build tool, and this can take care of multi-package projects; its `packages` stanza has only its directory as a default, but can contain a list of paths to other Cabal projects; e.g. in our case it could look like:

    packages:
    - .
    - analyze/
    - datasets/
    

## Contributing

1. Open an issue (https://github.com/DataHaskell/dh-core/issues) with a description of what you want to work on (if it's not already open)
2. Assign or add yourself to the issue contributors
3. Add code on the issue branch
4. Add tests "
5. Add yourself to the list of maintainers in the .cabal file (if you're not already)
6. Send a pull request, referencing the issue
7. Merge only _after_ another contributor has reviewed and approved the PR



### Contributed packages

Packages that are listed on Hackage already must be added here as distinct sub-directories. Once the migration is complete (PRs merged etc.), add the project to this table :


| Package | Original author(s) | First version after merge |
| --- | --- | --- |
| `analyze` | [Eric Conlon](https://github.com/ejconlon) | 0.2.0 | 
| `datasets` | [Tom Nielsen](https://github.com/glutamate) | 0.2.6 | 


NB: Remember to bump version numbers and change web links accordingly when moving in contributed packages.