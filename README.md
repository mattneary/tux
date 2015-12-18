# tux

> Reproducible tmux arrangements

## Setup

```sh
$ cabal install
$ cabal run [args...]
```

## Why?

Tux ensures that you can instantly spin up and organize all of your common
shell processes. Importantly, it adds a layer of indirection between the
processes you are working with and the full set of those which are running. Tux
is centered around a workspace into which your current processes are linked.
When you run tux it spins up only those processes which are not yet running and
then populates your workspace with the ones you need.

## Usage

Tux configures tmux sessions based on a tree of `.tux.yml` files, with the
root node being either in the current directory or in the path provided as
argument to tux.

A tux node points to other nodes through its `children` property.
Additionally, a tux node takes a `name`, a `rootPath` and `commands`, but its
`rootPath` and `children` are optional. A tux command is an object with a
`command` property and optionally a `path` property.

A node's `rootPath` is the path relative to which all commands are run. This
path is itself relative to the node file; child references are too.

Below is an example tux node.

```yaml
name: server
rootPath: ..
commands:
  - command: serve 8080
  - command: echo 'Running server'
    path: ../../
children: [./nested]
```

