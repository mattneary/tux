# tux

> Reproducible tmux arrangements

## Setup

```sh
$ cabal install
$ cabal run [args...]
```

## Usage

Tux configures tmux sessions based on a tree of `.tux.json` files, with the
root node being either in the current directory or in the path provided as
argument to tux.

A tux node points to other nodes through its `children` property.
Additionally, a tux node takes a `name`, a `rootPath` and `commands`. Its
`rootPath` and `children` are optional.

A tux command is an object with a `command` property and optionally a `path`
property.

Below is an example tux node.

```js
{
  "name": "server",
  "rootpath": ".",
  "commands": [{
    "command": "serve 8080"
  }, {
    "command": "echo 'running server'",
    "path": "../../"
  }],
  "children": ["./nested"]
}
```

