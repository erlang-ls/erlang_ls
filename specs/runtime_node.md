## Specification of the API between the erlang_ls server and a runtime node

### Context

We are moving toward an architecture having the following components

- `Client` - the user-facing endpoint using the Language Server Protocol (LSP)
  to communicate with the Language `Server`.  e.g. vscode via [erlang_ls
  extension](https://github.com/erlang-ls/vscode), emacs using lsp-mode.

- `Server` - the erlang_ls server started up by the `Client`, in the context of a
  particular erlang `Project`.

- `Runtime Node` - an erlang VM that runs in the context of the project being
  developed.

- `Project` - The Erlang project that is being developed by the user of the
  `Client`.

### Implications

The intention is for the `Server` to

  - implement the Language Server Protocol,
  - manage comms with the `Client`,
  - manage the indexing of the project, and
  - coordinate activity with the `Runtime Node`.

It specifically has no knowledge about how a particular `Project` is built, or
its detailed structure.

The `Runtime Node` is the interface between the `Project` and the `Server`. It
  - knows about the structure of the project
  - knows how to build artifacts for the project
  - knows how to interpret "backend" calls to provide information for specific
    LSP requests.  Examples are
      - xref info
      - ability to run a specific test

In the bigger picture, this allows us to distribute a standard `Server`, which
focuses on the best possible LSP support.  There will then be multiple standard
`Runtime Node` implementations, one for each popular build system.

Initially we envisage ones for
  - rebar3
  - "plain" project, i.e. just some random .erl files on the filesystem
    somewhere

Because the protocol between the `Server` and the `Runtime Node` will be
standardised, and we will have out of the box implementations for popular
project build systems, it becomes easy for particular Erlang production sites to
adapt these nodes for use in their particular environments.  This includes the
ability to build in proprietary build systems, test methods, or diagnostics
sources.

### `Server` <-> `Runtime Node` Protocol

The protocol makes use of standard Erlang RPC.

The `Server` config file specifies which `Runtime Node` to use for a given
`Project`, either by referring to one of the standard ones, or by specifying a
way to either launch or attach to the node.

That config file also enumerates the capabilities supported in the `Runtime
Node`, similar to the way code lens and diagnostic sources are currently
configured in the `erlang_ls.comfig` file.  The initial assumption is that there
is a well-known set of supported ones.  At a later stage we may include some
sort of capability query process on starting the `Runtime Node`.

The `Runtime Node` should be able to respond to the following requests.

- List files to be indexed
- Compile a file and return its diagnostics
- run a given test on the test suite
- run elvis diagnostics on a file
- format a file, or portion of a file
- provide xref information for a file
- run dialyzer

### Current Unknowns / Questions

- Should the `Server` and `Runtime Node` share a file system? i.e. should a URI
  be usable directly in any context for the current state of a file.

- What will performance be like? Depending on the above point resolution, we may
  end up pushing file source code over RPC.

- Should the protocol be based on the [Build Server
  Protocol](https://github.com/build-server-protocol/build-server-protocol/blob/master/docs/specification.md)
  but using RPC as the transport?
