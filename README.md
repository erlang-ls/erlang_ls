# erlang_ls

![erlang_ls](images/erlang-ls-logo-small.png?raw=true "Erlang LS")

![Build](https://github.com/erlang-ls/erlang_ls/workflows/Build/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/erlang-ls/erlang_ls/badge.svg?branch=main)](https://coveralls.io/github/erlang-ls/erlang_ls?branch=main)

An Erlang server implementing Microsoft's Language Server Protocol 3.15.

## Minimum Requirements

* [Erlang OTP 22+](https://github.com/erlang/otp)
* [rebar3 3.9.1+](https://github.com/erlang/rebar3)

## Quickstart

Compile the project:

    make

To install the produced `erlang_ls` escript in `/usr/local/bin`:

    make install

To install to a different directory set the `PREFIX` environment variable:

    PREFIX=/path/to/directory make -e install

## Command-line Arguments

These are the command-line arguments that can be provided to the
`erlang_ls` escript:

``` shell
Usage: Erlang LS [-v] [-t [<transport>]] [-p [<port>]] [-d [<log_dir>]]
                 [-l [<log_level>]]

  -v, --version    Print the current version of Erlang LS
  -t, --transport  Specifies the transport the server will use for the
                   connection with the client, either "tcp" or "stdio".
                   [default: tcp]
  -p, --port       Used when the transport is tcp. [default: 10000]
  -d, --log-dir    Directory where logs will be written.
                   [default: filename:basedir(user_log, "erlang_ls")]
  -l, --log-level  The log level that should be used. [default: info]
```

## Configuration

It is possible to customize the behaviour of the `erlang_ls` server
via a configuration file, named `erlang_ls.config` or `erlang_ls.yaml`.
That file should be placed in the root directory of a
given project to store the configuration for that project. It is also
possible to store a system-wide default configuration, which is shared
across multiple projects.

Please refer to the
[configuration](https://erlang-ls.github.io/configuration) section of
the documentation to learn how to configure Erlang LS for a specific
project.

## Getting in Touch

If you have any questions about the project, feel free to open a new
issue. You can also join the `#erlang-ls` channel in the
_Erlanger_ Slack if you would like to get involved or if you prefer a
more informal mean of communication.

All contributions are welcome, be them in the form of a bug report, a
question, feedback, or code.

I occasionally blog about the project on
[Medium](https://medium.com/about-erlang).

## References

https://microsoft.github.io/language-server-protocol/

## License

The `erlang_ls` project is licensed under the Apache License 2.0. Please refer
to the `LICENSE` file for details.
