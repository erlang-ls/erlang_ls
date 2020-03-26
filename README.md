# erlang_ls

![erlang_ls](images/erlang-ls-logo-small.png?raw=true "Erlang LS")

[![Build Status](https://travis-ci.org/erlang-ls/erlang_ls.svg?branch=master)](https://travis-ci.org/erlang-ls/erlang_ls)
[![Coverage Status](https://coveralls.io/repos/github/erlang-ls/erlang_ls/badge.svg?branch=master)](https://coveralls.io/github/erlang-ls/erlang_ls?branch=master)

An Erlang server implementing Microsoft's Language Server Protocol 3.15.

## Minimum Requirements

* Erlang OTP 21+
* rebar3 3.9.1+

## Quickstart

Compile the project:

    make

## Command-line Arguments

These are the command-line arguments that can be provided to the
`erlang_ls` escript:

``` shell
Usage: Erlang LS [-v] [-t [<transport>]] [-p [<port>]] [-d [<log_dir>]]
                 [-l [<log_level>]] [<port_old>]

  -v, --version    Print the current version of Erlang LS
  -t, --transport  Specifies the transport the server will use for the
                   connection with the client, either "tcp" or "stdio".
                   [default: tcp]
  -p, --port       Used when the transport is tcp. [default: 10000]
  -d, --log-dir    Directory where logs will be written.
                   [default: filename:basedir(user_log, "erlang_ls")]
  -l, --log-level  The log level that should be used. [default: info]
```

## Customization

It is possible to customize the behaviour of the `erlang_ls` server
via a configuration file, named `erlang_ls.config`. The
`erlang_ls.config` file should be placed in the root directory of a
given project to store the configuration for that project.

It is also possible to store a system-wide default configuration in an
`erlang_ls.config` file located in the _User Config_ directory. The
exact location of the _User Config_ directory depends on the operating
system used and it can be identified by executing the following
command on an Erlang shell:

    > filename:basedir(user_config, "erlang_ls").

Normally, the location of the _User Config_ directory is:

| Operating System | User Config Directory                               |
|------------------|-----------------------------------------------------|
| Linux            | /home/USER/.config/erlang\_ls                       |
| OS X             | /Users/USER/Library/Application\ Support/erlang\_ls |
| Windows          | c:/Users/USER/AppData/Local/erlang\_ls              |

Thus on Linux, for example, the full path to the default configuation file
would be `/home/USER/.config/erlang_ls/erlang_ls.config`

A sample `erlang_ls.config` file would look like the following:

```yaml
otp_path: "/path/to/otp/lib/erlang"
deps_dirs:
  - "lib/*"
include_dirs:
  - "include"
  - "_build/default/lib"
macros:
  - name: DEFINED_WITH_VALUE
    value: 42
  - name: DEFINED_WITHOUT_VALUE
code_reload:
  node: node@example
```

The file format is `yaml`.

The following customizations are possible:

| Parameter          | Description                                                                                                                               |
|--------------------|-------------------------------------------------------------------------------------------------------------------------------------------|
| otp\_path          | Path to the OTP installation                                                                                                              |
| plt\_path          | Path to the dialyzer PLT file. When none is provided the dialyzer diagnostics will not be available.                                      |
| deps\_dirs         | List of directories containing dependencies. It supports wildcards.                                                                       |
| apps\_dirs         | List of directories containing project applications. It supports wildcards.                                                               |
| include\_dirs      | List of directories provided to the compiler as include dirs. It supports wildcards.                                                      |
| macros             | List of cusom macros to be passed to the compiler, expressed as a name/value pair. If the value is omitted or is invalid, 'true' is used. |
| otp\_apps\_exclude | List of OTP applications that will not be indexed (default: megaco, diameter, snmp, wx)                                                   |
| code\_reload       | Whether or not an rpc call should be made to a remote node to compile and reload a module                                                 |

The `code_reload` takes the following options:
| Parameter | Description                                                          |
|-----------|----------------------------------------------------------------------|
| node      | The node to be called for code reloading. Example erlang_ls@hostname |

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
