# Changelog for `hotel-california`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- [#19](https://github.com/parsonsmatt/hotel-california/pull/19/)
    - Record the command's exit status (`process.exit_status`) and command
      attributes.
    - This replaces the default `process.executable.name` and related attributes
      set by `hs-opentelemetry`.

## 0.0.4.0 - 2024-01-26

- [#14](https://github.com/parsonsmatt/hotel-california/pull/14/)
    - Add the `--set-sigint-status` to control how `Ctrl-C` and `SIGINT` are
      reported.
- [#16](https://github.com/parsonsmatt/hotel-california/pull/16)
    - You can now provide attributes for the span that `hotel-california`
      creates by passing the CLI argument `--attribute KEY=VALUE`. Only string
      attributes are currently supported.

## 0.0.3.0 - 2023-09-18

- [#13](https://github.com/parsonsmatt/hotel-california/pull/13)
    - The `hotel` command will now propagate the `BAGGAGE` environment variable,
      according to the [W3C working draft](https://www.w3.org/TR/baggage/),
      similar how it already propagates `TRACEPARENT` and `TRACESTATE`.

## 0.0.2.0 - 2023-09-15

- [#7](https://github.com/parsonsmatt/hotel-california/pull/7)
    - Fixed escaping of shell commands. Previously, single quotes would not be
      passed correctly, so a command like this:
        ```
        hotel exec -- cabal build --ghc-options='-Werror -ferror-spans'
        ```
      would get passed like `cabal ["build", "--ghc-options='-Werror'", "-ferror-spans'"]`.
      This is now fixed, and it will be properly passed as `cabal ["build", "--ghc-options='-Werror -ferrorspans'"]`.
    - CLI interface changed subtly - now, `hotel exec command [args]` will do a
      process lookup for `command`. If you want to pass a shell script, instead
      do:
        ```
        hotel exec --shell 'echo a && echo b || true'
        ```

## 0.0.1.0 - 2023-09-12

- Initial Release
- Introduce `exec` functionality
