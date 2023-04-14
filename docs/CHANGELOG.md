# Changelog for `hlint-scan`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog] and this project adheres to
the [Haskell Package Versioning Policy].

[Haskell Package Versioning Policy]: https://pvp.haskell.org/
[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/

## Unreleased

*   Escape backslashes in messages.
*   Support scanning multiple paths.

## 0.9.0 - 2023-04-14

*   Support `hints` input for explicitly specifying the HLint configuration file.
*   Better message formatting and titles on GitHub.

## 0.4.1 - 2023-04-10

*   Added automated testing.

## 0.4.0 - 2023-04-09

*   Strip "./" from relative file paths.
    *   GitHub can now find files in repositories.

## 0.3.0 - 2023-04-06

*   Use prebuilt Docker image for the Docker composite action.

## 0.2.0.0 - 2023-04-06

*   Rewrite using a Docker container action using Haskell.
    *   It will no longer need HLint to be set up separately,
        although it can use one set up by the user if requested to do so.
    *   GitHub does not understand the locations yet.

## 0.1.0.0 - 2023-04-03

*   Initial working implementation with GitHub composite action.
