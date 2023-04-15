# Code scanning with HLint

This is a GitHub action which scans Haskell code using [HLint]
and uploads its suggested improvements to [GitHub code scanning].
It can serve as a status check for pull requests,
with any issues detected being reported.

## Usage

A minimal example for setting up code scanning with HLint:

```yaml
on: [push, pull_request]

jobs:
  scan:
    name: Scan code with HLint
    runs-on: ubuntu-latest
    permissions:
      # Needed to upload results to GitHub code scanning.
      security-events: write
    steps:
      - uses: actions/checkout@v3
      - uses: haskell-actions/hlint-scan@v1
```

### Inputs

None of the inputs are required.
You only need to set them if the defaults do not work for your situation.

#### `binary`

Path to the hlint binary.

#### `path`

Path of file or directory that HLint will be told to scan.
Multiple paths can be specified, delimited by whitespace.

#### `hints`

Path for [HLint configuration file].

#### `category`

[Category] distinguishing multiple analyses at the same commit.

### Outputs

#### `sarif-id`

The ID of the uploaded SARIF file.

## Status

[![Build](https://github.com/haskell-actions/hlint-scan/actions/workflows/build.yaml/badge.svg)](https://github.com/haskell-actions/hlint-scan/actions/workflows/build.yaml)
[![HLint](https://github.com/haskell-actions/hlint-scan/actions/workflows/hlint.yaml/badge.svg)](https://github.com/haskell-actions/hlint-scan/actions/workflows/hlint.yaml)
[![Ormolu](https://github.com/haskell-actions/hlint-scan/actions/workflows/ormolu.yaml/badge.svg)](https://github.com/haskell-actions/hlint-scan/actions/workflows/ormolu.yaml)
[![OpenSSF
Scorecard](https://api.securityscorecards.dev/projects/github.com/haskell-actions/hlint-scan/badge)](https://api.securityscorecards.dev/projects/github.com/haskell-actions/hlint-scan)

## Code of conduct

Be nice; see [`CODE_OF_CONDUCT.md`](docs/CODE_OF_CONDUCT.md) for details.

## Contributing

See [`CONTRIBUTING.md`](docs/CONTRIBUTING.md) for details.

## License

Apache 2.0; see [`LICENSE`](LICENSE) for details.

## Disclaimer

This project is not an official Google project. It is not supported by Google,
and Google specifically disclaims all warranties as to its quality,
merchantability, or fitness for a particular purpose.


[Category]: https://docs.github.com/en/code-security/code-scanning/automatically-scanning-your-code-for-vulnerabilities-and-errors/customizing-code-scanning#configuring-a-category-for-the-analysis

[GitHub code scanning]: https://docs.github.com/en/code-security/code-scanning/automatically-scanning-your-code-for-vulnerabilities-and-errors/about-code-scanning

[HLint]: https://github.com/ndmitchell/hlint

[HLint configuration file]: https://github.com/ndmitchell/hlint#customizing-the-hints

[haskell/actions/hlint-setup]: https://github.com/haskell/actions/tree/main/hlint-setup

[haskell/actions/hlint-run]: https://github.com/haskell/actions/tree/main/hlint-run
