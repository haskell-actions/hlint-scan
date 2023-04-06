# Code scanning with HLint

This is a GitHub action which scans Haskell code using [HLint]
and uploads its suggested improvements to [GitHub code scanning].

## Usage

A minimal example for setting up code scanning with HLint:

```yaml
on:
  push:
    branches: ['main']

jobs:
  scan:
    name: Scan code with HLint
    runs-on: ubuntu-latest
    permissions:
      # Needed to upload results to GitHub code scanning.
      security-events: write
    steps:
      - uses: actions/checkout@v3
      - uses: haskell-actions/hlint-scan@v0
```

### Inputs

None of the inputs are required.
You only need to set them if the defaults do not work for your situation.

`binary`
:   Path to the hlint binary.

`path`
:   Path of file or directory that HLint will be told to scan.

`category`
:   String used by GitHub code scanning for matching the analyses.

### Outputs

`sarif-id`
:   The ID of the uploaded SARIF file.

### Note

This does not fail the workflow when HLint finds any code which could be improved.
In other words, this action is not intended to be used as a status check.
Instead, its goal is to file [GitHub code scanning] alerts.
To use HLint for status checks, e.g., during pushes or pull requests,
see [haskell/actions/hlint-run] instead.

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


[GitHub code scanning]: https://docs.github.com/en/code-security/code-scanning/automatically-scanning-your-code-for-vulnerabilities-and-errors/about-code-scanning

[HLint]: https://github.com/ndmitchell/hlint

[haskell/actions/hlint-setup]: https://github.com/haskell/actions/tree/main/hlint-setup

[haskell/actions/hlint-run]: https://github.com/haskell/actions/tree/main/hlint-run
