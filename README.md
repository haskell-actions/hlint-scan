# Code scanning with HLint

This is a GitHub action which scans Haskell code using [HLint]
and uploads its suggested improvements to [GitHub code scanning].

This needs HLint to be set up.
This can be taken care of by [haskell/actions/hlint-setup].

**Warning**: This depends on unreleased versions of HLint,
and the location of this action will likely change in the future.
Neither has the action itself been officially released.

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
      - uses: haskell/actions/hlint-setup@v2
      - uses: chungyc/hlint-scan@main
```

### Inputs

None of the inputs are required.
You only need to set them if the defaults do not work for your situation.

`hlint-bin`
:   Path to the hlint binary.

`args`
:   Extra arguments to pass to HLint.

`path`
:   Path or array of paths that HLint will be told to scan.

`sarif_file`
:   The name of the SARIF file to write and upload to GitHub code scanning.

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

This action has not hardened security with its inputs yet.
Do *not* use this action in a situation where uncontrolled input can be passed to it.
E.g., another action which would pass its input to this one.

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
