# How to Contribute

We would love to accept your patches and contributions to this project.

## Before you begin

### Sign our Contributor License Agreement

Contributions to this project must be accompanied by a
[Contributor License Agreement](https://cla.developers.google.com/about) (CLA).
You (or your employer) retain the copyright to your contribution; this simply
gives us permission to use and redistribute your contributions as part of the
project.

If you or your current employer have already signed the Google CLA (even if it
was for a different project), you probably don't need to do it again.

Visit <https://cla.developers.google.com/> to see your current agreements or to
sign a new one.

### Review our Community Guidelines

This project follows [Google's Open Source Community
Guidelines](https://opensource.google/conduct/).

## Contribution process

### Code Reviews

All submissions, including submissions by project members, require review. We 
use [GitHub pull requests](https://docs.github.com/articles/about-pull-requests)
for this purpose.

### Coding standards

[HLint](https://github.com/ndmitchell/hlint) should report no issues,
and formatting should be according to [Ormolu](https://github.com/tweag/ormolu).

Changes to code should include corresponding tests, which should ideally be property-based.
This project uses [Hspec] and [QuickCheck] for testing.

[Hspec]: https://hspec.github.io/
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck

### Updating versions

When updating the version, these files should be updated accordingly:

*   [`CHANGELOG.md`](CHANGELOG.md) with user-visible changes.
*   [`package.yaml`](../package.yaml) with the new version.
*   [`action.yaml`](../action.yaml) with the new version for the Docker image.

Once that is done, a [release] should be done with the new version number.
After the release actions are complete, the tag for the major version should be updated.
Do *not* delete any `hlint-scan` packages, as it may render previous versions inaccessible.

[release]: https://github.com/haskell-actions/hlint-scan/releases
