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

[Hlint](https://github.com/ndmitchell/hlint) should report no issues,
and formatting should be according to [Ormolu](https://github.com/tweag/ormolu).

### Updating versions

When updating the version, these files should be updated accordingly:

*   [`CHANGELOG.md`](CHANGELOG.md)
*   [`package.yaml`](../package.yaml)
*   [`action.yaml`](../action.yaml)

Once that is done, a new [release] should be done with the new version number.
After the release actions are complete, the tag for the major version should be updated.

[release]: https://github.com/haskell-actions/hlint-scan/releases
