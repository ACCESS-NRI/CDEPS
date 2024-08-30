# ACCESS-NRI fork of CDEPS

This is the [ACCESS-NRI](https://www.access-nri.org.au) fork of the [Community Data models for Earth Prediction Systems](https://escomp.github.io/CDEPS/versions/master/html/index.html)

The canonical CDEPS repository is here: https://github.com/ESCOMP/CDEPS

## Fork management

ACCESS-NRI maintains this CDEPS fork to carry local changes and (in the future) enable and manage continuous integration
testing and release management for ACCESS-NRI support models. It is not envisaged that the code-base will diverge, so
there is a process required to keep this fork up to date with changes in the canonical upsteam CDEPS repository.

The `main` branch of the canonical CDEPS repository is tracked via the [`upstream-main` branch](https://github.com/ACCESS-NRI/CDEPS/tree/upstream-main). To incorporate changes into `main` from the upstream `ESCOMP/CDEPS:main` branch:
1. [Sync](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/syncing-a-fork) the `upstream-main` branch with `ESCOMP/CDEPS:main`.
1. Submit a pull request to to `main` from `upstream-main`.
1. Review and fix any conflicts and then merge the PR.

-----------

# CDEPS
Community Data Models for Earth Prediction Systems

For documentation see

https://escomp.github.io/CDEPS/versions/master/html/index.html

## A note on github tag action

This repository is setup to automatically create tags on merge to
main using .github/workflows/bumpversion.yml It uses
https://github.com/mathieudutour/github-tag-action  to look for
keywords in commit messages and determine what the new version should
be.  The default if no keywords is found is to bump the patch version.
