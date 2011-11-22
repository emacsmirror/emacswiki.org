# Emacswiki Git Repository

Every change made on the Emacswiki is commited individually (with the
username and summary being used as author and commit message) to a git
repository on the server hosting the Emacswiki.  That repository is
regularly pushed here.

While the [EmacsWiki](http://www.emacswiki.org)
and the [Emacsmirror](https://github.com/emacsmirror)
are separate projects this repository can be considered part of both.

## Package Branches and Repositories

Additionally each package is split into a separate branch (pkg/NAME).
Such a branch contains the full history of all files that are part of a
particular package but purges all other history. 

Most of these package branches are also available as individual
repositories at http://github.com/emacsmirror/NAME.

These branches and repositories are updated along with other packages
mirrored on the Emacsmirror - a task which is performed about five
times a week.

## Usage

If you want all branches including the hundreds of package branches:

    git clone git://github.com/emacsmirror/emacswiki.org.git emacswiki
    git checkout -b master origin/master

If you want just the master branch:

    git init emacswiki
    cd emacswiki
    git remote add origin git://github.com/emacsmirror/emacswiki.org.git
    git config remote.origin.fetch '+refs/heads/master:refs/remotes/origin/master'
    git fetch origin
    git checkout -b master origin/master

## History

The master branch containing the full history of all pages and libraries
since the 8th August 2009. The first 800 commits have been created by
importing from the
[SVN_repository](http://www.emacswiki.org/emacs/SVN_repository).
Each of these commits contains all changes of a complete day and have no
meaningful commit messages.

Since the 20th November 2011 this repository is integrated with Oddmuse -
every edit results in a separate commit. The announcement can be found
[here](http://www.emacswiki.org/emacs/2011-11-04).
