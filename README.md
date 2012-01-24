# Emacswiki Git Repository

Every time somebody edits a page on the Emacswiki or uploads a file
Oddmuse (Emacswiki's wiki engine) creates a commit using the provided
username and summary.  If this information is not provided default
values are used.  That repository is regularly pushed here.

## History

The master branch containing the full history of all pages and libraries
since the 8th August 2009. The first 800 commits have been created by
importing from the old
[subversion repository](http://www.emacswiki.org/emacs/SVN_repository).
Each of these commits contains all changes of a complete day and have no
meaningful commit messages.

Since the 20th November 2011 this repository is integrated with Oddmuse -
every edit results in a separate commit. The announcement can be found
[here](http://www.emacswiki.org/emacs/2011-11-04).

## Usage

    git clone git://github.com/emacsmirror/emacswiki.org.git emacswiki
    git checkout -b master origin/master

## Package Repositories

Additionally many package are available seperately on the Emacsmirror.
These repositories contains the full history of all files that are part
of a particular package but purges all other history.

These repositories are updated along with other packages mirrored on the
Emacsmirror - a task which is performed about five times a week.
