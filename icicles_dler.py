<pre>
#!/usr/bin/env python

from __future__ import with_statement
import os
import os.path
import time
import urllib2
from datetime import datetime, timedelta

INSTALL_DIRECTORY = '~/.emacs.d/site-lisp/icicles/'
DOWNLOAD_DIRECTORY = 'http://www.emacswiki.org/emacs/download/'

ICICLES_FILES = (
    'icicles.el',
    'icicles-chg.el',
    'icicles-cmd1.el',
    'icicles-cmd2.el',
    'icicles-doc1.el',
    'icicles-doc2.el',
    'icicles-face.el',
    'icicles-fn.el',
    'icicles-mac.el',
    'icicles-mcmd.el',
    'icicles-mode.el',
    'icicles-opt.el',
    'icicles-var.el')

def parse_dt(line):
    return datetime.strptime(line[17:-8], '%c')-int(line[-6]+'1')*timedelta(hours=int(line[-5:-3]), minutes=int(line[-3:-1]))

def run():
    inst_dir = os.path.expanduser('~/.emacs.d/site-lisp/icicles/')
    if not os.path.exists(inst_dir):
        os.makedirs(inst_dir)

    for filename in ICICLES_FILES:
        print "downloading file: %s" % (DOWNLOAD_DIRECTORY+filename)

        success = False
        for i in xrange(1, 4):
            try:
                if i > 1:
                    print 'trying again, attempt #%s...' % i
                response = urllib2.urlopen(DOWNLOAD_DIRECTORY+filename)
            except urllib2.HTTPError, e:
                sleep_time = 5+5*i
                print 'HTTPError thrown, trying again after waiting %s seconds, error was %s' % (sleep_time, e)
                time.sleep(sleep_time)
            else:
                success = True
                break
        assert success, 'download failed!'

        content_lines = response.readlines()

        found = False
        for line in content_lines:
            line = line.strip()
            if line.startswith(';; Last-Updated: '):
                new_modified_dt = parse_dt(line)
                found = True
                break
        assert found, 'Last updated not found in file %s' % filename

        local_modified_dt = datetime.min
        full_file_path = inst_dir+filename
        if os.path.isfile(full_file_path):
            with open(full_file_path, 'r') as f:
                found = False
                for line in f:
                    line = line.strip()
                    if line.startswith(';; Last-Updated: '):
                        local_modified_dt = parse_dt(line)
                        found = True
                        break
                assert found, 'Last updated not found in file %s' % filename

        print 'local: %s, remote: %s' % (local_modified_dt, new_modified_dt)
        if local_modified_dt < new_modified_dt:
            print 'updating...',
            with open(full_file_path, 'w') as f:
                f.writelines(content_lines)
            print 'done.'

if __name__ == '__main__':
    run()
</pre>
