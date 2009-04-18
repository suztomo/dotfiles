#!/usr/bin/env python
# http://www.emacswiki.org/cgi-bin/wiki/PythonMode
# Before you install this script, you need to install pylint command
# pylint http://www.logilab.org/857

import re
import sys

from subprocess import Popen, PIPE

p = Popen("pylint -f parseable -r n --disable-msg-cat=C,R --indent-string='    ' %s" %
          sys.argv[1], shell = True, stdout = PIPE).stdout

for line in p.readlines():
    match = re.search("\\[([WE])(, (.+?))?\\]", line)
    if match:
        kind = match.group(1)
        func = match.group(3)

        if kind == "W":
            msg = "Warning"
        else:
            msg = "Error"

        if func:
            line = re.sub("\\[([WE])(, (.+?))?\\]",
                          "%s (%s):" % (msg, func), line)
        else:
            line = re.sub("\\[([WE])?\\]", "%s:" % msg, line)
    print line,

p.close()
