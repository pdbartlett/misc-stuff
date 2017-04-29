import os

from cgi import escape
from os.path import join, dirname, splitext
from subprocess import call
from tempfile import mkstemp

import colorize

CALLGESHI_PATH = "C:\\Program Files\\GeSHi\\callGeSHi.php"

class Plugin(object):

    def __init__(self, processor):
        self.processor = processor
        self.extmap = { ".cs"   : "csharp",     \
                        ".java" : "java",       \
                        ".js"   : "javascript", \
                        ".h"    : "cpp",        \
                        ".cpp"  : "cpp",        \
                        ".rb"   : "ruby",       \
                        ".php"  : "php",        \
                        ".py"   : "python",     \
                      }

    def render_src_with_geshi(self, srcpath, lang):
        oshandle, tmppath = mkstemp()
        os.close(oshandle)

        call(["php", CALLGESHI_PATH, srcpath, lang, tmppath])

        handle = open(tmppath, "r")
        txtbuf = handle.read()
        handle.close()
        os.remove(tmppath)

        return txtbuf

    def render_generic_src(self, srcpath):
        srcfile = open(srcpath, "r")
        txtbuf = ""
        for line in srcfile:
            txtbuf += escape(line)
        return txtbuf

    def make_content(self, filepath, uservalues):

        filename = uservalues['source_file']
        content  = '<a href="%s">Download source</a><hr />\n<pre>' % (filename)

        relpath = join(dirname(filepath), uservalues['source_dir'])
        srcpath = join(relpath, filename)
        ext     = splitext(filename)[1]

        if self.extmap.has_key(ext):
            content += self.render_src_with_geshi(srcpath, self.extmap[ext])
        else:
            content += self.render_generic_src(srcpath)

        content += '</pre><hr />'
        return content

    def page(self, filepath, target, restindex, uservalues):
        return { 'codelist' : self.make_content(filepath, uservalues) }

