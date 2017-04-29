# Imports

from os import chmod, remove, listdir
from os.path import exists, join, isdir, dirname, basename, splitext
from stat import S_IWRITE
from shutil import rmtree, copytree, copy2
from subprocess import call

# Constants

REST2WEB_PATH   = "C:/Program Files/rest2web"
CALLGESHI_PATH  = "C:\\Program Files\\GeSHi\\callGeSHi.php"
CONFIG_FILE     = "Bartletts.ini"

SCREEN_WIDTH = 80

# Helper functions

def print_title(str, ch, both, width):
    if width == 0:
        width = len(str)
    if both:
        print ch * width
    print str
    print ch * width
    
def try_make_writeable(func, path, excInfo):
    if func.__name__ == "remove":
        # make writeable and retry
        chmod(path, S_IWRITE)
        remove(path)
    else:
        raise excInfo
        
def recursive_remove_dir(start, name):
    items = listdir(start)
    for item in items:
        path = join(start, item)
        if isdir(path):
            if item == name:
                rmtree(path, False, try_make_writeable)
            else:
                recursive_remove_dir(path, name)
                
def copy_to_dir(src, dstpath):
    filename = basename(src)
    dst = join(dstpath, filename)
    copy2(src, dst)
        
# Main script
    
print_title("Paul's rest2web site building wrapper", "=", True, SCREEN_WIDTH - 1)

print "Reading config file..."

iniFile = open(CONFIG_FILE, "r")
settings = {}

for line in iniFile:
    trimmed = line.strip()
    if len(trimmed) == 0 or trimmed[0] == "#":
        continue
        
    parts = trimmed.split("=")
    key = parts[0].strip()
    value = parts[1].strip().strip("'")
    
    settings[key] = value

iniFile.close()

srcdir = settings["start_directory"]
dstdir = settings["target_directory"]
resdir = join(srcdir, "res")

sample_subpath  = "projects\\samples"
sample_path     = join(resdir, sample_subpath)
autopath        = join(srcdir, sample_subpath)

print "Updating rest2web samples..."

copy_to_dir("build.py", sample_path)
copy_to_dir(join(REST2WEB_PATH, "rest2web\\plugins\\codelist.py"), sample_path)
copy_to_dir(CALLGESHI_PATH, sample_path)

print "Auto-generating code sample pages..."

existing = listdir(autopath)

for existfile in existing:
    existpath = join(autopath, existfile)
    if isdir(existpath):
        continue
    if splitext(existfile)[1] != ".txt":
        continue
    if existfile == "index.txt":
        continue
    remove(existpath)

files = listdir(sample_path)

for filename in files:
    if isdir(join(sample_path, filename)):
        continue
        
    autoname = filename.replace(".", "_") + ".txt"
    autofile = open(join(autopath, autoname), "w")
    
    autofile.write("restindex\n")
    autofile.write("\tcrumb: %s\n" % (filename))
    autofile.write("\toutput-encoding: utf8\n")
    autofile.write("\ttemplate-encoding: utf8\n")
    autofile.write("\tplugins: codelist\n")
    autofile.write("\tformat: html\n")
    autofile.write("\tpage-title: %s\n" % (filename))
    autofile.write("/restindex\n\n")

    autofile.write("uservalues\n")
    autofile.write("\tsource_dir = ..\\..\\res\\projects\\samples\n")
    autofile.write("\tsource_file = %s\n" % (filename))
    autofile.write("/uservalues\n\n")

    autofile.write("<% codelist %>\n")
    
    autofile.close()
    
print "Copying resources..."

if exists(dstdir):
    rmtree(dstdir, False, try_make_writeable)

copytree(resdir, dstdir)

# remove .svn directories
recursive_remove_dir(dstdir, ".svn")

print_title("Running rest2web...", "-", False, SCREEN_WIDTH - 1)

call(["python", join(REST2WEB_PATH, "rest2web.py"), CONFIG_FILE])
