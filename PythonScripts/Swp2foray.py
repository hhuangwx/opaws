#!/usr/bin/env python
#
#
import os, sys
import glob
from optparse import OptionParser

foray_exe = "/opt/local/foray/bin/sweep_trans"

#---------------------------------------------------------------------------------------------------
# Main function defined to return correct sys.exit() calls
#
def main(argv=None):
    if argv is None:
           argv = sys.argv
        
    parser = OptionParser()
    parser.add_option("-i", "--in_dir",  dest="in_dir", type="string", help="Input directory for sweep files")
    parser.add_option("-o", "--out_dir", dest="out_dir", type="string", default=None, help="Directory to create output netcdf, defaults to 'foray_netcdf'")

    (options, args) = parser.parse_args()

    if options.in_dir == None:
        print
        parser.print_help()
        print
        sys.exit(1)
    
    if options.out_dir == None:
        options.out_dir = "foray_netcdf"
        if not os.path.isdir(options.out_dir):
            print "\n%s does not exist, creating directory" % (options.out_dir)
            os.mkdir(options.out_dir)
    else:
        if not os.path.isdir(options.out_dir):
            print "\n%s does not exist, creating directory" % (options.out_dir)
            os.mkdir(options.out_dir)
        
    if not os.path.exists(options.in_dir):
        print "\nError!  Sweep file directory does not seem to exist?"
        print "Filename:  %s" % (options.in_dir)
        sys.exit(1)
    else:
        print "Input directory where sweep files will be read from:  %s" % options.in_dir

    # Get sweep files in directory
    
    files = glob.glob(options.in_dir + "/swp.*")
    
    print "Number of sweep files found:  %d" % len(files)
    if len(files) < 1:
      print "Problem, no sweep files found, exiting...."
      sys.exit(1)
    else:
      print "First file in the directory:  %s" % files[0]
      print "Last  file in the directory:  %s" % files[:-1]
    
    for file in files:
    
      cmd1 = foray_exe + " -d" + file + " -e " + options.out_dir
      print cmd1 + "\n"
      os.system(cmd1)

#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())
    
# End of file
