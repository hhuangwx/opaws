#!/usr/bin/env python
#
#
import os, sys
import glob
import netCDF4
from optparse import OptionParser

#---------------------------------------------------------------------------------------------------
# Main function defined to return correct sys.exit() calls
#
def main(argv=None):
    if argv is None:
           argv = sys.argv
        
    parser = OptionParser()
    parser.add_option("-i", "--in_dir",  dest="in_dir", type="string", help="Input directory containing Foray sweep files")

    (options, args) = parser.parse_args()

    if options.in_dir == None:
        print
        parser.print_help()
        print
        sys.exit(1)
    
# Get sweep files in directory
    
    files = glob.glob(options.in_dir + "/*.nc")
    
    print "Number of sweep files found:  %d" % len(files)
    if len(files) < 1:
      print "Problem, no sweep files found, exiting...."
      sys.exit(1)
    else:
      print "First file in the directory:  %s" % files[0]
      print "Last  file in the directory:  %s" % files[:-1]
    
    for file in files:
    
      f = netCDF4.Dataset(file, "r+")
      if f.variables['time_offset'][0] > 86000.:
          print "File %s has a bad time_offset of %f, fixing value" % (file, f.variables['time_offset'][0])
          f.variables['time_offset'][0] = f.variables['time_offset'][0] - 86400.
          print "File %s has a bad time_offset of %f, fixing value" % (file, f.variables['time_offset'][0])
      f.close()
      
#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())
    
# End of file
