#!/usr/bin/env python
#
import matplotlib
import pylab as P
import numpy as N
import sys
import netCDF4
from optparse import OptionParser
from netcdftime import utime
import os
import markup

#---------------------------------------------------------------------------------------------------
# Main function defined to return correct sys.exit() calls
#
def main(argv=None):
    if argv is None:
           argv = sys.argv
        
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="file", type="string", default= None, help="Name of netCDF file created from oban analysis")

    (options, args) = parser.parse_args()

    if options.file == None:
        print
        parser.print_help()
        print
        sys.exit(1)
        
    if not os.path.exists(options.file):
        print "\nError!  netCDF file does not seem to exist?"
        print "Filename:  %s" % (options.file)
        sys.exit(1)

    root  = netCDF4.Dataset(options.file, "r")

    d = None
    v = None

    try:
        v     = root.variables['VR']
    except:
        print "\n Cannot find variable VR"

    try:
        v     = root.variables['VT']
    except:
        print "\n Cannot find variable VT"

    try:
        v     = root.variables['VE']
    except:
        print "\n Cannot find variable VE"

    try:
        v     = root.variables['VEL']
    except:
        print "\n Cannot find variable VEL"

    try:
        v     = root.variables['VU']
    except:
        print "\n Cannot find variable VU"

    try:
        d     = root.variables['DZ']
    except:
        print "\n Cannot find variable DZ"

    e     = root.variables['EL']
    a     = root.variables['AZ']
    t     = root.variables['TIME']
    date  = netCDF4.chartostring(root.variables['start_date'][:])
    time  = netCDF4.chartostring(root.variables['start_time'][:])
    date2 = str.replace(date.tostring(),"/","-")
    
    date_string = "seconds since "+ date2[6:10]+"-"+date2[0:5] + " " + time.tostring()
    print
    print "COARDS string from file:  ",date_string
    
    sec_utime = utime(date_string)

    var_shape = t.shape
    
    print
    
    ke = []
    elevation = []
    
    for k in N.arange(0, var_shape[1]):
        el = N.ma.masked_array(e[0,k,:,:], e[0,k,:,:] <= -32767.)
        ti = N.ma.masked_array(t[0,k,:,:], t[0,k,:,:] <= -32767.)
        az = N.ma.masked_array(a[0,k,:,:], a[0,k,:,:] <= -32767.)
        if v != None:
            vv = N.ma.masked_array(v[0,-1,k,:,:], v[0,-1,k,:,:] <= -32767.)
        else:
            vv = N.ones((1))*-999.

        if d != None:
            dd = N.ma.masked_array(d[0,-1,k,:,:], d[0,-1,k,:,:] <= -32767.)
        else:
            dd = N.ones((1))*-999.

        print "Swp: %3d  EL: %04.2f  %04.2f  TIME: %4.2f  %4.2f  AZ: %4.2f  %4.2f  VR: %4.2f  %4.2f  DBZ: %4.2f  %4.2f " % (k,el.max(), el.min(), ti.max(), ti.min(), az.max(), az.min(), vv.max(), vv.min(), dd.max(), dd.min())

#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())
    
# End of file
