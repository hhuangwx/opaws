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
import ctables

output_format = "png"

# ADD the name of the variables you want plotted here.
#
ref_variables = ["DBZ", "DZ"]
vr_variables  = ["DV", "VE", "VR", "VEL", "VU"]

def create_html(directory,files):

    items = tuple(files)
    paras = ( "Quick and dirty output from oban program" )
    images = tuple(files)
    
    page = markup.page( )
    page.init( title="My title", 
    css=( 'one.css', 'two.css' ), 
    header="Objective Analysis of dBZ and Vr from each sweep file") 

# hack in an embedded tag for images - markup.py probably has a way to do this better...

    img_src = []
    for item in files:
        img_src.append("<img src='%s',  width='%s', height='%s'>" % (item,"300","600"))
    
    page.p( paras )
    page.a( tuple(img_src), href=images, title=images)
    
    html_file = open(str(directory)+".html", "w")
    html_file.write(page())
    html_file.close()
    html_file = open(str(directory)+"/"+str(directory)+".html", "w")
    html_file.write(page())
    html_file.close()
    return

def plotoban(ref, vr, x, y, elevation, az, time, pass_no=1, directory=None):

    filename = "%s/%s_%4.2f_pass_%d.png" % ("./"+directory,str.replace(time.isoformat(),"T","_"), elevation, pass_no)
               
#   fig_y_size = 8 * (y.max() - y.min()) / (x.max()-x.min())

    P.figure(figsize=(6,10))
    P.clf()
    P.axes().set_aspect('equal')

    plt = P.subplot(211)
    clevels = N.arange(0,75,5)
    plt = P.contourf(x, y, ref, clevels, cmap=ctables.NWSRef)
    cbar = P.colorbar(plt)
    cbar.ax.set_ylabel('dBZ')
    P.xlabel('X (km)')
    P.ylabel('Y (km)')
    P.title("DBZ Analysis / %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation, pass_no))
    
    plt = P.subplot(212)
    clevels = N.arange(-30,35,5)
    plt = P.contourf(x, y, vr, clevels, cmap=ctables.Not_PosDef_Default)
    cbar = P.colorbar(plt)
    cbar.ax.set_ylabel('Vr m/s')
    P.title("Vr Analysis /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))
    P.xlabel('X (km)')
    P.ylabel('Y (km)')

    P.xlabel('X (km)')
    P.ylabel('Y (km)')

    if output_format == "png": 
        print "\n Saving file %s" % (filename)
        P.savefig(filename, format="png")
        P.close()
    else:
        P.show()
        P.close()

    return filename
#---------------------------------------------------------------------------------------------------
# Main function defined to return correct sys.exit() calls
#
def main(argv=None):
    if argv is None:
           argv = sys.argv
        
    parser = OptionParser()
    parser.add_option("-f", "--file", dest="file", type="string", default= None, \
                                      help="Name of netCDF file created from oban analysis")
    parser.add_option("-d", "--dir",  dest="dir", type="string", \
                                      help="Directory to create output plots in, default is 'obanplots'")
    parser.add_option("-s", "--skip", dest="skip", type="int", default=1, \
                                      help="Skip N sweep files when plotting --> useful for big data sets")
    parser.add_option("-i", "--interactive", dest="interact", default=False, \
                                      help = "Boolean flag to show plots interactively default==False", action="store_true")

    (options, args) = parser.parse_args()

    if options.file == None:
        print
        parser.print_help()
        print
        sys.exit(1)
    
    if options.dir == None:
        options.dir = "obanplots"
        if not os.path.isdir(options.dir):
            print "\n%s does not exist, creating directory" % (options.dir)
            os.mkdir(options.dir)
    else:
        if not os.path.isdir(options.dir):
            print "\n%s does not exist, creating directory" % (options.dir)
            os.mkdir(options.dir)
        
    if not os.path.exists(options.file):
        print "\nError!  netCDF file does not seem to exist?"
        print "Filename:  %s" % (options.file)
        sys.exit(1)

# Decide what backend to use, either PDF (default), or interactive (which uses the users .matplotlib/rcParams file in ~home)

    if options.interact == True:
        output_format = "i"

    ncdf_file  = netCDF4.Dataset(options.file, "r")

# Here try and read in reflectivity data
#
    for item in ref_variables:
        try:
            d = ncdf_file.variables[item][:]
            print "\n ===> Read variable ", item
        except:
            print "\n Cannot find variable ", item

    try:
       print "Shape of reflectivity field:  ", d.shape
    except UnboundLocalError:
       print
       print "**** !!!!ERROR!!!! ****"
       print
       print "No valid Reflectivity field found"
       print "PlotOban looked for the following fields:  ", ref_variables
       print
       print "netCDF file has the following variables:  \n" 
       for item in ncdf_file.variables.keys():  print item 
       print
       print "Please add the name of the file's reflectivity field to the list of ref_variables at top of the PlotOban script"
       print
       sys.exit(1)
# Here try and read in radial velocity data
#
    for item in vr_variables:
        try:
            v = ncdf_file.variables[item][:]
            print "\n ===> Read variable ", item
        except:
            print "\n Cannot find variable ", item

    try:
       print "Shape of radial velocity field:  ", v.shape
    except UnboundLocalError:
       print
       print "**** !!!!ERROR!!!! ****"
       print
       print "No valid Radial Velocity field found"
       print "PlotOban looked for the following fields:  ", vr_variables
       print
       print "netCDF file has the following variables:  \n" 
       for item in ncdf_file.variables.keys():  print item 
       print
       print "Please add the name of the radial velocity field to the list of vr_variables at top of the PlotOban script"
       print
       sys.exit(1)

# Here read in other data needed for plotting - note failure is not an option!
#
    for item in ["EL","AZ","TIME","x","y"]:
        try:
            q = ncdf_file.variables[item][:]
            print "\n ===> Read variable ", item
        except:
            print "\n Cannot find %s variable, exiting " % item
            sys.exit(1)

        if item == "EL":    el = q.copy()
        if item == "AZ":    az = q.copy()
        if item == "TIME":  ti = q.copy()
        if item == "x":     x  = q.copy()
        if item == "y":     y  = q.copy()

    del(q)  # releasing some memory

    date  = netCDF4.chartostring(ncdf_file.variables['start_date'][:])
    time  = netCDF4.chartostring(ncdf_file.variables['start_time'][:])
    date2 = str.replace(date.tostring(),"/","-")
    
    date_string = "seconds since "+ date2[6:10]+"-"+date2[0:5] + " " + time.tostring()
    print
    print "COARDS string from file:  ",date_string
    
    sec_utime = utime(date_string)

    var_shape = v.shape
    
    print
    print "Directory images are stored in is:  ", options.dir
    print "Shape of fields is:  ", var_shape[:]
    print

    files = []
    
    for p in [var_shape[1] - 1]:
        print "Processing pass = ", p
        for k in N.arange(0, var_shape[2], options.skip):
            print "Processing level = ", k, el.shape, ti.shape
            print "Slice: %3d  Sweep elevation:  %4.2f  Sweep time %s " % \
                              (k,el[0,k,:,:].mean(), sec_utime.num2date(ti[0,k,:,:].mean()))
            files.append(plotoban(d[0,p,k], v[0,p,k], x, y, el[0,k].mean(), az[0,k], \
                         sec_utime.num2date(ti[0,k].mean()), pass_no = p+1, directory = options.dir))
        
# Now create the html file for looking at the data

    print
    print "Creating the HTML file for plots in local directory:  %s" % (options.dir+".html")
    create_html(options.dir,files)
    print 
    
#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())
    
# End of file
