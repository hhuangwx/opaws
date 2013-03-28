#!/usr/bin/env python
#
import matplotlib
import pylab as P
import numpy as N
import sys
import netCDF4
from optparse import OptionParser
from netcdftime import utime, num2date
import os
import markup
import ctables
import datetime as DT
from mpl_toolkits.axes_grid.inset_locator import inset_axes
from mpl_toolkits.basemap import Basemap


output_format = "png"

# ADD the name of the variables you want plotted here.
#
ref_variables = ["DBZ", "DZ","REF"]
vr_variables  = ["DV", "VDA", "VE", "VR", "VEL", "VU"]

hscale = 1.0 / 1000.

#===============================================================================
def dll_2_dxy(lat1, lat2, lon1, lon2, degrees=True):
    """dll_2_dxy returns the approximate distance in meters between two lat/lon pairs
       assuming a flat earth approximation (which is sufficient for radar data)

       INPUTS:  in radians

       if lon2 > lon1: x > 0

       if lat2 > lat1:  y > 0

       OUTPUTS:  DX, DY in meters
    """
    rearth = 1000.0 * 6367.0

    if degrees:
      lon1p = N.deg2rad(lon1)
      lon2p = N.deg2rad(lon2)
      lat1p = N.deg2rad(lat1)
      lat2p = N.deg2rad(lat2)
    else:
      lon1p = lon1
      lon1p = lon2
      lat1p = lat1
      lat2p = lat2


    if lon1 < 0.0:
        lon1p = lon1p+2.0*N.pi

    if lon2 < 0.0:
         lon2p = lon2p+2.0*N.pi

    x = rearth * N.cos(0.5*(lat1p+lat2p)) * (lon2p-lon1p)
    y = rearth * (lat2p-lat1p)

    return x, y

#===============================================================================
#
def dxy_2_ll(x, y, lat1, lon1):
    """dxy_2_ll returns the approximate lat/lon between an x,y coordinate and
       a reference lat/lon point.  Assumes a flat earth approximation (which is
       sufficient for radar data) and should be good out to distances of ~200 km.

       INPUTS:  x,y in meters, lat1, lon1 in radians.  Returns degrees

       if x > 0, lon > lon1

       if y > 0, lat > lat1

       OUTPUTS:  lat, lon in radians
    """
    rearth = 1000.0 * 6367.0

    if lon1 < 0.0:
        lon1p = lon1+2.0*N.pi
    else:
        lon1p = lon1

    lat = lat1 + y / rearth
    lon = lon1 + x / ( rearth * N.cos(0.5*(lat1+lat)) )

    lat = N.rad2deg(lat)
    lon = N.rad2deg(lon)
    lon = N.where( lon < 180., lon, 180 - lon)

    return lat, lon

def create_html(directory,files):

    items = tuple(files)
    paras = ( "Quick and dirty output from oban program" )
    images = tuple(files)
    
    page = markup.page( )
    page.init( title="My title", 
    css=( 'one.css' ), 
    header="Objective Analysis of dBZ from each sweep file") 

# hack in an embedded tag for images - markup.py probably has a way to do this better...

    img_src = []
    for item in files:
        img_src.append("<img src='%s',  width='%s', height='%s'>" % (item,"300","300"))
    
    page.p( paras )
    page.a( tuple(img_src), href=images, title=images)
    
    html_file = open(str(directory)+".html", "w")
    html_file.write(page())
    html_file.close()
    html_file = open(str(directory)+"/"+str(directory)+".html", "w")
    html_file.write(page())
    html_file.close()
    return

def plotoban(variable, ref, x, y, elevation, az, time, pass_no=1, directory=None):

    filename = "%s/%s_%4.2f_pass_%d.png" % ("./"+directory,str.replace(time.isoformat(),"T","_"), elevation, pass_no)
               
#   fig_y_size = 8 * (y.max() - y.min()) / (x.max()-x.min())

    P.figure(figsize=(6,10))
    P.clf()
    P.axes().set_aspect('equal')

    plt = P.subplot(111)

    if variable == "DBZ":
        clevels = N.arange(0,75,5)
        plt = P.contourf(x, y, ref, clevels, cmap=ctables.REF_default)
        cbar = P.colorbar(plt)
        cbar.ax.set_ylabel('dBZ')
        P.title("DBZ Analysis / %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation, pass_no))

    if variable == "VR":
        clevels = N.arange(-30,35,5)
        plt = P.contourf(x, y, ref, clevels, cmap=ctables.Not_PosDef_Default)
        cbar = P.colorbar(plt)
        cbar.ax.set_ylabel('Vr m/s')
        P.title("Vr Analysis /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))

    P.xlabel('X (km)')
    P.ylabel('Y (km)')
    P.ylim(y.min(), y.max())
    P.xlim(x.min(), x.max())
    
    if output_format == "png": 
        print "\n Saving file %s" % (filename)
        P.savefig(filename, format="png")
        P.close()
    else:
        P.show()
        P.close()

    return filename

def plotoban_gis(variable, ref, x, y, elevation, az, time, pass_no=1, directory=None, glat=None, glon=None, rlat=None, rlon=None):

    filename = "%s/%s_%4.2f_pass_%d.png" % ("./"+directory,str.replace(time.isoformat(),"T","_"), elevation, pass_no)
               
    fig = P.figure(figsize = (10,10))
    ax  = fig.add_subplot(111)

    xmax = max(x)
    xmin = min(x)
    ymax = max(y)
    ymin = min(y)

    if variable == "DBZ":
        clevels = N.arange(0,75,5)
        plot    = ax.contourf(x, y, ref, clevels, cmap=ctables.REF_default)
        axins   = inset_axes(ax, width="2%", height="35%", loc=3)
        locator = axins.get_axes_locator()
        locator.set_bbox_to_anchor((1.05, 0.05, 1, 1), ax.transAxes)
        locator.borderpad = 0.
        cbar = fig.colorbar(plot, cax=axins)
        cbar.ax.set_ylabel('dBZ')
        ax.set_title("DBZ Analysis / %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation, pass_no),ha='center')

    if variable == "VR":
        clevels = N.arange(-50,55,5)
        plot = ax.contourf(x, y, ref, clevels, cmap=ctables.Not_PosDef_Default)
        axins   = inset_axes(ax, width="2%", height="35%", loc=3)
        locator = axins.get_axes_locator()
        locator.set_bbox_to_anchor((1.05, 0.05, 1, 1), ax.transAxes)
        locator.borderpad = 0.
        cbar = fig.colorbar(plot, cax=axins)
        cbar.ax.set_ylabel('Vr m/s')
        ax.set_title("Vr Analysis /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))

    rdx, rdy = dll_2_dxy(glat, float(rlat), glon, float(rlon), degrees=True)
    plot3    = ax.plot(rdx*hscale, rdy*hscale, marker='o', color='black',markerfacecolor='black', markersize=10)

    ax.set_xlabel('X (km)')
    ax.set_ylabel('Y (km)')

    sw_lat, sw_lon = dxy_2_ll(xmin/hscale,ymin/hscale,N.deg2rad(glat),N.deg2rad(glon))
    ne_lat, ne_lon = dxy_2_ll(xmax/hscale,ymax/hscale,N.deg2rad(glat),N.deg2rad(glon))

# First, set the map up on a grid associated with the lat/lon from the model domain

    map = Basemap(projection='lcc', width=(xmax-xmin)/hscale, height=(ymax-ymin)/hscale, lat_1=sw_lat, lat_2=ne_lat, lat_0=0.5*(ne_lat+sw_lat), lon_0=0.5*(ne_lon+sw_lon), area_thresh=1.,suppress_ticks=True)
    
# Set plot limits

    ax.set_aspect('equal')

    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)

# Shape file stuff

    try:
        shapelist = os.getenv("PYESVIEWER_SHAPEFILES").split(":")

        if len(shapelist) > 0:

            mapaxes1 = inset_axes(ax,width="100%",height="100%",loc=3)       
            mapaxes1.set_axis_off()
            locator1 = mapaxes1.get_axes_locator()
            locator1.set_bbox_to_anchor((0,0,1,1),ax.transAxes)
            locator1.borderpad = 0.0

            for item in shapelist:
                items = item.split(",")
                shapefile  = items[0]
                color      = items[1]
                line_width = items[2]
                print shapefile, color, line_width

                map.readshapefile(shapefile,'GIS_INFO',drawbounds=True,linewidth=line_width,color=color,ax=mapaxes1)

#           map.readshapefile('/Users/wicker/Downloads/ci01jn11/ci01jn11','GIS_INFO',drawbounds=True,linewidth=line_width,color=color,ax=mapaxes1)

    except OSError:
           print "GIS_PLOT:  NO SHAPEFILE ENV VARIABLE FOUND " 
           pass

    if output_format == "png": 
        print "\n Saving file %s" % (filename)
        fig.savefig(filename, format="png")
    else:
        fig.show()

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
    parser.add_option("-g", "--gis",  dest="gis", default=False, \
                                      help = "Boolean flag to use GIS environment variable default==False", action="store_true")

    parser.add_option("-v", "--variable",  dest="variable", default=False, \
                                      help = "Name of field to plot, currently VR or DBZ are valid names")

    (options, args) = parser.parse_args()

    if options.file == None:
        print
        parser.print_help()
        print
        sys.exit(1)
    
    if options.variable == None:
        print
        print "==============>  No variable specified, defaulting to VR"
        print
        variable = "VR"
    else:
        variable = options.variable
    
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
    if variable.upper() == "DBZ":
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
 
# Here try and read in velocity data
#
    if variable.upper() == "VR":
        for item in vr_variables:
            try:
                d = ncdf_file.variables[item][:]
                print "\n ===> Read variable ", item
            except:
                print "\n Cannot find variable ", item

        try:
           print "Shape of radial velocity field:  ", d.shape
        except UnboundLocalError:
           print
           print "**** !!!!ERROR!!!! ****"
           print
           print "No valid radial velocity field found"
           print "PlotOban looked for the following fields:  ", vr_variables
           print
           print "netCDF file has the following variables:  \n" 
           for item in ncdf_file.variables.keys():  print item 
           print
           print "Please add the name of the file's radial velocity field to the list of ref_variables at top of the PlotOban script"
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

    date_string = date2[6:10]+"-"+date2[0:5] + "_" + time.tostring()
    print
    print "COARDS string from file:  seconds since ",date_string
    
    init = DT.datetime.strptime(date_string, '%Y-%m-%d_%H:%M:%S')  # create a datetime object
    
    var_shape = d.shape
    
    print
    print "Directory images are stored in is:  ", options.dir
    print "Shape of fields is:  ", var_shape[:]
    print

    files = []
    
    for p in [var_shape[1] - 1]:
        print "Processing pass = ", p
        for k in N.arange(0, var_shape[2], options.skip):
            azraw  = az[0,k].flatten()
            azmean = azraw[N.isfinite(azraw)].mean()
            elraw  = el[0,k].flatten()
            elmean = elraw[N.isfinite(elraw)].mean()
            tiraw  = ti[0,k].flatten()
            timean = tiraw[N.isfinite(tiraw)].mean()
            stime  = init + DT.timedelta(seconds=int(timean))

            print "Processing SWP: %d  EL:  %f  TIME:  %s " % (k, elmean, stime)

            if options.gis:
              files.append(plotoban_gis(variable,d[0,p,k], x, y, elmean, azmean, \
                           stime, pass_no = p+1, directory = options.dir, \
                           glat=ncdf_file.variables['grid_latitude'][0],  glon=ncdf_file.variables['grid_longitude'][0], \
                           rlat=ncdf_file.variables['radar_latitude'][0], rlon=ncdf_file.variables['radar_longitude'][0]))
            else:
              files.append(plotoban(variable,d[0,p,k], x, y, elmean, azmean, \
                           stime, pass_no = p+1, directory = options.dir))
        
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
