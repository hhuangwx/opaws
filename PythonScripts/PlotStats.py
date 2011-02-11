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

output_format = "png"

def my_colormaps(name="jet"):

      if name == "dbz_15_75":
      
             return[(0.000,0.000,0.804), \
                    (0.482,0.988,0.000), \
                    (0.000,0.933,0.000), \
                    (0.000,0.545,0.000), \
                    (1.000,1.000,0.000), \
                    (0.726,0.525,0.043), \
                    (1.000,0.549,0.000), \
                    (1.000,0.000,0.000), \
                    (0.804,0.000,0.000), \
                    (0.549,0.000,0.000), \
                    (0.933,0.070,0.537), \
                    (0.604,0.196,0.078) ]
      
      if name == "dbz_0_75":
            return [ (0.000,1.000,1.000), \
                     (0.118,0.566,1.000), \
                     (0.000,0.000,0.804), \
                     (0.482,0.988,0.000), \
                     (0.000,0.933,0.000), \
                     (0.000,0.545,0.000), \
                     (1.000,1.000,0.000), \
                     (0.726,0.525,0.043), \
                     (1.000,0.549,0.000), \
                     (1.000,0.000,0.000), \
                     (0.804,0.000,0.000), \
                     (0.549,0.000,0.000), \
                     (0.933,0.070,0.537), \
                     (0.604,0.196,0.078) ]
                             
      if name == "jet":
                  return "jet"
                  
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

def plotoban(d, v, y, x, elevation, az, time, pass_no=1, directory=None):

    filename = "%s/%s_%4.2f_pass_%d.png" % ("./"+directory,str.replace(time.isoformat(),"T","_"), elevation, pass_no)
               
#   fig_y_size = 8 * (y.max() - y.min()) / (x.max()-x.min())

    P.figure(figsize=(6,10))
    P.clf()
    P.axes().set_aspect('equal')

    plt = P.subplot(211)
    clevels = N.arange(0,75,5)
    plt = P.contourf(y, x, d, clevels, colors=my_colormaps("dbz_0_75"))
    cbar = P.colorbar(plt)
    cbar.ax.set_ylabel('dBZ')
    P.xlabel('X (km)')
    P.ylabel('Y (km)')
    P.title("DBZ Analysis / %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation, pass_no))
    
    plt = P.subplot(212)
    clevels = N.arange(-30,35,5)
    plt = P.contourf(y, x, v, clevels)
    cbar = P.colorbar(plt)
    cbar.ax.set_ylabel('Vr m/s')
    P.title("Vr Analysis /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))
    P.xlabel('X (km)')
    P.ylabel('Y (km)')

#   dvdy, dvdx = N.gradient(v)
#   
#   angle = N.radians(az)
#   
#   radial_convergence = - N.sin(angle) * dvdx - N.cos(angle) * dvdy
#   
#   azimuthal_shear    = N.sin(angle) * dvdy + N.cos(angle) * dvdx
#   
#   print "Unscaled    azimuthal shear max:  %f       azimuthal shear min:  %f  (m/s)" % (azimuthal_shear.max(), azimuthal_shear.min())
#   print "Unscaled radial convergence max:  %f    radial convergence min:  %f  (m/s)" % (radial_convergence.max(), radial_convergence.min())
#       
#   plt = P.subplot(223)
#   clevels = N.arange(-20.0,20.2,0.2)
#   plt = P.contourf(y, x, azimuthal_shear,clevels)
#   cbar = P.colorbar(plt)
#   cbar.ax.set_ylabel('Azimuthal Shear m/s/dtheta')
#   P.title("Az Shear /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))
#
#   P.xlabel('X (km)')
#   P.ylabel('Y (km)')
#   
#   plt = P.subplot(224)
#   clevels = N.arange(-20.0,20.2,0.2)
#   plt = P.contourf(y, x, radial_convergence, clevels)
#   cbar = P.colorbar(plt)
#   cbar.ax.set_ylabel('Radial Convergence m/s/dr')
#   P.title("Radial Conv /  %s   / %4.2f deg P=%d" % (str.replace(time.isoformat(),"T","_"),elevation,pass_no))

    P.xlabel('X (km)')
    P.ylabel('Y (km)')

    P.ylim(x.min(),x.max())
    P.xlim(y.min(),y.max())

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
    parser.add_option("-f", "--file", dest="file", type="string", default= None, help="Name of netCDF file created from oban analysis")
    parser.add_option("-s", "--skip", dest="skip", type="int", default=1, help="Skip N sweep files when plotting --> useful for big data sets")
    parser.add_option("-i", "--interactive", dest="interact", default=False,  help = "Boolean flag to show plots interactively default==False", action="store_true")

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

# Decide what backend to use, either PDF (default), or interactive (which uses the users .matplotlib/rcParams file in ~home)

    if options.interact == True:
        output_format = "i"

    root  = netCDF4.Dataset(options.file, "r")

    x     = root.variables['x'][:]
    y     = root.variables['y'][:]

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

    var_shape = v.shape
    
    print
    
    ke = []
    elevation = []
    
    for p in [var_shape[1]-1]:
        print p
        for k in N.arange(0, var_shape[2], options.skip):
            el = N.ma.masked_array(e[0,k,:,:], e[0,k,:,:] <= -32767.)
            el.mean()
            ti = N.ma.masked_array(t[0,k,:,:], t[0,k,:,:] <= -32767.)
            az = N.ma.masked_array(a[0,k,:,:],   v[0,p,k,:,:] <= -32767.)
            vv = N.ma.masked_array(v[0,p,k,:,:], v[0,p,k,:,:] <= -32767.)
            print "Slice: %3d  Sweep elevation:  %4.2f  Sweep time %s " % (k,el.mean(), sec_utime.num2date(ti.mean()))

            tmp = N.ravel(N.where(vv.mask != False))
            tmp = tmp ** 2.0
          
            print
            print "Mean kinetic Energy:  ", 0.5*tmp.mean()

            ke.append(0.5*tmp.mean())
            elevation.append(el.mean())

    matplotlib.rcParams['axes.unicode_minus'] = False
    fig = P.figure()
    ax  = fig.add_subplot(111)
    ax.plot(ke, elevation, 'o')
    ax.set_title('Kinetic Energy versus Elevation')
    P.xlabel('KE (m^2s^-1)')
    P.ylabel('Elevation (deg)')

    P.ylim(0.0,25)
    P.xlim(400.,1300.)

    P.show()
            
#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())
    
# End of file
