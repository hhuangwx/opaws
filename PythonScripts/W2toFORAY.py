#!/usr/bin/env python
#
#  W2toFORAY.py
#  
#
#  Created by Terra Thompson on 2/23/10.
#  Modified for 88D data that has been dealiased. 6/27/11
#
#  Modifications by LJW June/July 2010

import numpy as N
import sys
import time
import pylab as P
from optparse import OptionParser
import glob
from netCDF4 import *

#####################################################################################################
def W2toFORAYmain():

    stime = time.time()
    starttime = time.gmtime(stime)
    print
    print "  ------------- START TIME: -------------"
    print "                           ", starttime[3], starttime[4], starttime[5]
    print "  ---------------------------------------"
    print
    
    
    parser = OptionParser()
    parser.add_option("-p", "--path",       dest="path",        type="string", help = "Directory of the WDSSII radar files (aka, AliasVelocity, Reflectivity)")
    parser.add_option("-v", "--variable",   dest="variable",    type="string", help = "Reflectivity or Velocity")
    parser.add_option("-d", "--directory",  dest="directory",   type="string", help = "Directory to place the converted OPAWS foray files")
    (options, args) = parser.parse_args()

# path, where the netcdf input radar data is located
    if options.path == None:
        print
        parser.print_help()
        print
        sys.exit(0)
    else:
        mypath = options.path
    
# variable
    if options.variable == None:
        print "Variable not set, defaulting to Reflectivity"
        var = "Reflectivity"
    else:
        var = options.variable

# directory
    if options.directory == None:
        print "No directory specified - files will be placed in a directory 'foray'"
        directory = "foray"
    else:
        directory = options.directory

    if os.path.isdir(directory) == False:
        print "\nirectory does not exist, creating it...\n"
        os.mkdir(directory)
    else:
        print "\nDirectory exists, files are probably going to be overwritten!!! \n"

    datapath = os.path.join(options.path, "*/*.netcdf")

    print
    
    # for running on dir of files
    files =  glob(datapath) 


#------------------------------------------------------
    for field in files:
        
        # open the input files for reading
        # note the input files are NETCDF3_CLASSIC format
        f_in = Dataset(field, 'r')
        
        # make the output file name
        # this only works if the input data is /el/YYYYMMDD-HHmmSS.netcdf
        outname = directory + "/foray." + field[-22:-6]  + field[-28:-26] + "_" + field[-25:-23] + ".nc"
        print outname
        
        # get number of gates (needed to set output dimensions)
        g_size = f_in.dimensions['Gate']
        ng = len(g_size)
        
        
        # create the output file 
        out = Dataset(outname, 'w', format='NETCDF3_CLASSIC')
        
        # ouput dimensions
        out.createDimension('Time', None)
        out.createDimension('maxCells', ng)
        out.createDimension('numSystems', 1)
        out.createDimension('fields', 1)
        out.createDimension('short_string', 32)
        out.createDimension('long_string', 80)
        
        
        # output variables
        V_volume_start_time = out.createVariable('volume_start_time', 'i4')
        V_volume_start_time.long_name = "Unix Date/Time value for volume start time" ;
        V_volume_start_time.units = "seconds since 1970-01-01 00:00 UTC"
        
        V_Range_to_First_Cell = out.createVariable('Range_to_First_Cell', 'f4', fill_value='-9999.')
        V_Range_to_First_Cell.long_name = "Range to the center of the first cell" 
        V_Range_to_First_Cell.units = "meters" 
        V_Range_to_First_Cell.missing_value = -9999.

        V_Cell_Spacing_Method = out.createVariable('Cell_Spacing_Method', 'i4', fill_value='-9999')
        V_Cell_Spacing_Method.long_name = "Technique for recording cell spacing: 0 = by vector, 1 = by segment" 
        V_Cell_Spacing_Method.missing_value = -9999 ;

        V_Cell_Distance_Vector = out.createVariable('Cell_Distance_Vector', 'f4', ('maxCells',), fill_value='-9999.')
        V_Cell_Distance_Vector.long_name = "Vector of cell distances from radar." 
        V_Cell_Distance_Vector.units = "meters"  
        V_Cell_Distance_Vector.missing_value = -9999. 
        
        V_Nyquist_Velocity = out.createVariable('Nyquist_Velocity', 'f4', fill_value='-9999.')
        V_Nyquist_Velocity.long_name = "Effective unambigous velocity" 
        V_Nyquist_Velocity.units = "meters/second" 
        V_Nyquist_Velocity.missing_value = -9999.
        
        V_Unambiguous_Range = out.createVariable('Unambiguous_Range', 'f4', fill_value='-9999.')
        V_Unambiguous_Range.long_name = "Effective unambigous range" 
        V_Unambiguous_Range.units = "meters"
        V_Unambiguous_Range.missing_value = -9999.
        
        V_Latitude = out.createVariable('Latitude', 'f8', fill_value='-9999.')
        V_Latitude.long_name = "Latitude of the instrument" 
        V_Latitude.units = "degrees"
        V_Latitude.missing_value = -9999.
        V_Latitude.valid_range = -90., 90.

        V_Longitude = out.createVariable('Longitude', 'f8', fill_value='-9999.')
        V_Longitude.long_name = "Longitude of the instrument" 
        V_Longitude.units = "degrees" 
        V_Longitude.missing_value = -9999. 
        V_Longitude.valid_range = -360., 360.
        
        V_Altitude = out.createVariable('Altitude', 'f8', fill_value='-9999.')
        V_Altitude.long_name = "Altitude in meters (asl) of the instrument" 
        V_Altitude.units = "meters" 
        V_Altitude.missing_value = -9999. 
        V_Altitude.valid_range = -10000., 90000. 
        
        V_Radar_Constant = out.createVariable('Radar_Constant', 'f4', ('numSystems',), fill_value='-9999.')
        V_Radar_Constant.long_name = "Radar Constant" 
        V_Radar_Constant.units = "mm6/(m3.mW.km-2)" 
        V_Radar_Constant.missing_value = -9999.
        
        V_bm_width = out.createVariable('bm_width', 'f4', ('Time',), fill_value='-9999.')
        V_bm_width.long_name = "Beam Width" 
        V_bm_width.units = "degrees"
        V_bm_width.missing_value = -9999.
        
        V_time_offset = out.createVariable('time_offset', 'f8', ('Time',), fill_value='0.')
        V_time_offset.long_name = "time offset of the current record from base_time" 
        V_time_offset.units = "seconds"
        V_time_offset.missing_value = 0. 

        V_Azimuth = out.createVariable('Azimuth', 'f4', ('Time',), fill_value='-9999.')
        V_Azimuth.long_name = "Earth relative azimuth of the ray" 
        V_Azimuth.Comment = "Degrees clockwise from true North" 
        V_Azimuth.units = "degrees" 
        V_Azimuth.valid_range = -360., 360. 
        V_Azimuth.missing_value = -9999.
        
        V_Elevation = out.createVariable('Elevation', 'f4', ('Time',), fill_value='-9999.')
        V_Elevation.long_name = "Earth relative elevation of the ray" 
        V_Elevation.Comment = "Degrees from earth tangent towards zenith" 
        V_Elevation.units = "degrees" 
        V_Elevation.valid_range = -360., 360.
        V_Elevation.missing_value = -9999.
        
        if var == "Reflectivity" or var == "ReflectivityPRF2":
            V_DBZ = out.createVariable('DBZ', 'i2', ('Time', 'maxCells',), fill_value='-32767') 
            V_DBZ.long_name = "Reflectivity"
            V_DBZ.units = "unknown" 
            V_DBZ.scale_factor = 0.01
            V_DBZ.add_offset = -0. 
            V_DBZ.missing_value = -32767
            V_DBZ.polarization = "Unknown" 
            V_DBZ.Frequencies_GHz = 0. ;
            V_DBZ.InterPulsePeriods_secs = -32768. 
            V_DBZ.meters_to_first_cell = 0.
        elif var == "Velocity":
            V_VEL = out.createVariable('VEL', 'i2', ('Time', 'maxCells',), fill_value='-32767') 
            V_VEL.long_name =  "Velocity"
            V_VEL.units = "unknown" 
            V_VEL.scale_factor = 0.01
            V_VEL.add_offset = -0.
            V_VEL.missing_value = -32767
            V_VEL.polarization = "Unknown"
            V_VEL.Frequencies_GHz = 0.
            V_VEL.InterPulsePeriods_secs = -32768.
            V_VEL.meters_to_first_cell = 0.
        else:
            print "not DBZ or VR"
            sys.exit(0)

        # output global attributes
        out.Conventions = " "
        
        name = getattr(f_in, "radarName-value")
        out.Instrument_Name = name
        
        out.Instrument_Type = "Ground"
        out.Scan_Mode = "PPI"
        out.Volume_Start_Time = "1970/01/01 00:00:00"
        out.Year = 1970
        out.Month = 1
        out.Day = 1 
        out.Hour = 0 
        out.Minute = 0 
        out.Second = 0 
        out.Volume_Number = 1 
        out.Scan_Number = 2 
        out.Num_Samples = 0 
        out.Project_Name = " "
        out.Production_Date = starttime[:3]
        out.Producer_Name = "NSSL" ;
        out.Software = "Foray NcRadarFile Class" ;
        
        
        #---------------- set out variables
        V_volume_start_time[:] = 0.
        
        V_Range_to_First_Cell[:] = getattr(f_in, "RangeToFirstGate")
        
        V_Cell_Spacing_Method[:] = 1
        
        
        # get the input dimensions size
        az_size = f_in.dimensions['Azimuth']
        naz = len(az_size)
        
        # gates done above
        #g_size = f_in.dimensions['Gate']
        #ng = len(g_size)

        # get netCDF variable object
        f_gw = f_in.variables['GateWidth']
        # make an array contain the values of the netcdf variable
        gw = f_gw[:]
        # You need to know the size to write out the data to a new variable
        # Right now using the size of the azimuth dimension
        # write the values to the output netcdf variable
        V_Cell_Distance_Vector[:ng] = gw[0]
        
        
        V_Nyquist_Velocity[:] = getattr(f_in,"Nyquist_Vel-value")
        V_Unambiguous_Range[:] = getattr(f_in, "RangeFolded")
        
        
        lat = getattr(f_in, "Latitude")
        lon = getattr(f_in, "Longitude")
        V_Latitude[:] = lat
        V_Longitude[:] = lon
        
        # Height ***this assumes the height in the file is meters above sea level
        hgt_R = getattr(f_in,"Height")
        V_Altitude[:] = hgt_R
        

        el = getattr(f_in,"Elevation")
        V_Elevation[:naz] = el        
        
        
        f_bm = f_in.variables['BeamWidth']
        bw = f_bm[:]
        V_bm_width[:naz] = bw
        
        
        t = getattr(f_in, "Time")
        V_time_offset[:naz] = t
        
        
        f_az = f_in.variables['Azimuth']
        az = f_az[:]
        V_Azimuth[:naz] = az
        
        
        missing_PAR = -2700.
        
        if var == "Reflectivity" or var == "ReflectivityPRF2":
            dbz_dbz = f_in.variables[var]
            dbz = dbz_dbz[:]
            dbz = N.where(dbz <= missing_PAR, -32767, dbz)
            V_DBZ[:naz,:ng] = dbz
        
        elif var == "Velocity":
            vr_vr = f_in.variables['Velocity']
            #vr = vr_vr[:]*100
            vr = vr_vr[:]
            vr = N.where(vr <= missing_PAR, -32767, vr)
            V_VEL[:naz,:ng] = vr
        
        else:
            print "not DBZ or VR"
            sys.exit(0)
        
        # close files
        f_in.close()
        out.close()
    
    
    etime = time.time()
    endtime = time.gmtime(etime)
    print
    print "  --------------- END TIME: -------------"
    print "                           ", endtime[3], endtime[4], endtime[5]
    print "  ---------------------------------------"
    print "Total time in minutes: " 
    print (etime - stime)/60.0
#####################################################################################################       



#####################################################################################################
if __name__ == "__main__":
    W2toFORAYmain()   

    
