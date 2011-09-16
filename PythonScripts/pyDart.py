#!/usr/bin/env python
#
import sys, os
from numpy import *
import string
import re
from optparse import OptionParser
from tables import *
from netcdftime import utime
from datetime import datetime as py_datetime
import pylab as P
import numpy as N
import time

debug                = True
missing              = -999.
version_string       = "pyDART_file_version_2.1"
checked_file_version = False
hscale               = 1./1000.  # horizontal scale for plotting

radar_lat            =  35.4976                       # SR2 lat 30 May 2004 / 0000Z
radar_lon            = -98.4654                       # SR2 lon 30 May 2004 / 0000Z

radar_lat            =  35.33331                      # KTLX
radar_lon            = -97.27777                      # KTLX

radar_lat            =  35.235832                     # KOUN/PAR location
radar_lon            = -97.461945                     # KOUN/PAR location

day_utime = utime("days since 1601-01-01 00:00:00")
sec_utime = utime("seconds since 1970-01-01 00:00:00")

#===============================================================================
# SVN keywords

# $Rev:: 52            $:  Revision of last commit
# $Author:: wicker     $:  Author of last commit
# $Date:: 2010-08-24 1#$:  Date of last commit

#===============================================================================
def dll_2_dxy(lat1, lat2, lon1, lon2):
    """dll_2_dxy returns the approximate distance in meters between two lat/lon pairs
       assuming a flat earth approximation (which is sufficient for radar data)
       
       INPUTS:  in radians 
    
       if lon2 > lon1: x > 0
    
       if lat2 > lat1:  y > 0
       
       OUTPUTS:  DX, DY in meters
    """
    rearth = 1000.0 * 6367.0
    
    if lon1 < 0.0:
        lon1p = lon1+2.0*N.pi
    else:
        lon1p = lon1
    
    if lon2 < 0.0:
         lon2p = lon2+2.0*N.pi
    else:
         lon2p = lon2
    
    x = rearth * N.cos(0.5*(lat1+lat2)) * (lon2p-lon1p)
    y = rearth * (lat2-lat1)
    
    return x, y
 
#===============================================================================
def dxy_2_dll(x, y, lat1, lon1):
    """dxy_2_dll returns the approximate lat/lon between an x,y coordinate and 
       a reference lat/lon point.  Assumes a flat earth approximation (which is 
       sufficient for radar data) and should be good out to distances of ~200 km.
       
       INPUTS:  x,y in meters, lat1, lon1 in radians.  Returns radians
    
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
    
    return lat, lon

#===============================================================================
def chk_pyDart_version(h5file, verbose = True):
    """Checks the H5 pyDart file to see whether the file format is compatible"""
    
    global checked_file_version
    
    if h5file.title != version_string:
        print
        print "\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/"
        print "        !!! pyDART file is wrong version !!!"
        print "pyDart sofware expects version:  ", version_string
        print "pyDart file contains version:    ", h5file.title
        print "pyDart software is exiting"
        print "/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/"
        print
        sys.exit(0)
    else:
        if not checked_file_version and verbose == True:
            print
            print "pyDART file is version:  ",h5file.title
            print "/\/\/\/\/\/\/\/\\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/"
            print
            return
        if not checked_file_version:
            checked_file_version = True
            return

#===============================================================================
def open_pyDart_file(filename, return_root=False, verbose = None, append=False):
    
    """Open_pyDart_file opens the pyTables pyDart file, checks a few things and
       returns a the file handle and pyDart table object for use.
    """

# Open DART PyTables file
    
    if append:
        h5file = openFile(filename, mode = "a")
    else:
        h5file = openFile(filename, mode = "r")
    
    chk_pyDart_version(h5file,verbose)
    
    root   = h5file.root

# observations are a table in obs.observations
    
    if return_root:
        return h5file, root, table
    else:
        return h5file, root.obs.observations


#===================================================================================================
def ObType_LookUp(name,DART_name=False,Print_Table=False):
      """ObType_LookUp returns the DART kind number for an input variable type.  There seems
         to be several ways observations names are written in the observation inputs
         and output files, e.g., in the DART ascii files and in the ***.obs.nc files,
         so this function is designed to handle the variety of cases and return the
         integer corresponding to the DART definition.
      
         Exampled:   REFLECTIVITY is sometimes stored as REFL
                     T_2M         is sometimes stored as TEMP2m
                     TD_2M        is sometimes stored as DEWPT2m
      
         If you come across a variable that is not defined, you can add it to the lookup
         table (dictionary) below - just make sure you know the official DART definition
      
         Usage:  variable_kind = ObType_Lookup(variable_name)   where type(variable_name)=str
      
         If you need the return the actual DART_name as well, set the input flag to be True"""

# Create local dictionary for observation kind definition - these can include user abbreviations

#                      user's observation type            kind   DART official name

      Look_Up_Table={ "DOPPLER_VELOCITY":                 [11,   "DOPPLER_VELOCITY"] ,
                      "REFLECTIVITY":                     [12,   "REFLECTIVITY"],
                      "RADAR_CLEARAIR_REFLECTIVITY":      [13,   "RADAR_CLEARAIR_REFLECTIVITY"],  
                      "DIFFERENTIAL_REFLECTIVITY":        [300,  "DIFFERENTIAL_REFLECTIVITY"],
                      "SPECIFIC_DIFFERENTIAL_PHASE":      [301,  "SPECIFIC_DIFFERENTIAL_PHASE"],
                      "METAR_U_10_METER_WIND":            [1,    "METAR_U_10_METER_WIND"],
                      "METAR_V_10_METER_WIND":            [2,    "METAR_V_10_METER_WIND"],
                      "METAR_TEMPERATURE_2_METER":        [4,    "METAR_TEMPERATURE_2_METER"],
                      "DEW_POINT_2_METER":                [9,    "DEW_POINT_2_METER"],
                      "METAR_SPECIFIC_HUMIDITY_2_METER":  [5,    "METAR_SPECIFIC_HUMIDITY_2_METER"],
                      "VR":                               [11,   "DOPPLER_VELOCITY"],
                      "DBZ":                              [12,   "REFLECTIVITY"],
                      "0DBZ":                             [13,   "RADAR_CLEARAIR_REFLECTIVITY"],
                      "ZDR":                              [300,  "DIFFERENTIAL_REFLECTIVITY"],
                      "KDP":                              [301,  "SPECIFIC_DIFFERENTIAL_PHASE"],
                      "U10M":                             [1,    "METAR_U_10_METER_WIND"],
                      "V10M":                             [2,    "METAR_V_10_METER_WIND"],
                      "T2M":                              [4,    "METAR_TEMPERATURE_2_METER"],
                      "TD2M":                             [9,    "DEW_POINT_2_METER"],
                      "H2M":                              [5,    "METAR_SPECIFIC_HUMIDITY_2_METER"],
                      "U_10M":                            [1,    "METAR_U_10_METER_WIND"],
                      "V_10M":                            [2,    "METAR_V_10_METER_WIND"],
                      "T_2M":                             [4,    "METAR_TEMPERATURE_2_METER"],
                      "TD_2M":                            [9,    "DEW_POINT_2_METER"],
                      "H_2M":                             [5,    "METAR_SPECIFIC_HUMIDITY_2_METER"],
                      "TEMP2M":                           [4,    "METAR_TEMPERATURE_2_METER"],
                      "DEWPT2M":                          [9,    "DEW_POINT_2_METER"],
                      "REFL":                             [12,   "REFLECTIVITY"],
                      "FLASH_RATE_2D":                    [2014, "FLASH_RATE_2D"]
                    }
      
      if Print_Table:
            print
            print "VALID INPUT VARIABLE NAME              KIND  DART NAME"
            print "=========================================================================="
            for key in Look_Up_Table.keys():
                  print "%35s    %3d    %s" % (key, Look_Up_Table[key][0], Look_Up_Table[key][1])
            return
      
      name2 = name.upper().strip()
      if Look_Up_Table.has_key(name2):
            if DART_name == True:
                  return Look_Up_Table[name2][0], Look_Up_Table[name2][1]
            else:
                 return Look_Up_Table[name2][0]
      else:
            print "ObType_LookUp cannot find variable:  ", name, name2
            if DART_name == True:
                 return missing, 'None'
            else:
                 return missing

#===============================================================================
def mergeTables(table_new, tables):

    print "mergeTable called:  New table:  ",table_new
    print "mergeTable called:  Reading from:  ",tables, len(list(tables))

    if len(tables) == 1 or type(tables) == type('str'):
        print "Only one table for merging supplied, simply doing a copy..."
        h5file1, table1 = open_pyDart_file(tables)
        h5file1.copyFile(table_new, overwrite=True)
        h5file1.close()
        print "Finished copying %s into %s" % (tables, table_new)
    else:
        h5file1, table1 = open_pyDart_file(tables[0])
        print "Creating new table to copy into...."
        h5file1.copyFile(table_new, overwrite=True)
        print "Finished copying %s into %s" % (tables[0], table_new)

        h5file1.close()   
        h5file1, table1 = open_pyDart_file(table_new, append=True)
        cols = table1.cols

        for file in tables[1:]: 
            print "Now copying from table:  ", file
            h5file2, table2 = open_pyDart_file(file)

            row1 = table1.row
            for row in table2.iterrows():
                for key in cols._v_colnames:
                    row1[key] = row[key]
                row1.append()
            table1.flush()
            h5file2.close()

        print "Finished appending all table rows...."
        print "New table:    ", table1

        h5file1.close()

        return
#===============================================================================
class pyDART():
    def __init__(self):
        self.hdf5    = None
        self.ascii   = None
        self.index   = None
        self.verbose = False
        self.debug   = True

#-------------------------------------------------------------------------------
# File:  creates a standard set of filenames for pyDART ascii and hdf5 files
#-------------------------------------------------------------------------------
    
    def file(self,filename=None):
        
        if filename != None:
            
            if filename[-4:] == ".out":
                self.hdf5  = filename[:-4] + ".h5"
                self.ascii = filename
                return
            
            if filename[-3:] == ".h5":
                self.hdf5  = filename
                self.ascii = filename[:-3] + ".out"
                return
            
            if filename[-5:] == ".hdf5":
                self.hdf5  = filename
                self.ascii = filename[:-5] + ".out"
                return
        
        else:
            print "pyDART.init:  No file is supplied, please add one to self.hdf5 or self.ascii"
        
        return

#-------------------------------------------------------------------------------
# Search:
#-------------------------------------------------------------------------------
    
    def search(self, variable=None, start=None, end=None, condition=None, loc=None, selfdata=False):

# Construct a variable to search table
        
        if variable == None and start == None and end == None:
            print "pyDART.search:  no variable, condition, start, or end search time supplied....exiting"
            return -1
        
        if start != None and type(start) == tuple:
            self.start = py_datetime(start[0],start[1],start[2],start[3],start[4],start[5])
            if self.verbose:
                if self.debug:  print "pyDART.search:  Start datetime:  ",self.start
        
        if end != None and type(end) == tuple:
            self.end = py_datetime(end[0],end[1],end[2],end[3],end[4],end[5])
            if self.verbose:
                if self.debug:  print "pyDART.search:  End datetime:  ", self.end

# Build a list of search conditions
        cond = []
        
        if variable != None:
            cond.append( "( kind == " + str(ObType_LookUp(variable)) + " )" )
        
        if start != None:
            utime_start = sec_utime.date2num(self.start)
            cond.append( "(" + str(utime_start) + " <= utime)" )
        
        if end != None:
            utime_end   = sec_utime.date2num(self.end)
            cond.append( "(utime <= " + str(utime_end) + ")" )
        
        if condition != None:
            cond.append(condition)
        
        if loc != None:
            for item in loc:
                cond.append(item)

# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)

# Determine when you want observations from
        
        if self.verbose:
            print "PyDART SEARCH START TIME  %s  /  UTIME_START: %s" % (self.start, utime_start)
            print "PyDART SEARCH END   TIME  %s  /  UTIME_END:   %s" % (self.end, utime_end)
            print "PyDART converted utimes: ", sec_utime.num2date(utime_start)

# Create string for search
        
        if len(cond) != 0:
            
            search_string = cond[0]
            for x in cond[1:]:
                search_string = search_string + " & " + x
            
            if self.verbose:
                print
                print "PyDART SEARCH CONDITION IS:  ", search_string
                print
            self.index = table.getWhereList(search_string)    # Do the search
            if len(self.index) == 0:  self.index = None
        else:
            if self.verbose:
                print "pyDART.SEARCH:  NO SEARCH CONDITION CREATED....", cond
        
        h5file.close()
        
        return

#-------------------------------------------------------------------------------
# A quick routine to grid pyDART data
    
    def grid(self,data,dx=1000.,dy=1000.,variable=None):

# Need to remove all locations that are identical - do that by creating a complex number to
#      comprised of (x + j * y) which can then be sorted for multiple identical entries.
#      We then remove those entries and grid them using Jeff Whitaker's nat grid which has been
#      added to the ENTHOUGHT/numpy libs
#      (see http://sourceforge.net/project/showfiles.php?group_id=80706&package_id=142792)
        
        xy = data['x'].copy() + (0.+1.j)*data['y'].copy()
        xy_unique, index = N.unique(xy, return_index=True)
        
        print
        print "Number of points found from initial search:      %d" % (xy.size)
        print "Number of points which are unique (no overlap):  %d" % (xy_unique.size)
        #
        x   = N.real(xy_unique)
        y   = N.imag(xy_unique)
        z   = data['value'][index[:]]

# Always hard to create a reasonable guess at a grid.  Find middle of data, and work back
#        from there.  Using geometric, not arithmatic, mean for domain size.  Use 80% of it.
        
        xmean = x.mean()
        ymean = y.mean()
        del_x = x.max() - x.min()
        del_y = y.max() - y.min()
        xmin  = N.round(xmean - (del_y*del_x / (del_y+del_x)))
        ymin  = N.round(ymean - (del_y*del_x / (del_y+del_x)))
        xmax  = N.round(xmean + (del_y*del_x / (del_y+del_x)))
        ymax  = N.round(ymean + (del_y*del_x / (del_y+del_x)))
        
        xi = N.arange(xmin, xmax, dx)
        yi = N.arange(ymin, ymax, dy)
        zi = P.mlab.griddata(x,y,z,xi,yi)
        
        if self.verbose:
            print
            print 'X(nx) min/max of grid region:     ',xi.size, xi.min(), xi.max()
            print 'Y(ny) min/max of grid region:     ',yi.size, yi.min(), yi.max()
            print 'Z(nx,ny) min/max of grid region:  ',zi.shape, zi.min(), zi.max()
        
        return xi, yi, zi

#-------------------------------------------------------------------------------
# A quick and dirty plotting routine to make sure the pyDART is reasonable
    
    def plot(self, variable=None, savefig=None):

# define a quick and dirty colormap for reflectivity
        
        cmap  = [  (0.000,1.000,1.000), \
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

# Return some data specified (hopefully) by user search
# Later, we might add an error check here to make sure the data are all the same kind
        
        data = self.get_data()

# Now, call grid to grid some data
        
        xi, yi, zi = self.grid(data)
        yi         = yi * hscale
        xi         = xi * hscale
        
        P.figure(1)
        P.clf()
        
        if variable.upper() == "DBZ" or variable.upper() == "REFLECTIVITY":
            clevels = N.arange(0,75,5)
            plt = P.contourf(xi,yi,zi, clevels, colors=cmap)
            cbar = P.colorbar(plt)
            cbar.ax.set_ylabel('dBZ')
        else:
            clevels = N.arange(-40,45,5)
            plt = P.contourf(xi,yi,zi, clevels)
            cbar = P.colorbar(plt)
            cbar.ax.set_ylabel(variable)
        
        if savefig != None:
            P.savefig(savefig)
        else:
            P.show()
        
        return

#-------------------------------------------------------------------------------
    
    def get_data(self, variable=None, all=False):
        
        if self.index == None and all ==False:
            if self.verbose: print "pyDART.get_data:  No search indices supplied, returning all rows"

# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)
        
        if self.index == None and all == True:
            self.index = N.arange(table.nrows)
            if self.verbose: print "pyDART.get_data, return all rows of table!"
        
        if debug:  start = time.clock()
        
        data = table.readCoordinates(self.index)   # this is why I love pyTables!!!!
        
        if debug:  print "pyDart.get_data  Execution time for readCoordinates method:  ", time.clock() - start
#
# Original method which retrieves less data but takes 10x longer to get
#
#       start = time.clock()
#       data = {}
#       if type(variable) == type("str"):             # Determine the user's request data type
#           if variable == "all":                     # Retrieve all the columns from the row
#               for name in table.colnames:
#                   data[name] = table.col(name)[self.index[:]]
#           elif variable in table.colnames:
#                   data[variable] = table.col(variable)[self.index[:]]
#           else:
#               if self.verbose: print "read DART_table: /STR/ requested variable:  ",variable," does not exist"
#               data = None
#
#       elif type(variable) == type([]):
#           for var in variable:
#               if var in table.colnames:
#                       data[var] = table.col(var)[self.index[:]]
#               else:
#                   if self.verbose: print "read DART_table:  /LIST/ requested variable:  ",var," does not exist"
#                   data = None
#       else:
#           if self.verbose: print "read DART_table:  Problem, requested variable:  ",variable," does not exist"
#           data = None
#       print "Execution time for Table.col method:  ", time.clock() - start
        
        h5file.close()
        
        return data

#-------------------------------------------------------------------------------
    
    def list(self,variable=None,dumplength=None):
        
        if variable == None:
            if self.verbose: print "pyDART.list:  No variable supplied, listing all variables in ", self.hdf5,"/obs/observations"
            variable = "all"

# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)
        
        if type(variable) == type("str"):       # Determine the user's request data type
            if variable == "all":               # Retrieve all the columns from the row
                for name in table.colnames:
                    print name
                h5file.close()
                return
            else:                                     # User requests specific variables, search for DART variable index
                var_index = ObType_LookUp(variable)
            
            if var_index == missing:
                print "pyDart.list:  requested variable:  ",variable," does not exist"
                print "pyDart.list:  Valid variables are:  ", ObType_LookUp(variable,Print_Table=True)
                h5file.close()
                return
            
            if type(variable) == type(1):
                var_index = variable
            
            if self.index == None:
                table.getWhereList("kind == var_index")
            
            if len(self.index) != 0:
                if self.verbose: print "Number of observations:  ",len(self.index)
                data   = self.get_data()
                number = data['number']
                value  = data['value']
                time   = data['date']
                utime  = data['utime']
                lat    = N.degrees(data['lat'])
                lon    = N.degrees(data['lon'])
                x      = N.where(data['x'] != missing, data['x']/1000., missing)
                y      = N.where(data['y'] != missing, data['y']/1000., missing)
                z      = N.where(data['z'] != missing, data['z']/1000., missing)
                az     = N.where(data['azimuth']   != missing, data['azimuth'],   missing)
                el     = N.where(data['elevation'] != missing, data['elevation'], missing)
                
                if dumplength == True:
                    dumplength = len(self.index)
                    print
                    if self.verbose:
                        print "Printing ALL the values, hope it does not take too long because the list has ", dumplength,"  entries"
                        print
                        print "========================================================================================================================"
                        print "Index     Variable    Value  Date/Time       Lat    Lon    X(km)  Y(km)  Z(km)      AZ        EL"
                    
                    for n in range(0,dumplength-1):
                        print "%7d     %s     %9.5f    %s  %9.4f  %9.4f  %9.4f  %9.4f  %9.5f  %5.1f  %5.1f" \
                             % (number[n], variable.upper(), value[n], time[n], lat[n], lon[n], x[n], y[n], z[n], az[n], el[n])
                else:
                    dumplength = min(100,len(self.index))
                    print
                    if self.verbose:
                        print "Printing the first and last 100 values of search indices"
                        print
                        print "========================================================================================================================"
                        print "Index     Variable    Value          Date/Time           Lat        Lon        X(km)      Y(km)     Z(km)     AZ      EL"
                    
                    for n in range(0,dumplength-1):
                        print "%7d     %s     %9.5f    %s  %9.4f  %9.4f  %9.4f  %9.4f  %9.5f  %5.1f  %5.1f" \
                          % (number[n], variable.upper(), value[n], time[n], lat[n], lon[n], x[n], y[n], z[n], az[n], el[n])
                    
                    for n in range(len(time)-dumplength,len(time)):
                        print "%7d     %s     %9.5f    %s  %9.4f  %9.4f  %9.4f  %9.4f  %9.5f  %5.1f  %5.1f" \
                          % (number[n], variable.upper(), value[n], time[n], lat[n], lon[n], x[n], y[n], z[n], az[n], el[n])
            else:
                print "NO OBSERVATIONS FOUND FOR ", variable.upper()
        
        h5file.close()
        
        return
#-------------------------------------------------------------------------------
    
    def stats(self,variable=None,dumplength=None):
        
        if variable == None:
            if self.verbose: print "\n pyDART.stats:  No variable supplied, not valid, exiting \n"

# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)
        
                                    # User requests specific variables, search for DART variable index
        var_index = ObType_LookUp(variable)
            
        if var_index == missing:
            print "pyDart.list:  requested variable:  ",variable," does not exist"
            print "pyDart.list:  Valid variables are:  ", ObType_LookUp(variable,Print_Table=True)
            h5file.close()
            return
            
        if self.index == None:
            table.getWhereList("kind == var_index")
        
        if len(self.index) != 0:
            if self.verbose: print "Number of observations:  ", len(self.index)
            data   = self.get_data()
            number = data['number']
            value  = data['value']
            time   = data['date']
            utime  = data['utime']
            lat    = N.degrees(data['lat'])
            lon    = N.degrees(data['lon'])
            x      = N.where(data['x'] != missing, data['x']/1000., missing)
            y      = N.where(data['y'] != missing, data['y']/1000., missing)
            z      = N.where(data['z'] != missing, data['z']/1000., missing)
            az     = N.where(data['azimuth']   != missing, data['azimuth'],   missing)
            el     = N.where(data['elevation'] != missing, data['elevation'], missing)
            
            print "\nMean/Stdev for %s:  %f   %f  " % (variable, value.mean(), value.std())
            print "Mean/Stdev for X:     ", x.mean(), x.std()
            print "Mean/Stdev for Y:     ", y.mean(), y.std()
            print "Mean/Stdev for Z:     ", z.mean(), z.std()
            print "Mean/Stdev for AZ:    ", az.mean(), az.std()
            print "Mean/Stdev for EL:    ", el.mean(), el.std()
            print "Mean/Stdev for LAT:   ", lat.mean(), lat.std()
            print "Mean/Stdev for LON:   ", lon.mean(), lon.std()
            print

        else:
            print "NO OBSERVATIONS FOUND FOR ", variable.upper()
        
        h5file.close()
        
        return

#-------------------------------------------------------------------------------
    
    def ascii2hdf(self,filename=None,radar_loc=None):
        
        if filename != None:
            self.file(filename=filename)
        
        if radar_loc != None:
            print "PyDart.ascii2hdf:  radar location supplied:  lat: %f  lon: %f " % (N.rad2deg(radar_loc[0]),N.rad2deg(radar_loc[1]))

# Create PyTables file
        
        filter_spec = Filters(complevel=5, complib="zlib", shuffle=1, fletcher32=0)
        
        h5file = openFile(self.hdf5, mode = "w", title = version_string, filters=filter_spec)
        
        group_ob_kinds = h5file.createGroup("/", 'obs', 'Obs for DART file')

# Create group for observation descriptions
        
        table_ob_kinds = h5file.createTable(group_ob_kinds, 'kinds', DART_ob_kinds, 'Observation Descriptions')
        
        fi = open(self.ascii, 'r')
        fi.readline()                       # Read(str) "obs_sequence"
        fi.readline()                       # Read(str) "obs_kind_definitions"
        ob_kinds = long(fi.readline())      # Read(int) "number of observation types" 
        
        if self.debug:  print 'Number of observation types:  ', ob_kinds
        
        n = 0
        
        row = table_ob_kinds.row
        
        while n < ob_kinds:
            stuff = fi.readline()
            stuff = stuff.split()
            row['index'] = int(stuff[0])
            row['name']  = stuff[1]
            print 'Observation kind definitions:  ', row['index'], row['name']
            row.append()
            n += 1
        
        if self.debug:  print 'Completed reading obs_kind_definitions'

# Create group for misc file header information
        
        group_header = h5file.createGroup("/", 'header', 'Header Information for DART file')
        table_header = h5file.createTable(group_header, 'attributes', DART_header, 'Attributes of the observational file')
        
        stuff      = fi.readline()          # Read(str) "num_copies" line
        stuff      = stuff.split()
        
        num_copies = long(stuff[1])         # Important, if you have a truth file, you need to know this number (either 1 or 2)
        num_qc     = long(stuff[3])         
        
        stuff       = fi.readline()         # Read(str) "num_obs" line
        stuff       = stuff.split()
        num_obs     = long(stuff[1])
        max_num_obs = long(stuff[3])
        
        data_storage = []

        for numcp in N.arange(num_copies):
            data_storage.append(fi.readline())  # Read(str) how the data is written....e.g., "observations" or "truth"
            
        for numqc in N.arange(num_qc):          # If the num_qc flag is > 0, read the description of the qc flags
            data_storage.append(fi.readline())  # 

        stuff       = fi.readline()         # Read(str) "first   1   last   No of obs" line
        stuff       = stuff.split()
        first       = long(stuff[1])
        last        = long(stuff[3])
        
        row = table_header.row
        
        row['origin_file'] = "Original DART observation file is: " + self.ascii + "\n"
        row['num_copies']  = num_copies
        row['num_qc']      = num_qc
        row['num_obs']     = num_obs
        row['max_num_obs'] = max_num_obs
        row['first']       = first
        row['last']        = last
        
        row.append()
        table_header.flush()
        
        if self.verbose and self.debug:
            print "Number of observation copies:  ", num_copies
            print "Number of QC'd observations:   ", num_qc
            print "Number of observations:        ", num_obs
            print "Max number of observations:    ", max_num_obs

# Find the obs group to create table in
        
        root         = h5file.root
        group_obs    = root.obs
        group_header = root.header

# create table that will hold the observation information
        
        table_obs = h5file.createTable(group_obs, 'observations', DART_obs, 'Observations from DART file')
        
        row = table_obs.row

# Read in the obs sequentially!
        
        n = 0
        
        while n < group_header.attributes.cols.num_obs[0]:
            
            stuff            = fi.readline()        # Read(str) "OBS...." line
            stuff            = stuff.split()
            
            row["number"]    = long(stuff[1])
            
            for numcp in N.arange(num_copies+num_qc-1):      # Now read the observation, and if provided, the truth value
                if data_storage[numcp].find("obs") == 0:
                    row["value"]     = read_double_precision_string(fi.readline())
                elif data_storage[numcp].find("tru") == 0:
                    row["truth"]     = read_double_precision_string(fi.readline())
                elif data_storage[numcp].find("QC") == 0:
                    row["qc"]     = read_double_precision_string(fi.readline())
                else:
                    print "pyDART ascii2hdf:  Problem, data_storage is not 'observations' or 'truth' --> exiting!"
                    print data_storage
                    sys.exit(-1)
            
            for numqc in N.arange(num_qc):      # If the num_qc flag is > 0, read the quality control flag
                row["qc"]     = read_double_precision_string(fi.readline())

            stuff            = fi.readline()
            stuff            = stuff.split()
            row["previous"]  = long(stuff[0])
            row["next"]      = long(stuff[1])
            row["cov_group"] = long(stuff[2])
            
            fi.readline()
            fi.readline()
            
            stuff             = fi.readline()
            stuff             = stuff.split()
            row["lon"]        = read_double_precision_string(stuff[0])
            row["lat"]        = read_double_precision_string(stuff[1])
            row["height"]     = read_double_precision_string(stuff[2])
            if len(stuff) == 4:
                row["vert_coord"] = long(stuff[3])
            else:
                row["vert_coord"] = long(fi.readline())
            
            fi.readline()
            
            row["kind"]       = int(fi.readline())

# Check to see if its radial velocity and add platform information...need BETTER CHECK HERE!
            
            if row["kind"] == ObType_LookUp("VR"):
                
                fi.readline()
                fi.readline()
                
                stuff                      = fi.readline()
                stuff                      = stuff.split()
                row['platform_lon']        = read_double_precision_string(stuff[0])
                row['platform_lat']        = read_double_precision_string(stuff[1])
                row['platform_height']     = read_double_precision_string(stuff[2])
                if len(stuff) == 4:
                    row["platform_vert_coord"] = long(stuff[3])
                else:
                    row["platform_vert_coord"] = long(fi.readline())      
                fi.readline()
                
                stuff                    = fi.readline()
                stuff                    = stuff.split()
                
                row['platform_dir1']     = read_double_precision_string(stuff[0])
                row['platform_dir2']     = read_double_precision_string(stuff[1])
                row['platform_dir3']     = read_double_precision_string(stuff[2])
                row['platform_nyquist']  = read_double_precision_string(fi.readline())
                row['platform_key']      = long(fi.readline())

# If observation is radial velocity, store elevation and azimuth
                
                row['elevation']         = N.rad2deg(N.arcsin(row['platform_dir3']))
                if N.abs(N.cos(row['elevation'])) < 0.001:
                    row['azimuth'] = 0.0
                else:
                    row['azimuth'] = N.rad2deg( N.arctan2( row['platform_dir1'] / N.cos(N.deg2rad(row['elevation'])), \
                                                           row['platform_dir2'] / N.cos(N.deg2rad(row['elevation'])) ) )
            
            stuff            = fi.readline()
            stuff            = stuff.split()
            row['seconds']   = int(stuff[0])
            row['days']      = int(stuff[1])

# Add in a full date and time string to data set, as well as create a UTIME in seconds for searching
            
            days = float(stuff[1])+float(stuff[0])/86400.
            date = day_utime.num2date(days)
            
            row['date']      = str(date)
            row['utime']     = round(sec_utime.date2num(date)) #  to prevent sometimes truncating down to next integer
            
            row['error_var'] = read_double_precision_string(fi.readline())
            row['index']     = n

# Add in information about the observations relative location to radar.  Do this only for radar observation (for now)
            
            if radar_loc != None:
                radar_lat = radar_loc[0]
                radar_lon = radar_loc[1]
                
                row['x'], row['y'] = dll_2_dxy(radar_lat, row['lat'], radar_lon, row['lon'])
                row['z']         = row['height']
            
            if n % 5000 == 0:
                print "read_DART_ob:  Processed observation # ", n+1, days #,sec_utime.date2num(date) #,py_datetime(start[0],start[1],start[2],start[3],start[4],start[5])
                print "date,sec_utime,utime = ", date,sec_utime.date2num(date), row['utime']
            
            n += 1
            
            row.append()
            
            table_obs.flush
        
        h5file.close()
        
        fi.close()
        
        print "pyDART.ascii2h5:  Converted ascii DART file to HDF5 DART file"
        
        return 1
#-------------------------------------------------------------------------------
    
    def dict2hdf(self,obs_dict,filename=None,radar_loc=None):
        
        if filename != None:
            self.file(filename=filename)
        
        if radar_loc != None:
            print "PyDart.dict2hdf:  radar location supplied:  lat: %f  lon: %f " % (N.degrees(radar_loc[0]),N.degrees(radar_loc[1]))
        
        print
        print "!!! Warning from pyDart.dict2hdf !!!"
        print "pyDART_version_2:  Version 2 now stores lat and lon correctly - please make sure your analysis does as well..."
        print

# Create PyTables file
        
        filter_spec = Filters(complevel=5, complib="zlib", shuffle=1, fletcher32=0)
        
        h5file = openFile(self.hdf5, mode = "w", title = version_string, filters=filter_spec)
        group_ob_kinds = h5file.createGroup("/", 'obs', 'Obs for DART file')

# Create group for observation descriptions
        
        table_ob_kinds = h5file.createTable(group_ob_kinds, 'kinds', DART_ob_kinds, 'Observation Descriptions')
        
        row = table_ob_kinds.row
        
        row['index'] = ObType_LookUp("DBZ")
        row['name']  = "DOPPLER_RADIAL_VELOCITY" #"VR"
        row.append()
        row['index'] = ObType_LookUp("VR")
        row['name']  = "RADAR_REFLECTIVITY" #"DBZ"
        row.append()
        
        if self.debug:  print 'Completed reading obs_kind_definitions'

# Create group for misc file header information
        
        group_header = h5file.createGroup("/", 'header', 'Header Information for DART file')
        table_header = h5file.createTable(group_header, 'attributes', DART_header, 'Attributes of the observational file')
        
        print table_header
        row = table_header.row
        
        row['origin_file'] = "Original DART observation file is: " + self.ascii + "\n"
        row['num_copies']  = 1
        row['num_qc']      = 1
        row['num_obs']     = obs_dict['counter']
        row['max_num_obs'] = obs_dict['counter']
        row['first']       = 1
        row['last']        = obs_dict['counter']
        
        row.append()
        table_header.flush()
        
        if self.debug:
                print "Number of observation copies:  ", 1
                print "Number of QC'd observations:   ", 1
                print "Number of observations:        ", obs_dict['counter']
                print "Max number of observations:    ", obs_dict['counter']

# Find the obs group to create table in
        
        root         = h5file.root
        group_obs    = root.obs
        group_header = root.header

# create table that will hold the observation information
        
        table_obs = h5file.createTable(group_obs, 'observations', DART_obs, 'Observations from DART file')
        
        row = table_obs.row

# Read in the obs sequentially!
        
        for n in N.arange( obs_dict['counter'] ):
            
            row['number']    = long(n+1)
            row['value']     = obs_dict['value'][n]
            
            if n == 0:
                    row['previous'] = -1
            else:
                    row['previous'] = n-1
            
            row['next']       = n+1
            row['cov_group']  = -1

# Here is where you sometimes need to switch lat and lon in the codes....
            
            row['lon']        = obs_dict['lon'][n]
            row['lat']        = obs_dict['lat'][n]
            row['height']     = obs_dict['height'][n]
            row['vert_coord'] = 3
            row['kind']       = obs_dict['kind'][n]

# Check to see if its radial velocity and add platform information...need BETTER CHECK HERE!
            
            if row['kind'] == ObType_LookUp("VR"):

# Need to switch the lat and lon these around to be correct in version 2 of pyDART...
                
                row['platform_lon']        = obs_dict['platform_lon'][n]
                row['platform_lat']        = obs_dict['platform_lat'][n]
                row['platform_height']     = obs_dict['platform_height'][n]
                row['platform_vert_coord'] = obs_dict['platform_vert_coord'][n]
                
                row['platform_dir1']       = obs_dict['platform_dir1'][n]
                row['platform_dir2']       = obs_dict['platform_dir2'][n]
                row['platform_dir3']       = obs_dict['platform_dir3'][n]
                row['platform_nyquist']    = obs_dict['platform_nyquist'][n]
                row['platform_key']        = obs_dict['platform_key'][n]
                
                row['elevation']           = N.rad2deg(N.arcsin(obs_dict['platform_dir3'][n]))
            
                if N.abs(N.cos(row['elevation'])) < 0.001:
                    row['azimuth'] = 0.0
                else:
                    row['azimuth'] = N.rad2deg( N.arctan2( row['platform_dir1'] / N.cos(N.deg2rad(row['elevation'])), \
                                                           row['platform_dir2'] / N.cos(N.deg2rad(row['elevation'])) ) )
                    if row['azimuth'] < 0: row['azimuth'] = row['azimuth'] + 360.
                    
            if row['kind'] == ObType_LookUp("VR"):    # See if there is information about the azimuth and elevation supplied
            
                if 'azimuth' in obs_dict.keys():
                    row['azimuth'] = obs_dict['azimuth'][n]
                    if row['azimuth'] < 0: row['azimuth'] = row['azimuth'] + 360.

                if 'elevation' in obs_dict.keys():
                    row['elevation'] = obs_dict['elevation'][n]
                
# Add in information about the observations relative location to radar.  Do this only for radar observation (for now)
            
            if radar_loc != None:
                
                radar_lat = radar_loc[0]
                radar_lon = radar_loc[1]
                                
                row['x'], row['y'] = dll_2_dxy(radar_lat, row['lat'], radar_lon, row['lon'])
                row['z']           = row['height']

# Add in a full date and time string to data set, as well as create a UTIME in seconds for searching
            
            row['seconds']   = obs_dict['sec'][n]
            row['days']      = obs_dict['day'][n]
            
            days = float(obs_dict['day'][n]) + float(obs_dict['sec'][n])/86400.
            date = day_utime.num2date(days)
            
            row['date']      = str(date)
            row['utime']     = round(sec_utime.date2num(date))  # round to prevent sometimes truncating down to next integer
            
            row['error_var'] = obs_dict['error_var'][n]
            row['index']     = n
            
            if n % 5000 == 0:
                print "PyDart.dict2hdf:  Processed observation # ", n+1, days
                print " Date,sec_utime,utime = ", date,sec_utime.date2num(date), row['utime']
            
            row.append()
        
        table_obs.flush
        
        h5file.close()
        
        print "pyDART.dict2h5:  Converted DICTIONARY DATA to HDF5 DART file"
        
        return 1

#-------------------------------------------------------------------------------
    
    def correct_ens_output(self, ascii=None):
        
        if self.hdf5 == None:
            print "\n pyDart/correct_ens_output:  No HDF5 file name is defined, please add one to the command line... \n"
            return
            
# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)

# Open ASCII file
        
        if ascii == None:
            fi = open(self.ascii[:-4]+".tmp.out", "w")
        else:
            fi = open(ascii, "w")
        
        if self.index != None:
            fi.write("%d\n" % (len(self.index)) )
            
        else:
            fi.write(" %d\n" % (attr.col('num_obs')[0]))
                            
        if self.debug:  print "pyDart/correct_ens_output:  Completed writing out header information for ascii CORRECT_ENS file"

# If there is no search index defined, then create a temporary one to loop through all rows..
        
        if self.index == None:
            self.index = arange(table.nrows)
        
        n = 0
        for row in table.itersequence(self.index):
            n += 1
                        
            fi.write("    %14.7f   %14.7f    %14.7f   %8.3f \n" % (row["lon"], row["lat"], row["height"], row["value"] ))
            
            if n % 10000 == 0: print "pyDart/hdf2ascii:  Processed observation # ", n
        
        h5file.close()
        
        fi.close()
        
        print "pyDart/correct_ens_output:  Created CORRECT_ENS input file, N = ", n
        
        return 0

#-------------------------------------------------------------------------------
    
    def hdf2ascii(self, ascii=None, obs_error=None):
        
        if self.hdf5 == None:
            print "pyDart/hdf2ascii:  No HDF5 file name is defined, please add one to the command line..."
            return
            
        if obs_error != None:
                print "HEY!!  Changing standard deviation of fields:  ", obs_error, "\n"
                error_dart_fields = []
                for field in obs_error:
                    error_dart_fields.append(ObType_LookUp(field[0],DART_name=True)[0])
        else:
            error_dart_fields = None


# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose=self.verbose)

# Open ASCII file
        
        if ascii == None:
            fi = open(self.ascii[:-4]+".tmp.out", "w")
        else:
            fi = open(ascii, "w")

# Write out header information
        fi.write(" obs_sequence\n")
        fi.write("obs_kind_definitions\n")

# Observation types are in the h5file.root.obs.kinds directory
        
        kinds = h5file.root.obs.kinds
        
        fi.write("       %d\n" % size(kinds))

        for r in kinds.iterrows():
            fi.write("    %d          %s   \n" % (r['index'], r['name']) )
            print 'pyDart/hdf2ascii:  Written observational types:  ', r
        
        attr = h5file.root.header.attributes
        nobs = attr.col('num_obs')[0]

        fi.write("  num_copies:            %d  num_qc:            %d\n" % (attr.col('num_copies')[0], attr.col('num_qc')[0] ))
        
        if self.index != None:
            fi.write(" num_obs:       %d  max_num_obs:       %d\n" % (len(self.index), len(self.index)) )
        else:
            fi.write(" num_obs:       %d  max_num_obs:       %d\n" % (attr.col('num_obs')[0], attr.col('max_num_obs')[0]))

        if self.debug:
            print "pyDart/hdf2ascii:  Number of observation copies:  ", attr.col('num_copies')[0]
            print "pyDart/hdf2ascii:  Number of QC'd observations:   ", attr.col('num_qc')[0]
                    
        fi.write("observations\n")
        if attr.col('num_copies')[0] == 2:
            fi.write("truth\n")
            
        if attr.col('num_qc')[0] == 1:
            fi.write("QC\n")

        if self.index != None:
            fi.write("  first:            %d  last:       %d\n" % (1, len(self.index)))
            if self.debug:
               print "pyDart/hdf2ascii:  Max number of observations:    ", len(self.index)
        else:
            fi.write("  first:            %d  last:       %d\n" % (attr.col('first')[0], attr.col('last')[0]))
            if self.debug:
                print "pyDart/hdf2ascii:  Max number of observations:    ", attr.col('max_num_obs')[0]

        if self.debug:  print "pyDart/hdf2ascii:  Completed writing out header information for ascii DART file"

# If there is no search index defined, then create a temporary one to loop through all rows..
        
        if self.index == None:
            self.index = arange(table.nrows)
        
        n = 0
        for row in table.itersequence(self.index):
            n += 1
            
            fi.write(" OBS            %d\n" % n )
            fi.write("   %20.14f\n" % row["value"]  )
            
            if attr.col('num_copies')[0] == 2:
                fi.write("   %20.14f\n" % row["truth"]  )
            
            if attr.col('num_qc')[0] == 1:
                fi.write("   %20.14f\n" % row["qc"]  )
            
# Code (from RLT) to output correct index number for each ob in the output DART file
            if n == 1: 
                fi.write(" %d %d %d\n" % (-1, n+1, row["cov_group"]) ) # First obs.
            elif n == len(self.index):
                fi.write(" %d %d %d\n" % (n-1, -1, row["cov_group"]) ) # Last obs.
            else:
                fi.write(" %d %d %d\n" % (n-1, n+1, row["cov_group"]) ) 
            
            fi.write("obdef\n")
            fi.write("loc3d\n")
            
            fi.write("    %20.14f          %20.14f          %20.14f\n" % (row["lon"], row["lat"], row["height"]) )
            
            fi.write("     %d     \n" % row["vert_coord"] )
            
            fi.write("kind\n")
            
            fi.write("     %d     \n" % row["kind"] )

# Check to see if its radial velocity and add platform information...need BETTER CHECK HERE!
            
            if row["kind"] == ObType_LookUp("VR"):
                
                fi.write("platform\n")
                fi.write("loc3d\n")
                
                fi.write("    %20.14f          %20.14f        %20.14f\n" % (row["platform_lon"], row["platform_lat"], row["platform_height"]) )
                fi.write("     %d     \n" % row["platform_vert_coord"] )
                
                fi.write("dir3d\n")
                
                fi.write("    %20.14f          %20.14f        %20.14f\n" % (row["platform_dir1"], row["platform_dir2"], row["platform_dir3"]) )
                fi.write("     %20.14f     \n" % row["platform_nyquist"] )
                fi.write("     %d     \n" % row["platform_key"] )

# Done with special radial velocity obs back to dumping out time, day, error variance info
            
            fi.write("    %d          %d     \n" % (row["seconds"], row["days"]) )

# Logic for command line override of observational error variances

            if error_dart_fields != None:
            
                try:
                    std_dev = float(obs_error[error_dart_fields.index(row['kind'])][1])
                    variance = std_dev*std_dev
                    fi.write("    %20.14f  \n" % variance )
                except ValueError:
                    fi.write("    %20.14f  \n" % row['error_var'] )
                
            else:
                fi.write("    %20.14f  \n" % row['error_var'] )

            if n % 10000 == 0: print "pyDart/hdf2ascii:  Processed observation # ", n
        
        h5file.close()
        
        fi.close()
        
        print "pyDart/hdf2ascii:  Created ascii DART file, N = ", n
        
        return 0

#-------------------------------------------------------------------------------
    
    def getDartTimes(self, output_file_name=None):

# Open DART PyTables file
        
        h5file, table = open_pyDart_file(self.hdf5, verbose = self.verbose)
        
        n = 0
        date = []
        nobs = []
        for row in table.iterrows():
            if n == 0:
                date.append(row['date'])
                n = 1
            else:
                if date[n-1] != row['date']:
                    date.append(row['date'])
                    n += 1
        
        if output_file_name == None:
            print n
            for d in date:
                print "%s  %s  %s  %s  %s  %s" % (d[0:4],d[5:7],d[8:10],d[11:13],d[14:16],d[17:19])
        else:
            fi = open(output_file_name, "w")
            fi.write("   %d\n" % n )
            for d in date:
                fi.write("%s  %s  %s  %s  %s  %s\n" % (d[0:4],d[5:7],d[8:10],d[11:13],d[14:16],d[17:19]) )
            fi.close()
        
        h5file.close()
        
        return 0

#-------------------------------------------------------------------------------

def read_double_precision_string(the_string):  # We have a double precision scientific string which I can only read this way

      if the_string.count("D") > 0:
#           match      = re.search('-?\d*(\.\d+|)[Dd][+\-]\d\d\d?', the_string)
#           number_str = str(match.group(0))
#       number     = number_str.replace('D','e')
            number     = the_string.replace('D','e')
      else:
            number     = the_string

      return double(number)

#===============================================================================        

# DART FILE HEADER FORMAT  (slashes mean file text...)
#-------------------------------------------------------------------------------
#  /obs_sequence/
#  /obs_kind_defnitions/
#  ob_kinds[int8]
#  ob_kind_definition(1)[int8,str]
#  ob_kind_definition(2)[int8,str]
#       .
#       .
#       .
#       .
#  ob_kind_definition(ob_kinds)[int8,str]
#  /num_copies:/  num_copies[int8]   /num_qc:/  num_qc[int8]
#  /num_obs:   /  num_obs[int8]      /max_num_obs:/  max_num_obs[int8]
#  /observations/
#  /first:/   first[int8]    /last:/  last[int8]
#
#-------------------------------------------------------------------------------
# DART FILE OBSERVATION FORMAT  (slashes mean file text...)
#-------------------------------------------------------------------------------
#   /OBS/   ob_number[int8]
#   value[flt64]
#   prev[int8]  next[int8]  covariance_group[int8]
#   /obdef/
#   /loc3d/
#   ob_lat[flt64]  ob_lon[flt64] ob_height[flt64] ob_vert_coord[int8]
#   /type/
#   ob_type[int8]
#   /platform/              {if ob_type == Doppler_velocity, need to know where radar is...)
#   /loc3d/
#   plat_lat[flt64]  plat_lon[flt64] plat_height[flt64] plat_vert_coord[int8]
#   /dir3d/
#   dir1[flt64], dir2[flt64], dir3[flt64]
#   nyquist[flt32]          {Dowell convention adds in Nyquist information}
#   ob_def_key[int8]
#   seconds[int8]  days[int8]
#   ob_error_variance[flt32]
#

class DART_ob_kinds(IsDescription):
        index               = Int64Col(dflt=long(missing))
        name                = StringCol(255)

class DART_header(IsDescription):
        origin_file         = StringCol(255)
        num_copies          = Int64Col(dflt=long(missing))
        num_qc              = Int64Col(dflt=long(missing))
        qc_descrip          = StringCol(80,dflt="")
        num_obs             = Int64Col(dflt=long(missing))
        max_num_obs         = Int64Col(dflt=long(missing))
        first               = Int64Col(dflt=long(missing))
        last                = Int64Col(dflt=long(missing))

class DART_obs(IsDescription):
        number              = Float64Col(dflt=missing)
        value               = Float64Col(dflt=missing)
        truth               = Float64Col(dflt=missing)
        qc                  = Float64Col(dflt=missing)
        previous            = Float64Col(dflt=missing)
        next                = Float64Col(dflt=missing)
        cov_group           = Float64Col(dflt=missing)
        lat                 = Float64Col(dflt=missing)
        lon                 = Float64Col(dflt=missing)
        height              = Float64Col(dflt=missing)
        vert_coord          = Float64Col(dflt=missing)
        kind                = Int64Col  (dflt=long(missing))
        elevation           = Float64Col(dflt=missing)
        azimuth             = Float64Col(dflt=missing)
        error_var           = Float64Col(dflt=missing)
        seconds             = Int64Col  (dflt=long(missing))
        days                = Int64Col  (dflt=long(missing))
        date                = StringCol (25)
        utime               = Int64Col  (dflt=long(missing))
        index               = Int64Col  (dflt=long(missing))
        x                   = Float64Col(dflt=missing)
        y                   = Float64Col(dflt=missing)
        z                   = Float64Col(dflt=missing)
        platform_lat        = Float64Col(dflt=missing)
        platform_lon        = Float64Col(dflt=missing)
        platform_height     = Float64Col(dflt=missing)
        platform_vert_coord = Int64Col  (dflt=long(missing))
        platform_dir1       = Float64Col(dflt=missing)
        platform_dir2       = Float64Col(dflt=missing)
        platform_dir3       = Float64Col(dflt=missing)
        platform_nyquist    = Float64Col(dflt=missing)
        platform_key        = Int64Col  (dflt=long(missing))

#-------------------------------------------------------------------------------
# Main function defined to return correct sys.exit() calls

def main(argv=None):
    if argv is None:
           argv = sys.argv

# Init some local variables
    
    start = None
    end   = None

# Initialize class object...
    
    myDART = pyDART()

# Command line interface for PyDart
    
    parser = OptionParser()
    parser.add_option("-f", "--file",        dest="file",      type="string", help = "Filename of ascii or PyDART file for conversion/search")
    parser.add_option("-l", "--list",        dest="list",      default=False, help = "Boolean flag to list basic contents of the file",            action="store_true")
    parser.add_option(      "--stats",       dest="stats",     default=False, help = "Gives basic stats for variable, helpful to compare files",            action="store_true")
    parser.add_option("-d", "--dump",        dest="dump",      default=False, help = "Whether to print all values of the search (default = False)",action="store_true")
    parser.add_option(      "--ascii2hdf",   dest="ascii2hdf", default=False, help = "Boolean flag to convert ascii DART file to HDF5 DARTfile",   action="store_true")
    parser.add_option(      "--hdf2ascii",   dest="hdf2ascii", default=False, help = "Boolean flag to convert HDF5 DART file to ascii DART file",  action="store_true")
    parser.add_option(      "--start",       dest="start",     type="int",    help = "Start time of search in YYYYMMDDHHMMSS")
    parser.add_option(      "--end",         dest="end",       type="int",    help = "End time of search in YYYYMMDDHHMMSS")
    parser.add_option(      "--getDartTimes",dest="DartTimes", default=False, help = "Boolean flag to dump out observations times as in getDARTtimes", action="store_true")
    parser.add_option(      "--condition",   dest="condition", default=None,  type = "string", help = "string having following syntax for searches:  '( z1 < height < z2 )'" )
    parser.add_option(      "--variable",    dest="variable",  default=None,  type = "string", help = "String containing the type of observation to list information:  VR, DBZ")
    parser.add_option(      "--plot",        dest="plot",      default=False, help = "Boolean flag to plot observations data", action="store_true")
    parser.add_option(      "--quiet",       dest="verbose",   default=True,  help = "Boolean flag to suppress text print (default=False)", action="store_false")
    parser.add_option(      "--nodebug",     dest="nodebug",   default=False, help = "Boolean flag to suppress debug output (default=False)", action="store_true")
    parser.add_option("-o", "--output",      dest="output",    default=None,  type = "string", help = "Output filename to store output from various programs")
    parser.add_option(      "--radar_loc",   dest="radar_loc", default=None,  type = "float", nargs=2, help = "The lat/lon radar location used in creating X/Y offsets.  Usage:  --radar_loc 35.634 -95.787")
    parser.add_option("-s", "--search",      dest="search",    default=False, help = "Boolean flag to force search of table - use when search does not use time indices", action="store_true")
    parser.add_option(      "--xloc",        dest="xloc",      default=None,  type = "float", nargs=2, help = "Search for obs within these x limits. Usage:  --xloc Xmin Xmax (in km)")
    parser.add_option(      "--yloc",        dest="yloc",      default=None,  type = "float", nargs=2, help = "Search for obs within these y limits. Usage:  --yloc Ymin Ymax (in km)")
    parser.add_option(      "--zloc",        dest="zloc",      default=None,  type = "float", nargs=2, help = "search for obs within these z limits. Usage:  --zloc Zmin Zmax (in km)")
    parser.add_option(      "--obserror",    dest="obserror",  default=None,  type = "string", nargs=2, action="append", help = "Change the stored standard deviation of a observational type.  Usage:  --obserror DBZ 3.0")   
    parser.add_option(      "--correctens",  dest="correctens",default=False, help = "Boolean flag to dump out observed reflectivity to be ingested into correct_ensemble", action="store_true")   
    (options, args) = parser.parse_args()
    
    if options.nodebug:
        print "Turn off debug"
        myDART.debug = False
    
    if options.verbose:
        myDART.verbose = options.verbose
        print '------------------------------------------------------------------------------------'
        print
        print '  ==> BEGIN PROGRAM PYDART ',version_string
        print
        print '------------------------------------------------------------------------------------'
    
    if options.file == None:
        print "\n                HEY YOU!!!! SO NO INPUT FILE IS SUPPLIED, EXITING.... \n "
        parser.print_help()
        print
        sys.exit(1)

    if radar_lat != None and radar_lon != None:
        print "\n    Using radar location defined at top of script file!!!!! \n"
        radar_loc = [N.deg2rad(radar_lat),N.deg2rad(radar_lon)]
    else:
        radar_loc = None
    
    if options.radar_loc != None:
        radar_loc = [N.deg2rad(options.radar_loc[0]),N.deg2rad(options.radar_loc[1])]
    
    if options.ascii2hdf:
        if options.file[-3:] == "out":
            if options.verbose:
                print "\n  PyDart:  converting ASCII DART file:  ", options.file
            myDART.file(filename = options.file)
            myDART.ascii2hdf(radar_loc = radar_loc)
            print "\n PyDart:  Completed convertion, PyDART file:  ", myDART.hdf5
        else:
            print "\n File is not labeled `.out`, please rename file..."
            sys.exit(1)
    
    if options.start != None:           # Convert start flag to tuple for search
        char = str(options.start)
        if len(char) != 14:
            print "pyDart:  Incorrect date/time format for search start (need YYYYMMDDHHMMSSS)"
            print "pyDart:  submitted arg = ", char, " Exiting program"
            sys.exit(1)
        start =  (int(char[0:4]), int(char[4:6]), int(char[6:8]), int(char[8:10]), int(char[10:12]), int(char[12:14]))
        if options.end == None:
            end = (2040,01,01,00,00,00)
        options.search == True
    
    if options.end != None:              # Convert end flag to tuple for search
        char = str(options.end)
        if len(char) != 14:
            print "pyDart:  Incorrect date/time format for search start (need YYYYMMDDHHMMSSS)"
            print "pyDart:  submitted arg = ", char, " Exiting program"
            sys.exit(1)
        end =  (int(char[0:4]), int(char[4:6]), int(char[6:8]), int(char[8:10]), int(char[10:12]), int(char[12:14]))
        if options.start == None:
            start = (1970,01,01,00,00,00)
        options.search == True
    
    if options.start == None and options.end == None and not options.ascii2hdf:              # Convert end flag to tuple for search
        start = (1970,01,01,00,00,00)
        end   = (2040,01,01,00,00,00)
        options.search == True
    
    loc =[]
    if options.xloc != None:
        xloc = list(options.xloc)        # convert to list
        xloc = [x * 1000. for x in xloc] # convert to meters (ugly, but the only for lists)
        xloc.sort()                      # make sure the list is from xmin --> xmax...
        loc.append( "(" + str(xloc[0]) + " <= x )" )
        loc.append( "( x <= " + str(xloc[1]) + ")" )
        options.search = True
    
    if options.yloc != None:
        yloc = list(options.yloc)        # convert to list
        yloc = [y * 1000. for y in yloc] # convert to meters (ugly, but the only for lists)
        yloc.sort()                      # make sure the list is from ymin --> ymax...
        loc.append( "(" + str(yloc[0]) + " <= y )" )
        loc.append( "( y <= " + str(yloc[1]) + ")" )
        options.search = True
        
    if options.zloc != None:
        zloc = list(options.zloc)        # convert to list
        zloc = [z * 1000. for z in zloc] # convert to meters (ugly, but only way for lists)
        zloc.sort()                      # make sure the list is from zmin --> zmax...
        loc.append( "(" + str(zloc[0]) + " <= z )" )
        loc.append( "( z <= " + str(zloc[1]) + ")" )
        options.search = True
    
    if options.search:   # Do a search and return the index of that search
        if options.file[-2:] == "h5":
            myDART.file(filename = options.file)
            myDART.search(variable=options.variable, start = start, end = end, condition=options.condition, loc=loc)
            if options.verbose:
                if myDART.index != None:
                    print "\n pyDart: %d Observations found between %s and %s in file %s " % (len(myDART.index), str(start), str(end), myDART.hdf5)
                else:
                    print "\n pyDart:  No Observations found between %s and %s in file %s " % (str(start), str(end), myDART.hdf5)
                    sys.exit(1)
        else:
            print "\n  pyDart:  ERROR!!  Can only search on HDF5 pyDART file, exiting..."
            sys.exit(1)
    
    if options.list and options.variable == None:
        print
        if myDART.verbose:  print "\n PyDart:  Listing file contents"
        myDART.file(filename = options.file)
        myDART.list()
        print
    
    if options.list and options.variable != None:
        print
        if myDART.verbose:  print "\n PyDart:  Listing information requested about variable:  ", options.variable
        myDART.file(filename = options.file)
        myDART.list(variable = options.variable, dumplength = options.dump)
        print
    
    if options.stats:
        print
        if myDART.verbose:  print "\n PyDart:  Creating stats"
        myDART.file(filename = options.file)
        myDART.stats(variable = options.variable)
        print

    if options.plot:
        print
        if myDART.verbose:  print "\n PyDart:  plotting data"
        myDART.file(filename = options.file)
        myDART.plot(variable = options.variable, savefig=options.output)
        print
    
    if options.hdf2ascii:
        if myDART.index != None:
            if myDART.verbose:  print "\n PyDart:  converting HDF5 DART file:  ", options.file
            myDART.file(filename = options.file)
            myDART.hdf2ascii(obs_error=options.obserror)
            if myDART.verbose:  print "PyDart:  Completed convertion, PyDART file:  ", myDART.ascii
        else:
            print "\n PyDart:  No search indices supplied, so converting entire h5 file to ascii\n"
            myDART.file(filename = options.file)
            myDART.hdf2ascii(obs_error=options.obserror)
   
    if options.DartTimes:
        if options.verbose:
            print
            if myDART.verbose:  print "\n PyDart:  Dumping out getDARTtimes information"
            print
        myDART.file(filename = options.file)
        myDART.getDartTimes(output_file_name = options.output)

    if options.correctens:
        if options.verbose and myDART.verbose:  print "\n PyDart:  Creating correct_ensemble input file from observations\n "
        if options.file[-2:] == "h5":
            myDART.file(filename = options.file)
            myDART.search(variable="DBZ", start = start, end = end, condition=options.condition, loc=loc)
            if options.verbose:
                if myDART.index != None:
                    print "\n pyDart: %d Observations found between %s and %s in file %s " % (len(myDART.index), str(start), str(end), myDART.hdf5)
                else:
                    print "\n pyDart:  No Observations found between %s and %s in file %s " % (str(start), str(end), myDART.hdf5)
                    sys.exit(1)
            myDART.correct_ens_output()
            if options.verbose and myDART.verbose:  print "\n PyDart:  Created correct_ensemble input file from dbz observations \n"
        else:
            print "\n pyDart:  ERROR!!  Can only search on HDF5 pyDART file, exiting..."
            sys.exit(1)

#-------------------------------------------------------------------------------
# Main program for testing...
#
if __name__ == "__main__":
    sys.exit(main())

# End of file
