#!/usr/bin/env python
#
#---------------------------- README ------------------------------
#
#  This script is used to set up a series of objective 
#  analysis files, run the x.oban fortran code
#  and then convert them to pyTables HDF5 format (each)
#  and then can merge them.  For most people who are not using
#  the NSSL EnKF system written by Dowell, Wicker, and Mansell,
#  the last two options are not needed.  No support for this
#  script is provided - but it should be readable and understandable
#  by most people.  The user specifies some information regarding
#  the data and style of file below as well as the output prefix.
#  The oban_input_template is just that - it has a few macros
#  that the main Python script (at the bottom) will substitute
#  for in a loop over the the files - e.g., if you have volumes
#  and you want to analyze the data in 2 or 3D one volume at a time
#  That is how this script is set up to run on a PAR data set
#  from NSSL.  The script assume that the pyDart script is sitting
#  in the same directory as this code runs.  
#
#  Type:  "pyOban_example_script.py --help" to see some options.
#
#---------------------------- README ------------------------------
import matplotlib
import pylab as P
import numpy as N
import sys
from optparse import OptionParser
import os
import pyDart

#------------ USER SUPPLIED INFO ---------------------------------------------------------------------------------------------------
#
#

executable      = "x.oban"
data_dir_head   = "/Volumes/scr/ForayPAR29May/PAR29Maysplit1min"
sweep_file_head = "swp.*"
out_prefix      = "29May_C_2km"

oban_input_template = """
!============================= TOP OF FILE ===========================================
&parameters
   nx = 80,                           ! no. of grid points in x direction
   ny = 50,                           ! no. of grid points in y direction
                                      !
   xmin = -100.0,                     ! coordinates of lower southwest corner (in km)
   ymin = -10.0,                      !     of analysis grid relative to the 
                                      !     origin given by glat, glon, galt
                                      !
   dx = 2.0,                          ! grid spacing in x direction (km)
   dy = 2.0,                          ! grid spacing in y direction (km)
                                      !
   glon = -97.461945,                 ! longitude of grid origin (deg)
   glat = 35.235832,                  ! latitude  of grid origin (deg)
   galt = 0.373,                      ! altitude of grid origin (km MSL)

   rlon = -97.461945,                 ! radar longitude (deg)
   rlat = 35.235832,                  ! radar latitude (deg)
   ralt = 0.373,                      ! radar altitude (km MSL)

! Analysis date and time used to advect sweeps to if the UT/VT NE 0

   cyr = $YEAR                        ! year 
   cmo = $MONTH                       ! month
   cda = $DAY                         ! day
   chr = $HOUR                        ! hour
   cmn = $MINUTE                      ! minute
   cse = $SECOND                      ! second

   ut = 6.0,                          ! Translation velocity  
   vt = 0.0,                          ! IMPORTANT:  setting these will change the time and location of
                                      ! sweep data to the date and time specified in this file
                                      ! 
   day_cor = 0,                       ! offset in days to correct each ray
   sec_cor = 0,                       ! offset in seconds to correct each ray
                                      !
   output_prefix = $OUT_PREFIX,       ! prefix name for output files
                                      !
   analysis_type   = 2,               ! Type of analysis
                                      !   1 == 3D Cartesian
                                      !   2 == 2D sweep-by-sweep
   method = 1,                        ! Interpolation method:
                                      !   1 == Cressman
                                      !   2 == Barnes
   hsp0 = 1.00,                       ! either Cressman horiz. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)
   gamma = 0.3,                       ! gamma parameter for multi-pass Barnes analysis
   npass = 2,                         ! number of passes
                                      !
   minrange = 10.0,                   ! minimum-range threshold (in km, data closer to radar are discarded)
   mincount = 3,                      ! threshold for minimum number of data gates required
                                      !   to produce an objectively-analyzed observation
   minsum   = 0.1,                    ! threshold for minimum sum of weights required to produce
                                      !   an objectively-analyzed observation
                                      !
   nfld = 2,                          ! number of data fields to be gridded
                                      !
   ncgen_command = '/usr/local/netcdf3-32/bin/ncgen', 

                                      ! path/executable for local "ncgen" command
                                      !
   radar_data_format = 2,             ! radar data format: 1=dorade sweep files, 2=netcdf (FORAY)
                                      !
   nswp = -1                          ! controls file I/O:  nswp = -1 ==> read all files in directory at bottom of input file
                                      !                     nswp >  0 ==> read nswp # of filenames at bottom of input file
/
&fields
   fieldnames  = 'DBZ', 'VEL',        ! Names of the fields to be read from the dorade/netcdf radar-data files
   fill_flag   =    1,    0,          ! fill missing field values with a specified value? (0=no, 1=yes)
   fill_value  =  0.0,  0.0,          ! replacement value, if fill_flag==1
   error       =  5.0,  2.0,          ! Observational error standard deviation for the DART observations
/
======= DO NOT EDIT THIS LINE -> MUST START WITH 6 "=" for the oban formatted I/O to find the start of input files
"""
files = [  \
[(2004,5,30,0,50,0),"20040530-004953"],
[(2004,5,30,0,51,0),"20040530-005101"],
[(2004,5,30,0,52,0),"20040530-005203"],
[(2004,5,30,0,53,0),"20040530-005240"],
[(2004,5,30,0,54,0),"20040530-005357"],
[(2004,5,30,0,55,0),"20040530-005459"],
[(2004,5,30,0,56,0),"20040530-005552"],
[(2004,5,30,0,57,0),"20040530-005701"],
[(2004,5,30,0,58,0),"20040530-005802"],
[(2004,5,30,0,59,0),"20040530-005900"],
[(2004,5,30,1, 0,0),"20040530-010001"],
[(2004,5,30,1, 1,0),"20040530-010103"],
[(2004,5,30,1, 2,0),"20040530-010159"],
[(2004,5,30,1, 3,0),"20040530-010301"],
[(2004,5,30,1, 4,0),"20040530-010403"],
[(2004,5,30,1, 5,0),"20040530-010532"],
[(2004,5,30,1, 6,0),"20040530-010608"],
[(2004,5,30,1, 7,0),"20040530-010709"],
[(2004,5,30,1, 8,0),"20040530-010759"],
[(2004,5,30,1,10,0),"20040530-010955"],
[(2004,5,30,1,11,0),"20040530-011057"],
[(2004,5,30,1,12,0),"20040530-011158"],
[(2004,5,30,1,13,0),"20040530-011304"],
[(2004,5,30,1,14,0),"20040530-011406"],
[(2004,5,30,1,15,0),"20040530-011508"],
[(2004,5,30,1,16,0),"20040530-011602"],
[(2004,5,30,1,17,0),"20040530-011704"],
[(2004,5,30,1,18,0),"20040530-011753"],
[(2004,5,30,1,19,0),"20040530-011901"],
[(2004,5,30,1,20,0),"20040530-012003"],
[(2004,5,30,1,21,0),"20040530-012040"],
[(2004,5,30,1,22,0),"20040530-012207"],
[(2004,5,30,1,23,0),"20040530-012309"],
[(2004,5,30,1,24,0),"20040530-012410"],
[(2004,5,30,1,25,0),"20040530-012459"],
[(2004,5,30,1,26,0),"20040530-012601"],
[(2004,5,30,1,27,0),"20040530-012656"],
[(2004,5,30,1,28,0),"20040530-012758"],
[(2004,5,30,1,29,0),"20040530-012900"],
[(2004,5,30,1,30,0),"20040530-012955"],
[(2004,5,30,1,31,0),"20040530-013110"],
[(2004,5,30,1,32,0),"20040530-013214"],
[(2004,5,30,1,33,0),"20040530-013300"],
[(2004,5,30,1,34,0),"20040530-013402"],
[(2004,5,30,1,35,0),"20040530-013451"],
[(2004,5,30,1,36,0),"20040530-013600"],
[(2004,5,30,1,37,0),"20040530-013702"],
[(2004,5,30,1,38,0),"20040530-013739"],
[(2004,5,30,1,39,0),"20040530-013904"],
[(2004,5,30,1,40,0),"20040530-014006"]
]

#
# Run the analysis
#
#-------------------------------------------------------------------------------
# Main program 
# 
if __name__ == "__main__":

    print "\n<<<<<===========================================================================================>>>>>>\n"
    print
    print "                 pyOBAN:  Sets up and runs a set of objective analysis files\n\n"
 
    usage = "usage: %prog [options] arg"
    parser = OptionParser(usage)
    parser.add_option("-r",  "--run",    dest="run",     help="Run the oban code after creating the input deck", action="store_true")
    parser.add_option("-c",  "--convert",dest="convert", help="Run pyDart and convert the ascii files to HDF5 PyTables", action="store_true")
    parser.add_option("-m",  "--merge",  dest="merge",   help="Merge the individual pyDART hdf5 files into a single file", action="store_true")
    (options, args) = parser.parse_args()

    filenames = []   # Create list variable to store filenames for table merge

    for item in files:
        directory = item[1][:]
        oban_input = oban_input_template
        year   = "%4.4d" % (item[0][0])
        month  = "%2.2d" % (item[0][1])
        day    = "%2.2d" % (item[0][2])
        hour   = "%2.2d" % (item[0][3])
        minute = "%2.2d" % (item[0][4])
        second = "%2.2d" % (item[0][5])
        filename = "%s%s%s%s%s%s%s%s" % (out_prefix,"_",year,month,day,hour,minute,second)

        oban_input = oban_input.replace("$OUT_PREFIX", filename)
        oban_input = oban_input.replace("$YEAR", year)
        oban_input = oban_input.replace("$MONTH", month)
        oban_input = oban_input.replace("$DAY", day)
        oban_input = oban_input.replace("$HOUR", hour)
        oban_input = oban_input.replace("$MINUTE", minute)
        oban_input = oban_input.replace("$SECOND", second)

        total_path = os.path.join(data_dir_head,directory,sweep_file_head)
        oban_input = oban_input + ("%s%s%s\n" % ("'",total_path,"'"))
      
        print "\n<<<<<===========================================================================================>>>>>>\n"
      
        f = open(filename+".input","w")
        f.write(oban_input)
        f.close()

        if options.run:
            print "Running oban"
            cmd = executable + (" %s.input" % filename) + (" >& %s.output" % filename)
            print "Running cmd:  %s" % cmd
            os.system(cmd)

        if options.convert:
            print "Running pyDart conversion"
            cmd = "./pyDart.py" + (" -f obs_seq_%s.out --ascii2hdf" % filename) + (" >> %s.output" % filename)
            print "Running cmd:  %s" % cmd
            os.system(cmd)
        
        filenames.append("obs_seq_%s.h5" % filename)

    print "\n<<<<<===========================================================================================>>>>>>\n"

    if options.merge:
        if filenames == []:
            filenames = glob.glob(out_prefix + "*.h5")
        mergefile = filenames[0][:-9] + ".h5"
        print "Running pyDart merge, combined file:  %s" % mergefile
        pyDart.mergeTables(mergefile, filenames)
        print "\n\n                   Checking to see if merged file has VR in it!"
        print
        print "<<<<<===========================================================================================>>>>>>"
        print
        cmd = "./pyDart.py" + (" -f %s --variable VR --list --search" % mergefile)
        print "Running cmd:  %s" % cmd
        os.system(cmd)

    print "\n<<<<<===========================================================================================>>>>>>\n"
