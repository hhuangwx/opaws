#!/bin/csh

#############################################################################
#
# cycle_oban2.csh
#
# For a radar dataset (dorade sweep files), this script
# (1) runs x.oban at regular time intervals, producing the following output
#     a.  NetCDF analysis files
#     b.  text output (oban.out)
#     c.  DART obs_seq.out files
# (2) produces NCL plots.
#
# David Dowell
# 8/31/10  original version for OPAWS1
# 1/10/12 updated version for OPAWS2
#
# Acknowledgments:  Data Assimilation Research Testbed (DART), for date and time processing
#
############################## LICENSE ######################################
#
#   Copyright (C) <2012>  <David C. Dowell and Louis J. Wicker, NOAA>
#
#   This library is free software; you can redistribute it and/or
#   modify it under the terms of the GNU Lesser General Public
#   License as published by the Free Software Foundation; either
#   version 2.1 of the License, or (at your option) any later version.
#
#   This library is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#   Lesser General Public License for more details.
#
############################## LICENSE ######################################


#******************* USER NEEDS TO SPECIFY THE FOLLOWING INFORMATION ***********************

set RADAR = 'KBMX'

set START_YEAR = 2011
set START_MONTH = 04
set START_DAY = 27
set START_HOUR = 23
set START_MIN = 00
set START_SEC = 00

set INTERVAL_SEC = 180

set NUM_TIMES = 20

set SWEEP_DIR = /Volumes/username/27apr11/88D/${RADAR}/dorade
set ANALYSIS_DIR = /Volumes/username/27apr11/88D/${RADAR}/obs_seq

#****************************** END OF USER SPECIFICATIONS *********************************

if ($RADAR == 'KBMX') then
  set RADAR_LINE1 = 'rlon = -86.7697,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 33.1722,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.227,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -86.7697,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 33.1722,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.227,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KDGX') then
  set RADAR_LINE1 = 'rlon = -89.983,                    ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 32.283,                     ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.170,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -89.983,                    ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 32.283,                     ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.170,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KEVX') then
  set RADAR_LINE1 = 'rlon = -85.9214,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 30.5644,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.063,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -85.9214,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 30.5644,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.063,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KFFC') then
  set RADAR_LINE1 = 'rlon = -84.5658,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 33.3636,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.292,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -84.5658,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 33.3636,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.292,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KGWX') then
  set RADAR_LINE1 = 'rlon = -88.3289,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 33.8967,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.165,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -88.3289,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 33.8967,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.165,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KHTX') then
  set RADAR_LINE1 = 'rlon = -86.0833,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 34.9306,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.551,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -86.0833,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 34.9306,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.551,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KJGX') then
  set RADAR_LINE1 = 'rlon = -83.3511,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 32.6753,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.179,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -83.3511,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 32.6753,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.179,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KLIX') then
  set RADAR_LINE1 = 'rlon = -89.8256,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 30.3367,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.037,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -89.8256,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 30.3367,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.037,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KLZK') then
  set RADAR_LINE1 = 'rlon = -92.2622,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 34.8364,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.193,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -92.2622,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 34.8364,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.193,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KMOB') then
  set RADAR_LINE1 = 'rlon = -88.2397,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 30.6794,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.083,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -88.2397,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 30.6794,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.083,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KMRX') then
  set RADAR_LINE1 = 'rlon = -83.4017,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 36.1686,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.428,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -83.4017,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 36.1686,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.428,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KNQA') then
  set RADAR_LINE1 = 'rlon = -89.8733,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 35.3447,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.106,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -89.8733,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 35.3447,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.106,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KOHX') then
  set RADAR_LINE1 = 'rlon = -86.5625,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 36.2472,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.196,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -86.5625,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 36.2472,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.196,                      ! altitude of grid origin (km MSL)'
endif

if ($RADAR == 'KTLH') then
  set RADAR_LINE1 = 'rlon = -84.3289,                   ! radar longitude (deg)'
  set RADAR_LINE2 = 'rlat = 30.3975,                    ! radar latitude (deg)'
  set RADAR_LINE3 = 'ralt = 0.049,                      ! radar altitude (km MSL)'
  set GRID_LINE1  = 'glon = -84.3289,                   ! longitude of grid origin (deg)'
  set GRID_LINE2  = 'glat = 30.3975,                    ! latitude  of grid origin (deg)'
  set GRID_LINE3  = 'galt = 0.049,                      ! altitude of grid origin (km MSL)'
endif

set TEMP_DIR = 'temp_dir_cycle_oban'

set REMOVE = 'rm -rf'
set   MOVE = 'mv -f'

set YEAR = $START_YEAR
set MONTH = $START_MONTH
set DAY = $START_DAY
set HOUR = $START_HOUR
set MIN = $START_MIN
set SEC = $START_SEC

set days_in_month = ( 31 28 31 30 31 30 31 31 30 31 30 31 )


set time_index = 1

if (! -e ${ANALYSIS_DIR}) then
  mkdir -p ${ANALYSIS_DIR}
endif
cd ${ANALYSIS_DIR}

# START MAIN LOOPS

while ($time_index <= $NUM_TIMES)

  if (-e ${TEMP_DIR}) then
    echo 'error:  TEMP_DIR exists'
    exit
  endif
  mkdir ${TEMP_DIR}
  cd ${TEMP_DIR}

  set SWEEP_YEAR = $YEAR
  set SWEEP_MONTH = $MONTH
  set SWEEP_DAY = $DAY
  set SWEEP_HOUR = $HOUR
  set SWEEP_MIN = $MIN
  set SWEEP_SEC = $SEC

  @ SWEEP_SEC = $SWEEP_SEC - $INTERVAL_SEC / 2

  if ( `expr $SWEEP_YEAR \% 4` == 0 ) then
	set days_in_month[2] = 29
  endif
  if ( `expr $SWEEP_YEAR \% 100` == 0 ) then
	if ( `expr $SWEEP_YEAR \% 400` == 0 ) then
	  set days_in_month[2] = 29
	else
	  set days_in_month[2] = 28
	endif
  endif
  while ( $SWEEP_SEC < 0 )
	@ SWEEP_SEC = $SWEEP_SEC + 60
	@ SWEEP_MIN --
	if ( $SWEEP_MIN < 0 ) then
	  @ SWEEP_MIN = $SWEEP_MIN + 60
	  @ SWEEP_HOUR --
	endif
	if ( $SWEEP_HOUR < 0 ) then
	  @ SWEEP_HOUR = $SWEEP_HOUR + 24
	  @ SWEEP_DAY --
	endif
	if ($SWEEP_DAY < 1 ) then
	  @ SWEEP_MONTH --
	  set SWEEP_DAY = $days_in_month[$SWEEP_MONTH]
	endif
	if ($SWEEP_MONTH < 1 ) then
	  set SWEEP_MONTH = 12
	  @ SWEEP_YEAR --
	  if ( `expr $SWEEP_YEAR \% 4` == 0 ) then
		set days_in_month[2] = 29
	  endif
	  if ( `expr $SWEEP_YEAR \% 100` == 0 ) then
		if ( `expr $SWEEP_YEAR \% 400` == 0 ) then
		  set days_in_month[2] = 29
		else
		  set days_in_month[2] = 28
		endif
	  endif
	endif
  end     # while ( $SWEEP_SEC < 0 )
  set SWEEP_SEC = `expr $SWEEP_SEC \+ 100`
  set SWEEP_SEC = `echo $SWEEP_SEC | cut -c2-3`
  set SWEEP_MIN = `expr $SWEEP_MIN \+ 100`
  set SWEEP_MIN = `echo $SWEEP_MIN | cut -c2-3`
  set SWEEP_HOUR = `expr $SWEEP_HOUR \+ 100`
  set SWEEP_HOUR = `echo $SWEEP_HOUR | cut -c2-3`
  set SWEEP_DAY = `expr $SWEEP_DAY \+ 100`
  set SWEEP_DAY = `echo $SWEEP_DAY | cut -c2-3`
  set SWEEP_MONTH = `expr $SWEEP_MONTH \+ 100`
  set SWEEP_MONTH = `echo $SWEEP_MONTH | cut -c2-3`

  set sweep_time_index = 1

  while ($sweep_time_index <= $INTERVAL_SEC)

    if ($SWEEP_YEAR == 2009) then
      set YEAR_ALIAS = 109
    else if ($SWEEP_YEAR == 2010) then
      set YEAR_ALIAS = 110
    else if ($SWEEP_YEAR == 2011) then
      set YEAR_ALIAS = 111
    else if ($SWEEP_YEAR == 2012) then
      set YEAR_ALIAS = 112
    else
      echo 'error:  YEAR_ALIAS not set'
      exit
    endif

    ln -sf ${SWEEP_DIR}/swp.${YEAR_ALIAS}${SWEEP_MONTH}${SWEEP_DAY}${SWEEP_HOUR}${SWEEP_MIN}${SWEEP_SEC}* .

    @ sweep_time_index++
    @ SWEEP_SEC++

    while ( $SWEEP_SEC >= 60 )
      @ SWEEP_SEC = $SWEEP_SEC - 60
      @ SWEEP_MIN ++
      if ( $SWEEP_MIN >= 60 ) then
        @ SWEEP_MIN = $SWEEP_MIN - 60
        @ SWEEP_HOUR ++
      endif
      if ( $SWEEP_HOUR >= 24 ) then
        @ SWEEP_HOUR = $SWEEP_HOUR - 24
        @ SWEEP_DAY ++
      endif
      if ($SWEEP_DAY > $days_in_month[$SWEEP_MONTH] ) then
        set SWEEP_DAY = 1
        @ SWEEP_MONTH ++
      endif
      if ($SWEEP_MONTH > 12 ) then
        set SWEEP_MONTH = 1
        @ SWEEP_YEAR ++
        if ( `expr $SWEEP_YEAR \% 4` == 0 ) then
          set days_in_month[2] = 29
        endif
        if ( `expr $SWEEP_YEAR \% 100` == 0 ) then
          if ( `expr $SWEEP_YEAR \% 400` == 0 ) then
            set days_in_month[2] = 29
          else
            set days_in_month[2] = 28
          endif
        endif
      endif
    end     # while ( $SWEEP_SEC >= 60 )

    set SWEEP_SEC = `expr $SWEEP_SEC \+ 100`
    set SWEEP_SEC = `echo $SWEEP_SEC | cut -c2-3`
    set SWEEP_MIN = `expr $SWEEP_MIN \+ 100`
    set SWEEP_MIN = `echo $SWEEP_MIN | cut -c2-3`
    set SWEEP_HOUR = `expr $SWEEP_HOUR \+ 100`
    set SWEEP_HOUR = `echo $SWEEP_HOUR | cut -c2-3`
    set SWEEP_DAY = `expr $SWEEP_DAY \+ 100`
    set SWEEP_DAY = `echo $SWEEP_DAY | cut -c2-3`
    set SWEEP_MONTH = `expr $SWEEP_MONTH \+ 100`
    set SWEEP_MONTH = `echo $SWEEP_MONTH | cut -c2-3`

  end     # while ($sweep_time_index <= $INTERVAL_SEC)

  cd ..

  cat >! oban.nml <<EOF
&parameters
   ${RADAR_LINE1}
   ${RADAR_LINE2}
   ${RADAR_LINE3}

   ${GRID_LINE1}
   ${GRID_LINE2}
   ${GRID_LINE3}

   nx = 111,                          ! no. of grid points in x direction
   ny = 111,                          ! no. of grid points in y direction
   nz = 1,                            ! no. of grid points in z direction
                                      !
   xmin = -330.0,                     ! coordinates of lower southwest corner (in km)
   ymin = -330.0,                     !     of analysis grid relative to the 
   zmin = 0.0,                        !     origin given by glat, glon, galt
                                      !
   dx = 6.0,                          ! grid spacing in x direction (km)
   dy = 6.0,                          ! grid spacing in y direction (km)
   dz = 1.0,                          ! grid spacing in z direction (km)
                                      !
   map_proj = 0,                      ! map projection (for relating lat, lon to x, y):
                                      !   0 = flat earth
                                      !   1 = oblique azimuthal (not implemented...)
                                      !   2 = Lambert conformal (not implemented...)
                                      !
   output_prefix = '${RADAR}.${YEAR}_${MONTH}_${DAY}_${HOUR}_${MIN}_${SEC}',        ! prefix name for output files
                                      !
   output_vis5d = .false.             ! .true. if Vis5D output should be created
   output_dart = .true.               ! .true. if DART output should be created
                                      !
   analysis_type = 2,                 ! Type of analysis
                                      !   1 == 3D Cartesian
                                      !   2 == 2D sweep-by-sweep
   method = 1,                        ! Interpolation method:
                                      !   1 == Cressman
                                      !   2 == Barnes
   hsp0 = 2.120,                      ! either Cressman horizontal radius of influence or Barnes smoothing parameter (1.33*dx), in km
   vsp0 = 2.120,                      ! either Cressman vertical radius of influence or Barnes smoothing parameter (1.33*dx), in km
   npass = 1,                         ! number of passes
                                      !
   cm_use_clutter_mask = .true.,      ! .true. if clutter mask produced by clutter_stats should be implemented
   cm_min_fixed_clutter_freq = 0.50,  ! minimum frequency (0.0 - 1.0) of fixed clutter from clutter_stats that will be considered clutter
   cm_min_moving_clutter_freq = 1.01, ! minimum frequency (0.0 - 1.0) of moving clutter from clutter_stats that will be considered clutter
   cm_halo = 1,                       ! number of additional neighboring bins in each direction to search for maximum in clutter frequency
   cm_use_refl_test = .true.,         ! .true. if clutter masking should be conditional on difference between observed and mean reflectivity 
   cm_refl_fname = 'REF',             ! name of reflectivity field in input radar data
   cm_refl_exceedance = 20.0,         ! reflectivity exceedance above mean (dBZ) for which clutter mask is ignored
   cm_min_obs = 20,                   ! minimum number of observations for identifying clutter
   cm_min_sweeps = 20,                ! minimum number of sweeps for identifying clutter
   cm_ncfile = '../dorade/${RADAR}_clutter_stats.nc',     ! netcdf file that contains clutter-mask information (output from clutter_stats)
                                      !
   use_clear_air_type = .true.        ! .true. if clear-air reflectivity ob. type should be used for DART output
   clear_air_skip = 1                 ! thinning factor for clear-air reflectivity data
                                      !
   height_max = 15.0                  ! height (km above grid origin) above which observations are discarded
                                      !
   minrange = 5.0,                    ! minimum-range threshold (in km, data closer to radar are discarded)
   mincount = 1,                      ! threshold for minimum number of data gates required
                                      !   to produce an objectively-analyzed observation
   minsum   = 0.1,                    ! threshold for minimum sum of weights required to produce
                                      !   an objectively-analyzed observation
                                      !
   allow_extrapolation = .true.       ! should extrapolation be allowed?
                                      !   .true. for standard objective analysis, .false. for interpolation only
                                      !
   radar_data_format = 1,             ! radar data format: 1=dorade sweep files, 2=netcdf (FORAY)
                                      !
   nfld = 2,                          ! number of data fields to be gridded
                                      !
   nswp = -1                          ! controls file I/O:  nswp = -1 ==> read all files in directory at bottom of input file
                                      !                     nswp >  0 ==> read nswp # of filenames at bottom of input file
/
&fields
   fieldnames  = 'REF', 'VEL',        ! Names of the fields to be read from the dorade/netcdf radar-data files
   fill_flag   =     1,     0,        ! fill missing field values with a specified value? (0=no, 1=yes)
   fill_value  =   0.0,   0.0,        ! replacement value, if fill_flag==1
                                      !
   unfold_flag =     0,     1,        ! locally unfold field? (0=no, 1=yes)
                                      !
   error       =   5.0,   2.0,        ! Observational error standard deviation for the DART observations
/
======= DO NOT EDIT THIS LINE -> MUST START WITH 6 "=" for the oban formatted I/O to find the start of input files
'${TEMP_DIR}/swp*'

EOF


  x.oban oban.nml >& oban.out.${RADAR}_${YEAR}_${MONTH}_${DAY}_${HOUR}_${MIN}_${SEC}

  set plot_type = 1
  set plot_var = "REF"

  while ($plot_type <= 2)

    cat >! plot_oban_results.ncl <<EOF2
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"

begin

; -------------------------------------------------------------------------------
; Parameters needed to run the script

; Note:  To animate fields at more than one time, list multiple netcdf file names in the "files" array.

  files          = (/ \                                                          ; netcdf files
                      "${RADAR}.${YEAR}_${MONTH}_${DAY}_${HOUR}_${MIN}_${SEC}.nc"  \
                   /)

  input_format = 2    ; 1=OPAWS 1.x,    2=OPAWS 2.x
  pass = 1

; Note:  To plot fields from more than one radar scans, list multiple levels in the "heights" array.
;        To plot fields at all levels, set first array element to -999.

  heights = (/ -999. /)

  title          = "${RADAR}"      ; title of plot
  wks            = "pdf"           ; workstation:  x11, ps, eps, epsi, ncgm, or pdf
  plotfile       = "plot"
  xmin           = -999.           ; minimum x of plotting window (-999. for west side of domain)
  xmax           = -999.           ; maximum x of plotting window (-999. for east side of domain)
  ymin           = -999.           ; minimum y of plotting window (-999. for south side of domain)
  ymax           = -999.           ; maximum y of plotting window (-999. for north side of domain)

  if ("${plot_var}" .eq. "REF") then
    color          = 2             ; 0=gray, 1=color, 2=reflectivity
    minspvars      = 0.1           ; minimum contour interval
  end if

  if ("${plot_var}" .eq. "VEL") then
    color          = 1             ; 0=gray, 1=color, 2=reflectivity
    minspvars      = 0.1           ; minimum contour interval
  end if

  setfileoption ("nc", "MissingToFillValue", False)

  n_files = dimsizes(files)
  f              = addfiles(files,"r")   ; netcdf files

; -------------------------------------------------------------------------------
; Determine maximum value of contoured field.

  print("Determining maximum value of contoured field....")

  maxval = minspvars

  do file_index = 0, n_files-1

    z_plot = heights
    if (heights(0) .eq. -999.) then
      delete(z_plot)
      z_plot = f[file_index]->z
    end if
    n_heights = dimsizes(z_plot)

    z_file   = f[file_index]->z

    do height_index = 0, n_heights-1

      n        = ind(z_file .le. z_plot(height_index))
      nd       = dimsizes(n)
      zindex   = nd(0)-1
      delete(n)
      delete(nd)

      if (input_format .eq. 1) then
        cntr_var = f[file_index]->${plot_var}(0,zindex,:,:)
      end if
      if (input_format .eq. 2) then
        cntr_var = f[file_index]->${plot_var}(0,pass-1,zindex,:,:)
      end if

      delete(cntr_var@_FillValue)
      cntr_var@_FillValue = -32768

      maxval1 = max(fabs(cntr_var))

      if (.not.ismissing(maxval1)) then
        if ( maxval1 .gt. maxval ) then
          maxval = maxval1
        end if
      end if

      delete(cntr_var)

    end do

    delete(z_plot)
    delete(z_file)

  end do

; -------------------------------------------------------------------------------
; Open workstations with a detailed name

  vwks           = gsn_open_wks(wks,plotfile)

; -------------------------------------------------------------------------------
; Loop over number of files

  do file_index = 0, n_files-1

    analysis_type = f[file_index]->analysis_type

;-----------------------------------------------------------------------------------
; Loop over number of heights

    z_plot = heights
    if (heights(0) .eq. -999.) then
      delete(z_plot)
      z_plot = f[file_index]->z
    end if
    n_heights = dimsizes(z_plot)

    z_file   = f[file_index]->z

    do height_index = 0, n_heights-1

      n        = ind(z_file .le. z_plot(height_index))
      nd       = dimsizes(n)
      zindex   = nd(0)-1
      delete(n)
      delete(nd)

      print("Input   variable is:  ${plot_var}")
      print("Height index:  " + zindex)

;-----------------------------------------------------------------------------------
; Get data

      if (input_format .eq. 1) then
        cntr_var = f[file_index]->${plot_var}(0,zindex,:,:)
      end if
      if (input_format .eq. 2) then
        cntr_var = f[file_index]->${plot_var}(0,pass-1,zindex,:,:)
      end if
      el_array = f[file_index]->EL(0,zindex,:,:)

      delete(cntr_var@_FillValue)
      cntr_var@_FillValue = -32768
      el_array@_FillValue = -32768

      el_avg = avg(el_array)

;     Change specific values?
      if (False) then
        dims = dimsizes(cntr_var)
        cv_1d = ndtooned(cntr_var)
        i = ind(cv_1d.eq.0.0)
        if (dimsizes(i) .gt. 1) then
          cv_1d(i) = -20.0
        end if
        delete(cntr_var)
        cntr_var = onedtond(cv_1d, dims)
        delete(dims)
        delete(cv_1d)
        delete(i)
      end if  

;     Change specific values?
      if (False) then
        dims = dimsizes(cntr_var)
        cv_1d = ndtooned(cntr_var)
        i = ind(cv_1d.gt.-20.0)
        if (dimsizes(i) .gt. 1) then
          cv_1d(i) = 0.0
        end if
        delete(cntr_var)
        cntr_var = onedtond(cv_1d, dims)
        delete(dims)
        delete(cv_1d)
        delete(i)
      end if  

;     Replace missing data with a specified value?
      if (False) then
        dims = dimsizes(cntr_var)
        cv_1d = ndtooned(cntr_var)
        i = ind(ismissing(cv_1d))
        if (dimsizes(i) .gt. 1) then
          cv_1d(i) = 0.0
        end if
        delete(cntr_var)
        cntr_var = onedtond(cv_1d, dims)
        delete(dims)
        delete(cv_1d)
        delete(i)
      end if  

      dims = dimsizes(cntr_var)
      nx = dims(1)
      ny = dims(0)

      xaxis     = f[file_index]->x
      yaxis     = f[file_index]->y
      zaxis     = f[file_index]->z
      date_char = f[file_index]->start_date
      date = charactertostring(date_char)
      time_char = f[file_index]->start_time
      time = charactertostring(time_char)
  
;-----------------------------------------------------------------------------------
; Define color table

      resources = True

      if (color .eq. 0) then

       cmap = (/"(/1.000,  1.000,  1.000/)", \   
                "(/0.000,  0.000,  0.000/)", \
                "(/0.13,  0.13,  0.13/)", \
                "(/0.19,  0.19,  0.19/)", \
                "(/0.21,  0.21,  0.21/)", \
                "(/0.24,  0.24,  0.24/)", \
                "(/0.34,  0.34,  0.34/)", \
                "(/0.46,  0.46,  0.46/)", \
                "(/0.60,  0.60,  0.60/)", \
                "(/0.74,  0.74,  0.74/)", \
                "(/0.92,  0.92,  0.92/)", \
                "(/0.92,  0.92,  0.92/)", \
                "(/0.74,  0.74,  0.74/)", \
                "(/0.60,  0.60,  0.60/)", \
                "(/0.46,  0.46,  0.46/)", \
                "(/0.34,  0.34,  0.34/)", \
                "(/0.24,  0.24,  0.24/)", \
                "(/0.21,  0.21,  0.21/)", \
                "(/0.19,  0.19,  0.19/)", \
                "(/0.13,  0.13,  0.13/)" /)
  
        gsn_define_colormap(vwks,cmap)

        ncontlvls      = 19
        mnmxint = nice_mnmxintvl( -maxval, maxval, ncontlvls, False)

        resources@gsnContourNegLineDashPattern = 1
    
      end if

      if( color .eq. 1 ) then

       cmap = (/"(/1.000,  1.000,  1.000/)", \   
                "(/0.000,  0.000,  0.000/)", \
                "(/0.142,  0.000,  0.850/)", \
                "(/0.097,  0.112,  0.970/)", \
                "(/0.160,  0.342,  1.000/)", \
                "(/0.240,  0.531,  1.000/)", \
                "(/0.340,  0.692,  1.000/)", \
                "(/0.460,  0.829,  1.000/)", \
                "(/0.600,  0.920,  1.000/)", \
                "(/0.740,  0.978,  1.000/)", \     
                "(/0.920,  1.000,  1.000/)", \
                "(/1.000,  1.000,  0.920/)", \
                "(/1.000,  0.948,  0.740/)", \
                "(/1.000,  0.840,  0.600/)", \
                "(/1.000,  0.676,  0.460/)", \
                "(/1.000,  0.472,  0.340/)", \
                "(/1.000,  0.240,  0.240/)", \
                "(/0.970,  0.155,  0.210/)", \
                "(/0.850,  0.085,  0.187/)", \
                "(/0.650,  0.000,  0.130/)" /)

        gsn_define_colormap(vwks, cmap)

        ncontlvls = 19
        mnmxint = nice_mnmxintvl( -maxval, maxval, ncontlvls, False)

      end if

      if( color .eq. 2 ) then 
       cmap = (/"(/1.000,1.000,1.000/)", \   
                "(/0.000,0.000,0.000/)", \
                "(/1.000,1.000,1.000/)", \
                "(/1.000,1.000,1.000/)", \
                "(/0.023,0.270,0.629/)", \
                "(/0.332,0.059,0.922/)", \
                "(/0.312,0.590,0.621/)", \
                "(/0.000,0.523,0.000/)", \
                "(/0.328,0.691,0.328/)", \
                "(/0.750,0.863,0.750/)", \
                "(/0.938,0.859,0.352/)", \     
                "(/0.938,0.688,0.078/)", \
                "(/0.738,0.516,0.191/)", \
                "(/0.555,0.359,0.250/)", \
                "(/0.965,0.312,0.500/)", \
                "(/0.832,0.016,0.184/)", \
                "(/1.000,0.098,0.500/)" /)
        gsn_define_colormap(vwks, cmap)

        ncontlvls = 15

        maxval = 65.
        minval = 0.0
        mnmxint = nice_mnmxintvl( minval, maxval, ncontlvls, False)
      end if

;-----------------------------------------------------------------------------------
; Set NCL parameters.

      resources@lbTitleString            = "${plot_var}"
      resources@tiMainString             = title

; Define plot range
      resources@sfXArray                 = xaxis
      resources@sfYArray                 = yaxis

      if ( xmin .eq. -999. ) then
        xmin = xaxis(0)
      end if
      resources@sfXCStartSubsetV = xmin
  
      if ( xmax .eq. -999. ) then
        xmax = xaxis(nx-1)
      end if
      resources@sfXCEndSubsetV = xmax

      if ( ymin .eq. -999. ) then
        ymin = yaxis(0)
      end if
      resources@sfYCStartSubsetV = ymin
 
      if ( ymax .eq. -999. ) then
        ymax = yaxis(ny-1)
      end if
      resources@sfYCEndSubsetV = ymax

;
      resources@gsnShape                 = True

; Define plot colors and lines
      resources@cnMonoFillPattern        = True
      resources@cnFillOn                 = True       ; Turn on contour fill.
      resources@gsnSpreadColors          = True       ; use full colormap
      resources@cnInfoLabelOn            = True       ; Contour info label.
      resources@cnLinesOn                = True       ; Contour lines
      resources@cnLineLabelsOn           = False      ; Line labels.
      resources@cnLevelSelectionMode     = "ManualLevels" ; manual levels
;      resources@gsnPanelXWhiteSpacePercent = 0.001
;      resources@gsnPanelYWhiteSpacePercent = 0.001

      resources@gsnContourZeroLineThicknessF = 0.0     ; Thickness of zero contour (default 1.0)

      resources@cnMinLevelValF           = mnmxint(0)
      resources@cnMaxLevelValF           = mnmxint(1)
      resources@gsnSpreadColorStart = 2

;     Define the plot label bar
      resources@pmLabelBarDisplayMode    = "Always"   ; Turn on a label bar.
      resources@pmLabelBarWidthF         = 0.1        ; control size of colorbar 
      resources@pmLabelBarHeightF        = 0.4        
      resources@pmLabelBarParallelPosF   = 0.35
      resources@pmLabelBarOrthogonalPosF = 0.02       ; position wrt plot
      resources@lbTitleFontHeightF       = 0.015      ; label bar title font size
      resources@lbTitleFont              = 21         ; label bar title font
      resources@lbPerimOn                = False      ; Turn off label bar perim.
      resources@lbLabelFontHeightF       = 0.015      ; label bar font size
      resources@lbLabelFont              = 21         ; label bar font
      resources@lbOrientation            = "vertical" ; label bar orientation
      resources@lbPerimOn                = False      ; no box around label bar
      resources@lbAutoManage             = False      ; we control how drawn not plot
      resources@lbLabelAutoStride        = True       ; Avoid overlapping labels
      resources@gsnDraw                  = False
      resources@gsnFrame                 = False

; Resources for the label
      txres                              = True
      txres@txFont                       = 21
      txres@txFontHeightF                = 0.016
      resources@gsnScale                 = True
      resources@vpXF                     = 0.1        ; Change the size and location of the
      resources@vpYF                     = 0.9        ; plot on the viewport.
      resources@vpWidthF                 = 0.7
      resources@vpHeightF                = 0.7
      resources@tmXBLabelsOn            = True



      if (mnmxint(2) .ge. minspvars) then

        maxstr=sprintf("%3.2f",max(cntr_var))
        minstr=sprintf("%3.2f",min(cntr_var))
        resources@cnLevelSpacingF  = mnmxint(2)

      else ; small enough to pretend it is a null field
 
;        print("WARNING:  Input min/max/cint values  are: " + mnmxint)
;        print("WARNING:  Input minimum contour value is: " + minspvars)
;        print("WARNING:  setting scalar field to zero...")

        maxstr=sprintf("%3.2f",max(cntr_var))
        minstr=sprintf("%3.2f",min(cntr_var))
        resources@cnLevelSpacingF  = minspvars
;        cntr_var(:,:) = 0.

      end if

;-------------------------------------------------------------------------------
; Actual Plotting

; Format the labels

      resources@lbLabelStrings   = sprintf("%3.1f",zaxis(zindex)) 

      plot = gsn_contour(vwks,cntr_var,resources); Draw plot

      gsn_text_ndc(vwks,"DATE",0.12,0.95,txres)
      gsn_text_ndc(vwks,date,0.22,0.95,txres)
      gsn_text_ndc(vwks,"TIME",0.12,0.92,txres)
      gsn_text_ndc(vwks,time,0.22,0.92,txres)
      gsn_text_ndc(vwks,"MAX",0.85,0.84,txres)
      gsn_text_ndc(vwks,maxstr,0.85,0.82,txres)
      gsn_text_ndc(vwks,"MIN",0.85,0.79,txres)
      gsn_text_ndc(vwks,minstr,0.85,0.77,txres)

      if (analysis_type .eq. 1) then     ; 3D
        hgtstr=sprintf("%5.3f",zaxis(zindex))
        gsn_text_ndc(vwks,"HGT",0.85,0.74,txres)
        gsn_text_ndc(vwks,hgtstr+" km",0.85,0.72,txres)
      end if
      if ( (analysis_type .eq. 2) .and. .not.ismissing(el_avg) ) then     ; 2D
        el_string = sprintf("%4.1f",el_avg)
        gsn_text_ndc(vwks,"EL",0.85,0.74,txres)
        gsn_text_ndc(vwks,el_string,0.85,0.72,txres)
      end if

      draw(plot)
      frame(vwks)
      delete(plot)

    end do      ;  do height_index = 0, n_heights-1

    delete(z_plot)
    delete(z_file)

  end do      ; do file_index = 0, n_files-1

  delete(f)
  delete(vwks)

  print("")
  print("Plotting completed")
  print("")

end
EOF2

    ncl plot_oban_results.ncl

    ${MOVE} plot.pdf ${plot_var}_${YEAR}_${MONTH}_${DAY}_${HOUR}_${MIN}_${SEC}.pdf

    @ plot_type ++
    set plot_var = "VEL"

  end    # while ($plot_type <= 2)

  @ time_index++

  @ SEC = $SEC + $INTERVAL_SEC

  if ( `expr $YEAR \% 4` == 0 ) then
    set days_in_month[2] = 29
  endif
  if ( `expr $YEAR \% 100` == 0 ) then
    if ( `expr $YEAR \% 400` == 0 ) then
      set days_in_month[2] = 29
    else
      set days_in_month[2] = 28
    endif
  endif
  while ( $SEC >= 60 )
    @ SEC = $SEC - 60
    @ MIN ++
    if ( $MIN >= 60 ) then
      @ MIN = $MIN - 60
      @ HOUR ++
    endif
    if ( $HOUR >= 24 ) then
      @ HOUR = $HOUR - 24
      @ DAY ++
    endif
    if ($DAY > $days_in_month[$MONTH] ) then
      set DAY = 1
      @ MONTH ++
    endif
    if ($MONTH > 12 ) then
      set MONTH = 1
      @ YEAR ++
      if ( `expr $YEAR \% 4` == 0 ) then
        set days_in_month[2] = 29
      endif
      if ( `expr $YEAR \% 100` == 0 ) then
        if ( `expr $YEAR \% 400` == 0 ) then
          set days_in_month[2] = 29
        else
          set days_in_month[2] = 28
        endif
      endif
    endif
  end     # while ( $SEC >= 60 )
  set SEC = `expr $SEC \+ 100`
  set SEC = `echo $SEC | cut -c2-3`
  set MIN = `expr $MIN \+ 100`
  set MIN = `echo $MIN | cut -c2-3`
  set HOUR = `expr $HOUR \+ 100`
  set HOUR = `echo $HOUR | cut -c2-3`
  set DAY = `expr $DAY \+ 100`
  set DAY = `echo $DAY | cut -c2-3`
  set MONTH = `expr $MONTH \+ 100`
  set MONTH = `echo $MONTH | cut -c2-3`


  ${REMOVE} ${TEMP_DIR}


end     # while ($time_index <= $NUM_TIMES)




