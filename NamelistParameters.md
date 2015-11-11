The UNIX command-line syntax for running the _x.oban_ objective-analysis program is

**x.oban oban.nml**

Note that _oban.nml_ is the example file name that will be used here.  Users are free to choose other file names.

The _oban.nml_ file must include three sections:  a Fortran namelist called _&parameters_ (general information about the desired objective analysis), a Fortran namelist called _&fields_ (information specific to individual data fields), and a list of sweep-file names.  These sections of the _oban.nml_ file are described in more detail below.

# &parameters #

**required integer parameters:**
```
nx                  ! number of grid points in x direction
ny                  ! number of grid points in y direction
nz                  ! number of grid points in z direction (required for 3D analysis, not required for 2D analysis)

analysis_type       ! Type of analysis
                    !   1 = 3D Cartesian
                    !   2 = 2D sweep-by-sweep

method              ! Interpolation method:
                    !   1 = Cressman
                    !   2 = Barnes

radar_data_format   ! radar data format: 1=dorade sweep files, 2=Foray NetCDF

nfld                ! number of data fields to be gridded

nswp                ! controls file I/O:  nswp = -1 ==> read all files corresponding to path name at bottom of input file
                    !                     nswp >  0 ==> read nswp # of filenames at bottom of input file
```

**required real parameters:**
```
xmin                ! x coordinate (km) of west grid edge, relative to the origin given by glat, glon
ymin                ! y coordinate (km) of south grid edge, relative to the origin given by glat, glon
zmin                ! z coordinate (km) of grid bottom, relative to galt (required for 3D analysis, not required for 2D analysis)

dx                  ! grid spacing (km) in x direction
dy                  ! grid spacing (km) in y direction
dz                  ! grid spacing (km) in z direction (required for 3D analysis, not required for 2D analysis)

glon                ! longitude (deg) of grid origin
glat                ! latitude (deg) of grid origin
galt                ! altitude (km MSL) of grid origin

hsp0                ! either Cressman horiz. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)

vsp0                ! either Cressman vert. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2) (not required for 2D analysis)

gamma               ! convergence parameter for multi-pass Barnes analysis (not required for Cressman and/or single-pass Barnes analysis)
```

**required character (string) parameters:**
```
output_prefix       ! prefix name for output files
```

**optional integer parameters:**
```
map_proj            ! map projection (for relating lat, lon to x, y)  [default: 0]
                    !   0 = flat earth for horizontal distance calculations, 4/3 earth radius for height calculations
                    !   1 = oblique azimuthal (not implemented...)
                    !   2 = Lambert conformal (not implemented...)

cyr                 ! year in reference date used for time-to-space conversion  [default:  year in first sweep file]
cmo                 ! month in reference date used for time-to-space conversion  [default:  month in first sweep file]
cda                 ! day in reference date used for time-to-space conversion  [default:  day in first sweep file]
chr                 ! hour in reference time used for time-to-space conversion  [default:  hour in first sweep file]
cmn                 ! minute in reference time used for time-to-space conversion  [default:  minute in first sweep file]
cse                 ! second in reference time used for time-to-space conversion  [default:  second in first sweep file]

year_cor            ! offset in years to correct each ray  [default: 0]
day_cor             ! offset in days to correct each ray  [default: 0]
sec_cor             ! offset in seconds to correct each ray  [default: 0]

az_corr_flag        ! method of additional azimuth-angle correction  [default: 0]
                    !   0 = none
                    !   1 = average current and previous angle

npass               ! number of objective-analysis passes  [default:  1]

clear_air_skip      ! thinning factor for clear-air reflectivity data  [default: 0]

mincount            ! threshold for minimum number of observations required
                    !   to produce an objectively-analyzed value  [default:  1]
```

**optional real parameters:**
```
rlon                ! radar longitude (deg)  [default: value obtained from radar-data sweep file]
rlat                ! radar latitude (deg)  [default: value obtained from radar-data sweep file]
ralt                ! radar altitude (km MSL)  [default: value obtained from radar-data sweep file]
                           
ut                  ! x-direction translation velocity (m/s) used for time-to-space conversion  [default: 0.0]
vt                  ! y-direction translation velocity (m/s) used for time-to-space conversion  [default: 0.0]
                    ! If ut and/or vt is nonzero, then each observation time is reset to the reference time after the time-to-space conversion.

elcor               ! elevation-angle offset (deg)  [default: 0.0]

azcor               ! azimuth-angle offset (deg)  [default: 0.0]

                    ! The following 3 parameters are for the Steiner and Smith (2002, J. Atmos. Oceanic Technol.) 
                    !   algorithm for removing echoes not associated with precipitation. (not currently implemented)
refl_thresh         ! reflectivity threshold (dBZ) for first two steps in Steiner and Smith (2002) algorithm  [default:  5.0]
refl_fluc           ! reflectivity fluctuation (dBZ) for spin-change portion of algorithm  [default:  2.0]
grad_thresh         ! reflectivity gradient (dBZ/degree) for vertical-gradient portion of algorithm  [default:  10.0]

cm_min_refl_avail   ! minimum reflectivity availability (%) for clutter mask  [default:  75.0]
cm_min_refl         ! minimum reflectivity (dBZ) for clutter mask  [default:  -5.0]
cm_max_refl_sd      ! maximum reflectivity standard deviation (dBZ) for clutter mask  [default:  10.0]
cm_refl_exceedance  ! reflectivity exceedance (dBZ) for which standard clutter mask is ignored  [default:  20.0]
cm_min_vel_sd       ! minimum velocity standard deviation (m/s) for highway detection in clutter mask  [default:  7.5]
cm_max_vel          ! maximum velocity absolute magnitude (m/s) for standard clutter detection in clutter mask  [default:  2.0]
cm_max_vel_sd       ! maximum velocity standard deviation (m/s) for standard clutter detection in clutter mask  [default:  2.0]

height_max          ! height (km above grid origin) above which observations are discarded  [default:  no height thresholding]

minrange            ! minimum-range threshold (km); data closer to radar are discarded  [default: 0.0]

minsum              ! threshold for minimum sum of weights required to produce
                    !   an objectively-analyzed value  [default:  0.0]
```

**optional logical parameters:**
```
umass_data          ! .true. if the subroutine for UMass radar-data corrections should be used  [default:  .false.]

output_beam_info    ! .true. if beam information (NetCDF) should be output  [default:  .true.]
output_dart         ! .true. if DART obs_seq.out output should be created  [default:  .true.]
output_netcdf       ! .true. if NetCDF analysis output should be created  [default:  .true.]
output_vis5d        ! .true. if Vis5D output should be created  [default:  .true.]

                       ! The following 4 parameters are for the Steiner and Smith (2002, J. Atmos. Oceanic Technol.) 
                       !   algorithm for removing echoes not associated with precipitation. (not currently implemented)
remove_gc_refl_thresh  ! true. if ground clutter should be removed based on reflectivity threshold [default:  .false.]
remove_gc_echo_top     ! true. if ground clutter should be removed based on echo top [default:  .false.]
remove_gc_spin_change  ! true. if ground clutter should be removed based on spin change [default:  .false.]
remove_gc_vert_grad    ! true. if ground clutter should be removed based on vertical gradient [default:  .false.]

use_clutter_mask    ! .true. if clutter mask based on ppi stats should be implemented [default:  .false.]

use_clear_air_type  ! .true. if clear-air reflectivity ob. type should be used for DART output  [default:  .false.]

allow_extrapolation ! .true. if extrapolation should be allowed  [default:  .true.]
                    ! .true. corresponds to a standard objective analysis
                    ! .false. means that existence of observations in each octant around a gridpoint is required to produce an objectively-analyzed value
```

**optional character (string) parameters:**
```
cm_ncfile           ! netcdf "ppi stats" file name for clutter masking (required if use_clutter_mask is .true.)  [default:  none]
cm_refl_fname       ! name of reflectivity field in current analysis  [default:  "REF"]
cm_refl_fname_ppi   ! name of reflectivity field in "ppi stats" file  [default:  "REF"]
cm_vel_fname_ppi    ! name of velocity field in "ppi stats" file  [default:  "VEL"]

ncgen_command       ! path/executable for local "ncgen" command  [default:  "ncgen"]
```


# &fields #

**required real parameters:**
```
error               ! list of observational error standard deviations (e.g., 5.0, 2.0) for each field  [default:  none]
                    ! (required if output_dart is .true.)
```

**required character (string) parameters:**
```
fieldnames          ! list of names of the fields (e.g., 'DZ', 'VU') to be read from the dorade/netcdf radar-data files
```

**optional integer parameters:**
```
fill_flag           ! list of flags for each field (e.g., 1, 0) to indicate whether missing values should be replaced with a specified value (0=no, 1=yes)  [default:  0]

unfold_flag         ! list of flags for each field (e.g., 0, 1) to indicate whether velocity data should be locally unfolded  [default:  0]
                    ! 1=yes, 0=no

                      ! These are extra filters for the data.  The thresholding is done on
                      ! the first field read in, which is usually reflectivity.  If you are not sure
                      ! what to use, turn these off.
pre_oban_filter_flag  ! list of PRE-oban filter flags for each field  [default:  0]
                      ! ON=1 (lower threshold) or -1 (upper threshold), OFF=0
post_oban_filter_flag ! list of POST-oban filter flags for each field  [default:  0]
                      ! ON=1 (lower threshold) or -1 (upper threshold), OFF=0
```

**optional real parameters:**
```
fill_value          ! list of replacement values (e.g., 5.0, 0.0) for filling data voids (required if fill_flag is 1 for any field)  [default:  none]

pre_oban_filter_value    ! list of threshold values for setting observations to missing before the objective analysis  [default:  none]
                         ! Example:
                         !         If pre_oban_filter_flag is 1 and pre_oban_filter_value is 20.0 for the current field,
                         !         then observed values of the current field are set to missing where field #1 (usually reflectivity) is < 20.0.

post_oban_filter_value   ! list of threshold values for setting analysis values to missing after the objective analysis  [default:  none]
                         ! Example:
                         !         If post_oban_filter_flag is -1 and post_oban_filter_value is 70.0 for the current field,
                         !         then analysis values of the current field are set to missing where analysis field #1 (usually reflectivity) is > 70.0.
```


# Sweep File List #

The end of the _oban.nml_ file is the list of radar-data sweep files to be analyzed.  A line with 6 "=" signs must precede the list of sweep files (see examples below).  Sweep file names can be listed explicitly (example 1 below), and the number of file names must correspond to _nswp_ in the parameters section.  Alternatively (example 2), if _nswp_=-1, then a single path name is specified, and all files in the directory that match the specified name are read as input.

# Example 1 #

This example _oban.nml_ file does a Cressman 3D analysis of dorade sweep files.  Sweep-file names are specified explicitly.

```
&parameters
   nx = 101,                          ! no. of grid points in x direction
   ny = 101,                          ! no. of grid points in y direction
   nz = 21,                           ! no. of grid points in z direction
                                      !
   xmin = -50.0,                      ! coordinates of lower southwest corner (in km)
   ymin = -30.0,                      !     of analysis grid relative to the 
   zmin = 0.0,                        !     origin given by glat, glon, galt
                                      !
   dx = 1.0,                          ! grid spacing in x direction (km)
   dy = 1.0,                          ! grid spacing in y direction (km)
   dz = 0.5,                          ! grid spacing in z direction (km)
                                      !
   glon = -97.4619,                   ! longitude of grid origin (deg)
   glat = 35.2360,                    ! latitude  of grid origin (deg)
   galt = 0.380,                      ! altitude of grid origin (km MSL)
                                      !
   rlon = -97.4619,                   ! radar longitude (deg)
   rlat = 35.2360,                    ! radar latitude (deg)
   ralt = 0.380,                      ! radar altitude (km MSL)
                                      !
   cyr = 2003,                        ! reference date used for (optional) time-to-space conversion: year
   cmo = 05,                          ! "                                                         ": month
   cda = 08,                          ! "                                                         ": day
   chr = 22,                          ! reference time used for (optional) time-to-space conversion: hour
   cmn = 05,                          ! "                                                         ": minute
   cse = 00,                          ! "                                                         ": second
                                      ! 
   ut = 14.0,                         ! x-translation velocity (m/s) used for time-to-space conversion
   vt = 8.0,                          ! y-translation velocity (m/s) used for time-to-space conversion
                                      ! 
   sec_cor = -150,                    ! offset in seconds to correct each ray
                                      !
   output_prefix = 'KOUN2205',        ! prefix name for output files
                                      !
   analysis_type   = 1,               ! Type of analysis
                                      !   1 == 3D Cartesian
                                      !   2 == 2D sweep-by-sweep
   method = 1,                        ! Interpolation method:
                                      !   1 == Cressman
                                      !   2 == Barnes
   hsp0 = 1.00,                       ! either Cressman horiz. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)
   vsp0 = 1.00,                       ! either Cressman vert. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)
                                      !
   minsum   = 0.1,                    ! threshold for minimum sum of weights required to produce
                                      !   an objectively-analyzed observation
                                      !
   nfld = 2,                          ! number of data fields to be gridded
                                      !
   ncgen_command = 'ncgen',           ! path/executable for local "ncgen" command
                                      !
   radar_data_format = 1,             ! radar data format: 1=dorade sweep files, 2=netcdf (FORAY)
                                      !
   nswp = 14                          ! controls file I/O:  nswp = -1 ==> read all files in directory at bottom of input file
                                      !                     nswp >  0 ==> read nswp # of filenames at bottom of input file
/
&fields
   fieldnames  = 'DZ', 'VU',          ! Names of the fields to be read from the dorade/netcdf radar-data files
   fill_flag   =    0,    0,          ! fill missing field values with a specified value? (0=no, 1=yes)
   fill_value  =  0.0,  0.0,          ! replacement value, if fill_flag==1
   error       =  5.0,  2.0,          ! Observational error standard deviation for the DART observations
/
======= DO NOT EDIT THIS LINE -> MUST START WITH 6 "=" for the oban formatted I/O to find the start of input files
./KOUN/swp.1030508220458.OSF_RVP7.0.0.5_PPI_v1
./KOUN/swp.1030508220515.OSF_RVP7.0.1.5_PPI_v1
./KOUN/swp.1030508220534.OSF_RVP7.0.2.5_PPI_v1
./KOUN/swp.1030508220551.OSF_RVP7.0.3.5_PPI_v1
./KOUN/swp.1030508220609.OSF_RVP7.0.4.5_PPI_v1
./KOUN/swp.1030508220645.OSF_RVP7.0.5.5_PPI_v1
./KOUN/swp.1030508220703.OSF_RVP7.0.6.5_PPI_v1
./KOUN/swp.1030508220721.OSF_RVP7.0.7.5_PPI_v1
./KOUN/swp.1030508220740.OSF_RVP7.0.8.7_PPI_v1
./KOUN/swp.1030508220757.OSF_RVP7.0.10.0_PPI_v1
./KOUN/swp.1030508220833.OSF_RVP7.0.12.0_PPI_v1
./KOUN/swp.1030508220851.OSF_RVP7.0.14.0_PPI_v1
./KOUN/swp.1030508220909.OSF_RVP7.0.16.7_PPI_v1
./KOUN/swp.1030508220927.OSF_RVP7.0.19.5_PPI_v1
```

# Example 2 #

This example _oban.nml_ file does a Barnes 2-pass, 2-dimensional analysis of Foray-NetCDF radar data.  Sweep-file names are specified by path name.

```
&parameters
   nx = 70,                           ! no. of grid points in x direction
   ny = 70,                           ! no. of grid points in y direction
                                      !
   xmin = -80.0,                      ! coordinates of lower southwest corner (in km)
   ymin = -80.0,                      !     of analysis grid relative to the 
                                      !     origin given by glat, glon, galt
                                      !
   dx = 2.0,                          ! grid spacing in x direction (km)
   dy = 2.0,                          ! grid spacing in y direction (km)
                                      !
   glon = -97.461945,                 ! longitude of grid origin (deg)
   glat = 35.235832,                  ! latitude  of grid origin (deg)
   galt = 0.373,                      ! altitude of grid origin (km MSL)
                                      !
   rlon = -97.461945,                 ! radar longitude (deg)
   rlat = 35.235832,                  ! radar latitude (deg)
   ralt = 0.373,                      ! radar altitude (km MSL)
                                      ! 
   day_cor = -1,                      ! offset in days to correct each ray
   sec_cor = -150,                    ! offset in seconds to correct each ray
                                      !
   output_prefix = 'KOUN_barnes',     ! prefix name for output files
                                      !
   analysis_type   = 2,               ! Type of analysis
                                      !   1 == 3D Cartesian
                                      !   2 == 2D sweep-by-sweep
   method = 2,                        ! Interpolation method:
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
   ncgen_command = 'ncgen',           ! path/executable for local "ncgen" command
                                      !
   radar_data_format = 2,             ! radar data format: 1=dorade sweep files, 2=netcdf (FORAY)
                                      !
   nswp = -1                          ! controls file I/O:  nswp = -1 ==> read all files in directory at bottom of input file
                                      !                     nswp >  0 ==> read nswp # of filenames at bottom of input file
/
&fields
   fieldnames  = 'DZ', 'VU',          ! Names of the fields to be read from the dorade/netcdf radar-data files
   fill_flag   =    1,    0,          ! fill missing field values with a specified value? (0=no, 1=yes)
   fill_value  =  0.0,  0.0,          ! replacement value, if fill_flag==1
   error       =  5.0,  2.0,          ! Observational error standard deviation for the DART observations
/
======= DO NOT EDIT THIS LINE -> MUST START WITH 6 "=" for the oban formatted I/O to find the start of input files
'./koun_foray/ncswp_*'
```