!############################# LICENSE ######################################
!
!   Copyright (C) <2010>  <David C. Dowell and Louis J. Wicker, NOAA>
!
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 2.1 of the License, or (at your option) any later version.
!
!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Lesser General Public License for more details.
!
!############################# LICENSE ######################################

!===========================================================================
!
!
!
!
!   /////////////////////           BEGIN            \\\\\\\\\\\\\\\\\\\\
!   \\\\\\\\\\\\\\\\\\\\\        STRING_MODULE       ////////////////////
!
!
!
!
!===========================================================================
MODULE STRING_MODULE

 CONTAINS 
!-------------------------------------------------------------------------------
! 
!  FUNCTION UCASE
!    
! 
!  Purpose: 
!    To shift a character string to upper case on any processor, 
!    regardless of collating sequence. 
! 
!  Record of revisions: 
!      Date       Programmer          Description of change 
!      ====       ==========          ===================== 
!    01/09/96    S. J. Chapman        Original code 
!    12/22/05    L. J. Wicker         Adapted for NCOMMAS work 
!
!-------------------------------------------------------------------------------
 FUNCTION UCASE ( string ) 
  IMPLICIT NONE 

! Declare calling parameters: 

  CHARACTER(len=*), INTENT(IN) :: string      ! Input string 
  CHARACTER(len=LEN(string))   :: ucase       ! Function 

! Declare local variables: 
  INTEGER :: n                 ! Loop index 
  INTEGER :: length            ! Length of input string 

! Get length of string 

  length = LEN ( string ) 

! Now shift lower case letters to upper case. 

  DO n = 1, length 

   IF ( LGE(string(n:n),'a') .AND. LLE(string(n:n),'z') ) THEN 
    ucase(n:n) = ACHAR ( IACHAR ( string(n:n) ) - 32 ) 
   ELSE 
    ucase(n:n) = string(n:n) 
   END IF 

  END DO 

 END FUNCTION UCASE 

 SUBROUTINE STRING_LIMITS( string, ibeg, iend )

  IMPLICIT NONE

! List of dummy arguments

  CHARACTER(len=*),INTENT(IN) :: string ! Input string
  INTEGER,INTENT(OUT)         :: ibeg   ! First non-blank character
  INTEGER,INTENT(OUT)         :: iend   ! Last non-blank character

! List of local variables:

  INTEGER :: length            ! Length of input string

! Get the length of the input string.

  length = LEN ( string )

! Look for first character used in string.  Use a WHILE loop to find the first non-blank character.

  ibeg = 0

  DO
   ibeg = ibeg + 1
   IF ( ibeg > length ) EXIT
   IF ( string(ibeg:ibeg) /= ' ' ) EXIT
  END DO

! If ibeg > length, the whole string was blank.  Set
! ibeg = iend = 1.  Otherwise, find the last non-blank
! character.

  IF ( ibeg > length ) THEN
   ibeg = 1
   iend = 1
  ELSE
! Find last nonblank character.
   iend = length + 1
   DO
      iend = iend - 1
      IF ( string(iend:iend) /= ' ' .and. ICHAR(string(iend:iend)) /= 0 ) EXIT
   END DO
  END IF

 END SUBROUTINE STRING_LIMITS

END MODULE STRING_MODULE


!===========================================================================
!
!
!
!
!   /////////////////////             BEGIN             \\\\\\\\\\\\\\\\\\\\
!   \\\\\\\\\\\\\\\\\\\\\    OBAN_PARAMETERS MODULE     ////////////////////
!
!
!
!
!===========================================================================
MODULE OBAN_PARAMETERS

  USE DICTIONARY

  implicit none

! DCD 12/9/10  moved declarations of r_missing and i_missing to DICTIONARY module

  integer :: nx = i_missing                     ! no. of grid points in x direction
  integer :: ny = i_missing                     ! no. of grid points in y direction
  integer :: nz = i_missing                     ! no. of grid points in z direction

  real    :: xmin = r_missing                   ! coordinates (km) of lower southwest corner
  real    :: ymin = r_missing                   !     of analysis grid relative to the 
  real    :: zmin = 0.0                         !     origin given by glat, glon, galt
                               
  real    :: dx = r_missing                     ! grid spacing in x direction (km)
  real    :: dy = r_missing                     ! grid spacing in y direction (km)
  real    :: dz = 1.0                           ! grid spacing in z direction (km)

  real    :: glon = r_missing                   ! longitude of grid origin (deg)
  real    :: glat = r_missing                   ! latitude  of grid origin (deg)
  real    :: galt = r_missing                   ! altitude of grid origin (km MSL)

! Note:  If the following parameters (radar location) are specified in the namelist input, then the input values are used.
!        Otherwise, these parameters are read from the radar data themselves.
  real    :: rlon = r_missing                   ! radar longitude (deg)
  real    :: rlat = r_missing                   ! radar latitude (deg)
  real    :: ralt = r_missing                   ! radar altitude (km MSL)

  integer :: map_proj = 0                       ! map projection (for relating lat, lon to x, y):
                                                !   0 = flat earth
                                                !   1 = oblique azimuthal (not implemented...)
                                                !   2 = Lambert conformal (not implemented...)

                                                ! reference date and time used for (optional) time-to-space conversion:
  integer(kind=2) :: cyr = i_missing            !    year
  integer(kind=2) :: cmo = i_missing            !    month
  integer(kind=2) :: cda = i_missing            !    day
  integer(kind=2) :: chr = i_missing            !    hour
  integer(kind=2) :: cmn = i_missing            !    minute
  integer(kind=2) :: cse = i_missing            !    second

  real    :: ut = 0.0                           ! x-translation velocity (m/s) used for time-to-space conversion
  real    :: vt = 0.0                           ! y-translation velocity (m/s) used for time-to-space conversion

  integer :: year_cor = 0                       ! offset in years to correct each ray
  integer :: day_cor = 0                        ! offset in days to correct each ray
  integer :: sec_cor = 0                        ! offset in seconds to correct each ray

  logical :: umass_data = .false.               ! .true. (.false.) if UMass radar-data corrections are (not) needed

  integer :: az_corr_flag = 0                   ! method of additional azimuth-angle correction
                                                !   0 = none
                                                !   1 = average current and previous angle

  real    :: elcor = 0.0                        ! elevation-angle correction (deg)
  real    :: azcor = 0.0                        ! azimuth-angle offset (deg)

  character(LEN=80) :: output_prefix = ""       ! string which is prefix for output files
  logical :: output_beam_info = .true.          ! .true. if beam information should be output
  logical :: output_dart = .true.               ! .true. if DART obs_seq.out output should be created
  logical :: output_netcdf = .true.             ! .true. if NetCDF analysis output should be created
  logical :: output_vis5d = .true.              ! .true. if Vis5D output should be created
  integer :: analysis_type = i_missing          ! Type of analysis
                                                !   1 == 3D Cartesian
                                                !   2 == 2D sweep by sweep
  integer :: method = i_missing                 ! interpolation method:
                                                !   1 == Cressman
                                                !   2 == Barnes
  real    :: hsp0 = r_missing                   ! either Cressman horiz. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)
  real    :: vsp0 = r_missing                   ! either Cressman vert. radius of influence (km) or Barnes smoothing parameter (kappa, in km**2)
  real    :: gamma = r_missing                  ! gamma parameter for multi-pass Barnes anaylsis
  integer :: npass = 1                          ! number of passes

                                                ! The following 7 parameters affect the implementation of the Steiner and Smith (2002,
                                                !   J. Atmos. Oceanic Technol.) algorithm for removing echoes not associated with precipitation.
  logical :: remove_gc_refl_thresh = .false.    ! true. if ground clutter should be removed based on reflectivity threshold
  logical :: remove_gc_echo_top = .false.       ! true. if ground clutter should be removed based on echo top
  real    :: refl_thresh = 5.0                  ! reflectivity threshold (dBZ) for first two steps in Steiner and Smith (2002) algorithm
  logical :: remove_gc_spin_change = .false.    ! true. if ground clutter should be removed based on spin change
  real    :: refl_fluc = 2.0                    ! reflectivity fluctuation (dBZ) for spin-change portion of algorithm
  logical :: remove_gc_vert_grad = .false.      ! true. if ground clutter should be removed based on vertical gradient
  real    :: grad_thresh = 10.0                 ! reflectivity gradient (dBZ/degree) for vertical-gradient portion of algorithm

  logical :: use_clutter_mask = .false.         ! .true. if clutter mask based on ppi stats should be implemented
  real    :: cm_min_refl_avail = 75.0           ! minimum reflectivity availability (%) for clutter mask
  real    :: cm_min_refl = -5.0                 ! minimum reflectivity (dBZ) for clutter mask
  real    :: cm_max_refl_sd = 10.0              ! maximum reflectivity standard deviation (dBZ) for clutter mask
  real    :: cm_refl_exceedance = 20.0          ! reflectivity exceedance (dBZ) for which standard clutter mask is ignored
  real    :: cm_min_vel_sd = 7.5                ! minimum velocity standard deviation (m/s) for highway detection in clutter mask
  real    :: cm_max_vel = 2.0                   ! maximum velocity absolute magnitude (m/s) for standard clutter detection in clutter mask
  real    :: cm_max_vel_sd = 2.0                ! maximum velocity standard deviation (m/s) for standard clutter detection in clutter mask
  character(LEN=100) :: cm_ncfile = ""          ! netcdf "ppi stats" file name for clutter masking
  character(LEN=8) :: cm_refl_fname = "REF"     ! name of reflectivity field in current analysis
  character(LEN=8) :: cm_refl_fname_ppi = "REF" ! name of reflectivity field in "ppi stats" file
  character(LEN=8) :: cm_vel_fname_ppi = "VEL"  ! name of velocity field in "ppi stats" file

  logical :: use_clear_air_type = .false.       ! .true. if clear-air reflectivity ob. type should be used for DART output
  integer :: clear_air_skip = 0                 ! thinning factor for clear-air reflectivity data

  real    :: height_max = r_missing             ! height (km above grid origin) above which observations are discarded

  real    :: minrange = 0.0                     ! minimum-range threshold (in km, data closer to radar are discarded)
  integer :: mincount = 1                       ! threshold for minimum number of data gates required
                                                !   to produce an objectively-analyzed observation
  real    :: minsum = 0.0                       ! threshold for minimum sum of weights required to produce
                                                !   an objectively-analyzed observation
  logical :: allow_extrapolation = .true.       ! should extrapolation be allowed?
                                                !   .true. for standard objective analysis, .false. for interpolation only

  integer :: radar_data_format = i_missing      ! format for input radar data:
                                                !   1 == dorade sweep files
                                                !   2 == netcdf (FORAY)

  integer :: nfld = 0                           ! number of data fields to be gridded
  integer :: nswp = 0                           ! controls file I/O:  nswp = -1 ==> use all files in directory specified 
                                                !                                        at bottom of input file
                                                !                     nswp >  0 ==> read nswp # of filenames at bottom of 
                                                !                                        input file

  NAMELIST /parameters/                          &
                          nx,                    &
                          ny,                    &
                          nz,                    &
                          xmin,                  &
                          ymin,                  &
                          zmin,                  &
                          dx,                    &
                          dy,                    &
                          dz,                    &
                          glon,                  &
                          glat,                  &
                          galt,                  &
                          rlon,                  &
                          rlat,                  &
                          ralt,                  &
                          map_proj,              &
                          cyr,                   &
                          cmo,                   &
                          cda,                   &
                          chr,                   &
                          cmn,                   &
                          cse,                   &
                          ut,                    &
                          vt,                    &
                          year_cor,              &
                          day_cor,               &
                          sec_cor,               &
                          umass_data,            &
                          az_corr_flag,          &
                          elcor,                 &
                          azcor,                 &
                          output_prefix,         &
                          output_beam_info,      &
                          output_netcdf,         &
                          output_dart,           &
                          output_vis5d,          &
                          analysis_type,         &
                          method,                &
                          remove_gc_refl_thresh, &
                          remove_gc_echo_top,    &
                          refl_thresh,           &
                          remove_gc_spin_change, &
                          refl_fluc,             &
                          remove_gc_vert_grad,   &
                          grad_thresh,           &
                          use_clutter_mask,      &
                          cm_min_refl_avail,     &
                          cm_min_refl,           &
                          cm_max_refl_sd,        &
                          cm_refl_exceedance,    &
                          cm_min_vel_sd,         &
                          cm_max_vel,            &
                          cm_max_vel_sd,         &
                          cm_ncfile,             &
                          cm_refl_fname,         &
                          cm_refl_fname_ppi,     &
                          cm_vel_fname_ppi,      &
                          use_clear_air_type,    &
                          clear_air_skip,        &
                          hsp0,                  &
                          vsp0,                  &
                          gamma,                 &
                          npass,                 &
                          height_max,            &
                          minrange,              &
                          mincount,              &
                          minsum,                &
                          allow_extrapolation,   &
                          radar_data_format,     &
                          nfld,                  &
                          nswp

 CONTAINS

   SUBROUTINE OBAN_PARAMETERS_INIT()

     CALL DICT_CREATE( "nx",       int=nx )
     CALL DICT_CREATE( "ny",       int=ny )
     CALL DICT_CREATE( "nz",       int=nz )
     CALL DICT_CREATE( "xmin",     flt=xmin )
     CALL DICT_CREATE( "ymin",     flt=ymin )
     CALL DICT_CREATE( "zmin",     flt=zmin )
     CALL DICT_CREATE( "dx",       flt=dx )
     CALL DICT_CREATE( "dy",       flt=dy )
     CALL DICT_CREATE( "dz",       flt=dz )
     CALL DICT_CREATE( "glon",     flt=glon )
     CALL DICT_CREATE( "glat",     flt=glat )
     CALL DICT_CREATE( "galt",     flt=galt )
     CALL DICT_CREATE( "rlon",     flt=rlon )
     CALL DICT_CREATE( "rlat",     flt=rlat )
     CALL DICT_CREATE( "ralt",     flt=ralt )
     CALL DICT_CREATE( "map_proj", int=map_proj )
     CALL DICT_CREATE( "central year",   int=int(cyr) )
     CALL DICT_CREATE( "central month",  int=int(cmo) )
     CALL DICT_CREATE( "central day",    int=int(cda) )
     CALL DICT_CREATE( "central hour",   int=int(chr) )
     CALL DICT_CREATE( "central minute", int=int(cmn) )
     CALL DICT_CREATE( "central second", int=int(cse) )
     CALL DICT_CREATE( "U-Trans",        flt=ut )
     CALL DICT_CREATE( "V-Trans",        flt=vt )
     CALL DICT_CREATE( "year offset",    int=year_cor )
     CALL DICT_CREATE( "day offset",     int=day_cor )
     CALL DICT_CREATE( "seconds offset", int=sec_cor )
     CALL DICT_CREATE( "UMass data",      log=umass_data )
     CALL DICT_CREATE( "AZ correction flag", int=az_corr_flag )
     CALL DICT_CREATE( "EL correction flag", flt=elcor )
     CALL DICT_CREATE( "AZ offset flag",     flt=azcor )
     CALL DICT_CREATE( "Output prefix",      str=output_prefix )
     CALL DICT_CREATE( "Output beam info",   log=output_beam_info )
     CALL DICT_CREATE( "Output NetCDF",      log=output_netcdf )
     CALL DICT_CREATE( "Output DART",        log=output_dart )
     CALL DICT_CREATE( "Output Vis5D",       log=output_vis5d )
     CALL DICT_CREATE( "Analysis type",      int=analysis_type )
     CALL DICT_CREATE( "Analysis method",    int=method )
     CALL DICT_CREATE( "Remove gc refl thresh",    log=remove_gc_refl_thresh )
     CALL DICT_CREATE( "Remove gc echo top",       log=remove_gc_echo_top )
     CALL DICT_CREATE( "Reflectivity threshold",   flt=refl_thresh )
     CALL DICT_CREATE( "Remove gc spin change",    log=remove_gc_spin_change )
     CALL DICT_CREATE( "Reflectivity fluctuation", flt=refl_fluc )
     CALL DICT_CREATE( "Remove gc vert grad",      log=remove_gc_vert_grad )
     CALL DICT_CREATE( "Refl vert grad",           flt=grad_thresh )
     CALL DICT_CREATE( "Use clutter mask",        log=use_clutter_mask )
     CALL DICT_CREATE( "Min refl availability",   flt=cm_min_refl_avail )
     CALL DICT_CREATE( "Min reflectivity",        flt=cm_min_refl )
     CALL DICT_CREATE( "Max refl std dev",        flt=cm_max_refl_sd )
     CALL DICT_CREATE( "Reflectivity exceedance", flt=cm_refl_exceedance )
     CALL DICT_CREATE( "Min vel std dev",         flt=cm_min_vel_sd )
     CALL DICT_CREATE( "Max velocity",            flt=cm_max_vel )
     CALL DICT_CREATE( "Max vel std dev",         flt=cm_max_vel_sd )
     CALL DICT_CREATE( "PPI stats file name",     str=cm_ncfile )
     CALL DICT_CREATE( "Refl field name analysis",   str=cm_refl_fname )
     CALL DICT_CREATE( "Refl field name ppi stats",  str=cm_refl_fname_ppi )
     CALL DICT_CREATE( "Vel field name ppi stats",   str=cm_vel_fname_ppi )
     CALL DICT_CREATE( "Use clear air type", log=use_clear_air_type )
     CALL DICT_CREATE( "Clear air skip",     int=clear_air_skip )
     CALL DICT_CREATE( "Horiz. r.o.i. / smoothing param.", flt=hsp0 )
     CALL DICT_CREATE( "Vert. r.o.i. / smoothing param.",  flt=vsp0 )
     CALL DICT_CREATE( "Gamma for Barnes",    flt=gamma )
     CALL DICT_CREATE( "Number of Passes",    int=npass )
     CALL DICT_CREATE( "Max Ob Height",       flt=height_max )
     CALL DICT_CREATE( "Min Range",           flt=minrange )
     CALL DICT_CREATE( "Min Bin Count",       int=mincount )
     CALL DICT_CREATE( "Min sum ",            flt=minsum )
     CALL DICT_CREATE( "Extrapolate flag ",   log=allow_extrapolation )
     CALL DICT_CREATE( "Radar data format",   int=radar_data_format )
     CALL DICT_CREATE( "No. of radar fields", int=nfld )
     CALL DICT_CREATE( "No. of files",        int=nswp )

   END SUBROUTINE OBAN_PARAMETERS_INIT

END MODULE OBAN_PARAMETERS
!===========================================================================
!
!
!
!
!   /////////////////////          BEGIN            \\\\\\\\\\\\\\\\\\\\
!   \\\\\\\\\\\\\\\\\\\\\    OBAN_FIELDS MODULE     ////////////////////
!
!
!
!
!===========================================================================
MODULE OBAN_FIELDS

  USE DICTIONARY

  implicit none

  include 'opaws.inc'

  character(LEN=8) :: fieldnames(maxflds)        ! Names of the variables to read in the sweep/foray files (e.g., VR, DZ)
  data fieldnames /maxflds*""/

  integer :: fill_flag(maxflds)                  ! fill missing field values with a specified value? (0=no, 1=yes)
  data fill_flag /maxflds*0/
  real    :: fill_value(maxflds)                 ! replacement value, if fill_flag==1
  data fill_value/maxflds*r_missing/

  integer :: unfold_flag(maxflds)                ! locally unfold field? (0=no, 1=yes)
  data unfold_flag /maxflds*0/

  real    :: error(maxflds)                      ! standard deviation of ob errors the DART output
  data error /maxflds*r_missing/
                                                 ! These are extra filters for the data.  The thresholding is done on
                                                 ! the first field read in, which is usually DBZ.  If you are not sure
                                                 ! what to use, turn these off.
  integer :: pre_oban_filter_flag(maxflds)       ! PRE-oban filter ON=1 (lower threshold) or -1 (upper threshold), OFF=0
  data pre_oban_filter_flag/maxflds*0/
  real    :: pre_oban_filter_value(maxflds)      ! All fields are set to missing below/above this value 
  data pre_oban_filter_value/maxflds*r_missing/  ! Example:
                                                 !         Threshold Vr on dBZ:  pre_oban_filter_flag = 1, Vr=missing for dBZ < 20.

  integer :: post_oban_filter_flag(maxflds)      ! POST-oban filter ON=1 (lower threshold) or -1 (upper threshold), OFF=0
  data post_oban_filter_flag/maxflds*0/
  real    :: post_oban_filter_value(maxflds)     ! All fields are set to missing below/above this value 
  data post_oban_filter_value/maxflds*r_missing/ ! Example:
                                                 !   Threshold Vr on dBZ:  post_oban_filter_flag = -1, Vr=missing for dBZ > 70.
                                                 !

  NAMELIST /fields/                              &
                          error,                 &
                          fieldnames,            &
                          fill_flag,             &
                          fill_value,            &
                          unfold_flag,           &
                          pre_oban_filter_flag,  &
                          pre_oban_filter_value, &
                          post_oban_filter_flag, &
                          post_oban_filter_value
          

 CONTAINS

   SUBROUTINE OBAN_FIELDS_INIT()

     integer           :: int
     real              :: flt
     character(len=80) :: str
     logical           :: log

     integer n, nfld
     character(LEN=80) :: string = ""

     nfld = DICT_GET( "No. of radar fields", int )

     DO n = 1,nfld

      write(string,'("field-",i1)') n 
      CALL DICT_CREATE( trim(string),   str=fieldnames(n) )

      write(string,'("fill_flag-",i1)') n 
      CALL DICT_CREATE( trim(string),   int=fill_flag(n) )

      write(string,'("fill_value-",i1)') n 
      CALL DICT_CREATE( trim(string),   flt=fill_value(n) )

      write(string,'("error_std_dev-",i1)') n 
      CALL DICT_CREATE( trim(string),   flt=error(n) )

      write(string,'("pre_oban_filter_flag-",i1)') n 
      CALL DICT_CREATE( trim(string),   int=pre_oban_filter_flag(n) )

      write(string,'("pre_oban_filter_value-",i1)') n 
      CALL DICT_CREATE( trim(string),   flt=pre_oban_filter_value(n) )

      write(string,'("post_oban_filter_flag-",i1)') n 
      CALL DICT_CREATE( trim(string),   int=post_oban_filter_flag(n) )

      write(string,'("post_oban_filter_value-",i1)') n 
      CALL DICT_CREATE( trim(string),   flt=post_oban_filter_value(n) )

     ENDDO

   END SUBROUTINE OBAN_FIELDS_INIT

END MODULE OBAN_FIELDS
!===========================================================================
!
!
!
!
!   /////////////////////          BEGIN            \\\\\\\\\\\\\\\\\\\\
!   \\\\\\\\\\\\\\\\\\\\\     NAMELIST_MODULE       ////////////////////
!
!
!
!
!===========================================================================
MODULE NAMELIST_MODULE

 USE OBAN_PARAMETERS
 USE OBAN_FIELDS
 USE STRING_MODULE

 implicit none

 CONTAINS

  LOGICAL FUNCTION READ_NAMELIST(filename,namelist0)

    character(LEN=*) filename
    character(LEN=*) namelist0
    character(LEN=50) namelist

    integer ibeg, iend
    logical if_exist
    integer istat

    CALL STRING_LIMITS(filename, ibeg, iend)

    INQUIRE(file=filename(ibeg:iend), exist=if_exist)

    IF( .NOT. if_exist ) THEN
      write(0,*) 'READ_NAMELIST:  ERROR - INPUT FILE:  ', filename(ibeg:iend), ' DOES NOT EXIST!!!'
      write(0,*) 'READ_NAMELIST:  ERROR - DOES NOT HAVE A FILE TO READ, EXITING'
      STOP
    ENDIF

    open(15,file=filename(ibeg:iend),status='old',form='formatted')
    rewind(15)

    namelist = UCASE(namelist0)
    CALL STRING_LIMITS(namelist, ibeg, iend)

    SELECT CASE( namelist(ibeg:iend) )

      CASE( 'FIELDS')
        read(15,NML=fields,iostat=istat)
        READ_NAMELIST = .true.
        IF ( istat .ne. 0 ) THEN
          write(0,'("Problem reading namelist: ",a, "Error:  ", i10)'), namelist, istat
          READ_NAMELIST = .false.
        ENDIF
        CALL OBAN_FIELDS_INIT()

      CASE( 'PARAMETERS')
        read(15,NML=parameters,iostat=istat)
        READ_NAMELIST = .true.
        IF ( istat .ne. 0 ) THEN
          write(0,'("Problem reading namelist: ",a, "Error:  ", i10)'), namelist, istat
          READ_NAMELIST = .false.
        ENDIF
        CALL OBAN_PARAMETERS_INIT()

      CASE DEFAULT
        write(0,*) 'READ_NAMELIST:  ERROR - UNKNOWN NAMELIST REQUESTED:  ', namelist(ibeg:iend)
        write(0,*) 'READ_NAMELIST:  ERROR - NAMELIST WAS NOT READ!!'
        READ_NAMELIST = .false.

      END SELECT

    close(15)
    READ_NAMELIST = .true.

  END FUNCTION READ_NAMELIST

!-------------------------------------------------------------------------------

  SUBROUTINE WRITE_NAMELISTS(iunit)

    USE DICTIONARY

    implicit none

    integer iunit

    write(iunit,'(a)') '! INPUT NAMELIST values for the OBAN'

    write(iunit,'(a)')

    CALL DICT_PRINT()

    write(iunit,'(a)')

  END SUBROUTINE WRITE_NAMELISTS

!-------------------------------------------------------------------------------

  SUBROUTINE VERIFY_NAMELISTS(iunit)

    implicit none

    integer iunit, f

    write(iunit,'(a)')

    write(iunit,'(a)') 'Checking namelists for required information...'

    write(iunit,'(a)')

    IF ( nx .eq. i_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'nx')
    IF ( ny .eq. i_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'ny')

    IF ( xmin .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'xmin')
    IF ( ymin .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'ymin')

    IF ( dx .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'dx')
    IF ( dy .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'dy')

    IF ( glon .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'glon')
    IF ( glat .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'glat')
    IF ( galt .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'galt')

    IF ( map_proj .ne. 0) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'map_proj')

    IF ( (az_corr_flag .ne. 0) .and. (az_corr_flag .ne. 1) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'az_corr_flag')

    IF ( (analysis_type .ne. 1) .and. (analysis_type .ne. 2) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'analysis_type')

    IF ( output_prefix .eq. "") CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'output_prefix')

    IF ( (method .ne. 1) .and. (method .ne. 2) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'method')

    IF ( use_clutter_mask .and. ( cm_ncfile .eq. "") ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'cm_ncfile')

    IF ( use_clutter_mask .and. (analysis_type .ne. 2) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'use_clutter_mask')

    IF ( hsp0 .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'hsp0')

    IF ( ( vsp0 .eq. r_missing) .and. (analysis_type .eq. 1) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'vsp0')

    IF ( (npass.gt.1) .and. (gamma .eq. r_missing) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'gamma')

    IF ( (radar_data_format .ne. 1) .and. (radar_data_format .ne. 2) )   &
       CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'radar_data_format')

    IF ( nfld .le. 0) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'nfld')

    IF ( (nswp .eq. 0) .or. (nswp .lt. -1) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'nswp')

    DO f = 1, nfld
      IF ( (fill_flag(f) .ne. 0) .and. (fill_flag(f) .ne. 1) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'fill_flag')
      IF ( (fill_flag(f) .eq. 1) .and. (fill_value(f) .eq. r_missing) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'fill_value')
      IF ( (unfold_flag(f) .lt. -1) .or. (unfold_flag(f) .gt. 1) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'unfold_flag')
      IF ( (unfold_flag(f) .eq. 1) .and. (analysis_type .ne. 2) ) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'unfold_flag')
            print *, error(f)
      IF (error(f) .eq. r_missing) CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'error')
      IF ( (pre_oban_filter_flag(f) .lt. -1) .or. (pre_oban_filter_flag(f) .gt. 1) )  &
         CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'pre_oban_filter_flag')
      IF ( (pre_oban_filter_flag(f) .ne. 0) .and. (pre_oban_filter_value(f) .eq. r_missing) )   &
         CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'pre_oban_filter_value')
      IF ( (post_oban_filter_flag(f) .lt. -1) .or. (post_oban_filter_flag(f) .gt. 1) )  &
         CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'post_oban_filter_flag')
      IF ( (post_oban_filter_flag(f) .ne. 0) .and. (post_oban_filter_value(f) .eq. r_missing) )   &
         CALL REPORT_NAMELIST_ERROR_AND_ABORT(iunit, 'post_oban_filter_value')
    ENDDO

  END SUBROUTINE VERIFY_NAMELISTS

!-------------------------------------------------------------------------------

  SUBROUTINE REPORT_NAMELIST_ERROR_AND_ABORT(iunit, parameter_name)

    implicit none

    integer iunit
    character(LEN=*) parameter_name

    write(iunit,*) 'ERROR -- invalid value of namelist parameter:  ', parameter_name
    stop

  END SUBROUTINE REPORT_NAMELIST_ERROR_AND_ABORT

END MODULE NAMELIST_MODULE


