!############################# LICENSE ######################################
!
!   Copyright (C) <2011>  <David Dowell, Curtis Alexander, and Lou Wicker, NOAA>
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
!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######                 PROGRAM CLUTTER_STATS                ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This program computes reflectivity and Doppler velocity statistics as a
!     function of range, azimuth angle, and elevation angle for a set of dorade
!     sweep or Foray netcdf radar-data files.  These statistics are used to
!     develop a clutter map, roughly as described in the Lakshmanan et al.
!     reference below.  Statistics are output in netcdf and can be used by OPAWS
!     oban for ground-clutter removal during objective analysis.
!
!     Those who wish to modify dorade sweep files based on the derived
!     clutter map (i.e., actually remove the ground clutter from the raw data)
!     can use the DReader editor (reference below), which contains an implementation
!     of the same algorithm as in this program.
!
!     inputs:
!     (1) parameter (namelist) file (e.g., clutter_stats.nml)
!     (2) radar data (dorade sweep or Foray netcdf files)
!
!     outputs:
!     (1) text output
!     (2) gridded statistics in netcdf file
!
!     references:
!     - DReader software by Curtis Alexander
!       http://sourceforge.net/projects/dreader/
!     - "A Statistical Approach to Mitigating Persistent Clutter in Radar
!       Reflectivity Data" by V. Lakshmanan, J. Zhang, K. Hondl, and C. Langston
!       http://www.cimms.ou.edu/~lakshman/Papers/cluttermap.pdf
!
!############################################################################
!
!     Authors:  David Dowell and Curtis Alexander
!
!     Creation date:  March 2010
!
!     Last modifications:
!
!     (David Dowell, July 2011)
!     Renamed from ppi_stats to clutter_stats during transition from OPAWS1
!        to OPAWS2.
!     Coordinate system changed from (x, y, elevation angle)
!        to (range, azimuth angle, elevation angle).
!
!############################################################################

!
!  Run this job in background with the following command line syntax:
!
!   x.clutter_stats clutter_stats.nml >&! clutter_stats.output &
!
!   where clutter_stats.nml is the input namelist and clutter_stats.output is the printer output.
!

PROGRAM CLUTTER_STATS

  USE CS_PARAMETERS
  USE CS_NAMELIST_MODULE

  implicit none

  include 'opaws.inc'

  character(LEN=128), parameter :: version = "Version 2.0.2:  Updated 30 August 2011 [DCD]"

  character(len=255), allocatable, dimension(:) :: swpfilename    ! names of radar-data sweep files

  real, allocatable :: r_coord(:)                    ! range coordinates of bin edges (km)
  real, allocatable :: a_coord(:)                    ! azimuth angle coordinates of bin edges (deg)
  real, allocatable :: e_coord(:)                    ! elevation angle coordinates of bin edges (deg)

  real, allocatable :: stats(:,:,:,:)                ! statistics as a function of range, azimuth angle, elevation angle, and statistic type
  character(len=20), allocatable :: stat_names(:)    ! names of statistic fields
  integer nstats                                     ! number of different statistic types

  character(len=128) infile                          ! input namelist parameter file
  character(len=128) path                            ! path name (e.g., '/data/swp*DOW2*') of sweep files to be read in
  character(len=128) sfname0                         ! sweep file name temp variable
  character(len=128) command                         ! command to be executed at unix command line
  integer ls                                         ! length of input string
  integer status                                     ! status flag returned from unix command
  integer s, r, a, e                                 ! loop indices
  integer indx, iargc
  integer index, system, iostat
  integer, parameter ::   lunr = 10

!############################################################################
!
! Read the input parameters, and allocate space for data.
!
!############################################################################
      
  write(6,*)
  write(6,'("PROGRAM CLUTTER_STATS ")')
  write(6,'(a)') version

  indx = iargc( )
  IF ( indx .eq. 1 ) THEN
  
    call getarg ( 1, infile )
    write(6,'("PROGRAM CLUTTER_STATS:  Namelist/Input file: ",a)') infile
    write(6,*)

  ELSE
  
    write(6,*) 'If nothing happens, remember to use x.oban oban.in on the command line '
 
  ENDIF

!
! Read in the namelist input data
!

  IF( .not. READ_NAMELIST(infile,'CLUTTER_STAT_PARAMS') ) THEN
    print *, 'CLUTTER_STATS:  PROBLEM READING NAMELIST'
    stop
  ELSE
    CALL WRITE_NAMELISTS(6)
    CALL VERIFY_NAMELISTS(6)
  ENDIF

! Logical for various types of sweepfile input

  IF (number_of_sweeps.eq.-1) THEN

    open(unit=lunr,file=infile,status='old')

! Added code here to skip over the namelists and get to the raw ascii input at bottom of file

    file_search_loop1: DO WHILE (.true.)
      read(lunr,*) path
      if( path(1:6) .eq. "======") exit
    ENDDO file_search_loop1

! Now do what we normally do

    read(lunr,*) path
    ls = index(path, ' ') - 1
    write(command,'(A,A,A)') 'ls -1 ',path(1:ls),' > sweep_file_list.txt'
    write(6,'(a,a)') 'command = ', command
    status = system(command)
    IF (status.ne.0) THEN
      write(6,*) 'PROGRAM CLUTTER_STATS:  Unable to execute: ', command
      stop
    ENDIF
    close(lunr)

    open(unit=11, file='sweep_file_list.txt', status='old')     ! count number of sweep files
    number_of_sweeps = 0
    iostat = 0
    DO WHILE (iostat == 0 ) 
      read(11,*,iostat=iostat) sfname0
      number_of_sweeps = number_of_sweeps + 1
    ENDDO
    close(11)
    number_of_sweeps = number_of_sweeps - 1

    allocate(swpfilename(number_of_sweeps))
    
    open(unit=11, file='sweep_file_list.txt', status='old')     ! read sweep file names
    DO s = 1,number_of_sweeps
      read(11,'(A)',end=12) swpfilename(s)
      write(6,FMT='(2x,"SWP filename:  ",i5.5,1x,a)') s, trim(swpfilename(s))
    ENDDO

12  CONTINUE

    close(11)

  ELSE

    allocate(swpfilename(number_of_sweeps))

! Added code here to skip over the namelists and get to the raw ascii input at bottom of file

    open(unit=lunr,file=infile,status='old')
    file_search_loop2: DO WHILE (.true.)
      read(lunr,*) path
      if( path(1:6) .eq. "======") exit
    ENDDO file_search_loop2

! Now do what we normally do
    
    DO s = 1,number_of_sweeps
      read(lunr,'(A)') swpfilename(s)
      write(6,FMT='("SWP filename:  ",i5.5,1x,a)') s, trim(swpfilename(s))
    ENDDO
    close(lunr)

  ENDIF
  
  write(6,*)


! Initialize arrays

  allocate(r_coord(nr+1))
  allocate(a_coord(na+1))
  allocate(e_coord(ne+1))

  do r=1, nr+1
    r_coord(r) = rmin + dr*(r-1)
  enddo
  do a=1, na+1
    a_coord(a) = amin + da*(a-1)
  enddo
  do e=1, ne+1
    e_coord(e) = emin + de*(e-1)
  enddo

  nstats = 5
  allocate(stat_names(nstats))
  stat_names(1) = 'freq_fixed_clutter'
  stat_names(2) = 'total_gates'
  stat_names(3) = 'total_sweeps'
  stat_names(4) = 'freq_moving_clutter'
  stat_names(5) = 'mean_reflectivity'
  allocate(stats(nr,na,ne,nstats))

!############################################################################
!
! Compute statistics.
!
!############################################################################

  call COMPUTE_CLUTTER_STATS(number_of_sweeps, swpfilename, input_data_format,  &
                             refl_field_name, vel_field_name,                   &
                             use_min_refl_threshold, min_refl,                  &
                             use_max_vel_threshold, max_vel,                    &
                             use_min_vel_sd_threshold, min_vel_sd,              &
                             rmin, dr, nr, amin, da, na, emin, de, ne,          &
                             nstats, stat_names, stats)

!############################################################################
!
! Output the results.
!
!############################################################################

  write(6,*) 'creating netcdf file...'
  call WRITE_NETCDF_CLUTTER_STATS(nc_output_file_name, nr, r_coord, na, a_coord, ne, e_coord, &
                                  nstats, stat_names, stats)

!############################################################################
!
! Clean up.
!
!############################################################################

  deallocate(swpfilename)
  deallocate(r_coord)
  deallocate(a_coord)
  deallocate(e_coord)
  deallocate(stats)
  deallocate(stat_names)

  write(6,*) 'finished.'

END PROGRAM CLUTTER_STATS


!###########################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######           SUBROUTINE COMPUTE_CLUTTER_STATS           ######
!     ######                                                      ######
!     ##################################################################
!
!     PURPOSE:
!
!     This subroutine computes miscellaneous reflectivity and velocity statistics
!     as a function of range, azimuth angle, and elevation angle for a set of
!     radar sweep files.  These statistics could be used from ground-
!     clutter detection.
!
!     David Dowell, March 2010
!
!     modifications by David Dowell, July 2011:
!     - compute_ppi_stats renamed to compute_clutter_stats
!     - statistics now computed in observation space
!
!############################################################################

SUBROUTINE COMPUTE_CLUTTER_STATS(number_of_sweeps, swpfilename, input_data_format,  &
                                 refl_field_name, vel_field_name,                   &
                                 use_min_refl_threshold, min_refl,                  &
                                 use_max_vel_threshold, max_vel,                    &
                                 use_min_vel_sd_threshold, min_vel_sd,              &
                                 rmin, dr, nr, amin, da, na, emin, de, ne,          &
                                 nstats, stat_names, stats)

  implicit none

  include 'opaws.inc'
  include 'structures.inc'

! input variables

  integer number_of_sweeps                ! number of sweep files
  character(len=*) swpfilename(number_of_sweeps)      ! names of radar-data sweep files
  integer input_data_format               ! format for input radar data:
                                          !   1 == dorade sweep files
                                          !   2 == netcdf (FORAY)
  character(len=8) refl_field_name        ! name of reflectivity field
  character(len=8) vel_field_name         ! name of velocity field
  logical use_min_refl_threshold          ! .true. if minimum reflectivity threshold should be used for detecting clutter
  real min_refl                           ! minimum reflectivity threshold above which clutter (fixed and/or moving) is possible (dBZ)
  logical use_max_vel_threshold           ! .true. if maximum velocity threshold should be used for detecting fixed clutter
  real max_vel                            ! maximum velocity threshold (absolute value) below which fixed clutter is possible (m/s)
  logical use_min_vel_sd_threshold        ! .true. if minimum velocity standard-deviation threshold should be used for detecting moving clutter
  real min_vel_sd                         ! minimum velocity standard-deviation threshold above which moving clutter is possible (m/s)
  real rmin                               ! minimum range of lowest range bin (km)
  real dr                                 ! range bin width (km)
  integer nr                              ! number of range bins
  real amin                               ! minimum azimuth angle of lowest azimuth bin (deg)
  real da                                 ! azimuth-angle bin width (deg)
  integer na                              ! number of azimuth-angle bins
  real emin                               ! minimum elevation angle of lowest elevation bin (deg)
  real de                                 ! elevation-angle bin width (deg)
  integer ne                              ! number of elevation-angle bins
  integer nstats                          ! number of different statistic types
  character(len=*) stat_names(nstats)     ! names of statistic fields

! returned variables
  real stats(nr,na,ne,nstats)             ! statistics as a function of range, azimuth angle, elevation angle, and statistic type

! local variables
  integer s                               ! sweep file number
  integer r                               ! ray number
  integer g                               ! gate number
  integer n                               ! range index
  integer a                               ! azimuth index
  integer e                               ! elevation index

  logical no_data_found                   ! .true. if sweepread did not return any valid rays
  logical good_gate                       ! .true. if gate has valid data
  logical clutter_gate                    ! .true. if gate meets thresholds for fixed clutter

  real rangekm_to_gate                    ! functions used by this subroutine

  integer, allocatable :: total_gates_fc(:,:,:)          ! number of data used for computing fixed clutter in (range, az, el) bin
  integer, allocatable :: total_gates_refl(:,:,:)        ! number of data used for computing reflectivity stats in (range, az, el) bin
  integer, allocatable :: total_gates_vel(:,:,:)         ! number of data used for computing reflectivity stats in (range, az, el) bin
  integer, allocatable :: total_sweeps(:,:,:)            ! number of different sweeps used for computing fixed clutter (range, az, el) bin
  logical, allocatable :: data_from_current_sweep(:,:,:) ! .true. if current sweep contributed valid data to (range, az, el) bin
  integer, allocatable :: count_fixed_clutter(:,:,:)     ! number of data meeting criteria for fixed clutter in (range, az, el) bin
  real, allocatable :: refl_sum(:,:,:)                   ! sum of reflectivity values in (range, az, el) bin
  real, allocatable :: vel_sum(:,:,:)                    ! sum of velocity values in (range, az, el) bin
  real, allocatable :: vel_sq_sum(:,:,:)                 ! sum of squared velocity values in (range, az, el) bin

  real mean                               ! average
  real sd                                 ! standard deviation
  real q                                  ! term inside square root in standard deviation computation

  type(vold_info)                     :: vold
  type(radd_info)                     :: radd
  type(celv_info)                     :: celv
  type(cfac_info)                     :: cfac
  type(parm_info), dimension(maxflds) :: parm
  type(swib_info)                     :: swib
  type(ryib_info), dimension(maxrays) :: ryib
  type(asib_info), dimension(maxrays) :: asib
  type(rdat_info), dimension(maxrays) :: rdat_r, rdat_v

  integer i                 ! index of current statistic type
  integer i_fixed_clutter   ! stat index corresponding to frequency of fixed clutter
  integer i_moving_clutter  ! stat index corresponding to frequency of moving clutter
  integer i_total_gates     ! stat index corresponding to total number of valid data values
  integer i_total_sweeps    ! stat index corresponding to total number of sweeps contributing valid data
  integer i_mean_refl       ! stat index corresponding to mean reflectivity


  i_fixed_clutter = 0
  i_moving_clutter = 0
  i_total_gates = 0
  i_total_sweeps = 0
  i_mean_refl = 0

  DO i=1, nstats
    IF (stat_names(i) .eq. 'freq_fixed_clutter')  i_fixed_clutter = i
    IF (stat_names(i) .eq. 'freq_moving_clutter') i_moving_clutter = i
    IF (stat_names(i) .eq. 'total_gates')         i_total_gates = i
    IF (stat_names(i) .eq. 'total_sweeps')        i_total_sweeps = i
    IF (stat_names(i) .eq. 'mean_reflectivity')   i_mean_refl = i
  ENDDO

  allocate(total_gates_fc(nr,na,ne))
  allocate(total_gates_refl(nr,na,ne))
  allocate(total_gates_vel(nr,na,ne))
  allocate(total_sweeps(nr,na,ne))
  allocate(data_from_current_sweep(nr,na,ne))
  allocate(count_fixed_clutter(nr,na,ne))
  allocate(refl_sum(nr,na,ne))
  allocate(vel_sum(nr,na,ne))
  allocate(vel_sq_sum(nr,na,ne))

  total_gates_fc(:,:,:) = 0
  total_gates_refl(:,:,:) = 0
  total_gates_vel(:,:,:) = 0
  total_sweeps(:,:,:) = 0
  count_fixed_clutter(:,:,:) = 0
  refl_sum(:,:,:) = 0.0
  vel_sum(:,:,:) = 0.0
  vel_sq_sum(:,:,:) = 0.0

  DO s = 1, number_of_sweeps

    data_from_current_sweep(:,:,:) = .false.

    IF (input_data_format.eq.1) THEN

      write(6,FMT='("sweep file: ",a)') trim(swpfilename(s))

      no_data_found = .false.

      write(6,FMT='("field: ",a8)') refl_field_name
      call sweepread(swpfilename(s),vold,radd,celv,cfac,parm,swib,ryib,asib,rdat_r,refl_field_name)
      IF (swib%num_rays .eq. 0) THEN
        write(6,*) 'sweepread did not return any valid rays'
        no_data_found = .true.
      ENDIF

      write(6,FMT='("field: ",a8)') vel_field_name
      call sweepread(swpfilename(s),vold,radd,celv,cfac,parm,swib,ryib,asib,rdat_v,vel_field_name)
      IF (swib%num_rays .eq. 0) THEN
        write(6,*) 'sweepread did not return any valid rays'
        no_data_found = .true.
      ENDIF

      call check_sweep_size(radd%num_param_desc, swib%num_rays)
      IF (no_data_found) swib%num_rays=0

      DO r=1, swib%num_rays
        a = nint( 0.5 + (ryib(r)%azimuth - amin) / da )
        e = nint( 0.5 + (ryib(r)%elevation - emin) / de )
        DO g = 1,celv%total_gates
          n = nint( 0.5 + (rangekm_to_gate(celv, g) - rmin) / dr )

          IF ( (n.ge.1) .and. (n.le.nr) .and. (a.ge.1) .and. (a.le.na) .and. (e.ge.1) .and. (e.le.ne) ) THEN

            good_gate = .true.
            clutter_gate = .true.

            IF (use_min_refl_threshold) THEN
              IF (rdat_r(r)%data(g) .eq. sbad) THEN
                good_gate = .false.
                clutter_gate = .false.
              ELSE IF (rdat_r(r)%data(g) .lt. min_refl) THEN
                clutter_gate = .false.
              ENDIF
            ENDIF

            IF (use_max_vel_threshold) THEN
              IF (rdat_v(r)%data(g) .eq. sbad) THEN
                good_gate = .false.
                clutter_gate = .false.
              ELSE IF (abs(rdat_v(r)%data(g)) .gt. max_vel) THEN
                clutter_gate = .false.
              ENDIF
            ENDIF

            IF (good_gate) data_from_current_sweep(n,a,e) = .true.
            IF (good_gate) total_gates_fc(n,a,e) = total_gates_fc(n,a,e) + 1
            IF (clutter_gate) count_fixed_clutter(n,a,e) = count_fixed_clutter(n,a,e) + 1

            IF (rdat_r(r)%data(g) .ne. sbad) THEN
              total_gates_refl(n,a,e) = total_gates_refl(n,a,e) + 1
              refl_sum(n,a,e) = refl_sum(n,a,e) + rdat_r(r)%data(g)
            ENDIF

            IF (rdat_v(r)%data(g) .ne. sbad) THEN
              total_gates_vel(n,a,e) = total_gates_vel(n,a,e) + 1
              vel_sum(n,a,e) = vel_sum(n,a,e) + rdat_v(r)%data(g)
              vel_sq_sum(n,a,e) = vel_sq_sum(n,a,e) + rdat_v(r)%data(g) * rdat_v(r)%data(g)
            ENDIF

          ENDIF
        ENDDO
      ENDDO

    ENDIF     ! if (input_data_format.eq.1)

    IF (input_data_format.eq.2) then
      write(6,*) 'COMPUTE_CLUTTER_STATS:  unsupported input_data_format = ', input_data_format
      stop
    ENDIF

    DO n=1, nr
      DO a=1, na
        DO e=1, ne
          IF (data_from_current_sweep(n,a,e)) total_sweeps(n,a,e) = total_sweeps(n,a,e) + 1
        ENDDO
      ENDDO
    ENDDO

  ENDDO   ! DO s = 1, number_of_sweeps


  DO n=1, nr
    DO a=1, na
      DO e=1, ne

        IF (i_total_gates .ne. 0)  stats(n,a,e,i_total_gates) = total_gates_fc(n,a,e)
        IF (i_total_sweeps .ne. 0) stats(n,a,e,i_total_sweeps) = total_sweeps(n,a,e)

        IF (total_gates_fc(n,a,e).le.0) THEN
          IF (i_fixed_clutter .ne. 0)  stats(n,a,e,i_fixed_clutter) = sbad
        ELSE
          IF (i_fixed_clutter .ne. 0)  stats(n,a,e,i_fixed_clutter) = float(count_fixed_clutter(n,a,e))  &
                                                                    / total_gates_fc(n,a,e)
        ENDIF

        IF (total_gates_refl(n,a,e).le.0) THEN
          IF (i_mean_refl .ne. 0)      stats(n,a,e,i_mean_refl) = sbad
        ELSE
          IF (i_mean_refl .ne. 0)      stats(n,a,e,i_mean_refl) = refl_sum(n,a,e) / total_gates_refl(n,a,e)
        ENDIF

        IF (i_moving_clutter .ne. 0) THEN

          stats(n,a,e,i_moving_clutter) = 1.0

          IF (use_min_vel_sd_threshold) THEN
            IF (total_gates_vel(n,a,e).le.1) THEN
              stats(n,a,e,i_moving_clutter) = 0.0
            ELSE
              mean = vel_sum(n,a,e) / total_gates_vel(n,a,e)
              q = vel_sq_sum(n,a,e) - total_gates_vel(n,a,e) * mean * mean
              q = max(q, 0.0)
              sd = sqrt( q / (total_gates_vel(n,a,e)-1.0) )
              IF (sd .lt. min_vel_sd) THEN
                stats(n,a,e,i_moving_clutter) = 0.0
              ENDIF
            ENDIF
          ENDIF

          IF (use_min_refl_threshold) THEN
            IF (total_gates_refl(n,a,e).le.0) THEN
              stats(n,a,e,i_moving_clutter) = 0.0
            ELSE
              IF ( (refl_sum(n,a,e) / total_gates_refl(n,a,e)) .lt. min_refl) THEN
                stats(n,a,e,i_moving_clutter) = 0.0
              ENDIF
            ENDIF
          ENDIF

        ENDIF

      ENDDO
    ENDDO
  ENDDO

  deallocate(total_gates_fc)
  deallocate(total_gates_refl)
  deallocate(total_gates_vel)
  deallocate(total_sweeps)
  deallocate(data_from_current_sweep)
  deallocate(count_fixed_clutter)
  deallocate(refl_sum)
  deallocate(vel_sum)
  deallocate(vel_sq_sum)

END SUBROUTINE COMPUTE_CLUTTER_STATS
