!############################# LICENSE ######################################
!
!   Copyright (C) <2012>  <David C. Dowell and Louis J. Wicker, NOAA>
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
!     ######                PROGRAM MOSAIC_2_DART                 ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     This program converts data in a netcdf NMQ 3D reflectivity mosaic data file
!     to DART observation format.
!
!     David Dowell, May 2012
!
!############################################################################

!
!  Run this job in background with the following command line syntax:
!
!   mosaic_2_dart mosaic_2_dart.nml >&! mosaic_2_dart.output &
!
!   where mosaic_2_dart.nml is the input namelist and mosaic_2_dart.output is the printer output.
!

PROGRAM MOSAIC_2_DART

  USE MD_PARAMETERS
  USE MD_NAMELIST_MODULE
  USE DART_module
  
  implicit none

  include 'opaws.inc'

  character(LEN=128), parameter :: version = "Version 1.1:  Updated 28 June 2013 [DCD]"

  integer n_tiles
  integer tile
  integer, parameter :: max_tiles = 8

  integer nx(max_tiles)                                             ! number of gridpoints in longitude direction
  integer ny(max_tiles)                                             ! number of gridpoints in latitude direction
  integer nz(max_tiles)                                             ! number of gridpoints in vertical direction
  real, allocatable :: mrefl(:,:,:,:)   ! reflectivity (dBZ)
  real, allocatable :: height(:,:,:,:)  ! height (m MSL)
  real, allocatable :: lat(:,:,:,:)     ! latitude (deg N)
  real, allocatable :: lon(:,:,:,:)     ! longitude (deg E)

  character(len=128) nml_file                       ! input namelist file
  character(len=200) mosaic_file                    ! input netcdf mosaic file for a particular tile
  integer indx, iargc
  integer year, month, day, hour, minute, second
  integer days, secs         ! Gregorian time
  integer ls1, ls2

  integer i, j, k
  real(kind=8) olat, olon, oheight                 ! observation lat, lon (rad) and height (m)
  real qc_value
  character(len=129) qc_string
  real(kind=8) error_variance

      

  write(6,*)

  indx = iargc( )

  write(6,'("PROGRAM MOSAIC_2_DART:  running version ",a)') version

  IF ( indx .eq. 1 ) THEN
  
    call getarg ( 1, nml_file )
    write(6,'("PROGRAM MOSAIC_2_DART:  Namelist/Input file: ",a)') nml_file
    write(6,*)

  ELSE
  
    write(6,*) 'If nothing happens, remember to use mosaic_2_dart mosaic_2_dart.nml on the command line '
 
  ENDIF

! Read in the namelist data

  IF( .not. READ_NAMELIST(nml_file,'MOSAIC_2_DART_PARAMS')) THEN
   print *, 'MOSAIC_2_DART:  PROBLEM READING NAMELIST'
   stop
  ELSE
   CALL WRITE_NAMELISTS(6)
   CALL VERIFY_NAMELISTS(6)
  ENDIF
  
  if (tiled_mosaic) then
    n_tiles = 8
  else
    n_tiles = 1
  endif

  read(mosaic_file_name(1:4),*) year
  read(mosaic_file_name(5:6),*) month
  read(mosaic_file_name(7:8),*) day
  read(mosaic_file_name(10:11),*) hour
  read(mosaic_file_name(12:13),*) minute
  read(mosaic_file_name(14:15),*) second

  call set_date_gregorian(days, secs, year, month, day, hour, minute, second)

! Print info

  write(6,*) 'mosaic_directory_name = ', mosaic_directory_name
  write(6,*) 'mosaic_file_name = ', mosaic_file_name
  write(6,*) 'tiled_mosaic = ', tiled_mosaic
  write(6,*) 'dart_output_file_name = ', dart_output_file_name
  write(6,*) 'dbz_error_sd = ', dbz_error_sd
  write(6,*) 'mosaic_horiz_skip = ', mosaic_horiz_skip
  write(6,*) 'mosaic_vert_skip = ', mosaic_vert_skip
  write(6,*)
  write(6,*) 'year = ', year
  write(6,*) 'month = ', month
  write(6,*) 'day = ', day
  write(6,*) 'hour = ', hour
  write(6,*) 'minute = ', minute
  write(6,*) 'second = ', second
  write(6,*)
  write(6,*) 'days = ', days
  write(6,*) 'secs = ', secs

! Read the mosaic radar data for each tile

  allocate(mrefl(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z, n_tiles))
  allocate(height(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z, n_tiles))
  allocate(lat(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z, n_tiles))
  allocate(lon(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z, n_tiles))

  ls1 = index(mosaic_directory_name, ' ') - 1
  ls2 = index(mosaic_file_name, ' ') - 1
  do tile = 1, n_tiles
    if (tiled_mosaic) then
      write(mosaic_file, fmt='(A,A,I1,A,A)') mosaic_directory_name(1:ls1), '/tile', tile, '/', mosaic_file_name(1:ls2)
    else
      write(mosaic_file, fmt='(A,A,A)') mosaic_directory_name(1:ls1), '/', mosaic_file_name(1:ls2)
    endif
    write(6,*)
    write(6,*) 'reading from ', mosaic_file
    call READ_MOSAIC_3D(mosaic_file, tiled_mosaic, nx(tile), ny(tile), nz(tile), &
                        mrefl(1,1,1,tile), height(1,1,1,tile), lat(1,1,1,tile), lon(1,1,1,tile))
    write(6,*) 'nx, ny, nz = ', nx(tile), ny(tile), nz(tile)
  enddo

! Count number of valid obs

  num_obs = 0
  do tile=1, n_tiles
    do k=1, nz(tile), mosaic_vert_skip + 1
      do j=1, ny(tile), mosaic_horiz_skip + 1
        do i=1, nx(tile), mosaic_horiz_skip + 1
          if (mrefl(i,j,k,tile) .ne. sbad) num_obs = num_obs + 1
        enddo
      enddo
    enddo
  enddo
  write(6,*) 'num_obs = ', num_obs
  
! Write obs to DART file

  open(unit=11, file=dart_output_file_name, status='unknown')

  use_obs_kind_reflectivity = .true.
  use_obs_kind_Doppler_velocity = .false.
  use_obs_kind_clearair_reflectivity = .false.
  use_obs_kind_zdr = .false.
  use_obs_kind_kdp = .false.
  use_obs_kind_u_10m = .false.
  use_obs_kind_v_10m = .false.
  use_obs_kind_T_2m = .false.
  use_obs_kind_THETA_2m = .false.
  use_obs_kind_Td_2m = .false.
  use_obs_kind_qv_2m = .false.

  num_copies  =  1        ! observations only (no "truth")
  num_qc  =  1
  qc_string = 'QC radar'
  qc_value = 1.0
  max_num_obs = num_obs   ! This will write in the maximum number of observations at top of DART file

  call write_DART_header(11, .false., qc_string)

  num_obs = 0
  error_variance = dbz_error_sd * dbz_error_sd

  do tile=1, n_tiles
    do k=1, nz(tile), mosaic_vert_skip + 1
      do j=1, ny(tile), mosaic_horiz_skip + 1
        do i=1, nx(tile), mosaic_horiz_skip + 1
          if (mrefl(i,j,k,tile) .ne. sbad) then
            num_obs = num_obs + 1
            olat = dtor * lat(i,j,k,tile)
            olon = dtor * lon(i,j,k,tile)
            oheight = height(i,j,k,tile)
            CALL write_DART_ob(11, num_obs, mrefl(i,j,k,tile), 0.0,    &
                               olat, olon, oheight, 3, 0.0, 0.0,       &
                               0.0, num_obs, olat, olon, oheight,      &
                               obs_kind_reflectivity, secs, days,      &
                               error_variance, qc_value)
            if (mod(num_obs,10000).eq.0) write(6,*) num_obs

!            if (mrefl(i,j,k,tile) .gt. 30.0) write(6,*) 'dbz = ', mrefl(i,j,k,tile)

          endif
        enddo
      enddo
    enddo
  enddo

  close(11)


! Finish

  deallocate(mrefl)
  deallocate(height)
  deallocate(lat)
  deallocate(lon)

  write(6,*)
  write(6,*) 'PROGRAM MOSAIC_2_DART IS FINISHED.'
  write(6,*)



END PROGRAM MOSAIC_2_DART

