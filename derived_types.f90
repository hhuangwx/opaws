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

!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######                MODULE DATATYPES                      ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     These are the data structures that store the input radar observations
!     and the analyzed 2D or 3D fields from the objective analysis
!
!############################################################################

MODULE DTYPES_module

  TYPE OBSERVATION
    real :: x, y, z                          ! observation location relative to radar
    real :: el
    real :: az
    real :: value                            ! DCD 1/27/11
    real :: analysis_value                   ! DCD 1/27/11
    real :: time
    real :: range
  END TYPE OBSERVATION
  
  TYPE FIELDS
    integer                                      :: number_of_valid_obs
    TYPE(OBSERVATION), allocatable, dimension(:) :: ob
    character(len=8)                             :: name
    real                                         :: obmax               
    real                                         :: obmin 
    real                                         :: el  
    real                                         :: time
  END TYPE FIELDS

! DCD 11/26/10
  TYPE SWEEP_INFORMATION
    real, allocatable :: Nyquist_vel(:,:)        ! Nyquist velocity (field number, sweep number) in m/s
  END TYPE SWEEP_INFORMATION

  TYPE SWEEPS
    TYPE(FIELDS), allocatable, dimension(:)       :: field
  END TYPE SWEEPS

  TYPE VOLUME
    TYPE(SWEEPS)                                  :: sweep
    character(len=255), allocatable, dimension(:) :: filename
    integer                                       :: day                 ! Gregorian day since beginning of base year
    integer                                       :: sec                 ! seconds since beginning of day
    real                                          :: rlat, rlon, ralt    ! radar latitude (deg), longitude (deg), and altitude (km MSL)
    real                                          :: xoffset, yoffset, zoffset    ! radar location relative to grid origin
    real                                          :: missing
  END TYPE VOLUME
  
  TYPE ANALYSISGRID
    real                 :: dx, dy, dz
    real                 :: xmin, ymin, zmin
    real                 :: glat, glon, galt
    real                 :: rlat, rlon, ralt   ! radar latitude (deg), longitude (deg), and altitude (km MSL)
    real, allocatable    :: xg(:)              ! x-cordinate of analysis grid
    real, allocatable    :: yg(:)              ! y-coordinate of analysis grid
    real, allocatable    :: zg(:)              ! z-coordinate of analysis grid
                                               !  may or may not be used
    real, allocatable    :: f(:,:,:,:,:)       ! multipass data fields (nx,ny,nz,field,pass)
    real, allocatable    :: az(:,:,:)          ! interpolated azimuth angle (deg)
    real, allocatable    :: el(:,:,:)          ! interpolated elevation angle (deg)
    real, allocatable    :: height(:,:,:)      ! interpolated height of obs (km)
    real, allocatable    :: time(:,:,:)        ! time, relative to central time (sec)
    integer, allocatable :: count(:,:,:,:)     ! # gates used in interpolation
    character(len=8), allocatable :: name(:)   ! field names
    integer, allocatable :: day(:)             ! Gregorian day since beginning of base year
    integer, allocatable :: sec(:)             ! seconds since beginning of day
    real, allocatable    :: error(:)           ! standard deviation from input of observation type
  END TYPE ANALYSISGRID
  
  TYPE FORAY_DATA
    character*8              :: name
    integer                  :: nazimuths, ngates
    real, allocatable        :: az(:), field(:,:), el(:), range(:)
    integer*8, allocatable   :: time(:)
    integer                  :: missing
    integer                  :: year, month, day, hour, minute, second
    real                     :: latitude, longitude, height
    real                     :: scale_factor, add_offset
    real                     :: real_missing = -32768.
! DCD 11/26/10
    real                     :: Nyquist_Velocity
  END TYPE FORAY_DATA

END MODULE DTYPES_module
