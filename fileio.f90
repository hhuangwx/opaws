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
!     ######               SUBROUTINE WRITENETCDF                 ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine writes out a netcdf file containing the gridded fields.
!
!     Author:  David Dowell
!
!     Creation Date:  August 2005
!
!     Latest update:  using new data structure (LJW, Feb 2010)
!
!############################################################################

    SUBROUTINE WRITENETCDF(prefix, anal, ut, vt, yr, mo, da, hr, mn, se)

      USE DTYPES_module
      USE netcdf
      USE OBAN_FIELDS

      implicit none

!---- input parameters

      character(len=*) prefix       ! prefix file name
      TYPE(ANALYSISGRID) anal
      real ut, vt                     ! storm translation velocity (m/s)
      integer yr, mo, da              ! year, month, and day
      integer hr, mn, se              ! hour, minute, and second

!---- local variables

      integer nx, ny, nz              ! no. of grid points in x, y, and z directions
      integer npass                   ! number of analysis passes on data
      integer nfld                    ! number of analysis passes on data
      real, allocatable :: sf(:)      ! scaling factors
      integer i, j, k, n, s, p
      integer ls                      ! string length
      character(len=150) filename     ! output netcdf filename
      
!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE
      
!############################################################################

      integer ncid                    ! netCDF file ID

!---- dimension lengths

      integer, parameter :: long_string_len = 80
      integer, parameter :: short_string_len = 8
      integer, parameter :: date_string_len = 10
      integer field_dims(5)

!---- dimension IDs

      integer fields_dim
      integer pass_dim
      integer long_string_dim
      integer short_string_dim
      integer date_string_dim
      integer time_dim
      integer x_dim
      integer y_dim
      integer z_dim
      integer el_dim

!---- variable IDs

      integer start_date_id
      integer end_date_id
      integer start_time_id
      integer end_time_id
      integer bad_data_flag_id
      integer grid_latitude_id
      integer grid_longitude_id
      integer grid_altitude_id
      integer radar_latitude_id
      integer radar_longitude_id
      integer radar_altitude_id
      integer x_min_id
      integer y_min_id
      integer x_max_id
      integer y_max_id
      integer x_spacing_id
      integer y_spacing_id
      integer x_id
      integer y_id
      integer z_id
      integer el_id
      integer z_spacing_id
      integer u_translation_id
      integer v_translation_id
      integer field_names_id
      integer elev_id
      integer AZ_id
      integer TIME_id
      integer HEIGHT_id
      integer field_ids(20)

!---- variable shapes

      integer start_date_dims(1)
      integer end_date_dims(1)
      integer start_time_dims(1)
      integer end_time_dims(1)
      integer x_dims(1)
      integer y_dims(1)
      integer z_dims(1)
      integer el_dims(1)
      integer field_names_dims(2)
      integer fourd_dims(4)

!---- variable declarations

      integer bad_data_flag
      real  grid_latitude
      real  grid_longitude
      real  grid_altitude
      real  radar_latitude
      real  radar_longitude
      real  radar_altitude
      real  x_min
      real  y_min
      real  x_max
      real  y_max
      real  x_spacing
      real  y_spacing
      real  x(size(anal%xg))
      real  y(size(anal%yg))
      real  z(size(anal%zg))
      real  el(size(anal%zg))
      real  z_spacing
      real  u_translation
      real  v_translation

!---- attribute vectors

      integer textval(1)
      double precision doubleval(1)

!---- strings

      character (len=10) start_date
      character (len=10) end_date
      character (len=8) start_time
      character (len=8) end_time
      character (len=8) :: fieldname(6)

!---- get dimension sizes                                                                                                                                                                                                   
      nx    = size(anal%xg)
      ny    = size(anal%yg)
      nz    = size(anal%zg)
      nfld  = size(anal%f,dim=4)
      npass = size(anal%f,dim=5)

      write(6,*)
      write(6,FMT='(" WRITENETCDF -> INPUT DIMS NX:    ", i3)') nx
      write(6,FMT='(" WRITENETCDF -> INPUT DIMS NY:    ", i3)') ny
      write(6,FMT='(" WRITENETCDF -> INPUT DIMS NZ:    ", i3)') nz
      write(6,FMT='(" WRITENETCDF -> INPUT DIMS NPASS: ", i3)') npass

      allocate(sf(nfld))
      sf(:) = 1.0

!---- write netcdf filename and open file

      ls = index(prefix, ' ') - 1
      filename=prefix(1:ls)//".nc"

!---- enter define mode

      call check(nf90_create(filename, nf90_clobber, ncid));

!---- define dimensions
      
      call check(nf90_def_dim(ncid, 'fields', nfld+4, fields_dim));
      call check(nf90_def_dim(ncid, 'pass', npass, pass_dim));
      call check(nf90_def_dim(ncid, 'long_string', long_string_len, long_string_dim));
      call check(nf90_def_dim(ncid, 'short_string', short_string_len, short_string_dim));
      call check(nf90_def_dim(ncid, 'date_string', date_string_len, date_string_dim));
      call check(nf90_def_dim(ncid, 'time', NF90_UNLIMITED, time_dim));
      call check(nf90_def_dim(ncid, 'x', nx, x_dim));
      call check(nf90_def_dim(ncid, 'y', ny, y_dim));
      call check(nf90_def_dim(ncid, 'z', nz, z_dim));
      call check(nf90_def_dim(ncid, 'el', nz, el_dim));

!---- define variables

      start_date_dims(1) = date_string_dim
      call check(nf90_def_var(ncid, 'start_date', nf90_char, start_date_dims, start_date_id));

      end_date_dims(1) = date_string_dim
      call check(nf90_def_var(ncid, 'end_date', nf90_char, end_date_dims, end_date_id));

      start_time_dims(1) = short_string_dim
      call check(nf90_def_var(ncid, 'start_time', nf90_char, start_time_dims, start_time_id));

      end_time_dims(1) = short_string_dim
      call check(nf90_def_var(ncid, 'end_time', nf90_char, end_time_dims, end_time_id));

      call check(nf90_def_var(ncid, 'bad_data_flag', nf90_int, bad_data_flag_id));
      call check(nf90_def_var(ncid, 'grid_latitude', nf90_float, grid_latitude_id));
      call check(nf90_def_var(ncid, 'grid_longitude', nf90_float, grid_longitude_id));
      call check(nf90_def_var(ncid, 'grid_altitude', nf90_float, grid_altitude_id));
      call check(nf90_def_var(ncid, 'radar_latitude', nf90_float, radar_latitude_id));
      call check(nf90_def_var(ncid, 'radar_longitude', nf90_float, radar_longitude_id));
      call check(nf90_def_var(ncid, 'radar_altitude', nf90_float, radar_altitude_id))
      call check(nf90_def_var(ncid, 'x_min', nf90_float, x_min_id));
      call check(nf90_def_var(ncid, 'y_min', nf90_float, y_min_id));
      call check(nf90_def_var(ncid, 'x_max', nf90_float, x_max_id));
      call check(nf90_def_var(ncid, 'y_max', nf90_float, y_max_id));
      call check(nf90_def_var(ncid, 'x_spacing', nf90_float, x_spacing_id));
      call check(nf90_def_var(ncid, 'y_spacing', nf90_float, y_spacing_id));

      x_dims(1) = x_dim
      call check(nf90_def_var(ncid, 'x', nf90_float, x_dims, x_id));

      y_dims(1) = y_dim
      call check(nf90_def_var(ncid, 'y', nf90_float, y_dims, y_id));

      z_dims(1) = z_dim
      call check(nf90_def_var(ncid, 'z', nf90_float, z_dims, z_id));

      el_dims(1) = z_dim
      call check(nf90_def_var(ncid, 'el', nf90_float, el_dims, el_id));

      call check(nf90_def_var(ncid, 'z_spacing', nf90_float, z_spacing_id));
      call check(nf90_def_var(ncid, 'u_translation', nf90_float, u_translation_id));
      call check(nf90_def_var(ncid, 'v_translation', nf90_float, v_translation_id));

      field_names_dims(1) = short_string_dim
      field_names_dims(2) = fields_dim
      call check(nf90_def_var(ncid, 'field_names', nf90_char, field_names_dims, field_names_id));

      DO n = 1,nfld
        fieldname(n)=anal%name(n)
      ENDDO

      fieldname(nfld+1)='EL'
      fieldname(nfld+2)='AZ'
      fieldname(nfld+3)='TIME'
      fieldname(nfld+4)='HEIGHT'

      fourd_dims(1) = x_dim
      fourd_dims(2) = y_dim
      fourd_dims(3) = z_dim
      fourd_dims(4) = time_dim

      call check(nf90_def_var(ncid, 'EL', nf90_float, fourd_dims, elev_id));
      call check(nf90_def_var(ncid, 'AZ', nf90_float, fourd_dims, AZ_id));
      call check(nf90_def_var(ncid, 'TIME', nf90_float, fourd_dims, TIME_id));
      call check(nf90_def_var(ncid, 'HEIGHT', nf90_float, fourd_dims, HEIGHT_id));

!---- Define field variables (e.g. DBZ, VEL) and assign attributes

      field_dims(1) = x_dim
      field_dims(2) = y_dim
      field_dims(3) = z_dim
      field_dims(4) = pass_dim
      field_dims(5) = time_dim

      DO n = 1,nfld
        ls = index(anal%name(n), ' ') - 1
        call check(nf90_def_var(ncid, anal%name(n)(1:ls), nf90_float, field_dims, field_ids(n)))
        call check(nf90_put_att(ncid, field_ids(n), 'scale_factor', sf(n)))
        call check(nf90_put_att(ncid, field_ids(n), 'add_offset', 0.0))
        call check(nf90_put_att(ncid, field_ids(n), 'missing_value', sbad))
      ENDDO

!---- assign variable attributes

      call check(nf90_put_att(ncid, grid_latitude_id, 'value', 'Grid origin latitude'))
      call check(nf90_put_att(ncid, grid_latitude_id, 'units', 'deg'))

      call check(nf90_put_att(ncid, grid_longitude_id, 'value', 'Grid origin longitude'))
      call check(nf90_put_att(ncid, grid_longitude_id, 'units', 'deg'))

      call check(nf90_put_att(ncid, grid_altitude_id, 'value', 'Altitude of grid origin'))
      call check(nf90_put_att(ncid, grid_altitude_id, 'units', 'km MSL'))

      call check(nf90_put_att(ncid, radar_latitude_id, 'value', 'Radar latitude'))
      call check(nf90_put_att(ncid, radar_latitude_id, 'units', 'deg'))

      call check(nf90_put_att(ncid, radar_longitude_id, 'value', 'Radar longitude'))
      call check(nf90_put_att(ncid, radar_longitude_id, 'units', 'deg'))

      call check(nf90_put_att(ncid, radar_altitude_id, 'value', 'Altitude of radar'))
      call check(nf90_put_att(ncid, radar_altitude_id, 'units', 'km MSL'))

      call check(nf90_put_att(ncid, x_min_id, 'value', 'The minimum x grid value'))
      call check(nf90_put_att(ncid, x_min_id, 'units', 'km'))

      call check(nf90_put_att(ncid, y_min_id, 'value', 'The minimum y grid value'))
      call check(nf90_put_att(ncid, y_min_id, 'units', 'km'))

      call check(nf90_put_att(ncid, x_max_id, 'value', 'The maximum x grid value'))
      call check(nf90_put_att(ncid, x_max_id, 'units', 'km'))

      call check(nf90_put_att(ncid, y_max_id, 'value', 'The maximum y grid value'))
      call check(nf90_put_att(ncid, y_max_id, 'units', 'km'))

      call check(nf90_put_att(ncid, x_spacing_id, 'units', 'km'))
      call check(nf90_put_att(ncid, y_spacing_id, 'units', 'km'))

      call check(nf90_put_att(ncid, u_translation_id, 'value', 'storm motion u component'))
      call check(nf90_put_att(ncid, u_translation_id, 'units', 'm/s'))

      call check(nf90_put_att(ncid, v_translation_id, 'value', 'storm motion v component'))
      call check(nf90_put_att(ncid, v_translation_id, 'units', 'm/s'))

      call check(nf90_put_att(ncid, elev_id, 'scale_factor', sf(1)))
      call check(nf90_put_att(ncid, elev_id, 'add_offset', 0.0))
      call check(nf90_put_att(ncid, elev_id, 'missing_value', sbad))

      call check(nf90_put_att(ncid, AZ_id, 'scale_factor', sf(1)))
      call check(nf90_put_att(ncid, AZ_id, 'add_offset', 0.0))
      call check(nf90_put_att(ncid, AZ_id, 'missing_value', sbad))

      call check(nf90_put_att(ncid, TIME_id, 'scale_factor', sf(1)))
      call check(nf90_put_att(ncid, TIME_id, 'add_offset', 0.0))
      call check(nf90_put_att(ncid, TIME_id, 'missing_value', sbad))

      call check(nf90_put_att(ncid, HEIGHT_id, 'scale_factor', sf(1)))
      call check(nf90_put_att(ncid, HEIGHT_id, 'add_offset', 0.0))
      call check(nf90_put_att(ncid, HEIGHT_id, 'missing_value', sbad))

!---- leave define mode

      call check(nf90_enddef(ncid));

!---- write variables                                                                                                                                                                                                     

      write(start_date, '(I2.2, A, I2.2, A, I4.4)') mo, '/', da, '/', yr
      write(end_date, '(I2.2, A, I2.2, A, I4.4)') mo, '/', da, '/', yr
      write(start_time, '(I2.2, A, I2.2, A, I2.2)') hr, ':', mn, ':', se
      write(end_time, '(I2.2, A, I2.2, A, I2.2)') hr, ':', mn, ':', se

      call check( nf90_put_var(ncid, start_date_id, start_date) )
      call check( nf90_put_var(ncid, end_date_id, end_date) )
      call check( nf90_put_var(ncid, start_time_id, start_time) )
      call check( nf90_put_var(ncid, end_time_id, end_time) )
      call check( nf90_put_var(ncid, bad_data_flag_id, int(sbad)) )
      call check( nf90_put_var(ncid, grid_latitude_id, anal%glat) )
      call check( nf90_put_var(ncid, grid_longitude_id, anal%glon) )
      call check( nf90_put_var(ncid, grid_altitude_id, anal%galt) )
      call check( nf90_put_var(ncid, radar_latitude_id, anal%rlat) )
      call check( nf90_put_var(ncid, radar_longitude_id, anal%rlon) )
      call check( nf90_put_var(ncid, radar_altitude_id, anal%ralt) )
      call check( nf90_put_var(ncid, x_min_id, anal%xmin) )
      call check( nf90_put_var(ncid, y_min_id, anal%ymin) )
      call check( nf90_put_var(ncid, x_max_id, anal%xg(nx)) )
      call check( nf90_put_var(ncid, y_max_id, anal%yg(ny)) )
      call check( nf90_put_var(ncid, x_spacing_id, anal%dx) )
      call check( nf90_put_var(ncid, y_spacing_id, anal%dy) )
      call check( nf90_put_var(ncid, z_spacing_id, anal%dz) )
      call check( nf90_put_var(ncid, u_translation_id, ut) )
      call check( nf90_put_var(ncid, v_translation_id, vt) )
      call check( nf90_put_var(ncid, x_id, anal%xg) )
      call check( nf90_put_var(ncid, y_id, anal%yg) )
      call check( nf90_put_var(ncid, z_id, anal%zg) )
      call check( nf90_put_var(ncid, el_id, anal%zg) )
      call check( nf90_put_var(ncid, elev_id, anal%el) )
      call check( nf90_put_var(ncid, AZ_id, anal%az) )
      call check( nf90_put_var(ncid, TIME_id, anal%time) )
      call check( nf90_put_var(ncid, HEIGHT_id, anal%height) )
      call check( nf90_put_var(ncid, field_names_id, fieldname) )
      
      DO n = 1,nfld
        call check( nf90_put_var(ncid, field_ids(n), anal%f(:,:,:,n,:)) )
      ENDDO

      call check( nf90_close(ncid) )

    deallocate(sf)

    RETURN

END SUBROUTINE WRITENETCDF
!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######               SUBROUTINE DART_radar_out              ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine outputs a PPI single-Doppler file in
!     the ascii format that can be read by DART.
!
!############################################################################
!
!     Author:  David Dowell
!
!     Creation Date:  23 March 2005
!
!     Modifications:
!     * Feb 2010 - version 2 created by L Wicker to accomodate
!                  the new objective analysis code and data structure
!     * Nov 2010 - D Dowell changed subroutine name from DARTout2
!                  to DART_radar_out and incorporated updates from OPAWS 1.6.1
!
!############################################################################

! DCD 11/26/10
  SUBROUTINE DART_radar_out(prefix, anal, sweep_info, map_proj,   &
                            nfld, min_threshold, fill_flag, fill_value, use_clear_air_type, clear_air_skip)

    USE DTYPES_module
    USE DART_module

    implicit none

    include 'opaws.inc'

! Passed variables:  possibly changed by subroutine (if use_clear_air_type is .true.)
    TYPE(ANALYSISGRID) :: anal

! Passed variables:  not changed by subroutine
    character(len=*) prefix   ! prefix of the output file name
    TYPE(SWEEP_INFORMATION) :: sweep_info      ! info. about individual sweeps, such as Nyquist velocity (m/s) for each field
    integer map_proj             ! map projection (for relating lat, lon to x, y):
                                 !   0 = flat earth
                                 !   1 = oblique azimuthal
                                 !   2 = Lambert conformal
    integer nfld                 ! number of fields
    real min_threshold(nfld)     ! value below which input observations are set to missing
    integer fill_flag(nfld)      ! fill missing values with a specified value? (0=no, 1=yes)
    real fill_value(nfld)        ! replacement value, if fill_flag==1
    logical use_clear_air_type   ! .true. (.false.) if clear-air reflectivity ob. type should (not) be used for DART output
    integer clear_air_skip       ! thinning factor for clear air reflectivity data
      
! Local variables
    integer nswp                                     ! number of radar sweeps
    integer nx, ny                                   ! no. of grid points in x and y directions
    integer npass                                    ! index of final pass in objective analysis
    integer i, j, k, ii, jj                          ! loop variables
    integer imin, imax, istep, jmin, jmax, jstep
    integer fi; parameter(fi=23)                     ! file unit number
    integer dbz_index, vr_index, kdp_index, zdr_index
    real    ob_value
    real glat, glon                                  ! grid lat and lon (rad)
    real(kind=8) rlat, rlon, rheight                 ! radar lat, lon (rad) and height (m)
    real lat, lon                                    ! lat and lon (rad)
    real(kind=8) olat, olon, oheight                 ! observation lat, lon (rad) and height (m)
    real x, y                                        ! observation coordinates (km)
    real(kind=8) error_variance
    real qc_value
    character(len=129) qc_string
    integer ls
    integer vr_count                                 ! current count of number of Doppler velocity obs
    integer dbz_count                                ! current count of number of reflectivity obs
    integer clear_air_count                          ! current count of number of clear-air reflectivity obs
    integer zdr_count                                ! current count of number of differential reflectivity obs
    integer kdp_count                                ! current count of number of specific differential phase obs
    logical, allocatable :: dbz_ob_is_clear_air(:,:,:)   ! .true. if observation is a clear-air reflectivity observation



!############################################################################
!
!   Get dimension sizes, identify valid observation types, initialize values

    nx    = size(anal%xg)
    ny    = size(anal%yg)
    nswp    = size(anal%zg)
! DCD 11/26/10
!    nfld  = size(anal%f,dim=4)
    npass = size(anal%f,dim=5)

    allocate(dbz_ob_is_clear_air(nx,ny,nswp))
    dbz_ob_is_clear_air(:,:,:) = .false.

    glat    = dtor*anal%glat
    glon    = dtor*anal%glon
    rlat    = dtor*anal%rlat
    rlon    = dtor*anal%rlon
    rheight = 1000.0*anal%ralt

    IF (nfld .le. 0) THEN
      write(6,*) 'error:  nfld = ', nfld
      stop
    ENDIF

    use_obs_kind_Doppler_velocity = .false.
    use_obs_kind_reflectivity = .false.
    use_obs_kind_clearair_reflectivity = .false.
    use_obs_kind_zdr = .false.
    use_obs_kind_kdp = .false.
    use_obs_kind_u_10m = .false.
    use_obs_kind_v_10m = .false.
    use_obs_kind_T_2m = .false.
    use_obs_kind_THETA_2m = .false.
    use_obs_kind_Td_2m = .false.
    use_obs_kind_qv_2m = .false.

    dbz_index = 0
    vr_index = 0
    zdr_index = 0
    kdp_index = 0

    DO i = 1,nfld

! DCD 11/23/10
      if ( index(anal%name(i),'dBZ') .ne. 0) dbz_index=i
      if ( index(anal%name(i),'DZ' ) .ne. 0) dbz_index=i
      if ( index(anal%name(i),'DBZ') .ne. 0) dbz_index=i
      if ( index(anal%name(i),'REF') .ne. 0) dbz_index=i

      if ( index(anal%name(i),'VU') .ne. 0) vr_index=i
      if ( index(anal%name(i),'VE') .ne. 0) vr_index=i
      if ( index(anal%name(i),'VEL') .ne. 0) vr_index=i
      if ( index(anal%name(i),'VR') .ne. 0) vr_index=i
      if ( index(anal%name(i),'VT') .ne. 0) vr_index=i
      if ( index(anal%name(i),'DV') .ne. 0) vr_index=i

      if ( index(anal%name(i),'DR') .ne. 0) zdr_index=i

      if ( index(anal%name(i),'KD') .ne. 0) kdp_index=i

    ENDDO

    write(6,*)
    if (dbz_index.eq.0) then
      write(6,*) 'DART_radar_out:  NO REFLECTIVITY FIELD FOUND'
    else
      write(6,*) 'DART_radar_out:  Found reflectivity field:  ', dbz_index, anal%name(dbz_index)
    endif
    if (vr_index.eq.0) then
      write(6,*) 'DART_radar_out:  NO VELOCITY FIELD FOUND'
    else
      write(6,*) 'DART_radar_out:  Found velocity field:  ', vr_index, anal%name(vr_index)
    endif
    if (zdr_index.eq.0) then
      write(6,*) 'DART_radar_out:  NO DIFFERENTIAL REFLECTIVITY FIELD FOUND'
    else
      write(6,*) 'DART_radar_out:  Found differential reflectivity field:  ', zdr_index, anal%name(zdr_index)
    endif
    if (kdp_index.eq.0) then
      write(6,*) 'DART_radar_out:  NO SPECIFIC DIFFERENTIAL PHASE FIELD FOUND'
    else
      write(6,*) 'DART_radar_out:  Found specific differential phase field:  ', kdp_index, anal%name(kdp_index)
    endif

! DCD 11/30/10
!     Identify, modify value of, and/or thin clear-air reflectivity observations.

    if ( (dbz_index.ne.0) .and. (fill_flag(dbz_index).eq.1) .and. use_clear_air_type ) then

      write(6,*)
      write(6,*) 'identifying clear-air reflectivity observations...'
      if (clear_air_skip.gt.0) write(6,*) 'thinning clear-air reflectivity observations...'

      do k=1, nswp

        ! identify clear-air observations

        do j=1, ny
          do i=1, nx
            if ( (anal%f(i,j,k,dbz_index,npass).ne.sbad) .and. (anal%az(i,j,k).ne.sbad) .and.     &
     &           (anal%el(i,j,k).ne.sbad) .and. (anal%height(i,j,k).ne.sbad) .and.                &
     &           (abs(anal%f(i,j,k,dbz_index,npass)-fill_value(dbz_index)).lt.0.01) ) then
              dbz_ob_is_clear_air(i,j,k) = .true.
            endif
            if ( (anal%f(i,j,k,dbz_index,npass).ne.sbad) .and. (anal%az(i,j,k).ne.sbad) .and.     &
     &           (anal%el(i,j,k).ne.sbad) .and. (anal%height(i,j,k).ne.sbad) .and.                &
     &           (anal%f(i,j,k,dbz_index,npass).lt.min_threshold(dbz_index)) ) then
              anal%f(i,j,k,dbz_index,npass) = fill_value(dbz_index)          ! replace with fill value
              dbz_ob_is_clear_air(i,j,k) = .true.
            endif
          enddo
        enddo

        ! thin clear-air observations

        ! the index order is changed from sweep to sweep to make the observation-location distributions
        ! less similar from one sweep to the next

        if (clear_air_skip.gt.0) then

          select case(mod(k,4))
            case(0)
              imin = 1
              imax = nx
              istep = 1
              jmin = 1
              jmax = ny
              jstep = 1
            case(1)
              imin = nx
              imax = 1
              istep = -1
              jmin = 1
              jmax = ny
              jstep = 1
            case(2)
              imin = 1
              imax = nx
              istep = 1
              jmin = ny
              jmax = 1
              jstep = -1
            case(3)
              imin = nx
              imax = 1
              istep = -1
              jmin = ny
              jmax = 1
              jstep = -1
          end select

          do j=jmin, jmax, jstep
            do i=imin, imax, istep

              if (dbz_ob_is_clear_air(i,j,k)) then

                do ii=max(1, i-clear_air_skip), min(nx, i+clear_air_skip)
                  do jj=max(1, j-clear_air_skip), min(ny, j+clear_air_skip)
                  
                    if ( (ii.eq.i) .and. (jj.eq.j) ) then
                        ! do nothing
                    else if (dbz_ob_is_clear_air(ii,jj,k)) then
                    
!                      write(6,*) 'removing clear-air observation at ', ii, jj

                      anal%f(ii,jj,k,dbz_index,npass) = sbad
                      dbz_ob_is_clear_air(ii,jj,k) = .false.

                    endif
                  enddo
                enddo
              
              endif

            enddo
          enddo
          
        endif       ! if (clear_air_skip.gt.0)

      enddo

    endif      ! if ( (dbz_index.ne.0) .and. (fill_flag(dbz_index).eq.1) .and. use_clear_air_type )


! Count number of valid observations.

    num_obs = 0

! Need to check first to see if variable index NE 0  BEFORE we use it to check value,
!      else we get an out of bounds error

    DO k = 1,nswp
      DO j = 1,ny
        DO i = 1,nx

          IF ( (anal%az(i,j,k)                .ne. sbad) .and. &
               (anal%el(i,j,k)                .ne. sbad) .and. &
               (anal%height(i,j,k)            .ne. sbad)         ) THEN

            IF ( dbz_index .ne. 0 ) THEN

              IF( anal%f(i,j,k,dbz_index,npass) .ne. sbad ) THEN
                num_obs = num_obs + 1
                use_obs_kind_reflectivity = .true.
                IF (use_clear_air_type) use_obs_kind_clearair_reflectivity = .true.
              ENDIF

            ENDIF

            IF ( vr_index .ne. 0 ) THEN

              IF( anal%f(i,j,k,vr_index,npass) .ne. sbad ) THEN
                num_obs = num_obs + 1
                use_obs_kind_Doppler_velocity = .true.
              ENDIF

            ENDIF

            IF ( kdp_index .ne. 0 ) THEN

              IF( anal%f(i,j,k,kdp_index,npass) .ne. sbad ) THEN
                num_obs = num_obs + 1
                use_obs_kind_kdp = .true.
              ENDIF

            ENDIF

            IF ( zdr_index .ne. 0 ) THEN

              IF( anal%f(i,j,k,zdr_index,npass) .ne. sbad ) THEN
                num_obs = num_obs + 1
                use_obs_kind_zdr = .true.
              ENDIF

            ENDIF

          ENDIF  ! END OUTER CHECK FOR GOOD LOCATION

        ENDDO
      ENDDO
    ENDDO

    write(6,*) "DART_radar_out:  Writing ", num_obs, " obs to DART file"

! Write header information.

    ls = index(prefix, ' ') - 1
! DCD 11/24/10
    open(unit=fi, file='obs_seq_'//prefix(1:ls)//'.out', status='unknown')

    num_copies  =  1        ! observations only (no "truth")
    num_qc  =  1
    qc_string = 'QC radar'
    qc_value = 1.0
    max_num_obs = num_obs   ! This will write in the maximum number of observations at top of DART file

    call write_DART_header(fi, qc_string)

! Write observations.

    num_obs         = 0
    vr_count        = 0
    dbz_count       = 0
    clear_air_count = 0
    zdr_count       = 0
    kdp_count       = 0

    DO k = 1,nswp
      DO j = 1,ny
        DO i = 1,nx

! Again - check to see if location is valid, then check individual variables

          IF( (anal%az(i,j,k)                .ne. sbad) .and. &
              (anal%el(i,j,k)                .ne. sbad) .and. &
              (anal%height(i,j,k)            .ne. sbad)      ) THEN   

            x = anal%xg(i)
            y = anal%yg(j)

            CALL xy_to_ll(lat, lon, map_proj, x, y, glat, glon)

            olat    = lat
            olon    = lon
            oheight = 1000.0 * (anal%galt + anal%height(i,j,k))

            IF( dbz_index .ne. 0) THEN
              IF( anal%f(i,j,k,dbz_index,npass) .ne. sbad) THEN
                num_obs        = num_obs + 1
                dbz_count      = dbz_count + 1
                ob_value       = anal%f(i,j,k,dbz_index,npass)
                error_variance = anal%error(dbz_index)**2

! DCD 12/1/10
                IF( dbz_ob_is_clear_air(i,j,k) ) THEN
                  clear_air_count = clear_air_count + 1
                  CALL write_DART_ob(fi, num_obs, ob_value, 0.0,                                   &
                                     olat, olon, oheight, 3, anal%az(i,j,k), anal%el(i,j,k),       &
                                     0.0, dbz_count, rlat, rlon, rheight,                          &
                                     obs_kind_clearair_reflectivity, anal%sec(k), anal%day(k),     &
                                     error_variance, qc_value)
                ELSE      
                  CALL write_DART_ob(fi, num_obs, ob_value, 0.0,                                   &
                                     olat, olon, oheight, 3, anal%az(i,j,k), anal%el(i,j,k),       &
                                     0.0, dbz_count, rlat, rlon, rheight,                          &
                                     obs_kind_reflectivity, anal%sec(k), anal%day(k),              &
                                     error_variance, qc_value)
                ENDIF  ! end clear air

              ENDIF    ! end valid data
            ENDIF    ! end DBZ_INDEX
          
            IF( vr_index .ne. 0) THEN
              IF( anal%f(i,j,k,vr_index,npass) .ne. sbad) THEN
                num_obs        = num_obs + 1
                vr_count       = vr_count + 1
                ob_value       = anal%f(i,j,k,vr_index,npass)
                error_variance = anal%error(vr_index)**2

                CALL write_DART_ob(fi, num_obs, ob_value, 0.0,                                         &
                                   olat, olon, oheight, 3, anal%az(i,j,k), anal%el(i,j,k),             &
                                   sweep_info%Nyquist_vel(vr_index,k), vr_count, rlat, rlon, rheight,  &
                                   obs_kind_Doppler_velocity, anal%sec(k), anal%day(k),                &
                                   error_variance, qc_value)

              ENDIF ! end valid data
            ENDIF   ! end vr_index

            IF( kdp_index .ne. 0) THEN
              IF(anal%f(i,j,k,kdp_index,npass) .ne. sbad) THEN
                num_obs        = num_obs + 1
                kdp_count      = kdp_count + 1
                ob_value       = anal%f(i,j,k,kdp_index,npass)
                error_variance = anal%error(kdp_index)**2

                CALL write_DART_ob(fi, num_obs, ob_value, 0.0,                                   &
                                   olat, olon, oheight, 3, anal%az(i,j,k), anal%el(i,j,k),       &
                                   0.0, kdp_count, rlat, rlon, rheight,                          &
                                   obs_kind_kdp, anal%sec(k), anal%day(k), error_variance, qc_value)

              ENDIF  ! end valid data
            ENDIF ! end kdp_index

            IF(zdr_index .ne. 0) THEN
              IF(anal%f(i,j,k,zdr_index,npass) .ne. sbad) THEN
                num_obs        = num_obs + 1
                zdr_count      = zdr_count + 1
                ob_value       = anal%f(i,j,k,zdr_index,npass)
                error_variance = anal%error(zdr_index)**2

                CALL write_DART_ob(fi, num_obs, ob_value, 0.0,                                   &
                                   olat, olon, oheight, 3, anal%az(i,j,k), anal%el(i,j,k),       &
                                   0.0, zdr_count, rlat, rlon, rheight,                          &
                                   obs_kind_zdr, anal%sec(k), anal%day(k), error_variance, qc_value)

              ENDIF  ! end valid data
            ENDIF  ! end zdr_index

          ENDIF  ! end valid location

        ENDDO
      ENDDO
    ENDDO

    deallocate(dbz_ob_is_clear_air)

    write(6,*) 'DART_radar_out:  number of vr obs                        = ', vr_count
    write(6,*) 'DART_radar_out:  number of dbz obs (including clear air) = ', dbz_count
    write(6,*) 'DART_radar_out:  number of clear air dbz obs             = ', clear_air_count

    close(fi)

  RETURN
  END

!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######          SUBROUTINE WRITE_NETCDF_PPI_STATS           ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine writes out a netcdf file containing gridded PPI statistics.
!
!     Author:  David Dowell
!
!     Creation Date:  March 2010
!
!############################################################################

      subroutine write_netcdf_ppi_stats(ncfile, ncgen_command, nfld, fname, f_units,           &
                                        nstats, stat_names, stats, num_el_angles, el_angles,   &
                                        glat, glon, galt, rlat, rlon, ralt,                    &
                                        nx, ny, dx, dy, xmin, ymin, ut, vt,                    &
                                        yr, mo, da, hr, mn, se)

      implicit none

      include 'opaws.inc'

!---- input parameters

      character(len=80) ncfile         ! netcdf file name
      character(len=*) ncgen_command  ! path/executable for local "ncgen" command
      integer nfld                      ! number of fields
      character(len=8) fname(nfld)      ! field names
      character(len=20) f_units(nfld)   ! units
      integer nx, ny                    ! no. of grid points in x and y directions
      integer nstats                    ! number of different statistic types
      integer num_el_angles             ! number of different elevation angles for which statistics were computed
      character(len=20) stat_names(nstats)          ! names of statistic fields
      real stats(nx,ny,num_el_angles,nstats,nfld)   ! statistics as a function of space, elevation angle, statistic type, and field
      real el_angles(num_el_angles)     ! elevation angles (deg) for which statistics were computed
      real glat, glon                   ! latitude and longitude of grid origin (deg)
      real galt                         ! altitude of grid origin (km MSL)
      real rlat, rlon                   ! latitude and longitude of radar (deg)
      real ralt                         ! altitude of radar (km MSL)
      real dx, dy                       ! grid spacing in x and y directions (km)
      real xmin, ymin                   ! horizontal coordinates of lower southwest corner
                                        !   of grid, relative to origin (km)
      real ut, vt                       ! storm translation velocity (m/s)
      integer yr, mo, da                ! year, month, and day
      integer hr, mn, se                ! hour, minute, and second


!---- local variables

      integer i, j, k, n, s, q
      integer ls, ls2                   ! string length
      character(len=150) command
      real sf(nfld)                     ! scaling factor


      sf(:) = 1.0

! DCD 1/26/11:  commented this section out because "bad" is no longer used in OPAWS2
!      do n=1, nfld
!        do s=1, nstats
!          do k=1, num_el_angles
!            do j=1, ny
!              do i=1, nx
!                if (stats(i,j,k,s,n).eq.bad) stats(i,j,k,s,n)=sbad
!              enddo
!            enddo
!          enddo
!        enddo
!      enddo

!     open ascii file that will be converted to netcdf

      open(unit=11, file='ncgen.input', status='unknown')

      ls = index(ncfile, ' ') - 1
      write(11,'(3A)') 'netcdf ', ncfile(1:ls), ' {'

!     "dimensions" section

      write(11,'(A)') 'dimensions:'
      write(11,'(T9,A)') 'long_string = 20 ;'
      write(11,'(T9,A)') 'short_string = 8 ;'
      write(11,'(T9,A)') 'date_string = 10 ;'
      write(11,'(T9,A,I0,A)') 'x = ', nx, ' ;'
      write(11,'(T9,A,I0,A)') 'y = ', ny, ' ;'
      write(11,'(T9,A,I0,A)') 'el = ', num_el_angles, ' ;'
      write(11,'(T9,A,I0,A)') 'nstats = ', nstats, ' ;'

!     "variables" section

      write(11,'(A)') 'variables:'
      write(11,'(T9,A)') 'char start_date(date_string) ;'
      write(11,'(T9,A)') 'char end_date(date_string) ;'
      write(11,'(T9,A)') 'char start_time(short_string) ;'
      write(11,'(T9,A)') 'char end_time(short_string) ;'
      write(11,'(T9,A)') 'int bad_data_flag ;'
      write(11,'(T9,A)') 'float grid_latitude ;'
      write(11,'(T17,A)') 'grid_latitude:value = "Grid origin latitude" ;'
      write(11,'(T17,A)') 'grid_latitude:units = "deg" ;'
      write(11,'(T9,A)') 'float grid_longitude ;'
      write(11,'(T17,A)') 'grid_longitude:value = "Grid origin longitude" ;'
      write(11,'(T17,A)') 'grid_longitude:units = "deg" ;'
      write(11,'(T9,A)') 'float grid_altitude ;'
      write(11,'(T17,A)') 'grid_altitude:value = "Altitude of grid origin" ;'
      write(11,'(T17,A)') 'grid_altitude:units = "km MSL" ;'
      write(11,'(T9,A)') 'float radar_latitude ;'
      write(11,'(T17,A)') 'radar_latitude:value = "Radar latitude" ;'
      write(11,'(T17,A)') 'radar_latitude:units = "deg" ;'
      write(11,'(T9,A)') 'float radar_longitude ;'
      write(11,'(T17,A)') 'radar_longitude:value = "Radar longitude" ;'
      write(11,'(T17,A)') 'radar_longitude:units = "deg" ;'
      write(11,'(T9,A)') 'float radar_altitude ;'
      write(11,'(T17,A)') 'radar_altitude:value = "Altitude of radar" ;'
      write(11,'(T17,A)') 'radar_altitude:units = "km MSL" ;'
      write(11,'(T9,A)') 'float x_min ;'
      write(11,'(T17,A)') 'x_min:value = "The minimum x grid value" ;'
      write(11,'(T17,A)') 'x_min:units = "km" ;'
      write(11,'(T9,A)') 'float y_min ;'
      write(11,'(T17,A)') 'y_min:value = "The minimum y grid value" ;'
      write(11,'(T17,A)') 'y_min:units = "km" ;'
      write(11,'(T9,A)') 'float x_max ;'
      write(11,'(T17,A)') 'x_max:value = "The maximum x grid value" ;'
      write(11,'(T17,A)') 'x_max:units = "km" ;'
      write(11,'(T9,A)') 'float y_max ;'
      write(11,'(T17,A)') 'y_max:value = "The maximum y grid value" ;'
      write(11,'(T17,A)') 'y_max:units = "km" ;'
      write(11,'(T9,A)') 'float x_spacing ;'
      write(11,'(T17,A)') 'x_spacing:units = "km" ;'
      write(11,'(T9,A)') 'float y_spacing ;'
      write(11,'(T17,A)') 'y_spacing:units = "km" ;'

      write(11,'(T9,A)') 'float x(x) ;'
      write(11,'(T9,A)') 'float y(y) ;'
      write(11,'(T9,A)') 'float el(el) ;'
      write(11,'(T9,A)') 'float u_translation ;'
      write(11,'(T17,A)') 'u_translation:value = "storm motion u component" ;'
      write(11,'(T17,A)') 'u_translation:units = "m/s" ;'
      write(11,'(T9,A)') 'float v_translation ;'
      write(11,'(T17,A)') 'v_translation:value = "storm motion v component" ;'
      write(11,'(T17,A)') 'v_translation:units = "m/s" ;'
      write(11,'(T9,A)') 'char stat_names(nstats, long_string) ;'

      do n=1, nfld
        ls = index(fname(n), ' ') - 1
        ls2 = index(f_units(n), ' ') - 1
        write(11,'(T9,A,A,A)') 'float ', fname(n)(1:ls), '(nstats, el, y, x) ;'
        write(11,'(T17,A,A,A,A)') fname(n)(1:ls), ':units = "', f_units(n)(1:ls2), '" ;'
        write(11,'(T17,A,A,ES14.7,A)') fname(n)(1:ls), ':scale_factor = ', sf(n), ' ;'
        write(11,'(T17,A,A)') fname(n)(1:ls), ':add_offset = 0.0 ;'
        write(11,'(T17,A,A,ES14.7,A,ES14.7,A)') fname(n)(1:ls), ':missing_value = ', sbad, ', ', sbad, ' ;'
      enddo

!     "data" section

      write(11,'(A)') 'data:'
      write(11,*)
      write(11,'(A,I2.2,A,I2.2,A,I4.4,A)') ' start_date = "', mo, '/', da, '/', yr, '" ;'
      write(11,*)
      write(11,'(A,I2.2,A,I2.2,A,I4.4,A)') ' end_date = "', mo, '/', da, '/', yr, '" ;'
      write(11,*)
      write(11,'(A,I2.2,A,I2.2,A,I2.2,A)') ' start_time = "', hr, ':', mn, ':', se, '" ;'
      write(11,*)
      write(11,'(A,I2.2,A,I2.2,A,I2.2,A)') ' end_time = "', hr, ':', mn, ':', se, '" ;'
      write(11,*)
      write(11,'(A,I0,A)') ' bad_data_flag = ', int(sbad), ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' grid_latitude = ', glat, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' grid_longitude = ', glon, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' grid_altitude = ', galt, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' radar_latitude = ', rlat, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' radar_longitude = ', rlon, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' radar_altitude = ', ralt, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' x_min = ', xmin, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' y_min = ', ymin, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' x_max = ', xmin+(nx-1)*dx, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' y_max = ', ymin+(ny-1)*dy, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' x_spacing = ', dx, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' y_spacing = ', dy, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' u_translation = ', ut, ' ;'
      write(11,*)
      write(11,'(A,ES14.7,A)') ' v_translation = ', vt, ' ;'
      write(11,*)

      write(11,'(A)', advance='no') ' x = '
      do i=1, nx
        write(11,'(ES14.7)', advance='no') xmin+(i-1)*dx
        if (i.eq.nx) then
          write(11,'(A)') ' ;'
        else
          write(11,'(A)', advance='no') ', '
          if (mod(i,5).eq.4) write(11,*)
        endif
      enddo
      write(11,*)

      write(11,'(A)', advance='no') ' y = '
      do j=1, ny
        write(11,'(ES14.7)', advance='no') ymin+(j-1)*dy
        if (j.eq.ny) then
          write(11,'(A)') ' ;'
        else
          write(11,'(A)', advance='no') ', '
          if (mod(j,5).eq.4) write(11,*)
        endif
      enddo
      write(11,*)

      write(11,'(A)', advance='no') ' el = '
      do k=1, num_el_angles
        write(11,'(ES14.7)', advance='no') el_angles(k)
        if (k.eq.num_el_angles) then
          write(11,'(A)') ' ;'
        else
          write(11,'(A)', advance='no') ', '
          if (mod(k,5).eq.4) write(11,*)
        endif
      enddo
      write(11,*)

      write(11,'(A)') ' stat_names = '
      do s=1, nstats
        write(11,'(A,A20,A)', advance='no') '  "', stat_names(s), '"'
        if (s.eq.nstats) then
          write(11,'(A)') ' ;'
        else
          write(11,'(A)') ','
        endif
      enddo

!     Write 4D fields.

      do n=1, nfld

        write(11,*)
        ls = index(fname(n), ' ') - 1
        write(11,'(1X,A,A,A)') ' ', fname(n)(1:ls), ' ='
        q = 0

        do s=1, nstats
          do k=1, num_el_angles
            do j=1, ny
              do i=1, nx
                write(11,'(ES14.7)', advance='no') stats(i,j,k,s,n)
                q = q + 1
                if (q.eq.(nx*ny*num_el_angles*nstats)) then
                  write(11,'(A)') ' ;'
                else
                  write(11,'(A)', advance='no') ', '
                  if (mod(q,5).eq.4) write(11,*)
                endif
              enddo
            enddo
          enddo
        enddo

      enddo

!     Write final character and then close file.

      write(11,'(A)') '}'
      close(11)

!     Convert ascii file to netcdf.

      ls = index(ncfile, ' ') - 1
      write(command,'(A,A,A)') ncgen_command//' -o ', ncfile(1:ls), ' ncgen.input'

      write(6,FMT='(1x,A,A)') 'Now executing command = ', command

      call system(command)

      return
      end


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######           SUBROUTINE READ_NETCDF_PPI_STATS           ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads in a specified statistics field from a netcdf file
!     containing gridded PPI statistics.  If the grid parameters in the netcdf
!     file do not match the specified input parameters, then the subroutine fails.
!
!     Author:  David Dowell
!
!     Creation Date:  March 2010
!
!############################################################################

      subroutine read_netcdf_ppi_stats(ncfile, fname, stat_name,                    &
                                       glon, glat, galt,                            &
                                       nx, ny, dx, dy, xmin, ymin, max_el_angles,   &
                                       num_el_angles, el_angles, stat)

      implicit none

      include 'opaws.inc'
      include 'netcdf.inc'

!---- input parameters

      character(len=80) ncfile         ! input netcdf file name
      character(len=8) fname            ! field name
      character(len=*) stat_name        ! name of statistical field
      real glon, glat                   ! latitude and longitude of grid origin (deg)
      real galt                         ! altitude of grid origin (km MSL)
      integer nx, ny                    ! no. of grid points in x and y directions
      real dx, dy                       ! grid spacing in x and y directions (km)
      real xmin, ymin                   ! horizontal coordinates of lower southwest corner
                                        !   of grid, relative to origin (km)
      integer max_el_angles             ! maximum number of elevation angles

!---- returned variables

      integer num_el_angles             ! number of different elevation angles for which statistics were computed
      real el_angles(max_el_angles)     ! elevation angles (deg) for which statistics were computed
      real stat(nx,ny,max_el_angles)    ! statistics as a function of space and elevation angle

!---- local variables

      integer ncid, status, id
      real iglon, iglat                 ! latitude and longitude input from ncfile (deg)
      real igalt                        ! altitude input from ncfile (km MSL)
      integer inx, iny                  ! no. of grid points in x and y directions, input from ncfile
      real idx, idy                     ! grid spacing in x and y directions (km), input from ncfile
      real ixmin, iymin                 ! horizontal coordinates of lower southwest corner
                                        !   of grid, relative to origin (km), input from ncfile
      integer nstats                                     ! number of different statistic types
      character(len=20), allocatable :: stat_names(:)    ! names of statistic fields
      real, allocatable :: stats(:,:,:,:)                ! statistics as a function of space, elevation angle, and statistic type
      integer stat_index                                 ! index of specified statistic type
      integer i, j, k, n


!     Read grid dimensions and check for agreement with expected values.
      call get_dims_netcdf(ncfile, inx, iny, num_el_angles)
      if (inx .ne. nx) then
        write(*,*) 'inx and nx are not the same:  ', inx, nx
        stop
      endif
      if (iny .ne. ny) then
        write(*,*) 'iny and ny are not the same:  ', iny, ny
        stop
      endif
      if (num_el_angles .gt. max_el_angles) then
        write(*,*) 'num_el_angles too large:  ', num_el_angles, max_el_angles
        stop
      endif

!     Open netcdf file.

      status = NF_OPEN(ncfile, NF_NOWRITE, ncid)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem opening file: ', NF_STRERROR(status), ncid
        stop
      endif

!     Read scalar variables.

! DCD 1/26/11 changed bad to sbad
      el_angles(:) = sbad
      status = NF_INQ_VARID(ncid, 'el', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for el: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, el_angles(1:num_el_angles))
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining el: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'grid_latitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for iglat: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, iglat)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining iglat: ', NF_STRERROR(status)
        stop
      endif
      if (iglat .ne. glat) then
        write(*,*) 'iglat and glat are not the same:  ', iglat, glat
        stop
      endif

      status = NF_INQ_VARID(ncid, 'grid_longitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for iglon: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, iglon)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining iglon: ', NF_STRERROR(status)
        stop
      endif
      if (iglon .ne. glon) then
        write(*,*) 'iglon and glon are not the same:  ', iglon, glon
        stop
      endif

      status = NF_INQ_VARID(ncid, 'grid_altitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for igalt: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, igalt)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining igalt: ', NF_STRERROR(status)
        stop
      endif
      if (igalt .ne. galt) then
        write(*,*) 'igalt and galt are not the same:  ', igalt, galt
        stop
      endif

      status = NF_INQ_VARID(ncid, 'x_spacing', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for idx: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, idx)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining idx: ', NF_STRERROR(status)
        stop
      endif
      if (idx .ne. dx) then
        write(*,*) 'idx and dx are not the same:  ', idx, dx
        stop
      endif

      status = NF_INQ_VARID(ncid, 'y_spacing', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for idy: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, idy)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining idy: ', NF_STRERROR(status)
        stop
      endif
      if (idy .ne. dy) then
        write(*,*) 'idy and dy are not the same:  ', idy, dy
        stop
      endif

      status = NF_INQ_VARID(ncid, 'x_min', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for ixmin: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, ixmin)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining ixmin: ', NF_STRERROR(status)
        stop
      endif
      if (ixmin .ne. xmin) then
        write(*,*) 'ixmin and xmin are not the same:  ', ixmin, xmin
        stop
      endif

      status = NF_INQ_VARID(ncid, 'y_min', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for iymin: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, iymin)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining iymin: ', NF_STRERROR(status)
        stop
      endif
      if (iymin .ne. ymin) then
        write(*,*) 'iymin and ymin are not the same:  ', iymin, ymin
        stop
      endif

!     Read statistic names.

      status = NF_INQ_DIMID(ncid, 'nstats', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for nstats: ', NF_STRERROR(status)
        stop
      endif
      status = NF_INQ_DIMLEN(ncid, id, nstats)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining nstats: ', NF_STRERROR(status)
        stop
      endif

      allocate(stat_names(nstats))

      status = NF_INQ_VARID(ncid, 'stat_names', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for stat_names: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_TEXT(ncid, id, stat_names)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining stat_names: ', NF_STRERROR(status)
        stop
      endif

      stat_index = 0
      do n=1, nstats
        if (index(stat_names(n), stat_name) .ne. 0) stat_index = n
      enddo
      if (stat_index .eq. 0) then
        write(*,*) 'Could not find matching stat_name:  ', stat_name
        stop
      endif

      deallocate(stat_names)

!     Read entire statistics array and return specified portion.

      allocate(stats(nx,ny,num_el_angles,nstats))

      status = NF_INQ_VARID(ncid, fname, id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for ', fname, ': ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, stats)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining ', fname, ': ', NF_STRERROR(status)
        stop
      endif

! DCD 1/26/11:  making "bad" obsolete, to be consistent with NSSL_Oban2D
!      do n=1, nstats
!        do k=1, num_el_angles
!          do j=1, ny
!            do i=1, nx
!              if (stats(i,j,k,n).eq.sbad) stats(i,j,k,n)=bad
!            enddo
!          enddo
!        enddo
!      enddo

! DCD 1/26/11:  changed "bad" to "sbad"
      stat(:,:,:) = sbad
      do k=1, num_el_angles
        stat(:,:,k) = stats(:,:,k,stat_index)
      end do

      deallocate(stats)

!     Close  netcdf file

      status=NF_CLOSE(ncid)
      if (status.ne.NF_NOERR) then
        write(*,*) 'Error closing file: ', NF_STRERROR(status)
      endif


      return
      end

!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######               SUBROUTINE GET_DIMS_NETCDF             ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads gridded fields from the netcdf file.
!
!     Author:  David Dowell
!
!     Creation Date:  March 2010
!
!############################################################################

      subroutine get_dims_netcdf(ncfile, nx, ny, nz)

      implicit none

      include 'netcdf.inc'

!---- input parameters

      character(len=80) ncfile    ! netcdf file name

!---- returned variables

      integer nx, ny, nz           ! no. of grid points in x, y, and z directions

!---- local variables

      integer ncid, status, id

!     Open netcdf file.

      status = NF_OPEN(ncfile, NF_NOWRITE, ncid)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem opening file: ', NF_STRERROR(status), ncid
        stop
      endif

!     Read dimensions.

      status = NF_INQ_DIMID(ncid, 'x', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for nx: ', NF_STRERROR(status)
        stop
      endif
      status = NF_INQ_DIMLEN(ncid, id, nx)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining nx: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_DIMID(ncid, 'y', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for ny: ', NF_STRERROR(status)
        stop
      endif
      status = NF_INQ_DIMLEN(ncid, id, ny)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining ny: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_DIMID(ncid, 'z', id)
      if (status .ne. NF_NOERR) then
        status = NF_INQ_DIMID(ncid, 'el', id)
        if (status .ne. NF_NOERR) then
          write(*,*) 'Problem obtaining id for z and el: ', NF_STRERROR(status)
          stop
        endif
        status = NF_INQ_DIMLEN(ncid, id, nz)
        if (status .ne. NF_NOERR) then
          write(*,*) 'Problem obtaining el: ', NF_STRERROR(status)
          stop
        endif

      else
        status = NF_INQ_DIMLEN(ncid, id, nz)
        if (status .ne. NF_NOERR) then
          write(*,*) 'Problem obtaining z: ', NF_STRERROR(status)
          stop
        endif

      endif

!     Close  netcdf file

      status=NF_CLOSE(ncid)
      if (status.ne.NF_NOERR) then
        write(*,*) 'Error closing file: ', NF_STRERROR(status)
      endif


      return
      end


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE WRITEV5D                   ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine writes out a VIS5D format file containing the
!     synthesized dual-Doppler fields.
!
!     Author:  David Dowell
!
!     Creation Date:  August 2005
!
!     Modifications:  Feb 2010, L Wicker, accomodate new data structure
!
!############################################################################

  SUBROUTINE WRITEV5D(prefix, anal, cyr, cmo, cda, chr, cmn, cse)

    USE DTYPES_module

    implicit none

    include 'v5df.h'
    include 'opaws.inc'

! Input parameters

    character(len=*) prefix      ! output file

! Analysis grid and radar data are stored in these two derived types

    TYPE(ANALYSISGRID) :: anal
 
    integer(kind=2) cyr,cmo,cda  ! central date
    integer(kind=2) chr,cmn,cse  ! central time

! Local variables

    integer i, j, k, s           ! grid indices
    integer nx, ny, nz           ! no. of grid points in x, y, and z directions
    integer nfld                 ! number of fields
    integer npass
    integer ls

! Vis5d variables

    integer n
    integer it, iv
    real(kind=4), allocatable :: G(:,:,:)

    integer nr, nc, nl
    integer numtimes
    integer numvars
    character(len=10) varname(MAXVARS)
    integer dates(MAXTIMES)
    integer times(MAXTIMES)
    real northlat
    real latinc
    real westlon
    real loninc
    real bottomhgt
    real hgtinc
    character(len=256) file_string

    integer ndays(12)

! Vis5d variables initialized to missing values

    data nr,nc,nl / IMISSING, IMISSING, IMISSING /
    data numtimes,numvars / IMISSING, IMISSING /
    data (varname(i),i=1,MAXVARS) / MAXVARS*"          " /
    data (dates(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
    data (times(i),i=1,MAXTIMES) / MAXTIMES*IMISSING /
    data northlat, latinc / MISSING, MISSING /
    data westlon, loninc / MISSING, MISSING /
    data bottomhgt, hgtinc / MISSING, MISSING /
    data ndays /31,29,31,30,31,30,31,31,30,31,30,31/

!############################################################################
!
!   Get dimension sizes

    nx   = size(anal%xg)
    ny   = size(anal%yg)
    nz   = size(anal%zg)
    nfld = size(anal%f,dim=4)
    npass = size(anal%f,dim=5)

    IF (nfld .le. 0) THEN
      write(6,*) 'WriteV5D:  Error:  Number of fields <= ZERO!'
      stop
    ENDIF

! Initialize variables

    nr = ny
    nc = nx
    nl = nz
    numtimes = 1
    numvars = nfld

    allocate(G(ny,nx,nz))

    DO s = 1,nfld
      varname(s) = anal%name(s)
    ENDDO

    dates(1) = 0
    times(1) = 0
    northlat = anal%ymin + (ny-1)*anal%dy
    northlat = anal%glat + (ny-1)*anal%dy*(1./111.) + anal%ymin*(1./111.)
    latinc   = anal%dy
    latinc   = anal%dy*(1./111.)
    westlon  = -anal%xmin
    westlon  = -anal%glon - anal%xmin*(180./3.14159)/(6371.*cos(anal%glat*3.14159/180.))
    loninc   = anal%dx
    loninc   = anal%dx*(180./3.14159)/(6371.*cos(anal%glat*3.14159/180.))

    DO WHILE ( (northlat.gt.90.0)                 .or. &
              ((northlat-(ny-1)*latinc).lt.-90.0) .or. &
                (westlon.lt.-90.0)                .or. &
               ((westlon+(nx-1)*loninc).gt.90.0) )
      northlat = 0.1*northlat
      latinc   = 0.1*latinc
      westlon  = 0.1*westlon
      loninc   = 0.1*loninc
    ENDDO

! DCD 11/24/10
!    bottomhgt = 0.0
    bottomhgt = anal%zmin
    hgtinc    = anal%dz

! Compute Julian day for vis5d.

    dates(1) = 2004000
    IF (cmo.gt.1) THEN
      DO i = 1, cmo-1
        dates(1) = dates(1) + ndays(i)
      ENDDO
    ENDIF
    dates(1) = dates(1) + cda
    times(1) = chr*10000 + cmn*100 + cse

! Create the v5d file.

    ls = index(prefix, ' ') - 1

    write(6,*) 'Creating Vis5D file:  ', prefix(1:ls)//'.v5d'

    file_string = prefix(1:ls)//".v5d "

    n = V5DCREATESIMPLE(file_string, numtimes, numvars, nr, nc, nl,  &
                        varname, times, dates, northlat, latinc, westlon, loninc, bottomhgt, hgtinc )

    IF (n .eq. 0) THEN
      write(6,*) 'WriteV5D:  !!! Error creating v5d file !!!'
      stop
    ENDIF

    DO it = 1,numtimes
      DO iv = 1,numvars

        DO k = 1,nl
          DO j = 1,nr
            DO i = 1,nc

               G(nr-j+1,i,k) = anal%f(i,j,k,iv,npass)

               IF (G(nr-j+1,i,k) .eq. sbad) G(nr-j+1,i,k) = 9.9E30

            ENDDO
          ENDDO
        ENDDO

! Write the 3-D grid to the v5d file

       n = V5DWRITE( it, iv, G )

       IF (n .eq. 0) THEN
         write(6,*) 'WriteV5D:  !!! Error writing to v5d file !!!'
         stop
       ENDIF

       ENDDO
    ENDDO

! Close the v5d file and exit

    n = V5DCLOSE()

    IF (n .eq. 0) THEN
      write(6,*) 'WriteV5D:  !!! Error closing v5d file !!!'
      stop
    ENDIF

    deallocate(G)


  RETURN
  END


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######              SUBROUTINE WRITE_BEAM_INFO              ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine writes out a netcdf file containing the beam information:
!     time offset from central time, azimuth angle, and elevation angle.
!
!     Author:  David Dowell
!
!     Creation Date:  March 2007
!
!############################################################################

      SUBROUTINE WRITE_BEAM_INFO(ncfile, nswp, num_beams, beam_info)

      USE netcdf
      implicit none
      include 'opaws.inc'

!---- input parameters

      character(len=*) ncfile               ! netcdf file name
      integer nswp                          ! number of sweeps
      integer num_beams(nswp)               ! number of beams in each sweep
      real beam_info(maxrays,nswp,3)        ! beam information:
                                            !   (1) time offset (s) from central time
                                            !   (2) azimuth angle (deg)
                                            !   (3) elevation angle (deg)
                                            
!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE

!---- local variables
      integer i, j
      
! netCDF id
      integer  ncid
! dimension ids
      integer  nswp_dim
      integer  maxrays_dim
      
! variable ids
      integer  num_beams_id
      integer  time_id
      integer  az_id
      integer  el_id
      
! rank (number of dimensions) for each variable
      integer, parameter :: num_beams_rank = 1
      integer, parameter :: time_rank      = 2
      integer, parameter :: az_rank        = 2
      integer, parameter :: el_rank        = 2
      
! variable shapes
      integer  num_beams_dims(num_beams_rank)
      integer  time_dims(time_rank)
      integer  az_dims(az_rank)
      integer  el_dims(el_rank)
      
! variables variables
      real  time(maxrays, nswp)
      real  az(maxrays, nswp)
      real  el(maxrays, nswp)
            
! enter define mode
      call check( nf90_create('beam_info.nc', NF90_CLOBBER, ncid), &                               
                               message='Error opening file')

! define dimensions
      call check( nf90_def_dim(ncid, 'nswp', nswp, nswp_dim), &
                               message='Error defining nswp dimension')
      
      call check( nf90_def_dim(ncid, 'maxrays', maxrays, maxrays_dim ), &
                               message='Error defining nswp dimension')
 
! define variables
      call check( nf90_def_var(ncid, 'num_beams', NF90_INT, num_beams_id), &
                               message='Error defining num_beams variable')

      time_dims(2) = nswp_dim
      time_dims(1) = maxrays_dim
      call check( nf90_def_var(ncid, 'time', NF90_REAL, time_id), &
                               message='Error defining num_beams variable')

      az_dims(2) = nswp_dim
      az_dims(1) = maxrays_dim
      call check( nf90_def_var(ncid, 'az', NF90_REAL, az_id), &
                               message='Error defining azimuth variable')

      el_dims(2) = nswp_dim
      el_dims(1) = maxrays_dim
      call check( nf90_def_var(ncid, 'el', NF90_REAL,  el_id), &
                               message='Error defining elevation variable')
     
! assign attributes
      call check( nf90_put_att(ncid, num_beams_id, 'description', 'number of beams in sweep'), &
                               message='Error writing num_beams description attribute')
      
      call check( nf90_put_att(ncid, time_id, 'description', 'time offset from base time'), &
                               message='Error writing time description attribute')
      
      call check( nf90_put_att(ncid, time_id, 'units', 's'), &
                               message='Error writing time units attribute')
      
      call check( nf90_put_att(ncid, az_id, 'description', 'azimuth angle'), &
                                message='Error writing azimuthal description attribute')
      
      call check( nf90_put_att(ncid, az_id, 'units','deg'), &
                               message='Error writing azimuthal units attribute')
      
      
      call check( nf90_put_att(ncid, el_id, 'description', 'elevation angle'), &
                               message='Error writing elevation description attribute')
      
      call check( nf90_put_att(ncid, el_id, 'units', 'deg'), &
                               message='Error writing elevation units attribute')
      
! leave define mode
      call check( nf90_enddef(ncid), message='Error leaving define mode')
            
! store num_beams
      call check( nf90_put_var(ncid, num_beams_id, num_beams), message='Error writing num_beams variable')
      
! store time
      DO i = 1,nswp
        DO j = 1,maxrays
            time(j,i) = beam_info(j,i,1)
            az(j,i)   = beam_info(j,1,2)
            el(j,i)   = beam_info(j,i,3)
        ENDDO
      ENDDO
      
      call check( nf90_put_var(ncid, time_id, time), message='Error writing time variable')
      call check( nf90_put_var(ncid, az_id, az) , message='Error writing azimuthal variable')
      call check( nf90_put_var(ncid, el_id, el), message='Error writing elevation variable')
      
      call check( NF90_CLOSE(ncid), message='Error closing beaminfo file: ')
      
    RETURN
    END SUBROUTINE WRITE_BEAM_INFO
      
    SUBROUTINE CHECK(status, message)
    
      USE NETCDF
      integer, intent (in) :: status
      character(len=*), optional :: message
    
      IF( status /= nf90_noerr ) THEN 
        IF( present(message) ) THEN
          write(6,*), message//"  "//trim(nf90_strerror(status))
          stop "Stopped by Subroutine CHECK"
        ELSE
          write(6,*) trim(nf90_strerror(status))
          stop "Stopped by Subroutine CHECK"
        ENDIF
      END IF
      
    RETURN
    END SUBROUTINE CHECK  


