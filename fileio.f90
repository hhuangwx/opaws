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

    SUBROUTINE WRITENETCDF(prefix, analysis_type, anal, ut, vt, yr, mo, da, hr, mn, se)

      USE DTYPES_module
      USE netcdf
      USE OBAN_FIELDS

      implicit none

!---- input parameters

      character(len=*) prefix       ! prefix file name
      integer analysis_type         ! Type of analysis
                                    !   1 == 3D Cartesian
                                    !   2 == 2D sweep by sweep
      TYPE(ANALYSISGRID) anal
      real ut, vt                     ! storm translation velocity (m/s)
      integer yr, mo, da              ! year, month, and day
      integer hr, mn, se              ! hour, minute, and second

!---- local variables

      integer nx, ny, nz              ! no. of grid points in x, y, and z directions
      integer npass                   ! number of analysis passes on data
      integer nfld                    ! number of analysis passes on data
      real, allocatable :: sf(:)      ! scaling factors
      integer n
!      integer i, j, k, s, p
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

      integer analysis_type_id
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

!      integer bad_data_flag
!      real  grid_latitude
!      real  grid_longitude
!      real  grid_altitude
!      real  radar_latitude
!      real  radar_longitude
!      real  radar_altitude
!      real  x_min
!      real  y_min
!      real  x_max
!      real  y_max
!      real  x_spacing
!      real  y_spacing
!      real  x(size(anal%xg))
!      real  y(size(anal%yg))
!      real  z(size(anal%zg))
!      real  el(size(anal%zg))
!      real  z_spacing
!      real  u_translation
!      real  v_translation

!---- attribute vectors

!      integer textval(1)
!      double precision doubleval(1)

!---- strings

      character (len=10) start_date
      character (len=10) end_date
      character (len=8) start_time
      character (len=8) end_time
      character (len=8), allocatable :: fieldname(:)

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
      allocate(fieldname(nfld+4))
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

      call check(nf90_def_var(ncid, 'analysis_type', nf90_int, analysis_type_id));

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

        if (ls .eq. -1) ls = 8       ! DCD 8/18/11 handle special case of field name that is 8 characters

        call check(nf90_def_var(ncid, anal%name(n)(1:ls), nf90_float, field_dims, field_ids(n)))
        call check(nf90_put_att(ncid, field_ids(n), 'scale_factor', sf(n)))
        call check(nf90_put_att(ncid, field_ids(n), 'add_offset', 0.0))
        call check(nf90_put_att(ncid, field_ids(n), 'missing_value', sbad))
      ENDDO

!---- assign variable attributes

      call check(nf90_put_att(ncid, analysis_type_id, 'value', 'Analysis type (1=3D, 2=2D)'))

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

      call check( nf90_put_var(ncid, analysis_type_id, analysis_type) )
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
    deallocate(fieldname)

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
  SUBROUTINE DART_radar_out(prefix, anal, sweep_info,                      &
                            nfld, min_threshold, fill_flag, fill_value,    &
                            append_eval, use_clear_air_type, clear_air_skip)

    USE DTYPES_module
    USE DART_module

    implicit none

    include 'opaws.inc'

! Passed variables:  possibly changed by subroutine (if use_clear_air_type is .true.)
    TYPE(ANALYSISGRID) :: anal

! Passed variables:  not changed by subroutine
    character(len=*) prefix   ! prefix of the output file name
    TYPE(SWEEP_INFORMATION) :: sweep_info      ! info. about individual sweeps, such as Nyquist velocity (m/s) for each field
    integer nfld                 ! number of fields
    real min_threshold(nfld)     ! value below which input observations are set to missing
    integer fill_flag(nfld)      ! fill missing values with a specified value? (0=no, 1=yes)
    real fill_value(nfld)        ! replacement value, if fill_flag==1
    logical append_eval          ! .true. if "_EVAL" should be appended to the names of DART observation types
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
    real tlat                                        ! First true latitude (rad N) of Lambert conformal projection
    real blat                                        ! Second true latitude (rad N) of Lambert conformal projection
    real cenlon                                      ! Reference longitude (rad E) of Lambert conformal projection
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

    if (anal%map_proj .eq. 2) then
      tlat   = dtor * anal%tlat1
      blat   = dtor * anal%tlat2
      cenlon = dtor * anal%clon
    endif

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

    call write_DART_header(fi, append_eval, qc_string)

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

            CALL xy_to_ll(lat, lon, anal%map_proj, tlat, blat, cenlon, x, y, glat, glon)

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
!     ######        SUBROUTINE WRITE_NETCDF_CLUTTER_STATS         ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine writes out a netcdf file containing ground-clutter
!     statistics in radar coordinates.
!
!     Author:  David Dowell
!
!     Creation Date:  July 2011
!
!############################################################################

      subroutine WRITE_NETCDF_CLUTTER_STATS(ncfile, nr, r_coord, na, a_coord, ne, e_coord,    &
                                            nstats, stat_names, stats)

      USE netcdf

      implicit none

      include 'opaws.inc'

!---- input parameters

      character(len=100) ncfile        ! netcdf file name
      integer nr                       ! number of range bins
      real r_coord(nr+1)               ! range coordinates of bin edges (km)
      integer na                       ! number of azimuth-angle bins
      real a_coord(na+1)               ! azimuth angle coordinates of bin edges (deg)
      integer ne                       ! number of elevation-angle bins
      real e_coord(ne+1)               ! elevation angle coordinates of bin edges (deg)
      integer nstats                   ! number of different statistic types
      character(len=20) stat_names(nstats)          ! names of statistic fields
      real stats(nr,na,ne,nstats)      ! statistics as a function of space, elevation angle, statistic type, and field

!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE
      
!---- local variables

      integer ncid                    ! netCDF file ID
      integer n                       ! statistic index

!---- dimension IDs

      integer rc_dim      ! id's of bin center dimensions
      integer ac_dim
      integer ec_dim
      integer re_dim      ! id's of bin edge dimensions
      integer ae_dim
      integer ee_dim

!---- variable IDs

      integer r_id
      integer a_id
      integer e_id
      integer field_id(nstats)

!---- variable shapes

      integer threed_dims(3)



      write(6,*)
      write(6,FMT='(" WRITE_NETCDF_CLUTTER_STATS -> INPUT DIMS NR:    ", i3)') nr
      write(6,FMT='(" WRITE_NETCDF_CLUTTER_STATS -> INPUT DIMS NA:    ", i3)') na
      write(6,FMT='(" WRITE_NETCDF_CLUTTER_STATS -> INPUT DIMS NE:    ", i3)') ne

!---- enter define mode

      call check(nf90_create(ncfile, nf90_clobber, ncid));

!---- define dimensions
      
      call check(nf90_def_dim(ncid, 'rc', nr,   rc_dim));
      call check(nf90_def_dim(ncid, 're', nr+1, re_dim));
      call check(nf90_def_dim(ncid, 'ac', na,   ac_dim));
      call check(nf90_def_dim(ncid, 'ae', na+1, ae_dim));
      call check(nf90_def_dim(ncid, 'ec', ne,   ec_dim));
      call check(nf90_def_dim(ncid, 'ee', ne+1, ee_dim));

!---- define variables and attributes

      call check(nf90_def_var(ncid, 'range', nf90_float, re_dim, r_id));
      call check(nf90_put_att(ncid, r_id, 'units', 'km'))
      call check(nf90_put_att(ncid, r_id, 'description', 'range to bin edge'))

      call check(nf90_def_var(ncid, 'az', nf90_float, ae_dim, a_id));
      call check(nf90_put_att(ncid, a_id, 'units', 'deg'))
      call check(nf90_put_att(ncid, a_id, 'description', 'azimuth angle of bin edge'))

      call check(nf90_def_var(ncid, 'el', nf90_float, ee_dim, e_id));
      call check(nf90_put_att(ncid, e_id, 'units', 'deg'))
      call check(nf90_put_att(ncid, e_id, 'description', 'elevation angle of bin edge'))

      threed_dims(1) = rc_dim
      threed_dims(2) = ac_dim
      threed_dims(3) = ec_dim

      do n=1, nstats
        call check(nf90_def_var(ncid, stat_names(n), nf90_float, threed_dims, field_id(n)));
        call check(nf90_put_att(ncid, field_id(n), 'missing_value', sbad))
      enddo

!---- leave define mode

      call check(nf90_enddef(ncid));

!---- write variables                                                                                                                                                                                                     

      call check( nf90_put_var(ncid, r_id, r_coord) )
      call check( nf90_put_var(ncid, a_id, a_coord) )
      call check( nf90_put_var(ncid, e_id, e_coord) )
      
      DO n=1, nstats
        call check( nf90_put_var(ncid, field_id(n), stats(:,:,:,n)) )
      ENDDO

      call check( nf90_close(ncid) )

      RETURN

      END SUBROUTINE WRITE_NETCDF_CLUTTER_STATS


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######      SUBROUTINE READ_NETCDF_CLUTTER_STAT_DIMS        ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads variable dimensions from a netcdf file produced
!     by the clutter_stats program.
!
!     Author:  David Dowell
!
!     Creation Date:  July 2011
!
!############################################################################

      subroutine READ_NETCDF_CLUTTER_STAT_DIMS(ncfile, nr, na, ne)

      USE netcdf

      implicit none

      include 'opaws.inc'

!---- input parameters

      character(len=100) ncfile                ! input netcdf file name

!---- returned variables
      integer nr                               ! number of range bins
      integer na                               ! number of azimuth-angle bins
      integer ne                               ! number of elevation-angle bins

!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE
      
!---- local variables

      integer ncid                    ! netCDF file ID
      integer id                      ! dimension / variable id


      write(6,*)
      write(6,*) "READ_NETCDF_CLUTTER_STAT_DIMS -> INPUT FILE:"
      write(6,*) ncfile

      call check(nf90_open(ncfile, nf90_nowrite, ncid));

      call check(nf90_inq_dimid(ncid, 'rc', id));
      call check(nf90_inquire_dimension(ncid, id, len = nr));

      call check(nf90_inq_dimid(ncid, 'ac', id));
      call check(nf90_inquire_dimension(ncid, id, len = na));

      call check(nf90_inq_dimid(ncid, 'ec', id));
      call check(nf90_inquire_dimension(ncid, id, len = ne));

      call check( nf90_close(ncid) )

      RETURN

      END SUBROUTINE READ_NETCDF_CLUTTER_STAT_DIMS


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######        SUBROUTINE READ_NETCDF_CLUTTER_STATS          ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads a netcdf file produced by the clutter_stats program.
!
!     Author:  David Dowell
!
!     Creation Date:  July 2011
!
!############################################################################

      subroutine READ_NETCDF_CLUTTER_STATS(ncfile, nr, na, ne, r, a, e,               &
                                           freq_fixed_clutter, freq_moving_clutter,   &
                                           total_gates, total_sweeps, mean_refl)

      USE netcdf

      implicit none

      include 'opaws.inc'

!---- input parameters

      character(len=100) ncfile                       ! input netcdf file name
      integer nr                                      ! number of range bins
      integer na                                      ! number of azimuth-angle bins
      integer ne                                      ! number of elevation-angle bins

!---- returned variables
      real r(nr+1)                         ! range coordinates of bin edges (km)
      real a(na+1)                         ! azimuth angle coordinates of bin edges (deg)
      real e(ne+1)                         ! elevation angle coordinates of bin edges (deg)
      real freq_fixed_clutter(nr,na,ne)    ! frequency of gates in (range, azimuth, elevation) bin that were detected as fixed clutter
      real freq_moving_clutter(nr,na,ne)   ! frequency of gates in (range, azimuth, elevation) bin that were detected as moving clutter
      integer total_gates(nr,na,ne)        ! number of gates used for computing statistics in (range, azimuth, elevation) bin
      integer total_sweeps(nr,na,ne)       ! number of sweeps used for computing statistics in (range, azimuth, elevation) bin
      real mean_refl(nr,na,ne)             ! mean reflectivity (dBZ) for (range, azimuth, elevation) bin

!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE
      
!---- local variables

      integer ncid                    ! netCDF file ID
      integer id                      ! dimension / variable id
      real, allocatable :: temp(:,:,:)



      write(6,*)
      write(6,*) "READ_NETCDF_CLUTTER_STATS -> INPUT FILE:"
      write(6,*) ncfile

      call check(nf90_open(ncfile, nf90_nowrite, ncid))

      allocate(temp(nr, na, ne))

      call check(nf90_inq_varid(ncid, 'range', id))
      call check(nf90_get_var(ncid, id, r))

      call check(nf90_inq_varid(ncid, 'az', id))
      call check(nf90_get_var(ncid, id, a))

      call check(nf90_inq_varid(ncid, 'el', id))
      call check(nf90_get_var(ncid, id, e))

      call check(nf90_inq_varid(ncid, 'freq_fixed_clutter', id))
      call check(nf90_get_var(ncid, id, freq_fixed_clutter))

      call check(nf90_inq_varid(ncid, 'freq_moving_clutter', id))
      call check(nf90_get_var(ncid, id, freq_moving_clutter))

      call check(nf90_inq_varid(ncid, 'total_gates', id))
      call check(nf90_get_var(ncid, id, temp))
      total_gates(:,:,:) = nint(temp(:,:,:))

      call check(nf90_inq_varid(ncid, 'total_sweeps', id))
      call check(nf90_get_var(ncid, id, temp))
      total_sweeps(:,:,:) = nint(temp(:,:,:))

      call check(nf90_inq_varid(ncid, 'mean_reflectivity', id))
      call check(nf90_get_var(ncid, id, mean_refl))

      call check( nf90_close(ncid) )

      deallocate(temp)

      RETURN

      END SUBROUTINE READ_NETCDF_CLUTTER_STATS


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
!     June 2013, D Dowell, output multiple times for sweep-by-sweep analyses
!
!############################################################################

  SUBROUTINE WRITEV5D(prefix, anal, numtimes, year, month, day, hour, minute, second)

    USE DTYPES_module

    implicit none

    include 'v5df.h'
    include 'opaws.inc'

! Input parameters and data

    character(len=*) prefix      ! output file
    TYPE(ANALYSISGRID) :: anal   ! analysis grid and radar data in derived type
    integer numtimes
    integer(kind=2) :: year(numtimes)
    integer(kind=2) :: month(numtimes)
    integer(kind=2) :: day(numtimes)
    integer(kind=2) :: hour(numtimes)
    integer(kind=2) :: minute(numtimes)
    integer(kind=2) :: second(numtimes)

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
    data numvars / IMISSING /
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
    if (numtimes.gt.1) then
      nl = 1                       ! sweep-by-sweep data
    else
      nl = nz
    endif
    numvars = nfld

    allocate(G(ny,nx,nz))

    DO s = 1,nfld
      varname(s) = anal%name(s)
    ENDDO

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

! Compute YYDDD for vis5d.

    DO it = 1,numtimes
      dates(it) = mod(year(it),100) * 1000
      IF (month(it).gt.1) THEN
        DO i = 1, month(it)-1
          dates(it) = dates(it) + ndays(i)
        ENDDO
      ENDIF
      dates(it) = dates(it) + day(it)
      times(it) = hour(it)*10000 + minute(it)*100 + second(it)
    ENDDO

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

               if (numtimes.gt.1) then
                 G(nr-j+1,i,k) = anal%f(i,j,it,iv,npass)    ! sweep-by-sweep data
               else
                 G(nr-j+1,i,k) = anal%f(i,j,k,iv,npass)
               endif

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
      integer  nswp_dim_id
      integer  maxrays_dim_id
      
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
      call check( nf90_def_dim(ncid, 'nswp', nswp, nswp_dim_id), &
                               message='Error defining nswp dimension')
      
      call check( nf90_def_dim(ncid, 'maxrays', maxrays, maxrays_dim_id), &
                               message='Error defining maxrays dimension')

! define variables
      num_beams_dims(1) = nswp_dim_id
      call check( nf90_def_var(ncid, 'num_beams', NF90_INT, num_beams_dims, num_beams_id), &
                               message='Error defining num_beams variable')

      time_dims(2) = nswp_dim_id
      time_dims(1) = maxrays_dim_id
      call check( nf90_def_var(ncid, 'time', NF90_REAL, time_dims, time_id), &
                               message='Error defining num_beams variable')

      az_dims(2) = nswp_dim_id
      az_dims(1) = maxrays_dim_id
      call check( nf90_def_var(ncid, 'az', NF90_REAL, az_dims, az_id), &
                               message='Error defining azimuth variable')

      el_dims(2) = nswp_dim_id
      el_dims(1) = maxrays_dim_id
      call check( nf90_def_var(ncid, 'el', NF90_REAL, el_dims, el_id), &
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
            az(j,i)   = beam_info(j,i,2)
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
          write(6,*) message//"  "//trim(nf90_strerror(status))
          stop "Stopped by Subroutine CHECK"
        ELSE
          write(6,*) trim(nf90_strerror(status))
          stop "Stopped by Subroutine CHECK"
        ENDIF
      END IF
      
    RETURN
    END SUBROUTINE CHECK  


!############################################################################
!
!     ##################################################################
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE READNETCDF                 ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads gridded fields from a netcdf file (in the format
!     produced by oban or dualdop).
!
!############################################################################

      subroutine readnetcdf(ncfile, nx, ny, nz, nfld, fname, f,   &
                             glon, glat, galt,                    &
                             cyr, cmo, cda, chr, cmn, cse,        &
                             dx, dy, dz,                          &
                             xmin, ymin, zmin, ut, vt)

      implicit none

      include 'opaws.inc'
      include 'netcdf.inc'

!---- input parameters

      character(len=100) ncfile    ! netcdf file name
      integer nx, ny, nz           ! no. of grid points in x, y, and z directions
      integer nfld                 ! number of data fields needed
      character(len=8) fname(nfld) ! field names

!---- returned variables

      real f(nx,ny,nz,nfld)        ! data fields
      real glat, glon              ! latitude and longitude of grid origin (deg)
      real galt                    ! altitude of grid origin (km MSL)
      integer cyr,cmo,cda          ! central date
      integer chr,cmn,cse          ! central time
      real dx, dy, dz              ! grid spacing in x, y, and z directions (km)
      real xmin, ymin, zmin        ! coordinates of lower southwest corner
                                   !   of grid, relative to origin (km)
      real ut, vt                  ! translation velocity (m/s)

!---- local variables

      integer ncid, status, id
      character(len=10) date
      character(len=8) time
      integer n
      integer npass
      real z(nz)
      real, allocatable :: in4d(:,:,:,:)
      real, allocatable :: in5d(:,:,:,:,:)


!     Open netcdf file.

      status = NF_OPEN(ncfile, NF_NOWRITE, ncid)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem opening file: ', NF_STRERROR(status), ncid
        stop
      endif

!     Read dimensions.
      status = NF_INQ_DIMID(ncid, 'pass', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for pass: ', NF_STRERROR(status)
        stop
      endif
      status = NF_INQ_DIMLEN(ncid, id, npass)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining pass: ', NF_STRERROR(status)
        stop
      endif

!     Read scalar variables.

      status = NF_INQ_VARID(ncid, 'grid_latitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for glat: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, glat)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining glat: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'grid_longitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for glon: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, glon)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining glon: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'grid_altitude', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for galt: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, galt)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining galt: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'start_date', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for start_date: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_TEXT(ncid, id, date)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining start_date: ', NF_STRERROR(status)
        stop
      endif
      read(date(7:10),*) cyr
      read(date(1:2),*) cmo
      read(date(4:5),*) cda

      status = NF_INQ_VARID(ncid, 'start_time', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for start_time: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_TEXT(ncid, id, time)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining start_time: ', NF_STRERROR(status)
        stop
      endif
      read(time(1:2),*) chr
      read(time(4:5),*) cmn
      read(time(7:8),*) cse

      status = NF_INQ_VARID(ncid, 'x_spacing', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for dx: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, dx)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining dx: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'y_spacing', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for dy: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, dy)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining dy: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'z_spacing', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for dz: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, dz)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining dz: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'x_min', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for xmin: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, xmin)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining xmin: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'y_min', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for ymin: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, ymin)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining ymin: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'z', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for z: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, z)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining z: ', NF_STRERROR(status)
        stop
      endif
      zmin = z(1)

      status = NF_INQ_VARID(ncid, 'u_translation', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for ut: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, ut)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining ut: ', NF_STRERROR(status)
        stop
      endif

      status = NF_INQ_VARID(ncid, 'v_translation', id)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining id for vt: ', NF_STRERROR(status)
        stop
      endif
      status = NF_GET_VAR_REAL(ncid, id, vt)
      if (status .ne. NF_NOERR) then
        write(*,*) 'Problem obtaining vt: ', NF_STRERROR(status)
        stop
      endif

!     Read 3D arrays.

      allocate(in4d(nx,ny,nz,1))
      allocate(in5d(nx,ny,nz,npass,1))

      do n=1, nfld

        status = NF_INQ_VARID(ncid, fname(n), id)
        if (status .ne. NF_NOERR) then
          write(*,*) 'Problem obtaining id for ', fname(n), ': ', NF_STRERROR(status)
          stop
        endif

        if ( (fname(n).eq.'EL') .or. (fname(n).eq.'AZ') .or. (fname(n).eq.'TIME') .or. (fname(n).eq.'HEIGHT') ) then
          status = NF_GET_VAR_REAL(ncid, id, in4d)
          f(:,:,:,n) = in4d(:,:,:,1)
        else
          status = NF_GET_VAR_REAL(ncid, id, in5d)
          f(:,:,:,n) = in5d(:,:,:,npass,1)
        endif
        if (status .ne. NF_NOERR) then
          write(*,*) 'Problem obtaining ', fname(n), ': ', NF_STRERROR(status)
          stop
        endif

      enddo

      deallocate(in4d)
      deallocate(in5d)

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
!     ######              SUBROUTINE READ_MOSAIC_3D               ######
!     ######                                                      ######
!     ##################################################################
!     ##################################################################
!
!
!############################################################################
!
!     PURPOSE:
!
!     This subroutine reads data from a netcdf NMQ 3D reflectivity mosaic file.
!
!     Reference:  "The NSSL National 3-D Refectivity Mosaic Data Specifications
!                  and Product Suite", 16 March 2011
!
!     Author:  David Dowell
!
!     Creation Date:  May 2012
!
!############################################################################

      subroutine READ_MOSAIC_3D(ncfile, nx, ny, nz, mrefl, height, lat, lon)

      USE netcdf

      implicit none

      include 'opaws.inc'

!---- input parameters

      character(len=*) ncfile                                ! input netcdf file name

!---- returned variables
      integer nx                                             ! number of gridpoints in longitude direction
      integer ny                                             ! number of gridpoints in latitude direction
      integer nz                                             ! number of gridpoints in vertical direction
      real mrefl(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z)   ! reflectivity (dBZ)
      real height(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z)  ! height (m MSL)
      real lat(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z)     ! latitude (deg N)
      real lon(mosaic_dim_x, mosaic_dim_y, mosaic_dim_z)     ! longitude (deg E)

!---- Interface block for SUBROUTINE CHECK

      INTERFACE
        SUBROUTINE CHECK(status, message)
          integer, intent (in) :: status
          character(len=*), optional :: message
        END SUBROUTINE CHECK
      END INTERFACE
      
!---- local variables

      integer ncid                    ! netCDF file ID
      integer id                      ! dimension / variable id
      integer i, j, k
      integer(kind=2), allocatable :: refl_short(:,:,:)
      real, allocatable            :: height_1d(:)
      real refl_scale
      real missing_data_flag
      real lat_nw, lon_nw
      real delta_lat, delta_lon




      write(6,*)
      write(6,*) "READ_MOSAIC_3D -> INPUT FILE:"
      write(6,*) ncfile

      call check(nf90_open(ncfile, nf90_nowrite, ncid))

      call check(nf90_inq_dimid(ncid, 'Ht', id));
      call check(nf90_inquire_dimension(ncid, id, len = nz));

      call check(nf90_inq_dimid(ncid, 'Lat', id));
      call check(nf90_inquire_dimension(ncid, id, len = ny));

      call check(nf90_inq_dimid(ncid, 'Lon', id));
      call check(nf90_inquire_dimension(ncid, id, len = nx));

      if (nz .gt. mosaic_dim_z) then
        write(6,*) 'ERROR IN READ_MOSAIC_3D:  nz, mosaic_dim_z = ', nz, mosaic_dim_z
        stop
      endif

      if (ny .gt. mosaic_dim_y) then
        write(6,*) 'ERROR IN READ_MOSAIC_3D:  ny, mosaic_dim_y = ', ny, mosaic_dim_y
        stop
      endif

      if (nx .gt. mosaic_dim_x) then
        write(6,*) 'ERROR IN READ_MOSAIC_3D:  nx, mosaic_dim_x = ', nx, mosaic_dim_x
        stop
      endif


      allocate(refl_short(nx, ny, nz))
      allocate(height_1d(nz))

      call check( nf90_inq_varid(ncid, 'mrefl_mosaic', id) )
      call check( nf90_get_var(ncid, id, refl_short) )
      call check( nf90_get_att(ncid, id, 'Scale', refl_scale) )

      call check( nf90_inq_varid(ncid, 'Height', id) )
      call check( nf90_get_var(ncid, id, height_1d) )

      call check( nf90_get_att(ncid, NF90_GLOBAL, 'MissingData', missing_data_flag) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, 'Latitude', lat_nw) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, 'Longitude', lon_nw) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, 'LatGridSpacing', delta_lat) )
      call check( nf90_get_att(ncid, NF90_GLOBAL, 'LonGridSpacing', delta_lon) )

      do k=1, nz
        do j=1, ny
          do i=1, nx

            height(i,j,k) = height_1d(k)

            mrefl(i,j,k) = refl_short(i,j,k) / refl_scale

            if (mrefl(i,j,k) .eq. missing_data_flag) mrefl(i,j,k) = sbad

            lat(i,j,k) = lat_nw - delta_lat*(j-1)
            lon(i,j,k) = lon_nw + delta_lon*(i-1)

          enddo
        enddo
      enddo

      call check( nf90_close(ncid) )

      deallocate(refl_short)
      deallocate(height_1d)

      RETURN

      END SUBROUTINE READ_MOSAIC_3D
