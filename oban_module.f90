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

MODULE OBAN_MODULE

  USE DTYPES_module

  implicit none

  integer, private :: nx, ny                                   ! horizontal grid dimensions
  integer, private :: nz                                       ! number of levels (3D analysis) or sweeps (2D analysis)
  real, private, allocatable, dimension(:,:,:,:,:) :: sumdir   ! sum of weights in directional bins
  real, private, allocatable, dimension(:,:,:,:) :: f          ! interpolated fields for current pass
  real, private, allocatable, dimension(:,:,:,:) :: sumf       ! sum of weights for each field
  real, private, allocatable, dimension(:,:,:,:) :: azcomp     ! azimuth-angle components
  real, private, allocatable, dimension(:,:,:,:) :: count      ! number of gates used in interpolation
  real, private, allocatable, dimension(:,:,:)   :: el         ! interpolated elevation angle (deg)
  real, private, allocatable, dimension(:,:,:)   :: height     ! interpolated observation height (km), relative to grid origin
  real, private, allocatable, dimension(:,:,:)   :: time       ! interpolated time (s), relative to central time

CONTAINS

!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######               SUBROUTINE OBAN_INIT                   ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     Allocate and initialize variables needed for the other subroutines
!     in this module.
!
!############################################################################
!
!     Author:  David Dowell, January 2011
!
!############################################################################

  SUBROUTINE OBAN_INIT(nfld, anal, allow_extrapolation)

    implicit none

! Passed variables

    integer nfld                    ! number of data fields to be gridded
    integer i, j, k
    TYPE(ANALYSISGRID) anal
    logical allow_extrapolation     ! should extrapolation be allowed?
                                    !   .true. for standard objective analysis, .false. for interpolation only

!############################################################################
!
!   Get dimension sizes

    nx   = size(anal%xg)
    ny   = size(anal%yg)
    nz   = size(anal%zg)
    write(6,*) 'OBAN_INIT->INPUT DIMS NX: ',nx,' NY: ',ny,' NZ: ',nz

!############################################################################
!
!   Initialize local storage and set == 0

    IF ( .not. allow_extrapolation .and. .not. allocated(sumdir) ) allocate(sumdir(8,nx,ny,nz,nfld))
    IF ( .not. allocated(f) ) allocate(f(nx,ny,nz,nfld))
    IF ( .not. allocated(sumf) ) allocate(sumf(nx,ny,nz,0:nfld))
    IF ( .not. allocated(azcomp) ) allocate(azcomp(nx,ny,nz,2))
    IF ( .not. allocated(count) ) allocate(count(nx,ny,nz,nfld))
    IF ( .not. allocated(el) ) allocate(el(nx,ny,nz))
    IF ( .not. allocated(height) ) allocate(height(nx,ny,nz))
    IF ( .not. allocated(time) ) allocate(time(nx,ny,nz))

    IF ( .not. allow_extrapolation ) sumdir(:,:,:,:,:)   = 0.0
    DO i = 1,nx
    DO j = 1,ny
    DO k = 1,nz
      f(i,j,k,1:nfld)      = 0.0
      sumf(i,j,k,0:nfld)   = 0.0
      azcomp(i,j,k,1:2)    = 0.0
      count(i,j,k,1:nfld)  = 0
      el(i,j,k)            = 0.0
      height(i,j,k)        = 0.0
      time(i,j,k)          = 0.0
   ENDDO
   ENDDO
   ENDDO

  RETURN
  END SUBROUTINE OBAN_INIT


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######                SUBROUTINE OBAN_CLEANUP               ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     Deallocate arrays.
!
!############################################################################
!
!     Author:  David Dowell, January 2011
!
!############################################################################

  SUBROUTINE OBAN_CLEANUP

  implicit none

    IF ( allocated(sumdir) ) deallocate(sumdir)
    deallocate(f)
    deallocate(sumf)
    deallocate(azcomp)
    deallocate(count)
    deallocate(el)
    deallocate(height)
    deallocate(time)

  RETURN
  END SUBROUTINE OBAN_CLEANUP


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######               SUBROUTINE PROCESS_SWEEP               ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     This subroutine updates the running sums of the analyzed fields, based on
!     the observations in one sweep of radar data.  The coordinate system for the
!     objective analyis can be either 2D (sweep-by-sweep) or 3D (Cartesian).
!
!     Two options are available for the weighting function used by the interpolation scheme:
!
!     1.  Cressman weighting function:
!
!         (a) 2D:  wgt = ( 1.0 - deltax**2/hsp**2 - deltay**2/hsp**2 )
!                      / ( 1.0 + deltax**2/hsp**2 + deltay**2/hsp**2 )
!         (b) 3D:  wgt = ( 1.0 - deltax**2/hsp**2 - deltay**2/hsp**2 - deltaz**2/vsp**2 )
!                      / ( 1.0 + deltax**2/hsp**2 + deltay**2/hsp**2 + deltaz**2/vsp**2 )
!
!    2.  Barnes weighting function:
!
!         (a) 2D:  wgt = exp ( -deltax**2/hsp - deltay**2/hsp )
!         (b) 3D:  wgt = exp ( -deltax**2/hsp - deltay**2/hsp - deltaz**2/vsp )
!
!############################################################################
!
!     Original Author:  David Dowell, Created:  December 2001
!
!     Modified for multipass Barnes by Penn State (see below):  Jan-April 2007
!
!     Rewritten by Lou Wicker, January 2010, to work with new f90
!               objective analysis code 
!
!############################################################################

  SUBROUTINE PROCESS_SWEEP(vol, anal, analysis_type, pass, s,   &
                           nfld, unfold_flag, Nyquist_vel,      &
                           method, gamma, hsp0, vsp0,           &
                           allow_extrapolation,                 &
                           minsum, minrange, map_proj)

    implicit none

    include 'opaws.inc'

! Passed variables

    TYPE(VOLUME) vol
    TYPE(ANALYSISGRID) anal
    
    integer analysis_type           ! Type of analysis:  1=3D Cartesian, 2=2D sweep by sweep
    integer pass                    ! pass
    integer s                       ! sweep index
    integer nfld                    ! number of data fields to be gridded
    integer unfold_flag(nfld)       ! locally unfold field? (0=no, 1=yes)
    real Nyquist_vel(nfld)          ! Nyquist velocity, in m/s
    integer method                  ! interpolation method:
                                    !   1=Cressman
                                    !   2=Barnes
    real gamma                      ! Barnes scale factor used on higher order passes
    real hsp0                       ! horizontal smoothing parameter:
                                    !   method=1:  hsp0 is the Cressman radius of influence (km)
                                    !   method=2:  hsp0 is the Barnes smoothing parameter (kappa, in km*km) 
    real vsp0                       ! vertical smoothing parameter:
                                    !   method=1:  vsp0 is the Cressman radius of influence (km)
                                    !   method=2:  vsp0 is the Barnes smoothing parameter (kappa, in km*km) 
    logical allow_extrapolation     ! should extrapolation be allowed?
                                    !   .true. for standard objective analysis, .false. for interpolation only
    real minsum                     ! threshold for minimum sum of weights required to produce
                                    !   an objectively-analyzed observation
    real minrange                   ! minimum-range threshold (data closer to radar
                                    !   are discarded)
    integer map_proj                ! map projection:
                                    !   0 = flat earth
                                    !   1 = oblique azimuthal
                                    !   2 = Lambert conformal
                         
 !---- Local variables

    integer n                       ! field number
    integer o                       ! observation index
    real x, y, z                    ! location of gate relative to grid origin (km)
    integer i, j, k                 ! grid indices
    integer imin, imax              ! minimimum and maximum i for search
    integer jmin, jmax              ! minimimum and maximum j for search
    integer kmin, kmax              ! minimimum and maximum k for search
    integer irad, jrad, krad        ! radius of search area
    integer ii, jj, kk              ! nearest grid point to gate location
    real dh2                        ! square of horizontal distance (km*km)
    real dv2                        ! square of vertical distance (km*km)
    real wgt                        ! Cressman / Barnes weight
    real hsp                        ! hsp for current pass
    real hspsq                      ! hsp*hsp
    real vsp                        ! vsp for current pass
    real vspsq                      ! vsp*vsp
    integer count2                  ! counter for the RMS statistics
    real rms                        ! root mean square error of analysis interp back to obs
    real dmax, dmin                 ! difference field max/min
    real tlint2d, tlint
    integer find_index

! The following parameters are for local velocity unfolding (Miller et al. 1986).
    real, allocatable :: vref(:,:)  ! reference velocity for local velocity unfolding
    real KN                         ! number of Nyquist intervals between current ob. and vref
    integer kappa                   ! local unfolding factor (Miller et al. 1986, p. 165)
    integer n_bin                   ! number of velocity bins
    integer count_max               ! mode of bin_count
    integer b_max                   ! bin index corresponding to mode
    integer b                       ! bin index
    integer, allocatable :: bin_count(:,:,:)   ! number of velocity observations in each bin
    real vr_bin_size                ! size of velocity bins, for computing mode
    parameter(vr_bin_size=2.0)

    write(6,*) 'PROCESS_SWEEP->PASS #, SWEEP #: ',pass, ' ', s

    IF (pass.eq.1) THEN
      hsp = hsp0
    ELSE
      hsp = hsp0 * (gamma**(pass-1))
    ENDIF
    write (6,*) 'PROCESS_SWEEP:  Horizontal smoothing parameter:  ',hsp
    hspsq = hsp*hsp

    IF (analysis_type .eq. 1) THEN
      IF (pass.eq.1) THEN
        vsp = vsp0
      ELSE
        vsp = vsp0 * (gamma**(pass-1))
      ENDIF
      write (6,*) 'PROCESS_SWEEP:  Vertical smoothing parameter:  ',vsp
      vspsq = vsp*vsp
    ENDIF

    IF (method.eq.1) THEN
      irad = 1+nint(hsp/anal%dx)
      jrad = 1+nint(hsp/anal%dy)
      IF (analysis_type .eq. 1) THEN
        krad = 1+nint(vsp/anal%dz)
        write(6,FMT='(" PROCESS_SWEEP:  Cressman analysis method requested:  I/J/K Radius = ",i2,1x,i2,1x,i2)') irad, jrad, krad
      ENDIF
      IF (analysis_type .eq. 2) THEN
        write(6,FMT='(" PROCESS_SWEEP:  Cressman analysis method requested:  I/J Radius = ",i2,1x,i2)') irad, jrad
      ENDIF
      write(6,*)
    ELSE IF (method.eq.2) THEN
      irad = 1+nint(0.5+sqrt(10.0*hsp)/anal%dx)  ! Changed this calc slightly to use more points
      jrad = 1+nint(0.5+sqrt(10.0*hsp)/anal%dy)  ! Changed this calc slightly to use more points
      IF (analysis_type .eq. 1) THEN
        krad = 1+nint(0.5+sqrt(10.0*vsp)/anal%dz)  ! Changed this calc slightly to use more points
        write(6,FMT='(" PROCESS_SWEEP:  Barnes analysis method requested:  I/J/K Radius = ",i2,1x,i2,1x,i2)') irad, jrad, krad
      ENDIF
      IF (analysis_type .eq. 2) THEN
        write(6,FMT='(" PROCESS_SWEEP:  Barnes analysis method requested:  I/J Radius = ",i2,1x,i2)') irad, jrad
      ENDIF
      write(6,*)
    ELSE
      write(6,*) 'PROCESS_SWEEP  Unknown analysis method requested:  ', method
      stop
    ENDIF


!############################################################################
!
!   Back interpolate from the analyzed fields to the observations.

    IF (pass.gt.1) THEN

      DO n = 1,nfld

! DTD (09/30/2010): First check to see if the current field is allocated.  If it isn't,
! skip the field.  Originally, if the field was bad (no good gates read in from READSWEEP),
! the vol%sweep%field%ob structure would still contain data from the last pass.  This
! was effectly resulting in copies of fields in the oban file wherever a given field
! was "skipped" in the READSWEEP routine.  The following line of code will skip the analysis
! in such cases

        IF( .not. allocated(vol%sweep%field(n)%ob)) CYCLE

        write(6,*) 'PROCESS_SWEEP  back interpolation of field ', vol%sweep%field(n)%name

        rms = 0.0
        count2 = 0
        dmax = 0.0
        dmin = 0.0
        DO o = 1,vol%sweep%field(n)%number_of_valid_obs
          
          IF (vol%sweep%field(n)%ob(o)%range .ge. minrange) THEN

            x = vol%sweep%field(n)%ob(o)%x + vol%xoffset
            y = vol%sweep%field(n)%ob(o)%y + vol%yoffset
            z = vol%sweep%field(n)%ob(o)%z + vol%zoffset

            IF (analysis_type .eq. 1) THEN
              vol%sweep%field(n)%ob(o)%analysis_value = tlint(anal%f(1,1,1,n,pass-1), x, y, z, nx, ny, nz,  &
                                                              anal%xg, anal%yg, anal%zg, sbad)
            ENDIF
            IF (analysis_type .eq. 2) THEN
              vol%sweep%field(n)%ob(o)%analysis_value = tlint2d(anal%f(1,1,s,n,pass-1), x, y, nx, ny,  &
                                                                anal%xg, anal%yg, sbad)
            ENDIF
                                                        
            IF( vol%sweep%field(n)%ob(o)%analysis_value .ne. sbad) THEN
              vol%sweep%field(n)%ob(o)%value = vol%sweep%field(n)%ob(o)%value          &
                                             - vol%sweep%field(n)%ob(o)%analysis_value
              rms    = rms + vol%sweep%field(n)%ob(o)%value**2
              count2 = count2 + 1
              dmax = max(dmax, vol%sweep%field(n)%ob(o)%value)
              dmin = min(dmin, vol%sweep%field(n)%ob(o)%value)
            ELSE
              vol%sweep%field(n)%ob(o)%value = sbad
            ENDIF
            
          ENDIF          ! minrange conditional
        ENDDO            ! observation in each sweep loop
        
        IF (analysis_type .eq. 1) THEN
          write(6,FMT='(1x,a,i3.3,a,a8,2(2x,f9.2,2x,f9.2),2x,f9.2,2x,i6)') 'SWEEP: ',s,' MAX/MIN: ', &
                      vol%sweep%field(n)%name,                                                       &
                      maxval(anal%f(:,:,:,n,pass-1),anal%f(:,:,:,n,pass-1).ne.sbad),                 &
                      minval(anal%f(:,:,:,n,pass-1),anal%f(:,:,:,n,pass-1).ne.sbad),                 &
                      vol%sweep%field(n)%obmax, vol%sweep%field(n)%obmin,                            &
                      sqrt(rms / float(count2)), count2
        ENDIF
        IF (analysis_type .eq. 2) THEN
          write(6,FMT='(1x,a,i3.3,a,a8,2(2x,f9.2,2x,f9.2),2x,f9.2,2x,i6)') 'SWEEP: ',s,' MAX/MIN: ', &
                      vol%sweep%field(n)%name,                                                       &
                      maxval(anal%f(:,:,s,n,pass-1),anal%f(:,:,s,n,pass-1).ne.sbad),                 &
                      minval(anal%f(:,:,s,n,pass-1),anal%f(:,:,s,n,pass-1).ne.sbad),                 &
                      vol%sweep%field(n)%obmax, vol%sweep%field(n)%obmin,                            &
                      sqrt(rms / float(count2)), count2
        ENDIF
        write(6,FMT='(7x," Max/Min Difference from back interpolation:  ", f9.2, 2x, f9.2)') dmax, dmin
        write(6,*)

      ENDDO              ! field loop

    ENDIF   ! IF (pass.gt.1)

!############################################################################
!
!   Process sweep files and compute weighted sums.

    DO n = 1,nfld

! DTD (09/30/2010): First check to see if the current field is allocated.  If it isn't,
! skip the field.  Originally, if the field was bad (no good gates read in from READSWEEP),
! the vol%sweep%field%ob structure would still contain data from the last pass.  This
! was effectly resulting in copies of fields in the oban file wherever a given field
! was "skipped" in the READSWEEP routine.  The following IF/ENDIF block of code will skip the analysis
! in such cases (and also set all analysis values (anal%f) to the bad data flag (sbad) for this 
! field, pass, and sweep

      IF( .not. allocated(vol%sweep%field(n)%ob)) THEN
        write(6,*) 'Sweep has no good gates! Skipping this sweep!'
        CYCLE  
      END IF

      write(6,FMT='(1x,90("-"))') 
      write(6,*) 'COMPARISON of GRID BOX VERSUS OB LOCATIONS for VARIABLE:  ',vol%sweep%field(n)%name
      write(6,*)
      write(6,*) 'GRID(XMIN)     OB(XMIN)      OB(XMAX)     GRID(XMAX)'
      write(6,*) anal%xg(1),                                      &
                 minval(vol%sweep%field(n)%ob(:)%x)+vol%xoffset,  &
                 maxval(vol%sweep%field(n)%ob(:)%x)+vol%xoffset,  &
                 anal%xg(nx)
      write(6,*)
      write(6,*) 'GRID(YMIN)     OB(YMIN)      OB(YMAX)     GRID(YMAX)'
      write(6,*) anal%yg(1),                                      &
                 minval(vol%sweep%field(n)%ob(:)%y)+vol%yoffset,  &
                 maxval(vol%sweep%field(n)%ob(:)%y)+vol%yoffset,  &
                 anal%yg(ny)
      write(6,FMT='(1x,90("-"))') 

! Determine reference velocity for local velocity unfolding (Miller et al. 1986).
      IF (unfold_flag(n).eq.1) THEN

        IF (pass.gt.1) THEN
          write(6,*) 'velocity unfolding for multi-pass scheme not yet implemented'
          stop
        ENDIF

        IF (analysis_type.ne.2) THEN
          write(6,*) 'local unfolding does not work with analysis type ', analysis_type
          stop
        ENDIF

        write(6,*) 'field ', vol%sweep%field(n)%name, ' will be unfolded with Nyquist velocity ', Nyquist_vel(n)
        n_bin = int(abs(Nyquist_vel(n))/vr_bin_size + 0.5)
        allocate(vref(nx,ny))
        allocate(bin_count(nx,ny,-n_bin:n_bin))
        bin_count(:,:,:) = 0

! Note:  k index is not needed for loops below because analysis_type is 2 (2D sweep-by-sweep).

        DO o = 1,vol%sweep%field(n)%number_of_valid_obs
          IF (vol%sweep%field(n)%ob(o)%range .ge. minrange .and.    &
              vol%sweep%field(n)%ob(o)%value .ne. sbad) THEN
            x = vol%sweep%field(n)%ob(o)%x + vol%xoffset
            y = vol%sweep%field(n)%ob(o)%y + vol%yoffset
            ii = find_index(x,anal%xg, nx)
            jj = find_index(y,anal%yg, ny)
            imin = max(ii-irad, 1)              
            imax = min(ii+irad, nx)
            jmin = max(jj-jrad, 1)
            jmax = min(jj+jrad, ny)
            DO j = jmin,jmax
              DO i = imin,imax
                IF (method.eq.1) THEN
                  dh2 = (x-anal%xg(i))**2 + (y-anal%yg(j))**2
                  wgt = (1.0 - dh2/hspsq) / (1.0 + dh2/hspsq)
                ELSE 
                  wgt = exp( -(x-anal%xg(i))**2/hsp - (y-anal%yg(j))**2/hsp )
                ENDIF
                IF (wgt .gt. 0.0) THEN
                  IF (vol%sweep%field(n)%ob(o)%value .ge. 0.0) THEN
                    b = int(vol%sweep%field(n)%ob(o)%value / vr_bin_size + 0.5)
                  ELSE
                    b = int(vol%sweep%field(n)%ob(o)%value / vr_bin_size - 0.5)
                  ENDIF
                  IF (iabs(b).gt.n_bin) THEN
                    write(6,*) 'outside Nyquist interval:'
                    write(6,*) vol%sweep%field(n)%ob(o)%value, Nyquist_vel(n), b
                    stop
                  ELSE
                    bin_count(i,j,b) = bin_count(i,j,b) + 1
                  ENDIF
                ENDIF    ! if (wgt.gt.0.0)
              ENDDO      ! i loop
            ENDDO        ! j loop
          ENDIF          ! minrange conditional
        ENDDO            ! observation in each sweep loop

        DO j=1, ny
          DO i=1, nx
            count_max = 0
            b_max = 0
            DO b = -n_bin, n_bin
              IF (bin_count(i,j,b).gt.count_max) THEN
                count_max = bin_count(i,j,b)
                b_max = b
              ENDIF
            ENDDO
            vref(i,j) = b_max*vr_bin_size     ! set vref to mode of binned velocity distribution
          ENDDO
        ENDDO

      ENDIF      ! IF (unfold_flag(n).eq.1)

!############################################################################
!
!       Here's the main loop over each observation.
!
!############################################################################

      DO o = 1,vol%sweep%field(n)%number_of_valid_obs

        IF (vol%sweep%field(n)%ob(o)%range .ge. minrange .and.    &
            vol%sweep%field(n)%ob(o)%value .ne. sbad) THEN

          x = vol%sweep%field(n)%ob(o)%x + vol%xoffset
          y = vol%sweep%field(n)%ob(o)%y + vol%yoffset
          z = vol%sweep%field(n)%ob(o)%z + vol%zoffset

          ii = find_index(x,anal%xg, nx)
          jj = find_index(y,anal%yg, ny)

          imin = max(ii-irad, 1)              
          imax = min(ii+irad, nx)
          jmin = max(jj-jrad, 1)
          jmax = min(jj+jrad, ny)
            
          IF (analysis_type .eq. 1) THEN              
            kk = find_index(z,anal%zg, nz)
            kmin = max(kk-krad, 1)
            kmax = min(kk+krad, nz)
          ENDIF
          IF (analysis_type .eq. 2) THEN
            kmin = s
            kmax = s
          ENDIF

          DO k = kmin,kmax
            DO j = jmin,jmax
              DO i = imin,imax

                IF (method.eq.1) THEN
                  dh2 = (x-anal%xg(i))**2 + (y-anal%yg(j))**2
                  IF (analysis_type .eq. 1) THEN
                    dv2 = (z-anal%zg(k))**2
                    wgt = (1.0 - dh2/hspsq - dv2/vspsq) / (1.0 + dh2/hspsq + dv2/vspsq)
                  ENDIF
                  IF (analysis_type .eq. 2) THEN
                    wgt = (1.0 - dh2/hspsq) / (1.0 + dh2/hspsq)
                  ENDIF
                ELSE
                  IF (analysis_type .eq. 1) THEN
                    wgt = exp( -(x-anal%xg(i))**2/hsp - (y-anal%yg(j))**2/hsp - (z-anal%zg(k))**2/vsp )
                  ENDIF
                  IF (analysis_type .eq. 2) THEN
                    wgt = exp( -(x-anal%xg(i))**2/hsp - (y-anal%yg(j))**2/hsp )
                  ENDIF
                ENDIF

                IF (wgt .gt. 0.0) THEN

                  IF (unfold_flag(n).eq.1) THEN      ! local velocity unfolding (Miller et al. 1986)
                    KN = (vref(i,j) - vol%sweep%field(n)%ob(o)%value) / (2.0*Nyquist_vel(n))
                    kappa = nint(KN)
                    vol%sweep%field(n)%ob(o)%value = vol%sweep%field(n)%ob(o)%value + kappa*2.0*Nyquist_vel(n)
                  ENDIF
                                  
                  f(i,j,k,n)      = f(i,j,k,n)      + wgt * vol%sweep%field(n)%ob(o)%value
                  sumf(i,j,k,n)   = sumf(i,j,k,n)   + wgt
                  count(i,j,k,n)  = count(i,j,k,n)  + 1

                  IF (.not. allow_extrapolation) THEN
                    IF (analysis_type .eq. 1) THEN
                      CALL update_dir_bins(sumdir(1,i,j,k,n), wgt, x, y, z, anal%xg(i), anal%yg(j), anal%zg(k))
                    ENDIF
                    IF (analysis_type .eq. 2) THEN
                      CALL update_dir_bins(sumdir(1,i,j,k,n), wgt, x, y, 0.0, anal%xg(i), anal%yg(j), 0.0)
                    ENDIF
                  ENDIF

                  IF( pass .eq. 1 ) THEN
                    sumf(i,j,k,0)   = sumf(i,j,k,0)   + wgt
                    height(i,j,k)   = height(i,j,k)   + wgt * z
                    time(i,j,k)     = time(i,j,k)     + wgt * vol%sweep%field(n)%ob(o)%time
                    el(i,j,k)       = el(i,j,k)       + wgt * vol%sweep%field(n)%ob(o)%el
                    azcomp(i,j,k,1) = azcomp(i,j,k,1) + wgt * sin(dtor*vol%sweep%field(n)%ob(o)%az)
                    azcomp(i,j,k,2) = azcomp(i,j,k,2) + wgt * cos(dtor*vol%sweep%field(n)%ob(o)%az)
                  ENDIF
                  
                ENDIF    ! if (wgt.gt.0.0)

              ENDDO      ! i loop
            ENDDO        ! j loop
          ENDDO          ! k loop
            
        ENDIF          ! minrange conditional

      ENDDO            ! observation in each sweep loop

      IF (unfold_flag(n).eq.1) THEN
        deallocate(vref)
        deallocate(bin_count)
      ENDIF

    ENDDO              ! field loop

  RETURN
  END SUBROUTINE PROCESS_SWEEP


!############################################################################
!
!     ##################################################################
!     ######                                                      ######
!     ######               SUBROUTINE FINISH_ANALYSIS             ######
!     ######                                                      ######
!     ##################################################################
!
!
!     PURPOSE:
!
!     Compute analysis values from weighted sums.
!
!############################################################################
!
!     Original Author:  David Dowell, Created:  December 2001
!
!     Modified for multipass Barnes by Penn State (see below):  Jan-April 2007
!
!     Rewritten by Lou Wicker, January 2010, to work with new f90
!               objective analysis code
!
!     Separated from mpass_oban2d to allow 3D analyses by David Dowell, January 2011
!
!############################################################################

  SUBROUTINE FINISH_ANALYSIS(vol, anal, pass, nfld, allow_extrapolation, minsum)

    implicit none

    include 'opaws.inc'

! Passed variables

    TYPE(VOLUME) vol
    TYPE(ANALYSISGRID) anal
    integer pass                    ! pass
    integer nfld                    ! number of data fields to be gridded
    logical allow_extrapolation     ! should extrapolation be allowed?
                                    !   .true. for standard objective analysis, .false. for interpolation only
    real minsum                     ! threshold for minimum sum of weights required to produce
                                    !   an objectively-analyzed observation

! Local variables

    integer n                       ! field number
    real comptoaz                   ! functions used by this subroutine
    integer i, j, k                 ! grid indices
    integer d                       ! index of directional bin (octant)
    logical data_in_all_octants     ! .true. if there are nearby data in all octants surrounding a gridpoint
!    real x, y                       ! location of gate relative to grid origin (km)
!    real compute_az, compute_height ! functions for filling void regions with az/el/height info


!############################################################################
!
!   Compute interpolated values from weighted sums.

    DO k = 1,nz
      DO j = 1,ny
        DO i = 1,nx
        
          DO n = 1,nfld

! DCD 1/26/11:  removed mincount from IF statement because mincount thresholding occurs in main program (oban)
!            IF( sumf(i,j,k,n) .ge. minsum .and. count(i,j,k,n) .ge. mincount ) THEN
            IF( sumf(i,j,k,n) .ge. minsum ) THEN
              f(i,j,k,n) = f(i,j,k,n) / sumf(i,j,k,n)
              IF (.not. allow_extrapolation) THEN
                data_in_all_octants = .true.
                DO d=1, 8
                  IF (sumdir(d,i,j,k,n) .lt. (0.1*minsum) ) data_in_all_octants = .false.
                ENDDO
                IF (.not. data_in_all_octants) f(i,j,k,n) = sbad
              ENDIF
            ELSE
              f(i,j,k,n) = sbad
            ENDIF

! LJW:  Tricky here:  if pass > 1:
!                         if f(i,j,k,n) NE sbad:
!                             add increment
!                         else
!                             assign it to previous pass analysis value
!                         if pass  == 1:
!                            set analysis to f(i,j,k,n)
            
            IF( pass .gt. 1 ) THEN
              IF( f(i,j,k,n) .ne. sbad ) THEN
                anal%f(i,j,k,n,pass) = anal%f(i,j,k,n,pass-1) + f(i,j,k,n)
              ELSE
                anal%f(i,j,k,n,pass) = anal%f(i,j,k,n,pass-1)
              ENDIF
            ELSE
              anal%f(i,j,k,n,pass) = f(i,j,k,n)
            ENDIF

!           DCD 1/14/11
            IF (pass .eq. 1) THEN
              anal%count(i,j,k,n) = count(i,j,k,n)
            ENDIF

          ENDDO   ! n loop

          IF( pass .eq. 1 ) THEN
            anal%az(i,j,k)       = sbad
            anal%el(i,j,k)       = sbad
            anal%height(i,j,k)   = sbad
            anal%time(i,j,k)     = sbad
          ENDIF

! I dont change these on successive passes.  Perhaps I should.
! I also dont threshold these values, because when I create a filled field (like dBZ=0), I need the
! time, azimuths and elevations for the DART file....

! DCD 1/26/11:  changed .gt. to .ge., for consistency with previous use of minsum

          IF( pass .eq. 1 .and. sumf(i,j,k,0) .ge. minsum ) THEN
          
            azcomp(i,j,k,1)    = azcomp(i,j,k,1) / sumf(i,j,k,0)
            azcomp(i,j,k,2)    = azcomp(i,j,k,2) / sumf(i,j,k,0)
            anal%az(i,j,k)     = comptoaz(azcomp(i,j,k,1),azcomp(i,j,k,2))
            anal%el(i,j,k)     = el(i,j,k)     / sumf(i,j,k,0)
            anal%height(i,j,k) = height(i,j,k) / sumf(i,j,k,0)
            anal%time(i,j,k)   = time(i,j,k)   / sumf(i,j,k,0)

! Make sure each analysis grid point has geometry and time information
! DCD 1/27/11  prefer not to use these computations          
!          ELSEIF( pass .eq. 1 ) THEN             
!            x                   = anal%xg(i)-vol%xoffset
!            y                   = anal%yg(j)-vol%yoffset
!            anal%az(i,j,k)      = compute_az(x,y)
!            anal%el(i,j,k)      = vol%sweep%field(1)%el
!            anal%height(i,j,k)  = compute_height(x,y,anal%el(i,j,k),anal%ralt,anal%galt)
!            anal%time(i,j,k)    = vol%sweep%field(1)%time
          ENDIF

        ENDDO   ! i loop
      ENDDO   ! j loop 
    ENDDO   ! k loop
    
    write(6,FMT='(80("-"))')
    write(6,*)

  RETURN
  END SUBROUTINE FINISH_ANALYSIS
  
END MODULE OBAN_MODULE
