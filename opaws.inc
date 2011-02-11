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
!      real bad               ! bad/missing data flag in OPAWS1, now obsolete in OPAWS2
!      parameter (bad=9.9E9)
      real sbad              ! bad/missing data flag in sweep files
      real clutter_flag      ! flag for removed ground clutter in sweep files
      parameter (sbad=-32768., clutter_flag=-32767.)
      integer maxflds        ! maximum no. of fields in sweep file
      integer maxrays        ! maximum no. of rays in sweep file
      parameter (maxflds=20, maxrays=3000)
      real pi                ! pi
      parameter (pi=3.14159265)
      real dtor              ! conversion from degrees to radians
      real rtod              ! conversion from radians to degrees
      parameter (dtor=0.0174533, rtod=57.29578)
      integer pd(12)         ! cumulative previous days (for julian day computation)
      data pd /0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334/
      real rearth            ! actual radius of earth (km)
      real eer               ! effective earth radius (km)
      parameter (rearth=6367.0)
      parameter (eer=rearth*4.0/3.0)
!      parameter (eer=rearth)
      real omega             ! rotation rate of earth (rad/s)
      parameter (omega=0.0000729)
      real, parameter :: max_el_angle_diff = 0.15  ! maximum difference between two elevation angles (deg) that are considered essentially the same