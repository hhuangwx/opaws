!############################# LICENSE ######################################
!
!   Copyright (C) <2010>  <David C. Dowell and Louis J. Wicker, NOAA>
!
!   This liopaws.incbrary is free software; you can redistribute it and/or
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
c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE XDERIV                    ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the x direction of
c     a gridded quantity at each grid point.  At the sides, derivatives
c     are first order, uncentered.  Within the interior, derivatives
c     are second order, centered when possible.  If the value of
c     q at i-1 or i+1 is bad/missing, then a first order, uncentered
c     derivative is considered.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine xderiv(q, dqdx, nx, imax, ny, jmax, nz, kmax, dx)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dx                  ! grid spacing in x direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdx(nx, ny, nz)    ! x derivative of q
      integer i, j, k          ! grid indices
      real dx2                 ! 2.0*dx

      dx2 = 2.0*dx

      do k=1, kmax
        do j=1, jmax

          if (imax.eq.1) then
            dqdx(1,j,k) = sbad          
          else
            if ( (q(1,j,k).eq.sbad) .or. (q(2,j,k).eq.sbad) ) then
              dqdx(1,j,k) = sbad
            else
              dqdx(1,j,k) = ( (q(2,j,k)) - (q(1,j,k)) ) / dx
            endif

            if ( (q(imax-1,j,k).eq.sbad) .or. (q(imax,j,k).eq.sbad) ) then
              dqdx(imax,j,k) = sbad
            else
              dqdx(imax,j,k) = ( (q(imax,j,k)) - (q(imax-1,j,k)) ) / dx
            endif
          endif

          do i=2, imax-1
            if (q(i+1,j,k).eq.sbad) then
              if ( (q(i-1,j,k).eq.sbad) .or. (q(i,j,k).eq.sbad) ) then
                dqdx(i,j,k) = sbad
              else
                dqdx(i,j,k) = ( q(i,j,k) - q(i-1,j,k) ) / dx
              endif
            else if (q(i-1,j,k).eq.sbad) then
              if ( (q(i,j,k).eq.sbad) .or. (q(i+1,j,k).eq.sbad) ) then
                dqdx(i,j,k) = sbad
              else
                dqdx(i,j,k) = ( q(i+1,j,k) - q(i,j,k) ) / dx
              endif
            else
              dqdx(i,j,k) = ( q(i+1,j,k) - q(i-1,j,k) ) / dx2
            endif
          enddo

        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE YDERIV                    ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the y direction of
c     a gridded quantity at each grid point.  At the sides, derivatives
c     are first order, uncentered.  Within the interior, derivatives
c     are second order, centered when possible.  If the value of
c     q at j-1 or j+1 is bad/missing, then a first order, uncentered
c     derivative is considered.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine yderiv(q, dqdy, nx, imax, ny, jmax, nz, kmax, dy)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dy                  ! grid spacing in y direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdy(nx, ny, nz)    ! y derivative of q
      integer i, j, k          ! grid indices
      real dy2                 ! 2.0*dy

      dy2 = 2.0*dy

      do k=1, kmax
        do i=1, imax

          if (jmax.eq.1) then
            dqdy(i,1,k) = sbad
          else
            if ( (q(i,1,k).eq.sbad) .or. (q(i,2,k).eq.sbad) ) then
              dqdy(i,1,k) = sbad
            else
              dqdy(i,1,k) = ( (q(i,2,k)) - (q(i,1,k)) ) / dy
            endif

            if ( (q(i,jmax-1,k).eq.sbad) .or. (q(i,jmax,k).eq.sbad) ) then
              dqdy(i,jmax,k) = sbad
            else
              dqdy(i,jmax,k) = ( (q(i,jmax,k)) - (q(i,jmax-1,k)) ) / dy
            endif
          endif

          do j=2, jmax-1
            if (q(i,j+1,k).eq.sbad) then
              if ( (q(i,j-1,k).eq.sbad) .or. (q(i,j,k).eq.sbad) ) then
                dqdy(i,j,k) = sbad
              else
                dqdy(i,j,k) = ( q(i,j,k) - q(i,j-1,k) ) / dy
              endif
            else if (q(i,j-1,k).eq.sbad) then
              if ( (q(i,j,k).eq.sbad) .or. (q(i,j+1,k).eq.sbad) ) then
                dqdy(i,j,k) = sbad
              else
                dqdy(i,j,k) = ( q(i,j+1,k) - q(i,j,k) ) / dy
              endif
            else
              dqdy(i,j,k) = ( q(i,j+1,k) - q(i,j-1,k) ) / dy2
            endif
          enddo

        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE ZDERIV                    ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the z direction of
c     a gridded quantity at each grid point.  At the top/bottom, derivatives
c     are first order, uncentered.  Within the interior, derivatives
c     are second order, centered when possible.  If the value of
c     q at k-1 or k+1 is bad/missing, then a first order, uncentered
c     derivative is considered.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine zderiv(q, dqdz, nx, imax, ny, jmax, nz, kmax, dz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dz                  ! grid spacing in z direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdz(nx, ny, nz)    ! z derivative of q
      integer i, j, k          ! grid indices
      real dz2                 ! 2.0*dz

      dz2 = 2.0*dz

      do j=1, jmax
        do i=1, imax

          if (kmax.eq.1) then
            dqdz(i,j,1) = sbad
          else
            if ( (q(i,j,1).eq.sbad) .or. (q(i,j,2).eq.sbad) ) then
              dqdz(i,j,1) = sbad
            else
              dqdz(i,j,1) = ( (q(i,j,2)) - (q(i,j,1)) ) / dz
            endif

            if ( (q(i,j,kmax-1).eq.sbad) .or. (q(i,j,kmax).eq.sbad) ) then
              dqdz(i,j,kmax) = sbad
            else
              dqdz(i,j,kmax) = ( (q(i,j,kmax)) - (q(i,j,kmax-1)) ) / dz
            endif
          endif

          do k=2, kmax-1
            if (q(i,j,k+1).eq.sbad) then
              if ( (q(i,j,k-1).eq.sbad) .or. (q(i,j,k).eq.sbad) ) then
                dqdz(i,j,k) = sbad
              else
                dqdz(i,j,k) = ( q(i,j,k) - q(i,j,k-1) ) / dz
              endif
            else if (q(i,j,k-1).eq.sbad) then
              if ( (q(i,j,k).eq.sbad) .or. (q(i,j,k+1).eq.sbad) ) then
                dqdz(i,j,k) = sbad
              else
                dqdz(i,j,k) = ( q(i,j,k+1) - q(i,j,k) ) / dz
              endif
            else
              dqdz(i,j,k) = ( q(i,j,k+1) - q(i,j,k-1) ) / dz2
            endif
          enddo

        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE XDERIVC                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the x direction of
c     a gridded quantity at each grid point.  Only second order,
c     centered derivatives are computed.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine xderivc(q, dqdx, nx, imax, ny, jmax, nz, kmax, dx)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dx                  ! grid spacing in x direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdx(nx, ny, nz)    ! x derivative of q
      integer i, j, k          ! grid indices
      real dx2                 ! 2.0*dx

      dx2 = 2.0*dx

      do k=1, kmax
        do j=1, jmax

          dqdx(1,j,k) = sbad
          dqdx(imax,j,k) = sbad

          do i=2, imax-1
            if ( (q(i-1,j,k).eq.sbad) .or. (q(i+1,j,k).eq.sbad) ) then
              dqdx(i,j,k) = sbad
            else
              dqdx(i,j,k) = ( q(i+1,j,k) - q(i-1,j,k) ) / dx2
            endif
          enddo

        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE YDERIVC                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the y direction of
c     a gridded quantity at each grid point.  Only second order,
c     centered derivatives are computed.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine yderivc(q, dqdy, nx, imax, ny, jmax, nz, kmax, dy)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dy                  ! grid spacing in y direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdy(nx, ny, nz)    ! y derivative of q
      integer i, j, k          ! grid indices
      real dy2                 ! 2.0*dy

      dy2 = 2.0*dy

      do k=1, kmax
        do i=1, imax

          dqdy(i,1,k) = sbad
          dqdy(i,jmax,k) = sbad

          do j=2, jmax-1
            if ( (q(i,j-1,k).eq.sbad) .or. (q(i,j+1,k).eq.sbad) ) then
              dqdy(i,j,k) = sbad
            else
              dqdy(i,j,k) = ( q(i,j+1,k) - q(i,j-1,k) ) / dy2
            endif
          enddo

        enddo
      enddo

      return
      end

c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE ZDERIVC                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine computes the derivative in the z direction of
c     a gridded quantity at each grid point.  Only second order,
c     centered derivatives are computed.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine zderivc(q, dqdz, nx, imax, ny, jmax, nz, kmax, dz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions
      integer imax, jmax, kmax ! no. of valid grid points in each direction 
      real dz                  ! grid spacing in z direction (km)
      real q(nx, ny, nz)       ! gridded variable
      real dqdz(nx, ny, nz)    ! z derivative of q
      integer i, j, k          ! grid indices
      real dz2                 ! 2.0*dz

      dz2 = 2.0*dz

      do j=1, jmax
        do i=1, imax

          dqdz(i,j,1) = sbad
          dqdz(i,j,kmax) = sbad

          do k=2, kmax-1
            if ( (q(i,j,k-1).eq.sbad) .or. (q(i,j,k+1).eq.sbad) ) then
              dqdz(i,j,k) = sbad
            else
              dqdz(i,j,k) = ( q(i,j,k+1) - q(i,j,k-1) ) / dz2
            endif
          enddo

        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE HSMTH                   ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This program applies a 5-point horizontal smoother
c     at each horizontal level to the input 3D scalar field
c     
c
c############################################################################
c
c     Author: Steve Weygandt  Dec 1999
c
c
c############################################################################

      subroutine hsmth(fld,nx,ny,nz,npass)

      implicit none

      include 'opaws.inc'

      integer numfld
      integer i,j,k,np,npass
      integer nx,ny,nz
      real fld(nx,ny,nz)
      real temfld(nx,ny)
      real avgfld


      do np = 1,npass

        do k = 1,nz
          do i = 2,nx-1
            do j = 2,ny-1

              numfld = 0
              avgfld = 0.0

              if(fld(i,j,k).ne.sbad) then
                numfld = numfld + 1
                avgfld = avgfld + fld(i,j,k)
              end if

              if(fld(i+1,j,k).ne.sbad) then
                numfld = numfld + 1
                avgfld = avgfld + fld(i+1,j,k)
              end if

              if(fld(i-1,j,k).ne.sbad) then
                numfld = numfld + 1
                avgfld = avgfld + fld(i-1,j,k)
              end if

              if(fld(i,j+1,k).ne.sbad) then
                numfld = numfld + 1
                avgfld = avgfld + fld(i,j+1,k)
              end if

              if(fld(i,j-1,k).ne.sbad) then
                numfld = numfld + 1
                avgfld = avgfld + fld(i,j-1,k)
              end if

              if(numfld.gt.0) then
                temfld(i,j) = avgfld/float(numfld)
              else
                temfld(i,j) = sbad
              end if

            enddo
          enddo
        enddo

        do i = 2,nx-1
          do j = 2,ny-1
            fld(i,j,k) = temfld(i,j)
          enddo
        enddo

      enddo     ! do np = 1,npass

      return
      end



c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE FILL2DD                 ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine fills data voids in a 2-D array.  Dirichlet
c     boundary conditions are used for the poisson equation.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine fill2dd(p,missing,nx,ny,dx,dy,concr)

      implicit none

      include 'opaws.inc'

      real alpha                      ! relaxation parameter
      parameter(alpha=1.35)
      integer nx, ny                  ! number of gridpoints in each direction
      real p(nx, ny)                  ! array to be filled
      integer missing(nx, ny)         ! 1 (0) if the data value is (not) missing
      real dx, dy                     ! grid spacing in each direction (m)
      real concr                      ! convergence criterion
      real pbar                       ! value obtained from Gauss-Seidel soln.
      integer i, j                    ! loop variables
      real cx, cy, c                  ! coefficients in residual equation
      real diff                       ! difference between old and new p
      real maxdiff                    ! maximum difference in the domain
      integer done                    ! 1 (0) if (not) finished with solution
      integer iter                    ! no. of iterations at particular level


      c = 0.5*dx*dx*dy*dy / (dx*dx + dy*dy)
      cx = c / (dx*dx)
      cy = c / (dy*dy)

c     Check boundary values.

      do i=1, nx
        if (p(i,1).eq.sbad) then
          write(*,*) 'fill2dd:  bad value at ', i, 1
          stop
        endif
        if (p(i,ny).eq.sbad) then
          write(*,*) 'fill2dd:  bad value at ', i, ny
          stop
        endif
      enddo
      do j=1, ny
        if (p(1,j).eq.sbad) then
          write(*,*) 'fill2dd:  bad value at ', 1, j
          stop
        endif
        if (p(nx,j).eq.sbad) then
          write(*,*) 'fill2dd:  bad value at ', nx, j
          stop
        endif
      enddo



c     Here is the main loop.

      done=0
      iter=0

      do while (done.eq.0)

        iter = iter + 1
        maxdiff = 0.0
        do i=2, nx-1
          do j=2, ny-1
            if (missing(i,j).eq.1) then
              pbar = cx * ( p(i+1,j)+p(i-1,j) )
     $             + cy * ( p(i,j+1)+p(i,j-1) )
              if (abs(pbar).lt. 1.0E-30) then
                pbar = 0.0         ! try to avoid "underflow" error messages
              endif
              diff = abs(pbar - p(i,j))
              if (diff.gt.maxdiff) maxdiff=diff
              p(i,j) = (1.0-alpha)*p(i,j) + alpha*pbar
            endif
          enddo
        enddo

        if (iter.eq.10000) done=1
        if (maxdiff.le.concr) done=1

      enddo

      write(*,*) 'fill2dd: ', iter, ' iterations'
      write(*,*) 'concr=', concr

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE FILLN                   ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine fills data voids in a 3-D array.  The filling
c     procedure is a least squares method in 2-D (horizontal).  Neumann
c     (zero gradient) boundary conditions are used for the poisson equation.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine filln(pf,nx,ny,nz,dx,dy,concr)

      implicit none

      include 'opaws.inc'

      real alpha                      ! relaxation parameter
      parameter(alpha=1.35)
      integer nx, ny, nz              ! number of gridpoints in each direction
      real pf(nx, ny, nz)             ! array to be filled
      real p(0:nx+1, 0:ny+1, nz)      ! temporary array
      integer missing(nx, ny)         ! 1 (0) if the data value is (not) missing
      real dx, dy                     ! grid spacing in each direction (m)
      real concr                      ! convergence criterion
      real pbar                       ! value obtained from Gauss-Seidel soln.
      integer i, j, k                 ! loop variables
      real cx, cy, c                  ! coefficients in residual equation
      real diff                       ! difference between old and new p
      real maxdiff                    ! maximum difference in the domain
      integer done                    ! 1 (0) if (not) finished with solution
      integer iter                    ! no. of iterations at particular level


      c = 0.5*dx*dx*dy*dy / (dx*dx + dy*dy)
      cx = c / (dx*dx)
      cy = c / (dy*dy)

      do k=1, nz

c       Initialize temporary array.

        do j=1, ny
          do i=1, nx
            if (pf(i,j,k).eq.sbad) then
              p(i,j,k) = 0.0
              missing(i,j) = 1
            else
              p(i,j,k) = pf(i,j,k)
              missing(i,j) = 0
            endif
          enddo
        enddo

c       Here is the main loop.

        done=0
        iter=0

        do while (done.eq.0)

c         Boundary conditions.

          do i=1, nx
            p(i,0,k) = p(i,1,k)
            p(i,ny+1,k) = p(i,ny,k)
          enddo
          do j=1, ny
            p(0,j,k) = p(1,j,k)
            p(nx+1,j,k) = p(nx,j,k)
          enddo

c         Interior of array.

          iter = iter + 1
          maxdiff = 0.0
          do i=1, nx
            do j=1, ny
              if (missing(i,j).eq.1) then
                pbar = cx * ( p(i+1,j,k)+p(i-1,j,k) )
     $               + cy * ( p(i,j+1,k)+p(i,j-1,k) )
                if (abs(pbar).lt. 1.0E-30) then
                  pbar = 0.0         ! try to avoid "underflow" error messages
                endif
                diff = abs(pbar - p(i,j,k))
                if (diff.gt.maxdiff) maxdiff=diff
                p(i,j,k) = (1.0-alpha)*p(i,j,k) + alpha*pbar
              endif
            enddo
          enddo

          if (iter.eq.10000) done=1
          if (maxdiff.le.concr) done=1

        enddo

        write(*,*) 'filln: ', iter, ' iterations, concr=', concr

        do j=1, ny
          do i=1, nx
            pf(i,j,k) = p(i,j,k)
          enddo
        enddo

      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE POISSON2DN                ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine solves the 2-D Poisson equation
c
c                         d2p/dx2 + d2p/dy2 = f
c
c     with Neumann boundary conditions.  All values of f and the boundary
c     conditions must be valid.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  18 August 2003
c
c############################################################################

      subroutine poisson2dn(f, pfinal,
     $                      bcwest, bceast, bcsouth, bcnorth,
     $                      nx, ny, dx, dy, concr)

      implicit none

      include 'opaws.inc'

      real alpha                      ! relaxation parameter
      parameter(alpha=1.35)
      integer nx, ny                  ! number of gridpoints in each direction
      real pfinal(nx, ny)             ! solution of poisson equation
      real p(0:nx+1, 0:ny+1)          ! temporary array
      real f(nx, ny)                  ! forcing function
      real bcwest(ny)                 ! west boundary condition for dp/dx
      real bceast(ny)                 ! east boundary condition for dp/dx
      real bcnorth(nx)                ! north boundary condition for dp/dy
      real bcsouth(nx)                ! south boundary condition for dp/dy
      real dx, dy                     ! grid spacing in each direction (m)
      real concr                      ! convergence criterion
      real pbar                       ! value obtained from Gauss-Seidel soln.
      integer i, j                    ! loop variables
      real cx, cy, c                  ! coefficients in residual equation
      real diff                       ! difference between old and new p
      real maxdiff                    ! maximum difference in the domain
      integer done                    ! 1 (0) if (not) finished with solution
      integer iter                    ! no. of iterations at particular level


      c = 0.5*dx*dx*dy*dy / (dx*dx + dy*dy)
      cx = c / (dx*dx)
      cy = c / (dy*dy)

c     Initialize temporary array.

      p(:,:) = 0.0

c     Here is the main loop.

      done=0
      iter=0

      do while (done.eq.0)

c       Neumann boundary conditions.

        do i=1, nx
          p(i,0) = p(i,2) - 2.0*dy*bcsouth(i)
          p(i,ny+1) = p(i,ny-1) + 2.0*dy*bcnorth(i)
        enddo
        do j=1, ny
          p(0,j) = p(2,j) - 2.0*dx*bcwest(j)
          p(nx+1,j) = p(nx-1,j) + 2.0*dx*bceast(j)
        enddo

c       Interior of array.

        iter = iter + 1
        maxdiff = 0.0
        do i=1, nx
          do j=1, ny
            pbar = cx * ( p(i+1,j)+p(i-1,j) )
     $           + cy * ( p(i,j+1)+p(i,j-1) )
     $           - c*f(i,j)
            if (abs(pbar).lt. 1.0E-30) then
              pbar = 0.0         ! try to avoid "underflow" error messages
            endif
            diff = abs(pbar - p(i,j))
            if (diff.gt.maxdiff) maxdiff=diff
            p(i,j) = (1.0-alpha)*p(i,j) + alpha*pbar
          enddo
        enddo

        if (iter.eq.10000) done=1
        if (maxdiff.le.concr) done=1

      enddo

      write(*,*) 'poisson2dn: iter=', iter, ', concr=', concr,
     $           ', maxdiff=', maxdiff

      do j=1, ny
        do i=1, nx
          pfinal(i,j) = p(i,j)
        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE REMOVEHAVG                ######
c     ######                                                      ######
c     ##################################################################
c     ##################################################################
c
c
c############################################################################
c
c     PURPOSE:
c
c     This subroutine subtracts the horizontal average of q from q
c     at each level in a 3D array.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  18 August 2003
c
c############################################################################

      subroutine removehavg(q, nx, ny, nz)

      implicit none

      integer nx, ny, nz       ! grid dimensions
      real q(nx, ny, nz)       ! gridded variable
      integer i, j, k          ! grid indices
      real qsum                ! sum of values at current level
      real qavg                ! average of q at current level


      do k=1, nz
        qsum = 0.0
        do j=1, ny
          do i=1, nx
            qsum = qsum + q(i,j,k)
          enddo
        enddo
        qavg = qsum / (nx*ny)
        do j=1, ny
          do i=1, nx
            q(i,j,k) = q(i,j,k) - qavg
          enddo
        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                  SUBROUTINE DEFORMATION              ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the following:
c     1.  magnitude of the resultant horizontal deformation
c     2.  x component of horizontal deformation "vector"
c     3.  y component of horizontal deformation "vector" (/s)
c     4.  Dave Schultz' diagnostic quantity:  (def)**2 - (vorz)**2
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  24 November 2004
c
c############################################################################

      subroutine deformation(u, v, w, def, xdef, ydef, defvort,
     $                       nx, ny, nz, dx, dy)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dx, dy              ! grid spacing in each direction (km)
      real dxm, dym            ! grid spacing (m)
      real u(nx,ny,nz)         ! x component of velocity (u) (m/s)
      real v(nx,ny,nz)         ! y component of velocity (v) (m/s)
      real w(nx,ny,nz)         ! z component of velocity (w) (m/s)
      real def(nx,ny,nz)       ! magnitude of the resultant horizontal deformation (/s)
      real xdef(nx,ny,nz)      ! x component of horizontal deformation "vector" (/s)
      real ydef(nx,ny,nz)      ! y component of horizontal deformation "vector" (/s)
      real defvort(nx,ny,nz)   ! Dave Schultz' diagnostic quantity:
                               !   (def)**2 - (vorz)**2  (/s**2)
      real d1, d2, vorz
      integer i, j, k          ! grid indices

      real, allocatable :: dvdx(:,:,:)  ! velocity derivatives
      real, allocatable :: dudy(:,:,:)  ! "                  "
      real, allocatable :: dudx(:,:,:)  ! "                  "
      real, allocatable :: dvdy(:,:,:)  ! "                  "


      dxm = dx * 1000.0
      dym = dy * 1000.0

      allocate(dvdx(nx,ny,nz))
      allocate(dudy(nx,ny,nz))
      allocate(dudx(nx,ny,nz))
      allocate(dvdy(nx,ny,nz))

      call xderivc(v, dvdx, nx, nx, ny, ny, nz, nz, dxm)
      call xderivc(u, dudx, nx, nx, ny, ny, nz, nz, dxm)
      call yderivc(u, dudy, nx, nx, ny, ny, nz, nz, dym)
      call yderivc(v, dvdy, nx, nx, ny, ny, nz, nz, dym)

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if ( (dudx(i,j,k).eq.sbad) .or. (dudy(i,j,k).eq.sbad) .or.
     $           (dvdx(i,j,k).eq.sbad) .or. (dvdy(i,j,k).eq.sbad) ) then
              def(i,j,k) = sbad
              xdef(i,j,k) = sbad
              ydef(i,j,k) = sbad
              defvort(i,j,k) = sbad
            else
              d1 = dudx(i,j,k) - dvdy(i,j,k)
              d2 = dvdx(i,j,k) + dudy(i,j,k)
              vorz = dvdx(i,j,k) - dudy(i,j,k)
              def(i,j,k) = sqrt(d1*d1 + d2*d2)
              xdef(i,j,k) = sqrt(0.5*(def(i,j,k)**2 + d1*def(i,j,k)))
              ydef(i,j,k) = sqrt(0.5*(def(i,j,k)**2 - d1*def(i,j,k)))
              if (d2.lt.0.0) ydef(i,j,k)=-ydef(i,j,k)
              defvort(i,j,k) = def(i,j,k)**2 - vorz**2
            endif
          enddo
        enddo
      enddo

      deallocate(dvdx)
      deallocate(dudx)
      deallocate(dudy)
      deallocate(dvdy)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                  SUBROUTINE VORTICITY                ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the 3-D vorticity.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine vorticity(u, v, w, vorx, vory, vorz,
     $                     nx, ny, nz, dx, dy, dz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dx, dy, dz          ! grid spacing in each direction (km)
      real dxm, dym, dzm       ! grid spacing (m)
      real u(nx,ny,nz)         ! x component of velocity (u) (m/s)
      real v(nx,ny,nz)         ! y component of velocity (v) (m/s)
      real w(nx,ny,nz)         ! z component of velocity (w) (m/s)
      real vorx(nx,ny,nz)      ! x component of vorticity (/s)
      real vory(nx,ny,nz)      ! y component of vorticity (/s)
      real vorz(nx,ny,nz)      ! z component of vorticity (/s)
      integer i, j, k          ! grid indices

      real, allocatable :: dvdx(:,:,:)  ! velocity derivatives
      real, allocatable :: dudy(:,:,:)  ! "                  "
      real, allocatable :: dwdy(:,:,:)  ! "                  "
      real, allocatable :: dvdz(:,:,:)  ! "                  "
      real, allocatable :: dudz(:,:,:)  ! "                  "
      real, allocatable :: dwdx(:,:,:)  ! "                  "


      dxm = dx * 1000.0
      dym = dy * 1000.0
      dzm = dz * 1000.0

      allocate(dvdx(nx,ny,nz))
      allocate(dudy(nx,ny,nz))
      allocate(dwdy(nx,ny,nz))
      allocate(dvdz(nx,ny,nz))
      allocate(dudz(nx,ny,nz))
      allocate(dwdx(nx,ny,nz))

c      call xderivc(v, dvdx, nx, nx, ny, ny, nz, nz, dxm)
c      call yderivc(u, dudy, nx, nx, ny, ny, nz, nz, dym)
c      call xderivc(w, dwdx, nx, nx, ny, ny, nz, nz, dxm)
c      call yderivc(w, dwdy, nx, nx, ny, ny, nz, nz, dym)
c      call zderivc(v, dvdz, nx, nx, ny, ny, nz, nz, dzm)
c      call zderivc(u, dudz, nx, nx, ny, ny, nz, nz, dzm)

      call xderiv(v, dvdx, nx, nx, ny, ny, nz, nz, dxm)
      call yderiv(u, dudy, nx, nx, ny, ny, nz, nz, dym)
      call xderiv(w, dwdx, nx, nx, ny, ny, nz, nz, dxm)
      call yderiv(w, dwdy, nx, nx, ny, ny, nz, nz, dym)
      call zderiv(v, dvdz, nx, nx, ny, ny, nz, nz, dzm)
      call zderiv(u, dudz, nx, nx, ny, ny, nz, nz, dzm)

c      write(*,*) 'IGNORING W SHEAR IN VORTICITY'
c      dwdx(:,:,:) = 0.0
c      dwdy(:,:,:) = 0.0

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if ( (dwdy(i,j,k).eq.sbad) .or.
     $           (dvdz(i,j,k).eq.sbad) ) then
              vorx(i,j,k) = sbad
            else
              vorx(i,j,k) = dwdy(i,j,k) - dvdz(i,j,k)
            endif
            if ( (dudz(i,j,k).eq.sbad) .or.
     $           (dwdx(i,j,k).eq.sbad) ) then
              vory(i,j,k) = sbad
            else
              vory(i,j,k) = dudz(i,j,k) - dwdx(i,j,k)
            endif
            if ( (dvdx(i,j,k).eq.sbad) .or.
     $           (dudy(i,j,k).eq.sbad) ) then
              vorz(i,j,k) = sbad
            else
              vorz(i,j,k) = dvdx(i,j,k) - dudy(i,j,k)
            endif
          enddo
        enddo
      enddo

      deallocate(dvdx)
      deallocate(dudy)
      deallocate(dwdy)
      deallocate(dvdz)
      deallocate(dudz)
      deallocate(dwdx)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE TILTING                 ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes tilting of horizontal vorticity into
c     the vertical.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine tilting(u, v, w, tilt, nx, ny, nz, dx, dy, dz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dx, dy, dz          ! grid spacing in each direction (km)
      real dxm, dym, dzm       ! grid spacing (m)
      real u(nx,ny,nz)         ! u component of velocity (m/s)
      real v(nx,ny,nz)         ! v component of velocity (m/s)
      real w(nx,ny,nz)         ! w component of velocity (m/s)
      real tilt(nx,ny,nz)      ! tilting (/s**2)
      integer i, j, k          ! grid indices

      real, allocatable :: dudz(:,:,:)  ! velocity derivatives
      real, allocatable :: dwdy(:,:,:)  ! "                  "
      real, allocatable :: dvdz(:,:,:)  ! "                  "
      real, allocatable :: dwdx(:,:,:)  ! "                  "


      dxm = dx * 1000.0
      dym = dy * 1000.0
      dzm = dz * 1000.0

      allocate(dudz(nx,ny,nz))
      allocate(dwdy(nx,ny,nz))
      allocate(dvdz(nx,ny,nz))
      allocate(dwdx(nx,ny,nz))

      call zderivc(u, dudz, nx, nx, ny, ny, nz, nz, dzm)
      call yderivc(w, dwdy, nx, nx, ny, ny, nz, nz, dym)
      call zderivc(v, dvdz, nx, nx, ny, ny, nz, nz, dzm)
      call xderivc(w, dwdx, nx, nx, ny, ny, nz, nz, dxm)

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if ( (dudz(i,j,k).eq.sbad) .or.
     $           (dwdy(i,j,k).eq.sbad) .or.
     $           (dvdz(i,j,k).eq.sbad) .or.
     $           (dwdx(i,j,k).eq.sbad) ) then
              tilt(i,j,k) = sbad
            else
              tilt(i,j,k) = dudz(i,j,k)*dwdy(i,j,k)
     $                    - dvdz(i,j,k)*dwdx(i,j,k)
            endif
          enddo
        enddo
      enddo

      deallocate(dudz)
      deallocate(dwdy)
      deallocate(dvdz)
      deallocate(dwdx)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE STRETCHING                 ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes stretching of vertical vorticity.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine stretching(u, v, stretch, nx, ny, nz, dx, dy)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dx, dy              ! grid spacing in each direction (km)
      real dxm, dym            ! grid spacing (m)
      real u(nx,ny,nz)         ! u component of velocity (m/s)
      real v(nx,ny,nz)         ! v component of velocity (m/s)
      real stretch(nx,ny,nz)   ! stretching of vertical vorticity (/s**2)
      integer i, j, k          ! grid indices

      real, allocatable :: dvdx(:,:,:)  ! velocity derivatives
      real, allocatable :: dudy(:,:,:)  ! "                  "
      real, allocatable :: dudx(:,:,:)  ! "                  "
      real, allocatable :: dvdy(:,:,:)  ! "                  "


      dxm = dx * 1000.0
      dym = dy * 1000.0

      allocate(dvdx(nx,ny,nz))
      allocate(dudy(nx,ny,nz))
      allocate(dudx(nx,ny,nz))
      allocate(dvdy(nx,ny,nz))

      call xderivc(u, dudx, nx, nx, ny, ny, nz, nz, dxm)
      call xderivc(v, dvdx, nx, nx, ny, ny, nz, nz, dxm)
      call yderivc(u, dudy, nx, nx, ny, ny, nz, nz, dym)
      call yderivc(v, dvdy, nx, nx, ny, ny, nz, nz, dym)

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if ( (dudx(i,j,k).eq.sbad) .or.
     $           (dvdx(i,j,k).eq.sbad) .or.
     $           (dudy(i,j,k).eq.sbad) .or.
     $           (dvdy(i,j,k).eq.sbad) ) then
              stretch(i,j,k) = sbad
            else
              stretch(i,j,k) = - (dvdx(i,j,k)-dudy(i,j,k))
     $                          *(dudx(i,j,k)+dvdy(i,j,k))
            endif
          enddo
        enddo
      enddo

      deallocate(dvdx)
      deallocate(dudy)
      deallocate(dudx)
      deallocate(dvdy)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                  SUBROUTINE ADVECTION                ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the 3-D advection of q.  A regular
c     (not staggered) grid is assumed.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  15 August 2003
c
c############################################################################

      subroutine advection(q, qadv, u, v, w, nx, ny, nz, dx, dy, dz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dx, dy, dz          ! grid spacing in each direction (m)
      real u(nx,ny,nz)         ! x component of velocity (u) (m/s)
      real v(nx,ny,nz)         ! y component of velocity (v) (m/s)
      real w(nx,ny,nz)         ! z component of velocity (w) (m/s)
      real q(nx,ny,nz)         ! advected quantity
      real qadv(nx,ny,nz)      ! advection of q
      real dqdx(nx,ny,nz)      ! spatial derivative of q
      real dqdy(nx,ny,nz)      ! "                     "
      real dqdz(nx,ny,nz)      ! "                     "
      integer i, j, k          ! grid indices


      call xderiv(q, dqdx, nx, nx, ny, ny, nz, nz, dx)
      call yderiv(q, dqdy, nx, nx, ny, ny, nz, nz, dy)
      call zderiv(q, dqdz, nx, nx, ny, ny, nz, nz, dz)

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if ( (dqdx(i,j,k).eq.sbad) .or.
     $           (dqdy(i,j,k).eq.sbad) .or.
     $           (dqdz(i,j,k).eq.sbad) .or.
     $           (u(i,j,k).eq.sbad) .or.
     $           (v(i,j,k).eq.sbad) .or.
     $           (w(i,j,k).eq.sbad) ) then
              qadv(i,j,k) = sbad
            else
              qadv(i,j,k) = -u(i,j,k)*dqdx(i,j,k)
     $                     - v(i,j,k)*dqdy(i,j,k)
     $                     - w(i,j,k)*dqdz(i,j,k)
            endif
          enddo
        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                  SUBROUTINE MEANWIND                 ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine outputs the mean horizontal wind at each level.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine meanwind(u, v, nx, ny, nz, dz, zmin)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz       ! grid dimensions 
      real dz                  ! vertical grid spacing (km)
      real zmin                ! height of lowest grid level (km)
      real u(nx,ny,nz)         ! u component of velocity (m/s)
      real v(nx,ny,nz)         ! v component of velocity (m/s)
      integer i, j, k          ! grid indices
      integer i1, i2, j1, j2   ! horizontal limits
      real usum, vsum          ! sums of u and v at each level
      integer nsum             ! number of summed values


      i1 = 1
      i2 = nx
      j1 = 1
      j2 = ny

      write(20,*)
      write(20,*) 'MEAN WINDS AT EACH LEVEL'
      write(20,*)
      write(20,*) 'i1, i2 = ', i1, i2
      write(20,*) 'j1, j2 = ', j1, j2
      write(20,*)

      do k=1, nz

        nsum = 0
        usum = 0.0
        vsum = 0.0

        do j=j1, j2
          do i=i1, i2
            if ( (u(i,j,k).ne.sbad) .and. (v(i,j,k).ne.sbad) ) then
              usum = usum + u(i,j,k)
              vsum = vsum + v(i,j,k)
              nsum = nsum + 1
            endif
          enddo
        enddo

        if (nsum.eq.0) then
          write(20,98) zmin+(k-1.0)*dz
 98       format('z = ', F7.2, '   NO DATA')
        else
          write(20,99) zmin+(k-1.0)*dz, usum/nsum, vsum/nsum
 99       format('z = ', F7.2, '     U = ', F7.2, ', V = ', F7.2)
        endif

      enddo

      write(20,*)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE LL_TO_XY                ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the projected (x, y) coordinates of the
c     point (lat2, lon2) relative to (lat1, lon1).  Various map projections
c     are possible.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  25 February 2005
c
c############################################################################

      subroutine ll_to_xy(x, y, map_proj, lat1, lon1, lat2, lon2)

      implicit none

      include 'opaws.inc'

c---- Passed variables

      real lat1, lon1             ! coordinates of first point (rad)
      real lat2, lon2             ! coordinates of second point (rad)
      integer map_proj            ! map projection:
                                  !   0 = flat earth
                                  !   1 = oblique azimuthal
                                  !   2 = Lambert conformal

c---- Returned variables

      real x, y                   ! distance (km)


      if (map_proj.eq.0) then
        x = rearth * cos(0.5*(lat1+lat2)) * (lon2-lon1)
        y = rearth * (lat2-lat1)
      else
        write(*,*) 'map projection unavailable:  ', map_proj
        stop
      endif

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE XY_TO_LL                ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the projected (lat, lon) coordinates of the
c     point (x, y) relative to (lat0, lon0).  Various map projections
c     are possible.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  25 February 2005
c
c############################################################################

      subroutine xy_to_ll(lat, lon, map_proj, x, y, lat0, lon0)

      implicit none

      include 'opaws.inc'

c---- Passed variables

      integer map_proj            ! map projection:
                                  !   0 = flat earth
                                  !   1 = oblique azimuthal
                                  !   2 = Lambert conformal
      real x, y                   ! distance (km)
      real lat0, lon0             ! coordinates (rad) of origin (where x=0, y=0)

c---- Returned variables

      real lat, lon               ! coordinates (rad) of point


      if (map_proj.eq.0) then
        lat = lat0 + y / rearth
        lon = lon0 + x / ( rearth * cos(0.5*(lat0+lat)) )
      else
        write(*,*) 'map projection unavailable:  ', map_proj
        stop
      endif

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                   SUBROUTINE DIST                    ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine computes the approximate distance (along a curved
c     path following the earth's surface) from (lon1, lat1, alt1)
c     to (lon2, lat2, alt2).
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  17 December 2001
c
c############################################################################

      subroutine dist(x, y, z, lon1, lat1, alt1, lon2, lat2, alt2)

      implicit none

      include 'opaws.inc'

      real x, y, z                     ! distance (km)
      real lon1, lon2                  ! longitude (deg)
      real lat1, lat2                  ! latitude (deg)
      real alt1, alt2                  ! altitude (km MSL)

      x = 2.0*pi*rearth * cos(0.5*(lat1+lat2)*dtor)
     $                  * (lon2-lon1)/360.0
      y = 2.0*pi*rearth * (lat2-lat1)/360.0
      z = alt2 - alt1

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE CORRECT_AZ_EL               ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutine accomplishes three things:
c     1. adds a user specified correction to the elevation angles
c     2. adds a user specified correction to the azimuth angles
c     3. adjusts the beam locations to correspond to the middle of the
c        integration period rather than the end
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  5 March 2003
c
c############################################################################

      subroutine correct_az_el(ryib, num_rays, azcor, elcor, az_corr_flag)

      implicit none

      include 'opaws.inc'
      include 'structures.inc'

      type(ryib_info), dimension(maxrays) :: ryib    ! ray info. block
      integer num_rays      ! number of rays
      real azcor            ! user specified azimuth angle correction (deg)
      real elcor            ! user specified elevation angle correction (deg)
      integer r             ! ray number
      real azcompx          ! x component of azimuth direction
      real azcompy          ! y component of azimuth direction
      real comptoaz         ! function used by this subroutine
      real oldaz, oldel     ! previous values of az and el
      integer az_corr_flag  ! method of additional azimuth-angle correction
                            !   0 = none
                            !   1 = average current and previous angle


      if (num_rays.lt.3) then
        write(*,*) 'correct_az_el:  not enough rays'
        write(*,*) 'num_rays = ', num_rays
        stop
      endif

c     Add the user specified corrections.

      do r=1, num_rays
        ryib(r)%azimuth = ryib(r)%azimuth + azcor
        if (ryib(r)%azimuth .gt. 360.0) then
          ryib(r)%azimuth = ryib(r)%azimuth - 360.0
        endif
        if (ryib(r)%azimuth .lt. 0.0) then
          ryib(r)%azimuth = ryib(r)%azimuth + 360.0
        endif
        ryib(r)%elevation = ryib(r)%elevation + elcor
        if (ryib(r)%elevation .gt. 180.0) then
          ryib(r)%elevation = ryib(r)%elevation - 360.0
        endif
      enddo

      if (az_corr_flag.eq.1) then

c       To determine the middle location of beam r, average
c       the end locations of beams r and r-1.

        do r=num_rays, 2, -1
          azcompx = 0.5*sin(dtor*ryib(r)%azimuth)
     $            + 0.5*sin(dtor*ryib(r-1)%azimuth)
          azcompy = 0.5*cos(dtor*ryib(r)%azimuth)
     $            + 0.5*cos(dtor*ryib(r-1)%azimuth)
          ryib(r)%azimuth = comptoaz(azcompx, azcompy)
          ryib(r)%elevation = 0.5*ryib(r)%elevation
     $                      + 0.5*ryib(r-1)%elevation
        enddo

c       We can't determine exactly the middle location of the first beam.
c       Try extrapolating from the locations of the second and
c       third beams.

        oldaz = ryib(1)%azimuth
        oldel = ryib(1)%elevation

        azcompx = 2.0*sin(dtor*ryib(2)%azimuth)
     $          - 1.0*sin(dtor*ryib(3)%azimuth)
        azcompy = 2.0*cos(dtor*ryib(2)%azimuth)
     $          - 1.0*cos(dtor*ryib(3)%azimuth)
        ryib(1)%azimuth = comptoaz(azcompx, azcompy)
        ryib(1)%elevation = 2.0*ryib(2)%elevation
     $                    - 1.0*ryib(3)%elevation

        if ( (abs(oldaz-ryib(1)%azimuth).gt.1.0) .or.
     $       (abs(oldel-ryib(1)%elevation).gt.1.0) ) then
          write(*,*) '*** warning from correct_az_el about ',
     $               'extrapolated beam location'
          write(*,*) 'old azimuth and elevation: ', oldaz, oldel
          write(*,*) 'new azimuth and elevation: ',
     $                ryib(1)%azimuth, ryib(1)%elevation
        endif

      endif        ! if (az_corr_flag.eq.1)

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 REAL FUNCTION COMPTOAZ               ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This function computes the azimuth angle from the two components
c     of the beam direction.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  5 March 2003
c
c############################################################################

      real function comptoaz(azcompx, azcompy)

      implicit none

      include 'opaws.inc'

      real azcompx        ! x component of azimuth direction
      real azcompy        ! y component of azimuth direction

      if ( (azcompx.eq.0.0) .and.
     $     (azcompy.eq.0.0) ) then
        comptoaz = sbad
      else if (azcompy.eq.0.0) then
        if (azcompx.gt.0.0) then
          comptoaz = 90.0
        else
          comptoaz = 270.0
        endif
      else if ( (azcompx.ge.0.0) .and.
     $          (azcompy.gt.0.0) ) then
        comptoaz = atan(azcompx/azcompy)
     $           * rtod
      else if ( (azcompx.ge.0.0) .and.
     $          (azcompy.lt.0.0) ) then
        comptoaz = -atan(azcompx
     $                   /abs(azcompy))
     $              * rtod + 180.0
      else if ( (azcompx.lt.0.0) .and.
     $          (azcompy.lt.0.0) ) then
        comptoaz = atan(azcompx/azcompy)
     $             * rtod + 180.0
      else
        comptoaz = -atan(abs(azcompx)
     $                   /azcompy)
     $              * rtod + 360.0
      endif

      return
      end

c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 FUNCTION COMPUTE_HEIGHT              ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This function returns the height (km, relative to the grid origin)
c     of the point with radar-relative coordinates (x, y, el).
c
c############################################################################
c
c     Author:  unknown
c
c############################################################################
      REAL FUNCTION COMPUTE_HEIGHT(x, y, el, ralt, galt)

        include 'opaws.inc'

        real, intent(in)  :: x, y, el, ralt, galt
        real              :: r, h

! r is the slant path range to the point.  

        r = sqrt( x**2 + y**2) / cos(dtor*el)
      
! observation height and great-circle distance [Doviak and Zrnic 1993, p. 21]

        h              = sqrt( r*r + eer*eer + 2.0*r*eer*sin(dtor*el) ) - eer
        compute_height = h + ralt - galt

      RETURN
      END FUNCTION

c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 FUNCTION COMPUTE_AZ                  ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This function computes the azimuth angle from the horizontal components
c     of the beam direction.
c
c############################################################################
c
c     Author:  unknown
c
c############################################################################
      REAL FUNCTION COMPUTE_AZ(x, y)

        include 'opaws.inc'

        real, intent(in)  :: x, y
        real              :: pii, rxy

        pii = 4.0*atan(1.0)
        rxy = sqrt( x**2 + y**2)
      
        az = sbad
        el = sbad
      
        IF( x == 0.0 .and. y == 0.0 ) THEN

            az = 0.0
      
        ELSE IF ( y == 0.0 ) THEN

            IF( x > 0.0 ) THEN
                az = 0.5*pii
            ELSE
                az = 1.5*pii
            ENDIF
      
        ELSE IF( x >= 0.0 .and. y > 0.0 ) THEN

            az = atan(x/y)
      
        ELSE IF( x >= 0.0 .and. y < 0.0 ) THEN

            az = -atan(x/abs(y)) + pii
      
        ELSE IF( x < 0.0 .and. y < 0.0 ) THEN

            az = atan(x/y) + pii
      
        ELSE
            az = -atan(abs(x)/y) + 2.0*pii

        ENDIF

        IF( az .ne. sbad ) az = az * 180. / pii

        compute_az = az

        RETURN
        END FUNCTION

c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE COMPUTE_AZ_EL             ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This function computes the azimuth and elevation angles from the
c     three components of the beam direction.
c
c############################################################################
c
c     Author:  unknown
c
c############################################################################
      subroutine compute_az_el(x, y, z, el, az)

        include 'opaws.inc'

        real, intent(in)  :: x, y, z
        real, intent(out) :: el, az
        real              :: pii, rxy

        pii = 4.0*atan(1.0)
        rxy     = sqrt( x**2 + y**2)
      
        az = sbad
        el = sbad
      
        IF( x == 0.0 .and. y == 0.0 ) THEN

            az = 0.0
      
        ELSE IF ( y == 0.0 ) THEN

            IF( x > 0.0 ) THEN
                az = 0.5*pii
            ELSE
                az = 1.5*pii
            ENDIF
      
        ELSE IF( x >= 0.0 .and. y > 0.0 ) THEN

            az = atan(x/y)
      
        ELSE IF( x >= 0.0 .and. y < 0.0 ) THEN

            az = -atan(x/abs(y)) + pii
      
        ELSE IF( x < 0.0 .and. y < 0.0 ) THEN

            az = atan(x/y) + pii
      
        ELSE
            az = -atan(abs(x)/y) + 2.0*pii

        ENDIF
      
        IF( rxy == 0.0 ) THEN

          IF( z > 0.0 ) THEN
              el = 0.5*pii
          ELSE IF( z <= 0.0 ) THEN
              el = -0.5*pii
          ENDIF

        ELSE

!         el = asin( ((z + eer)**2 - rxy**2 - eer**2) / (2.*rxy*eer))
          el = atan(z/rxy)

        ENDIF
      
        IF( az .ne. sbad ) az = az * pii / 180.
        IF( el .ne. sbad ) el = el * pii / 180.

        RETURN
        END


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE CORRECT_TIME               ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     Correct the time fields in the input radar data.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  January 2006
c
c############################################################################

      subroutine correct_time(julian_day, jdcor, hour, hrcor, minute, mncor, second, secor)

      implicit none

      integer(kind=4) julian_day            ! julian day
      integer(kind=2) hour, minute, second  ! time
      integer jdcor                         ! correction to julian day
      integer hrcor                         ! correction to hour
      integer mncor                         ! correction to minute
      integer secor                         ! correction to second

      julian_day = julian_day + jdcor
      hour = hour + hrcor
      minute = minute + mncor
      second = second + secor

      do while (second.lt.0)
        second = second + 60
        minute = minute - 1
      enddo
      do while (second.gt.59)
        second = second - 60
        minute = minute + 1
      enddo

      do while (minute.lt.0)
        minute = minute + 60
        hour = hour - 1
      enddo
      do while (minute.gt.59)
        minute = minute - 60
        hour = hour + 1
      enddo

      do while (hour.lt.0)
        hour = hour + 24
        julian_day = julian_day - 1
      enddo
      do while (hour.gt.23)
        hour = hour - 24
        julian_day = julian_day + 1
      enddo

      if ( (julian_day .lt. 1) .or. (julian_day .gt. 366) ) then
        write(*,*) 'correct_time aborting -- invalid value of julian_day:  ', julian_day
        stop
      endif

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######             SUBROUTINE CORRECT_UMASS_DATA            ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     This subroutines tries to correct some of the errors in UMass radar data.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  15 February 2005
c
c############################################################################

      subroutine correct_umass_data(num_rays, ryib, total_gates, rdat)

      implicit none

      include 'opaws.inc'
      include 'structures.inc'

      integer num_rays
      integer total_gates
      type(ryib_info), dimension(maxrays) :: ryib
      type(rdat_info), dimension(maxrays) :: rdat
      integer r, g

c     Corrections:
c     1. Elevation angles are not known, and the values that are stored in
c        the radar data are meaningless.  For now, assume the elevation
c        angle should be 0.5 degrees.
c     2. Bad/missing data are represented by many different flags in UMass radar
c        data.  For now, assume any number <= -60.0 represents bad data.

      write(*,*) 'correcting UMass radar data...'

      do r=1, num_rays
        ryib(r)%elevation = 0.5
        do g=1, total_gates
          if (rdat(r)%data(g) .le. -60.00) then
            rdat(r)%data(g) = sbad
          endif
        enddo
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE GRID_COORDINATES            ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     Fill the array "x" with the Cartesian coordinates corresponding
c     to the input parameters.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  February 2005
c
c############################################################################

      subroutine grid_coordinates(x, nx, dx, xmin)

      implicit none

      integer nx           ! number of grid points
      real x(nx)           ! grid coordinates
      real dx              ! distance between grid points
      real xmin            ! x(1)
      integer i

      do i=1, nx
        x(i) = xmin+(i-1.0)*dx
      enddo

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE CHECK_SWEEP_SIZE            ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     Determine whether array bounds "maxflds" and "maxrays" are large
c     enough to store the input sweep file.  If they are not, then halt.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  February 2005
c
c############################################################################

      subroutine check_sweep_size(num_param_desc, num_rays)

      implicit none

      include 'opaws.inc'

      integer(kind=2) num_param_desc
      integer(kind=4) num_rays

      if (num_param_desc .gt. maxflds) then
        write(*,*) 'too many fields in sweep file'
        write(*,*) 'num_param_desc = ', num_param_desc
        write(*,*) 'maxflds = ', maxflds
        stop
      endif
      if (num_rays .gt. maxrays) then
        write(*,*) 'too many rays in sweep file'
        write(*,*) 'num_rays = ', num_rays
        write(*,*) 'maxrays = ', maxrays
        stop
      endif

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######              REAL FUNCTION RANGEKM_TO_GATE           ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     Return the range, in km, to the center of the current gate.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  February 2005
c
c############################################################################

      real function rangekm_to_gate(celv, g)

      implicit none

      include 'opaws.inc'
      include 'structures.inc'

      type(celv_info) :: celv    ! information about ranges, in m, to the
                                 !   near edges of the gates
      integer g                  ! current gate number


      if ( (celv%total_gates.le.1) .or. (g.lt.1) .or. (g.gt.celv%total_gates) ) then
        rangekm_to_gate = sbad
      else if (g.eq.celv%total_gates) then
        rangekm_to_gate = ( celv%gate_spacing(g)
     $                     +0.5*( celv%gate_spacing(g)
     $                           -celv%gate_spacing(g-1) ) )
     $                  / 1000.0
      else
        rangekm_to_gate = 0.5 * ( celv%gate_spacing(g)
     $                           +celv%gate_spacing(g+1) )
     $                  / 1000.0
      endif

      return
      end


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE UPDATE_DIR_BINS             ######
c     ######                                                      ######
c     ##################################################################
c
c
c     PURPOSE:
c
c     Update the runnings sums of weights for each directional bin.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Creation Date:  February 2005
c
c############################################################################

      subroutine update_dir_bins(sum, wgt, x, y, z, xg, yg, zg)

      implicit none

      real sum(8)
      real wgt
      real x, y, z
      real xg, yg, zg

      if ( (x.le.xg) .and. (y.le.yg) .and. (z.le.zg) ) sum(1) = sum(1) + wgt
      if ( (x.le.xg) .and. (y.le.yg) .and. (z.ge.zg) ) sum(2) = sum(2) + wgt
      if ( (x.le.xg) .and. (y.ge.yg) .and. (z.le.zg) ) sum(3) = sum(3) + wgt
      if ( (x.le.xg) .and. (y.ge.yg) .and. (z.ge.zg) ) sum(4) = sum(4) + wgt
      if ( (x.ge.xg) .and. (y.le.yg) .and. (z.le.zg) ) sum(5) = sum(5) + wgt
      if ( (x.ge.xg) .and. (y.le.yg) .and. (z.ge.zg) ) sum(6) = sum(6) + wgt
      if ( (x.ge.xg) .and. (y.ge.yg) .and. (z.le.zg) ) sum(7) = sum(7) + wgt
      if ( (x.ge.xg) .and. (y.ge.yg) .and. (z.ge.zg) ) sum(8) = sum(8) + wgt

      return
      end


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE XYZLOC                    ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This subroutine computes the projected (x, y, z) coordinates of a
c     radar observation, relative to the grid origin.  The observation
c     location is adjusted for storm motion.
c
c############################################################################
c
c     Author:  David Dowell
c
c     Created:  December 2001
c
c     Modified:  February 2005
c
c############################################################################

      subroutine xyzloc(x, y, z, r, az0, el0, 
     $                  map_proj, glat, glon, galt, rlat, rlon, ralt,
     $                  ut, vt, ti)

      implicit none

      include 'opaws.inc'

c---- Passed variables

      real r                   ! slant-path range (km) to observation
      real az0                 ! azimuth angle (rad) at radar
      real el0                 ! elevation angle (rad) at radar
      integer map_proj         ! map projection:
                               !   0 = flat earth
                               !   1 = oblique azimuthal
                               !   2 = Lambert conformal
      real glat, glon          ! latitude and longitude of grid origin (rad)
      real galt                ! altitude of grid origin (km MSL)
      real rlat, rlon          ! radar latitude and longitude (rad)
      real ralt                ! radar altitude (km MSL)
      real ti                  ! time (s) relative to central time
      real ut, vt              ! storm translation velocity (m/s)

c---- Returned variables

      real x, y, z             ! location of gate relative to grid origin (km)

c---- Local variables

      real h                   ! height (km) of observation relative to radar
      real s                   ! great-circle distance (km) along spherical earth
      real d                   ! great-circle distance, expressed in radians
      real lat, lon            ! observation latitude and longitude (rad)
      real dlon                ! longitude difference (rad)


c     observation height and great-circle distance [Doviak and Zrnic 1993, p. 21]

      h = sqrt( r*r + eer*eer + 2.0*r*eer*sin(el0) ) - eer
      z = h + ralt - galt
      s = eer * asin( (r*cos(el0)) / (eer+h) )
      d = s / rearth

c     observation (lat, lon) [E. Williams, "Aviation Formulary V1.42"]

      lat  = asin(sin(rlat)*cos(d) + cos(rlat)*sin(d)*cos(az0))
      dlon = atan2(sin(az0)*sin(d)*cos(rlat), cos(d)-sin(rlat)*sin(lat))
      lon  = mod(rlon+dlon+pi, 2.0*pi) - pi

c     observation (x, y)

      call ll_to_xy(x, y, map_proj, glat, glon, lat, lon)

c     storm-motion adjustment

      x = x - ti*(ut/1000.0)
      y = y - ti*(vt/1000.0)

      return
      end


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                REAL FUNCTION TLINT_OLD               ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function returns the tri-linearly interpolated value of a scalar
c     quantity at the given (x,y,z) location.
c
c############################################################################

      real function tlint_old(v, x, y, z, nx, ny, nz, dx, dy, dz,
     $                    xlsw, ylsw, zlsw)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz                 ! number of gridpoints in x, y, and
                                         !   z directions
      real v(nx,ny,nz)                   ! scalar array
      real x, y, z                       ! parcel location
      real dx, dy, dz                    ! grid spacings in x/y/z
      real xlsw, ylsw, zlsw              ! coordinates of lower southwest
                                         !   corner of grid
      integer i, j, k                    ! indices of lower southwest corner of
                                         !   gridbox in which parcel is located
      real xi, yj, zk                    ! coordinates corresponding to these
                                         !   indices
      real xb, yb, zb                    ! coordinates of parcel with respect
                                         !   to grid box corner
      real q1, q2, vb, vt                ! temporary quantities

      i = aint(1.0 + (x-xlsw)/dx )
      j = aint(1.0 + (y-ylsw)/dy )
      k = aint(1.0 + (z-zlsw)/dz )
      xi = xlsw + (i-1)*dx
      yj = ylsw + (j-1)*dy
      zk = zlsw + (k-1)*dz
      xb = x - xi
      yb = y - yj
      zb = z - zk

      if ((i.ge.1).and.(j.ge.1).and.(k.ge.1).and.
     $    (i.lt.nx).and.(j.lt.ny).and.(k.lt.nz)) then
        if ((v(i,j,k).eq.sbad) .or. (v(i+1,j,k).eq.sbad) .or.
     $      (v(i+1,j+1,k).eq.sbad) .or. (v(i,j+1,k).eq.sbad) .or.
     $      (v(i,j,k+1).eq.sbad) .or. (v(i+1,j,k+1).eq.sbad) .or.
     $      (v(i+1,j+1,k+1).eq.sbad) .or. (v(i,j+1,k+1).eq.sbad)) then
          tlint_old = sbad
        else
          q1 = ((dx-xb)*v(i,j,k)+xb*v(i+1,j,k)) / (dx*dy)
          q2 = ((dx-xb)*v(i,j+1,k)+xb*v(i+1,j+1,k)) / (dx*dy)
          vb = q1*(dy-yb) + q2*yb
          q1 = ((dx-xb)*v(i,j,k+1)+xb*v(i+1,j,k+1)) / (dx*dy)
          q2 = ((dx-xb)*v(i,j+1,k+1)+xb*v(i+1,j+1,k+1)) / (dx*dy)
          vt = q1*(dy-yb) + q2*yb
          tlint_old = (vb*(dz-zb)+vt*zb)/dz
        endif
      else
        tlint_old = sbad
      endif

      return
      end


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                REAL FUNCTION TLINT                   ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function returns the tri-linearly interpolated value of a scalar
c     quantity at the given (x,y,z) location.
c
c############################################################################

      real function tlint(v, x, y, z, nx, ny, nz, xg, yg, zg, sbad)

      implicit none

      integer nx, ny, nz                 ! number of gridpoints in x, y, and z directions
      real v(nx,ny,nz)                   ! scalar array
      real x, y, z                       ! parcel location
      real xg(nx), yg(ny), zg(nz)        ! analysis grid coordinates
      integer i, j, k                    ! indices near parcel location
      real xi, yj, zk                    ! coordinates corresponding to these indices
      real xb, yb, zb                    ! coordinates of parcel with respect to grid box corner
      real q1, q2, vb, vt                ! temporary quantities
      real dx, dy, dz
      real sbad
      integer find_index

      i  = find_index(x, xg, nx)
      j  = find_index(y, yg, ny)
      k  = find_index(z, zg, nz)
      dx = xg(i+1) - xg(i)
      dy = yg(j+1) - yg(j)
      dz = zg(k+1) - zg(k)
      xi = xg(i)
      yj = yg(j)
      zk = zg(k)
      xb = x - xi
      yb = y - yj
      zb = z - zk

      IF ((i.ge.1).and.(j.ge.1).and.(k.ge.1).and.
     $    (i.lt.nx).and.(j.lt.ny).and.(k.lt.nz)) THEN
        IF ((v(i,  j,  k)  .eq.sbad) .or. (v(i+1,j,  k)  .eq.sbad) .or.
     $      (v(i+1,j+1,k)  .eq.sbad) .or. (v(i,  j+1,k)  .eq.sbad) .or.
     $      (v(i,  j,  k+1).eq.sbad) .or. (v(i+1,j,  k+1).eq.sbad) .or.
     $      (v(i+1,j+1,k+1).eq.sbad) .or. (v(i,  j+1,k+1).eq.sbad)) THEN
          tlint = sbad
        ELSE
          q1 = ((dx-xb)*v(i,j,k)  +xb*v(i+1,j,k))   / (dx*dy)
          q2 = ((dx-xb)*v(i,j+1,k)+xb*v(i+1,j+1,k)) / (dx*dy)
          vb = q1*(dy-yb) + q2*yb
          q1 = ((dx-xb)*v(i,j,k+1)  +xb*v(i+1,j,k+1))   / (dx*dy)
          q2 = ((dx-xb)*v(i,j+1,k+1)+xb*v(i+1,j+1,k+1)) / (dx*dy)
          vt = q1*(dy-yb) + q2*yb
          tlint = (vb*(dz-zb)+vt*zb)/dz
        ENDIF
      ELSE
        tlint = sbad
      ENDIF

      return
      end

c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                REAL FUNCTION TLINT2D                 ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function returns the bi-linearly interpolated value of a scalar
c     quantity at the given (x,y) location.
c
c############################################################################

      real function tlint2d(v, x, y, nx, ny, xg, yg, sbad)

      implicit none

      integer nx, ny                     ! number of gridpoints in x and y directions
      real v(nx,ny)                      ! scalar array
      real x, y                          ! parcel location
      real xg(nx), yg(ny)                ! location of analysis grid
      integer i, j                       ! indices near parcel location
      real xi, yj                        ! coordinates corresponding to these indices
      real xb, yb                        ! coordinates of parcel with respect to grid box corner
      real q1, q2                        ! temporary quantities
      real dx, dy
      real sbad
      integer find_index

      i  = find_index(x, xg, nx)
      j  = find_index(y, yg, ny)
      dx = xg(i+1) - xg(i)
      dy = yg(j+1) - yg(j)
      xi = xg(i)
      yj = yg(j)
      xb = x - xi
      yb = y - yj

      IF ((i.ge.1).and.(j.ge.1).and.(i.lt.nx).and.(j.lt.ny)) THEN
        IF ((v(i,  j  ).eq.sbad) .or. (v(i+1,j)  .eq.sbad) .or.
     $      (v(i+1,j+1).eq.sbad) .or. (v(i,  j+1).eq.sbad)) THEN
          tlint2d = sbad
        ELSE
          q1 = ((dx-xb)*v(i,j  )+xb*v(i+1,j  )) / dx
          q2 = ((dx-xb)*v(i,j+1)+xb*v(i+1,j+1)) / dx
          tlint2d = (q1*(dy-yb) + q2*yb)/dy
C          write(51,*) 'x', xg(i), x, xg(i+1), dx
C          write(51,*) 'y', yg(j), y, yg(j+1), dy
C          write(51,*) 'Q1/Q2/TLINT2D:  ', q1, q2, tlint2d
        ENDIF
      ELSE
        tlint2d = sbad
      ENDIF

      RETURN
      END


c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######              REAL FUNCTION TIMEDIFF                  ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function computes the difference (in seconds) between
c     two times (time1 - time2).
c
c     Note:  the possiblity of a leap year is not considered.
c
c############################################################################

      real function timediff(y1, mo1, d1, h1, mi1, s1, ms1,
     $                       y2, mo2, d2, h2, mi2, s2, ms2)

      implicit none

      include 'opaws.inc'

      integer(kind=2) y1, y2                ! year
      integer(kind=2) mo1, mo2              ! month
      integer(kind=2) d1, d2                ! day
      integer(kind=2) h1, h2                ! hour
      integer(kind=2) mi1, mi2              ! minute
      integer(kind=2) s1, s2                ! second
      integer(kind=2) ms1, ms2              ! milliseconds
      real(kind=8) deltat
      integer jd1, jd2                      ! julian days


      jd1 = d1 + pd(mo1)
      jd2 = d2 + pd(mo2)

      deltat = 31536000.0 * (y1-y2)
      deltat = deltat + 86400.0 * (jd1-jd2)
      deltat = deltat + 3600.0 * (h1-h2)
      deltat = deltat + 60.0 * (mi1-mi2)
      deltat = deltat + (s1-s2)
      deltat = deltat + 0.001*(ms1-ms2)

      timediff = deltat

      return
      end

c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######              REAL FUNCTION TIMEDIFF4                 ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function computes the difference (in seconds) between
c     two times (time1 - time2).
c
c     Note:  the possiblity of a leap year is not considered.
c
c     TIMEDIFF4 does the time diff for 4-byte integers
c
c############################################################################

      real function timediff4(y1, mo1, d1, h1, mi1, s1, ms1,
     $                        y2, mo2, d2, h2, mi2, s2, ms2)

      implicit none

      include 'opaws.inc'

      integer y1, y2                ! year
      integer mo1, mo2              ! month
      integer d1, d2                ! day
      integer h1, h2                ! hour
      integer mi1, mi2              ! minute
      integer s1, s2                ! second
      integer ms1, ms2              ! milliseconds
      real(kind=8) deltat
      integer jd1, jd2              ! julian days

      jd1 = d1 + pd(mo1)
      jd2 = d2 + pd(mo2)

      deltat = 31536000.0 * (y1-y2)
      deltat = deltat + 86400.0 * (jd1-jd2)
      deltat = deltat + 3600.0 * (h1-h2)
      deltat = deltat + 60.0 * (mi1-mi2)
      deltat = deltat + (s1-s2)
      deltat = deltat + 0.001*(ms1-ms2)

      timediff4 = deltat

      return
      end

c############################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######              REAL FUNCTION TIMEDIFF_JD               ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function computes the difference (in seconds) between
c     two times (time1 - time2).
c
c     Note:  the possiblity of a leap year is not considered.
c
c############################################################################

      real function timediff_jd(jd1, h1, mi1, s1, ms1,
     $                          jd2, h2, mi2, s2, ms2)

      implicit none

      integer(kind=4) jd1, jd2              ! julian day
      integer(kind=2) h1, h2                ! hour
      integer(kind=2) mi1, mi2              ! minute
      integer(kind=2) s1, s2                ! second
      integer(kind=2) ms1, ms2              ! milliseconds
      real(kind=8) deltat

      deltat = 86400.0 * (jd1-jd2)
      deltat = deltat + 3600.0 * (h1-h2)
      deltat = deltat + 60.0 * (mi1-mi2)
      deltat = deltat + (s1-s2)
      deltat = deltat + 0.001*(ms1-ms2)

      timediff_jd = deltat

      return
      end


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######               SUBROUTINE ADJUST_DATE                 ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     INPUTS:  year, month, day, hour, min, sec, and offset in seconds
c     OUTPUTS  year, month, day, hour, min, sec
c
c############################################################################
c
c     Author:  Lou Wicker Feb 2010
c
c     major revision by David Dowell Jan 2011
c
c############################################################################

      subroutine adjust_date(year, month, day, hour, minute, second, offset)

      implicit none

!---- returned variables
      integer   :: year, month, day, hour, minute, second

!---- passed variables
      integer(kind=8) :: offset                   ! offset, in seconds of input 

!---- local variables
      integer   :: prev
      integer(kind=8) :: sec_offset, min_offset, hour_offset, day_offset
      logical   :: leap
      integer   :: days_this_month, days_per_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
      integer(kind=8), parameter :: long_sec = 60, long_day = 24


      sec_offset = offset
      
!      print*, 'second before = ', second
!      print*, 'sec_offset before = ', sec_offset

      prev = second
      second = modulo( (second + sec_offset), long_sec )
      sec_offset = sec_offset + prev - second

!      print*, 'second after = ', second
!      print*, 'sec_offset after = ', sec_offset

      min_offset = sec_offset / 60

!      print*, 'minute before = ', minute
!      print*, 'min_offset before = ', min_offset

      prev = minute
      minute = modulo( (minute + min_offset), long_sec )
      min_offset = min_offset + prev - minute

!      print*, 'minute after = ', minute
!      print*, 'min_offset after = ', min_offset

      hour_offset = min_offset / 60

!      print*, 'hour before = ', hour
!      print*, 'hour_offset before = ', hour_offset

      prev = hour
      hour = modulo( (hour + hour_offset), long_day )
      hour_offset = hour_offset + prev - hour

!      print*, 'hour after = ', hour
!      print*, 'hour_offset after = ', hour_offset

      day_offset = hour_offset / 24

!      print*, 'day before = ', day
!      print*, 'day_offset before = ', day_offset

      DO WHILE (day_offset .ne. 0)

        IF (day_offset .gt. 0) THEN
          day = day + 1
          day_offset = day_offset - 1
        ENDIF
        IF (day_offset .lt. 0) THEN
          day = day - 1
          day_offset = day_offset + 1
        ENDIF

        ! Note:  The Gregorian calendar assigns each year evenly
        ! divisible by 4 that is not a century year unevenly divisible by 400
        ! as a leap-year. (i.e. 1700,1800,1900 are not leap-years, 2000 is)

        IF (day .lt. 1) THEN

          month = month - 1
          IF (month .lt. 1) THEN
            month = 12
            year = year - 1
            day = days_per_month(12)
          ELSE
            days_this_month = days_per_month(month)
            IF (month == 2) THEN
              leap=(modulo(year,4).eq.0)
              IF((modulo(year,100).eq.0).and.(modulo(year,400).ne.0))THEN
                leap=.false.
              ENDIF
              IF(leap) days_this_month = 29
            ENDIF
            day = days_this_month
          ENDIF

        ELSE 

          days_this_month = days_per_month(month)
          IF (month == 2) THEN
            leap=(modulo(year,4).eq.0)
            IF((modulo(year,100).eq.0).and.(modulo(year,400).ne.0))THEN
              leap=.false.
            ENDIF
            IF(leap) days_this_month = 29
          ENDIF
          IF (day .gt. days_this_month) THEN
            month = month + 1
            day = 1
            IF (month .gt. 12) THEN
              month = 1
              year = year + 1
            ENDIF
          ENDIF

        ENDIF

      END DO

!      print *, "NEW YR/MN/DAY/HR/MIN/SEC:  ", year, month, day, hour, minute, second

      return
      end subroutine adjust_date


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######           SUBROUTINE SET_DATE_GREGORIAN              ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     Computes time corresponding to date for gregorian calendar.
c
c############################################################################
c
c     Author:  Data Assimilation Research Testbed -- DART
C              Data Assimilation Initiative, University Corporation for Atmospheric Research
c
c     Created:  September 9, 2004
c
c     Modified:  May 31, 2005 (David Dowell)
c
c############################################################################

      subroutine set_date_gregorian(days, secs, year, month, day, hours, minutes, seconds)

      implicit none

!---- returned variables

      integer :: days                  ! Gregorian day since beginning of base year
      integer :: secs                  ! seconds since beginning of day

!---- input variables

      integer :: day, month, year
      integer :: seconds, minutes, hours

!---- local variables

      integer(kind=4) :: julian_day
      integer :: nleapyr
      integer :: base_year = 1601


      call set_julian_day(julian_day, year, month, day)

      ! Need to check for bogus times

      if ( seconds > 59 .or. seconds  < 0 .or.
     $     minutes > 59 .or. minutes  < 0 .or.
     $     hours   > 23 .or. hours    < 0 ) then
        write(*,*) 'set_date_gregorian:  ',
     $             seconds,minutes,hours,
     $             ' not a valid time'
        stop
      endif

      ! Compute number of leap years fully past since base_year

      nleapyr = (year - base_year) / 4 - (year - base_year) / 100 + (year - base_year) / 400

      secs = seconds + 60*(minutes + 60 * hours)
      days = julian_day - 1 + 365*(year - base_year - nleapyr) + 366*(nleapyr)

      return
      end subroutine set_date_gregorian

c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######           SUBROUTINE SET_DATE_GREGORIAN_JD           ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     Computes time corresponding to date for gregorian calendar.
c
c############################################################################
c
c     Author:  Data Assimilation Research Testbed -- DART
C              Data Assimilation Initiative, University Corporation for Atmospheric Research
c
c     Created:  September 9, 2004
c
c     Modified:  January 19, 2011 (David Dowell)
c
c############################################################################

      subroutine set_date_gregorian_jd(days, secs, year, julian_day, hours, minutes, seconds)

      implicit none

!---- returned variables

      integer :: days                  ! Gregorian day since beginning of base year
      integer :: secs                  ! seconds since beginning of day

!---- input variables

      integer :: year, julian_day
      integer :: seconds, minutes, hours

!---- local variables

      integer :: nleapyr
      integer :: base_year = 1601


      ! Need to check for bogus times

      if ( seconds    > 59 .or.  seconds    < 0 .or.
     $     minutes    > 59 .or.  minutes    < 0 .or.
     $     hours      > 23 .or.  hours      < 0 .or.
     $     julian_day > 366 .or. julian_day < 0) then
        write(*,*) 'set_date_gregorian_jd:  ',
     $             seconds,minutes,hours,julian_day,
     $             ' not a valid time'
        stop
      endif

      ! Compute number of leap years fully past since base_year

      nleapyr = (year - base_year) / 4 - (year - base_year) / 100 + (year - base_year) / 400

      secs = seconds + 60*(minutes + 60 * hours)
      days = julian_day - 1 + 365*(year - base_year - nleapyr) + 366*(nleapyr)

      return
      end subroutine set_date_gregorian_jd


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######              SUBROUTINE SET_JULIAN_DAY               ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     Computes julian day corresponding to input year, month, and day
c
c############################################################################
c
c     Author:  Data Assimilation Research Testbed -- DART
c              Data Assimilation Initiative, University Corporation for Atmospheric Research
c
c     Created:  September 9, 2004
c
c     Modified:  January 28, 2006 (David Dowell)
c
c############################################################################

      subroutine set_julian_day(julian_day, year, month, day)

      implicit none

!---- returned variables

      integer(kind=4) julian_day

!---- input variables

      integer day, month, year

!---- local variables

      integer :: base_year = 1601
      integer :: ndays, m
      logical :: leap
      integer :: days_per_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)


      ! Need to check for bogus dates

      if (                   day      < 1 .or.
     $     month   > 12 .or. month    < 1 .or.
     $                       year     < base_year ) then
        write(*,*) 'set_julian_day:  ', day,month,year,
     $             ' not a valid date'
        stop
      endif

      if (month /= 2 .and. day > days_per_month(month)) then
         write(*,*) 'month (',month,') does not have ',day,' days'
         stop
      endif

      ! Is this a leap year? Gregorian calandar assigns each year evenly
      ! divisible by 4 that is not a century year unevenly divisible by 400
      ! as a leap-year. (i.e. 1700,1800,1900 are not leap-years, 2000 is)

      leap=(modulo(year,4).eq.0)
      if((modulo(year,100).eq.0).and.(modulo(year,400).ne.0))then
       leap=.false.
      endif

      ! Finish checking for day specification errors

      if (month == 2 .and. (day > 29 .or. ((.not. leap) .and. day > 28))) then
        write(*,*) 'month (',month,') does not have ',
     $             day,' days in a lon-leap year'
        stop
      endif

      ! Count up days in this year

      ndays = 0
      do m=1,month-1
       ndays = ndays + days_per_month(m)
       if(leap .and. m == 2) ndays = ndays + 1
      enddo
      julian_day = ndays + day

      return
      end subroutine set_julian_day


c############################################################################
c
c     Author:  Data Assimilation Research Testbed -- DART
C              Data Assimilation Initiative, University Corporation for Atmospheric Research
c
c     Created:  September 9, 2004
c
c     Modified:  May 31, 2005 (David Dowell)
c
c############################################################################

      subroutine get_date_gregorian(days, secs, year, month, day, hour, minute, second)

      implicit none

!---- returned variables

      integer :: day, month, year
      integer :: second, minute, hour

!---- input variables

      integer :: days                  ! Gregorian day since beginning of base year
      integer :: secs                  ! seconds since beginning of day

!---- local variables

      integer :: num_days, m
      integer :: base_year = 1601
      integer :: iyear, days_this_year, days_this_month, t
      logical :: leap
      integer :: days_per_month(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)

! Do this the inefficient inelegant way for now, top is 10000 years

      num_days = days
      do iyear = base_year, 10000

        ! Is this a leap year? Gregorian calendar assigns each year evenly
        ! divisible by 4 that is not a century year unevenly divisible by 400
        ! as a leap-year. (i.e. 1700,1800,1900 are not leap-years, 2000 is)

         leap=(modulo(iyear,4).eq.0)
         if((modulo(iyear,100).eq.0).and.(modulo(iyear,400).ne.0))then
            leap=.false.
         endif

         if(leap) then
            days_this_year = 366
         else
            days_this_year = 365
         endif

         if(num_days >= days_this_year) then
            num_days = num_days - days_this_year
         else
            year = iyear
            goto 111
         endif

      end do

 111  continue

      ! find month and day
      do m = 1, 12
         month = m
         days_this_month = days_per_month(m)
         if(leap .and. m == 2) days_this_month = 29
         if(num_days < days_this_month) exit
         num_days = num_days - days_this_month
      end do

      day = num_days + 1

      ! Find hour,minute and second
      t      = secs
      hour   = t / (60 * 60)
      t      = t - hour * (60 * 60)
      minute = t / 60
      second = t - 60 * minute

      return
      end subroutine get_date_gregorian


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                SUBROUTINE CLUTTER_MASK               ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This subroutine defines a clutter mask based on information in
c     an output file from the "clutter_stats" program.
c
c     Original version:  David Dowell, March 2010
c
c     Revision:  David Dowell, July 2011
c     - modified to read observation-space rather than grid-space statistics
c
c############################################################################

      SUBROUTINE CLUTTER_MASK(nr, na, ne, clutter, r_coord, a_coord, e_coord, mean_refl,
     $                        min_fixed_clutter_freq, min_moving_clutter_freq,
     $                        halo, min_obs, min_sweeps, ncfile)

      implicit none

      include 'opaws.inc'

c     input variables

      integer nr                       ! number of range bins
      integer na                       ! number of azimuth-angle bins
      integer ne                       ! number of elevation-angle bins
      real    min_fixed_clutter_freq   ! minimum frequency (0.0 - 1.0) of fixed clutter from clutter_stats that will be considered clutter
      real    min_moving_clutter_freq  ! minimum frequency (0.0 - 1.0) of moving clutter from clutter_stats that will be considered clutter
      integer halo                     ! number of additional neighboring bins in each direction to search for maximum in clutter frequency
      integer min_obs                  ! minimum number of observations for identifying clutter
      integer min_sweeps               ! minimum number of sweeps for identifying clutter
      character(LEN=100) ncfile        ! netcdf file that contains clutter-mask information (output from clutter_stats)

c     returned variables

      logical clutter(nr,na,ne)        ! .true. if (range, azimuth, elevation) bin has been determined to be contaminated by ground clutter
      real r_coord(nr+1)               ! range coordinates of bin edges (km)
      real a_coord(na+1)               ! azimuth angle coordinates of bin edges (deg)
      real e_coord(ne+1)               ! elevation angle coordinates of bin edges (deg)
      real mean_refl(nr,na,ne)         ! mean reflectivity (dBZ) for (range, azimuth, elevation) bin

c     local variables
      integer r, a, e, rr, aa
      real, allocatable :: freq_fixed_clutter(:,:,:)  ! frequency of gates in (range, azimuth, elevation) bin that were detected as fixed clutter
      real, allocatable :: freq_moving_clutter(:,:,:) ! frequency of gates in (range, azimuth, elevation) bin that were detected as moving clutter
      integer, allocatable :: total_gates(:,:,:)      ! number of gates used for computing statistics in (range, azimuth, elevation) bin
      integer, allocatable :: total_sweeps(:,:,:)     ! number of sweeps used for computing statistics in (range, azimuth, elevation) bin


      write(6,*) "CLUTTER_MASK:  allocating arrays..."
      allocate(freq_fixed_clutter(nr, na, ne))
      allocate(freq_moving_clutter(nr, na, ne))
      allocate(total_gates(nr, na, ne))
      allocate(total_sweeps(nr, na, ne))

      write(6,*) "CLUTTER_MASK:  reading clutter stats..."
      call READ_NETCDF_CLUTTER_STATS(ncfile, nr, na, ne, r_coord, a_coord, e_coord,
     $                               freq_fixed_clutter, freq_moving_clutter,
     $                               total_gates, total_sweeps, mean_refl)

      write(6,*) "CLUTTER_MASK:  defining clutter mask..."

      clutter(:,:,:) = .false.

      do e=1, ne
        do a=1, na
          do r=1, nr

            if ( (freq_fixed_clutter(r,a,e).ge.min_fixed_clutter_freq) .and.
     $           (total_gates(r,a,e).ge.min_obs) .and.
     $           (total_sweeps(r,a,e).ge.min_sweeps) ) then
              do aa=max(1,a-halo), min(na,a+halo)
                do rr=max(1,r-halo), min(nr,r+halo)
                  clutter(rr,aa,e) = .true.
                enddo
              enddo
            endif

            if ( (freq_moving_clutter(r,a,e).ge.min_moving_clutter_freq) .and.
     $           (total_gates(r,a,e).ge.min_obs) .and.
     $           (total_sweeps(r,a,e).ge.min_sweeps) ) then
              do aa=max(1,a-halo), min(na,a+halo)
                do rr=max(1,r-halo), min(nr,r+halo)
                  clutter(rr,aa,e) = .true.
                enddo
              enddo
            endif

          enddo
        enddo
      enddo

      deallocate(freq_fixed_clutter)
      deallocate(freq_moving_clutter)
      deallocate(total_gates)
      deallocate(total_sweeps)

      return
      end




c############################################################################
c
c from http://www.fortran.com/quick_sort2.f
c
      SUBROUTINE SORTRX(N,DATA,INDEX)
C===================================================================
C
C     SORTRX -- SORT, Real input, indeX output
C
C
C     Input:  N     INTEGER
C             DATA  REAL
C
C     Output: INDEX INTEGER (DIMENSION N)
C
C This routine performs an in-memory sort of the first N elements of
C array DATA, returning into array INDEX the indices of elements of
C DATA arranged in ascending order.  Thus,
C
C    DATA(INDEX(1)) will be the smallest number in array DATA;
C    DATA(INDEX(N)) will be the largest number in DATA.
C
C The original data is not physically rearranged.  The original order
C of equal input values is not necessarily preserved.
C
C===================================================================
C
C SORTRX uses a hybrid QuickSort algorithm, based on several
C suggestions in Knuth, Volume 3, Section 5.2.2.  In particular, the
C "pivot key" [my term] for dividing each subsequence is chosen to be
C the median of the first, last, and middle values of the subsequence;
C and the QuickSort is cut off when a subsequence has 9 or fewer
C elements, and a straight insertion sort of the entire array is done
C at the end.  The result is comparable to a pure insertion sort for
C very short arrays, and very fast for very large arrays (of order 12
C micro-sec/element on the 3081K for arrays of 10K elements).  It is
C also not subject to the poor performance of the pure QuickSort on
C partially ordered data.
C
C Created:  15 Jul 1986  Len Moss
C
C===================================================================
 
      INTEGER   N,INDEX(N)
      REAL      DATA(N)
 
      INTEGER   LSTK(31),RSTK(31),ISTK
      INTEGER   L,R,I,J,P,INDEXP,INDEXT
      REAL      DATAP
 
C     QuickSort Cutoff
C
C     Quit QuickSort-ing when a subsequence contains M or fewer
C     elements and finish off at end with straight insertion sort.
C     According to Knuth, V.3, the optimum value of M is around 9.
 
      INTEGER   M
      PARAMETER (M=9)
 
C===================================================================
C
C     Make initial guess for INDEX
 
      DO 50 I=1,N
         INDEX(I)=I
   50    CONTINUE
 
C     If array is short, skip QuickSort and go directly to
C     the straight insertion sort.
 
      IF (N.LE.M) GOTO 900
 
C===================================================================
C
C     QuickSort
C
C     The "Qn:"s correspond roughly to steps in Algorithm Q,
C     Knuth, V.3, PP.116-117, modified to select the median
C     of the first, last, and middle elements as the "pivot
C     key" (in Knuth's notation, "K").  Also modified to leave
C     data in place and produce an INDEX array.  To simplify
C     comments, let DATA[I]=DATA(INDEX(I)).
 
C Q1: Initialize
      ISTK=0
      L=1
      R=N
 
  200 CONTINUE
 
C Q2: Sort the subsequence DATA[L]..DATA[R].
C
C     At this point, DATA[l] <= DATA[m] <= DATA[r] for all l < L,
C     r > R, and L <= m <= R.  (First time through, there is no
C     DATA for l < L or r > R.)
 
      I=L
      J=R
 
C Q2.5: Select pivot key
C
C     Let the pivot, P, be the midpoint of this subsequence,
C     P=(L+R)/2; then rearrange INDEX(L), INDEX(P), and INDEX(R)
C     so the corresponding DATA values are in increasing order.
C     The pivot key, DATAP, is then DATA[P].
 
      P=(L+R)/2
      INDEXP=INDEX(P)
      DATAP=DATA(INDEXP)
 
      IF (DATA(INDEX(L)) .GT. DATAP) THEN
         INDEX(P)=INDEX(L)
         INDEX(L)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
      IF (DATAP .GT. DATA(INDEX(R))) THEN
         IF (DATA(INDEX(L)) .GT. DATA(INDEX(R))) THEN
            INDEX(P)=INDEX(L)
            INDEX(L)=INDEX(R)
         ELSE
            INDEX(P)=INDEX(R)
         ENDIF
         INDEX(R)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
C     Now we swap values between the right and left sides and/or
C     move DATAP until all smaller values are on the left and all
C     larger values are on the right.  Neither the left or right
C     side will be internally ordered yet; however, DATAP will be
C     in its final position.
 
  300 CONTINUE
 
C Q3: Search for datum on left >= DATAP
C
C     At this point, DATA[L] <= DATAP.  We can therefore start scanning
C     up from L, looking for a value >= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         I=I+1
         IF (DATA(INDEX(I)).LT.DATAP) GOTO 300
 
  400 CONTINUE
 
C Q4: Search for datum on right <= DATAP
C
C     At this point, DATA[R] >= DATAP.  We can therefore start scanning
C     down from R, looking for a value <= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         J=J-1
         IF (DATA(INDEX(J)).GT.DATAP) GOTO 400
 
C Q5: Have the two scans collided?
 
      IF (I.LT.J) THEN
 
C Q6: No, interchange DATA[I] <--> DATA[J] and continue
 
         INDEXT=INDEX(I)
         INDEX(I)=INDEX(J)
         INDEX(J)=INDEXT
         GOTO 300
      ELSE
 
C Q7: Yes, select next subsequence to sort
C
C     At this point, I >= J and DATA[l] <= DATA[I] == DATAP <= DATA[r],
C     for all L <= l < I and J < r <= R.  If both subsequences are
C     more than M elements long, push the longer one on the stack and
C     go back to QuickSort the shorter; if only one is more than M
C     elements long, go back and QuickSort it; otherwise, pop a
C     subsequence off the stack and QuickSort it.
 
         IF (R-J .GE. I-L .AND. I-L .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=J+1
            RSTK(ISTK)=R
            R=I-1
         ELSE IF (I-L .GT. R-J .AND. R-J .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=L
            RSTK(ISTK)=I-1
            L=J+1
         ELSE IF (R-J .GT. M) THEN
            L=J+1
         ELSE IF (I-L .GT. M) THEN
            R=I-1
         ELSE
C Q8: Pop the stack, or terminate QuickSort if empty
            IF (ISTK.LT.1) GOTO 900
            L=LSTK(ISTK)
            R=RSTK(ISTK)
            ISTK=ISTK-1
         ENDIF
         GOTO 200
      ENDIF
 
  900 CONTINUE
 
C===================================================================
C
C Q9: Straight Insertion sort
 
      DO 950 I=2,N
         IF (DATA(INDEX(I-1)) .GT. DATA(INDEX(I))) THEN
            INDEXP=INDEX(I)
            DATAP=DATA(INDEXP)
            P=I-1
  920       CONTINUE
               INDEX(P+1) = INDEX(P)
               P=P-1
               IF (P.GT.0) THEN
                  IF (DATA(INDEX(P)).GT.DATAP) GOTO 920
               ENDIF
            INDEX(P+1) = INDEXP
         ENDIF
  950    CONTINUE
 
C===================================================================
C
C     All done
 
      END


c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######                 SUBROUTINE FALLSPEED                 ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This subroutine computes an estimate of the precipitation fallspeed
c     from an empirial relationship involving reflectivity and density.
c
c############################################################################

      subroutine fallspeed(wt, rf, rho, nx, ny, nz)

      implicit none

      include 'opaws.inc'

      integer nx, ny, nz        ! no. of grid points in x, y, and z directions
      real rf(nx,ny,nz)         ! reflectivity (dBZ)
      real rho(nz)              ! density (kg m**-3)
      real wt(nx,ny,nz)         ! precipitation terminal fall speed (m/s)
      integer i, j, k           ! grid indices

      do k=1, nz
        do j=1, ny
          do i=1, nx
            if (rf(i,j,k).eq.sbad) then
              wt(i,j,k) = sbad
            else
              wt(i,j,k) = -2.6 * (10.0**(0.1*rf(i,j,k)))**0.107
     $                  * (1.2/rho(k))**0.4
            endif
          enddo
        enddo
      enddo

      return
      end

c###########################################################################
c
c     ##################################################################
c     ######                                                      ######
c     ######             INTEGER FUNCTION FIND_INDEX              ######
c     ######                                                      ######
c     ##################################################################
c
c     PURPOSE:
c
c     This function returns the array index (here, the value returned by
c     find_index is designated as i) such that x is between xa(i) and xa(i+1).
c     If x is less than xa(1), then i=0 is returned.  If x is greater than
c     xa(n), then i=n is returned.  It is assumed that the values of
c     xa increase monotonically with increasing i.
c
c############################################################################
c
c     Author:  David Dowell (based on "locate" algorithm in Numerical Recipes)
c
c     Creation Date:  17 November 2004
c
c############################################################################

      integer function find_index(x, xa, n)

      implicit none

      integer n                          ! array size
      real xa(n)                         ! array of locations
      real x                             ! location of interest
      integer il, im, iu                 ! lower and upper limits, and midpoint


      il = 0
      iu = n+1
      
      IF ( x .gt. xa(n) ) THEN
        il = n+2
      ELSEIF ( x .lt. xa(1) ) THEN
        il = -1
      ELSE

 10   if ((iu-il).gt.1) then
        im=(il+iu)/2
        if (x.ge.xa(im)) then
          il=im
        else
          iu=im
        endif
        go to 10
      endif
      
      ENDIF

c      if ( (il.eq.0) .and. (n.gt.1) .and.
c           ( (xa(1)-x) .lt. 0.001*(xa(2)-xa(1)) ) ) then
c        il = 1
c      endif
c      if ( (il.eq.n) .and. (n.gt.1) .and.
c           ( (x-xa(n)) .lt. 0.001*(xa(n)-xa(n-1)) ) ) then
c        il = n-1
c      endif

      find_index = il

      return
      end
