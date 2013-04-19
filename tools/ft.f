*========================================================================
* File Name : ft.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 17-11-2010
* Last Modified : Mon 28 Feb 2011 04:39:18 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================
      subroutine discrete_fourier_transform_0(f,F_w_real,F_w_image)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/corr.h"
      include "../include/tester/fourier.h"
      include "../include/BIMD/BIMD.h"
      real*8 x_temp1,x_temp2
      real*8 t(-observe_time_step_max:observe_time_step_max)
      real*8 w(-observe_time_step_max:observe_time_step_max)
      real*8 f(-observe_time_step_max:observe_time_step_max),
     &F_w_real(-observe_time_step_max:observe_time_step_max),
     &F_w_image(-observe_time_step_max:observe_time_step_max)
C      pi=dacos(-1.D0)
C      Please input pi first,pi is a common real number in vacf.h
      x_temp1=2.D0*pi/dble(2*corr_observe_time_step+1)
      do I0=-corr_observe_time_step,corr_observe_time_step
        x_temp2=dble(I0)*x_temp1
        do I1=-corr_observe_time_step,corr_observe_time_step
          F_w_image(I0)=F_w_image(I0)+f(I1)*dsin(-x_temp2*t(I1))
          F_w_real(I0)=F_w_real(I0)+f(I1)*dcos(-x_temp2*t(I1))
        enddo
        F_w_real(I0)=F_w_real(I0)*delta_time
        F_w_image(I0)=F_w_image(I0)*delta_time
      enddo
      return
      end

      subroutine DFT(f,total_f_num,w_real,w_img)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/corr.h"
      include "../include/tester/fourier.h"
      include "../include/BIMD/BIMD.h"
      integer total_f_num
      real*8 theta,theta_t
      real*8 f(-observe_time_step_max:observe_time_step_max),
     &w_real(-observe_time_step_max:observe_time_step_max),
     &w_img(-observe_time_step_max:observe_time_step_max)
C      pi=dacos(-1.D0)
      theta=2.D0*pi/dble(2.D0*total_f_num+1)
      do I0=-(total_f_num-1),total_f_num-1
        theta_t=dble(I0)*theta
        w_img(I0)=0.D0
        w_real(I0)=0.D0
        do I1=-(total_f_num-1),total_f_num-1
          w_img(I0)=w_img(I0)+f(I1)*dsin(-theta_t*dble(I1))
          w_real(I0)=w_real(I0)+f(I1)*dcos(-theta_t*dble(I1))
        enddo
      enddo
      return
      end


 
      SUBROUTINE four1(data,nn,isign)
C     nn must be power of 2
      INTEGER isign,nn
      REAL data(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2

2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
            tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep

      goto 2
      endif
      return
      end

      SUBROUTINE fourn(data,nn,ndim,isign)
      INTEGER isign,ndim,nn(ndim)
      REAL data(*)
      INTEGER i1,i2,i2rev,i3,i3rev,ibit,idim,ifp1,ifp2,ip1,ip2,ip3,k1,
     *k2,n,nprev,nrem,ntot
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      ntot=1
      do 11 idim=1,ndim
        ntot=ntot*nn(idim)
11    continue
      nprev=1
      do 18 idim=1,ndim
        n=nn(idim)
        nrem=ntot/(n*nprev)
        ip1=2*nprev
        ip2=ip1*n
        ip3=ip2*nrem
        i2rev=1
        do 14 i2=1,ip2,ip1
          if(i2.lt.i2rev)then

            do 13 i1=i2,i2+ip1-2,2
              do 12 i3=i1,ip3,ip2
                i3rev=i2rev+i3-i2
                tempr=data(i3)
                tempi=data(i3+1)
                data(i3)=data(i3rev)
                data(i3+1)=data(i3rev+1)
                data(i3rev)=tempr
                data(i3rev+1)=tempi
12            continue
13          continue
          endif
          ibit=ip2/2
1         if ((ibit.ge.ip1).and.(i2rev.gt.ibit)) then
            i2rev=i2rev-ibit
            ibit=ibit/2
          goto 1
          endif

          i2rev=i2rev+ibit
14      continue
        ifp1=ip1
2       if(ifp1.lt.ip2)then
          ifp2=2*ifp1
          theta=isign*6.28318530717959d0/(ifp2/ip1)
          wpr=-2.d0*sin(0.5d0*theta)**2
          wpi=sin(theta)
          wr=1.d0
          wi=0.d0
          do 17 i3=1,ifp1,ip1
            do 16 i1=i3,i3+ip1-2,2
              do 15 i2=i1,ip3,ifp2
                k1=i2
                k2=k1+ifp1
                tempr=sngl(wr)*data(k2)-sngl(wi)*data(k2+1)
                tempi=sngl(wr)*data(k2+1)+sngl(wi)*data(k2)
                data(k2)=data(k1)-tempr

                data(k2+1)=data(k1+1)-tempi
                data(k1)=data(k1)+tempr
                data(k1+1)=data(k1+1)+tempi
15            continue
16          continue
            wtemp=wr
            wr=wr*wpr-wi*wpi+wr
            wi=wi*wpr+wtemp*wpi+wi
17        continue
          ifp1=ifp2
        goto 2
        endif
        nprev=n*nprev
18    continue
      return
      END

      SUBROUTINE realft(data,n,isign)
      INTEGER isign,n
      REAL data(n)
CU    USES four1
      INTEGER i,i1,i2,i3,i4,n2p3
      REAL c1,c2,h1i,h1r,h2i,h2r,wis,wrs
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/dble(n/2)
      c1=0.5
      if (isign.eq.1) then
        c2=-0.5
        call four1(data,n/2,+1)
      else
        c2=0.5
        theta=-theta
      endif
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      wr=1.0d0+wpr
      wi=wpi
      n2p3=n+3
      do 11 i=2,n/4
        i1=2*i-1
        i2=i1+1
        i3=n2p3-i2
        i4=i3+1
        wrs=sngl(wr)

        wis=sngl(wi)
        h1r=c1*(data(i1)+data(i3))
        h1i=c1*(data(i2)-data(i4))
        h2r=-c2*(data(i2)+data(i4))
        h2i=c2*(data(i1)-data(i3))
        data(i1)=h1r+wrs*h2r-wis*h2i
        data(i2)=h1i+wrs*h2i+wis*h2r
        data(i3)=h1r-wrs*h2r+wis*h2i
        data(i4)=-h1i+wrs*h2i+wis*h2r
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
11    continue
      if (isign.eq.1) then
        h1r=data(1)
        data(1)=h1r+data(2)
        data(2)=h1r-data(2)
      else
        h1r=data(1)
        data(1)=c1*(h1r+data(2))

        data(2)=c1*(h1r-data(2))
        call four1(data,n/2,-1)
      endif
      return
      END

      SUBROUTINE sinft(y,n)
      INTEGER n
      REAL y(n)
CU    USES realft
      INTEGER j
      REAL sum,y1,y2
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/dble(n)
      wr=1.0d0
      wi=0.0d0
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      y(1)=0.0
      do 11 j=1,n/2
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
        y1=wi*(y(j+1)+y(n-j+1))
        y2=0.5*(y(j+1)-y(n-j+1))
        y(j+1)=y1+y2
        y(n-j+1)=y1-y2
11    continue
      call realft(y,n,+1)
      sum=0.0
      y(1)=0.5*y(1)
      y(2)=0.0
      do 12 j=1,n-1,2
        sum=sum+y(j)
        y(j)=y(j+1)
        y(j+1)=sum

12    continue
      return
      END

      SUBROUTINE cosft1(y,n)
      INTEGER n
      REAL y(n+1)
CU    USES realft
      INTEGER j
      REAL sum,y1,y2
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=3.141592653589793d0/n
      wr=1.0d0
      wi=0.0d0
      wpr=-2.0d0*sin(0.5d0*theta)**2
      wpi=sin(theta)
      sum=0.5*(y(1)-y(n+1))
      y(1)=0.5*(y(1)+y(n+1))
      do 11 j=1,n/2-1
        wtemp=wr
        wr=wr*wpr-wi*wpi+wr
        wi=wi*wpr+wtemp*wpi+wi
        y1=0.5*(y(j+1)+y(n-j+1))
        y2=(y(j+1)-y(n-j+1))
        y(j+1)=y1-wi*y2
        y(n-j+1)=y1+wi*y2
        sum=sum+wr*y2
11    continue
      call realft(y,n,+1)
      y(n+1)=y(2)
      y(2)=sum
      do 12 j=4,n,2

        sum=sum+y(j)
        y(j)=sum
12    continue
      return
      END

      SUBROUTINE cosft2(y,n,isign)
      INTEGER isign,n
      REAL y(n)
CU    USES realft
      INTEGER i
      REAL sum,sum1,y1,y2,ytemp
      DOUBLE PRECISION theta,wi,wi1,wpi,wpr,wr,wr1,wtemp,PI
      PARAMETER (PI=3.141592653589793d0)
      theta=0.5d0*PI/n
      wr=1.0d0
      wi=0.0d0
      wr1=cos(theta)
      wi1=sin(theta)
      wpr=-2.0d0*wi1**2
      wpi=sin(2.d0*theta)
      if(isign.eq.1)then
        do 11 i=1,n/2
          y1=0.5*(y(i)+y(n-i+1))
          y2=wi1*(y(i)-y(n-i+1))
          y(i)=y1+y2
          y(n-i+1)=y1-y2

          wtemp=wr1
          wr1=wr1*wpr-wi1*wpi+wr1
          wi1=wi1*wpr+wtemp*wpi+wi1
11      continue
        call realft(y,n,1)
        do 12 i=3,n,2
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
          y1=y(i)*wr-y(i+1)*wi
          y2=y(i+1)*wr+y(i)*wi
          y(i)=y1
          y(i+1)=y2
12      continue
        sum=0.5*y(2)
        do 13 i=n,2,-2
          sum1=sum
          sum=sum+y(i)
          y(i)=sum1
13      continue
      else if(isign.eq.-1)then
        ytemp=y(n)
        do 14 i=n,4,-2
          y(i)=y(i-2)-y(i)

14      continue
        y(2)=2.0*ytemp
        do 15 i=3,n,2
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
          y1=y(i)*wr+y(i+1)*wi
          y2=y(i+1)*wr-y(i)*wi
          y(i)=y1
          y(i+1)=y2
15      continue
        call realft(y,n,-1)
        do 16 i=1,n/2
          y1=y(i)+y(n-i+1)
          y2=(0.5/wi1)*(y(i)-y(n-i+1))
          y(i)=0.5*(y1+y2)
          y(n-i+1)=0.5*(y1-y2)
          wtemp=wr1
          wr1=wr1*wpr-wi1*wpi+wr1
          wi1=wi1*wpr+wtemp*wpi+wi1
16      continue
      endif
      return
      END

* ======================GNU General Public License=======================
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
* =======================================================================
