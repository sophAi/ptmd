*========================================================================
* File Name : moment_usr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時05分47秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      SUBROUTINE moment(data_in,n,skip_id,ave,adev,sdev,var,skew,curt)
C===============================
C     data_in:input data of dimension n
C     n:dimension of the data
C     ave: 1st moment or mean value of the data
C     adev: average deviation sum(|x-ave|)/N
C     sdev: standard deviation sqrt(sum(x-ave)^2)=sqrt(var)
C     var: 2nd moment or variation. The width of the distribution
C     skew: 3rd moment or skewness. The asymmetirc tail toward +x or -x.
C     Should be >sqrt(15/N) for safty.
C     curt: 4th moment or Kurtosis. Relative peakedness and flatness to
C     normal distribution. Positive case is leptokurtic and negative is
C     platykurtic. The in-between is called mesokurtic.
      integer n,skip_id
      real*8 adev,ave,curt,sdev,skew,var,data_in(n),n_real
      integer j
      real*8 p,s,ep
      s=0.D0
      n_real=dble(n)
      do j=1,skip_id_
        s=s+data_in(j)
      enddo
      do j=skip_id+1,n
        s=s+data_in(j)
      enddo
      ave=s/n_real
      adev=0.D0
      var=0.D0
      skew=0.D0
      curt=0.D0
      ep=0.D0
      do 12 j=1,n
        s=data_in(j)-ave
        ep=ep+s
        adev=adev+dabs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s

        curt=curt+p
12    continue
      adev=adev/n_real
      var=(var-ep**2/n_real)/(n_real-1.D0)
      sdev=dsqrt(var)
      if(var.ne.0.D0)then
        skew=skew/(n_real*sdev**3)
        curt=curt/(n_real*var**2)-3.D0
      else
C        pause 'no skew or kurtosis when zero variance in moment'
      skew=0.D0
      curt=0.D0
      endif
      return
      end

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
