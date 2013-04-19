*========================================================================
* File Name : convlv.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 17-11-2010
* Last Modified : Wed 17 Nov 2010 03:30:28 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      SUBROUTINE convlv(data,n,respns,m,isign,ans)
      INTEGER isign,m,n,NMAX
      REAL data(n),respns(n)
      COMPLEX ans(n)
      PARAMETER (NMAX=4096)
CU    USES realft,twofft
      INTEGER i,no2
      COMPLEX fft(NMAX)
      do 11 i=1,(m-1)/2
        respns(n+1-i)=respns(m+1-i)
11    continue
      do 12 i=(m+3)/2,n-(m-1)/2
        respns(i)=0.0
12    continue
      call twofft(data,respns,fft,ans,n)
      no2=n/2
      do 13 i=1,no2+1
        if (isign.eq.1) then
          ans(i)=fft(i)*ans(i)/no2
        else if (isign.eq.-1) then
          if (abs(ans(i)).eq.0.0) pause

     *'deconvolving at response zero in convlv'
          ans(i)=fft(i)/ans(i)/no2
        else
          pause 'no meaning for isign in convlv'
        endif
13    continue
      ans(1)=cmplx(real(ans(1)),real(ans(no2+1)))
      call realft(ans,n,-1)
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
