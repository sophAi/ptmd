*========================================================================
* File Name : correl.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 17-11-2010
* Last Modified : Thu 09 Dec 2010 05:02:33 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      SUBROUTINE correl(data1,data2,n,ans)
      INTEGER n,NMAX
      REAL data1(n),data2(n)
      COMPLEX ans(n)
      PARAMETER (NMAX=4096)
CU    USES realft,twofft
      INTEGER i,no2
      COMPLEX fft(NMAX)
      call twofft(data1,data2,fft,ans,n)
      no2=n/2
      do 11 i=1,no2+1
        ans(i)=fft(i)*conjg(ans(i))/float(no2)
11    continue
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
