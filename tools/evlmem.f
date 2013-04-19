*========================================================================
* File Name : evlmem.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 17-11-2010
* Last Modified : Wed 17 Nov 2010 03:42:44 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      FUNCTION evlmem(fdt,d,m,xms)
      INTEGER m
      REAL evlmem,fdt,xms,d(m)
      INTEGER i
      REAL sumi,sumr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      theta=6.28318530717959d0*fdt
      wpr=cos(theta)
      wpi=sin(theta)
      wr=1.d0
      wi=0.d0
      sumr=1.
      sumi=0.
      do 11 i=1,m
        wtemp=wr
        wr=wr*wpr-wi*wpi
        wi=wi*wpr+wtemp*wpi
        sumr=sumr-d(i)*sngl(wr)
        sumi=sumi-d(i)*sngl(wi)
11    continue
      evlmem=xms/(sumr**2+sumi**2)
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
