*========================================================================
* File Name : temp2freq.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時07分27秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine temp2freq
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/BIMD/BIMD.h"
      include "../include/pes.h"
      real*8 w,pi,h,landan,N,L
      N=1.D0
      landan=1.D0
      temp=100.D0
      L=6.D0
      pi=read_unit("Pi ","Pi ","1 ")
      h=read_unit("Planck_const ","h ","evs ")
      w=(4.D0*(pi**2)/(h*landan))*((pi*L**3)/6.D0*N)**(1.D0/3.D0)*temp
      write(*,*) "w=",w
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
