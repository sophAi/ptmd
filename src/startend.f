*========================================================================
* File Name : startend.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時42分20秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine startend(myid,nproc,is1,is2,istart,iend)
      implicit none
      integer myid,nproc,is1,is2,istart,iend,iblock,ir,ilength
      ilength=is2-is1+1
      iblock=ilength/nproc
      ir=ilength-iblock*nproc
      if(myid.lt.ir)then
        istart=is1+myid*(iblock+1)
        iend=istart+iblock
      else
        istart=is1+myid*iblock+ir
        iend=istart+iblock-1
      endif
      if(ilength.lt.1) then
        istart=1
        iend=0
      endif

C      if(myid.eq.0)then
C        istart=is1
C        iend=istart+iblock-1
C      else if(myid.gt.0.and.myid.le.ir) then
C         istart=is1+myid*(iblock+1)-1
C         iend=istart+iblock
C      else if(myid.gt.ir)then
c         istart=is1+myid*iblock+ir
C         iend=istart+iblock-1
C      endif
C      if(ilength.lt.1) then
C         istart=1
c         iend=0
C      endif
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
