*========================================================================
* File Name : BIMD_gen.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 03 Aug 2011 04:26:02 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_gen
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      call comvel
C===========MANUALLY INPUT VELOCITY================
C      if(debug)then
C        write(*,*) "DEBUG MODE"
C        open(40,file="vel.dat",status="old")
C        do J1=1,atom_num
C          read(40,*) v(J1*3-2),v(J1*3-1),v(J1*3)
C          write(*,"(I2,1x,3(F14.8,1x))")J1,v(J1*3-2),v(J1*3-1),v(J1*3)
C        enddo
C        pause"debug"
C        close(40)
C      endif
C=========End of debug============================
      do J1=1,top_frz_num
        J3=top_frz_id(J1)
        J2=J3*3
        q(J1)=x(J2-2)
        q(J1+N1)=x(J2-1)
        q(J1+N1*2)=x(J2)
        q(J1+N1*3)=v(J2-2)*mass(J3)
        q(J1+N1*4)=v(J2-1)*mass(J3)
        q(J1+N1*5)=v(J2)*mass(J3)
      enddo
      q(N1*6+1)=0.D0
      q(N1*6+2)=0.D0
      q(N1*6+3)=0.D0
      q(N1*6+4)=0.D0
      q(N1*6+5)=0.D0
      q(N1*6+6)=0.D0
      q(N1*6+7)=0.D0
      q(N1*6+8)=0.D0
      q(N1*6+9)=0.D0
      do J1=1,q_dim
        fk2(J1)=0.D0
        fk1(J1)=0.D0
        fk0(J1)=0.D0
        yk3(J1)=0.D0
        yk2(J1)=0.D0
        yk1(J1)=0.D0
        yk0(J1)=0.D0
        ycp(J1)=0.D0
C        write(*,*) J1,q(J1)
      enddo
C      pause "init q"
      do J1=1,q_dim           ! INITIAL POINT
        yk3(J1)=q(J1)
      enddo
      CALL TDIFF(q,fk2)  !second Point
C      write(*,*) "TEST After 1"
      do J1=1,q_dim
        yk2(J1)=q(J1)
C        write(*,*) J1,yk2(J1),fk2(J1)
      enddo
C      pause "1st fk2"
C      write(*,*) "TEST After 2"
      CALL TDIFF(q,fk1)
      do J1=1,q_dim           ! THIRD POINT
        yk1(J1)=q(J1)
C        write(*,*) J1,yk1(J1),fk1(J1)
      enddo
      CALL TDIFF(q,fk0)
      do J1=1,q_dim           ! FOURTH POINT
        ycp(J1)=0.0D0
        yk0(J1)=q(J1)
      enddo
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
