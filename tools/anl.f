*========================================================================
* File Name : anl.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時03分16秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine write_anl(file_name,frame_num,access_method,
     &min_method)
C     write_atomic nearest-neighbor list,should be placed before minimization
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/anl.h"
      integer frame_num
      character access_method*3,min_method*3
      if(min_method.eq."min") call min_pes
      if(access_method.eq."app")then
        open(21,file=file_name,access="append")
      else
        open(21,file=file_name,status="replace")
      endif
      do J1=1,atom_num
        anl_num(J1)=0
        do J2=1,atom_num
          if(J1.ne.J2)then
            if(dist(J1,J2).le.bond_cutoff(J1,J2))then
              anl_num(J1)=anl_num(J1)+1
              anl_neighbor_matrix(J1,anl_num(J1))=J2
            endif
          endif
        enddo
        write(21,"(I13,1x,I5,1x,A10,1x,(100(I3,1x)))")
     &J1,anl_num(J1),"neighbors:",(
     &anl_neighbor_matrix(J1,I1),I1=1,anl_num(J1))
      enddo 
      close(21)
      return
      end

      subroutine write_anl_matrix(file_name,frame_num,access_method,
     &min_method)
C     write_atomic nearest-neighbor list,should be placed before
C     minimization
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/anl.h"
      integer frame_num
      character access_method*3,min_method*3
      if(min_method.eq."min") call min_pes
      if(access_method.eq."app")then
        open(21,file=file_name,access="append")
      else
        open(21,file=file_name,status="replace")
      endif
      do J1=1,atom_num
        anl_num(J1)=0
        do J2=J1+1,atom_num
          if(J1.ne.J2.and.dist(J1,J2).le.bond_cutoff(J1,J2))then
            anl_neighbor_matrix(J1,J2)=1
            anl_neighbor_matrix(J2,J1)=1
          else
            anl_neighbor_matrix(J1,J2)=0
            anl_neighbor_matrix(J2,J1)=0
          endif
        enddo
        write(21,*)(anl_neighbor_matrix(J1,J3),J3=1,atom_num)
      enddo
      close(21)
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
