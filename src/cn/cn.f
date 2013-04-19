*========================================================================
* File Name : cn.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時50分21秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine cn
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer common_neighbor_num,common_neighbor_index(448)
      integer bonded_num,iso_num
      real*8 dist0,dist1,dist2,dist3
      do I0=1,total_neighbors
        pair_aboundance(I0)=0
      enddo
      do J0=1,atom_num-1
        I0=J0*3
        do J1=J0+1,atom_num
          I1=J1*3
          dist0=(x(I0-2)-x(I1-2))**2+
     &(x(I0-1)-x(I1-1))**2+(x(I0)-x(I1))**2
          if(dist0.le.bond_cutoff(J0,J1)**2) then
            dig1=0               !root pair is bonding 
          else
            dig1=1               !root pair is not bonding
          endif        !digit 1
          common_neighbor_num=0
          dig2=-1
          dig3=0
          do J2=1,atom_num                         !star loop common neighbor
            I2=J2*3
            if(J2.ne.J1.and.J2.ne.J0)then
              dist1=(x(I0-2)-x(I2-2))**2+
     &(x(I0-1)-x(I2-1))**2+(x(I0)-x(I2))**2
              dist2=(x(I1-2)-x(I2-2))**2+
     &(x(I1-1)-x(I2-1))**2+(x(I1)-x(I2))**2
              if(dist1.le.bond_cutoff(J0,J2)**2.and.dist2
     &.le.bond_cutoff(J1,J2)**2)then
                common_neighbor_num=common_neighbor_num+1
                common_neighbor_index(common_neighbor_num)=J2
                dig2=dig2+1                      !ditig 2
              endif
            endif
          enddo
          iso_num=0  ! define this as the atom number without any bond(isolated atom)
          do J2=1,common_neighbor_num
            I2=common_neighbor_index(J2)*3
            bonded_num=0
            do J3=1,common_neighbor_num
              if(J2.ne.J3)then
                I3=common_neighbor_index(J3)*3
                dist3=(x(I2-2)-x(I3-2))**2+
     &(x(I2-1)-x(I3-1))**2+(x(I2)-x(I3))**2
                if(dist3.le.bond_cutoff(J2,J3)**2) then
                  dig3=dig3+1
                  bonded_num=bonded_num+1
                endif
              endif
            enddo
            if(bonded_num.eq.0) iso_num=iso_num+1
          enddo
          dig3=dig3/2   !double counting
          if(dig2-dig3.ge.2.and.dig3.ge.2)then
            dig4=dig2-dig3-iso_num          !4'th digits is (isolated number+1) for *42*,*52*,*53*,*62*,*63*,*64* 
C  Fro a 4 digits *mn* , I defined (m-n-iso_num) correspond to 4'th digits
C 1,which means no break between bonds(connect on by on)
C To realize this method, you should plot all the possible configuration from 1 to n isolated atom 
          else
            dig4=0
          endif
C          if(dig2-dig3.ge.2.and.dig3.ge.2)then
C            if(iso_num.eq.1)then
C              dig4=0                  !with 1 isolated atoms
C            else
C              dig4=1
C            endif
C          else
C            dig4=0                  !other topology         this definition is only for 2421,2422 2531,2532.2521.2522
C          endif
C           if(iso_num.eq.1)dig4=2   !other choice for define dig4      if it is more than 5 neighbors and less than 3 bonds,this should be considerd!
C           if(iso_num.eq.2)dig4=3
C===========================Collect all digits=====================================================
          if(dig2.ge.0)then  ! if root pair has common neighbors
            dig1=dig1+1
            dig2=dig2+1
            dig4=dig4+1
            call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,2)
            pair_num=pair_num+1
            pair_aboundance(matrix_index)=
     &pair_aboundance(matrix_index)+1         !Storing pair_num and pair_aboundance with matrix_index
          endif
        enddo
      enddo
C========================output pair_aboundance correspond to pair_digits_index===============
      return
      end

      subroutine digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,sw)   !sw=1:matrix to digit/sw=2:digit to matrix
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer temp_num,temp_mod,sw
      if(sw.eq.1)then   !imput matrix_index output pair_digits and four digs
        temp_num=dig1_max*dig2_max*dig3_max
        dig4=(matrix_index-1)/temp_num
        temp_mod=mod((matrix_index-1),temp_num)
        temp_num=dig1_max*dig2_max
        dig3=temp_mod/temp_num
        temp_mod=mod(temp_mod,temp_num)
        dig2=temp_mod/dig1_max
        dig1=mod(temp_mod,dig1_max)
        pair_digits=(dig1+1)*1000+(dig2+1)*100+dig3*10+(dig4+1) !the definition of pair_digits
      else if (sw.eq.2)then !input four digits,output pair_digits and matrix_index
        pair_digits=dig1*1000+dig2*100+dig3*10+dig4
        matrix_index=(dig1)+((dig2-1)*dig1_max)+(dig3*dig1_max*dig2_max)
     &+((dig4-1)*dig1_max*dig2_max*dig3_max)
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
