*========================================================================
* File Name : write_neighbor.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時51分20秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine initialize_neighbors
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/mbh_ibin.h"
       include "../../include/ensemble.h"
       include "../../include/neighbors.h"
       pair_num=0
       do I0=1,total_neighbors
         call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,I0,1)
         pair_aboundance(I0)=0
         pair_digits_index(I0)=pair_digits
       enddo
       return
       end

       subroutine common_neighbors(ndim,x_temp)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/mbh_ibin.h"
       include "../../include/ensemble.h"
       include "../../include/neighbors.h"
       integer common_neighbor_num,common_neighbor_index(448)
       integer bonded_num,iso_num
       real*8 x_temp(3000)
     &,dist0,dist1,dist2,dist3
     &,cutoff_dist0,cutoff_dist1,cutoff_dist2,
     &cutoff_dist3
       do J0=1,atom_num-1
         I0=J0*dimension_index
         do J1=J0+1,atom_num
           I1=J1*dimension_index
           if(J0.le.atom_num_a.and.J1.le.atom_num_a)then
             cutoff_dist0=cutoff_AA
           else if(J0.gt.atom_num_a.and.J1.gt.atom_num_a)then
             cutoff_dist0=cutoff_BB
           else
             cutoff_dist0=cutoff_AB
           endif
           if(dimension_index.eq.3)
     &dist0=(x_temp(I0-2)-x_temp(I1-2))**2+
     &(x_temp(I0-1)-x_temp(I1-1))**2+(x_temp(I0)-x_temp(I1))**2
           if(dimension_index.eq.2)
     &dist0=(x_temp(I0-1)-x_temp(I1-1))**2+(x_temp(I0)-x_temp(I1))**2
           if(dist0.le.cutoff_dist0**2) then 
             dig1=0               !root pair is bonding 
           else
             dig1=1               !root pair is not bonding
           endif        !digit 1
           common_neighbor_num=0
           dig2=-1
           dig3=0
           do J2=1,atom_num                         !star loop common neighbor
             I2=J2*dimension_index
             if(J2.ne.J1.and.J2.ne.J0)then
               if(J0.le.atom_num_a.and.J2.le.atom_num_a)then
                 cutoff_dist1=cutoff_AA
               else if(J0.gt.atom_num_a.and.J2.gt.atom_num_a)then
                 cutoff_dist1=cutoff_BB
               else
                 cutoff_dist1=cutoff_AB
               endif
               if(J1.le.atom_num_a.and.J2.le.atom_num_a)then
                 cutoff_dist2=cutoff_AA
               else if(J1.gt.atom_num_a.and.J2.gt.atom_num_a)then
                 cutoff_dist2=cutoff_BB
               else
                 cutoff_dist2=cutoff_AB
               endif
               if(dimension_index.eq.3)then
                 dist1=(x_temp(I0-2)-x_temp(I2-2))**2+
     &(x_temp(I0-1)-x_temp(I2-1))**2+(x_temp(I0)-x_temp(I2))**2  
                 dist2=(x_temp(I1-2)-x_temp(I2-2))**2+
     &(x_temp(I1-1)-x_temp(I2-1))**2+(x_temp(I1)-x_temp(I2))**2
               endif
               if(dimension_index.eq.2)then
                 dist1=(x_temp(I0-1)-x_temp(I2-1))**2+
     &(x_temp(I0)-x_temp(I2))**2
                 dist2=(x_temp(I1-1)-x_temp(I2-1))**2+
     &(x_temp(I1)-x_temp(I2))**2
               endif
               if(dist1.le.cutoff_dist1**2.and.dist2.le.cutoff_dist2**2)
     &then
                 common_neighbor_num=common_neighbor_num+1
                 common_neighbor_index(common_neighbor_num)=J2
                 dig2=dig2+1                      !ditig 2
               endif
             endif
           enddo
           iso_num=0           
           do J2=1,common_neighbor_num-1
             I2=common_neighbor_index(J2)*dimension_index
             bonded_num=0
             do J3=1,J2
               if(J2.ne.J3)then
                 I3=common_neighbor_index(J3)*dimension_index
                 if(J2.le.atom_num_a.and.J3.le.atom_num_a)then
                   cutoff_dist3=cutoff_AA
                 else if(J2.gt.atom_num_a.and.J3.gt.atom_num_a)then
                   cutoff_dist3=cutoff_BB
                 else
                   cutoff_dist3=cutoff_AB
                 endif
                 if(dimension_index.eq.3)then
                   dist3=(x_temp(I2-2)-x_temp(I3-2))**2+
     &(x_temp(I2-1)-x_temp(I3-1))**2+(x_temp(I2)-x_temp(I3))**2
                 endif
                 if(dimension_index.eq.2)then
                   dist3=(x_temp(I2-1)-x_temp(I3-1))**2+
     &(x_temp(I2)-x_temp(I3))**2
                 endif
                 if(dist3.le.cutoff_dist3**2) then
                   bonded_num=bonded_num+1
                 endif
               endif
             enddo
             do J3=J2+1,common_neighbor_num
               I3=common_neighbor_index(J3)*dimension_index
               if(J2.le.atom_num_a.and.J3.le.atom_num_a)then
                 cutoff_dist3=cutoff_AA
               else if(J2.gt.atom_num_a.and.J3.gt.atom_num_a)then
                 cutoff_dist3=cutoff_BB
               else
                 cutoff_dist3=cutoff_AB
               endif               
               if(dimension_index.eq.3)then
                 dist3=(x_temp(I2-2)-x_temp(I3-2))**2+
     &(x_temp(I2-1)-x_temp(I3-1))**2+(x_temp(I2)-x_temp(I3))**2
               endif
               if(dimension_index.eq.2)then
                 dist3=(x_temp(I2-1)-x_temp(I3-1))**2+
     &(x_temp(I2)-x_temp(I3))**2
               endif
               if(dist3.le.cutoff_dist3**2) then
                 dig3=dig3+1      !digit 3
                 bonded_num=bonded_num+1        
               endif   
C               write(*,*) "index=",
C     &common_neighbor_index(J2),common_neighbor_index(J3)            
             enddo
             if(bonded_num.eq.0) iso_num=iso_num+1
           enddo
C           do J4=1,common_neighbor_num
C             write(*,*) "common=",common_neighbor_index(J4)
C           enddo
C           write(*,*) "dig=",dig1+1,dig2+1,dig3,"root=",J0,J1
           if(dig2-dig3.ge.2)then
             if(iso_num.eq.1)then
               dig4=0                  !with 1 isolated atoms
             else
               dig4=1
             endif
           else
             dig4=0                  !other topology         this definition is only for 2421,2422 2531,2532.2521.2522
           endif
C           if(iso_num.eq.1)dig4=2   !other choice for define dig4      if it is more than 5 neighbors and less than 3 bonds,this should be considerd!
C           if(iso_num.eq.2)dig4=3
           if(dig2.ge.0)then  ! if root pair has common neighbors
             call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,2)
             pair_num=pair_num+1
             pair_aboundance(matrix_index)=
     &pair_aboundance(matrix_index)+1         !Storing pair_num and pair_aboundance with matrix_index
           endif
         enddo
       enddo
       return
       end

       subroutine digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,sw)   !sw=1:matrix to digit/sw=2:digit to matrix
       implicit none
       include "../../include/common.h"
       include "../../include/neighbors.h"
       integer temp_num,temp_mod,sw
       if(sw.eq.1)then   !imput matrix_index output pair_digits and four digs
         temp_num=max_dig1*max_dig2*max_dig3
         dig4=(matrix_index-1)/temp_num
         temp_mod=mod((matrix_index-1),temp_num)
         temp_num=max_dig1*max_dig2
         dig3=temp_mod/temp_num
         temp_mod=mod(temp_mod,temp_num)
         dig2=temp_mod/max_dig1
         dig1=mod(temp_mod,max_dig1)
         pair_digits=(dig1+1)*1000+(dig2+1)*100+dig3*10+(dig4+1) !the definition of pair_digits
       else if (sw.eq.2)then !input four digits,output pair_digits and matrix_index
         pair_digits=(dig1+1)*1000+(dig2+1)*100+dig3*10+(dig4+1)
         matrix_index=(dig1+1)+(dig2*max_dig1)+(dig3*max_dig1*max_dig2)
     &+(dig4*max_dig1*max_dig2*max_dig3)
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
