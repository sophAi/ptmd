*========================================================================
* File Name : correlation.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月23日 (週六) 16時13分09秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine p_matrix(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      include "../include/tester/tester.h"
      if(tester_data_type.eq."cn")then
        call p_matrix_cn
      endif
      if(tester_data_type.eq."anl")then
        call p_matrix_anl
      endif
      return
      end

      subroutine s_matrix
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/sliding.h"
      integer dp,p_first(sliding_p_length_max),
     &p_second(sliding_p_length_max),s(sliding_p_length_max)
      open(21,file=source_file_name(:index(source_file_name," ")-1)
     &//".pts",status="old")
      read(21,*)(p_first(I0),I0=1,p_matrix_length)
      open(22,file=source_file_name(:index(source_file_name," ")-1)
     &//".sts",status="replace")
      do I1=2,total_p_num
        read(21,*)(p_second(I0),I0=1,p_matrix_length)
        do I2=1,p_matrix_length
          dp=p_second(I2)-p_first(I2)
          if(dp.eq.0)then              !calculate dp
            s(I2)=0
          else if(dp.gt.0)then
            s(I2)=1
          else
            s(I2)=-1
          endif
          p_first(I2)=p_second(I2)
        enddo
C=======calculate c matrix===============
        write(22,"(3000(I4,1x))") (s(I3),I3=1,p_matrix_length)
      enddo
      total_s_num=total_p_num-1
      s_matrix_length=p_matrix_length
      close(21)
      close(22)
      return
      end

      subroutine c_matrix
C     If we have a=window_width,b=sliding_width,with N=total_step
C     ,we have (N-a)/b sliding_windows
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/sliding.h"
      integer c(p_matrix_max*p_matrix_max),
     &c_prev(p_matrix_max*p_matrix_max),
     &c_next(p_matrix_max*p_matrix_max)
      integer s(sliding_p_length_max),start_loop_int,end_loop_int
      call c_id_init  ! initialized the c_id matrix,including c_id(i,j),c_i(c_id),c_j(c_id)
      call zero_c(c)
      open(21,file=source_file_name(:index(source_file_name," ")-1)
     &//".sts",status="old")
      open(22,file=source_file_name(:index(source_file_name," ")-1)
     &//".cts",status="replace")
      start_loop_int=1
      end_loop_int=int((total_p_num-window_width)/sliding_width) !(N-a)/b=n
      call zero_c(c_prev)
      do I1=1,sliding_width
        read(21,*)(s(I2),I2=1,s_matrix_length)
        do I2=1,s_matrix_length-1
          do I3=I2+1,s_matrix_length
            c_prev(c_id_ij(I2,I3))=c_prev(c_id_ij(I2,I3))+
     &abs((s(I2)+s(I3))*s(I3))/2             !sign comparison 
          enddo
        enddo
      enddo
C===========sliding windows========================
      do I0=start_loop_int,end_loop_int+1  !loops for windows
        call zero_c(c_next)
        do I1=sliding_width+1,window_width-1   !to 1999,not 2000
          read(21,*)(s(I2),I2=1,s_matrix_length)
          do I2=1,s_matrix_length-1
            do I3=I2+1,s_matrix_length
              c_next(c_id_ij(I2,I3))=c_next(c_id_ij(I2,I3))+
     &abs((s(I2)+s(I3))*s(I3))/2             !sign comparison 
            enddo
          enddo
        enddo
        read(21,"(3000(I4,1x))",end=101)(s(I2),I2=1,s_matrix_length)    !Note here,1~1000,1001~1999,but for 2nd windows,it starts from 1001~2000. We have to read 2000 for 2nd window
C============check c value,add c(j,i) make one half
101     do I2=1,s_matrix_length-1 
          do I3=I2+1,s_matrix_length
            c(c_id_ij(I2,I3))=
     &c_prev(c_id_ij(I2,I3))+c_next(c_id_ij(I2,I3))
            c(c_id_ij(I3,I2))=c(c_id_ij(I2,I3))             !note c is integer
            c_prev(c_id_ij(I2,I3))=c_next(c_id_ij(I2,I3))+
     &abs((s(I2)+s(I3))*s(I3))/2  !very important here, add 2000'th for 2nd windows
          enddo
        enddo
        write(22,"(3000(I4,1x))")
     &(c(I3),I3=1,s_matrix_length*s_matrix_length)
C===============================================
C===========check c=============================
C        write(*,*) "windows= ",I0
C        do I2=1,s_matrix_length
C          write(*,"1000(I2,1x)")(c(c_id_ij(I2,I3)),I3=1,s_matrix_length)
C        enddo
C        pause
C===============================================
      enddo
      close(22)
      close(21) 
      return
      end
 
      subroutine c_histogram(c_matrix,c_hist)
      implicit none 
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/sliding.h"
      integer c_matrix(p_matrix_max),c_hist(p_matrix_max)
      call zero_c(c_hist)
      do I0=1,s_matrix_length*s_matrix_length
        c_hist(c_matrix(I0))=c_hist(c_matrix(I0))+1 
      enddo
      return
      end    

      subroutine c_histogram_gap
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/sliding.h"

      return
      end

      subroutine c_id_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/sliding.h"
      do I0=1,s_matrix_length !row
        do J0=1,s_matrix_length !column
          c_id_ij(I0,J0)=J0+(I0-1)*s_matrix_length
          c_id_i(c_id_ij(I0,J0))=I0
          c_id_j(c_id_ij(I0,J0))=J0          
        enddo
      enddo
      return
      end

      subroutine zero_c(c)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/sliding.h"
      integer c(p_matrix_max*p_matrix_max)
      do I0=1,s_matrix_length-1
        c(c_id_ij(I0,I0))=0        !C(i,i) already claimed here
        do I1=I0+1,s_matrix_length
          c(c_id_ij(I0,I1))=0
          c(c_id_ij(I1,I0))=0
        enddo
      enddo
      return
      end
  
      subroutine locate_c(init_window,end_window)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tester/sliding.h"
      integer init_window,end_window 
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
