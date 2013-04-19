*========================================================================
* File Name : correlation_lin2.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時04分27秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine correlation_p
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      integer start_loop,end_loop,interval_loop,window_loop
c---------- input the file name ---------------      
      file_name="BIMD_pes_gupta_pure_Ag0014_0100.CN"
c----------------------------------------------

c      I1=I0*1000+1
c      I2=I1+1999
c     J1=3001
c     J2=5000
       write(*,*)"please input the window length (ex:1.D6)"
       read(*,*)window_loop
       write(*,*)"please input the start step (ex:1)"
       read(*,*)start_loop
       write(*,*)"please input the end step (ex:1)"
       read(*,*)end_loop
c       start_loop=1
c       end_loop=1
c      interval_loop=1000
c       window_loop=2000
       do I0=start_loop,end_loop
         J1=(I0-1)*window_loop+1
         J2=(I0-1)*window_loop+window_loop
        call correlation_p_matrix(file_name,J1,J2,window_loop)
       enddo
      return
      end

      subroutine correlation_p_matrix(file_name,cor_window_init,
     &cor_window_final,cor_window_time)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      integer cor_window_time,previous_total_pair_digits
c      integer cor_window_time
c     parameter(max_cor=1000) !  maximun correlation 
      integer p_exist(CN_max),p(CN_max),s(CN_max),dp
      integer cor(CN_max,CN_max),p_exist_time(cor_window_time,CN_max)
      integer cor_window_init,cor_window_final,time_step
      integer total_p_num,p_id_out(CN_max),digit_out(CN_max),
     &p_time(cor_window_time,CN_max)
      integer cor_hist(cor_window_time),largest_gap_min,largest_gap_max
      integer threshold,largest_gap,gap,upper_hist_index
      integer cor_cluster(CN_max),cor_clus(CN_max) ! by Lin
      real*8 real_dummy1
      character*50 file_name_22,file_name_23
      do I0=1,CN_max
        p_exist(I0)=0
        do J0=1,CN_max
          cor(I0,J0)=0
        enddo
        do J0=cor_window_init,cor_window_final
          p_exist_time(J0,I0)=0
          p_time(J0,I0)=0
        enddo
      enddo

c--------- change the name of output file -------------
      file_name_22="correlation_p_100K_Ag14_s=+1.dat"
      file_name_23="correlation_p_100K_Ag14_s=-1.dat"
c------------------------------------------------------

      open(21,file=file_name,status="old")
      open(22,file=file_name_22,access="append")
      open(23,file=file_name_23,access="append")
c********* if you wnat to change different short time interval ********
c********* , you should change some marked part (--+--)        ********

c                           --+--(ex:+9)
      do I0=1,cor_window_init
        read(21,*)time_step,real_dummy1,CN_name
      enddo
      write(*,*) "start from ",time_step
      call CN2digits
      do I0=1,total_pair_digits
        p_exist(CN_matrix(I0))=1
        p_time(cor_window_init,CN_matrix(I0))=CN_aboundance(I0)
        p_exist_time(cor_window_init,CN_matrix(I0))=1
      enddo

c                         --+--(ex:+19)        --+--(ex:,10) 
      do I0=cor_window_init+1,cor_window_final
c ------( --+-- )-----
c        do J0=1,9
c        read(21,*)
c        enddo
c ------( --+-- )-----
        read(21,*)time_step,real_dummy1,CN_name
        call CN2digits
        do I1=1,total_pair_digits
          p_exist(CN_matrix(I1))=1
          p_exist_time(I0,CN_matrix(I1))=1
          p_time(I0,CN_matrix(I1))=CN_aboundance(I1)
        enddo
        do I1=1,CN_max
          s(I1)=0
c                           --+--(ex:-10)
          if((p_exist_time(I0-1,I1).ne.0).or.(p_exist_time(I0,I1).ne.0))
     &then 
c                   --+--(ex:-10)
           dp=p_time(I0,I1)-p_time(I0-1,I1)
            if(dp.lt.0)then
c                                 --+--(ex:/10 -1)
            write(22,"3(I9,1x)")I0-1,I1 !,-1
            elseif(dp.gt.0)then
c                                 --+--(ex:/10 -1)
            write(23,"3(I9,1x)")I0-1,I1 !,1
c******************************************************************
            else
            continue
c            write(22,"3(I9,1x)")I0-5,I1,0
            endif
          endif
        enddo
      enddo
      close(21)
      close(22)
      close(23)
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
