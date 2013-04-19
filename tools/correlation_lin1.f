*========================================================================
* File Name : correlation_lin1.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時04分11秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine correlation
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      integer start_loop,end_loop,interval_loop,window_loop
      character*50 file_name_24
      character*20 s_name


      file_name="BIMD_pes_gupta_pure_Ag0014_0100.CN"
c      I1=I0*1000+1
c      I2=I1+1999
c     J1=3001
c     J2=5000
c      write(*,*)"please input the window length (ex:2000)"
c      read(*,*)window_loop
c      write(*,*)"please input the starting window"
c      read(*,*)start_loop
c      write(*,*)"please input the endding window"
c      read(*,*)end_loop

c****** change window ********************************
       start_loop=1
       end_loop=999
       interval_loop=1000
       window_loop=2000
       do I0=start_loop,end_loop
         J1=(I0-1)*interval_loop+1
         J2=(I0-1)*interval_loop+window_loop
        call correlation_matrix(file_name,J1,J2)
       enddo
c**  you should change file name two parts  ***
c******* change file name here ***********************  
c----- special name for each data (s_name) ---------- 1 )

c    ============
      s_name = "Ag14_100K"
c    ============

c----- name of strongly 4digit (file_name_24) --------      
      file_name_24 ="correlation_4digit_"//s_name//".dat"
c-----------------------------------------------------

      open(24,file=file_name_24,access="append")
      write(24,"(I4)")3 ! by Lin DNA code=3(end)
      return
      end

      subroutine correlation_matrix(file_name,cor_window_init,
     &cor_window_final)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      integer cor_window_time,previous_total_pair_digits
      parameter(cor_window_time=2000)
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
      character*50 file_name_22,file_name_23,file_name_24,file_name_25
      character*20 s_name

c******* change file name here ***********************
c----- special name for each data (s_name) --------- 2 )

c    =============
      s_name = "Ag14_100K"
c    =============

c----- name of correlation matrix (file_name_22) -----   
      file_name_22 ="correlation_matrix_"//s_name//".dat" 
c----- name of histogram (file_name_23) --------------
      file_name_23 ="correlation_histogram_"//s_name//".dat"
c----- name of strongly 4digit (file_name_24) --------
      file_name_24 ="correlation_4digit_"//s_name//".dat" 
c----- name of CN data (file_name_25) ----------------
      file_name_25 ="correlation_cn_data_"//s_name//".dat"
c-----------------------------------------------------

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
      open(21,file=file_name,status="old")
c ************** if i want to change short time interval *********
c ************ you should change the marked part(---+---) ********

c                         ---+---(ex: +4)
      do I0=1,cor_window_init
        read(21,*) time_step,real_dummy1,CN_name
      enddo
      write(*,*) "start from ",time_step
      call CN2digits
      do I0=1,total_pair_digits
        p_exist(CN_matrix(I0))=1
        p_time(cor_window_init,CN_matrix(I0))=CN_aboundance(I0)
        p_exist_time(cor_window_init,CN_matrix(I0))=1
      enddo
c                      ----+----(here(ex:+9)) -+-(ex: ,5)
      do I0=cor_window_init+1,cor_window_final
c     ----remove c ---------
c        do J0=1,4   !------choose larger short time----
c        read(21,*)
c        enddo       !----------------------------------
c     ----------------------
        read(21,*)time_step,real_dummy1,CN_name
        call CN2digits
        do I1=1,total_pair_digits
          p_exist(CN_matrix(I1))=1
          p_exist_time(I0,CN_matrix(I1))=1
          p_time(I0,CN_matrix(I1))=CN_aboundance(I1)
        enddo
        do I1=1,CN_max
          s(I1)=0
c                        ----+----(choose the short time(ex: -5))
          if(p_exist_time(I0-1,I1).ne.0.or.p_exist_time(I0,I1).ne.0)then
c                                    --+--(ex: -5)                                   
            dp=p_time(I0,I1)-p_time(I0-1,I1)
c ******************************************************************
            if(dp.gt.0)then
              s(I1)=1
            else if(dp.lt.0)then
              s(I1)=-1
            else
              s(I1)=0
            endif
C            if(s(I1).ne.0)then
C              call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,I1,1)
C              write(*,*) "I0=",I0,",I1=",I1,",dp=",dp,",d=",s(I1),
C     &pair_digits
C              pause
C            endif
          endif
        enddo
        do I1=1,CN_max
          do J1=I1+1,CN_max
c            cor(I1,J1)=cor(I1,J1)+
c     &abs((s(I1)+s(J1)+2)*(s(J1)+s(I1)-2)*s(J1)*s(I1))/4 ! +-(fun)Lin 
             cor(I1,J1)=cor(I1,J1)+
     &abs((s(I1)+s(J1))*s(J1))/2
            cor(J1,I1)=cor(I1,J1)
C            if(cor(I1,J1).ne.0)write(*,*) I1,J1,cor(I1,J1)
          enddo
        enddo
      enddo
      close(21)
C================output==========================
      open(22,file=file_name_22,access="append")
      total_p_num=0
      do I1=1,CN_max
        if(p_exist(I1).ne.0)then
          total_p_num=total_p_num+1
          p_id_out(total_p_num)=I1
          call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,I1,1)
          digit_out(total_p_num)=pair_digits
        endif
      enddo
      write(22,*)cor_window_init,cor_window_final
      write(22,"1000(I4,1x)") 
     &total_p_num,(digit_out(I0),I0=1,total_p_num)
c  = = = = = = = = = = = = = = = = = = = = Lin 
      open(25,file=file_name_25,access="append")
      write(25,*)(total_p_num*(total_p_num-1))/2 
     &,cor_window_final/1000-1
      do I1=1,total_p_num
        do J1=I1+1,total_p_num
c          if (cor(p_id_out(I1),p_id_out(J1)).ne.0) then
          write(25,"3(I4,1x)") digit_out(I1),digit_out(J1)    
     &,cor(p_id_out(I1),p_id_out(J1))
c          endif
        enddo
      enddo
c  = = = = = = = = = = = = = = = = = = = = = Lin
      upper_hist_index=0
c    =========debug by Lin(make cor_hist=0 )===============
      do I1=0,2000         ! suppose the maximun correlation =1000
        
         cor_hist(I1)=0
        
      enddo
c    ========debug by Lin =============================== 
      do I1=1,total_p_num
        write(22,"1000(I4,1x)")digit_out(I1),
     &(cor(p_id_out(I1),p_id_out(J1)),J1=1,total_p_num)
        do J1=I1+1,total_p_num
          cor_hist(cor(p_id_out(I1),p_id_out(J1)))=
     &cor_hist(cor(p_id_out(I1),p_id_out(J1)))+1
          if(cor(p_id_out(I1),p_id_out(J1)).gt.upper_hist_index)then
            upper_hist_index=cor(p_id_out(I1),p_id_out(J1))
          endif
        enddo
      enddo
      write(*,*) "upper_hist_index=",upper_hist_index
      open(23,file=file_name_23,access="append")
      write(23,*) "# ",cor_window_init,cor_window_final
      largest_gap_min=1
      largest_gap_max=1
      largest_gap=0
      if (upper_hist_index.gt.1)then
       do I0=1,upper_hist_index-1
         if(cor_hist(I0).ne.0)then
          write(23,"2(I6,1x)") I0,cor_hist(I0)
          gap=0
           do J0=I0+1,upper_hist_index
             if(cor_hist(J0).eq.0)then
              gap=gap+1
             else
              I1=I0
              J1=J0
              goto 11
             endif
           enddo
11         if(gap.ge.largest_gap)then
            largest_gap=gap
            largest_gap_min=I1
            largest_gap_max=J1
           endif
         endif
       enddo
      endif
      write(23,"2(I6,1x)") upper_hist_index,cor_hist(upper_hist_index)
      threshold=(largest_gap_max+largest_gap_min)/2
      write(22,*)"largest_gap_min= ",largest_gap_min,
     &",largest_gap_max= ",largest_gap_max,",threshold=",threshold
      close(23)
      close(22)
      close(25)  ! Lin
C==============Clustering===by Lin=======================
      do I1=1,CN_max
         cor_cluster(I1)=0
      enddo
      open(24,file=file_name_24,access="append")
      write(24,"(I4,1X,I7,"":"",I7)")0,cor_window_init,
     &cor_window_final     ! DNA code=0(start)
      do I1=1,total_p_num
        do J1=I1+1,total_p_num
         if(cor(p_id_out(I1),p_id_out(J1)).gt.threshold) then
c        cor_cluster(2*I1-1)=digit_out(I1)
c        cor_cluster(2*I1)=digit_out(J1)
         write(24,"10(I4,1x)") 1,digit_out(I1),digit_out(J1)! DNA code=1(read)
         endif
        enddo
      enddo
      write(24,"3(I4,1x)")2,2,2 !DNA code=2(stop)
c     do J1=1,CN_max
c      do I1=1,CN_max
c       if(cor_cluster(I1).ne.0)then
c       cor_clus(J1)=cor_cluster(I1)
c       endif
c      enddo
c     enddo
c     write(24,100)
c    &(cor_clus(I1),I1=1,CN_max) 
100   format (100(I4,1X))

      
      close(24)
C=========================================================
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
