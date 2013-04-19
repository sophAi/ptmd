*========================================================================
* File Name : edit_mom_score.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 11 Jun 2012 08:58:08 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_usr_score
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/score.h"
      include "../../include/tester/tester.h"
      character*2 sel,sel2
!Default input file name
      if(pes_type.eq."none ")then
        write(*,*) "No PES, USR requare mass information"
        write(*,*) "Be sure to read the initial config file first"
        write(*,*) "Please select a PES first"
        call pes_cycle
      endif
      call edit_source
      call edit_target
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"delta_loop ")
        write(*,*) " 4.Select which moments to calculate"
        write(*,*) " 5.Select the score function"
        call show_parameter(6,"score_target_init ")
        call show_parameter(7,"score_target_final ")
        call show_parameter(8,"score_output_flag ")
        call show_parameter(9,"tester_output_flag ")
        write(*,*) "---------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_loop ")
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")call edit_parameter("delta_loop ")
        if(sel.eq."4")then
          sel2="99"
          do while(sel2.ne."0")
            call show_parameter(0,"quit ")
            call show_parameter(1,"score_1st_io ")
            call show_parameter(2,"score_2nd_io ")
            call show_parameter(3,"score_3rd_io ")
            call show_parameter(4,"score_4th_io ")
            write(*,*) "-----------------------------------------"
            read(*,*) sel2
            write(*,*) "NOTE: 1=on/0=off"
            if(sel2.eq."1")call edit_parameter("score_1st_io ")
            if(sel2.eq."2")call edit_parameter("score_2nd_io ")
            if(sel2.eq."3")call edit_parameter("score_3rd_io ")
            if(sel2.eq."4")call edit_parameter("score_4th_io ")
          enddo
        endif
        if(sel.eq."5")then
          write(*,*) 
     &" NOTE! Use consine will decrease the sensitivity"
          call show_parameter(0,"quit ")
          write(*,*) " 1.Use denominator function for score(0~1)"
          write(*,*) " 2.Use cosine function for score(-1~1)"
          write(*,*) " 3.Output a specific moment" 
          write(*,*) "-------------------------------------------"
          call edit_parameter("score_function ")
        endif
        if(sel.eq."6")call edit_parameter("score_target_init ")
        if(sel.eq."7")call edit_parameter("score_target_final ")
        if(sel.eq."8")then
          write(*,*)
     & "0. Do not output moments,calculate ensemble average only"
          write(*,*) "1. Write moment vs time(*.mom)"
          write(*,*) "2. Write score vs time(*.scr)"
          write(*,*) "3. Write both"
          call edit_parameter("score_output_flag ")
        endif
        if(sel.eq."9")then
          write(*,*) "Please select the output method"
          write(*,*) " 1. Output additional octave headers"
          write(*,*) " 2. Output sophai file only"
          call edit_parameter("tester_output_flag ")
        endif
      enddo
      return
      end

      subroutine write_usr_score
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/score.h"
      include "../../include/tester/tester.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x,
     &A10,1x,I3,1x,
     &A9,1x,I5,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,
     &A10,1x,F13.1,1x,
     &A11,1x,F13.1,1x,
     &A11,1x,F13.1,1x,
     &A20,1x,I1,1x,
     &A13,1x,I1,1x,
     &A13,1x,I1,1x,
     &A13,1x,I1,1x,
     &A13,1x,I1,1x,
     &A15,1x,I1,1x,
     &A18,1x,F10.1,1x,
     &A19,1x,F10.1,1x,
     &A12,1x,I2,1x,
     &A17,1x,I2,1x,
     &A17,1x,I2,1x,
     &A17,1x,I2,1x,
     &A12,1x,A20,1x,
     &A12,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1,1x,
     &A12,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1,1x,
     &A24,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1)")

     &20,10,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"delta_loop=",delta_loop,
     &"score_output_method=",score_output_flag,
     &"score_1st_io=",score_1st_io,
     &"score_2nd_io=",score_2nd_io,
     &"score_3rd_io=",score_3rd_io,
     &"score_4th_io=",score_4th_io,
     &"score_function=",score_function,
     &"score_target_init=",score_target_init,
     &"score_target_final=",score_target_final,
     &"output_flag=",tester_output_flag,
     &"source_file_flag=",source_file_flag,
     &"target_file_flag=",target_file_flag,
     &"header_file_flag=",header_file_flag,
     &"tester_flag=",tester_flag,
     &"source_type=",source_type,
     &"source_file_type=",source_file_type,
     &"source_file_name= '",source_file_name,"'",
     &"target_type=",target_type,
     &"target_file_type=",target_file_type,
     &"target_file_name= '",target_file_name,"'",
     &"header_file_source_type=",header_file_source_type,
     &"header_file_type=",header_file_type,
     &"header_file_name= '",header_file_name,"'"
      close(20)
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
