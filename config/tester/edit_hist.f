*=========================================================================
* File Name : edit__hist.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年04月13日 (週三) 12時57分51秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_hist
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h" 
      include "../../include/tester/hist.h"
      include "../../include/tools/histogram.h"
      include "../../include/header.h"
      real*8 ref_hist_interval_lower,ref_hist_interval_upper,
     &ref_hist_max_lower,ref_hist_max_upper
      character*2 sel,sel2,sel3
!Default input file name
      call edit_source
      sel="99"
      kb=read_unit("Boltzmann_const ","kb ","evKelven^-1 ")
      ref_hist_interval_lower=(hist_max_lower-hist_min_lower)/
     &dble(hist_bin_max)
      ref_hist_max_lower=hist_min_lower+hist_interval_lower*
     &dble(hist_bin_max)
      do while(sel.ne."0")
        write(*,*) "Current Status:"
        write(*,*) 
        write(*,*) "Source type= ",source_type,",source file type=",
     &source_file_type
        write(*,*) "hist_bin_max=",hist_bin_max
        write(*,*) "hist_min_lower=",hist_min_lower,"/hist_min_upper=",
     &hist_min_upper
        write(*,*) "hist_max_lower=",hist_max_lower,"/hist_max_upper=",
     &hist_max_upper
        write(*,*) "hist_interval_lower=",hist_interval_lower,
     &"/hist_interval_upper=",hist_interval_upper
        write(*,*)"================================================="
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"delta_loop ")
        call show_parameter(4,"hist_method ")
        write(*,*) " 5.Change the minimum value of histogram"
        write(*,*) " 6.Change the maximum value of histogram"
        write(*,*) " 7.Change the interval of histogram"
        call  show_parameter(8,"tester_output_flag ")
        write(*,*) " 9.Check the range of a file"
        write(*,*) "---------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_loop ")
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")call edit_parameter("delta_loop ")
        if(sel.eq."4")then
          write(*,*)
     &" 1.Calculate histogram of eng from unformatted output(1~n)"
          write(*,*)
     &" 2.Calculate histogram of lmin from unformatted output"
          write(*,*)
     &" 3.Calculate histogram of grad from unformatted output"
          write(*,*)
     &" 4.Calculate histogram from formatted xyz or pdb"
          write(*,*)
     &" 5.Calculate histogram from formatted dat file (n row)"
          write(*,*)
     &" 6.Calculate histogram from formatted scr file (n rows)"
          write(*,*) 
     &" 7.Calculate histogram from formatted psd file (n rows)"
          write(*,*) "-----------------------------------------"
          call edit_parameter("hist_method ")
304       write(*,*) "Please select a file format:"
          if(hist_method.eq.1)then
C            hist_source_type="ufe"
C            source_type="ufe"
            write(*,*)
     &"Will read ufe file and compute energy histogram"
          else if(hist_method.eq.2)then
C            hist_source_type="min"
C            source_type="ufx"
            write(*,*) 
     &"Will read ufx file and compute local minimal potential per atom"
          else if(hist_method.eq.3)then
            write(*,*)
     &"Will read ufg file and compute local minimal potential per atom"
          else if(hist_method.eq.4)then
            write(*,*) " 1. *.xyz"
            write(*,*) " 2. *.pdb"
            read(*,*) sel2
            if(sel2.eq."1")then
C              source_type="xyz"
C              hist_source_type="xyz"
            else if(sel2.eq."2")then
C              source_type="pdb"
C              hist_source_type="pdb"
            else
              write(*,*) "WARNING! Please select again!"
              goto 304
            endif
          else if(hist_method.eq.5)then
C            source_type="dat"
C            hist_source_type="dat"
          else if(hist_method.eq.6)then
C            source_type="scr"
C            hist_source_type="scr"
          else if(hist_method.eq.7)then
C            source_type="dft"
C            hist_source_type="dft"
          endif
        endif
        if(sel.eq."5")then
          sel2="99"
          do while(sel2.ne."0")
            write(*,*) "hist_min will be fixed all the time"
            write(*,*) "Also, the total bin number hist_bin_max=",
     &hist_bin_max
            write(*,*) " is fixed. Therefore, both hist_max and"
            write(*,*) " hist_interval will be changed autometically"
            write(*,*)
     &"according to which one is the dominant parameter" 
            write(*,*) " The hist_interval parameter is always choosen"
            write(*,*) " as a dominant parameter before hist_max"
            ref_hist_max_lower=hist_min_lower+
     &hist_interval_lower*dble(hist_bin_max)
            ref_hist_max_upper=hist_min_upper+
     &hist_interval_upper*dble(hist_bin_max)
            call update_parameter("hist_max_lower ",
     &ref_hist_max_lower)
            call update_parameter("hist_max_upper ",
     &ref_hist_max_upper)
            write(*,*) "FIX: hist_bin_max=",hist_bin_max
            write(*,*) "FIX: hist_interval_lower=",hist_interval_lower,
     &"/hist_interval_upper=",hist_interval_upper
            write(*,*) "--------->"
            write(*,*) "WILL CHANGE: hist_max_lower=",hist_max_lower,
     &"/hist_max_upper=",hist_max_upper
            call show_parameter(0,"quit ")
            call show_parameter(1,"hist_min_lower ")
            call show_parameter(2,"hist_min_upper ")
            read(*,*)sel2
            if(sel2.eq."1")call edit_parameter("hist_min_lower ")
            if(sel2.eq."2")call edit_parameter("hist_min_upper ")
          enddo
        endif
        if(sel.eq."6")then
          sel2="99"
          do while(sel2.ne."0")
            write(*,*) "You could use either hist_max or hist_interval"
            write(*,*) "as a dominant parameter"
            write(*,*) "Important!!"
            write(*,*) "If you choose hist_max as a dominant parameter"
            write(*,*) ", the value of hist_interval will be "
            write(*,*) "changed autometically"
            write(*,*)
            ref_hist_interval_lower=(hist_max_lower-hist_min_lower)/
     &dble(hist_bin_max)
            ref_hist_interval_upper=(hist_max_upper-hist_min_upper)/
     &dble(hist_bin_max)
            call update_parameter("hist_interval_lower ",
     &ref_hist_interval_lower)
            call update_parameter("hist_interval_upper ",
     &ref_hist_interval_upper)
            write(*,*) "FIX: hist_bin_max=",hist_bin_max
            write(*,*) "FIX: hist_min_lower=",hist_min_lower,
     &"/hist_min_upper=",hist_min_upper
            write(*,*) "-------->"
            write(*,*) "WILL CHANGE: hist_interval_lower=",
     &hist_interval_lower,"/hist_interval_upper=",hist_interval_upper
            call show_parameter(0,"quit ")
            write(*,*) " 1.Use referenced setting= ",
     &ref_hist_max_lower
            call show_parameter(2,"hist_max_lower ")
            call show_parameter(3,"hist_max_upper ")
            read(*,*)sel2
            if(sel2.eq."1")then
              call update_parameter("hist_max_lower ",
     &ref_hist_max_lower)
              call update_parameter("hist_max_upper ",
     &ref_hist_max_upper)
            endif
            if(sel2.eq."2")call edit_parameter("hist_max_lower ")
            if(sel2.eq."3")call edit_parameter("hist_max_upper ")
          enddo
        endif
        if(sel.eq."7")then
          sel2="99"
          do while(sel2.ne."0")
            write(*,*) "You could use either hist_max or hist_interval"
            write(*,*) "as a dominant parameter"
            write(*,*) "Important!!"
            write(*,*) "If you change this setting"
            write(*,*) ", it will influence the value of hist_max"
            ref_hist_max_lower=hist_min_lower+dble(hist_bin_max)*
     &hist_interval_lower
            ref_hist_max_upper=hist_min_upper+dble(hist_bin_max)*
     &hist_interval_upper
            call update_parameter("hist_max_lower ",
     &ref_hist_max_lower)
            call update_parameter("hist_max_upper ",
     &ref_hist_max_lower)
            write(*,*) "FIX: hist_bin_max=",hist_bin_max
            write(*,*) "FIX: hist_min_lower=",hist_min_lower,
     &"/hist_min_upper=",hist_min_upper
            write(*,*) "--------->"
            write(*,*) "WILL CHANGE: hist_max_lower=",hist_max_lower,
     &"/hist_max_upper=",hist_max_upper
            call show_parameter(0,"quit ")
            write(*,*) " 1.Use referenced setting= ",
     &ref_hist_interval_lower
            call show_parameter(2,"hist_interval_lower ")
            call show_parameter(3,"hist_interval_upper ")
            read(*,*)sel2
            if(sel2.eq."1")then
              call update_parameter("hist_interval_lower ",
     &ref_hist_interval_lower)
              call update_parameter("hist_interval_upper ",
     &ref_hist_interval_upper)
            endif
            if(sel2.eq."2")then
              write(*,*) "The referenced value is ",hist_interval_lower
              call edit_parameter("hist_interval_lower ")
            endif
            if(sel2.eq."3")then
              write(*,*) "The referenced value is ",hist_interval_upper
              call edit_parameter("hist_interval_upper ")
            endif
          enddo
        endif
        if(sel.eq."8")then
          write(*,*) "Please select the output method"
          write(*,*) " 1. Output additional octave headers"
          write(*,*) " 2. Output sophai file only"
          call edit_parameter("tester_output_flag ")
        endif
        if(sel.eq."9")then
          call check_range("man")
        endif
      enddo
      return
      end

      subroutine check_range(access_method)
      implicit none  !if access_method=man, input the file name manually, if access_method=aut, read the data from source
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/tester/hist.h"
      include "../../include/tools/histogram.h"
      integer read_loop,init_id,final_id,delta_id
      real*8 min_temp,max_temp,read_value(100000),norm_vector
      character dummy*1,sel*2,sel2*2,file_name1*80,file_name2*80,
     &file_name3*80,index_char4*4,access_method*3
      character*3 read_char_temp(100)
      write(*,*) "1. Scan a bunch of files by changing the index"
      write(*,*) "2. Scan a single file"
      read(*,*) sel2
      if(sel2.eq."1")then
        write(*,*) "Please input the fixed name before the index"
        read(*,*) file_name1
        write(*,*) "Please input the fixed name after the index"
        read(*,*) file_name2
        write(*,*) "Please input the index range from "
        read(*,*) init_id
        write(*,*) "to"
        read(*,*) final_id
        write(*,*) "with the interval"
        read(*,*) delta_id
        read_loop=(final_id-init_id)/delta_id+1
      else if(sel2.eq."2")then
        read_loop=1
        init_id=1
        final_id=1
        delta_id=1
50      write(*,*) "Please input the file name"
        read(*,*) file_name1
        file_name2=" "
        call check_file_exist(file_name1)
        if(.not.file_exist)then
          write(*,*) "1. Try again"
          write(*,*) "2. Don't try. Return."
          read(*,*) sel
          if(sel.eq."2")then
            return
          else
            goto 50
          endif
        else
          write(*,*) "Locate the file=",file_name1
        endif
      endif
      write(*,*) "Please select the file type"
      write(*,*) "1. ufe"
      write(*,*) "2. ufv,ufg,ufx (will calculate the norm)"
      write(*,*) "3. scr"
      write(*,*) "4. dat"
      read(*,*) sel
      write(*,*) "Searching for minimum and maximum..."
      min_temp=999999999.D0
      max_temp=-999999999.D0
      do 999 J0=init_id,final_id,delta_id
        if(sel2.eq."1")then
          call int2char4(J0,index_char4)
          file_name=file_name1(:index(file_name1," ")-1)//index_char4//
     &file_name2(:index(file_name2," ")-1)
        elseif(sel2.eq."2")then
          file_name=file_name1(:index(file_name1," ")-1)
        endif
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*) "Skip! Cannot find ",file_name
          goto 999
        endif
        dummy="#"
        if(sel.eq."1")then
          call read_unformatted_header(file_name,"file_x_dim ")
          call read_unformatted_header(file_name,"file_y_dim ")
          open(40,file=file_name,form="unformatted",status="old")
          read(40)
          do I0=1,file_x_dim
            read(40) (read_value(I1),I1=1,file_y_dim)
            do I2=1,file_y_dim
              if(read_value(I2).ge.max_temp)max_temp=read_value(I2)
              if(read_value(I2).le.min_temp)min_temp=read_value(I2)
            enddo
          enddo
          write(*,*) "Done ",file_name
        elseif(sel.eq."2")then
          call read_unformatted_header(file_name,"file_x_dim ")
          call read_unformatted_header(file_name,"file_y_dim ")
          open(40,file=file_name,form="unformatted",status="old")
          read(40)
          do I0=1,file_x_dim
C            read(40) (read_char_temp(I1),I1=1,file_y_dim)
C            backspace(40)
            read(40) (read_value(I1),I1=1,file_y_dim)
C            do I2=1,file_y_dim
C              if(read_char_temp(I2).eq."NaN")then
C                write(*,*)I2,read_char_temp(I2)
C                pause
C              endif
C            enddo
            do I2=1,file_y_dim/3
              I3=I2*3
              norm_vector=
     &(read_value(I3-2)**2+read_value(I3-1)**2+read_value(I3)**2)
              if(norm_vector.ge.max_temp)max_temp=norm_vector
              if(norm_vector.le.min_temp)min_temp=norm_vector
            enddo
          enddo
          min_temp=dsqrt(min_temp)
          max_temp=dsqrt(max_temp)
          write(*,*) "Done ",file_name
        elseif(sel.eq."3")then
          open(40,file=file_name,form="formatted",status="old")
          do while(dummy.eq."#")
            read(40,*)dummy
          enddo
          backspace(40)
99        read(40,*,end=100) dummy,read_value(1)
          if(read_value(1).ge.max_temp)max_temp=read_value(1)
          if(read_value(1).le.min_temp)min_temp=read_value(1)
          goto 99    
100       write(*,*) "Done ",file_name      
        elseif(sel.eq."4")then
          open(40,file=file_name,form="formatted",status="old")
          do while(dummy.eq."#")
            read(40,*)dummy
          enddo
          backspace(40)
101       read(40,*,end=102) read_value(1)
          if(read_value(1).ge.max_temp)max_temp=read_value(1)
          if(read_value(1).le.min_temp)min_temp=read_value(1)
          goto 101
102       write(*,*) "Done ",file_name
        endif
        write(*,*) "The minimum= ",min_temp
        write(*,*) "The maximum= ",max_temp
999   continue
      write(*,*) "Output the range of histogram to hist_range.dat"
      open(60,file="hist_range.dat",access="append")
      write(60,*) "For ",file_name1(:index(file_name1," ")-1)//
     &file_name2(:index(file_name2," ")-1)
      write(60,*) "init_id=",init_id," final_id=",final_id,
     &" delta_id=",delta_id,"min=",min_temp," max=",max_temp
      close(60)
      write(*,*) "The minimum is found=",min_temp,
     &". Current hist_min is ",hist_min_lower
      write(*,*) "The maximum is found=",max_temp,
     &". Current hist_max is ",hist_max_lower
      write(*,*) "Would you like to ..."
      write(*,*) " 0. Do nothing"
      write(*,*) " 1. Overwrite the minimum"
      write(*,*) " 2. Overwrite the maximum"
      write(*,*) " 3. Overwrite both"
      read(*,*) sel
      if(sel.eq."1".or.sel.eq."3")then
        write(*,*) "Overwrite the minimum to ",
     &min_temp-(hist_interval_lower*2.D0)
        call update_parameter("hist_min_lower ",
     &(min_temp-hist_interval_lower*2.D0))
        call update_parameter("hist_min_upper ",
     &(min_temp-hist_interval_upper*2.D0))
        call update_parameter("hist_min_delta ",0.00001D0)
      endif
      if(sel.eq."2".or.sel.eq."3")then
        write(*,*) "Overwrite the maximum to ",
     &max_temp+(hist_interval_lower*2.D0)
        call update_parameter("hist_max_lower ",
     &(max_temp+hist_interval_lower*2.D0))
        call update_parameter("hist_max_upper ",
     &(max_temp+hist_interval_upper*2.D0))
        call update_parameter("hist_max_delta ",1.D0)
        call update_parameter("hist_interval_lower ",
     &((hist_max_lower-hist_min_lower)/dble(hist_bin_max)))
        call update_parameter("hist_interval_upper ",
     &((hist_max_upper-hist_min_upper)/dble(hist_bin_max)))
        call update_parameter("hist_interval_delta ",1.D0)
        write(*,*) "Overwrite the hist_interval to ",hist_interval_lower
      endif
      if(sel.eq."1")then
        call update_parameter("hist_max_lower ",
     &(hist_min_lower+hist_interval_lower*dble(hist_bin_max)))
        call update_parameter("hist_max_upper ",
     &(hist_min_upper+hist_interval_upper*dble(hist_bin_max)))
        call update_parameter("his_max_delta ",1.D0) 
        write(*,*) "Overwrite the hist_interval to ",hist_interval_lower
      endif
      close(40)
      return
      end

      subroutine write_hist
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist.h"
      include "../../include/tools/histogram.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x,
     &A10,1x,I3,1x,
     &A9,1x,I5,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,
     &A10,1x,F12.1,1x,
     &A11,1x,F15.1,1x,
     &A11,1x,F10.1,1x,
     &A12,1x,I1,1x,
     &A15,1x,F17.7,1x,
     &A15,1x,F17.7,1x,
     &A15,1x,F17.7,1x,
     &A15,1x,F17.7,1x,
     &A15,1x,F17.7,1x,
     &A15,1x,F17.7,1x,
     &A20,1x,F17.7,1x,
     &A20,1x,F17.7,1x,
     &A20,1x,F17.7,1x,
     &A12,1x,I2,1x,
     &A17,1x,I2,1x,
     &A17,1x,I2,1x,
     &A12,1x,A20,1x,
     &A12,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1,1x,
     &A24,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1)")

     &21,7,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"delta_loop=",delta_loop,
     &"hist_method=",hist_method,
     &"hist_min_lower=",hist_min_lower,
     &"hist_min_upper=",hist_min_upper,
     &"hist_min_delta=",hist_min_delta,
     &"hist_max_lower=",hist_max_lower,
     &"hist_max_upper=",hist_max_upper,
     &"hist_max_delta=",hist_max_delta,
     &"hist_interval_lower=",hist_interval_lower,
     &"hist_interval_upper=",hist_interval_upper,
     &"hist_interval_delta=",hist_interval_delta,
     &"output_flag=",tester_output_flag,
     &"source_file_flag=",source_file_flag,
     &"header_file_flag=",header_file_flag,
     &"tester_flag=",tester_flag,
     &"source_type=",source_type,
     &"source_file_type=",source_file_type,
     &"source_file_name= '",source_file_name,"'",
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
