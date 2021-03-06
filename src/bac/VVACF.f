*========================================================================
* File Name : GVACF.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月01日 (週四) 16時56分47秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine GVACF   !A general Vector Autocorrelation Function
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/VACF/VACF.h" 
      integer idelay,ishift_step,shift_num,frame_num
      real*8 real_time
      real*8 theta,theta_t,dummy3
      real*8 f_ave(-observe_time_step_max:observe_time_step_max),
     &f_i(-observe_time_step_max:observe_time_step_max),
     &F_real_ave(-observe_time_step_max:observe_time_step_max),
     &F_img_ave(-observe_time_step_max:observe_time_step_max),
     &F_real_i(-observe_time_step_max:observe_time_step_max),
     &F_img_i(-observe_time_Step_max:observe_time_step_max),
     &F_real(-observe_time_step_max:observe_time_step_max,1:
     &atom_num_max),
     &F_img(-observe_time_Step_max:observe_time_step_max,1:atom_num_max)
C============modified============================
      integer ishift,idelta,ishift_step_buffer
      real*8 vv0(atom_num_max),vv(atom_num_max),mean_vv0,mean_vv
      real*8 ct(atom_num_max),mean_ct
C================================================
      character*4 int2char_temp,int2char_atom_num
      character dummy1*2,dummy2*11,dummy4*4,dummy5*6,dummy6*12
      integer dummy_int1,dummy_int2
      file_name=
     &VACF_input_name(:index(VACF_input_name,".")-1)//".vec"
      call check_file_exist(file_name)
      if(file_exist)then
        if(wscreen)
     &write(*,"(I5,1x,A10,1x,A80)") myid,"Read file=",file_name
      else 
        write(*,"(I5,1x,A23,A80)")
     &myid,"Skip job, can not find ",file_name
        return
      endif
      open(20,file=file_name,status="old")
      read(20,*)dummy1,VACF_total_time_step,dummy5,atom_num
      call int2char4(atom_num,int2char_atom_num)
      call int2char4(temp,int2char_temp)
      file_name=
     &VACF_org_vaf_name(:index(VACF_org_vaf_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A22,1x,A80)")
     &myid,"Write original VAF to ",file_name
      open(21,file=file_name,status="replace")
      file_name=
     &VACF_nor_vaf_name(:index(VACF_nor_vaf_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A24,1x,A80)")
     &myid,"Write normalized VAF to ",file_name
      open(22,file=file_name,status="replace")
      write(21,"(A59,I4,A1)")
     &"# real_time org_mean_VAF org_VAFI(atom_1) to org_VAFI(atom_",
     &atom_num,")"
      write(22,"(A59,I4,A1)")
     &"# real_time nor_mean_VAF nor_VAFI(atom_1) to nor_VAFI(atom_",
     &atom_num,")"
C==========Check if VACF_total_time_step is OK===========
200   if(VACF_total_time_step.le.VACF_observe_time_step)then
        VACF_observe_time_step=VACF_total_time_step-1
        if(wscreen)write(*,"(I5,1x,A34,1x,I13)") 
     &myid,"observe_time_step exceeded! Fix to",VACF_observe_time_step
      endif  
      if(VACF_observe_time_step.lt.VACF_window_time_step)then
        VACF_window_time_step=VACF_observe_time_step
        if(wscreen)write(*,"(I5,1x,A33,1x,I13)")
     &myid,"window_time_step exceeded! Fix to",VACF_window_time_step
      endif
      if(atom_num*VACF_total_time_step.gt.time_step_max)then
        if(wscreen)write(*,"(I5,1x,A38)")
     &myid,"Insufficient array for storing vectors"
        return
      endif     
      if(wscreen)then
        write(*,"(I5,1x,A16,1x,I13,1x,A18,1x,I13)")
     &myid,"Total time step=",
     &VACF_total_time_step,"Observe time step=",VACF_observe_time_step
        write(*,"(I5,1x,A25,1x,I5)") 
     &myid,"Total elements for GVACF=",atom_num
        write(*,"(I5,1x,A43,1x,F7.2)") 
     &myid,"Start calculate GVACF with parallel label= ",temp
      endif
C==========Start reading source file up to VACF_observe_time_step=========
      if(wscreen)write(*,"(I5,1x,A25)") 
     &myid,"Start reading source file"
      I4=0
      do I1=1,VACF_total_time_step
        read(20,*) dummy5,frame_num !dymmy5="frame="
        do I2=1,atom_num
          I3=I2*3
          read(20,*) dummy1,v(I3-2),v(I3-1),v(I3)      
          I4=I4+1
          vx(I4)=v(I3-2)
          vy(I4)=v(I3-1)
          vz(I4)=v(I3)    
C          write(*,*)I4,vx(I4),vy(I4),vz(I4)
        enddo
      enddo
C=======Complete the reading up to VACF_total_time_step===
      if(wscreen)write(*,"(I5,1x,A23)") 
     &myid,"Start calculating GVACF"
C      ishift_step=VACF_total_time_step-VACF_observe_time_step+1  !The maximal shifted steps f
      ishift_step=VACF_total_time_step    !empty buffer
      shift_num=(VACF_observe_time_step-1)*atom_num
C============Modified for unlimited array=================
C============Before empty buffer==========================
      mean_ct=0.D0
      mean_vv0=0.D0
      do I2=1,atom_num   !Calculate vv0
        vv0(I2)=0.D0
        ct(I2)=0.D0
        do I3=1,ishift_step
            ishift=(I3-1)*atom_num+I2
            vv0(I2)=vv0(I2)
     &+vx(ishift)*vx(ishift)
     &+vy(ishift)*vy(ishift)
     &+vz(ishift)*vz(ishift)
        enddo
        vv0(I2)=vv0(I2)/dble(ishift_step)
        mean_vv0=mean_vv0+vv0(I2)
        ct(I2)=vv0(I2)
      enddo
      mean_vv0=mean_vv0/dble(atom_num)
      mean_ct=mean_vv0
        write(21,"(F14.6,(1001(1x,F14.7)))") 0.D0,mean_vv0,
     &(vv0(I2),I2=1,atom_num)
        write(22,"(F14.6,(1001(1x,F14.7)))") 0.D0,1.D0,
     &(vv0(I2)/vv0(I2),I2=1,atom_num)
C==========================================================
      do I1=1,VACF_observe_time_step  !Calculate vv1,vv2,vv3...
        idelta=atom_num*I1
        mean_vv=0.D0
        do I2=1,atom_num
          vv(I2)=0.D0
C         ishift_step_buffer=ishift_step      !withoug empty buffer         
          ishift_step_buffer=ishift_step-I1   !empty buffer
          do I3=1,ishift_step_buffer
            ishift=(I3-1)*atom_num+I2
            vv(I2)=vv(I2)
     &+vx(ishift)*vx(ishift+idelta)
     &+vy(ishift)*vy(ishift+idelta)
     &+vz(Ishift)*vz(Ishift+idelta)
          enddo
          vv(I2)=vv(I2)/dble(ishift_step_buffer)
          mean_vv=mean_vv+vv(I2)
          ct(I2)=ct(I2)+vv(I2)
        enddo
        mean_vv=mean_vv/dble(atom_num)
        mean_ct=mean_ct+mean_vv
        real_time=dble(I1)*VACF_delta_time
        write(21,"(F14.8,(1001(1x,F14.8)))") real_time,mean_vv,
     &(vv(I2),I2=1,atom_num)
C==============End of no empty buffer=======================
C==============Normalized GVACF=============================
        do I2=1,atom_num
          vv(I2)=vv(I2)/vv0(I2)
          mean_vv=mean_vv/mean_vv0
        enddo
        write(22,"(F14.6,(1001(1x,F14.7)))") real_time,mean_vv,
     &(vv(I2),I2=1,atom_num)
      enddo
C=============Output Correlation time======================
      file_name=
     &VACF_diff_name(:index(VACF_diff_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A26,1x,A80)")
     &myid,"Write correlation time to ",file_name
      open(23,file=file_name,status="replace")
      write(23,"(A29,1x,I4,1x,A21)")
     &"# correlation_time_for_vector",atom_num,
     &"mean_correlation_time"
      do I2=1,atom_num
        write(23,"(I5,1x,F20.10,1x,F20.10)")I2,ct(I2),
     &mean_ct
      enddo
      file_name=
     &VACF_diff_dat_name(:index(VACF_diff_dat_name," ")-1)
      call wait_till_file_close(file_name)
      open(24,file=file_name,access="append")
      write(24,"(A38)") "# label mean_correlation_time time_label"
      write(24,"((2(F14.6,1x)),F14.6)")
     &temp,mean_ct,time_label
      close(20)
      close(21)
      close(22)
      close(23)
      close(24)
      return
C========Power Spectrum transformation DFT=========================
      file_name=
     &VACF_org_vaf_name(:index(VACF_org_vaf_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Write DFT to ",file_name
      open(30,file=file_name,status="old")
      read(30,*)         ! first line is the definition
      do I1=0,VACF_observe_time_step-1
C        read(30,*)dummy3,vcf(I1),(vsq(I1,I2),I2=1,atom_num)
      enddo
      close(30)
      if(wscreen)write(*,"(I5,1x,A10,1x,A80)")
     &myid,"Read file=",file_name
C=================End of manual input==========================
      do I1=0,VACF_window_time_step
C        f_ave(I1)=vcf(I1)
C        f_ave(-I1)=vcf(I1)
      enddo
      do I1=VACF_window_time_step+1,VACF_observe_time_step-1
        f_ave(I1)=0.D0
        f_ave(-I1)=0.D0
      enddo
      call DFT(f_ave,F_real_ave,F_img_ave)
      if(wscreen)write(*,"(I5,1x,A29)")
     &myid,"DFT for average VAF completed"
      do I0=atom_num,1,-1
        do I1=0,VACF_window_time_step
C          f_i(I1)=vsq(I1,I0)
C          f_i(-I1)=vsq(I1,I0)
        enddo
        do I1=VACF_window_time_step+1,VACF_observe_time_step-1
          f_i(I1)=0.D0
          f_i(-I1)=0.D0
        enddo
        call DFT(f_i,F_real_i,F_img_i)
        do I1=-(VACF_observe_time_step-1),VACF_observe_time_step-1
          F_real(I1,I0)=F_real_i(I1)
          F_img(I1,I0)=F_img_i(I1)     
C          write(*,*) I0,I1,F_real(I1,I0),F_img(I1,I0) 
        enddo
        if(wscreen)write(*,"(I5,1x,A12,1x,I4,1x,A9)")
     &myid,"DFT for atom",I0,"completed"
      enddo
      if(wscreen)write(*,"(I5,1x,A22)")
     &myid,"DFT for VAFI completed"
C=============Output DFT==========================================
      pi=read_unit("Pi ","Pi ","1 ")
      theta=2.D0*pi/dble(2.D0*VACF_observe_time_step+1)   !w=(2*pi*k)/N and F=F*dt 
      theta_t=theta/VACF_delta_time
      file_name=
     &VACF_real_dft_name(:index(VACF_real_dft_name," ")-1)
      open(25,file=file_name,status="replace")
      file_name=
     &VACF_img_dft_name(:index(VACF_img_dft_name," ")-1)
      open(26,file=file_name,status="replace")
      write(25,"(A62,I5,A1)")
     &"# freq_in_rad DFT_mean_real DFT_real_atom(1) to DFT_real_atom("
     &,atom_num,")"
      write(26,"(A65,I5,A1)")
     &"# freq_in_rad DFT_mean_image DFT_image_atom(1) to DFT_image_atom(
     &",atom_num,")"
C      do I1=-(VACF_observe_time_step-1),VACF_observe_time_step-1
      do I1=0,VACF_observe_time_step-1
        if(dabs(F_real_ave(I1)).gt.0.001D0)then
          write(25,102) theta_t*dble(I1),F_real_ave(I1)*VACF_delta_time,
     &(F_real(I1,I0)*VACF_delta_time,I0=1,atom_num)
        endif
      enddo
      do I1=-(VACF_observe_time_step-1),VACF_observe_time_step-1
        if(dabs(F_img_ave(I1)).gt.0.001D0)then
           write(26,102) theta_t*dble(I1),F_img_ave(I1)*VACF_delta_time,
     &(F_img(I1,I0)*VACF_delta_time,I0=1,atom_num)
        endif
      enddo
102   format(F14.7,1x,F14.7,1000(1x,F14.7))
      close(25)
      close(26)
C========End of DFT and output======================================
C=============VACF Option:Output and delete xyz===================
      if(VACF_option.eq.1)then   !Delete recorded xyz file only!
        if(wscreen)write(*,"(I5,1x,A32)")myid,
     &"Start deleting recorded xyz file"        
        file_name=
     &VACF_input_name(:index(VACF_input_name," ")-1)
        call clean_file(file_name)
      endif
      if(VACF_option.eq.2)then   !Delete recorded xyz file and rename VACF output
        if(wscreen)write(*,"(I5,1x,A32)")myid,
     &"Start deleting recorded xyz file"        
        file_name=
     &VACF_input_name(:index(VACF_input_name," ")-1)
        call clean_file(file_name)
        if(wscreen)write(*,"(I5,1x,A32)")myid,
     &"Start renaming VACF output files"        
        call GVACF_rename
      endif
      if(VACF_option.eq.3)then   !Rename VACF output only(add time label)
        if(wscreen)write(*,"(I5,1x,A32)")myid,
     &"Start renaming VACF output files" 
        call GVACF_rename
      endif
C=================================================================
      return
      end

      subroutine GVACF_rename
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/VACF/VACF.h"
      include "../../include/BIMD/BIMD.h"
      character*80 source_name,modified_name
      integer temp_int
      temp_int=dint(temp)
      call int2char4(temp_int,int2char)
      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_org.vaf"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_org_"//
     &time_label_char//".vaf"
      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_nor.vaf"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_nor_"//
     &time_label_char//".vaf"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dft_path(:index(dft_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_real.dft"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dft_path(:index(dft_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_real_"
     &//time_label_char//".dft"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dft_path(:index(dft_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_img.dft"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dft_path(:index(dft_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_img_"//
     &time_label_char//".dft"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &diff_path(:index(diff_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//".diff"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &diff_path(:index(diff_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_"//
     &time_label_char//".diff"

      call rename(source_name,modified_name)
C=========Preserve *_diff.dat file===========================
C      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
C     &diff_path(:index(diff_path," ")-1)//
C     &source_file(:index(source_file," ")-1)//"_diff.dat"

C      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
C     &diff_path(:index(diff_path," ")-1)//
C     &source_file(:index(source_file," ")-1)//"_diff_"//
C     &time_label_char//".dat"

C      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &hist_path(:index(hist_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.dat"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &hist_path(:index(hist_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist_"//time_label_char//".dat"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &hist_path(:index(hist_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.xyz"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &hist_path(:index(hist_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist_"//time_label_char//".xyz"

      call rename(source_name,modified_name)

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
