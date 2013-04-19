*========================================================================
* File Name : VACF.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 13 Oct 2010 04:08:09 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine VACF
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/VACF/VACF.h" 
      integer idelay,ishift_step,shift_num
      real*8  vcf(0:observe_time_step_max),
     &vsq(0:observe_time_step_max,1:atom_num),
     &vcf_nor(0:observe_time_step_max),
     &vsq_nor(0:observe_time_step_max,1:atom_num)
      real*8 vcf_max,vsq_max,real_time
      real*8 theta,theta_t,dummy3
      real*8 f_ave(-observe_time_step_max:observe_time_step_max),
     &f_i(-observe_time_step_max:observe_time_step_max),
     &F_real_ave(-observe_time_step_max:observe_time_step_max),
     &F_img_ave(-observe_time_step_max:observe_time_step_max),
     &F_real_i(-observe_time_step_max:observe_time_step_max),
     &F_img_i(-observe_time_Step_max:observe_time_step_max),
     &F_real(-observe_time_step_max:observe_time_step_max,1:atom_num),
     &F_img(-observe_time_Step_max:observe_time_step_max,1:atom_num)
      character*4 int2char_temp,int2char_atom_num
      character dummy1*2,dummy2*11,dummy4*4,dummy5*6,dummy6*12
      integer dummy_int1,dummy_int2
C      if(VACF_method.eq.2)call histogram_pot_init
      file_name=
     &VACF_input_name(:index(VACF_input_name," ")-1)
      call check_file_exist(file_name)
      if(file_exist)then
        if(wscreen)
     &write(*,"(I5,1x,A10,1x,A80)") myid,"Read file=",file_name
      else 
        return
      endif
C==========Quick jump to 200===================
C      VACF_total_time_step=1000000
C      goto 200
C==============================================
      if(source_file_flag.eq.1.or.source_file_flag.eq.2)then
        open(19,file=file_name,status="old")
        read(19,*)dummy_int1,dummy5,dummy_int2,dummy6,
     &VACF_total_time_step
        close(19)
      endif
200   if(VACF_total_time_step.lt.VACF_observe_time_step)then
        VACF_observe_time_step=VACF_total_time_step
        if(wscreen)write(*,"(I5,1x,A34,1x,I13)") 
     &myid,"observe_time_step exceeded! Fix to",VACF_observe_time_step
      endif  
      if(VACF_observe_time_step.lt.VACF_window_time_step)then
        VACF_window_time_step=VACF_observe_time_step
        if(wscreen)write(*,"(I5,1x,A33,1x,I13)")
     &myid,"window_time_step exceeded! Fix to",VACF_window_time_step
      endif
      if(wscreen)write(*,"(I5,1x,A16,1x,I13,1x,A18,1x,I13)")
     &myid,"Total time step=",
     &VACF_total_time_step,"Observe time step=",VACF_observe_time_step
      if(wscreen)write(*,"(I5,1x,A35,1x,F7.2)") 
     &myid,"Start calculate VAF at temperature ",temp
      ishift_step=VACF_total_time_step-VACF_observe_time_step+1
      shift_num=(VACF_observe_time_step-1)*atom_num
      do I1=0,VACF_observe_time_step-1
        vcf(I1)=0.D0
        do I2=1,atom_num
          vsq(I1,I2)=0.D0
        enddo
      enddo
      file_name=
     &VACF_input_name(:index(VACF_input_name," ")-1)
      call wait_till_file_close(file_name)
C      goto 500  !jump to DFT
      if(wscreen)write(*,"(I5,1x,A40,1x,I10)") 
     &myid,"Start read xyz file, observe_time_step= ",
     &VACF_observe_time_step
      open(20,file=file_name,status="old")
      I4=0
      do I1=1,VACF_observe_time_step
        read(20,*)
        read(20,*)dummy4,pot
        do I2=1,atom_num
          I3=I2*3
          read(20,*) dummy1,x(I3-2),x(I3-1),x(I3),dummy2,
     &v(I3-2),v(I3-1),v(I3)          
        enddo
        call centre
C========debug==============
        call centre_velocity
C=======end================
        call fix_angular
C        if(VACF_method.eq.2)call histogram_pot_lmin
C        pause
C        call cn_init
C        call cn
C        call cn2name
C        pause
        do I2=1,atom_num
          I3=I2*3
          I4=I4+1
          vx(I4)=v(I3-2)
          vy(I4)=v(I3-1)
          vz(I4)=v(I3)
        enddo
      enddo
      if(wscreen)write(*,"(I5,1x,A38,1x,I10)") 
     &myid,"Start calculate VAF, total_time_step= ",
     &VACF_total_time_step
      do 100 I1=1,ishift_step
C        if(wscreen.and.mod(I1,10000).eq.0)
C     &write(*,*) I1,"'shift,total=",ishift_step
        do I2=0,VACF_observe_time_step-1
          do I3=1,atom_num
            idelay=atom_num*I2
            vsq(I2,I3)=vsq(I2,I3)+vx(I3)*vx(I3+idelay)
            vsq(I2,I3)=vsq(I2,I3)+vy(I3)*vy(I3+idelay)
            vsq(I2,I3)=vsq(I2,I3)+vz(I3)*vz(I3+idelay)
          enddo
        enddo
        if (I1.eq.ishift_step) goto 100
        do I4=atom_num+1,atom_num*VACF_observe_time_step
          vx(I4-atom_num)=vx(I4)
          vy(I4-atom_num)=vy(I4)
          vz(I4-atom_num)=vz(I4)
        enddo
        read(20,*)
Cdummy_int1,dummy5,dummy_int2
        read(20,*)dummy4,pot
        do I5=1,atom_num
          I6=I5*3
          read(20,*,end=999) dummy1,x(I6-2),x(I6-1),x(I6),dummy2,
     &v(I6-2),v(I6-1),v(I6)
        enddo
999     call centre
C==============debug=============
        call centre_velocity
C============end=================
        call fix_angular
C        if(VACF_method.eq.2)call histogram_pot_lmin
        do I5=1,atom_num
          I6=I5*3
          vx(shift_num+I5)=v(I6-2)
          vy(shift_num+I5)=v(I6-1)
          vz(shift_num+I5)=v(I6)
        enddo
100   continue
      do I1=0,VACF_observe_time_step-1
        do I2=1,atom_num
          vcf(I1)=vcf(I1)+vsq(I1,I2)
          vsq(I1,I2)=vsq(I1,I2)/dble(ishift_step)
        enddo
        vcf(I1)=vcf(I1)/dble(atom_num*(ishift_step))
      enddo
C==============Normalized VACF=======================
      if(wscreen)write(*,"(I5,1x,A28)") 
     &myid,"Start normalize VAF and VAFI"
      do I1=0,VACF_observe_time_step-1
        vcf_nor(I1)=vcf(I1)/vcf(0)
        do I2=1,atom_num
          vsq_nor(I1,I2)=vsq(I1,I2)/vsq(0,I2)
        enddo
      enddo
C=================End of Normalized===================
C=================Start of Output=====================
      call int2char4(atom_num,int2char_atom_num)
      call int2char4(temp,int2char_temp)
      file_name=
     &VACF_org_vaf_name(:index(VACF_org_vaf_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A22,1x,A80)") 
     &myid,"Write original VAF in ",file_name
      open(21,file=file_name,status="replace")
      file_name=
     &VACF_nor_vaf_name(:index(VACF_nor_vaf_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A24,1x,A80)") 
     &myid,"Write normalized VAF in ",file_name
      open(22,file=file_name,status="replace")
      write(21,"(A59,I4,A1)")
     &"# real_time org_mean_VAF org_VAFI(atom_1) to org_VAFI(atom_",
     &atom_num,")" 
      write(22,"(A59,I4,A1)")
     &"# real_time nor_mean_VAF nor_VAFI(atom_1) to nor_VAFI(atom_",
     &atom_num,")"
      do I1=0,VACF_observe_time_step-1
        real_time=dble(I1)*VACF_delta_time
        write(21,"(F14.8,(1001(1x,F14.8)))") real_time,vcf(I1),
     &(vsq(I1,I2),I2=1,atom_num)
        write(22,"(F14.8,(1001(1x,F14.8)))") real_time,vcf_nor(I1)
     &,(vsq_nor(I1,I2),I2=1,atom_num)
      enddo
      close(20)
      close(21)
      close(22)
C========Calculating Diffusion constant============================
1000  if(wscreen)write(*,"(I5,1x,A36)") 
     &myid,"Start calculating Diffusion Constant"
      file_name=
     &VACF_diff_name(:index(VACF_diff_name," ")-1)
      open(33,file=file_name,status="replace")
      do I1=0,VACF_observe_time_step-1
        diffusion_vcf=diffusion_vcf+vcf(I1)
      enddo
      do I0=1,atom_num
        do I1=0,VACF_observe_time_step-1
          diffusion_vsq(I0)=diffusion_vsq(I0)+vsq(I1,I0)
        enddo
        write(33,"(A31,A2,1x,A20)")
     &"# atom_num diffusion_const_for_",atom_name(I0),
     &"mean_diffusion_const"
        write(33,"(I5,1x,F20.10,1x,F20.10)")I0,diffusion_vsq(I0)/3.D0,
     &diffusion_vcf/3.D0
      enddo
      close(33)
      file_name=
     &VACF_diff_dat_name(:index(VACF_diff_dat_name," ")-1)
       call wait_till_file_close(file_name)
       open(34,file=file_name,access="append")
       write(34,"(A38)") "# temp mean_diffusion_const time_label"
       write(34,"((2(F13.8,1x)),F15.6)")
     &temp,diffusion_vcf/3.D0,time_label
       close(34)
C========End of Diffusion Constant=================================
C========Power Spectrum transformation DFT=========================
500   if(wscreen)write(*,"(I5,1x,A21)") myid,"Start calculating DFT"
C===================Manual input===================================
      file_name=
     &VACF_org_vaf_name(:index(VACF_org_vaf_name," ")-1)
      open(30,file=file_name,status="old")
      read(30,*)         ! first line is the definition
      do I1=0,VACF_observe_time_step-1
        read(30,*)dummy3,vcf(I1),(vsq(I1,I2),I2=1,atom_num)
      enddo
      close(30)
      if(wscreen)write(*,"(I5,1x,A10,1x,A80)")
     &myid,"Read file=",file_name
C=================End of manual input==========================
      do I1=0,VACF_window_time_step
        f_ave(I1)=vcf(I1)
        f_ave(-I1)=vcf(I1)
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
          f_i(I1)=vsq(I1,I0)
          f_i(-I1)=vsq(I1,I0)
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
102   format(F14.8,1x,F14.8,1000(1x,F14.8))
      close(25)
      close(26)
C========End of DFT and output======================================
C=============Writing Energy Gistogram================
C      if(VACF_method.eq.2)then
C        if(wscreen)write(*,"(I5,1x,A30)")
C     &myid,"Start writing energy histogram"
C        call write_histogram_pot_lmin
C      endif
C=============End of Writing Energy Histogram=========
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
        call VACF_rename
      endif
      if(VACF_option.eq.3)then   !Rename VACF output only(add time label)
        if(wscreen)write(*,"(I5,1x,A32)")myid,
     &"Start renaming VACF output files" 
        call VACF_rename
      endif
C=================================================================
      return
      end

      subroutine VACF_rename
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
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_real.dft"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_real_"
     &//time_label_char//".dft"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_img.dft"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_img_"//
     &time_label_char//".dft"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dtf_path(:index(dtf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//".diff"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dtf_path(:index(dtf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_"//
     &time_label_char//".diff"

      call rename(source_name,modified_name)
C=========Preserve *_diff.dat file===========================
C      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
C     &dtf_path(:index(dtf_path," ")-1)//
C     &source_file(:index(source_file," ")-1)//"_diff.dat"

C      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
C     &dtf_path(:index(dtf_path," ")-1)//
C     &source_file(:index(source_file," ")-1)//"_diff_"//
C     &time_label_char//".dat"

C      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.dat"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist_"//time_label_char//".dat"

      call rename(source_name,modified_name)

      source_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.xyz"
      modified_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
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
