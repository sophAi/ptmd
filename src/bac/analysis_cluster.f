*========================================================================
* File Name : analysis_cluster.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時50分12秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine analysis_cluster(ibin,ndim)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/ensemble.h"
       include "../../include/mbh_ndim.h"
       include "../../include/mbh_ibin.h"
       include "../../include/file_tools.h"
       include "../../include/neighbors.h"
       include "../../include/analysis.h"
       radius=999999.D0
       radius2=radius*radius   !!radius2 is required for gradient critirion
       write(*,*) "@ Analysis mode:"
40     write(*,*) "========================================="
       write(*,*) "1.potential and gradient of xyz"
       write(*,*) "2.Internal strain"
       write(*,*) "3.Common neighbor"
       write(*,*) "4.Mixing energy and Order parameter"
       write(*,*) "5.<<Stop program"
       write(*,*) "========================================="
       read(*,*) sel_1
       if(sel_1.eq.1)then
         call xyz_check
       else if(sel_1.eq.2)then
C         call xyz_internal_strain(ibin,ndim)
       else if(sel_1.eq.3)then
         call xyz_common_neighbor
       else if(sel_1.eq.4)then
C        call xyz_mixing_order(ibin,ndim)
       else if(sel_1.eq.5)then
         write(*,*) "End of analysis"
         stop
       end if
       goto 40
       return
       end

       subroutine xyz_check
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/ensemble.h"
       include "../../include/mbh_ndim.h"
       include "../../include/mbh_ibin.h"
       include "../../include/file_tools.h"
       include "../../include/neighbors.h"
       include "../../include/analysis.h"
       integer total_bin,max_step,kbin,nor_hist,nor_lhist
       parameter(total_bin=1000,max_step=1000000)
       real*8 total_bond
       real*8 ene_hist(max_step),lmin_hist(max_step),
     &ene_max,ene_min,ene_de,ene_bin
       integer hist(total_bin),lhist(total_bin)
       real*8 ene_hist_kbin(total_bin),lmin_hist_kbin(total_bin)
       integer move_to_center,count_in_file,file_index1,file_index2
       character dummy1*2
19     format(I5,1x,A7,1x,I7,1x,A16,1x,I7,1x,A10,1x,A30,1x,A16,1x,I7)
20     format(A4,1x,F19.12,1x,A6,1x,F19.12,1x,A13,1x,I4,1x,A5,1x,F15.6,
     &1x,A31)
21     format(A2,1x,F11.6,1x,F11.6,1x,F11.6,1x,A12,1x,F12.5,1x,F12.5,1x,
     &F12.5,1x,F10.5,1x,F12.5,1x,I5,1x,I3)
22     format(A2,1x,F11.6,1x,F11.6,1x,F3.1,1x,A12,1x,F12.5,1x,
     &F12.5,1x,F3.1,1x,F10.5,1x,F12.5,1x,I5,1x,I3)
23     format(I7,1x,F15.7,1x,F15.7,1x,F20.10,1x,I7,1x,I7,3x,A30)
C============INPUT FILE=========================================
       ene_max=-99999999.D0
       ene_min=99999999.D0
       nor_hist=0
       nor_lhist=0
       do J0=1,total_bin
         hist(J0)=0
         lhist(J0)=0
         ene_hist_kbin(J0)=0.D0
         lmin_hist_kbin(J0)=0.D0
       enddo
       write(*,*)"1.Read only one file(includes histogram file)"
       write(*,*)"2.Read more than one file(include many histogram file"
       read(*,*) sel_1
       write(*,*)"Write all data in a file?(y/n)"
       read(*,*)dummy_load1
       write(*,*) "Transform to the local minimun?(y/n)"
       read(*,*) tran_to_min
       if(dummy_load1.eq."y")then
         write(*,*) "Please input the name of the output file(complete)"
         read(*,*) file_name2
         write(*,*) 
         write(*,*) "The summary information are stored in ",
     &"tab_"//file_name2," which is an .dat file,not xyz file"
         write(*,*) 
         write(*,*) "1.move all clusters to center of mass"
         write(*,*) "2.Do not move and pretain the original data"
         read(*,*) move_to_center
       endif
       dummy_load2="y"
       count_in_file=0
       file_index1=100
       file_index2=102
       I1=0
       do while(dummy_load2.eq."y")
         count_in_file=count_in_file+1
         write(*,*)
     &"Please input the xyz file name(completed,not partial)"
         read(*,*)file_name1
         call xyz_frame_counter(file_name1,.true.)
         write(*,*) "Please input inital frame(from 1 to ",
     &total_frame,")"
         read(*,*) query_frame_init
         write(*,*) "Please input last frame(from",query_frame_init,
     &" to ",total_frame,")"
         read(*,*) query_frame_end
         if(dummy_load1.eq."y")then
           write(*,*) "Write to frame ",I1," in ",file_name2
           if(count_in_file.le.1)then
             open(file_index1,file=file_name2,status="replace")
             open(file_index2,file="tab_"//file_name2,
     &status="replace")
           else if (count_in_file.gt.1)then
             open(file_index1,file=file_name2,access="append",
     &status="old")
             open(file_index2,file="tab_"//file_name2,
     &access="append",status="old")
           endif
         endif
         open(1414,file=file_name1,status="old")
         do J0=query_frame_init,query_frame_end
           query_frame=J0
C           call xyz_frame_query(file_name1,query_frame,query_xinib,0)
           read(1414,*)
           read(1414,*)
           do J1=1,num_ndim/3
             J2=J1*3
             read(1414,*)dummy1,ana_xinib(J2-2),ana_xinib(J2-1),
     &ana_xinib(J2)
           enddo
C          do J1=1,num_ndim
C             ana_xinib(J1)=query_xinib(J1)
C           enddo
           print_pot_only=.false.
           print_xyz=.false.
C           write(*,*)J0," frames"
           if(ana_xinib(0).eq.0.D0.and.ana_xinib(4).eq.0.D0.and.
     &ana_xinib(7).eq.0.D0)then
C             write(*,*) "Frame ",J0," is empty,skip!"
           else
             I1=I1+1
             call check_pot_grad_vt(num_ndim,ana_pot,ana_xinib,
     &print_pot_only,print_xyz)
             ene_hist(J0)=ana_pot
             if(ana_pot.gt.ene_max)ene_max=ana_pot
             if(ana_pot.lt.ene_min)ene_min=ana_pot
             if(tran_to_min.eq."y".and.mod(J0,50).eq.0) then
               ibin=1000
               BH_SW=2
               print_xyz=.true.
               call LOCAL_MIN(num_ndim,ana_pot,ana_xinib,grad,vt,ibin) 
               write(*,*) "frame=",J0,",After Local min=",ana_pot
               lmin_hist(J0)=ana_pot
C===========================================================
               if((ana_pot.ge.-84.68).and.(ana_pot.le.-84.64
     &))then
                 write(*,*) "HIT!!,pot=",ana_pot
                 pause
                 open(2441,file="local.xyz",access="append")
                 write(2441,19) num_total," frame=",I1,
     &",original frame=",J0,"from file=",file_name1,
     &",Total bond num=",int(total_bond)
               write(2441,20) "Eng=",ana_pot,",bind=",
     &ana_pot/dble(num_total)," and A atoms="
     &,num_A,",RMS=",RMS,",VT(i)-bind,|Grad|,  bond#(i),i"
                 if(move_to_center.eq.1)call centre(ana_xinib,num_total)
                 do J1=1,atom_num_a
                   grad_length=dsqrt(grad(J1*3-2)**2+
     &grad(J1*3-1)**2+grad(J1*3)**2)
                   write(2441,21) kind1,ana_xinib(J1*3-2)*
     &reduced_fac,ana_xinib(J1*3-1)*reduced_fac,
     &ana_xinib(J1*3)*reduced_fac,
     &" atom_vector ",grad(J1*3-2)*-1.D0,
     &grad(J1*3-1)*-1.D0,grad(J1*3)
     &*-1.D0,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
                 do J1=atom_num_a+1,atom_num
                   grad_length=dsqrt(grad(J1*3-2)**2+
     &grad(J1*3-1)**2+grad(J1*3)**2)
                   write(2441,21) kind2,ana_xinib(J1*3-2)*
     &reduced_fac,ana_xinib(J1*3-1)*reduced_fac,
     &ana_xinib(J1*3)*reduced_fac,
     &" atom_vector ",grad(J1*3-2)*-1.D0,
     &grad(J1*3-1)*-1.D0,grad(J1*3)
     &*-1.D0,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
                 close(2441)
               endif
C====================================== 
             endif
             if(ana_pot.gt.ene_max)ene_max=ana_pot
             if(ana_pot.lt.ene_min)ene_min=ana_pot
             total_bond=0
             do J3=1,num_total
               total_bond=total_bond+bond_num(J3)
             enddo
             if(dummy_load1.eq."y")then
               write(*,*) "Write to frame ",I1," in ",file_name2
               write(file_index1,19) num_total," frame=",I1,
     &",original frame=",J0,"from file=",file_name1,
     &",Total bond num=",int(total_bond)
               write(file_index1,20) "Eng=",ana_pot,",bind=",
     &ana_pot/dble(num_total)," and A atoms="
     &,num_A,",RMS=",RMS,",VT(i)-bind,|Grad|,  bond#(i),i"
               if(dimension_index.eq.3)then
                 if(move_to_center.eq.1)call centre(ana_xinib,num_total)
                 do J1=1,atom_num_a
                   grad_length=dsqrt(grad(J1*3-2)**2+
     &grad(J1*3-1)**2+grad(J1*3)**2)
                   write(file_index1,21) kind1,ana_xinib(J1*3-2)*
     &reduced_fac,ana_xinib(J1*3-1)*reduced_fac,
     &ana_xinib(J1*3)*reduced_fac,
     &" atom_vector ",grad(J1*3-2)*-1.D0,
     &grad(J1*3-1)*-1.D0,grad(J1*3)
     &*-1.D0,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
                 do J1=atom_num_a+1,atom_num
                   grad_length=dsqrt(grad(J1*3-2)**2+
     &grad(J1*3-1)**2+grad(J1*3)**2)
                   write(file_index1,21) kind2,ana_xinib(J1*3-2)*
     &reduced_fac,ana_xinib(J1*3-1)*reduced_fac,
     &ana_xinib(J1*3)*reduced_fac,
     &" atom_vector ",grad(J1*3-2)*-1.D0,
     &grad(J1*3-1)*-1.D0,grad(J1*3)
     &*-1.D0,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
               else if(dimension_index.eq.2)then      !     for dimension 2
                 do J1=1,atom_num_a
                   grad_length=dsqrt(grad(J1*2-1)**2+
     &grad(J1*2)**2)
                   write(file_index1,22) kind1,ana_xinib(J1*2-1)*
     &reduced_fac*amp_bond_length,ana_xinib(J1*2)*reduced_fac
     &*amp_bond_length,0.,
     &" atom_vector ",grad(J1*2-1)*-1.D0,
     &grad(J1*2)*-1.D0,0.
     &,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
                 do J1=atom_num_a+1,atom_num_a+atom_num_b
                   grad_length=dsqrt(grad(J1*2-1)**2+
     &grad(J1*2)**2)
                   write(file_index1,22) kind2,ana_xinib(J1*2-1)*
     &reduced_fac*amp_bond_length,ana_xinib(J1*2)*reduced_fac
     &*amp_bond_length,0.,
     &" atom_vector ",grad(J1*2-1)*-1.D0,
     &grad(J1*2)*-1.D0,0.
     &,vt(J1)-binding_eng,grad_length,int(bond_num(J1)),J1
                 enddo
               endif
             endif
             write(file_index2,23)I1,ana_pot,ana_pot/dble(num_total),
     &RMS,int(total_bond),J0,file_name1
                               !write summery data
           endif
C           if(mod(J0,100000).eq.0)then
C             ene_de=(ene_max-ene_min)/dble(total_bin)
C             DE1=ene_de
C             EMAX_MIN_GL=ene_max
C             write(*,*) "EMAX=",ene_max,",EMIN=",ene_min,",de=",DE1
C             do J1=1,total_bin
C               ene_hist_kbin(J1)=0.D0
C               lmin_hist_kbin(J1)=0.D0 
C               hist(J1)=0
C               lhist(J1)=0   
C             enddo
C             do J1=query_frame_init,J0
C               ene_bin=ene_hist(J1)
C               call eng2bin(ene_bin,kbin,1)
C               hist(kbin)=hist(kbin)+1
C               ene_hist_kbin(kbin)=ene_hist(J1)
C====================================================
C               ene_bin=lmin_hist(J1)
C               call eng2bin(ene_bin,kbin,1)
C               lhist(kbin)=lhist(kbin)+1
C               lmin_hist_kbin(kbin)=lmin_hist(J1)
C             enddo
C             open(1224,file="energy_hist.dat",status="replace")
C             do J1=1,total_bin
C               if(hist(J1).ne.0.or.lhist(J1).ne.0)then
C                 write(1224,"F14.8,1x,I8,1x,F15.13,1x,
C     &F14.8,1x,I8,1x,F20.15")
C     &ene_hist_kbin(J1),hist(J1),dble(hist(J1))/
C     &dble(J0-
C     &query_frame_init+1),
C     &lmin_hist_kbin(J1),lhist(J1),dble(lhist(J1))/
C     &dble(J0-
C     &query_frame_init+1)
C                 write(*,*) J1,ene_hist_kbin(J1),lmin_hist_kbin(J1)
C               endif
C             enddo
C             close(1224)
C             pause
C           endif
         enddo
C================energy histogram=============================
         ene_de=(ene_max-ene_min)/dble(total_bin)
         DE1=ene_de
         EMAX_MIN_GL=ene_max
         write(*,*) "EMAX=",ene_max,",EMIN=",ene_min,",de=",DE1
         do J0=query_frame_init,query_frame_end
           ene_bin=ene_hist(J0)
           call eng2bin(ene_bin,kbin,1)
           hist(kbin)=hist(kbin)+1
           ene_hist_kbin(kbin)=ene_bin
C====================================================
           ene_bin=lmin_hist(J0)
           call eng2bin(ene_bin,kbin,1)
           lhist(kbin)=lhist(kbin)+1
           lmin_hist_kbin(kbin)=ene_bin
         enddo
         open(1223,file="energy_hist.dat",status="replace")
         do J0=1,total_bin
           write(1223,"F14.8,1x,I8,1x,F15.13,1x,
     &F14.8,1x,I8,1x,F20.15")
     &ene_hist_kbin(J0),hist(J0),dble(hist(J0))/
     &dble(query_frame_end-
     &query_frame_init+1),
     &lmin_hist_kbin(J0),lhist(J0),dble(lhist(J0))/
     &dble(query_frame_end-
     &query_frame_init+1)
         enddo
         close(1223)
C=========End of Energy histogram============================
         write(*,*) "Total frame=",query_frame_end-query_frame_init+1
         write(*,*) "Non-zero frames=",I1
         write(*,*) "End of writing to ",file_name2
         if(sel_1.eq.2)then
           write(*,*) "Input another file?(y/n)"
           read(*,*) dummy_load2
         else
           dummy_load2="n"
         endif
         if(dummy_load1.eq."y")then
           close(file_index1)
           close(file_index2)
           file_index1=101
           file_index2=103
         endif
       enddo 
       write(*,*) "Writing complete"
       write(*,*) "The data in tab_",file_name2
       write(*,*) "are:"
       write(*,*) 
     &"frame#,pot,binding_pot,RMS,total_bonds,original frame# filename"
C==============END OF INPUT FILE================================
C=================other function for future use======================
C       write(*,*) "Move the center of mass to origin(y/n)?(default=y)"
C       read(*,*) dummy_load1
C       if(dummy_load1.eq."y") call CENTRE(ana_xinib,ndim/3)
C       write(*,*)
C     &"Check center of mass to other points(y/n)?(default=n)"
C       read(*,*) dummy_load1
C       if(dummy_load1.eq."y") then
C         write(*,*) "Input new x coords for center of mass"
C         read(*,*) cent_x
C         write(*,*) "Input new y coords for center of mass"
C         read(*,*) cent_y
C         write(*,*) "Input new z coords for center of mass"
C         read(*,*) cent_z
C         call CENTRE(ana_xinib,ndim/3)
C         do J1=1,ndim/3
C           J2=J1*3
C           ana_xinib(J2-2)=ana_xinib(J2-2)+cent_x
C           ana_xinib(J2-1)=ana_xinib(J2-1)+cent_y
C           ana_xinib(J2)=ana_xinib(J2)+cent_z
C         enddo
C       endif
c       write(*,*) "Rotate the cluster(y/n)?(default=n)"
C       read(*,*) dummy_load1
C       if(dummy_load1.eq."y")then
C         write(*,*) "angle=(unit in pei)"
C         read(*,*) angle
C         call rotate(ana_xinib,ndim,angle)
C       endif
C       call check_pot_grad_vt(ndim,ana_pot,ana_xinib,print_pot_only)
C=============move atoms and check for future use==================
C       write(*,*) "Print gradient and vt on screen?(y/n)(default=y)"
C       read(*,*) dummy_load1
C       if(dummy_load1.eq."y")then
C         print_pot_only=.false.
C       else
C         print_pot_only=.true.
C       endif
C       call move_atom(ndim,ana_xinib,print_pot_only,print_xyz)
C==============write xyz file for future use========================
C       write(*,*)
C       write(*,*) "Please input the output file name(default="
C     &,name0,"an0X"
C       write(*,*) "or ",name0//name_alloy_a,")"
C       read(*,*) file_name
C       write(*,*) "writing to ",file_name,".xyz"
C       call write_xyz(ndim,ana_xinib,ana_pot,file_name)
C       write(*,*) "writing complete"
C       write(*,*) 
       return
       end

       subroutine xyz_common_neighbor
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/ensemble.h"
       include "../../include/mbh_ndim.h"
       include "../../include/mbh_ibin.h"
       include "../../include/file_tools.h"
       include "../../include/neighbors.h"
       include "../../include/analysis.h"
       integer count_in_file,file_index1,file_index2,last_frame
       logical exist_file
24     format(I7,1x,I10,1x,F18.8,1x,I7,1x,I7,1x,I7,1x,I7,3x,A30)
30     format(I7,1x,A3,1x,F15.7,1x,F15.7,1x,F15.7,1x,F15.7,1x,F15.7)
31     format(I7,1x,I10,1x,I7,1x,I7,1x,I7)
32     format(I7,1x,I10,1x,I7,1x,I7,1x,I7,1x,A49)
C============INPUT FILE=========================================
       write(*,*)"1.Read only one file(includes histogram file)"
       write(*,*)"2.Read more than one file(include many histogram file"
       read(*,*) sel_1
       write(*,*)"Write all data in a file?(y/n)"
       read(*,*)dummy_load1
       if(dummy_load1.eq."y")then
         write(*,*) "Please input the name of the output file(partial)"
         read(*,*) file_name2
         write(*,*)
         write(*,*) 
     &"The summary data will be stored in tab_com_"//file_name2
         write(*,*) 
       endif
       dummy_load2="y"
       count_in_file=0
       file_index1=200
       file_index2=202
       query_frame_length=0
       last_frame=0
       I2=0
       write(*,*) "TOTAL Neighbor=",total_neighbors
       do while(dummy_load2.eq."y")
         count_in_file=count_in_file+1  
         write(*,*)
     &"Please input the xyz file name(completed,not partial)"
         read(*,*)file_name1
         call xyz_frame_counter(file_name1,.true.)
         write(*,*) "Please input inital frame(from 1 to ",
     &total_frame,")"
         read(*,*) query_frame_init
         write(*,*) "Please input last frame(from",query_frame_init,
     &" to ",total_frame,")"
         read(*,*) query_frame_end
         if(dummy_load1.eq."y")then
           if(count_in_file.le.1)
     &open(file_index2,file="tab_com_"//file_name2,status="replace")
           if(count_in_file.gt.1)then
             inquire(file="tab_com_"//file_name2,exist=exist_file)
             if(exist_file)then
               open(file_index2,
     &file="tab_com_"//file_name2,access="append",
     &status="old")
             else
               open(file_index2,
     &file="tab_com_"//file_name2,status="replace")
             endif
           endif
         endif
         do J0=query_frame_init,query_frame_end
           query_frame=J0
           call xyz_frame_query(file_name1,query_frame,query_xinib,1)
           ETAB_MIN_GL(J0)=query_pot
C=============END OF INPUT FILE================================
           pair_analysis(J0,J1)=0
           if(query_xinib(1).ne.0.D0.and.query_xinib(4).ne.0.D0.
     &and.query_xinib(7).ne.0.D0)then                          
             call initialize_neighbors
             call common_neighbors(num_ndim,query_xinib)
           endif
           do J1=1,total_neighbors
             if(pair_aboundance(J1).ne.0)then
               pair_aboundance_total(J1)=pair_aboundance_total(J1)+1
               pair_analysis(J0,J1)=pair_aboundance(J1)
             endif
           enddo
         enddo
         do J1=1,total_neighbors
           if(pair_aboundance_total(J1).ne.0)then
             pair_digits=pair_digits_index(J1)
             call int2char4(pair_digits,pair_name)
             if(dummy_load1.eq."y")then
               if(count_in_file.le.1)
     &open(file_index1,file="par_"//pair_name//"."//file_name2,
     &status="replace")
               if(count_in_file.gt.1)then
                 inquire(file="par_"//pair_name//"."//file_name2,
     &exist=exist_file)
                 if(exist_file)then
                   open(file_index1,
     &file="par_"//pair_name//"."//file_name2,
     &access="append",status="old")
                 else
                   open(file_index1,
     &file="par_"//pair_name//"."//file_name2,status="replace")
                 endif
               endif
             endif
             I1=0
             do J0=query_frame_init,query_frame_end
               if(pair_analysis(J0,J1).ne.0)then
                 I1=I1+1
                 if(dummy_load1.eq."y")then
                   write(file_index1,24)
     &J0,pair_analysis(J0,J1),
     &ETAB_MIN_GL(J0),pair_digits_index(J1),J0,J1,pair_num,file_name1
                   write(*,32)J0,pair_analysis(J0,J1),
     &pair_digits_index(J1),J1,pair_num," write to par_"//pair_name
     &//"."//file_name2
                 else
                   write(*,31)J0,pair_analysis(J0,J1),
     &pair_digits_index(J1),J1,pair_num
                 endif
               endif
             enddo
             if(dummy_load1.eq."y")then
               close(file_index1)
               file_index1=201
             endif
           endif
         enddo
         if(dummy_load1.eq."y")then
           do J0=query_frame_init,query_frame_end
             I2=I2+1
             do J1=1,total_neighbors
               if(pair_analysis(J0,J1).ne.0)then
                 write(file_index2,24)
     &I2,pair_analysis(J0,J1),ETAB_MIN_GL(J0),pair_digits_index(J1),
     &J0,J1,pair_num,file_name1 
               endif
             enddo
           enddo
         endif
         close(file_index2)
         file_index2=203
         write(*,*) "  # aboundance digits matrix total"
         if(sel_1.eq.2)then
           write(*,*) "Input another file?(y/n)"
           read(*,*) dummy_load2
         else
           dummy_load2="n"
         endif
       enddo
       write(*,*) "Writing complete"
       write(*,*) "The data in par_",pair_name,".",file_name2
       write(*,*) "are:"
       write(*,*) 
     &"original frame#,aboundance,Gmin,"
       write(*,*)"4digits,matrix1,matrix2,total_par,file"
       return
       end

       subroutine move_atom(ndim,xinib,print_pot_only,print_xyz)
       implicit none
       include"../../include/global_common.h"
       include"../../include/common.h"
       include"../../include/mbh_ndim.h"
       integer atom_number,sel
       real*8 move_x,move_y,move_z
       logical print_pot_only,print_xyz
       atom_number=1
       do while(move_x.ne.0.D0.and.move_y.ne.0.D0.and.move_z.ne.0.D0)
         write(*,*) "1.Select atom"
         write(*,*) "2.move atom"
         write(*,*) "3.write to xyz file"
         write(*,*) "4.<<Back"
         read(*,*) sel
         if(sel.eq.1)then
           write(*,*)"Input atom_number"    
           read(*,*) atom_number
         else if(sel.eq.2)then
           write(*,*)"move ",atom_number," atom x="
           read(*,*) move_x
           xinib(atom_number*3-2)=xinib(atom_number*3-2)+move_x
           write(*,*)"move ",atom_number," atom y="
           read(*,*) move_y
           xinib(atom_number*3-1)=xinib(atom_number*3-1)+move_y
           write(*,*)"move ",atom_number," atom z="
           read(*,*) move_z    
           xinib(atom_number*3)=xinib(atom_number*3)+move_z
           call check_pot_grad_vt(ndim,pot,xinib,print_pot_only,
     &print_xyz)
         else if(sel.eq.3)then
           write(*,*) 
           write(*,*) "writing to ",name0//"an.xyz"
           call write_xyz(ndim,xinib,name0//"an")
           write(*,*) "writing complete"
           write(*,*) 
         else if(sel.eq.4)then
           move_x=0.D0
           move_y=0.D0
           move_z=0.D0
         endif
       enddo
       return
       end
      
       subroutine rotate(xnew,ndim,eular_fac)
       integer seed,ndim,i
       real*8 xnew(3000),xold(3000),pei,angle1,angle2,angle3,
     &rnd,eular_fac
       parameter(pei=3.14159D0)
       angle1=pei*eular_fac
       angle2=pei*eular_fac
       angle3=pei*eular_fac
C       angle3=pei*rnd(seed)
       do i=1,ndim
         xold(i)=xnew(i)
       enddo
       do i=1,ndim,3
         xnew(i)=((dcos(angle3)*dcos(angle1)-dcos(angle2)*dsin(angle1)
     &*dsin(angle3))*xold(i)
     &+(dcos(angle3)*dsin(angle1)+dcos(angle2)*dcos(angle1)*dsin
     &(angle3))*xold(i+1)
     &+(dsin(angle3)*dsin(angle2))*xold(i+2))
         xnew(i+1)=(((-1.)*dsin(angle3)*dcos(angle1)-dcos(angle2)*
     &dsin(angle1)*dcos(angle3))*xold(i)
     &+((-1.)*dsin(angle3)*dsin(angle1)+dcos(angle2)*dcos(angle1)
     &*dcos(angle3))*xold(i+1)
     &+(dcos(angle3)*dsin(angle2))*xold(i+2))
         xnew(i+2)=((dsin(angle2)*dsin(angle1))*xold(i)
     &+((-1.)*dsin(angle2)*dcos(angle1))*xold(i+1)
     &+dcos(angle2)*xold(i+2))
       enddo
       return
       end 

       subroutine check_pot_grad_vt(ndim,pot,xinib,print_pot_only,
     &print_xyz)
       implicit none
       include"../../include/global_common.h"
       include"../../include/common.h"
       include"../../include/mbh_ndim.h"
       logical print_pot_only,print_xyz   
30     format(I3,1x,A3,1x,F21.7,1x,F21.7,1x,F21.7,1x,F21.7,1x,F21.7)
32     format(A3,1x,F21.7,1x,F21.7,1x,F21.7)
31     format(A25,1x,F16.8,1x,A5,1x,F27.12)
       call POTENTIAL(ndim,pot,xinib,grad,vt)
       write(*,*) "If the potential is incorrect,please check"
       write(*,*) "reduced unit function in config.out"
       write(*,31) "After checking potential=",pot,",RMS=",RMS
       if(print_xyz)then
         write(*,*) " type          X             Y           Z"
         do J1=1,ndim/3
           J2=J1*3
           if(J1.le.atom_num_a)then
             write(*,32) kind1,xinib(J2-2),xinib(J2-1),xinib(J2)
           else
             write(*,32) kind2,xinib(J2-2),xinib(J2-1),xinib(J2)
           endif
         enddo
       endif
       if(print_pot_only) return
       write(*,*)
     &" # type        G_X             G_Y             G_Z
     &VT         grad_length"
       do J1=1,ndim/3
         J2=J1*3
         if(J1.le.atom_num_a)then
           write(*,30) J1,kind1,-grad(J2-2),-grad(J2-1),
     &-grad(J2),vt(J1),
     &dsqrt(grad(J2-2)**2+grad(J2-1)**2+grad(J2)**2)
         else
           write(*,30) J1,kind2,-grad(J2-2),-grad(J2-1),
     &-grad(J2),vt(J1),
     &dsqrt(grad(J2-2)**2+grad(J2-1)**2+grad(J2)**2)
         endif
       enddo
       write(*,31) "After checking potential=",pot,",RMS=",RMS
       write(*,*) 
       return
       end

       subroutine check_xyz_pot(ndim)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       character check_xyz_pot_name*60,check_meterial*2
     &,check_xyz_dummy
       real*8 uncheck_pot,check_pot,x_check(3000)
       write(*,*) "please input the file name(ex:0038CA0018.xyz)"
       read(*,*) check_xyz_pot_name
       open(20,file=check_xyz_pot_name,status="old")
       read(20,*) check_meterial,uncheck_pot
       read(20,*) 
       do J0=1,ndim/3
         read(20,*)
     &check_xyz_dummy,x_check(J0*3-2),x_check(J0*3-1),x_check(J0*3)
         x_check(J0*3-2)=x_check(J0*3-2)/reduced_fac
         x_check(J0*3-1)=x_check(J0*3-1)/reduced_fac
         x_check(J0*3)=x_check(J0*3)/reduced_fac
       enddo
       close(20)
       call check_pot_grad_vt(ndim,check_pot,x_check,.false.,.false.)
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
