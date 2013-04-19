      subroutine usr_score_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/usr/usr.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/usr_score.h"
      integer source_init,source_final,source_delta
      real*8 total_usr_score_num,diff_moment
      character target_file_name*80
      integer target_frame(tdusr_lmin_max),total_target_num
      integer moment_init,moment_final,total_source_num,
     &moment_length
      real*8 usr_target_moment(tdusr_lmin_max,total_usr_moment)
      integer dummy_frame,dummy_cst,dummy_fct,dummy_ftf
      real*8 dummy_pot
      integer end_1st,end_2nd,end_3rd,end_4th

      file_name=usr_score_file_name(:index(usr_score_file_name," ")-1)
      call check_file_exist(file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A31)")myid,"Source file doesn't exist!Skip!"
        return
      endif
      target_file_name=
     &usr_score_target_file_name
     &(:index(usr_score_target_file_name," ")-1)
      call check_file_exist(target_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30)")myid,"target.usr doesn't exist!Stop!"
        stop
      endif
C========start calculating usr_score======================
      target_init=dint(usr_score_target_init)
      target_final=dint(usr_score_target_final)
      source_init=dint(usr_score_init_loop)
      source_final=dint(usr_score_final_loop)
      source_delta=dint(usr_score_delta_loop)
      total_usr_score_num=
     &(usr_score_final_loop-usr_score_init_loop+1.D0)
     &/usr_score_delta_loop
      total_source_num=dint(total_usr_score_num)
      total_target_num=target_final-target_init+1
C=======Calculate moment length=======================
      moment_length=0.D0
      if(usr_score_1st_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_2nd_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_3rd_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_4th_moment.eq.1)moment_length=moment_length+4.D0
      end_1st=usr_score_1st_moment*4
      end_2nd=usr_score_2nd_moment*8
      end_3rd=usr_score_3rd_moment*12
      end_4th=usr_score_4th_moment*16
C=====================================================
      open(21,file=target_file_name,status="old")
      do I0=1,target_init-1
        read(21,*)
      enddo
      do I0=target_init,target_final
        read(21,*)target_frame(I0),dummy_pot,dummy_cst,dummy_fct,
     &dummy_ftf,(usr_target_moment(I0,J2),J2=1,total_usr_moment)
        TDUSR(I0)=0.D0
      enddo
      close(21)
      open(22,file=file_name,status="old")
      do I0=1,source_init-1
        read(22,*)
      enddo
      do I0=source_init,source_final        !ensemble average
        read(22,*) dummy_frame,dummy_pot,dummy_cst,dummy_fct,dummy_ftf,
     &(usr_moment(J2),J2=1,total_usr_moment)
        do I1=1,source_delta-1   !skip interval
          read(22,*)
        enddo
        do I1=target_init,target_final      !calculate local minima
          diff_moment=0.D0
          do I2=1,end_1st    !if usr_score_1st_moment=0(off),it will skip 1st moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=5,end_2nd    !if usr_score_2nd_moment=0(off),it will skip 2nd moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=9,end_3rd   !if usr_score_3rd_moment=0(off),it will skip 3rd moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=13,end_4th  !if usr_score_4th_moment=0(off),it will skip 4th moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo 
          TDUSR(I1)=TDUSR(I1)+1.D0/(1.D0+(diff_moment/moment_length))
        enddo
      enddo 
      do I1=target_init,target_final
        TDUSR(I1)=TDUSR(I1)/total_usr_score_num
      enddo
      TDUSR_temp=dint(temp) 
      close(22)
      return
      end
