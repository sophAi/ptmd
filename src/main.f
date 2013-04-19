*========================================================================
* File Name : main.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年03月23日 (週三) 21時56分41秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
C====================================================
C                      sophAi 
C          Statistical Optimization and
C    Phenomenal Histogram Artificial Intellegence
C    
C         Copyright (C) 2008-2011 Po-Jen Hsu     
C
C====================================================
       program main   !initialized the MPI environment
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       call mpi_init(mpi_err)
       call mpi_comm_size(mpi_comm_world,nproc,mpi_err)
       call mpi_comm_rank(mpi_comm_world,myid,mpi_err)
C====================main code========================
       call check_file
       call check_wscreen
       call check_debug
       call initiate_neocortex
       call activate_neocortex
       call stop_neocortex
C====================End==============================
       call mpi_finalize(mpi_err)
       end
      
       subroutine initiate_neocortex  ! control the behavior of the main program
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/ensemble.h"
       include "../include/job.h"
       include "../include/pes.h"
       call update_version(version,update)
       call global_init
       if(wscreen.and.myid.eq.0)then
         write(*,*)
         write(*,*) "      =============================="
         write(*,*) "                  sophAi"
         write(*,*) "   Copyright (C) 2008-2012 Po-Jen Hsu  "
         write(*,*) "                Ver.",version
         write(*,*) "          Last Update:",update
         write(*,*) "     ==============================="
         write(*,*) "    "
         write(*,*) "                 sophAi:       "
         write(*,*) "       \                         /"
         write(*,*) "             I HAVE CONTROL!           "
         write(*,*) "       /                         \"
         write(*,*)
       endif
       return
       end
 
       subroutine activate_neocortex  ! control the behavior of the main program
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/ensemble.h"
       include "../include/job.h"
       include "../include/pes.h"
       call count_life_num
       do life_num=1,total_life_num
         if(myid.eq.0)then
           call check_time_label
           call check_program_status(life_num)
           if(wscreen)then
             write(*,*)
             write(*,"(A16,1x,I5,1x,A1,1x,I5,1x,A12,1x,F15.6,1x,A10)") 
     &"<<<<<<<<<< Life=",life_num,"/",total_life_num,
     &";Time Label=",time_label,">>>>>>>>>>"
           endif
         endif
         call mpi_bcast(time_label,1,mpi_real8,0,mpi_comm_world
     &,mpi_err)
         call time_label2char
         call locate_current_life   !output life_line_num=life_num
         call check_job_status(0,0)
         call parallel_shell
         call mpi_barrier(mpi_comm_world,mpi_err)
       enddo
       return
       end
 
       subroutine stop_neocortex
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/ensemble.h"
       include "../include/job.h"
       include "../include/pes.h"
       if(myid.eq.0)then
         call check_time_label
         call check_program_status(0)
         if(wscreen)then
           write(*,*) 
           write(*,*) ">>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<"
           write(*,*) "<<         SophAi Stop!           >>" 
           write(*,*) ">>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<"
           write(*,*) 
         endif
       endif
       return
       end

       subroutine check_program_status(ini)
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/life.h"
       include "../include/file.h"
       integer ini,days,hours,minutes,seconds
C       integer*4 today(3),now(3)
       integer today(3),now(3)
       real*4 time,test
       real*8 initial_time_in_sec,finish_time_in_sec,
     &computing_time_in_sec
       common /status/ initial_time_in_sec
999    format ( 'Life  ',I5,'/',I5,' at date ',i2.2,'/',
     &i2.2, '/',i4.2,'; time ',
     &i2.2,':',i2.2,':',i2.2,'; Time label= ',F15.6)
1000   format ( 'Started Life 1 /',I5,' at date ',
     &i2.2,'/',i2.2, '/',i4.2,'; time ',
     &i2.2,':',i2.2,':',i2.2,'; Time label= ',F15.6)
1001   format ( ' Finished at date ',i2.2,'/',i2.2, '/',i4.2,'; time ',
     &i2.2,':',i2.2,':',i2.2)
1002   format (A18,1x,i4,1x,A6,1x,i2,1x,A7,1x,i2,1x,A13,1x,i2,1x,A8)
1003   format (A9,1x,F30.10,1x,A5)
C       call mpi_barrier(mpi_comm_world,mpi_err)
       if(ini.eq.1)then        !program start         
         initial_time_in_sec=mpi_wtime()
         call idate(today)   ! today(1)=day, (2)=month, (3)=year
         call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
         open(101,file=program_status_file,status="replace")
         write(101,1000) total_life_num,
     &today(2),today(1),today(3),now,time_label
         close(101)
       else if(ini.eq.0)then !program stop
         open(102,file=program_status_file,access="append",
     &status="old")
         call idate(today)   ! today(1)=day, (2)=month, (3)=year
         call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
         write(102,1001) today(2),today(1),today(3),now
         write(102,*)
         finish_time_in_sec=mpi_wtime()
         computing_time_in_sec=finish_time_in_sec-initial_time_in_sec
         days=dint(computing_time_in_sec/86400.D0)
         hours=dint(computing_time_in_sec/3600.D0-24.D0*dble(days))
         minutes=dint(computing_time_in_sec/60.D0-1440.D0*
     &dble(days)-60.D0*dble(hours))
         seconds=dint(computing_time_in_sec-86400.D0*
     &dble(days)-3600.D0*dble(hours)-60.D0*dble(minutes))
         write(102,"(A64)")
     &"<<<<<<<<<<<<<<<<<<<<<<<<<<<<Summery>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
         write(102,1002) "The program costs ",days," days,",hours,
     &" hours,",minutes," minutes and ",seconds," seconds"
         write(102,1003) "Total in ",computing_time_in_sec," secs"
         close(102)
       else
         call idate(today)   ! today(1)=day, (2)=month, (3)=year
         call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
         open(103,file=program_status_file,access="append",
     &status="old")
         finish_time_in_sec=mpi_wtime()
         computing_time_in_sec=finish_time_in_sec-initial_time_in_sec
         days=dint(computing_time_in_sec/86400.D0)
         hours=dint(computing_time_in_sec/3600.D0-24.D0*dble(days))
         minutes=dint(computing_time_in_sec/60.D0-1440.D0*
     &dble(days)-60.D0*dble(hours))
         seconds=dint(computing_time_in_sec-86400.D0*
     &dble(days)-3600.D0*dble(hours)-60.D0*dble(minutes))
         write(103,1002) " From last life = ",days,
     &" days,",hours," hours,",minutes," minutes and ",seconds,
     &" seconds"
         write(103,*)
         write(103,999) ini,total_life_num,
     &today(2),today(1),today(3),now,time_label
         close(103)
       endif
       return
       end

       subroutine check_job_status(job_point,initial_sw)
       implicit none
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/life.h"
       include "../include/file.h"
       include "../include/ensemble.h"
       include "../include/job.h"
       integer dummy3,dummy4,dummy8,dummy10,job_point,initial_sw
       integer dummy11
       character dummy6*22,dummy1*8,dummy2*5,dummy5*12,dummy7*5,
     &dummy9*11,dummy12*18
777    format(A8,1x,I6,1x,A5,1x,I5,1x,A12)
       if(initial_sw.eq.1)then
12       call wait_till_file_close(job_status_file)
         open(24,file=job_status_file,err=11,status="old")
         read(24,*)dummy7,dummy8,dummy9,dummy10,dummy6,
     &unfinished_job_num,dummy12,dummy11
         call int2char4(job_point_id,int2char)
         file_name=job_status_file(:index(job_status_file," ")-1)//
     &".tmp"
         open(23,file=file_name,status="replace")
         unfinished_job_num=unfinished_job_num-1
         write(23,"(A5,1x,I5,1x,A11,1x,I5,1x,A22,1x,I6,1x,A18,1x,I5)")
     &dummy7,life_num,dummy9,total_life_num,dummy6,unfinished_job_num
     &,dummy12,myid
         do I0=1,total_job_num
           read(24,*) dummy1,dummy3,dummy2,dummy4,dummy5
           if(I0.eq.job_point)then
             write(23,777)"job_seq=",I0,"myid=",job_node(job_point),
     &"finished"
           else
             write(23,777)"job_seq=",dummy3,"myid=",dummy4,
     &dummy5(:index(dummy5," ")-1)
           endif
         enddo
         close(24)
         close(23)
         call wait_till_file_close(job_status_file)
         call rename(file_name,job_status_file)
       else
         if(myid.eq.0)then
           open(22,file=job_status_file,status="replace")
           write(22,"(A5,1x,I5,1x,A11,1x,I5,1x,A22,1x,I6,1x,A18,1x,I5)")
     &"life=",life_num,"total_life="
     &,total_life_num,"unfinished_job_number=",total_job_num
     &,"last_updated_node=",0
           do I0=1,total_job_num
             write(22,777)"Job_seq=",I0,"myid=",0,"unfinished"
           enddo
           close(22)
         endif
       endif
11     return
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
