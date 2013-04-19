*========================================================================
* File Name : parallel_shell.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 28 Dec 2010 10:32:27 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine parallel_shell()
       implicit none
C===========Life=ensembles;ensemble=jobs=====================
       include "mpif.h"
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/job.h"
       include "../include/ensemble.h"
       include "../include/pes.h"
       include "../include/parallel.h"
       do J0=1,total_job_num
         job_status(J0)=0
       enddo
       do J0=0,nproc-1
         call startend(J0,nproc,1,total_job_num,ist1,iend1)
         start_point(J0)=ist1-1       !from 1,total_job_num,ist1*myid*(ndim)-2=start point of N dim 
C Be careful! ist1-1 is only for MPI_SCATTERV,MPI_GATHERV
         length(J0)=iend1-ist1+1      ! length*ndim=total_length
       enddo
C       if(myid.eq.0.and.wscreen)then
C         do I0=0,nproc-1
C           write(*,*) life_num,",myid=",I0,length(I0),
C     &start_point(I0),total_job_num
C         enddo
C       endif
       if(length(myid).eq.0)then
        if(wscreen)write(*,"(I5,1x,A20)") 
     &myid,"no job,now idling..."
        goto 99
       endif
       do parallel_id=1,length(myid)
         job_point_id=start_point(myid)+parallel_id   !The id # in one life accross all ensembles
         id=life_id(job_point_id)  !The id # in the one ensemble
         job_num=life_job(job_point_id) !This is the job_num'th job in one life
         job_id=life_job_id(job_point_id)  !The job id for this job_num'th job
         ensemble_num=
     &life_ensemble_id(job_point_id)         !The total ensemble number for this job_num'th job
         loop_num=life_loop_id(job_point_id) !The loop number of this job_num'th job
         pes_id=life_pes_id(job_point_id)    !The pes_id for this job_num'th job
         job_node(job_point_id)=myid         !This job_num'th job is currently running at which node
C         write(*,*) "pes_id=",pes_id,life_pes_id(2),start_point(myid)
C     &+parallel_id
         call locate_current_job_table_type   !output job_table_line_num
         call locate_current_job         !output job_line_num
         call locate_current_pes_type         !output pes_line_num
         call initial_job
         call initial_pes
         call job
         job_status(job_point_id)=1
         call check_job_status(job_point_id,1)
       enddo
99     call mpi_barrier(mpi_comm_world,mpi_err)
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
