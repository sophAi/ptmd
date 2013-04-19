C====================================================
C                      sophAi 
C          Statistical Optimization and
C    Phenomena Histogram Artificial Intellegence
C    
C         Copyright (C) 2008,2009 Po-Jen Hsu     
C
C====================================================
       program main   !initialized the MPI environment
       implicit none
       include "mpif.h"
       integer myid,nproc,mpi_err
       common myid,nproc,mpi_err
       call mpi_init(mpi_err)
       call mpi_comm_size(mpi_comm_world,nproc,mpi_err)
       call mpi_comm_rank(mpi_comm_world,myid,mpi_err)
C====================main code========================
       write(*,*) myid,"before check file"
C       call check_file
       write(*,*) myid, "after check file"
C       call check_wscreen
C       call check_debug
       call mpi_finalize(mpi_err)
C====================End==============================
C       call mpi_finalize(mpi_err)
       end
      
