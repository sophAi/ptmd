*========================================================================
* File Name : parameter.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 05 Apr 2011 02:39:37 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine parameter_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      character par_name_dummy*25
      file_name=
     &file_path(:index(file_path," ")-1)//parameter_file
     &(:index(parameter_file," ")-1)
      inquire(file=file_name,exist=file_exist)
      call count_file_line(file_name)
      total_par_num=file_line_num-1
      if(file_exist)then
        open(20,file=file_name,status="old")
        read(20,*)
        do I0=1,total_par_num
          read(20,*)par_read_id(I0),par_read_catalog(I0),
     &par_read_name(I0),par_read_type(I0),par_read_real8_def(I0),
     &par_read_real8_min(I0),par_read_real8_max(I0),
     &par_read_description(I0)
          par_map_id(par_read_id(I0))=I0   !trace I0 from par_read_id, very important
        enddo
        close(20)
        if(wscreen)
     &write(*,"(I5,1x,A29)") myid,"Read parameter file complete!"
      else 
        write(*,"(I5,1x,A34)") 
     &myid,"warning! parameter file not found!"
        stop
      endif
      file_name=
     &file_path(:index(file_path," ")-1)//parameter_config_file
     &(:index(parameter_config_file," ")-1)
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        call count_file_line(file_name)        !allow parameters in parameter_config_file less than total_par_num
        if(file_line_num.lt.1)write(*,"(I5,1x,A36)")myid,
     &"the parameter config file is invalid"
      endif
      if(.not.file_exist.or.file_line_num.lt.1)then
        write(*,"(I5,1x,A49)")
     &myid,"warning! you need a proper parameter config file!"
        write(*,"(I5,1x,A31,1x,A80)")
     &myid,"You should check the this file:",file_name
        write(*,"(I5,1x,A40)")
     &myid,"The default parameters will be generated"
        do I0=1,total_par_num
          par_read_real8_config(I0)=par_read_real8_def(I0)
        enddo
        call write_parameter
      endif
      call count_file_line(file_name)
      total_par_config_num=file_line_num
      open(23,file=file_name,status="old")
      do I0=1,total_par_config_num
        read(23,*) par_read_config_id(I0),par_name_dummy,
     &par_read_config_from_file(I0)
        I1=par_map_id(par_read_config_id(I0))
        par_read_real8_config(I1)=
     &par_read_config_from_file(I0)
        if(par_read_type(I1).eq."int")then
          par_int_config=dint(par_read_real8_config(I0))
        else if(par_read_type(I1).eq."real8")then
          par_real8_config=par_read_real8_config(I0)
        endif
        call parameter_assign(par_read_name(I0),"r")
      enddo
      close(23)
      return
      end

      subroutine write_parameter
      implicit none   
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      file_name=
     &file_path(:index(file_path," ")-1)//parameter_config_file
     &(:index(parameter_config_file," ")-1)
      open(21,file=file_name,status="replace")
      do I0=1,total_par_num
        write(21,"(I5,1x,A25,1x,F30.7)")par_read_id(I0),
     &par_read_name(I0)(:index(par_read_name(I0)," ")-1),
     &par_read_real8_config(I0)
      enddo
      close(21)
      file_name=
     &file_path(:index(file_path," ")-1)//parameter_hist_file
     &(:index(parameter_hist_file," ")-1)
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        call count_file_line(file_name)
        par_total_hist_frame=file_line_num/2
      else
        par_total_hist_frame=0
      endif
      open(22,file=file_name,access="append")
      write(22,"(I7,1x,I4,1x,1000(I20,1x))")
     &par_total_hist_frame+1,total_par_num,
     &(par_read_id(I0),I0=1,total_par_num)
      write(22,"(I7,1x,I4,1x,1000(F30.7,1x))")
     &par_total_hist_frame+1,total_par_num,
     &(par_read_real8_config(I0),I0=1,total_par_num)
      close(22)
      return
      end

      subroutine update_parameter(input_name,update_value) !Update the current parameter by the value!Make sure the input name has 1 space ex:"init_temp " and the update value is real8
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      real*8 update_value
      character input_name*25
      par_name=input_name(:index(input_name," ")-1)
      par_found=.false.
      do I0=1,total_par_num
        if(par_read_name(I0).eq.par_name)then
          par_found=.true.
          par_id=par_read_id(I0)
          par_catalog=par_read_catalog(I0)
          par_type=par_read_type(I0)
          par_description=par_read_description(I0)
          par_read_real8_config(I0)=update_value
          if(par_type.eq."int")then
            par_int_config=dint(update_value)
          else if(par_type.eq."real8")then
            par_real8_config=update_value
          endif
          call parameter_assign(input_name,"w")
          return
        endif
      enddo
      if(.not.par_found)then
        write(*,"(I5,1x,A38,1x,A25)")myid,
     &"Warning!! Cannot find this parameter= ",par_name
        stop
      endif
      return
      end

      subroutine name2parameter(input_name_temp,io_method) !Very important!!Make sure the input name has 1 space ex:"init_temp "
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      character input_name_temp*25,io_method*1  !"r"=read def,"w"=write def
      par_name=input_name_temp(:index(input_name_temp," ")-1)
      do I0=1,total_par_num
        if(par_read_name(I0).eq.par_name)then
          par_id=par_read_id(I0)
          par_catalog=par_read_catalog(I0)
          par_type=par_read_type(I0)
          par_description=par_read_description(I0)
          if(par_type.eq."int")then
            if(io_method.eq."r")then
              par_int_def=dint(par_read_real8_def(I0))
              par_int_config=dint(par_read_real8_config(I0))
              par_int_min=dint(par_read_real8_min(I0))
              par_int_max=dint(par_read_real8_max(I0))
            else if(io_method.eq."w")then
              par_read_real8_config(I0)=dble(par_int_config)
            endif
          else if(par_type.eq."real8")then
            if(io_method.eq."r")then
              par_real8_def=par_read_real8_def(I0)
              par_real8_config=par_read_real8_config(I0)
              par_real8_min=par_read_real8_min(I0)
              par_real8_max=par_read_real8_max(I0)
            else if(io_method.eq."w")then
              par_read_real8_config(I0)=par_real8_config
            endif
          endif
          return
        endif
      enddo
      return
      end

      subroutine show_parameter(input_id,input_name_temp)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      integer input_id
      character input_name*25,input_name_temp*25
      input_name=input_name_temp(:index(input_name_temp," ")-1)
      if(input_id.eq.0.or.input_name.eq."quit ".or.
     &input_name.eq."Quit ".or.input_name.eq."QUIT ")then
        write(*,"(A21)")"  0.Preserve settings"
      else
        call name2parameter(input_name,"r")
        if(par_type.eq."int")then
          write(*,"(I3,A11,1x,A50,A11,I20,A9,I20)")
     &input_id,".Change the"
     &,par_description
C(:index(par_description,"  ")-1)
     &,"=> Current=",par_int_config,"/Default=",par_int_def
        else if(par_type.eq."real8")then
          write(*,"(I3,A11,1x,A50,A11,F20.7,A9,F20.7)")
     &input_id,".Change the"
     &,par_description
C(:index(par_description,"  ")-1)
     &,"=> Current=",par_real8_config,"/Default=",par_real8_def
        endif
      endif
      return
      end

      subroutine edit_parameter(input_name_temp)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/parameter.h"
      character input_name*25,input_name_temp*25
      input_name=input_name_temp(:index(input_name_temp," ")-1)
      call name2parameter(input_name,"r")
      if(par_type.eq."int")then
        write(*,"(A10,A50,A11,I20,A9,I20)")
     &"Input the ",par_description,
     &"=> Current=",par_int_config,"/Default=",par_int_def
101     read(*,*) par_int_config
        if(par_int_config.gt.par_int_max.
     &or.par_int_config.lt.par_int_min)then
          write(*,"(A20,1x,I20,1x,A16)") 
     &"The value you input:",par_int_config,"is out of range!"
          write(*,"(A17,1x,I20,1x,A2,1x,I20)")
     &"It should be from",par_int_min,"to",par_int_max
          write(*,"(A34,1x,I20)")
     &"It is suggested you use this value",par_int_def
          write(*,"(A20)") "Please input again!"
          goto 101
        endif
      else if(par_type.eq."real8")then
        write(*,"(A10,A50,A11,F20.7,A9,F20.7)")
     &"Input the ",par_description,
     &"=> Current=",par_real8_config,"/Default=",par_real8_def
102     read(*,*) par_real8_config
        if(par_real8_config.gt.par_real8_max.
     &or.par_real8_config.lt.par_real8_min)then
          write(*,"(A20,1x,F20.7,1x,A16)")
     &"The value you input:",par_real8_config,"is out of range!"
          write(*,"(A18,1x,F20.7,1x,A2,1x,F20.7)")
     &"It shoud be within",par_real8_min,"to",par_real8_max
          write(*,"(A34,1x,F20.7)")
     &"It is suggested you use this value",par_real8_def
          write(*,"(A20)") "Please input again!"
          goto 102
        endif
      endif 
      call name2parameter(input_name,"w")
      call parameter_assign(input_name,"w")
      return
      end

      subroutine parameter_assign(input_name_temp,io_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/job.h"
      include "../include/life.h"
      include "../include/parallel.h"
      include "../include/pes.h"
      include "../include/parameter.h"
      include "../include/simulation.h"
      include "../include/BIMD/BIMD.h"
      include "../include/BIMD/BIMD_restore.h"
      include "../include/PTMC/PTMC.h"
      include "../include/cn/cn.h"
      include "../include/PES/gp.h"
      include "../include/PES/colloid.h"
      include "../include/PES/maxent.h"
      include "../include/step_move/step_move.h"
      include "../include/tools/anl.h"
      include "../include/tools/usr.h"
      include "../include/tools/ft.h"
      include "../include/tools/moment.h"
      include "../include/tools/histogram.h"
      include "../include/tester/corr.h"
      include "../include/tester/fourier.h"
      include "../include/tester/hist.h"
      include "../include/tester/score.h"
      include "../include/tester/sliding.h"
      include "../include/tester/tester.h"
      integer par_int_config_temp
      real*8 par_real8_config_temp
      character io_method*1
      character input_name*25,input_name_temp*25
      input_name=input_name_temp(:index(input_name_temp," ")-1)
C=== 1 ndim_fac
      if(input_name.eq."ndim_fac")then
        ndim_fac=par_int_config
C=== 2 atom_num
      else if(input_name.eq."atom_num")then
        atom_num=par_int_config
C=== 3 ndim
      else if(input_name.eq."ndim")then
        ndim=par_int_config
C=== 4 atom_num_a
      else if(input_name.eq."atom_num_a")then
        atom_num_a=par_int_config
C=== 5 ndim_a
      else if(input_name.eq."ndim_a")then
        ndim_a=par_int_config
C=== 6 atom_num_b
      else if(input_name.eq."atom_num_b")then
        atom_num_b=par_int_config
C=== 7 ndim_b
      else if(input_name.eq."ndim_b")then
        ndim_b=par_int_config
C=== 8 atom_num_c
      else if(input_name.eq."atom_num_c")then
        atom_num_c=par_int_config
C=== 9 ndim_c
      else if(input_name.eq."ndim_c")then
        ndim_c=par_int_config
C=== 10 min_PES_method
      else if(input_name.eq."min_PES_method")then
        min_PES_method=par_int_config
C=== 20 mva
      else if(input_name.eq."mva")then
        mva=par_real8_config
C=== 21 mvb
      else if(input_name.eq."mvb")then
        mvb=par_real8_config
C=== 22 mvc
      else if(input_name.eq."mvc")then
        mvc=par_real8_config
C=== 23 mvd
      else if(input_name.eq."mvd")then
        mvd=par_real8_config
C=== 24 mve
      else if(input_name.eq."mve")then
        mve=par_real8_config
C=== 25 mvf
      else if(input_name.eq."mvf")then
        mvf=par_real8_config
C=== 26 mvg
      else if(input_name.eq."mvg")then
        mvg=par_real8_config
C=== 27 mvh
      else if(input_name.eq."mvh")then
        mvh=par_real8_config
C=== 28 mvi
      else if(input_name.eq."mvi")then
        mvi=par_real8_config
C=== 30 init_temp
      else if(input_name.eq."init_temp")then
        if(par_real8_config.gt.final_temp.and.io_method.eq."w")then
          write(*,*)
     &"NOTE: initial temp > final temp,reset init_temp to ",final_temp
          par_real8_config=final_temp
        endif
        if(par_real8_config.eq.final_temp.and.io_method.eq."w")then
          write(*,*)
     &"NOTE: initial temp = final temp,reset delta_temp to 1"
          delta_temp=1.D0
          par_real8_config_temp=par_real8_config
          par_real8_config=1.D0
          call name2parameter("delta_temp ","w")
          par_real8_config=par_real8_config_temp
        endif
        init_temp=par_real8_config
C=== 31 final_temp
      else if(input_name.eq."final_temp")then
        if(par_real8_config.lt.init_temp.and.io_method.eq."w")then
          write(*,*) 
     &"NOTE: final temp < initial temp,reset final_temp to ",init_temp
          par_real8_config=init_temp
        endif
        if(par_real8_config.eq.init_temp.and.io_method.eq."w")then
          write(*,*)
     &"NOTE: initial temp = final temp,reset delta_temp to 1"
          delta_temp=1.D0
          par_real8_config_temp=par_real8_config
          par_real8_config=1.D0
          call name2parameter("delta_temp ","w")
          par_real8_config=par_real8_config_temp
        endif
        final_temp=par_real8_config
C=== 32 delta_temp
      else if(input_name.eq."delta_temp")then
        if(final_temp.eq.init_temp.and.io_method.eq."w")then
          write(*,*) 
     &"NOTE: final_temp = init_temp, reset delta_temp to 1"
          par_real8_config=1.D0
        endif
        delta_temp=par_real8_config
C=== 40 loop_num
      else if(input_name.eq."loop_num")then
        loop_num=par_int_config
C=== 41 init_loop
      else if(input_name.eq."init_loop")then
        if(par_real8_config.gt.final_loop.and.io_method.eq."w")then
          write(*,*)
     &"NOTE: initial loop > final loop, reset init_loop to ",final_loop
          par_real8_config=final_loop
        endif
        init_loop=par_real8_config
C=== 42 final_loop
      else if(input_name.eq."final_loop")then
        if(par_real8_config.lt.init_loop.and.io_method.eq."w")then
          write(*,*)
     &"NOTE: final loop < initial loop, reset final_loop to ",init_loop
          par_real8_config=init_loop
        endif
        final_loop=par_real8_config
C=== 43 break_loop
      else if(input_name.eq."break_loop")then
        break_loop=par_real8_config
C=== 44 delta_loop
      else if(input_name.eq."delta_loop")then
        delta_loop=par_real8_config
C=== 45 source_file_flag
      else if(input_name.eq."source_file_flag")then
        source_file_flag=par_int_config
C=== 46 target_file_flag
      else if(input_name.eq."target_file_flag")then
        target_file_flag=par_int_config
C=== 47 header_file_flag
      else if(input_name.eq."header_file_flag")then
        header_file_flag=par_int_config
C=== 50 lbfgs_GTOL
      else if(input_name.eq."lbfgs_GTOL")then
        lbfgs_GTOL=par_real8_config
C=== 51 lbfgs_MUPDATE
      else if(input_name.eq."lbfgs_MUPDATE")then
        lbfgs_MUPDATE=par_int_config
C=== 52 lbfgs_DGUESS
      else if(input_name.eq."lbfgs_DGUESS")then
        lbfgs_DGUESS=par_real8_config
C=== 53 lbfgs_GMAX
      else if(input_name.eq."lbfgs_GMAX")then
        lbfgs_GMAX=par_real8_config
C=== 54 lbfgs_MP
      else if(input_name.eq."lbfgs_MP")then
        lbfgs_MP=par_int_config
C=== 55 lbfgs_LP
      else if(input_name.eq."lbfgs_LP")then
        lbfgs_LP=par_int_config
C=== 56 lbfgs_STPMIN
      else if(input_name.eq."lbfgs_STPMIN")then
        lbfgs_STPMIN=par_real8_config
C=== 57 lbfgs_STPMAX
      else if(input_name.eq."lbfgs_STPMAX")then
        lbfgs_STPMAX=par_real8_config
C=== 58 lbfgs_MAXIT
      else if(input_name.eq."lbfgs_MAXIT")then
        lbfgs_MAXIT=par_int_config
C=== 59 lbfgs_EPS
      else if(input_name.eq."lbfgs_EPS")then
        lbfgs_EPS=par_real8_config      
C=== 60 simplex_TOL
      else if(input_name.eq."simplex_TOL")then
        simplex_TOL=par_real8_config
C=== 61 simplex_MAXIT
      else if(input_name.eq."simplex_MAXIT")then
        simplex_MAXIT=par_int_config
C=== 70 hist_method
      else if(input_name.eq."hist_method")then
        hist_method=par_int_config
C=== 71 hist_min_lower
      else if(input_name.eq."hist_min_lower")then
        hist_min_lower=par_real8_config
C=== 72 hist_min_upper
      else if(input_name.eq."hist_min_upper")then
        hist_min_upper=par_real8_config
C=== 73 hist_min_delta
      else if(input_name.eq."hist_min_delta")then
        hist_min_delta=par_real8_config
C=== 74 hist_max_lower
      else if(input_name.eq."hist_max_lower")then
        hist_max_lower=par_real8_config
C=== 75 hist_max_upper
      else if(input_name.eq."hist_max_upper")then
        hist_max_upper=par_real8_config
C=== 76 hist_max_delta
      else if(input_name.eq."hist_max_delta")then
        hist_max_delta=par_real8_config
C=== 77 hist_interval_lower
      else if(input_name.eq."hist_interval_lower")then
        hist_interval_lower=par_real8_config
C=== 78 hist_interval_upper
      else if(input_name.eq."hist_interval_upper")then
        hist_interval_upper=par_real8_config
C=== 79 hist_interval_delta
      else if(input_name.eq."hist_interval_delta")then
        hist_interval_delta=par_real8_config
C=== 80 simulation_min_method
      else if(input_name.eq."simulation_min_method")then
        simulation_min_method=par_int_config
C=== 81 simulation_rec_loop
      else if(input_name.eq."simulation_rec_loop")then
        simulation_rec_loop=par_real8_config
C=== 82 simulation_xyz_loop
      else if(input_name.eq."simulation_xyz_loop")then
        simulation_xyz_loop=par_real8_config
C=== 83 simulation_ufe_loop
      else if(input_name.eq."simulation_ufe_loop")then
        simulation_ufe_loop=par_real8_config
C=== 84 simulation_ufx_loop
      else if(input_name.eq."simulation_ufx_loop")then
        simulation_ufx_loop=par_real8_config
C=== 85 simulation_ufv_loop
      else if(input_name.eq."simulation_ufv_loop")then
        simulation_ufv_loop=par_real8_config
C        corr_delta_time=par_real8_config*simulation_delta_time   !Don't forget to reassign this parameter
C        if(wscreen)write(*,*) "NOTE: corr_delta_time set to ",
C     &corr_delta_time
C        par_real8_config_temp=par_real8_config
C        par_real8_config=corr_delta_time
C        call name2parameter("corr_delta_time ","w")
C        par_real8_config=par_real8_config_temp

C        fft_delta_time=corr_delta_time
C        if(wscreen)write(*,*) "NOTE: fft_delta_time set to ",
C     &fft_delta_time
C        par_real8_config_temp=par_real8_config
C        par_real8_config=corr_delta_time
C        call name2parameter("fft_delta_time ","w")
C        par_real8_config=par_real8_config_temp
C=== 86 simulation_ufg_loop
      else if(input_name.eq."simulation_ufg_loop")then
        simulation_ufg_loop=par_real8_config
C=== 87 simulation_vfx_loop
      else if(input_name.eq."simulation_vfx_loop")then
        simulation_vfx_loop=par_real8_config
C=== 90 simulation_delta_time
      else if(input_name.eq."simulation_delta_time")then
        simulation_delta_time=par_real8_config
C=== 91 simulation_reset_thermal
      else if(input_name.eq."simulation_reset_thermal")then
        simulation_reset_thermal=par_int_config
C=== 92 simulation_min_method
      else if(input_name.eq."simulation_min_method")then
        simulation_min_method=par_int_config
C=== 500 PTMC_acceptance_ratio
      else if(input_name.eq."PTMC_acceptance_ratio")then
        PTMC_acceptance_ratio=par_real8_config
C=== 501 PTMC_fix_method
      else if(input_name.eq."PTMC_fix_method")then
        PTMC_fix_method=par_int_config
C=== 502 PTMC_temp_ratio
      else if(input_name.eq."PTMC_temp_ratio")then
        PTMC_temp_ratio=par_real8_config
C=== 503 PTMC_step_ratio
      else if(input_name.eq."PTMC_step_ratio")then
        PTMC_step_ratio=par_real8_config
C=== 504 PTMC_restore_method
      else if(input_name.eq."PTMC_restore_method")then
        PTMC_restore_method=par_int_config
C=== 800 tester_output_flag
      else if(input_name.eq."tester_output_flag")then
        tester_output_flag=par_int_config
C=== 801 score_1st_io
      else if(input_name.eq."score_1st_io")then
        score_1st_io=par_int_config
C=== 802 score_2nd_io
      else if(input_name.eq."score_2nd_io")then
        score_2nd_io=par_int_config
C=== 803 score_3rd_io
      else if(input_name.eq."score_3rd_io")then
        score_3rd_io=par_int_config
C=== 804 score_4th_io
      else if(input_name.eq."score_4th_io")then
        score_4th_io=par_int_config
C=== 805 score_target_init
      else if(input_name.eq."score_target_init")then
        score_target_init=par_real8_config
C=== 806 score_target_final
      else if(input_name.eq."score_target_final")then
        score_target_final=par_real8_config
C=== 807 score_output_flag
      else if(input_name.eq."score_output_flag")then
        score_output_flag=par_int_config
C=== 808 score_function
      else if(input_name.eq."score_function")then
        score_function=par_int_config
C=== 809 corr_delta_time
      else if(input_name.eq."corr_delta_time")then
        corr_delta_time=par_real8_config
C=== 810 corr_observe_time_step
      else if(input_name.eq."corr_observe_time_step")then
        corr_observe_time_step=par_int_config
C=== 811 corr_method
      else if(input_name.eq."corr_method")then
        corr_method=par_int_config
C=== 812 fourier_delta_time
      else if(input_name.eq."fourier_delta_time")then
        fourier_delta_time=par_real8_config
C=== 813 fourier_window_time_step
      else if(input_name.eq."fourier_window_time_step")then
        fourier_window_time_step=par_int_config
C=== 814 fourier_method
      else if(input_name.eq."fourier_method")then
        fourier_method=par_int_config
      endif
      call name2parameter(input_name,"w")
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
