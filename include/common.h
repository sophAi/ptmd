       integer ndim_fac    !for 3-D this value=3,for 2-D this value=2
       integer ndim,ndim_a,ndim_b,ndim_c,
     &ndim_id(atom_num_max,ndim_fac_max)
       integer atom_num,atom_num_a,atom_num_b,atom_num_c
       integer N1,N2,N3,N4,N5,N6,N7,N8,N9,N10
       common /dimension0/ndim_fac
       common /dimension1/ndim,ndim_a,ndim_b,ndim_c,ndim_id
       common /dimension2/atom_num,atom_num_a,
     &atom_num_b,atom_num_c                  !ndim=atom_num*dimension_fac

       character atom_name_a*10,atom_name_b*10,atom_name_c*10
       character atom_name(atom_num_max)*10,config_name*18
       character atom_num_name*4,atom_num_name_a*4,atom_num_name_b*4,
     &atom_num_name_c*4
       common /dimension3/atom_name_a,atom_name_b,atom_name_c,
     &atom_num_name_a,atom_num_name_b,atom_num_name_c
       common /dimension4/atom_name,atom_num_name,config_name
       common /dimension5/N1,N2,N3,N4,N5,N6,N7,N8,N9,N10

       real*8 read_unit
C============global thermal=========================================
       real*8 kb
       common /thermal_real0/kb
C============lmin===================================================
       integer min_PES_method
       common /lmin_int0/min_PES_method
C============simplex================================================      
       integer simplex_MAXIT
       common /simplex_int0/simplex_MAXIT

       real*8 simplex_TOL,simplex_acc
       common /simplex_real0/ simplex_tol,simplex_acc
 
       logical simplex_converge
       common /simplex_logical0/simplex_converge
C============lbfgs==================================================
       integer lbfgs_MUPDATE,lbfgs_MAXIT,lbfgs_MP,lbfgs_LP
       common /lbfgs_int0/ lbfgs_MUPDATE,lbfgs_MAXIT

       real*8 lbfgs_GMAX,lbfgs_DGUESS,lbfgs_EPS,lbfgs_XTOL
     &,lbfgs_GTOL,lbfgs_STPMIN,lbfgs_STPMAX
       common /lbfgs_real0/lbfgs_GMAX,lbfgs_DGUESS,lbfgs_XTOL,
     &lbfgs_EPS

C============step=================================================    
       integer init_loop_int,final_loop_int,delta_loop_int,
     &total_loop_int
       common /step_int0/init_loop_int,final_loop_int,delta_loop_int,
     &total_loop_int

       real*8 init_loop,final_loop,break_loop,last_loop,start_loop,
     &end_loop,delta_time,delta_loop,total_loop
       common /step_real0/init_loop,final_loop,break_loop,
     &last_loop,start_loop,end_loop,delta_time,delta_loop,total_loop

       real*8 temp,delta_temp,init_temp,final_temp
       common /thermal_real0/temp,delta_temp,init_temp,final_temp

       common /LB3/ lbfgs_MP,lbfgs_LP,lbfgs_GTOL,lbfgs_STPMIN,
     &lbfgs_STPMAX
       logical lbfgs_converge
       common /lbfgs_logical0/lbfgs_converge

       logical NaN_flag,Inf_flag,evap_flag,error_input
       common /job_fail0/NaN_flag,Inf_flag,evap_flag,error_input
 
