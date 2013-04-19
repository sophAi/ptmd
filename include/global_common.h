       integer I0,I1,I2,I3,I4,I5,I6,I7,I8,I9
       integer J0,J1,J2,J3,J4,J5,J6,J7,J8,J9
       integer rndi
       integer nproc,myid,mpi_err
       logical wscreen,debug
       real*8 time_label
       character time_label_char*15
       common nproc,myid,wscreen,debug,time_label,time_label_char
       real*8 rnd

       character version*5,update*10


C=================parameters======================== 
       integer atom_num_max
       parameter(atom_num_max=200)
       integer ndim_max,ndim_fac_max
       parameter(ndim_max=atom_num_max*3,ndim_fac_max=5)
       integer q_max
       parameter(q_max=atom_num_max*6+9)
       integer ensemble_max
       parameter(ensemble_max=1000)
       integer read_max
       parameter(read_max=50)
       integer time_step_max
       parameter(time_step_max=10000000)
       integer observe_time_step_max
       parameter(observe_time_step_max=5000)  !more then 5000 will cause error,need to be fixed!
       integer header_par_num_max
       parameter(header_par_num_max=100)
       integer octave_par_num_max
       parameter(octave_par_num_max=100)
       integer octave_size_rank_max
       parameter(octave_size_rank_max=100)
       integer hist_dim_max
       parameter(hist_dim_max=50)
       integer hist_bin_max
       parameter(hist_bin_max=100000)
       integer hist_max_lmin,hist_max_pot
       parameter(hist_max_lmin=100000,hist_max_pot=100000)
       integer cn_max,dig1_max,dig2_max,dig3_max,dig4_max
       integer cn_history_max
       parameter(cn_history_max=100000)
       parameter(dig1_max=2,dig2_max=7,dig3_max=8,dig4_max=3)
       parameter(cn_max=dig1_max*dig2_max*dig3_max*dig4_max) !2*7*8*3=336
       integer p_matrix_max
       parameter(p_matrix_max=cn_max)
       integer hash_max,hash_prime
       parameter(hash_max=500000,hash_prime=997)
       integer sliding_p_length_max
       parameter(sliding_p_length_max=448)
       integer moment_z_max,moment_y_max
       parameter(moment_z_max=20,moment_y_max=atom_num_max)
       integer tdusr_lmin_max,mom_target_max
       parameter(tdusr_lmin_max=100,mom_target_max=100)
       integer nim_dim_max
       parameter(nim_dim_max=100)
       integer top_num_max
       parameter(top_num_max=4)
       integer periodic_num_max
       parameter(periodic_num_max=500)
C=================constants=================================
       real*8 inf_const
       parameter(inf_const=9.D10)

       integer par_max
       parameter(par_max=1000)
