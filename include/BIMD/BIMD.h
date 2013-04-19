      integer q_dim
      common /BIMD_int0/q_dim      

      real*8 c,atomic_mass,pi
      real*8 kinetic_per_dim 
  
      common/global_BIMD0/ c,atomic_mass,pi

      common/BIMD_real0/kinetic_per_dim

      real*8 mem1,multiply,I_N,I_N_over_2,I_N_over_3,I_NT,d0_over_NT,
     &p_factor(atom_num_max),pseudo_factor
      
      common /BIMD_tdiff_rea0/mem1,multiply,I_N,I_N_over_2,I_N_over_3,
     ^I_NT,d0_over_NT,p_factor,pseudo_factor

C==========================output================================== 
      real*8 BIMD_ave_energy,BIMD_ave_energy_square,BIMD_Cv,
     &BIMD_ave_x(ndim_max),
     &BIMD_ave_bond,BIMD_ave_temper,BIMD_ave_pot,BIMD_ave_kinetic

      common/BIMD_total_ave0/BIMD_ave_energy,BIMD_ave_energy_square,
     &BIMD_Cv,BIMD_ave_temper,BIMD_ave_pot,BIMD_ave_kinetic
      common/BIMD_total_ave1/BIMD_ave_x,BIMD_ave_bond

      real*8 BIMD_ave_dist(atom_num_max,atom_num_max),
     &BIMD_ave_dist2(atom_num_max,atom_num_max)
      common/BIMD_total_ave2/BIMD_ave_dist,BIMD_ave_dist2

      real*8 BIMD_equ_ave_energy,BIMD_equ_ave_energy_square,
     &BIMD_equ_Cv,BIMD_equ_ave_temper,BIMD_equ_ave_bond,
     &BIMD_equ_ave_pot,BIMD_equ_ave_kinetic

      real*8 DL0,DP0(atom_num_max),I_DP02(atom_num_max),
     &DA0,DP0_over_mass(atom_num_max),
     &I_DP0_mass(atom_num_max),I_DP02_mass(atom_num_max),
     &I_DP0(atom_num_max),DE0(atom_num_max)
      real*8 alpha(atom_num_max),beta(atom_num_max),chi(atom_num_max)
      common/BIMD_tdiff0/DL0,DP0,I_DP0,I_DP02,DP0_over_mass,
     &I_DP0_mass,I_DP02_mass,DA0,DE0  
      common/BIMD_tdiff1/alpha,beta,chi

      character*80 BIMD_thermal_file,BIMD_pes_file,BIMD_restore_file
      character*80 BIMD_thermal_ref,BIMD_pes_ref,BIMD_usr_file,
     &BIMD_restore_history,BIMD_cnl_file,BIMD_anl_file,BIMD_xyz_file,
     &BIMD_x_unformatted_file,BIMD_g_unformatted_file,
     &BIMD_v_unformatted_file,BIMD_pot_unformatted_file

      common/BIMD_file0/BIMD_thermal_file,BIMD_pes_file,
     &BIMD_restore_file
      common/BIMD_file1/BIMD_thermal_ref,BIMD_pes_ref,
     &BIMD_restore_history,BIMD_cnl_file,BIMD_anl_file,
     &BIMD_usr_file,BIMD_xyz_file,BIMD_x_unformatted_file,
     &BIMD_g_unformatted_file,BIMD_v_unformatted_file,
     &BIMD_pot_unformatted_file
