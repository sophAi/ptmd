      integer PTMC_fix_method,PTMC_restore_method
      common/PTMC_integer0/PTMC_restore_method,PTMC_fix_method

      real*8 PTMC_ave_pot,PTMC_ave_pot_square,
     &PTMC_ave_kinetic,PTMC_ave_kinetic_square,
     &PTMC_ave_energy,PTMC_ave_energy_square,PTMC_Cv
      common/PTMC_thermal_real0/PTMC_ave_pot,
     &PTMC_ave_pot_square,PTMC_Cv,
     &PTMC_ave_energy,
     &PTMC_ave_energy_square,PTMC_ave_kinetic,
     &PTMC_ave_kinetic_square

      real*8 transition_func
      real*8 PTMC_acceptance_ratio,PTMC_temp_ratio,PTMC_step_ratio,
     &MC_acceptance_ratio
      common/PTMC_real1/PTMC_acceptance_ratio,PTMC_temp_ratio,
     &PTMC_step_ratio,MC_acceptance_ratio

      character*80 PTMC_thermal_file,PTMC_pes_file,PTMC_restore_file,
     &PTMC_thermal_ref,PTMC_pes_ref,PTMC_restore_history,PTMC_xyz_file

      common/PTMC_char_file0/PTMC_thermal_file,PTMC_pes_file,
     &PTMC_restore_file,PTMC_thermal_ref,PTMC_pes_ref,
     &PTMC_restore_history,PTMC_xyz_file
C==========less important==================
      character*80 PTMC_usr_file,PTMC_cnl_file,PTMC_anl_file

      common/PTMC_char_file1/PTMC_usr_file,PTMC_cnl_file,
     &PTMC_anl_file
