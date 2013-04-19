      integer simulation_min_method,simulation_init_loop,
     &simulation_final_loop,simulation_reset_thermal
 
      common/simulation_int0/simulation_min_method,
     &simulation_init_loop,simulation_final_loop,
     &simulation_reset_thermal

      real*8 simulation_rec_loop,simulation_ufe_loop,
     &simulation_xyz_loop,simulation_ufx_loop,simulation_ufv_loop,
     &simulation_ufg_loop,simulation_vfx_loop
 
      common/simulation_real0/simulation_rec_loop,simulation_ufe_loop,
     &simulation_xyz_loop,simulation_ufx_loop,simulation_ufv_loop,
     &simulation_ufg_loop,simulation_vfx_loop
 
      real*8 simulation_rec_num,simulation_xyz_num,simulation_ufe_num,
     &simulation_ufx_num,simulation_ufv_num,simulation_ufg_num,
     &simulation_vfx_num

      common/simulation_real1/simulation_rec_num,simulation_xyz_num,
     &simulation_ufe_num,simulation_ufx_num,simulation_ufv_num,
     &simulation_ufg_num,simulation_vfx_num

      real*8 simulation_delta_time,simulation_time

      common/simulation_real2/simulation_delta_time,simulation_time

      character*80 simulation_thermal_file,simulation_thermal_ref,
     &simulation_restore_file,
     &simulation_restore_history,simulation_pes_ref,
     &simulation_usr_file,simulation_cnl_file,simulation_anl_file,
     &simulation_xyz_file,simulation_x_unformatted_file,
     &simulation_g_unformatted_file,simulation_v_unformatted_file,
     &simulation_e_unformatted_file,
     &simulation_vfx_unformatted_file

      common/simulation_file_char0/ simulation_thermal_file,
     &simulation_restore_file,simulation_thermal_ref,
     &simulation_restore_history,simulation_pes_ref,
     &simulation_usr_file,simulation_cnl_file,simulation_anl_file,
     &simulation_xyz_file,simulation_x_unformatted_file,
     &simulation_g_unformatted_file,simulation_v_unformatted_file,
     &simulation_e_unformatted_file,
     &simulation_vfx_unformatted_file

