      integer par_id,par_int_def,par_int_min,par_int_max,
     &par_int_config

      logical par_found

      common/parameter_int0/par_id,par_int_def,par_int_min,
     &par_int_max,par_int_config

      common/parameter_log0/par_found

      real*8 par_real8_def,par_real8_min,par_real8_max,
     &par_real8_config
      common/parameter_real0/par_real8_def,par_real8_min,
     &par_real8_max,par_real8_config

      character par_type*10 !r4,r8,i4,i8,ch,lo
      character par_catalog*8
      character par_name*25
      character par_description*50

      common/parameter_char0/par_type,par_catalog,par_name,
     &par_description
    

      integer par_map_id(par_max),total_par_num

      common/parameter_map0/par_map_id,total_par_num

      integer par_read_id(par_max),par_read_int_def(par_max),
     &par_read_int_min(par_max),par_read_int_max(par_max)

      common/parameter_read_int0/par_read_id,par_read_int_def,
     &par_read_int_min,par_read_int_max

      real*8 par_read_real8_def(par_max),par_read_real8_min(par_max),
     &par_read_real8_max(par_max),par_read_real8_config(par_max)

      common/parameter_read_real0/par_read_real8_def,par_read_real8_min,
     &par_read_real8_max,par_read_real8_config

      character par_read_type(par_max)*10
      character par_read_name(par_max)*25
      character par_read_catalog(par_max)*8
      character par_read_description(par_max)*50

      common/parameter_read_char0/par_read_type,par_read_name,
     &par_read_description,par_read_catalog

      integer par_read_config_id(par_max),total_par_config_num,
     &par_hist_frame_num,par_total_hist_frame
      common/par_read_config_int0/par_read_config_id,
     &total_par_config_num,par_hist_frame_num,par_total_hist_frame

      real*8 par_read_config_from_file(par_max)
      common/par_read_config_real0/ par_read_config_from_file

      
