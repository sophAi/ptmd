       integer file_line_num,file_row_num,list_file_num
       integer file_x_dim,file_y_dim,file_z_dim,header_line_num
       common/file_dim_int/file_x_dim,file_y_dim,file_z_dim
     &,header_line_num

       character*80 file_annotation
       common/file_annotation0/file_annotation

       character*80 file_path,input_path,output_path,thermal_path,
     &status_path,representation_path,action_path,CN_network_path
  
       common/write_file_path0/file_path,input_path,thermal_path,
     &output_path,status_path,representation_path,action_path,
     &CN_network_path

       character*80 rec_path,xyz_path,dat_path,bimd_path,ptmc_path,
     &usr_path,mom_path,cnl_path,anl_path,dtf_path,cor_path,his_path,
     &fft_path,scr_path

       common/write_file_path1/rec_path,xyz_path,bimd_path,ptmc_path,
     &dat_path,mom_path,usr_path,cnl_path,anl_path,dtf_path,cor_path,
     &his_path,fft_path,scr_path

       character*80 list_changed_file,life_file,job_file,
     &job_table_file,pes_file,version_file,wscreen_file,
     &unit_file,parameter_file,program_status_file,job_status_file,
     &debug_file,parameter_config_file,parameter_hist_file,
     &periodic_file
    

       common/write_file_shell0/file_line_num,file_row_num,
     &list_file_num
       common/write_file_shell1/list_changed_file,
     &life_file,job_file,job_table_file,pes_file,version_file
     &,wscreen_file,unit_file,parameter_file,program_status_file
     &,job_status_file,debug_file,parameter_config_file,
     &parameter_hist_file,periodic_file

       integer source_file_flag

       common/source_file0/ source_file_flag

       character source_file_name*80

       common/source_file1/source_file_name

       integer target_file_flag

       common/target_file0/ target_file_flag

       character target_file_name*80

       common/target_file1/target_file_name

       integer header_file_flag
 
       common/header_file0/ header_file_flag
  
       character header_file_name*80

       common/header_file1/header_file_name

       integer char_length
       character file_name*80,int2char*4,file_main*80,file_ext*5

       logical file_exist,file_opened      
       common /file_status0/file_exist,file_opened

       integer read_int(read_max)
       real*8 read_real8(read_max)
       character*80 read_flag(read_max),read_char(read_max)

       character*80 init_coord_file_name
       common /init_coord_char/ init_coord_file_name 

C========These are used for the general file structure
       character*80 root_path,simulation_path,source_path,type_path,
     &result_root_path,target_result_root_path,
     &header_result_root_path,target_path,target_type_path,
     &target_simulation_path,header_path,header_type_path,
     &header_simulation_path
     
       common /file_structure_char0/root_path,simulation_path,
     &type_path,source_path,result_root_path,
     &target_result_root_path,header_result_root_path,target_path,
     &target_type_path,target_simulation_path,header_path,
     &header_type_path,header_simulation_path

     
       character simulation_type*4,source_type*3,pes_file_name*40,
     &target_pes_file_name*40,header_pes_file_name*40,
     &parallel_parameter*4,file_description*20,file_type*3,
     &source_file_type*3,target_type*3,target_file_type*3,
     &target_simulation_type*3,header_file_source_type*3,
     &header_file_type*3,header_simulation_type*3
       
       common /file_structure_char1/simulation_type,source_type,
     &pes_file_name,parallel_parameter,file_description,file_type,
     &source_file_type,target_type,target_file_type,
     &target_simulation_type,header_file_source_type,header_file_type,
     &header_simulation_type

         
