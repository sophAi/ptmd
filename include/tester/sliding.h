      integer total_p_num,total_s_num
      integer sliding_width,window_width
      integer p_matrix_length,s_matrix_length
      integer c_id_ij(cn_max,cn_max),c_id_i(cn_max*cn_max),
     &c_id_j(cn_max*cn_max)      

      common/sliding_int0/ total_p_num,total_s_num,p_matrix_length
     &,s_matrix_length
      common/sliding_int1/ sliding_width,window_width
      common/sliding_int2/ c_id_ij,c_id_i,c_id_j
   
      character sliding_file_name*80
      
      common/sliding_char0/ sliding_file_name
