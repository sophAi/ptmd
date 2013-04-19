       integer total_neighbors,max_dig1,max_dig2,max_dig3,max_dig4
       integer pair_digits,dig1,dig2,dig3,dig4,matrix_index,pair_sw
       integer pair_num,pair_aboundance(cn_max),
     &pair_analysis(10000,cn_max)
       integer pair_digits_index(cn_max)
       integer pair_aboundance_total(cn_max)
     &,cn_frame_history(cn_history_max)
       real*8 pair_xinib(ndim_max)
       character pair_name*4,cn_name*200,read_cn_name*200
     &,cn_name_history(cn_history_max)*200,cn_prev_name*200
       common/neighbor1/total_neighbors,max_dig1,max_dig2,max_dig3,
     &max_dig4
     &,cn_frame_history
       common/neighbor3/pair_num,pair_aboundance,
     &pair_digits_index,pair_aboundance_total
 
       common/neighbor4/cn_name,read_cn_name
     &,cn_name_history,cn_prev_name
 
       integer cn_hash_total_pair_num
       integer cn_hash,cn_hash_num(hash_max),cn_hash_total_num
       common /cn_hash_int0/cn_hash,cn_hash_num,cn_hash_total_num
     &,cn_hash_total_pair_num
       real*8 cn_hash_weight(hash_max)
       common /cn_hash_real0/ cn_hash_weight

       character cn_hash_name(hash_max)*200
    
       logical cn_hash_exist(hash_max)
       common /cn_hash_logical0/cn_hash_exist

       integer cn_digits(cn_max),cn_aboundance(cn_max),
     &cn_matrix(cn_max),total_pair_digits
       common /cn2digits_int0/cn_digits,cn_aboundance,cn_matrix,
     &total_pair_digits
