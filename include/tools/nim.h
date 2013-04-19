      integer nim_dim,nim_eff_num,nim_threshold
      integer nim_matrix(nim_dim_max,nim_dim_max)
      integer nim_eff_var(nim_dim_max,nim_dim_max),
     &nim_var_num(nim_dim_max),nim_flag(nim_dim_max,nim_dim_max)

      common/nim_int0/nim_dim,nim_eff_num,nim_threshold
      common/nim_int1/nim_matrix,nim_eff_var,nim_var_num,nim_flag

      integer nim_hash_sum(nim_dim_max),nim_hash_tra(nim_dim_max)
      common/nim_int2/nim_hash_sum,nim_hash_tra
