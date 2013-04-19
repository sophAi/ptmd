       integer cst_id,fct_id,ftf_id
       common /usr_int0/ cst_id,fct_id,ftf_id
       real*8 dist_ctd(atom_num_max),dist_cst(atom_num_max)
       real*8 dist_fct(atom_num_max),dist_ftf(atom_num_max)
       real*8 usr_score,usr_entropy
       common /usr_real1/ usr_score,usr_entropy

       integer usr_top_atom_num,usr_use_top
       integer usr_top_matrix(atom_num_max)
       common /usr_top_int0/usr_top_atom_num,usr_use_top,usr_top_matrix
