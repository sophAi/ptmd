      integer use_top_sort,top_num_sort,top_id_sort(top_num_max)
      real*8 top_value_sort(top_num_max,atom_num_max)
      common /top_int1/use_top_sort,top_num_sort,top_id_sort
      common /top_real1/top_value_sort
      character top_type_sort(top_num_max)*3,
     &top_name_sort(top_num_max)*3
      common /top_char1/top_type_sort,top_name_sort

