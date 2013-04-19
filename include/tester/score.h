      integer score_moment_dim,score_moment_sub_dim

      common /score_moment_int0/score_moment_dim,score_moment_sub_dim

      real*8 score_y(moment_y_max),
     &score_mean(moment_y_max),score_temp
      common /score_moment_real0/ score_y,score_mean,score_temp
      real*8 score_1st_moment(moment_y_max),
     &score_2nd_moment(moment_y_max),score_3rd_moment(moment_y_max),
     &score_4th_moment(moment_y_max)
      common /score_moment_real1/ score_1st_moment,score_2nd_moment,
     &score_3rd_moment,score_4th_moment

      integer score_output_flag,score_1st_io,score_2nd_io,
     &score_3rd_io,score_4th_io,score_function

      common /score_moment_int1/ score_output_flag,score_1st_io,
     &score_2nd_io,score_3rd_io,score_4th_io,score_function

      integer target_init,target_final

      common /score_moment_int2/ target_init,target_final
      real*8 score_target_init,score_target_final

      common /score_moment_real1/ score_target_init,score_target_final

      character score_time_file_name*80,
     &score_mean_file_name*80,score_mom_time_file_name*80

      common /score_moment_char0/
     &score_time_file_name,score_mean_file_name,
     &score_mom_time_file_name
