      integer octave_par_num
      integer octave_par_line_num(octave_par_num_max)
      integer octave_size_rank(octave_par_num_max),
     &octave_size(octave_par_num_max,octave_size_rank_max)
      common /octave_int0/octave_par_num,octave_par_line_num,
     &octave_size_rank,octave_size
      character octave_par_name(octave_par_num_max)*25
      character octave_par_type(octave_par_num_max)*15
      common /octave_char0/ octave_par_name,octave_par_type
      logical octave_clear,octave_who
      common /octave_log0/ octave_clear,octave_who
