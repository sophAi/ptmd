      integer header_par_num

      common /header_int0/ header_par_num
      
      real*8 header_par_real(header_par_num_max),header_output
       
      common /header_real0/ header_par_real,header_output

      character header_par_name(header_par_num_max)*25,
     &header_source_type*3,header_annotation*80
 
      common/header_char0/header_par_name,header_source_type,
     &header_annotation

      logical header_found
      
      common/header_logic0/header_found

C========Octave headers===================
 
      integer header_columns,header_rows,
     &header_length,header_elements

      common /octave_header_int0/header_columns,header_rows,
     &header_length,header_elements

      character header_name*25,header_type*25
     
      common /octave_header_char0/header_name,header_type
