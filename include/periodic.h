      integer periodic_total_num,periodic_id(atom_num_max)
      integer periodic_atomic_num(periodic_num_max)
  
      common /periodic_int0/periodic_total_num,periodic_id,
     &periodic_atomic_num
 
      real*8 periodic_atomic_mass(periodic_num_max)

      common /periodic_real0/periodic_atomic_mass

      character*4 periodic_element(periodic_num_max)

      common /periodic_char0/periodic_element
