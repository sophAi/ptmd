      real*8 e_dip,e_cap,e_vdw,e_screen
      common /colloid_real0/e_dip,e_cap,e_vdw,e_screen

      real*8 radi_s,radi_m,radi_l,col_angle,ha,hw
      common/colloid_real1/radi_s,radi_m,radi_l,col_angle,ha,hw          !vwd
      real*8 tension,loa,lob,los,gravity
      common/colloid_real2/tension,loa,lob,los,gravity                   !cap
      real*8 z_s,z_m,z_l,e,esio,esi,kapa_s,kapa_m,kapa_l
      common/colloid_real3/z_s,z_m,z_l,e,esio,esi,kapa_s,
     &kapa_m,kapa_l   !ele
      real*8 disteq,rmax_fac
      common/colloid_real4/disteq,rmax_fac                   !container
      real*8 ther,col_p,col_q(atom_num_max,atom_num_max),
     &col_f(atom_num_max,atom_num_max),
     &col_k(atom_num_max,atom_num_max),col_h,col_b(atom_num_max),
     &rmax,landa
      common/colloid_real5/ther,col_p,col_q,col_f,col_k,col_h,
     &RMAX,landa !Parameters needed by potential
      real*8 col_radius,col_radius2
      common/colloid_real6/col_radius,col_radius2
      real*8 radi(atom_num_max),z(atom_num_max),kapa(atom_num_max),
     &col_kapa(atom_num_max,atom_num_max)
      common/colloid_real7/radi,z,kapa,col_kapa

