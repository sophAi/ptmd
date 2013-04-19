       real*8 p1,q1,epsilon1,zeta1,rzero1
       real*8 p2,q2,epsilon2,zeta2,rzero2
       real*8 p3,q3,epsilon3,zeta3,rzero3
 
       common/gp1/p1,q1,epsilon1,zeta1,rzero1
       common/gp2/p2,q2,epsilon2,zeta2,rzero2
       common/gp3/p3,q3,epsilon3,zeta3,rzero3

       real*8 p_ab(atom_num_max,atom_num_max),
     &q_ab(atom_num_max,atom_num_max),
     &epsilon_ab(atom_num_max,atom_num_max),
     &zeta_ab(atom_num_max,atom_num_max),
     &rzero_ab(atom_num_max,atom_num_max)
 
       common/gp4/p_ab,q_ab,epsilon_ab,zeta_ab,rzero_ab

       real*8 FAC_GRAD_ATT,FAC_GRAD_REP

       common/gp_grad0/FAC_GRAD_ATT,FAC_GRAD_REP
