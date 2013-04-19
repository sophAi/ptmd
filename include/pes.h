      integer pes_id,pes_line_num,total_pes_num,edit_pes_flag

      common /pes_type0/pes_id,pes_line_num,total_pes_num,
     &edit_pes_flag

      integer evap_num
  
      common /pes_int0/evap_num

      real*8 pot,kinetic,energy
      real*8 mean_pot_per_atom,mean_kinetic_per_atom,
     &mean_energy_per_atom
C==============coordinate information===================
      real*8 xinit(ndim_max),global_pot,mean_global_pot
      real*8 x(ndim_max),grad(ndim_max),v(ndim_max),rms,
     &pot_per_atom(atom_num_max) !pot_per_atom for BH  
      real*8 x_prev(ndim_max),grad_prev(ndim_max),pot_prev,rms_prev,
     &pot_per_atom_prev(atom_num_max),v_prev(ndim_max)
      real*8 x2(ndim_max),grad2(ndim_max),pot2,rms2,
     &pot2_per_atom(atom_num_max),v2(ndim_max)
      real*8 x_org(ndim_max),grad_org(ndim_max),pot_org,rms_org,
     &pot_per_atom_org(atom_num_max),v_org(ndim_max)

      real*8 dist(atom_num_max,atom_num_max),
     &dist_org(atom_num_max,atom_num_max),
     &dist_prev(atom_num_max,atom_num_max)
      real*8 bond_num(atom_num_max)    !bond num for each atom
      real*8 bond_num_org(atom_num_max)
      real*8 bond_num2(atom_num_max),bond_num_prev(atom_num_max)   !for step_move
C=============center of mass==========================================
      real*8 com_x,com_y,com_z,com_vx,com_vy,com_vz,
     &ang_v(ndim_max)
C=============unchange parameters=========================================

      real*8 outer_bound_radius,inner_bound_radius,restore_fac  !restore_fac usually is 10.D0 
      real*8 restore_per_atom(atom_num_max)      
      real*8 mass_a,mass_b,tmass_a,tmass_b
      real*8 mass(atom_num_max),tmass(atom_num_max)
      real*8 rcut,rcut_unit
      real*8 bond_cutoff(atom_num_max,atom_num_max),
     &bond_cutoff_ratio
      real*8 moving_length

      parameter(bond_cutoff_ratio=1.2D0)
      common /pes0/pot,kinetic,energy       !energy=pot+kinetic
      common /pes1/mean_pot_per_atom,mean_kinetic_per_atom,
     &mean_energy_per_atom
C=============================================
      common /pes2/xinit,global_pot,mean_global_pot
      common /pes3/x,grad,v,bond_num,rms,pot_per_atom,dist
      common /pes4/x2,grad2,v2,bond_num2,rms2,pot2_per_atom
      common /pes5/x_prev,grad_prev,v_prev,bond_num_prev,rms_prev,
     &pot_per_atom_prev,dist_prev,pot_prev
      common /pes5/x_org,grad_org,v_org,bond_num_org,rms_org,
     &pot_per_atom_org,dist_org
C==============================================
      common /pes7/outer_bound_radius,inner_bound_radius,
     &restore_fac,restore_per_atom
      common /pes8/mass_a,mass_b,tmass_a,tmass_b
      common /pes9/mass,tmass
      common /pes10/rcut,rcut_unit
      common /pes11/bond_cutoff,moving_length
      common /pes12/com_x,com_y,com_z,com_vx,com_vy,com_vz,
     &ang_v
C=============job pes data=================
      character pes_type*15,pes_content*30      
      common /pes_id0/ pes_type,pes_content
 
      logical evap
      logical find_1st_grad,numerical_1st_grad   !1st derivative
      logical find_2nd_grad,numerical_2nd_grad   !Hessian Matrix

      common /evap0/evap
      common /find_1st_grad0/find_1st_grad,numerical_1st_grad
      common /find_2nd_grad0/find_2nd_grad,numerical_2nd_grad
C=============Topology information========
      integer use_top,top_num,top_id(top_num_max)
      real*8 top_value(top_num_max,atom_num_max)
      common /top_int0/use_top,top_num,top_id
      common /top_real0/top_value
      character top_type(top_num_max)*3,top_name(top_num_max)*3
      common /top_char0/top_type,top_name
 
C=============Topology usr================
      integer top_usr_num,top_use_usr,top_usr_io(atom_num_max),
     &top_usr_id(atom_num_max),top_usr_locate
      common /top_usr_int0/top_usr_num,top_use_usr,top_usr_io,
     &top_usr_id,top_usr_locate
   
C=============Topology frz================
      integer top_frz_num,top_use_frz,top_frz_io(atom_num_max),
     &top_frz_id(atom_num_max),top_frz_locate
      common /top_frz_int0/top_frz_num,top_use_frz,top_frz_io,
     &top_frz_id,top_frz_locate
C=============Topology tcp================
      integer top_use_tcp,top_tcp_locate
      common /top_tcp_int0/top_use_tcp,top_tcp_locate
C=============Topology acc================
      integer top_use_acc,top_acc_locate
      common /top_acc_int0/top_use_acc,top_acc_locate
