        subroutine comvel   !initialized and output v
        implicit none
        include "../include/global_common.h"
        include "../include/common.h"
        include "../include/pes.h"
        include "../include/BIMD/BIMD.h"
	real*8 dummy,dumx,dumy,dumz,temp_v,t_test,gauss,sum1
        real*8 kinetic_per_atom,total_mass
!       external gauss
        kinetic_per_atom=3D0*1000.D0*temp*kb
        kinetic_per_dim=1000.D0*temp*kb
        temp_v=dsqrt(kinetic_per_atom)
        do J1=1,atom_num
          J2=3*J1
          if (J1.le.atom_num_a) then
            v(J2-2)=temp_v *gauss()/dsqrt(mass_a)
            v(J2-1)=temp_v *gauss()/dsqrt(mass_a)
            v(J2)= temp_v *gauss()/dsqrt(mass_a)
          else
            v(J2-2)=temp_v *gauss()/dsqrt(mass_b)
            v(J2-1)=temp_v *gauss()/dsqrt(mass_b)
            v(J2)= temp_v *gauss()/dsqrt(mass_b)
          endif  
        enddo
        dumx=0.D0
        dumy=0.D0
        dumz=0.D0
        do J1=1,atom_num_a
          J2=J1*3
          dumx=dumx+mass_a*v(J2-2) 
          dumy=dumy+mass_a*v(J2-1) 
          dumz=dumz+mass_a*v(J2) 
        enddo
        do J1=atom_num_a+1,atom_num
          J2=J1*3
          dumx=dumx+mass_b*v(J2-2)
          dumy=dumy+mass_b*v(J2-1)
          dumz=dumz+mass_b*v(J2)
        enddo
        total_mass=dble(atom_num_a)*mass_a+dble(atom_num_b)*mass_b
        dumx=dumx / total_mass
        dumy=dumy / total_mass
        dumz=dumz / total_mass
        do J1=1,atom_num
          J2=J1*3
          v(J2-2)=v(J2-2) - dumx
          v(J2-1)=v(J2-1) - dumy
          v(J2)=v(J2) - dumz    
        enddo
        sum1=0.D0
        do J1=1,atom_num
          J2=J1*3
          if (J1.le.atom_num_a) then
            sum1=sum1+mass_a*(v(J2-2)**2+ v(J2-1)**2 +v(J2)**2)
          else
            sum1=sum1+mass_b*(v(J2-2)**2+ v(J2-1)**2 +v(J2)**2)
          endif
        enddo
        t_test=(sum1 / dble(atom_num))
        do J1=1,atom_num
          J2=J1*3
          v(J2-2)=v(J2-2)*dsqrt(kinetic_per_atom/t_test)
          v(J2-1)=v(J2-1)*dsqrt(kinetic_per_atom/t_test)
          v(J2)=v(J2)*dsqrt(kinetic_per_atom/t_test)
        enddo
        return 
        end

        real*8 function gauss
        implicit none
        include "../include/global_common.h"
        include "../include/common.h"
        include "../include/BIMD/BIMD.h"
        real*8 a1,a3,a5,a7,a9,sum1,r,r2,i,x1
        parameter (a1=3.949846138D0 ,a3=0.252408784D0 )
        parameter (a5=0.076542912D0 ,a7=0.008355968D0 )
        parameter (a9=0.029899776D0)
        sum1=0.D0
        do i=1,12
        call random(x1)
        sum1=sum1+x1
        end do
        r=(sum1 -6.D0)/4.D0
        r2=r*r
        gauss = (((( a9*r2 +a7 ) *r2 +a5 )*r2 +a3 )*r2+a1) *r
        return
        end


        subroutine random(x1)
        implicit none
        include "../include/global_common.h"
        include "../include/common.h"
        include "../include/BIMD/BIMD.h"
        real*8 x1
        do j1=1,atom_num*3
           x1=1.D0*ran()
        enddo
        return
        end
