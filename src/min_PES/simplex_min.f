*========================================================================
* File Name : simplex_min.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時52分30秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine simplex_init
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
      
       return
       end

       Subroutine simplex_min
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       real*8 x_bac(ndim_max),y(ndim_max),p(ndim_max+1,ndim_max)
     &,DELTAX,pot_bac
       integer i,j,mini
       simplex_tol=1.D-7
       mini=1
       EVAP=.false.
       simplex_converge=.false.
*** The parameter of simplex method ***
C       DELTAX=4.D0
       DELTAX=0.4D0
***************************************
       call pes
C       write(*,*) "Check before simplex,pot=",pot
       pot_bac=pot
       do I0=1,ndim
         x_bac(I0)=x(I0)
       enddo
       do I0=1,ndim+1
         do J0=1,ndim
           if(I0.eq.1) then
             p(I0,J0)=x_bac(J0)
             x(J0)=p(I0,J0) 
           else if(J0.eq.(I0-1)) then
             p(I0,J0)=p(1,J0)+DELTAX
             x(J0)=p(I0,J0)
           else
             p(I0,J0)=p(1,J0)
             x(J0)=p(I0,J0)
           endif
         enddo 
         call pes
         y(I0)=pot
C         write(*,*) i,j,"In simplex,pot=",pot
       enddo 
       call amoeba(p,y)
       do I0=2,ndim+1
         if(y(I0).lt.y(mini)) then
           mini=I0
         endif
       enddo
       do I0=1,ndim
         x(I0)=p(mini,I0)
       enddo
       pot=y(mini)
       return
       end
       
       
       subroutine amoeba(p,y)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/pes.h"
       integer iter
       integer i,ihi,ilo,inhi,j,m,n
       real*8 p(ndim_max+1,ndim_max),y(ndim_max+1)
C       parameter (ITMAX=2**30-2)
       real*8 rtol,sum,swap,ysave,ytry,psum(ndim_max),amotry
       iter=0
1      do n=1,ndim
         sum=0.D0
         do m=1,ndim+1
           sum=sum+p(m,n)
         enddo
         psum(n)=sum
       enddo
2      ilo=1
       if(y(1).gt.y(2)) then
         ihi=1
         inhi=2
       else
         ihi=2
         inhi=1
       endif
       do i=1,ndim+1
         if(y(i).le.y(ilo)) ilo=i
         if(y(i).gt.y(ihi)) then
           inhi=ihi
           ihi=i
         else if(y(i).gt.y(inhi)) then
           if(i.ne.ihi) inhi=i
         endif
       enddo
       rtol=2.D0*dabs(y(ihi)-y(ilo))/(dabs(y(ihi))+dabs(y(ilo)))
       if(rtol.lt.simplex_tol) then
         swap=y(1)
         y(1)=y(ilo)
         y(ilo)=swap
         do n=1,ndim
           swap=p(1,n)
           p(1,n)=p(ilo,n)
           p(ilo,n)=swap
         enddo
         simplex_converge=.true.
C         write(*,*) "ITER=",iter
         return
       endif
       if(iter.ge.simplex_MAXIT) then
         write(*,"(I4,1x,A28)") myid,"SIMPLEX ITMAX exceeded amoba"
         return
       endif
       iter=iter+2
       ytry=amotry(p,y,ihi,-1.D0)
       if(ytry.le.y(ilo)) then
         ytry=amotry(p,y,ihi,2.D0)
       else if(ytry.ge.y(inhi)) then
         ysave=y(ihi)
         ytry=amotry(p,y,ihi,0.5D0)
         if(ytry.ge.ysave) then
           do i=1,ndim+1
             if(i.ne.ilo) then
               do j=1,ndim
                 x(j)=0.5D0*(p(i,j)+p(ilo,j))
                 p(i,j)=x(j)
               enddo
               call pes
               if(EVAP)then
                 y(i)=100000.D0
               else
                 y(i)=pot
               endif
C               write(*,*) "EREAL IN SIMPLEX=",EREAL,ndim
             endif
           enddo
           iter=iter+ndim
           goto 1
         endif
       else
         iter=iter-1
       endif
       goto 2
       end

       function amotry(p,y,ihi,fac)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/pes.h"
       integer ihi,j
       real*8 p(ndim_max+1,ndim_max),y(ndim_max+1),psum(ndim_max),fac
       real*8 fac1,fac2,ytry,ptry(ndim_max),amotry
       real*8 EREAL
       fac1=(1.D0-fac)/ndim
       fac2=fac1-fac
       do j=1,ndim
         x(j)=x(j)*fac1-p(ihi,j)*fac2
       enddo
       call pes
       if(EVAP)then
         ytry=100000.D0
       else
         ytry=pot
       endif
       if(ytry.lt.y(ihi)) then
         y(ihi)=ytry
         do j=1,ndim
           x(j)=x(j)-p(ihi,j)+ptry(j)
           p(ihi,j)=ptry(j)
         enddo
       endif
       amotry=ytry
       return
       end

* ======================GNU General Public License=======================
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*  
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
* =======================================================================
