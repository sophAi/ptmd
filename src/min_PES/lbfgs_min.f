*========================================================================
* File Name : lbfgs_min.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 27 Oct 2010 04:32:57 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine lbfgs_init
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       lbfgs_XTOL=2.2204460492503D-16
C       lbfgs_MP=6
C       lbfgs_LP=6
C       lbfgs_STPMIN=1.D-20
C       lbfgs_STPMAX=1.D+20 
C       lbfgs_MUPDATE=4
C       lbfgs_DGUESS=0.1D0
C       lbfgs_GMAX=0.001D0
C       lbfgs_EPS=0.0001D0    !for 1.D-6 accuracy
       lbfgs_EPS=0.00001D0   !for 1.D-8 accuracy
C============lbfgs manually input=================
C       lbfgs_GTOL=0.9D0
C       lbfgs_MAXIT=500    
C=================================================
       return
       end



*       Conjugate Gradient Driver
*
       subroutine lbfgs_min
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       integer IFLAG,NMUPMAX
       integer IPRINT(2),evap_iter
       parameter(NMUPMAX=4)
       real*8 WORK(ndim_max*(2*NMUPMAX+1)+2*NMUPMAX)
       real*8 DIAG(ndim_max),last_pot(500),last_rms(500),last_IFLAG(500)
       real*8 pot_bac,x_bac(ndim_max),grad_bac(ndim_max),
     &pot_per_atom_bac(ndim_max),RMS_bac,rcut_unit_bac
       character dummy*2
       IFLAG=0
       evap=.false.
       IPRINT(1)=-1
       IPRINT(2)=0 
       call pes
       pot_bac=pot
       RMS_bac=RMS
       evap_iter=0
       do J1=1,ndim
         x_bac(J1)=x(J1)
         grad_bac(J1)=grad(J1)
C         write(*,*) J1,x_bac(J1),grad_bac(J1),pot_bac 
       enddo
       do J1=1,atom_num
         pot_per_atom_bac(J1)=pot_per_atom(J1)
       enddo
       do I0=1,lbfgs_MAXIT
101      last_pot(I0)=pot
         last_rms(I0)=rms
         last_IFLAG(I0)=IFLAG
         call lbfgs(ndim,lbfgs_MUPDATE,x,pot,grad,
     &.FALSE.,DIAG,IPRINT,lbfgs_EPS,lbfgs_XTOL,WORK,IFLAG,lbfgs_DGUESS)
         call pes
C         write(*,*) I0,pot,IFLAG
C         call check_value(pot)
C         if(evap.or.pot.eq."NaN".or.dabs(pot).eq."INF")then
C           if (wscreen)then
C             write(*,"I5,1x,A22,1x,F20.8,1x,
C     &F20.8,1x,F20.8,1x,I5,1x,F20.8") 
C     &myid,"lbfgs false,evap! Pot=",pot,RMS,pot_bac,evap_iter,
C     &global_pot
C             do J3=1,I0
C               write(*,*) "Step=",J3,",pot=",last_pot(J3)
C             enddo
C           endif
C           pause "NAN print"
C           lbfgs_converge=.false.
C           evap=.false.
C           pot=pot_bac
C           RMS=RMS_bac
C           do J1=1,NDIM
C             x(J1)=x_bac(J1)
C             grad(J1)=grad_bac(J1)
C           enddo
C           do J1=1,atom_num
C             pot_per_atom_bac(J1)=pot_per_atom(J1)
C           enddo
C           write(*,*) "before reset,pot=",pot,rms,IFLAG
C           call pes
C           write(*,*) "Check pot again=",pot,rms,IFLAG
C           call lbfgs(ndim,lbfgs_MUPDATE,x,pot,grad,
C     &.FALSE.,DIAG,IPRINT,lbfgs_EPS,lbfgs_XTOL,WORK,IFLAG,lbfgs_DGUESS)
C           call pes
C           write(*,*) "After reset,pot=",pot,rms,IFLAG
C           pause
C           evap_iter=evap_iter+1
C           if(evap_iter.ge.lbfgs_MAXIT)return
C           goto 101
C-================================
C         endif 
         if (IFLAG.eq.0) then
           lbfgs_converge=.TRUE.
C           if(wscreen)write(*,"I5,1x,I5,1x,A14,1x,F20.8,1x,
C     &F20.8,1x,F20.8")
C     &myid,I0,",lbfgs OK, Pot=",pot,RMS,global_pot
C            pause
            return
         else if (IFLAG.lt.0) then
           lbfgs_converge=.FALSE.
C           if(wscreen)write(*,"I5,1x,I5,1x,A18,1x,F20.8,1x,
C     &F20.8,1x,F20.8") 
C     &myid,I0,",lbfgs false, Pot=",pot,RMS,global_pot
C           pause 
           return
         endif
C         call flush(6)
       enddo
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
