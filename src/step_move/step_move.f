*========================================================================
* File Name : step_move.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時57分10秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine step_move
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/step_move/step_move.h"
      prob=rnd() 
      if(prob.le.prob_MC_move)then     !MC move
        call operator_MC_move
      else if(prob.le.prob_BH_move)then   !BH_move
        call operator_BH_move
      else if(prob.le.prob_permutation_lmin)then  !permutation_lmin
        call operator_permutation_lmin
      else if(prob.le.prob_permutation)then      !permutation
        call operator_permutation
      else if(prob.le.prob_inversion)then      !inversion
        call operator_inversion
      else if(prob.le.prob_arithmetic)then       !arithmetic
        call operator_arithmetic
      else if(prob.le.prob_geometic)then         !geometic
        call operator_geometic
      else if(prob.le.prob_crossing)then         !crossing
        call operator_crossing
      else if(prob.le.prob_twopoint)then         !twopoint
        call operator_twopoint
      endif
      return
      end

      subroutine step_move_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/step_move/step_move.h"
      accept_steps=0
      reject_steps=0
      total_steps=0
      global_accept_steps=0
      global_reject_steps=0
      global_total_steps=0
C===========prob_MC_move===========================
      prob_MC_move=mva/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C===========prob_BH_move===========================
      prob_BH_move=prob_MC_move+
     &mvb/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C===========prob_permutation_lmin==================
      prob_permutation_lmin=prob_BH_move+
     &mvc/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C===========prob_permutation=======================
      prob_permutation=prob_permutation_lmin+
     &mvd/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_inversion==========================
      prob_inversion=prob_permutation+
     &mve/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_arithmetic=========================
      prob_arithmetic=prob_inversion+
     &mvf/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_geometic===========================
      prob_geometic=prob_arithmetic+
     &mvg/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_crossing===========================
      prob_crossing=prob_geometic+
     &mvh/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_twopoint===========================
      prob_twopoint=prob_crossing+
     &mvi/(mva+mvb+mvc+mvd+mve+mvf+mvg+mvh+mvi+mvj)
C==========prob_temp==============================
C      prob_temp=prob_twopoint+
c     &mvj/(mva+mvb+mvc+mvd+mve+mvf+mvu+mvh+mvi+mvj)
      if(wscreen)then
        write(*,"(I5,1x,A8,1x,F4.1,1x,A8,1x,F4.1,1x,A17,1x,F4.1
     &,1x,A12,1x,F4.1,1x,A10,1x,F4.1,1x,A11,1x,F4.1,1x,A9,1x,
     &F4.1,1x,A9,1x,F4.1,1x,A10,1x,F4.1)") 
     &myid,
     &"MC_move=",prob_MC_move,
     &"BH_move=",prob_BH_move-prob_MC_move,
     &"Permutation_lmin=",prob_permutation_lmin-prob_BH_move,
     &"Permutation=",prob_permutation-prob_permutation_lmin,
     &"Inversion=",prob_inversion-prob_permutation,
     &"Arithmetic=",prob_arithmetic-prob_inversion,
     &"Geometic=",prob_geometic-prob_arithmetic,
     &"Crossing=",prob_crossing-prob_geometic,
     &"Two-point=",prob_twopoint-prob_crossing
      endif
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
