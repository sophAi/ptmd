*========================================================================
* File Name : tester_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 17 Feb 2011 11:04:07 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine tester_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      if(tester_flag.eq."sliding_correlation")then
C        call sliding_file
C        call sliding_main
      endif
      if(tester_flag.eq."usr_score")then
        call usr_score_main
        call usr_score_output
      endif
      if(tester_flag.eq."mom_score")then
        call mom_score_main
        call mom_score_output
      endif
      if(tester_flag.eq."bond_vector")then
        call bond_vector_main
        call bond_vector_output
      endif
      if(tester_flag.eq."corr")then
        call corr_main
      endif  
      if(tester_flag.eq."fourier")then
C        call fourier_main
      endif
      if(tester_flag.eq."histogram")then
        call hist_main
C        call hist_output
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
