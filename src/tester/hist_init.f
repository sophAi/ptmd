*========================================================================
* File Name : hist_init.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月24日 (週日) 16時22分00秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine hist_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/parallel.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist.h"
      hist_min=hist_min_lower+(dabs(hist_min_upper-hist_min_lower)/
     &dble(ensemble_num))*dble(id-1)
      hist_max=hist_max_lower+(dabs(hist_max_upper-hist_max_lower)/
     &dble(ensemble_num))*dble(id-1)
      hist_interval=hist_interval_lower+(dabs(hist_interval_upper-
     &hist_interval_lower)/dble(ensemble_num))*dble(id-1)
      if(wscreen)then
        write(*,"(I5,1x,A20)") myid,"Generating Histogram"
        write(*,"(I5,1x,A10,F10.1,1x,A11,1x,F10.1,1x,A11,1x,F10.1)")
     &myid,"Init_loop=",init_loop,",final_loop=",
     &final_loop,",delta_loop=",delta_loop
        write(*,"(I5,1x,A16,1x,F17.6,1x,A17,1x,F17.6,1x,A22,1x,F19.9)")
     &myid,"Minimal boundry=",hist_min,",maximal boundry=",
     &hist_max,",histogram resolution=",hist_interval
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
