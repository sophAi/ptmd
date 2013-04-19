*========================================================================
* File Name : help.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時11分32秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine help
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       write(*,*) "Atomic mass unit 1u=1.660538782*10^-27kg"
       write(*,*) "Also, 1u=931.4812D6eV/(c^2) and"
       write(*,*) "1u=(1/Na)kg where Na is the Avogadro's number"
       write(*,*) "Na=6.022*10^23 mol^-1 and R=Na*Kb=8.3144*(mol*K)^-1"
       write(*,*) "The masses in pes are all in atomic mass unit"
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
