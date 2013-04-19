*========================================================================
* File Name : char.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時03分45秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       function char_length(file_name)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       char_length=index(file_name," ")
       return
       end
 
       subroutine int2char4(num,int2char)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer num,p_1,p_2,p_3,p_4
       p_1=num/1000
       p_2=num/100
       p_2=mod(p_2,10)
       p_3=num/10
       p_3=mod(p_3,10)
       p_4=mod(num,10)
       int2char=char(p_1+48)//char(p_2+48)//char(p_3+48)//char(P_4+48)
       return
       end      
 
       subroutine char42int(num,int2char) 
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h" 
       integer num,p_1,p_2,p_3,p_4
       p_1=ichar(int2char(1:1))-48
       p_2=ichar(int2char(2:2))-48
       p_3=ichar(int2char(3:3))-48
       p_4=ichar(int2char(4:4))-48
       num=p_1*1000+p_2*100+p_3*10+p_4
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
