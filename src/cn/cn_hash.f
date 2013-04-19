*========================================================================
* File Name : cn_hash.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時50分35秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine cn_hash_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"   
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      call int2char4(atom_num,int2char)
      file_name=cn_network_path(:index(cn_network_path," ")-1)
     &//int2char//".cn"
      inquire(file=file_name(:index(file_name," ")-1),exist=file_exist)
      if(file_exist)then
        open(21,file=file_name,status="old")
        read(21,*)cn_hash_total_pair_num,cn_hash_total_num
        do I0=1,cn_hash_total_pair_num
          read(21,*)cn_hash,cn_hash_weight(cn_hash),
     &cn_hash_num(cn_hash),cn_hash_name(cn_hash)
        enddo
        close(21)
      else
        cn_hash_total_num=0
        cn_hash_total_pair_num=0
        do I0=1,hash_max
          cn_hash_exist(I0)=.false.
          cn_hash_num(I0)=0
          cn_hash_weight(I0)=0.D0
          cn_hash_name(I0)="null"
        enddo
      endif
      return
      end

      subroutine cn_learning
      implicit none 
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      call cn_hash_id
      if(cn_hash_exist(cn_hash))then
        cn_hash_total_num=cn_hash_total_num+1
        cn_hash_num(cn_hash)=cn_hash_num(cn_hash)+1
        cn_hash_weight(cn_hash)=dble(cn_hash_num(cn_hash))/
     &dble(cn_hash_total_num)
      else
        cn_hash_exist(cn_hash)=.true.
        cn_hash_total_pair_num=cn_hash_total_pair_num+1
        cn_hash_total_num=cn_hash_total_num+1
        cn_hash_num(cn_hash)=cn_hash_num(cn_hash)+1
        cn_hash_weight(cn_hash)=dble(cn_hash_num(cn_hash))/
     &dble(cn_hash_total_num)
        cn_hash_name(cn_hash)=cn_name
      endif
      return
      end

      subroutine cn_hash_id
      implicit none
      include "../../include/global_common.h"   
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer total_pairs,total_3_pairs,hash_num,total_char
      character hash_name*4
      hash_name=cn_name(1:4)
      call char42int(total_pairs,hash_name)
      total_char=8*total_pairs+4
      J1=3
      cn_hash=0
      do J0=1,total_pairs*2
        hash_name=cn_name(J1:J1+3)
        call char42int(hash_num,hash_name)
        cn_hash=cn_hash+hash_num
C        write(*,*) hash_cn,hash_num
        J1=J1+4
      enddo
      hash_name="00"//cn_name(J1:total_char)
      call char42int(hash_num,hash_name)
      cn_hash=cn_hash+hash_num
      write(*,*) cn_hash,cn_name
      cn_hash=mod(cn_hash,hash_prime)
      write(*,*) "h#=",cn_hash,hash_prime
      return
      end

      subroutine cn_hash_memorize
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
     
      return
      end

      subroutine hash_check_collision
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"

      return
      end

      function hash_division(num,prime_num)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer num,prime_num,hash_division
      hash_division=mod(num,prime_num)     
      return
      end
 
      function hash_midsquare(num)      !Not transfering to bit representation
      implicit none
      include "../../include/global_common.h" 
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer num,hash_midsquare
      hash_midsquare=num**2
      write(*,*) hash_midsquare
      return
      end

      function hash_folding(num)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"   
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      integer num,hash_folding

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
