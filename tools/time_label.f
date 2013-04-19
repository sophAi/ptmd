*========================================================================
* File Name : time_label.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時07分56秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine check_time_label
C     date.time ==> format F13.6 for time_label
C     date_time ==> format A13 for time_label_char
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      integer today(3),now(3)
      call idate(today)
      call itime(now)
      time_label=dble(today(3))*1.D4+dble(today(2))*1.D2+dble(today(1))+
     &dble(now(1))*1.D-2+dble(now(2))*1.D-4+dble(now(3))*1.D-6
      return
      end

      subroutine time_label2char
      implicit none
      include "mpif.h"
      include "../include/global_common.h"
      include "../include/common.h" 
      integer year(4),month(2),day(2),hour(2),minute(2),second(2),
     &time_dec
      year(1)=dint(time_label/10000000.D0)
      year(2)=dint(time_label/1000000.D0)-year(1)*10
      year(3)=dint(time_label/100000.D0)-year(1)*100-year(2)*10
      year(4)=dint(time_label/10000.D0)-year(1)*1000-
     &year(2)*100-year(3)*10
      month(1)=dint(time_label/1000.D0)-year(1)*10000-year(2)*1000
     &-year(3)*100-year(4)*10
      month(2)=dint(time_label/100.D0)-year(1)*100000-year(2)*10000-
     &year(3)*1000-year(4)*100-month(1)*10
      day(1)=dint(time_label/10.D0)-year(1)*1000000-year(2)*100000-
     &year(3)*10000-year(4)*1000-month(1)*100-month(2)*10
      day(2)=dint(time_label)-year(1)*10000000-year(2)*1000000-
     &year(3)*100000-year(4)*10000-month(1)*1000-month(2)*100-day(1)*10
      time_dec=dint((time_label-dble(dint(time_label)))*1000000.D0)
      hour(1)=dint(time_dec/100000.D0)
      hour(2)=dint(time_dec/10000.D0)-hour(1)*10
      minute(1)=dint(time_dec/1000.D0)-hour(1)*100-hour(2)*10
      minute(2)=dint(time_dec/100.D0)-hour(1)*1000-hour(2)*100-
     &minute(1)*10
      second(1)=dint(time_dec/10.D0)-hour(1)*10000-hour(2)*1000-
     &minute(1)*100-minute(2)*10
      second(2)=time_dec-hour(1)*100000-hour(2)*10000-
     &minute(1)*1000-minute(2)*100-second(1)*10
      time_label_char=
     &char(48+year(1))//char(48+year(2))
     &//char(48+year(3))//char(48+year(4))
     &//char(48+month(1))//char(48+month(2))
     &//char(48+day(1))//char(48+day(2))
     &//"_"
     &//char(48+hour(1))//char(48+hour(2))
     &//char(48+minute(1))//char(48+minute(2))
     &//char(48+second(1))//char(48+second(2))
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
