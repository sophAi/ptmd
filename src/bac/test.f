*========================================================================
* File Name : test.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時59分25秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      program test
      integer int_test,int_test2
      character char_test
      open(22,file="test.dat",status="old")
      read(22,*)
      read(22,*) char_test,int_test,int_test2
      write(*,*) char_test,int_test,int_test2
      backspace(22)
      read(22,*) char_test,int_test,int_test2
      write(*,*) char_test,int_test,int_test2
      backspace(22)
      backspace(22)
      read(22,*) char_test,int_test,int_test2
      write(*,*) char_test,int_test,int_test2

      close(22)
      stop
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
