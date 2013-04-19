*========================================================================
* File Name : edit_header.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:07:31 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_header
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2,show_header_file_name*80
      sel="99"
      ensemble_num=dint((final_temp-init_temp)/delta_temp+1)
      do while(sel.ne."0")
        call write_header
        write(*,*) "======================================="
        write(*,*) "Current status:"
        write(*,*) "header source type= ",header_file_source_type
        write(*,*) 
     &"File name beofre parallel id= ",header_file_name
     &(:index(header_file_name," ")-1)
        write(*,*) "Source file type= ",header_file_type
        if(header_file_flag.ne.0)then
          write(*,*) "Initial T(K)= ",init_temp
          write(*,*) "Final T(K)= ",final_temp
          write(*,*) "Delta T(k)= ",delta_temp
        endif 
        write(*,*) "======================================="
        call show_parameter(0,"quit ")
        call show_parameter(1,"header_file_flag ")
        write(*,*) " 2.Change the header type"
        write(*,*) " 3.Change the header file type"
        read(*,*) sel
        if(sel.eq."1")then
          write(*,*) " 0.Manually input the header file"
          write(*,*) " 1.Read from BIMD"
          write(*,*) " 2.Read from PTMC"
          write(*,*) " 3.The same as the source file"
          call edit_parameter("header_file_flag ")
          if(header_file_flag.eq.0)then
            write(*,*) "Please input the file name of the header file"
            write(*,*) "Either full path or in the current directory"
            read(*,*) header_file_name
C            write(*,*) "Please input the output label(0,1,2,3,4..)"
C            read(*,*)init_temp
C            final_temp=init_temp
C            delta_temp=1.D0
C            ensemble_num=1
          else if(header_file_flag.eq.1.or.header_file_flag.eq.2)then
            call edit_temp
          else
            header_file_name=
     &source_file_name(:index(source_file_name," ")-1)
          endif
        else if(sel.eq."2")then
          call edit_header_file_source_type
        else if(sel.eq."3")then
          call edit_header_file_type
        endif
        
        if((header_file_flag.eq.1.or.header_file_flag.eq.2)
     &.and.pes_content.eq."none ")then
          write(*,*) "Please choose a pes first"
          call pes_cycle
          call write_header
          write(*,*) "Data type= ",header_file_source_type
          write(*,*) "Source file= ",header_file_name
        endif
      enddo
      return
      end

      subroutine edit_header_file_source_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2
99    write(*,*) "Please select the header type:"
      write(*,*) " 0. none"
      write(*,*) " 1. _dat_"
      write(*,*) " 2. _rec_"
      write(*,*) " 3. _xyz_"
      write(*,*) " 4. _pdb_"
      write(*,*) " 5. _ufe_"
      write(*,*) " 6. _ufx_"
      write(*,*) " 7. _ufv_"
      write(*,*) " 8. _ufg_"
      write(*,*) " 9. _his_"
      write(*,*) "10. _mom_"
      write(*,*) "11. _scr_"
      write(*,*) "12. _vaf_"
      write(*,*) "13. _psd_"
      write(*,*) "14. _dtf_"
      write(*,*) "15. _cnl_"
      write(*,*) "16. _anl_"
      write(*,*) "17. _vfx_"
      write(*,*) "18. _min_"
      write(*,*)"===================="
      read(*,*)sel
      if(sel.eq."0")then
        header_file_source_type="non"
      else if(sel.eq."1")then
        header_file_source_type="dat"
      else if(sel.eq."2")then 
        header_file_source_type="rec"
      else if(sel.eq."3")then
        header_file_source_type="xyz"
      else if(sel.eq."4")then
        header_file_source_type="pdb"
      else if(sel.eq."5")then
        header_file_source_type="ufe"
      else if(sel.eq."6")then
        header_file_source_type="ufx"
      else if(sel.eq."7")then
        header_file_source_type="ufv"
      else if(sel.eq."8")then
        header_file_source_type="ufg"
      else if(sel.eq."9")then
        header_file_source_type="his"
      else if(sel.eq."10")then
        header_file_source_type="mom"
      else if(sel.eq."11")then
        header_file_source_type="scr"
      else if(sel.eq."12")then
        header_file_source_type="vaf"
      else if(sel.eq."13")then
        header_file_source_type="psd"
      else if(sel.eq."14")then
        header_file_source_type="dtf"
      else if(sel.eq."15")then
        header_file_source_type="cnl"
      else if(sel.eq."16")then
        header_file_source_type="anl"
      else if(sel.eq."17")then
        header_file_source_type="vfx"
      else if(sel.eq."18")then
        header_file_source_type="min"
      else
        write(*,*) "Error!Please select again:"
        goto 99
      endif
      write(*,*) 
     &"The header type you choose is ",header_file_source_type
      return
      end

      subroutine edit_header_file_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2
99    write(*,*) "Please select the header file type:" 
      write(*,*) " 0. none"
      write(*,*) " 1. *.dat"
      write(*,*) " 2. *.rec"
      write(*,*) " 3. *.xyz"
      write(*,*) " 4. *.pdb"
      write(*,*) " 5. *.ufe"
      write(*,*) " 6. *.ufx"
      write(*,*) " 7. *.ufv"
      write(*,*) " 8. *.ufg"
      write(*,*) " 9. *.his"
      write(*,*) "10. *.mom"
      write(*,*) "11. *.scr"
      write(*,*) "12. *.vaf"
      write(*,*) "13. *.psd"
      write(*,*) "14. *.dtf"
      write(*,*) "15. *.cnl"
      write(*,*) "16. *.anl"
      write(*,*)"===================="
      read(*,*)sel
      if(sel.eq."0")then
        header_file_type="non"
      else if(sel.eq."1")then
        header_file_type="dat"
      else if(sel.eq."2")then
        header_file_type="rec"
      else if(sel.eq."3")then
        header_file_type="xyz"
      else if(sel.eq."4")then
        header_file_type="pdb"
      else if(sel.eq."5")then
        header_file_type="ufe"
      else if(sel.eq."6")then
        header_file_type="ufx"
      else if(sel.eq."7")then
        header_file_type="ufv"
      else if(sel.eq."8")then
        header_file_type="ufg"
      else if(sel.eq."9")then
        header_file_type="his"
      else if(sel.eq."10")then
        header_file_type="mom"
      else if(sel.eq."11")then
        header_file_type="scr"
      else if(sel.eq."12")then
        header_file_type="vaf"
      else if(sel.eq."13")then
        header_file_type="psd"
      else if(sel.eq."14")then
        header_file_type="dtf"
      else if(sel.eq."15")then
        header_file_type="cnl"
      else if(sel.eq."16")then
        header_file_type="anl"
      else
        write(*,*) "Error!Please select again:"
        goto 99
      endif
      write(*,*) "The header file type you choose is ",header_file_type
      return
      end


      subroutine write_header
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      if(header_file_flag.eq.1)then
        header_file_name="BIMD_"//header_file_source_type//"_"//
     &pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//"_"
      endif
      if(header_file_flag.eq.2)then
        header_file_name="PTMC_"//header_file_source_type//"_"//
     &pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//"_"
      endif
      if(header_file_flag.eq.0)then
        file_name=
     &header_file_name(:index(header_file_name," ")-1)
        call check_file_name(file_name,file_main,file_ext)
        header_file_type=file_ext(:index(file_ext," ")-1)
        header_file_source_type=header_file_type
      endif
      if(header_file_flag.eq.3)then
        header_file_name=
     &source_file_name(:index(source_file_name," ")-1)
        file_name=
     &source_file_name(:index(source_file_name," ")-1)
        call check_file_name(file_name,file_main,file_ext)
        header_file_source_type=file_ext(:index(file_ext," ")-1)
        header_file_type=source_file_type
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
