*========================================================================
* File Name : edit_source.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:08:06 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine initial_source
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/top_sort.h"
      pes_type="none "
      pes_id=0
      pes_content="none "
      config_name="none "
      init_coord_file_name="none "
      use_top=0
      atom_num=1
      atom_name(1)="no"
      source_type="non"
      source_file_type="non"
      source_file_name="none "
      x(1)=0
      x(2)=0
      x(3)=0
      return
      end
     
      subroutine edit_source
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2,show_source_file_name*80
      sel="99"
      ensemble_num=dint((final_temp-init_temp)/delta_temp+1)
      do while(sel.ne."0")
        call write_source
        write(*,*) "======================================="
        write(*,*) "Current status:"
        write(*,*) "source type= ",source_type
        write(*,*) 
     &"File name beofre parallel id= ",source_file_name
     &(:index(source_file_name," ")-1)
        write(*,*) "Source file type= ",source_file_type
        if(source_file_flag.ne.0)then
          write(*,*) "Initial T(K)= ",init_temp
          write(*,*) "Final T(K)= ",final_temp
          write(*,*) "Delta T(k)= ",delta_temp
        endif 
        write(*,*) "======================================="
        call show_parameter(0,"quit ")
        call show_parameter(1,"source_file_flag ")
        write(*,*) " 2.Change the source type"
        write(*,*) " 3.Change the source file type"
        write(*,*) 
     &" 4.Change the header source file (default=source file)"
        read(*,*) sel
        if(sel.eq."1")then
          write(*,*) " 0.Manually input the source file"
          write(*,*) " 1.Read from BIMD"
          write(*,*) " 2.Read from PTMC"
          call edit_parameter("source_file_flag ")
          if(source_file_flag.eq.0)then
            write(*,*) "Please input the file name of the source file"
            write(*,*) "Either full path or in the current directory"
            read(*,*) source_file_name
            write(*,*) "Please input the output label(0,1,2,3,4..)"
            read(*,*)init_temp
            final_temp=init_temp
            delta_temp=1.D0
            ensemble_num=1
          else
            call edit_temp
          endif
        else if(sel.eq."2")then
          call edit_source_type
        else if(sel.eq."3")then
          call edit_source_file_type
        else if(sel.eq."4")then
          call edit_header
        endif
        
        if((source_file_flag.eq.1.or.source_file_flag.eq.2)
     &.and.pes_content.eq."none ")then
          write(*,*) "Please choose a pes first"
          call pes_cycle
          call write_source
          write(*,*) "Data type= ",source_type
          write(*,*) "Source file= ",source_file_name
        endif
      enddo
      init_coord_file_name=source_file_name
      call write_header
      return
      end

      subroutine edit_source_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2
99    write(*,*) "Please select the source type:"
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
        source_type="non"
      else if(sel.eq."1")then
        source_type="dat"
      else if(sel.eq."2")then 
        source_type="rec"
      else if(sel.eq."3")then
        source_type="xyz"
      else if(sel.eq."4")then
        source_type="pdb"
      else if(sel.eq."5")then
        source_type="ufe"
      else if(sel.eq."6")then
        source_type="ufx"
      else if(sel.eq."7")then
        source_type="ufv"
      else if(sel.eq."8")then
        source_type="ufg"
      else if(sel.eq."9")then
        source_type="his"
      else if(sel.eq."10")then
        source_type="mom"
      else if(sel.eq."11")then
        source_type="scr"
      else if(sel.eq."12")then
        source_type="vaf"
      else if(sel.eq."13")then
        source_type="psd"
      else if(sel.eq."14")then
        source_type="dtf"
      else if(sel.eq."15")then
        source_type="cnl"
      else if(sel.eq."16")then
        source_type="anl"
      else if(sel.eq."17")then
        source_type="vfx"
      else if(sel.eq."18")then
        source_type="min"
      else
        write(*,*) "Error!Please select again:"
        goto 99
      endif
      write(*,*) "The source type you choose is ",source_type
      return
      end

      subroutine edit_source_file_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      character sel*2
99    write(*,*) "Please select the source file type:" 
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
        source_file_type="non"
      else if(sel.eq."1")then
        source_file_type="dat"
      else if(sel.eq."2")then
        source_file_type="rec"
      else if(sel.eq."3")then
        source_file_type="xyz"
      else if(sel.eq."4")then
        source_file_type="pdb"
      else if(sel.eq."5")then
        source_file_type="ufe"
      else if(sel.eq."6")then
        source_file_type="ufx"
      else if(sel.eq."7")then
        source_file_type="ufv"
      else if(sel.eq."8")then
        source_file_type="ufg"
      else if(sel.eq."9")then
        source_file_type="his"
      else if(sel.eq."10")then
        source_file_type="mom"
      else if(sel.eq."11")then
        source_file_type="scr"
      else if(sel.eq."12")then
        source_file_type="vaf"
      else if(sel.eq."13")then
        source_file_type="psd"
      else if(sel.eq."14")then
        source_file_type="dtf"
      else if(sel.eq."15")then
        source_file_type="cnl"
      else if(sel.eq."16")then
        source_file_type="anl"
      else
        write(*,*) "Error!Please select again:"
        goto 99
      endif
      write(*,*) "The source file type you choose is ",source_file_type
      return
      end


      subroutine write_source
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      if(source_file_flag.eq.1)then
        source_file_name="BIMD_"//source_type//"_"//
     &pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//"_"
      endif
      if(source_file_flag.eq.2)then
        source_file_name="PTMC_"//source_type//"_"//
     &pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//"_"
      endif
      if(source_file_flag.eq.0)then
        file_name=
     &source_file_name(:index(source_file_name," ")-1)
        call check_file_name(file_name,file_main,file_ext)
        source_file_type=file_ext(:index(file_ext," ")-1)
        source_type=source_file_type
      endif
      init_coord_file_name=source_file_name
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
