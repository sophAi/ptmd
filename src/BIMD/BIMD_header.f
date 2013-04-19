*========================================================================
* File Name : BIMD_header.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Fri 18 Feb 2011 04:58:15 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================

      subroutine BIMD_unformatted_header(rec_file_type,x_dim,y_dim)  !Write the information in the first line of unformatted file.
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/simulation.h"
      integer x_dim,y_dim
      character rec_file_type*3
      header_source_type=rec_file_type
      header_par_num=11
      header_par_name(1)="pes_id"
      header_par_real(1)=dble(pes_id)
      header_par_name(2)="time_label"
      header_par_real(2)=time_label
      header_par_name(3)="file_x_dim"
      header_par_real(3)=dble(x_dim)    !=total_frame_num
      header_par_name(4)="file_y_dim"
      header_par_real(4)=dble(y_dim)
      header_par_name(5)="init_loop"
      header_par_real(5)=simulation_init_loop
      header_par_name(6)="final_loop"
      header_par_real(6)=simulation_final_loop
      header_par_name(7)="simulation_delta_time"
      header_par_real(7)=simulation_delta_time
      header_par_name(8)="simulation_reset_thermal"
      header_par_real(8)=dble(simulation_reset_thermal)
      header_par_name(9)="simulation_min_method"
      header_par_real(9)=dble(simulation_min_method)
      if(rec_file_type.eq."ufe")then
        header_par_name(10)="simulation_ufe_loop"
        header_par_real(10)=simulation_ufe_loop
        header_par_name(11)="ndim_fac"
        header_par_real(11)=1.D0
        header_annotation="'pot/N pot(i=1~n) kinetic/N (pot+kinetic)/N'"
      else if(rec_file_type.eq."ufx")then
        header_par_name(10)="simulation_ufx_loop"
        header_par_real(10)=simulation_ufx_loop
        header_par_name(11)="ndim_fac"
        header_par_real(11)=dble(ndim_fac)
        header_annotation="'x(i=1~3n)'"
      else if(rec_file_type.eq."ufv")then
        header_par_name(10)="simulation_ufv_loop"
        header_par_real(10)=simulation_ufv_loop
        header_par_name(11)="ndim_fac"
        header_par_real(11)=dble(ndim_fac)
        header_annotation="'v(i=1~3n)'"
      else if(rec_file_type.eq."ufg")then
        header_par_name(10)="simulation_ufg_loop"
        header_par_real(10)=simulation_ufg_loop
        header_par_name(11)="ndim_fac"
        header_par_real(11)=dble(ndim_fac)
        header_annotation="'g(i=1~3n)'"
      else if(rec_file_type.eq."vfx")then
        header_par_name(10)="simulation_vfx_loop"
        header_par_real(10)=simulation_vfx_loop
        header_par_name(11)="ndim_fac"
        header_par_real(11)=dble(ndim_fac)
        header_annotation="'v_fixed(i=1~3n)'"
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
