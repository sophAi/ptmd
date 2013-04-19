C========old setting for pot and lmin===================
      integer reflash_hist_lmin(hist_max_lmin),
     &reflash_hist_pot(hist_max_pot),hist_pot(hist_max_pot),
     &hist_lmin(hist_max_lmin),hist_total_lmin,hist_total_pot
      common /histogram_pot_int0/ reflash_hist_lmin,
     &reflash_hist_pot,hist_pot,hist_lmin,hist_total_lmin,
     &hist_total_pot
 
      real*8 pot_hist(hist_max_pot)
      common /histogram_pot_real0/ pot_hist

      real*8 hist_xyz_lmin(hist_max_lmin,300)
      common /histogram_pot_real1/ hist_xyz_lmin
C==========new setting for histogram core==============
      integer hist_bin_id(hist_dim_max),hist_max_bin_id,
     &hist_min_bin_id,hist_method,hist_dim
      common /histogram_int0/ hist_bin_id,hist_max_bin_id,
     &hist_min_bin_id,hist_method,hist_dim
      real*8 hist_total_num(hist_dim_max),
     &hist_missing_num(hist_dim_max),hist_read_value(hist_dim_max)
      common /histogram_real0/ hist_total_num,hist_missing_num,
     &hist_read_value
      real*8 hist_x(hist_bin_max),hist_y(hist_dim_max,hist_bin_max),
     &hist_y_nor(hist_dim_max,hist_bin_max)
      common /histogram_real1/ hist_x,hist_y,hist_y_nor
      real*8 hist_min,hist_max,hist_interval
      real*8 hist_min_lower,hist_min_upper,hist_min_delta,
     &hist_max_lower,hist_max_upper,hist_max_delta,
     &hist_interval_lower,hist_interval_upper,hist_interval_delta
      common /histogram_real1/ hist_min,hist_max,hist_interval
      common /histogram_real2/
     &hist_min_lower,hist_min_upper,hist_min_delta,
     &hist_max_lower,hist_max_upper,hist_max_delta,
     &hist_interval_lower,hist_interval_upper,hist_interval_delta

