      integer total_moment_x_num,total_moment_z_num,
     &total_moment_y_num

      common/moment_int0/total_moment_x_num,
     &total_moment_z_num,total_moment_y_num
 
      real*8 mom(moment_z_max),ave,var,adev,sdev,skew,curt
 
      common/moment_real0/ mom,ave,var,adev,sdev,skew,curt

      real*8 ave_n(moment_y_max),var_n(moment_y_max),
     &adev_n(moment_y_max),sdev_n(moment_y_max),
     &skew_n(moment_y_max),curt_n(moment_y_max)

      common/moment_real1/ave_n,var_n,adev_n,sdev_n,skew_n,
     &curt_n
