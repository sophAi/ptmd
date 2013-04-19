       integer corr_observe_time_step,corr_total_time_step
       integer corr_method,corr_option
 
       common/corr_int0/corr_observe_time_step,corr_total_time_step,
     &corr_method,corr_option
 
C       real*8 xx(time_step_max),xy(time_step_max),
C     &xz(time_step_max)
C       real*8 vx(time_step_max),vy(time_step_max),
C     &vz(time_step_max) 
       real*8 diffusion_vcf
       real*8 diffusion_vsq(atom_num_max)
       real*8 corr_delta_time
       
C       common/corr_real0/xx,xy,xz,vx,vy,vz
       common/corr_real1/diffusion_vcf,diffusion_vsq
       common/corr_real2/corr_delta_time

       character*80 corr_cor_file_name,
     &corr_dtf_file_name
   
 
       common/corr_char0/ corr_cor_file_name,
     &corr_dtf_file_name

