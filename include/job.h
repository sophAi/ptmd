      integer job_num,total_job_num,job_id,job_line_num
     &,job_table_line_num,job_status(ensemble_max),
     &job_node(ensemble_max),unfinished_job_num

      integer job_point_id     !for mpi-bridge, job code name of all jobs in one life
      
      common/job0/ job_num,total_job_num,job_table_line_num,
     &job_line_num,job_point_id
      common/job1/ job_id,job_status,job_node,unfinished_job_num

      character job_type*15,job_content*50
 
      common/job2/job_type,job_content

      logical job_skip
       
      common/job_logical/job_skip
