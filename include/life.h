       integer total_life_num,life_num,loop_num,id
       integer life_job(ensemble_max),life_job_id(ensemble_max),
     &life_pes_id(ensemble_max),life_ensemble_id(ensemble_max),
     &life_loop_id(ensemble_max),life_id(ensemble_max)
     
       common/life0/ total_life_num,life_num,loop_num,id
       common/life2/life_job,life_job_id,life_pes_id,
     &life_ensemble_id,life_loop_id,life_id

       logical stop_life
       common/life3/stop_life
