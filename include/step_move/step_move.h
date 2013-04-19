      integer accept_steps,reject_steps,total_steps
      integer global_accept_steps,global_reject_steps,
     &global_total_steps
      common /step_move_int0/accept_steps,reject_steps,total_steps,
     &global_accept_steps,global_reject_steps,global_total_steps

      real*8 mva,mvb,mvc,mvd,mve,mvf,mvg,mvh,mvi,mvj
      real*8 prob,prob_MC_move,prob_BH_move,prob_permutation_lmin,
     &prob_permutation,prob_inversion,prob_arithmetic,prob_geometic,
     &prob_crossing,prob_twopoint


      common /step_move_ratio_real0/mva,mvb,mvc,mvd,mve,
     &mvf,mvg,mvh,mvi,mvj
      common /step_move_ratio_real1/prob_MC_move,prob_BH_move,
     &prob_permutation_lmin,prob_permutation,prob_inversion,
     &prob_arithmetic,prob_geometic,prob_crossing,prob_twopoint


