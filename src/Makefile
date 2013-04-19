PROG =	../bin_source/ptmd.out

#SRCS =


OBJS = ../tools/power2array.o ../tools/corr.o ../tools/top.o ../tools/nim.o ../tools/parameter.o ../tools/p_matrix.o ../tools/matrix_pool.o ../tools/moment.o ../tools/usr.o ../tools/anl.o ../tools/operators.o ../tools/correlation.o ../tools/sort.o ../tools/histogram.o ../tools/ft.o ../tools/centre.o ../tools/angular.o ../tools/time_label.o ../tools/read_unit.o ../tools/read_periodic.o ../tools/wscreen.o ../tools/debug.o ../tools/file.o ../tools/header.o ../tools/utilities.o ../tools/rnd.o ../tools/char.o main.o global_init.o startend.o parallel_shell.o job_assignment.o pes_assignment.o temp_assignment.o top_assignment.o source_assignment.o target_assignment.o header_assignment.o ndim_assignment.o loop_assignment.o simulation_init.o simulation_file.o reduced_unit.o ../config/xyz_file.o ../config/pdb_file.o ../config/check_file.o ../config/global_file.o ../config/locate_current_life.o ../config/locate_current_job_table.o ../config/locate_current_job.o ../config/locate_current_pes.o min_PES/min_pes.o min_PES/lbfgs_min.o min_PES/lbfgs.o min_PES/simplex_min.o PES/pes_restore.o PES/pes_gp_alloy.o PES/pes_gp_pure.o tester/tester_init.o tester/tester_file.o tester/tester_main.o tester/sliding_init.o tester/sliding_main.o tester/usr_score_init.o tester/usr_score_main.o tester/usr_score_output.o tester/mom_score_init.o tester/mom_score_main.o tester/mom_score_output.o tester/corr_init.o tester/corr_main.o tester/fourier_init.o tester/fourier_main.o tester/fourier_output.o tester/bond_vector_init.o tester/bond_vector_main.o tester/bond_vector_output.o tester/hist_init.o tester/hist_main.o tester/hist_output.o PTMC/PTMC_main.o PTMC/PTMC_init.o PTMC/PTMC.o PTMC/PTMC_thermal.o PTMC/PTMC_output.o BIMD/BIMD_main.o BIMD/BIMD_header.o BIMD/BIMD_init.o BIMD/BIMD.o BIMD/BIMD_thermal.o BIMD/BIMD_par.o BIMD/BIMD_gen.o BIMD/BIMD_program.o BIMD/BIMD_backup.o BIMD/BIMD_restore.o BIMD/BIMD_history.o BIMD/comvel.o BIMD/tdiff_opt.o BIMD/BIMD_output.o cn/cn_init.o cn/cn.o cn/cn_id.o cn/cn_hash.o step_move/step_move.o top/top_usr.o top/top_frz.o top/top_tcp.o top/top_acc.o

LIBS =
FC = mpif77
LDFLAGS= 
RUNCPP = 
#cpp -P -C

#Intel Fortran 32/64-bit SSE3===========
#FFLAGS=-O2 -axSSE3 -unroll-aggressive -ipo -no-prec-div -limf
#====================================
#intel Fortran 32/64-bit SSE2===========
#FFLAGS=-O2 -axSSE2 -unroll-aggressive -ipo -no-prec-div -limf 
#====================================
#gfortran============================
FFLAGS=-march=native -mfpmath='sse' -msse2 -ffast-math -funroll-loops -O3
#====================================
#g95=================================
#FFLAGS=-march=nocona -ffast-math -funroll-loops -O3
#====================================
#PGI Fortran safe for mpif77=========
#This is safe option,use this when debugging or writing new codes
#FFLAGS=-i4 -O3 -r8 -Mnoscalarsse -pc=80
#====================================
#PGI Fortran medium for mpif77=======
#This is fast option with sse,please use it when safe option is passed
# FFLAGS=-O2 -fastsse -Mipa=fast
#====================================
#PGI Fortran fast for mpif77=========
#Full power
#FFLAGS=-O3 -fastsse -Mipa=fast -Minfo -r8 -i4 -tp=amd64 -Mflushz -Munroll
#====================================
# g77 pentium 32bit==================
#FFLAGS=-O6 -ffixed-line-length-132 \
   -fno-f2c -fautomatic -ffast-math \
          -fstrength-reduce -fexpensive-optimizations -fcaller-saves \
          -funroll-loops -finline-functions -march=pentium
#====================================
# PGI Fortran with mpif90============
# Copied from some examples. Further test should be done. Please use it carefully.
# FFLAGS=-r8 -fast -mcmodel=medium -Mscalarsse -Mcache_align -Mflushz -w
#====================================
# g77 AMD64==========================
#  FFLAGS=-O6 -m64 -ffixed-line-length-132 \
   -fno-f2c -fautomatic -fforce-mem -ffast-math \
          -fstrength-reduce -fexpensive-optimizations -fcaller-saves \
          -funroll-loops -finline-functions
#====================================

all: $(PROG)

$(PROG): $(OBJS) $(EXTRAS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(EXTRAS) $(LIBS) 


clean:
	rm -f *.o*
	rm -f analysis/*.o*
	rm -f BH/*.o*
	rm -f BIMD/*.o*
	rm -f cn/*.o*
	rm -f top/*.o*
	rm -f GA/*.o*
	rm -f MAXENT/*.o*
	rm -f min_PES/*.o*
	rm -f PES/*.o*
	rm -f PTMC/*.o*
	rm -f step_move/*.o*
	rm -f tester/*.o*
	rm -f ../tools/*.o*
	rm -f ../config/*.o*
