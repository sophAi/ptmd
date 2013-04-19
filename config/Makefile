PROG =	../bin_source/config.out

#SRCS=

OBJS =../tools/read_periodic.o ../tools/read_unit.o ../tools/parameter.o ../tools/usr.o ../tools/moment.o ../tools/histogram.o ../tools/rnd.o ../tools/centre.o ../tools/wscreen.o ../tools/utilities.o ../tools/char.o ../tools/file.o ../tools/header.o main.o check_file.o global_life.o global_file.o global_job.o global_pes.o edit_loop.o edit_job.o edit_pes.o edit_simulation.o edit_source.o edit_target.o edit_header.o edit_temp.o edit_step_move.o edit_top.o top/edit_top_usr.o top/edit_top_frz.o locate_current_life.o locate_current_job_table.o locate_current_pes.o xyz_file.o pdb_file.o tester/edit_tester.o tester/edit_sliding.o tester/edit_usr_output.o tester/edit_mom_score.o tester/edit_usr_score.o tester/edit_convert_uf.o tester/edit_hist.o tester/edit_bond_vector.o tester/edit_read_headers.o BIMD/edit_BIMD.o tester/edit_corr.o tester/edit_fourier.o PTMC/edit_PTMC.o  ../src/BIMD/BIMD_thermal.o ../src/BIMD/BIMD_restore.o ../src/source_assignment.o ../src/target_assignment.o ../src/header_assignment.o ../src/temp_assignment.o ../src/global_init.o ../src/pes_assignment.o ../src/top_assignment.o ../src/top/top_usr.o ../src/top/top_frz.o ../src/top/top_tcp.o ../src/top/top_acc.o ../src/reduced_unit.o ../src/PES/pes_gp_alloy.o ../src/PES/pes_gp_pure.o ../src/PES/pes_restore.o ../src/min_PES/lbfgs_min.o ../src/min_PES/lbfgs.o ../src/min_PES/min_pes.o ../src/min_PES/simplex_min.f ../src/cn/cn_init.o ../src/cn/cn.o ../src/cn/cn_id.o 

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
#    -fno-f2c -fautomatic -ffast-math \
#              -fstrength-reduce -fexpensive-optimizations -fcaller-saves \
#          -funroll-loops -finline-functions -march=pentium
#====================================
# PGI Fortran with mpif90============
# Copied from some examples. Further test should be done. Please use it carefully.
# FFLAGS=-r8 -fast -mcmodel=medium -Mscalarsse -Mcache_align -Mflushz -w
#====================================
# g77 AMD64==========================
#  FFLAGS=-O6 -m64 -ffixed-line-length-132 \
#    -fno-f2c -fautomatic -fforce-mem -ffast-math \
#              -fstrength-reduce -fexpensive-optimizations -fcaller-saves \
#                        -funroll-loops -finline-functions
#====================================


all: $(PROG)

$(PROG): $(OBJS) $(EXTRAS)
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(EXTRAS) $(LIBS) 


clean:
	rm -f *.o*
	rm -f BIMD/*.o*
	rm -f GA/*.o*
	rm -f MAXENT/*.o*
	rm -f PTBH/*.o*
	rm -f PTMC/*.o*
	rm -f tester/*.o*
	rm -f top/*.o*
	rm -f ../src/*.o*
	rm -f ../src/cn/*.o*
	rm -f ../src/min_PES/*.o*
	rm -f ../src/top/*.o*
	rm -f ../src/PES/*.o*
	rm -f ../tools/*.o*
