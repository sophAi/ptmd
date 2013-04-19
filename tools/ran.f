      function ran()   
!This is the most uniform self-generating random number generator in numerical recipes of F77
!The codes are modified from ran2. MPICH2 is required.
!The function output the random values from 0.D0 to 1.D0 for double precision
!No need to input any parameter.
!Refined by Po-Jen Hsu
      implicit none
      include "mpif.h"
      integer idum,im1,im2,imm1,ia1,ia2,iq1,iq2,ir1,ir2,ntab,ndiv
      real*8 ran,am,rnmx,eps
      real*8 seed_from_time,time_scale,seed_scale
      parameter (im1=2147483563,im2=2147483399,am=1.D0/dble(im1),
     *imm1=im1-1,ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211,
     *ir2=3791,ntab=32,ndiv=1+imm1/ntab,eps=1.2D-7,
     *rnmx=1.D0-eps,time_scale=1D3,seed_scale=13D4)
      integer idum2,j,k,iv(ntab),iy
      save iv,iy,idum2
      data idum2/123456789/, iv/ntab*0/, iy/0/
      seed_from_time=mpi_wtime()*time_scale
      idum=
     &-dint((seed_from_time-dble(dint(seed_from_time)))*seed_scale) !Self-generating seed number, must < 0 to re-initialized 
C      write(*,*)dble(seed_from_time),dble(dint(seed_from_time))  !for test
C     &,seed_from_time-dble(dint(seed_from_time)),idum
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=ntab+8,1,-1
          k=idum/iq1
          idum=ia1*(idum-k*iq1)-k*ir1
          if (idum.lt.0) idum=idum+im1
          if (j.le.ntab) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/iq1     
      idum=ia1*(idum-k*iq1)-k*ir1
      if (idum.lt.0) idum=idum+im1
      k=idum2/iq2
      idum2=ia2*(idum2-k*iq2)-k*ir2
      if (idum2.lt.0) idum2=idum2+im2
      j=1+iy/ndiv
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+imm1
      ran=dmin1(am*dble(iy),rnmx)
      return
      end

      function rani(max_rani_range)  !Self-generating random number for integer.
      implicit none
      integer rani,max_rani_range !assign the range from 0 to rani_range
      real*8 ran
      rani=1+dint(dble(max_rani_range)*ran())
      end

