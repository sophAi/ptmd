	real*8 function gauss
	real*8 a1,a3,a5,a7,a9,sum1,r,r2,i,x1
        parameter (a1=3.949846138D0 ,a3=0.252408784D0 )
        parameter (a5=0.076542912D0 ,a7=0.008355968D0 )
        parameter (a9=0.029899776D0)
        external ranf
        sum1=0.D0
        do i=1,12
        call random(x1)
        sum1=sum1+x1
        end do
        r=(sum1 -6.D0)/4.D0
        r2=r*r
        gauss = (((( a9*r2 +a7 ) *r2 +a5 )*r2 +a3 )*r2+a1) *r
        return 
        end
