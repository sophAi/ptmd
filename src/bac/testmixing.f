       program testmixing
       real*8 Eaa,Ebb,Eab,A,Fa,Fb,deltaE
       Ebb=-130.540017D0
       Eaa=-108.967591D0
       Eaa=Eaa/38.D0
       Ebb=Ebb/38.D0
       write(*,*)"please input number of A"
       read(*,*)A
       write(*,*) "Please input Eab="
       read(*,*) Eab
       Eab=Eab/38.D0
       Fa=A/38.D0
       Fb=1.D0-Fa
       deltaE=Eab-Fa*Eaa-Fb*Ebb
       write(*,*) "DeltaEmix=deltaE",deltaE
       stop
       end
