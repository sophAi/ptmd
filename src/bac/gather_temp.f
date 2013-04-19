      program gather_temp
      implicit none
      integer j1,atoms
      real*8 z1,energy,ave_energy,ave_energy2,heat,k
      real*8 init_loop,finial_loop,temp
      real*8 init_T,finial_T,dis_T
      character name0*7,name1*1,name2*1,name3*1,name4*1
      character nameT0*4,nameT1*1,nameT2*1,nameT3*1,nameT4*1
      character na0*7,na1*1,na2*1,na3*1,na4*1
      character name_repeat*9
      character*1 name_repeat1,name_repeat2,name_repeat3,name_repeat4
      character*1 name_repeat5,name_repeat6,name_repeat7,name_repeat8
      character*1 name_repeat9

       write (*,*) "temp number ,for example 7D6"
       read (*,*) temp
       k=8.617385D-2

c       open (22,file="loop.dat")
c       read (22,*) init_loop,finial_loop
c       close (22) 
       open (16,file="init_parameter.dat")
       read (16,*) init_T,finial_T,dis_T
       close(16)
       open (17,file="atoms.dat")
       read (17,*) atoms
       close(17)
        nameT1= char(mod (int(init_T )/1000,10)+48)
        nameT2= char(mod (int(init_T)/100,10)+48)
        nameT3= char(mod (int(init_T)/10,10)+48)
        nameT4= char(mod (int(init_T)/1,10)+48)
        nameT0= nameT1
     &                          //nameT2//nameT3//nameT4


          name_repeat1=char(mod (int (1.)/int(1D8),10)+48)
          name_repeat2=char(mod (int (1.)/inT(1d7),10)+48)
          name_repeat3=char(mod (int (1.)/int(1D6),10)+48)
          name_repeat4=char(mod (int (1.)/int(1D5),10)+48)
          name_repeat5=char(mod (int (1.)/int(1D4),10)+48)
          name_repeat6=char(mod (int (1.)/int(1D3),10)+48)
          name_repeat7=char(mod (int (1.)/int(1D2),10)+48)
          name_repeat8=char(mod (int (1.)/int(1D1),10)+48)
          name_repeat9=char(mod (int (1.)/int(1D0),10)+48)
          name_repeat=name_repeat1//name_repeat2//name_repeat3
     &                //name_repeat4//name_repeat5//name_repeat6
     &                //name_repeat7//name_repeat8//name_repeat9



      open (40,file="temporany_"//"40_date9"//"_"//nameT0//".dat")

      ave_energy=0 
      ave_energy2=0 
      do j1=1,temp
         if (j1>9D6) then 
           read (40,*) z1,energy
c         write (*,*) z1,energy
           ave_energy=ave_energy+energy
           ave_energy2=ave_energy2+energy**2
         end if
      end do
      close(40)
         ave_energy=ave_energy/(temp-9D6)
         ave_energy2=ave_energy2/(temp-9D6)
         heat=((ave_energy2-ave_energy**2)*atoms**2)/
     &                      (init_T**2*((k*1D-3)**2))
         heat=heat/atoms    
        open (25,file=nameT0//"ave_energy.dat",status="replace")
        open (26,file=nameT0//"ave_heat.dat",status="replace") 
        write (25,*) init_T,ave_energy
        write (26,*) init_T,heat
        close(25)
        close(26)

      end 
