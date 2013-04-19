/* testdist.f -- translated by f2c (version 20050501).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__3 = 3;
static integer c__2 = 2;

/*<        program main >*/
/* Main program */ int MAIN__()
{
    /* System generated locals */
    address a__1[2], a__2[3];
    integer i__1[2], i__2[3], i__3, i__4, i__5;
    doublereal d__1, d__2, d__3;
    char ch__1[1], ch__2[1], ch__3[13], ch__4[12];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(), s_rsle(cilist *), e_rsle();
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);
    integer f_open(olist *), s_cmp(char *, char *, ftnlen, ftnlen), f_clos(
	    cllist *);
    double sqrt(doublereal);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    doublereal ra_total__, rb_total__;
    integer i__, j, n, atom_num1__;
    char name_xyz1__[12];
    integer a1;
    doublereal rab_total__, cent_dist__[1000];
    integer k1, cycle_num__;
    doublereal ratiocent, x1[3000], y1[3000], z1[3000], order_sum__;
    char name_dist1__[8];
    doublereal ra, rb;
    integer np[1000];
    doublereal kx[1000];
    char yn[1], quick_name__[2];
    doublereal k1a, k1b, k1c, m1a[3000], m1b[3000], m1c[3000], total_bond__, 
	    avdistcent[1000];
    char atom_name1a__[2], atom_name1b__[2];
    doublereal r01a, r01b, r01c, rab;
    integer num, flag1, type1;
    doublereal delta[1000000]	/* was [1000][1000] */, order, centx, centy, 
	    centz;
    char atom1a[2], atom1b[2];
    doublereal dist1a, dist1b, dist1c, ratio1a, ratio1b, ratio1c, distm1a[
	    3000], add_fac__, energy1, avdist1, maxtemp, avdist1a, avdist1b, 
	    avdist1c;

    /* Fortran I/O blocks */
    static cilist io___6 = { 0, 6, 0, 0, 0 };
    static cilist io___7 = { 0, 5, 0, 0, 0 };
    static cilist io___18 = { 0, 11, 0, 0, 0 };
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___24 = { 0, 5, 0, 0, 0 };
    static cilist io___28 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 5, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 5, 0, 0, 0 };
    static cilist io___32 = { 0, 6, 0, 0, 0 };
    static cilist io___33 = { 0, 5, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, 0, 0 };
    static cilist io___35 = { 0, 5, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___37 = { 0, 5, 0, 0, 0 };
    static cilist io___38 = { 0, 6, 0, 0, 0 };
    static cilist io___39 = { 0, 5, 0, 0, 0 };
    static cilist io___40 = { 0, 6, 0, 0, 0 };
    static cilist io___41 = { 0, 5, 0, 0, 0 };
    static cilist io___42 = { 0, 6, 0, 0, 0 };
    static cilist io___43 = { 0, 5, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, 0, 0 };
    static cilist io___45 = { 0, 5, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };
    static cilist io___47 = { 0, 5, 0, 0, 0 };
    static cilist io___49 = { 0, 1, 0, 0, 0 };
    static cilist io___53 = { 0, 1, 0, 0, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };
    static cilist io___62 = { 0, 10, 0, 0, 0 };
    static cilist io___79 = { 0, 10, 0, 0, 0 };
    static cilist io___80 = { 0, 10, 0, 0, 0 };
    static cilist io___81 = { 0, 10, 0, 0, 0 };
    static cilist io___82 = { 0, 10, 0, 0, 0 };
    static cilist io___83 = { 0, 10, 0, 0, 0 };
    static cilist io___84 = { 0, 11, 0, 0, 0 };
    static cilist io___85 = { 0, 10, 0, 0, 0 };
    static cilist io___88 = { 0, 11, 0, 0, 0 };
    static cilist io___89 = { 0, 10, 0, 0, 0 };
    static cilist io___94 = { 0, 10, 0, 0, 0 };
    static cilist io___95 = { 0, 10, 0, 0, 0 };
    static cilist io___96 = { 0, 10, 0, 0, 0 };
    static cilist io___97 = { 0, 10, 0, 0, 0 };
    static cilist io___98 = { 0, 10, 0, 0, 0 };
    static cilist io___99 = { 0, 10, 0, 0, 0 };
    static cilist io___100 = { 0, 10, 0, 0, 0 };
    static cilist io___101 = { 0, 10, 0, 0, 0 };
    static cilist io___107 = { 0, 11, 0, 0, 0 };
    static cilist io___108 = { 0, 12, 0, 0, 0 };
    static cilist io___109 = { 0, 13, 0, 0, 0 };
    static cilist io___111 = { 0, 6, 0, 0, 0 };
    static cilist io___113 = { 0, 31, 0, 0, 0 };
    static cilist io___114 = { 0, 6, 0, 0, 0 };
    static cilist io___115 = { 0, 6, 0, 0, 0 };
    static cilist io___116 = { 0, 6, 0, 0, 0 };
    static cilist io___117 = { 0, 6, 0, 0, 0 };
    static cilist io___118 = { 0, 6, 0, 0, 0 };
    static cilist io___119 = { 0, 6, 0, 0, 0 };


/*<        integer i,j,atom_num1,a1,flag1,cycle_num,np(1000),type1,num >*/
/*<        real*8 k1a,k1b,k1c,x1(3000),y1(3000),z1(3000),dist1a,dist1b >*/
/*<        real*8 energy1,R01a,ratio1a,ratio1b,avdist1a,avdist1b,avdist1c >*/
/*<        real*8 m1a(3000),m1b(3000),m1c(3000),R01c,ratio1c,cent_dist(1000) >*/
/*<        real*8 distm1a(3000),centx,centy,centz,dist1c,R01b,add_fac >*/
/*<    >*/
/*<        real*8 ratiocent,avdistcent(1000),maxtemp,kx(1000),total_bond >*/
/*<        character name_xyz1*12,atom1a*2,atom1b*2,quick_name*2 >*/
/*<        character yn*1,name_dist1*8 >*/
/*<        character atom_name1a*2,atom_name1b*2 >*/
/*<        k1=0.D0 >*/
    k1 = 0;
/*<        avdist1=0.D0 >*/
    avdist1 = 0.;
/*<        centx=0.D0 >*/
    centx = 0.;
/*<        centy=0.D0 >*/
    centy = 0.;
/*<        centz=0.D0 >*/
    centz = 0.;
/*<        write(*,*) "Quick input?(y/n)" >*/
    s_wsle(&io___6);
    do_lio(&c__9, &c__1, "Quick input?(y/n)", (ftnlen)17);
    e_wsle();
/*<        read(*,*) yn >*/
    s_rsle(&io___7);
    do_lio(&c__9, &c__1, yn, (ftnlen)1);
    e_rsle();
/*<        if(yn.eq."y")then >*/
    if (*(unsigned char *)yn == 'y') {
/*<          add_fac=0.01D0 >*/
	add_fac__ = .01;
/*<          cycle_num=50 >*/
	cycle_num__ = 50;
/*<          R01a=2.556D0 >*/
	r01a = 2.556;
/*<          ratio1a=1.D0 >*/
	ratio1a = 1.;
/*<          R01b=2.884D0 >*/
	r01b = 2.884;
/*<          ratio1b=1.D0 >*/
	ratio1b = 1.;
/*<          R01c=2.556D0 >*/
	r01c = 2.556;
/*<          ratio1c=1.D0 >*/
	ratio1c = 1.;
/*<          ratiocent=1.2D0 >*/
	ratiocent = 1.2;
/*<          write(11,*) cent_dist(i),avdistcent(i),atom_num1 >*/
	s_wsle(&io___18);
	do_lio(&c__5, &c__1, (char *)&cent_dist__[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&avdistcent[i__ - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__3, &c__1, (char *)&atom_num1__, (ftnlen)sizeof(integer));
	e_wsle();
/*<          yn="n" >*/
	*(unsigned char *)yn = 'n';
/*<        write(*,*) "Please input the number of cluster #1(ex:01,02,03..)" >*/
	s_wsle(&io___23);
	do_lio(&c__9, &c__1, "Please input the number of cluster #1(ex:01,02\
,03..)", (ftnlen)52);
	e_wsle();
/*<          read(*,*) num >*/
	s_rsle(&io___24);
	do_lio(&c__3, &c__1, (char *)&num, (ftnlen)sizeof(integer));
	e_rsle();
/*<          quick_name=char(int(num/10)+48)//char(mod(num,10)+48) >*/
/* Writing concatenation */
	*(unsigned char *)&ch__1[0] = num / 10 + 48;
	i__1[0] = 1, a__1[0] = ch__1;
	*(unsigned char *)&ch__2[0] = num % 10 + 48;
	i__1[1] = 1, a__1[1] = ch__2;
	s_cat(quick_name__, a__1, i__1, &c__2, (ftnlen)2);
/*<          name_xyz1="0038CA"//quick_name//".xyz" >*/
/* Writing concatenation */
	i__2[0] = 6, a__2[0] = "0038CA";
	i__2[1] = 2, a__2[1] = quick_name__;
	i__2[2] = 4, a__2[2] = ".xyz";
	s_cat(name_xyz1__, a__2, i__2, &c__3, (ftnlen)12);
/*<          goto 30 >*/
	goto L30;
/*<        endif >*/
    }
/*<        write(*,*) "Please input the add factor(default=0.01D0)" >*/
    s_wsle(&io___28);
    do_lio(&c__9, &c__1, "Please input the add factor(default=0.01D0)", (
	    ftnlen)43);
    e_wsle();
/*<        read(*,*) add_fac >*/
    s_rsle(&io___29);
    do_lio(&c__5, &c__1, (char *)&add_fac__, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "please input the cycle number(default=50)" >*/
    s_wsle(&io___30);
    do_lio(&c__9, &c__1, "please input the cycle number(default=50)", (ftnlen)
	    41);
    e_wsle();
/*<        read(*,*) cycle_num >*/
    s_rsle(&io___31);
    do_lio(&c__3, &c__1, (char *)&cycle_num__, (ftnlen)sizeof(integer));
    e_rsle();
/*<        write(*,*) "Please input the file name of cluster #1:(*.xyz)" >*/
    s_wsle(&io___32);
    do_lio(&c__9, &c__1, "Please input the file name of cluster #1:(*.xyz)", (
	    ftnlen)48);
    e_wsle();
/*<        read(*,*) name_xyz1 >*/
    s_rsle(&io___33);
    do_lio(&c__9, &c__1, name_xyz1__, (ftnlen)12);
    e_rsle();
/*<        write(*,*) "please input R0 of atom a-a(default=2.556D0)" >*/
    s_wsle(&io___34);
    do_lio(&c__9, &c__1, "please input R0 of atom a-a(default=2.556D0)", (
	    ftnlen)44);
    e_wsle();
/*<        read(*,*) R01a >*/
    s_rsle(&io___35);
    do_lio(&c__5, &c__1, (char *)&r01a, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "Please input the ratio of atom a-a(default=1.D0)" >*/
    s_wsle(&io___36);
    do_lio(&c__9, &c__1, "Please input the ratio of atom a-a(default=1.D0)", (
	    ftnlen)48);
    e_wsle();
/*<        read(*,*) ratio1a >*/
    s_rsle(&io___37);
    do_lio(&c__5, &c__1, (char *)&ratio1a, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "please input R0 of atom b-b(default=2.884D0)" >*/
    s_wsle(&io___38);
    do_lio(&c__9, &c__1, "please input R0 of atom b-b(default=2.884D0)", (
	    ftnlen)44);
    e_wsle();
/*<        read(*,*) R01b >*/
    s_rsle(&io___39);
    do_lio(&c__5, &c__1, (char *)&r01b, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "Please input the ratio of atom b-b(default=1.D0)" >*/
    s_wsle(&io___40);
    do_lio(&c__9, &c__1, "Please input the ratio of atom b-b(default=1.D0)", (
	    ftnlen)48);
    e_wsle();
/*<        read(*,*) ratio1b >*/
    s_rsle(&io___41);
    do_lio(&c__5, &c__1, (char *)&ratio1b, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "please input R0 of atom a-b(default=2.556D0)" >*/
    s_wsle(&io___42);
    do_lio(&c__9, &c__1, "please input R0 of atom a-b(default=2.556D0)", (
	    ftnlen)44);
    e_wsle();
/*<        read(*,*) R01c >*/
    s_rsle(&io___43);
    do_lio(&c__5, &c__1, (char *)&r01c, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        write(*,*) "Please input the ratio of atom a-b(default=1.D0)" >*/
    s_wsle(&io___44);
    do_lio(&c__9, &c__1, "Please input the ratio of atom a-b(default=1.D0)", (
	    ftnlen)48);
    e_wsle();
/*<        read(*,*) ratio1c >*/
    s_rsle(&io___45);
    do_lio(&c__5, &c__1, (char *)&ratio1c, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<    >*/
    s_wsle(&io___46);
    do_lio(&c__9, &c__1, "Please input the ratio of internal strain(default=\
1.2D0)", (ftnlen)56);
    e_wsle();
/*<        read(*,*) ratiocent >*/
    s_rsle(&io___47);
    do_lio(&c__5, &c__1, (char *)&ratiocent, (ftnlen)sizeof(doublereal));
    e_rsle();
/*< 30     name_dist1=name_xyz1 >*/
L30:
    s_copy(name_dist1__, name_xyz1__, (ftnlen)8, (ftnlen)12);
/*<        open(10,file=name_dist1//".dist",status="replace") >*/
    o__1.oerr = 0;
    o__1.ounit = 10;
    o__1.ofnmlen = 13;
/* Writing concatenation */
    i__1[0] = 8, a__1[0] = name_dist1__;
    i__1[1] = 5, a__1[1] = ".dist";
    s_cat(ch__3, a__1, i__1, &c__2, (ftnlen)13);
    o__1.ofnm = ch__3;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        open(1,file=name_xyz1,status="old") >*/
    o__1.oerr = 0;
    o__1.ounit = 1;
    o__1.ofnmlen = 12;
    o__1.ofnm = name_xyz1__;
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        read(1,*) atom_num1,energy1 >*/
    s_rsle(&io___49);
    do_lio(&c__3, &c__1, (char *)&atom_num1__, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&energy1, (ftnlen)sizeof(doublereal));
    e_rsle();
/*<        a1=1 >*/
    a1 = 1;
/*<        flag1=0 >*/
    flag1 = 0;
/*<        do i=1,atom_num1  >*/
    i__3 = atom_num1__;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<           read(1,*) atom1a,x1(i),y1(i),z1(i) >*/
	s_rsle(&io___53);
	do_lio(&c__9, &c__1, atom1a, (ftnlen)2);
	do_lio(&c__5, &c__1, (char *)&x1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	do_lio(&c__5, &c__1, (char *)&z1[i__ - 1], (ftnlen)sizeof(doublereal))
		;
	e_rsle();
/*<           centx=centx+x1(i) >*/
	centx += x1[i__ - 1];
/*<           centy=centy+y1(i) >*/
	centy += y1[i__ - 1];
/*<           centz=centz+z1(i) >*/
	centz += z1[i__ - 1];
/*<           if(atom1a.eq.atom1b.and.flag1.eq.0)then  >*/
	if (s_cmp(atom1a, atom1b, (ftnlen)2, (ftnlen)2) == 0 && flag1 == 0) {
/*<             a1=a1+1 >*/
	    ++a1;
/*<             atom_name1a=atom1a >*/
	    s_copy(atom_name1a__, atom1a, (ftnlen)2, (ftnlen)2);
/*<           else >*/
	} else {
/*<             atom_name1b=atom1a >*/
	    s_copy(atom_name1b__, atom1a, (ftnlen)2, (ftnlen)2);
/*<             if(i.ne.1) flag1=1 >*/
	    if (i__ != 1) {
		flag1 = 1;
	    }
/*<           endif >*/
	}
/*<           atom1b=atom1a >*/
	s_copy(atom1b, atom1a, (ftnlen)2, (ftnlen)2);
/*<        enddo >*/
    }
/*<        centx=centx/atom_num1 >*/
    centx /= atom_num1__;
/*<        centy=centy/atom_num1 >*/
    centy /= atom_num1__;
/*<        centz=centz/atom_num1 >*/
    centz /= atom_num1__;
/*<        close(1) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 1;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<    >*/
    s_wsle(&io___61);
    do_lio(&c__9, &c__1, "Read cluster #1 A=", (ftnlen)18);
    do_lio(&c__3, &c__1, (char *)&a1, (ftnlen)sizeof(integer));
    do_lio(&c__9, &c__1, ",atom1=", (ftnlen)7);
    do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
    do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
    do_lio(&c__5, &c__1, (char *)&r01a, (ftnlen)sizeof(doublereal));
    do_lio(&c__9, &c__1, ",atom2=", (ftnlen)7);
    do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
    do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
    do_lio(&c__5, &c__1, (char *)&r01b, (ftnlen)sizeof(doublereal));
    e_wsle();
/*<    >*/
    s_wsle(&io___62);
    do_lio(&c__9, &c__1, "Read cluster #1 A=", (ftnlen)18);
    do_lio(&c__3, &c__1, (char *)&a1, (ftnlen)sizeof(integer));
    do_lio(&c__9, &c__1, ",atom1=", (ftnlen)7);
    do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
    do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
    do_lio(&c__5, &c__1, (char *)&r01a, (ftnlen)sizeof(doublereal));
    do_lio(&c__9, &c__1, ",atom2=", (ftnlen)7);
    do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
    do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
    do_lio(&c__5, &c__1, (char *)&r01b, (ftnlen)sizeof(doublereal));
    e_wsle();
/*<        do n=1,cycle_num >*/
    i__3 = cycle_num__;
    for (n = 1; n <= i__3; ++n) {
/*<          avdist1a=0.D0 >*/
	avdist1a = 0.;
/*<          avdist1b=0.D0 >*/
	avdist1b = 0.;
/*<          avdist1c=0.D0 >*/
	avdist1c = 0.;
/*<          k1a=0.D0 >*/
	k1a = 0.;
/*<          k1b=0.D0 >*/
	k1b = 0.;
/*<          k1c=0.D0 >*/
	k1c = 0.;
/*<          do i=1,atom_num1 >*/
	i__4 = atom_num1__;
	for (i__ = 1; i__ <= i__4; ++i__) {
/*<            m1a(i)=0.D0 >*/
	    m1a[i__ - 1] = 0.;
/*<            m1b(i)=0.D0 >*/
	    m1b[i__ - 1] = 0.;
/*<            m1c(i)=0.D0 >*/
	    m1c[i__ - 1] = 0.;
/*<            distm1a(i)=0.D0 >*/
	    distm1a[i__ - 1] = 0.;
/*<            do j=1,atom_num1 >*/
	    i__5 = atom_num1__;
	    for (j = 1; j <= i__5; ++j) {
/*<              delta(i,j)=0.D0 >*/
		delta[i__ + j * 1000 - 1001] = 0.;
/*<              if(i.ne.j)then >*/
		if (i__ != j) {
/*<                dist1a=(x1(i)-x1(j))**2+(y1(i)-y1(j))**2+(z1(i)-z1(j))**2 >*/
/* Computing 2nd power */
		    d__1 = x1[i__ - 1] - x1[j - 1];
/* Computing 2nd power */
		    d__2 = y1[i__ - 1] - y1[j - 1];
/* Computing 2nd power */
		    d__3 = z1[i__ - 1] - z1[j - 1];
		    dist1a = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<                dist1a=dsqrt(dist1a) >*/
		    dist1a = sqrt(dist1a);
/*<                if((i.le.a1).and.(j.le.a1))then  >*/
		    if (i__ <= a1 && j <= a1) {
/*<                  if(dist1a.le.(ratio1a*R01a))then >*/
			if (dist1a <= ratio1a * r01a) {
/*<                    avdist1a=avdist1a+dist1a >*/
			    avdist1a += dist1a;
/*<                    distm1a(i)=distm1a(i)+dist1a >*/
			    distm1a[i__ - 1] += dist1a;
/*<                    k1a=k1a+1.D0 >*/
			    k1a += 1.;
/*<                    m1a(i)=m1a(i)+1.D0 >*/
			    m1a[i__ - 1] += 1.;
/*<                    delta(i,j)=1.D0 >*/
			    delta[i__ + j * 1000 - 1001] = 1.;
/*<                  endif >*/
			}
/*<                else if((i.gt.a1).and.(j.gt.a1))then >*/
		    } else if (i__ > a1 && j > a1) {
/*<                  if(dist1a.le.(ratio1b*R01b))then >*/
			if (dist1a <= ratio1b * r01b) {
/*<                    avdist1a=avdist1a+dist1a >*/
			    avdist1a += dist1a;
/*<                    distm1a(i)=distm1a(i)+dist1a >*/
			    distm1a[i__ - 1] += dist1a;
/*<                    k1a=k1a+1.D0 >*/
			    k1a += 1.;
/*<                    m1a(i)=m1a(i)+1.D0 >*/
			    m1a[i__ - 1] += 1.;
/*<                    delta(i,j)=1.D0 >*/
			    delta[i__ + j * 1000 - 1001] = 1.;
/*<                  endif >*/
			}
/*<                else >*/
		    } else {
/*<                  if(dist1a.le.(ratio1c*R01c))then >*/
			if (dist1a <= ratio1c * r01c) {
/*<                    avdist1a=avdist1a+dist1a >*/
			    avdist1a += dist1a;
/*<                    distm1a(i)=distm1a(i)+dist1a >*/
			    distm1a[i__ - 1] += dist1a;
/*<                    k1a=k1a+1.D0 >*/
			    k1a += 1.;
/*<                    m1a(i)=m1a(i)+1.D0 >*/
			    m1a[i__ - 1] += 1.;
/*<                    delta(i,j)=1.D0 >*/
			    delta[i__ + j * 1000 - 1001] = 1.;
/*<                  endif >*/
			}
/*<                endif >*/
		    }
/*<              endif >*/
		}
/*<            enddo >*/
	    }
/*<            distm1a(i)=distm1a(i)/m1a(i) >*/
	    distm1a[i__ - 1] /= m1a[i__ - 1];
/*<          enddo >*/
	}
/*<          do i=1,a1 >*/
	i__4 = a1;
	for (i__ = 1; i__ <= i__4; ++i__) {
/*<            do j=i+1,a1 >*/
	    i__5 = a1;
	    for (j = i__ + 1; j <= i__5; ++j) {
/*<              dist1b=(x1(i)-x1(j))**2+(y1(i)-y1(j))**2+(z1(i)-z1(j))**2 >*/
/* Computing 2nd power */
		d__1 = x1[i__ - 1] - x1[j - 1];
/* Computing 2nd power */
		d__2 = y1[i__ - 1] - y1[j - 1];
/* Computing 2nd power */
		d__3 = z1[i__ - 1] - z1[j - 1];
		dist1b = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<              dist1b=dsqrt(dist1b) >*/
		dist1b = sqrt(dist1b);
/*<              if(dist1b.le.(ratio1a*R01a))then >*/
		if (dist1b <= ratio1a * r01a) {
/*<                avdist1b=avdist1b+dist1b >*/
		    avdist1b += dist1b;
/*<                k1b=k1b+1.D0 >*/
		    k1b += 1.;
/*<                m1b(i)=m1b(i)+1.D0 >*/
		    m1b[i__ - 1] += 1.;
/*<              endif >*/
		}
/*<            enddo >*/
	    }
/*<          enddo >*/
	}
/*<          do i=a1+1,atom_num1 >*/
	i__4 = atom_num1__;
	for (i__ = a1 + 1; i__ <= i__4; ++i__) {
/*<            do j=i+1,atom_num1 >*/
	    i__5 = atom_num1__;
	    for (j = i__ + 1; j <= i__5; ++j) {
/*<              dist1c=(x1(i)-x1(j))**2+(y1(i)-y1(j))**2+(z1(i)-z1(j))**2 >*/
/* Computing 2nd power */
		d__1 = x1[i__ - 1] - x1[j - 1];
/* Computing 2nd power */
		d__2 = y1[i__ - 1] - y1[j - 1];
/* Computing 2nd power */
		d__3 = z1[i__ - 1] - z1[j - 1];
		dist1c = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<              dist1c=dsqrt(dist1c) >*/
		dist1c = sqrt(dist1c);
/*<              if(dist1c.le.(ratio1b*R01b))then >*/
		if (dist1c <= ratio1b * r01b) {
/*<                avdist1c=avdist1c+dist1c >*/
		    avdist1c += dist1c;
/*<                k1c=k1c+1.D0 >*/
		    k1c += 1.;
/*<                m1c(i)=m1c(i)+1.D0 >*/
		    m1c[i__ - 1] += 1.;
/*<              endif >*/
		}
/*<            enddo >*/
	    }
/*<          enddo >*/
	}
/*<          avdist1a=avdist1a/k1a >*/
	avdist1a /= k1a;
/*<          avdist1b=avdist1b/k1b >*/
	avdist1b /= k1b;
/*<          avdist1c=avdist1c/k1c >*/
	avdist1c /= k1c;
/*<          write(10,*) "<STEP>=",n >*/
	s_wsle(&io___79);
	do_lio(&c__9, &c__1, "<STEP>=", (ftnlen)7);
	do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
	e_wsle();
/*<          write(10,*) "For ",name_xyz1 >*/
	s_wsle(&io___80);
	do_lio(&c__9, &c__1, "For ", (ftnlen)4);
	do_lio(&c__9, &c__1, name_xyz1__, (ftnlen)12);
	e_wsle();
/*<          write(10,*) "Energy is ",energy1 >*/
	s_wsle(&io___81);
	do_lio(&c__9, &c__1, "Energy is ", (ftnlen)10);
	do_lio(&c__5, &c__1, (char *)&energy1, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<          write(10,*) "Average distance is ",avdist1a >*/
	s_wsle(&io___82);
	do_lio(&c__9, &c__1, "Average distance is ", (ftnlen)20);
	do_lio(&c__5, &c__1, (char *)&avdist1a, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<          write(10,*) "Center of mass=",centx,centy,centz >*/
	s_wsle(&io___83);
	do_lio(&c__9, &c__1, "Center of mass=", (ftnlen)15);
	do_lio(&c__5, &c__1, (char *)&centx, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&centy, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&centz, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<          open(11,file=name_xyz1,status="old") >*/
	o__1.oerr = 0;
	o__1.ounit = 11;
	o__1.ofnmlen = 12;
	o__1.ofnm = name_xyz1__;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
/*<          read(11,*) atom_num1,energy1 >*/
	s_rsle(&io___84);
	do_lio(&c__3, &c__1, (char *)&atom_num1__, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&energy1, (ftnlen)sizeof(doublereal));
	e_rsle();
/*<    >*/
	s_wsle(&io___85);
	do_lio(&c__9, &c__1, "# name  cent_dist  #nearest_bond  #aa  #bb  av\
g_bondist", (ftnlen)55);
	e_wsle();
/*<          RA=0.D0 >*/
	ra = 0.;
/*<          RB=0.D0 >*/
	rb = 0.;
/*<          do i=1,atom_num1 >*/
	i__4 = atom_num1__;
	for (i__ = 1; i__ <= i__4; ++i__) {
/*<             read(11,*) atom1a,x1(i),y1(i),z1(i) >*/
	    s_rsle(&io___88);
	    do_lio(&c__9, &c__1, atom1a, (ftnlen)2);
	    do_lio(&c__5, &c__1, (char *)&x1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&y1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&z1[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_rsle();
/*<          cent_dist(i)=(x1(i)-centx)**2+(y1(i)-centy)**2+(z1(i)-centz)**2 >*/
/* Computing 2nd power */
	    d__1 = x1[i__ - 1] - centx;
/* Computing 2nd power */
	    d__2 = y1[i__ - 1] - centy;
/* Computing 2nd power */
	    d__3 = z1[i__ - 1] - centz;
	    cent_dist__[i__ - 1] = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<             cent_dist(i)=dsqrt(cent_dist(i)) >*/
	    cent_dist__[i__ - 1] = sqrt(cent_dist__[i__ - 1]);
/*<             if(i.le.a1) RA=RA+cent_dist(i) >*/
	    if (i__ <= a1) {
		ra += cent_dist__[i__ - 1];
	    }
/*<             if(i.gt.a1) RB=RB+cent_dist(i) >*/
	    if (i__ > a1) {
		rb += cent_dist__[i__ - 1];
	    }
/*<    >*/
	    s_wsle(&io___89);
	    do_lio(&c__3, &c__1, (char *)&i__, (ftnlen)sizeof(integer));
	    do_lio(&c__9, &c__1, " ", (ftnlen)1);
	    do_lio(&c__9, &c__1, atom1a, (ftnlen)2);
	    do_lio(&c__5, &c__1, (char *)&cent_dist__[i__ - 1], (ftnlen)
		    sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&m1a[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&m1b[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&m1c[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&distm1a[i__ - 1], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/*<          enddo >*/
	}
/*<          RA_total=RA >*/
	ra_total__ = ra;
/*<          RB_total=RB >*/
	rb_total__ = rb;
/*<          RAB_total=RA+RB >*/
	rab_total__ = ra + rb;
/*<          RAB=(RA+RB)/atom_num1 >*/
	rab = (ra + rb) / atom_num1__;
/*<          if(a1.ne.0.D0)RA=RA/a1 >*/
	if ((doublereal) a1 != 0.) {
	    ra /= a1;
	}
/*<          if(a1.ne.atom_num1)RB=RB/(atom_num1-a1) >*/
	if (a1 != atom_num1__) {
	    rb /= atom_num1__ - a1;
	}
/*<          close(11) >*/
	cl__1.cerr = 0;
	cl__1.cunit = 11;
	cl__1.csta = 0;
	f_clos(&cl__1);
/*<    >*/
	s_wsle(&io___94);
	do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
	do_lio(&c__9, &c__1, "=", (ftnlen)1);
	do_lio(&c__5, &c__1, (char *)&avdist1b, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",HIT=", (ftnlen)5);
	do_lio(&c__5, &c__1, (char *)&k1b, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",", (ftnlen)1);
	do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
	do_lio(&c__9, &c__1, "=", (ftnlen)1);
	do_lio(&c__5, &c__1, (char *)&avdist1c, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",HIT=", (ftnlen)5);
	do_lio(&c__5, &c__1, (char *)&k1c, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<    >*/
	s_wsle(&io___95);
	do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
	do_lio(&c__9, &c__1, "/", (ftnlen)1);
	do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
	do_lio(&c__9, &c__1, " or ", (ftnlen)4);
	do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
	do_lio(&c__9, &c__1, "/", (ftnlen)1);
	do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
	e_wsle();
/*<          write(10,*) avdist1b/avdist1c," or ",avdist1c/avdist1b >*/
	s_wsle(&io___96);
	d__1 = avdist1b / avdist1c;
	do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, " or ", (ftnlen)4);
	d__2 = avdist1c / avdist1b;
	do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<          write(10,*) "=============================================" >*/
	s_wsle(&io___97);
	do_lio(&c__9, &c__1, "=============================================", 
		(ftnlen)45);
	e_wsle();
/*<          write(10,*) >*/
	s_wsle(&io___98);
	e_wsle();
/*         pause */
/*<          ratio1a=ratio1a+add_fac >*/
	ratio1a += add_fac__;
/*<          ratio1b=ratio1b+add_fac >*/
	ratio1b += add_fac__;
/*<          write(10,*) "=============================================" >*/
	s_wsle(&io___99);
	do_lio(&c__9, &c__1, "=============================================", 
		(ftnlen)45);
	e_wsle();
/*<    >*/
	s_wsle(&io___100);
	do_lio(&c__9, &c__1, "Ratio of cluster 1 ", (ftnlen)19);
	do_lio(&c__9, &c__1, atom_name1a__, (ftnlen)2);
	do_lio(&c__9, &c__1, "=", (ftnlen)1);
	do_lio(&c__5, &c__1, (char *)&ratio1a, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",dist=", (ftnlen)6);
	d__1 = ratio1a * r01a;
	do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
	do_lio(&c__5, &c__1, (char *)&r01a, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<    >*/
	s_wsle(&io___101);
	do_lio(&c__9, &c__1, "Ratio of cluster 1 ", (ftnlen)19);
	do_lio(&c__9, &c__1, atom_name1b__, (ftnlen)2);
	do_lio(&c__9, &c__1, "=", (ftnlen)1);
	do_lio(&c__5, &c__1, (char *)&ratio1b, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",dist=", (ftnlen)6);
	d__1 = ratio1b * r01b;
	do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_lio(&c__9, &c__1, ",R0=", (ftnlen)4);
	do_lio(&c__5, &c__1, (char *)&r01b, (ftnlen)sizeof(doublereal));
	e_wsle();
/*<        enddo >*/
    }
/*<        close(10) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 10;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<        open(11,file=name_dist1//"a.txt",status="replace") >*/
    o__1.oerr = 0;
    o__1.ounit = 11;
    o__1.ofnmlen = 13;
/* Writing concatenation */
    i__1[0] = 8, a__1[0] = name_dist1__;
    i__1[1] = 5, a__1[1] = "a.txt";
    s_cat(ch__3, a__1, i__1, &c__2, (ftnlen)13);
    o__1.ofnm = ch__3;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        open(12,file=name_dist1//"b.txt",status="replace") >*/
    o__1.oerr = 0;
    o__1.ounit = 12;
    o__1.ofnmlen = 13;
/* Writing concatenation */
    i__1[0] = 8, a__1[0] = name_dist1__;
    i__1[1] = 5, a__1[1] = "b.txt";
    s_cat(ch__3, a__1, i__1, &c__2, (ftnlen)13);
    o__1.ofnm = ch__3;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        open(13,file=name_dist1//".txt",status="replace") >*/
    o__1.oerr = 0;
    o__1.ounit = 13;
    o__1.ofnmlen = 12;
/* Writing concatenation */
    i__1[0] = 8, a__1[0] = name_dist1__;
    i__1[1] = 4, a__1[1] = ".txt";
    s_cat(ch__4, a__1, i__1, &c__2, (ftnlen)12);
    o__1.ofnm = ch__4;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        do i=1,atom_num1 >*/
    i__3 = atom_num1__;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          kx(i)=0.D0 >*/
	kx[i__ - 1] = 0.;
/*<          do j=1,atom_num1 >*/
	i__4 = atom_num1__;
	for (j = 1; j <= i__4; ++j) {
/*<            if(i.ne.j)then >*/
	    if (i__ != j) {
/*<              dist1a=(x1(i)-x1(j))**2+(y1(i)-y1(j))**2+(z1(i)-z1(j))**2 >*/
/* Computing 2nd power */
		d__1 = x1[i__ - 1] - x1[j - 1];
/* Computing 2nd power */
		d__2 = y1[i__ - 1] - y1[j - 1];
/* Computing 2nd power */
		d__3 = z1[i__ - 1] - z1[j - 1];
		dist1a = d__1 * d__1 + d__2 * d__2 + d__3 * d__3;
/*<              dist1a=dsqrt(dist1a) >*/
		dist1a = sqrt(dist1a);
/*<              if((i.le.a1).and.(j.le.a1))then >*/
		if (i__ <= a1 && j <= a1) {
/*<                if(dist1a.le.(ratiocent*R01a))then >*/
		    if (dist1a <= ratiocent * r01a) {
/*<                  avdistcent(i)=avdistcent(i)+dist1a >*/
			avdistcent[i__ - 1] += dist1a;
/*<                  kx(i)=kx(i)+1.D0 >*/
			kx[i__ - 1] += 1.;
/*<                endif >*/
		    }
/*<              else if((i.gt.a1).and.(j.gt.a1))then >*/
		} else if (i__ > a1 && j > a1) {
/*<                if(dist1a.le.(ratiocent*R01b))then >*/
		    if (dist1a <= ratiocent * r01b) {
/*<                  avdistcent(i)=avdistcent(i)+dist1a >*/
			avdistcent[i__ - 1] += dist1a;
/*<                  kx(i)=kx(i)+1.D0 >*/
			kx[i__ - 1] += 1.;
/*<                endif >*/
		    }
/*<              else >*/
		} else {
/*<                if(dist1a.le.(ratiocent*R01c))then >*/
		    if (dist1a <= ratiocent * r01c) {
/*<                  avdistcent(i)=avdistcent(i)+dist1a >*/
			avdistcent[i__ - 1] += dist1a;
/*<                  kx(i)=kx(i)+1.D0 >*/
			kx[i__ - 1] += 1.;
/*<                endif >*/
		    }
/*<              endif >*/
		}
/*<            endif >*/
	    }
/*<          enddo >*/
	}
/*<          avdistcent(i)=avdistcent(i)/kx(i) >*/
	avdistcent[i__ - 1] /= kx[i__ - 1];
/*<        enddo >*/
    }
/*<        do i=1,atom_num1 >*/
    i__3 = atom_num1__;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          np(i)=i >*/
	np[i__ - 1] = i__;
/*<        enddo >*/
    }
/*<        do i=1,atom_num1 >*/
    i__3 = atom_num1__;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          do j=i+1,atom_num1 >*/
	i__4 = atom_num1__;
	for (j = i__ + 1; j <= i__4; ++j) {
/*<            if(cent_dist(np(i)).gt.cent_dist(np(j)))then >*/
	    if (cent_dist__[np[i__ - 1] - 1] > cent_dist__[np[j - 1] - 1]) {
/*<              maxtemp=np(i) >*/
		maxtemp = (doublereal) np[i__ - 1];
/*<              np(i)=np(j) >*/
		np[i__ - 1] = np[j - 1];
/*<              np(j)=maxtemp >*/
		np[j - 1] = (integer) maxtemp;
/*<            endif >*/
	    }
/*<          enddo >*/
	}
/*<        enddo >*/
    }
/*<        do i=1,atom_num1 >*/
    i__3 = atom_num1__;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          total_bond=avdistcent(np(i))*kx(np(i)) >*/
	total_bond__ = avdistcent[np[i__ - 1] - 1] * kx[np[i__ - 1] - 1];
/*<          if(np(i).le.a1)then >*/
	if (np[i__ - 1] <= a1) {
/*<            type1=1 >*/
	    type1 = 1;
/*<    >*/
	    s_wsle(&io___107);
	    do_lio(&c__5, &c__1, (char *)&cent_dist__[np[i__ - 1] - 1], (
		    ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&avdistcent[np[i__ - 1] - 1], (
		    ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&kx[np[i__ - 1] - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&total_bond__, (ftnlen)sizeof(
		    doublereal));
	    i__4 = atom_num1__ - a1;
	    do_lio(&c__3, &c__1, (char *)&i__4, (ftnlen)sizeof(integer));
	    do_lio(&c__3, &c__1, (char *)&type1, (ftnlen)sizeof(integer));
	    e_wsle();
/*<          else >*/
	} else {
/*<            type1=2 >*/
	    type1 = 2;
/*<    >*/
	    s_wsle(&io___108);
	    do_lio(&c__5, &c__1, (char *)&cent_dist__[np[i__ - 1] - 1], (
		    ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&avdistcent[np[i__ - 1] - 1], (
		    ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__1, (char *)&kx[np[i__ - 1] - 1], (ftnlen)sizeof(
		    doublereal));
	    do_lio(&c__5, &c__1, (char *)&total_bond__, (ftnlen)sizeof(
		    doublereal));
	    i__4 = atom_num1__ - a1;
	    do_lio(&c__3, &c__1, (char *)&i__4, (ftnlen)sizeof(integer));
	    do_lio(&c__3, &c__1, (char *)&type1, (ftnlen)sizeof(integer));
	    e_wsle();
/*<          endif >*/
	}
/*<    >*/
	s_wsle(&io___109);
	do_lio(&c__5, &c__1, (char *)&cent_dist__[np[i__ - 1] - 1], (ftnlen)
		sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&avdistcent[np[i__ - 1] - 1], (ftnlen)
		sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&kx[np[i__ - 1] - 1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&total_bond__, (ftnlen)sizeof(doublereal)
		);
	i__4 = atom_num1__ - a1;
	do_lio(&c__3, &c__1, (char *)&i__4, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&type1, (ftnlen)sizeof(integer));
	e_wsle();
/*<        enddo >*/
    }
/*<        order_sum=0.D0 >*/
    order_sum__ = 0.;
/*<        write(*,*)order   >*/
    s_wsle(&io___111);
    do_lio(&c__5, &c__1, (char *)&order, (ftnlen)sizeof(doublereal));
    e_wsle();
/*<        do i=1,atom_num1-1 >*/
    i__3 = atom_num1__ - 1;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          do j=i+1,atom_num1 >*/
	i__4 = atom_num1__;
	for (j = i__ + 1; j <= i__4; ++j) {
/*<            order_sum=delta(i,j)+order_sum >*/
	    order_sum__ = delta[i__ + j * 1000 - 1001] + order_sum__;
/*<          enddo >*/
	}
/*<        enddo >*/
    }
/*<        do i=1,a1 >*/
    i__3 = a1;
    for (i__ = 1; i__ <= i__3; ++i__) {
/*<          do j=a1+1,atom_num1 >*/
	i__4 = atom_num1__;
	for (j = a1 + 1; j <= i__4; ++j) {
/*<            order=order+delta(i,j) >*/
	    order += delta[i__ + j * 1000 - 1001];
/*<          enddo >*/
	}
/*<        enddo >*/
    }
/*<        order=order/order_sum >*/
    order /= order_sum__;
/*<        open(31,file="0038CA"//quick_name//".ord",status="replace") >*/
    o__1.oerr = 0;
    o__1.ounit = 31;
    o__1.ofnmlen = 12;
/* Writing concatenation */
    i__2[0] = 6, a__2[0] = "0038CA";
    i__2[1] = 2, a__2[1] = quick_name__;
    i__2[2] = 4, a__2[2] = ".ord";
    s_cat(ch__4, a__2, i__2, &c__3, (ftnlen)12);
    o__1.ofnm = ch__4;
    o__1.orl = 0;
    o__1.osta = "replace";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/*<        write(31,*) 38-num,order,RA,RB,RAB,RA_total,RB_total,RAB_total >*/
    s_wsle(&io___113);
    i__3 = 38 - num;
    do_lio(&c__3, &c__1, (char *)&i__3, (ftnlen)sizeof(integer));
    do_lio(&c__5, &c__1, (char *)&order, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&ra, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rb, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rab, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&ra_total__, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rb_total__, (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&rab_total__, (ftnlen)sizeof(doublereal));
    e_wsle();
/*<        close(31) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 31;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<        close(11) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 11;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<        close(12) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 12;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<        close(13) >*/
    cl__1.cerr = 0;
    cl__1.cunit = 13;
    cl__1.csta = 0;
    f_clos(&cl__1);
/*<    >*/
    s_wsle(&io___114);
    do_lio(&c__9, &c__1, "The Output file is ", (ftnlen)19);
    do_lio(&c__9, &c__1, name_dist1__, (ftnlen)8);
    do_lio(&c__9, &c__1, ".dist , ", (ftnlen)8);
    do_lio(&c__9, &c__1, name_dist1__, (ftnlen)8);
    do_lio(&c__9, &c__1, ".txt", (ftnlen)4);
    e_wsle();
/*<        write(*,*) name_dist1,"a.txt and ",name_dist1,"b.txt" >*/
    s_wsle(&io___115);
    do_lio(&c__9, &c__1, name_dist1__, (ftnlen)8);
    do_lio(&c__9, &c__1, "a.txt and ", (ftnlen)10);
    do_lio(&c__9, &c__1, name_dist1__, (ftnlen)8);
    do_lio(&c__9, &c__1, "b.txt", (ftnlen)5);
    e_wsle();
/*<        write(*,*) "The *.ist file can be use to study the internal"  >*/
    s_wsle(&io___116);
    do_lio(&c__9, &c__1, "The *.ist file can be use to study the internal", (
	    ftnlen)47);
    e_wsle();
/*<        write(*,*) "strain by xmgr,x is the distance from center of mass" >*/
    s_wsle(&io___117);
    do_lio(&c__9, &c__1, "strain by xmgr,x is the distance from center of ma\
ss", (ftnlen)52);
    e_wsle();
/*<        write(*,*) "y is the average bond distance per atom" >*/
    s_wsle(&io___118);
    do_lio(&c__9, &c__1, "y is the average bond distance per atom", (ftnlen)
	    39);
    e_wsle();
/*<        write(*,*) "COMPLETED!!" >*/
    s_wsle(&io___119);
    do_lio(&c__9, &c__1, "COMPLETED!!", (ftnlen)11);
    e_wsle();
/*<        stop >*/
    s_stop("", (ftnlen)0);
/*<        end >*/
    return 0;
} /* MAIN__ */

/* Main program alias */ int main_ () { MAIN__ (); return 0; }
#ifdef __cplusplus
	}
#endif
