c-----------------------------------------------------------------------
      subroutine uservp (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
C     
      udiff =0.
      utrans=0.

      return
      end


c-----------------------------------------------------------------------
      subroutine userf  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
C     
      ffx = 0.0
      ffy = 0.0
      ffz = 0.0

      return
      end


c-----------------------------------------------------------------------
      subroutine userq  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
C     
      qvol   = 0.0
      source = 0.0

      return
      end


c-----------------------------------------------------------------------
      subroutine userchk
      include 'SIZE'  
      include 'TOTAL' 

      REAL DUMMY(lx1*ly1*lz1*lelt,3)
      REAL ERRX,ERRY
 
      LT = lx1*ly1*lz1*lelt
      NT = lx1*ly1*lz1*nelv

C     Vorticity field is saved down in the temperature field
      if (mod(istep,iostep).eq.0) then
         call comp_vort3(DUMMY(1,3),DUMMY(1,1),DUMMY(1,2),vx,vy,vz)
         call copy(t(1,1,1,1,1),DUMMY(1,3),LT)
      end if

C     Compute errors and print to file for plotting
      CALL ERRORS(ERRX,ERRY)

C     Print for plotting
      if (nid.eq.0) print *,'ErrXY',time,errx,erry

      return
      end


C-----------------------------------------------------------------------
      subroutine userbc (ix,iy,iz,iside,ieg)

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'
     
      real U0,delta

      IEL = GLLEL(IEG)

      

      if (cbu.eq.'o  '.and.x.gt.1.0) then
         U0 = 1.0 
         delta = 0.05 
         S0  = 0.5*(1-tanh(ux/U0/delta))  
         pa = -0.5*(ux*ux+uy*uy)*S0
         write(*,*) 'Blu  ', ux,uy,uz
         !write(*,*) 'Blaaaa', x,y,z
      else if (cbu.eq.'v  '.and.x.lt.1.0) then
         UX = 1.0
         UY = 0.0
         IF (IF3D) UZ = 0.0
      endif
      temp = 0.0
      return
      end

c-----------------------------------------------------------------------
      subroutine useric (ix,iy,iz,ieg)
C     
C     A single vortex is superimposed into an uniform flow at (x0,y0).
C     
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      A   = 16.0
      rad = 0.5
      x0  = 5.0
      y0  = 5.0
      xx = (x-x0)/rad
      yy = (y-y0)/rad

      ux = 1.0 - A*exp(-0.5*(xx**2+yy**2)) * yy/rad
      uy =       A*exp(-0.5*(xx**2+yy**2)) * xx/rad 
      IF (IF3D) uz = 0.0
      temp = 0.0
      return
      end


c-----------------------------------------------------------------------
      subroutine usrdat
      include 'SIZE'
      include 'TOTAL'
      return
      end


c-----------------------------------------------------------------------
      subroutine usrdat2
      include 'SIZE'
      include 'TOTAL'
C     ifto = .true.
C     param(66) = 4.            ! These give the std nek binary i/o and C	  are 
C     param(67) = 4.            ! good default values
      return
      end


c-----------------------------------------------------------------------
      subroutine usrdat3
      return
      end

c-----------------------------------------------------------------------
      subroutine errors(NORMVX,NORMVY)

      include 'SIZE'  
      include 'TOTAL'

      real NORMVX,NORMVY,NORMV
     $     ,VXE   (lx1,ly1,lz1,lelt)
     $     ,VYE   (lx1,ly1,lz1,lelt)
     $     ,ERRVX (lx1,ly1,lz1,lelt)
     $     ,ERRVY (lx1,ly1,lz1,lelt)
      integer NT

      NT = lx1*ly1*lz1*nelv

      A   = 16.0
      rad = 0.5
      x0  = 5.0 + TIME
      y0  = 5.0
      DO 11, I = 1,NT
         X = XM1(I,1,1,1)
         Y = YM1(I,1,1,1)
         xx = (x-x0)/rad
         yy = (y-y0)/rad
         VXE(I,1,1,1) = 1.0 - A*exp(-0.5*(XX**2+YY**2))*YY/RAD
         VYE(I,1,1,1) =       A*exp(-0.5*(XX**2+YY**2))*XX/RAD
 11   CONTINUE

      CALL SUB3(ERRVX,VXE,VX,NT) 
      CALL SUB3(ERRVY,VYE,VY,NT)

      NORMVX = GL2NORM(ERRVX,NT)
      NORMVY = GL2NORM(ERRVY,NT)

      return
      end
c
c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)
      return
      end
