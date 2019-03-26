PROGRAM calc_q2_msl
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM calc_q2_msl  ***
  !!
  !!  Purpose : take ERAinterim T2 D2 and MSL files as input 
  !!            and return a file with q2 values.
  !!
  !!  Method : (1) take the file name as input to make output filename
  !!           (2) open the files and check that ther is 12 frames (12 months) in it
  !!           (3) read T2  D2 and MSL fields and compute q2
  !!           (4) write the results in files
  !!
  !!  history: Original code : J.M. Molines Mqrch 2010 (from calc_q2, MM)
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 608 $
  !! $Date: 2013-03-01 15:27:00 +0100 (Fri, 01 Mar 2013) $
  !! $Id: calc_q2_msl.f90 608 2013-03-01 14:27:00Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji,  jt     !: some loop index
  INTEGER :: npi, npj    !: dimension of the fields
  INTEGER :: nptD, nptP
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(8), PARAMETER :: reps = 0.62197
  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: D2value,  Q2value, psat, HR
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: patm

  CHARACTER(LEN=80) :: cfilenameD  !: name of dewpoint input file
  CHARACTER(LEN=80) :: cfilenameP  !: name of MSL pressure
  CHARACTER(LEN=80) :: cfileout    !: name of output file
  CHARACTER(LEN=80) :: catt        !: attribute name
  ! netcdf stuff
  INTEGER :: ncidP, ncidD, ncout          !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt, idtD, idtP  !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt                !: ncdf id for variable
  INTEGER :: idvx1, idvy1, idvt1                 !: ncdf id for variable
  INTEGER :: idvD, idvM

  CHARACTER(LEN=80) :: cvar           !: to hold variable name

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : calc_q2_msl filenameD filenameP'
     PRINT *, ' filenameD : name of the ncdf input file containing the dewpoint value'
     PRINT *, ' filenameP : name of the ncdf input file containing the MSLP  value'
     PRINT *, ' output file name will  have the variable name changed to Q2'
     STOP 
  ENDIF

  CALL getarg(1, cfilenameD)
  CALL getarg(2, cfilenameP)

  ! 2) build output file name
  ! 2.1 look if the filenameD contain 2D
  ipos=INDEX(cfilenameD,'d2')
  IF ( ipos == 0 ) THEN
     ! weird ! not a dewpoint file
     PRINT *,' Error: first input file must be dewpoint file '
     STOP
  ENDIF
  ! 2.2 change append 2Q to 2D
  cfileout='q2'//cfilenameD(ipos+2:)

  ! 2.3 variable name ...
  cvar='q2'
  
  ! 3) Open input files
  istatus= NF90_OPEN(cfilenameD,NF90_NOWRITE,ncidD) ! open read-only
  istatus= NF90_OPEN(cfilenameP,NF90_NOWRITE,ncidP) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncidD,'lon',idx) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncidD,'lat',idy) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncidD,'time',idtD) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idtD,len=nptD)
  istatus=NF90_INQ_DIMID(ncidP,'time',idtP) ; istatus=NF90_INQUIRE_DIMENSION(ncidP,idtP,len=nptP)

  
  IF ( nptD /= nptP ) THEN
     PRINT *, 'not the same number of time in d2 and msl  Sorry :( '
     STOP
  ENDIF


  ! 4.2 file OK allocate space
  ALLOCATE (time(nptD), D2value( npi,npj), patm( npi,npj), Q2value( npi,npj), rlon(npi), rlat(npj) )
  ALLOCATE (psat( npi,npj))
  
  ! 5) Read data and compute Q2
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncidD,'time',idvt1) ; istatus=NF90_GET_VAR(ncidD,idvt1,time)
  istatus=NF90_INQ_VARID(ncidD,'lon',idvx1)  ; istatus=NF90_GET_VAR(ncidD,idvx1,rlon)
  istatus=NF90_INQ_VARID(ncidD,'lat',idvy1)  ; istatus=NF90_GET_VAR(ncidD,idvy1,rlat)

  istatus=NF90_INQ_VARID(ncidD,'D2M',idvD)
  istatus=NF90_INQ_VARID(ncidP,'MSL',idvM)

  ! prepare the output file
  ! 6) write output file
  ! 6.1 create output data set
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)
  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'lon',npi,idx)
  istatus=NF90_DEF_DIM(ncout,'lat',npj,idy)
  istatus=NF90_DEF_DIM(ncout,'time',NF90_UNLIMITED,idt)
  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'lon',NF90_FLOAT,(/idx/),idvx)
  istatus=NF90_DEF_VAR(ncout,'lat',NF90_FLOAT,(/idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time',NF90_FLOAT,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idx,idy,idt/),idv)

  ! 6.3.3 copy attributes
  ! lon :
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvx1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvx1,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvx1,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvy1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvy1,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvy1,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvt1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvt1,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvt1,catt,ncout,idvt)
  ENDDO
  ! field :
  catt="Specific humidity" ; istatus=NF90_PUT_ATT(ncout,idv,'long_name',catt)
  catt="kg/kg"             ; istatus=NF90_PUT_ATT(ncout,idv,'units',catt)
  catt="9999"              ; istatus=NF90_PUT_ATT(ncout,idv,'code',catt)
  catt="9999"              ; istatus=NF90_PUT_ATT(ncout,idv,'table',catt)
  catt="gaussian"          ; istatus=NF90_PUT_ATT(ncout,idv,'grid_type',catt)
  ! global attribute
  catt="computed from d2 and msl ERAinterim with calc_q2_msl" ; istatus=NF90_PUT_ATT(ncout,NF90_GLOBAL,'history',catt)
  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  !6.4 : write variables
! DO jt=1,nptD    ! working from ERAinterim file, time is already OK in the files
!  time(jt)=(jt-1)*3
! ENDDO
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time)

  DO jt = 1 , nptD 
     IF ( MOD(jt,10) == 0 ) print *, jt
  !  5.2 read dewpoint values
  istatus=NF90_GET_VAR(ncidD,idvD,D2value, start=(/1,1,jt/), count=(/npi,npj,1/) )

  !  5.3 read MSL values
  istatus=NF90_GET_VAR(ncidP,idvM,patm   , start=(/1,1,jt/), count=(/npi,npj,1/) )

  !  5.4 compute q2 (with intermediate values psat and HR)
  ! vapour pressure at saturation (Pa)
      psat = 100*( 10**(10.79574*(1 - 273.16/D2value) - 5.028*LOG10(D2value/273.16) &
         &       + 1.50475*10**(-4)*(1 - 10**(-8.2969*(D2value/273.16 - 1))) &
         &       + 0.42873*10**(-3)*(10**(4.76955*(1 - 273.16/D2value)) - 1) + 0.78614) )
      Q2value = reps*psat / ( patm - (1. - reps)*psat )

  istatus=NF90_PUT_VAR(ncout,idv,Q2value(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  ENDDO

  ! 7) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncidP)
  istatus=NF90_CLOSE(ncidD)
END PROGRAM calc_q2_msl

