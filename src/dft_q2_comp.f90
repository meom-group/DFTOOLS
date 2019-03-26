PROGRAM dft_q2_comp
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM dft_q2_comp  ***
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
  USE netcdf

  IMPLICIT NONE

  INTEGER :: ji,  jt     !: some loop index
  INTEGER :: npi, npj    !: dimension of the fields
  INTEGER :: nptD, nptP
  INTEGER :: iargc, narg , ijarg   !: command line variables
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

  CHARACTER(LEN=80) :: cv_q2           !: to hold variable name
  CHARACTER(LEN=80) :: cv_d2 ='d2'     !: to hold variable name
  CHARACTER(LEN=80) :: cv_msl='MSL'    !: to hold variable name
  CHARACTER(LEN=80) :: cv_lon='lon'
  CHARACTER(LEN=80) :: cv_lat='lat'
  CHARACTER(LEN=80) :: cv_time='time'

  CHARACTER(LEN=80) :: c_dimlon='lon'
  CHARACTER(LEN=80) :: c_dimlat='lat'
  CHARACTER(LEN=80) :: c_dimtim='time'
  CHARACTER(LEN=80) :: cdum            !: dummy character variable

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg == 0  ) THEN
     PRINT *,''
     PRINT *,' USAGE : dft_q2_comp -d2 DEWPOINT-file -msl MSL-file [-d2v D2-var]'
     PRINT *,'         [-mslv MSL-var] [-o Q2-file ] [-lon DIM-lon] [-lat DIM-lat]'
     PRINT *,'         [-time DIM-time]'
     PRINT *,''
     PRINT *,'   PURPOSE: '
     PRINT *,'     Compute humidity at 2m (q2) from the dewpoint temperature (d2) and ' 
     PRINT *,'     corresponding mean sea level pressure (msl).'
     PRINT *,'' 
     PRINT *,'   ARGUMENTS:'
     PRINT *, '      -d2 DEWPOINT-file : name of dewpoint temperature file'
     PRINT *, '      -msl MSL-file     : name of mean sea level pressure file'
     PRINT *, ''
     PRINT *,'   OPTIONS:'
     PRINT *,'      -d2v D2-var   : give name of dewpoint variable [default: d2]'
     PRINT *,'      -mslv MSL-var : give name of mean sea level pressure variable.'
     PRINT *,'                               [default: msl]'
     PRINT *,'      -o Q2-file    : give name of the output file.'
     PRINT *,'                               [default deduced from d2 file]'
     PRINT *,'      -lon DIM-lon  : give name of longitude dimension in the input files. '
     PRINT *,'                              [default: ',TRIM(c_dimlon),' ]'
     PRINT *,'      -lat DIM-lat  : give name of latitude dimension in the input files. '
     PRINT *,'                              [default: ',TRIM(c_dimlat),' ]'
     PRINT *,'      -time DIM-time : give name of time dimension in the input files. '
     PRINT *,'                              [default: ',TRIM(c_dimtim),' ]'
     PRINT *,''
     PRINT *,'   OUTPUT:'
     PRINT *,'     netcdf file Q2-file (like DEWPOINT-file, with d2 changed to q2)'
     PRINT *,'            variable :  q2 '
     PRINT *,'            units    :  kg/kg '
     PRINT *,''
     STOP 
  ENDIF

  ijarg = 1
  cfileout='none'
  DO WHILE ( ijarg <= narg ) 
     CALL getarg (ijarg, cdum) ; ijarg=ijarg+1
     SELECT CASE ( cdum)
     CASE ( '-d2'  ) ; CALL getarg(ijarg,cfilenameD) ; ijarg=ijarg+1
     CASE ( '-msl' ) ; CALL getarg(ijarg,cfilenameP) ; ijarg=ijarg+1
 
     CASE ( '-d2v' ) ; CALL getarg(ijarg,cv_d2     ) ; ijarg=ijarg+1
     CASE ( '-mslv') ; CALL getarg(ijarg,cv_msl    ) ; ijarg=ijarg+1
     CASE ( '-o'   ) ; CALL getarg(ijarg,cfileout  ) ; ijarg=ijarg+1

     CASE ( '-lon' ) ; CALL getarg(ijarg,c_dimlon ) ; ijarg=ijarg+1
     CASE ( '-lat' ) ; CALL getarg(ijarg,c_dimlat ) ; ijarg=ijarg+1
     CASE ( '-time') ; CALL getarg(ijarg,c_dimtim ) ; ijarg=ijarg+1
     END SELECT
  ENDDO

  IF ( cfileout == 'none' ) THEN !  not set in the options infer from d2 file
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
  ENDIF

  ! 2.3 variable name ...
  cv_q2='q2'
  
  ! 3) Open input files
  istatus= NF90_OPEN(cfilenameD,NF90_NOWRITE,ncidD) ! open read-only
  print *,NF90_STRERROR(istatus)
  istatus= NF90_OPEN(cfilenameP,NF90_NOWRITE,ncidP) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncidD,c_dimlon ,idx ) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncidD,c_dimlat ,idy ) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncidD,c_dimtim, idtD) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idtD,len=nptD)
  print *,NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidP,c_dimtim, idtP) ; istatus=NF90_INQUIRE_DIMENSION(ncidP,idtP,len=nptP)

  print *, nptD, nptP 
  IF ( nptD /= nptP ) THEN
     PRINT *, 'not the same number of time in d2 and msl  Sorry :( '
     STOP
  ENDIF


  ! 4.2 file OK allocate space
  ALLOCATE (time(nptD), D2value( npi,npj), patm( npi,npj), Q2value( npi,npj), rlon(npi), rlat(npj) )
  ALLOCATE (psat( npi,npj))
  
  ! 5) Read data and compute Q2
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncidD,cv_time,idvt1) ; istatus=NF90_GET_VAR(ncidD,idvt1,time)
  istatus=NF90_INQ_VARID(ncidD,cv_lon ,idvx1) ; istatus=NF90_GET_VAR(ncidD,idvx1,rlon)
  istatus=NF90_INQ_VARID(ncidD,cv_lat ,idvy1) ; istatus=NF90_GET_VAR(ncidD,idvy1,rlat)

  istatus=NF90_INQ_VARID(ncidD,cv_d2 ,idvD)
  istatus=NF90_INQ_VARID(ncidP,cv_msl,idvM)

  ! prepare the output file
  ! 6) write output file
  ! 6.1 create output data set
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)
  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,c_dimlon,npi,           idx)
  istatus=NF90_DEF_DIM(ncout,c_dimlat,npj,           idy)
  istatus=NF90_DEF_DIM(ncout,c_dimtim,NF90_UNLIMITED,idt)
  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,cv_lon ,NF90_FLOAT,(/idx/),idvx)
  istatus=NF90_DEF_VAR(ncout,cv_lat ,NF90_FLOAT,(/idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,cv_time,NF90_FLOAT,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cv_q2,NF90_FLOAT,(/idx,idy,idt/),idv)

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
  catt="computed from d2 and msl with dft_q2_comp" ; istatus=NF90_PUT_ATT(ncout,NF90_GLOBAL,'history',catt)
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
END PROGRAM dft_q2_comp

