PROGRAM mk_monthly_mean
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM mk_monthly_mean  ***
  !!
  !!  Purpose : take a forcing file as input ( 3-hourly, 6-hourly or daily )
  !!            and return a file with monthly mean value.
  !!
  !!  Method : Determine the time step for this file
  !!           Determine the number of days in the file
  !!           Determine the number of step per day
  !!           Set the number of days per month, leap year permitting
  !!           compute the mean for every month
  !!           Write the monthly mean on the output file
  !!
  !!  history: Original code : J.M. Molines February, 28, 2013 (From decumul.f90)
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 784 $
  !! $Date: 2016-02-19 16:22:07 +0100 (Fri, 19 Feb 2016) $
  !! $Id: mk_monthly_mean.f90 784 2016-02-19 15:22:07Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: ji, jj, jt, jdays, jmon !: some loop index
  INTEGER :: npi, npj, npt           !: dimension of the fields
  INTEGER :: ndays, nperday          !: number of days in year, number of data per day
  INTEGER :: iargc, narg             !: command line variables
  INTEGER :: ipos, ipos2, natt       !: file name management and attributes
  INTEGER :: ifrst, ioffst           !: index of the first value of the current day, offset
  INTEGER ,DIMENSION(:), ALLOCATABLE :: nd_month   !: number of day per month

  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: value1   ! 3rd dimension is 8 
  REAL(KIND=8), DIMENSION(:)    , ALLOCATABLE :: d_time, d_rlon, d_rlat, d_timeout
  REAL(KIND=8), DIMENSION(:,:)  , ALLOCATABLE :: d_value2

  CHARACTER(LEN=80) :: cfilename  !: name of input file
  CHARACTER(LEN=80) :: cfileout   !: name of output file
  CHARACTER(LEN=80) :: catt       !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable
  INTEGER :: idv1,idvx1, idvy1, idvt1 !: ncdf id for variable

  CHARACTER(LEN=80) :: cvar           !: to hold variable name

  LOGICAL :: ln_3hour = .false.
  LOGICAL :: ln_6hour = .false.
  LOGICAL :: ln_daily = .false.
  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : mk_monthly_mean filename variable_name'
     PRINT *, '    filename : name of the ncdf input file with either 3-hourly, 6-hourly '
     PRINT *,'       or daily data. '
     PRINT *, '    variable_name : name of the variable to be processed in the file '
     PRINT *, '    output file name will  ''monthlymean'' appended to original file name.'
     STOP 
  ENDIF

  CALL getarg(1, cfilename)
  CALL getarg(2, cvar     )

  ! 2) change append monthly to FC or AN
  cfileout=TRIM(cfilename)//'_monthlymean'


  ! 3) Open input file
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! The file does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Allocate working space
  !  4.1  Look for dimensions ( lon or lon0, lat or lat0, time or time_counter
  istatus=NF90_INQ_DIMID(ncid,'lon' ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  IF ( istatus /= NF90_NOERR ) THEN
    istatus=NF90_INQ_DIMID(ncid,'lon0' ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  ENDIF
  IF ( istatus /= NF90_NOERR ) THEN
    istatus=NF90_INQ_DIMID(ncid,'longitude' ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  ENDIF
  istatus=NF90_INQ_DIMID(ncid,'lat' ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  IF ( istatus /= NF90_NOERR ) THEN
    istatus=NF90_INQ_DIMID(ncid,'lat0' ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  ENDIF
  IF ( istatus /= NF90_NOERR ) THEN
    istatus=NF90_INQ_DIMID(ncid,'latitude' ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  ENDIF
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)
  IF ( istatus /= NF90_NOERR ) THEN
    istatus=NF90_INQ_DIMID(ncid,'time_counter',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)
  ENDIF

  !  4.2 infer type of file 3-hourly ,6-hourly or daily 
  SELECT CASE ( npt )
  CASE ( 2920, 2928 ) ; ln_3hour=.true. ; nperday = 8
  CASE ( 1460, 1464 ) ; ln_6hour=.true. ; nperday = 4
  CASE ( 365,  366  ) ; ln_daily=.true. ; nperday = 1
  CASE DEFAULT 
    PRINT *, TRIM(cfilename),'  is neither 3-hourly nor daily  file ' ; STOP
  END SELECT
  ndays=npt/nperday

  ! 4.3   allocate space
  ALLOCATE (d_time(npt), value1( npi,npj,nperday), d_value2( npi,npj), d_rlon(npi), d_rlat(npj) )
  ALLOCATE (nd_month(12), d_timeout(12) )

  ! 4.4   set up number of days per month
  nd_month=(/31,28,31,30,31,30,31,31,30,31,30,31/)
  IF ( ndays == 366 ) nd_month(2) = 29 

  ! 5) Proceed with invariant data
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time',idvt1) ; istatus=NF90_GET_VAR(ncid,idvt1,d_time)
  IF ( istatus /= NF90_NOERR ) THEN
     istatus=NF90_INQ_VARID(ncid,'time_counter',idvt1) ; istatus=NF90_GET_VAR(ncid,idvt1,d_time)
  ENDIF
  istatus=NF90_INQ_VARID(ncid,'lon' ,idvx1) ; istatus=NF90_GET_VAR(ncid,idvx1,d_rlon)
  IF ( istatus /= NF90_NOERR ) THEN
     istatus=NF90_INQ_VARID(ncid,'lon0' ,idvx1) ; istatus=NF90_GET_VAR(ncid,idvx1,d_rlon)
  ENDIF
  IF ( istatus /= NF90_NOERR ) THEN
     istatus=NF90_INQ_VARID(ncid,'longitude' ,idvx1) ; istatus=NF90_GET_VAR(ncid,idvx1,d_rlon)
  ENDIF
  istatus=NF90_INQ_VARID(ncid,'lat' ,idvy1) ; istatus=NF90_GET_VAR(ncid,idvy1,d_rlat)
  IF ( istatus /= NF90_NOERR ) THEN
     istatus=NF90_INQ_VARID(ncid,'lat0' ,idvy1) ; istatus=NF90_GET_VAR(ncid,idvy1,d_rlat)
  ENDIF
  IF ( istatus /= NF90_NOERR ) THEN
     istatus=NF90_INQ_VARID(ncid,'latitude' ,idvy1) ; istatus=NF90_GET_VAR(ncid,idvy1,d_rlat)
  ENDIF

  !  5.2 Look for var id for the working variable
  istatus=NF90_INQ_VARID(ncid,cvar,idv1)

  ! 6) Create output file

  ! 6.1 : create file
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)
  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'lon', npi,idx)
  istatus=NF90_DEF_DIM(ncout,'lat', npj,idy)
  istatus=NF90_DEF_DIM(ncout,'time',NF90_UNLIMITED,idt)

  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'lon',NF90_DOUBLE,(/idx/),idvx)
  istatus=NF90_DEF_VAR(ncout,'lat',NF90_DOUBLE,(/idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time',NF90_DOUBLE,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idx,idy,idt/),idv)

  ! 6.3.3 copy attributes
  ! lon :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvx1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvx1,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvx1,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvy1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvy1,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvy1,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvt1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvt1,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvt1,catt,ncout,idvt)
  ENDDO
  ! field :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idv1,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idv1,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idv1,catt,ncout,idv)
  ENDDO
  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  ! 6.4 : write invariant variable
  d_timeout=(/1,2,3,4,5,6,7,8,9,10,11,12/)
  istatus=NF90_PUT_VAR(ncout,idvx,d_rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,d_rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,d_timeout)

  ! 7 ) Loop on month in the file
  ioffst=0
  DO jmon=1,12
    d_value2(:,:)=0.d0
    IF ( jmon >= 2 ) ioffst = ioffst + nd_month(jmon -1 )  ! offset in days
    DO jdays =1, nd_month(jmon)
       ifrst = (ioffst +  jdays-1 ) * nperday + 1
       !  7.2 read  8 values per day
       istatus=NF90_GET_VAR(ncid,idv1,value1, start=(/1,1,ifrst/), count=(/npi,npj,nperday/) )
       d_value2(:,:)=d_value2(:,:) + SUM(value1*1.d0, dim=3)/nperday   ! daily mean at this level.
    ENDDO
    d_value2(:,:) = d_value2(:,:)/ nd_month(jmon)
    istatus=NF90_PUT_VAR(ncout,idv,d_value2(:,:), start=(/1,1,jmon/), count=(/npi,npj,1/) )
  ENDDO

  ! 8) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)

END PROGRAM mk_monthly_mean

