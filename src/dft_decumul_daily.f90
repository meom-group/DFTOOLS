PROGRAM dft_decumul_daily
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM dft_decumul_daily  ***
  !!
  !!  Purpose : take a ERAinterim  input ( where variables are cumulated)
  !!            and return a file with daily mean value.
  !!
  !!  Method : Cumulated file normaly hold 8 time steps per day( 3 6 9 12 15 18 21 24 h)
  !!           values at 3 6 9 12 are cumulated since 0h, values at 15 18 21 24 are
  !!           cumulated since 12h. 
  !!           (1) take the file name as input and look for the variable name
  !!           (2) open the file and check that there are 8 frames in it
  !!           (3) read field at 12h then add it to frame at 24h
  !!           (4) divide the resulting 24h cumulated values by the number of seconds per day
  !!           (5) write the resulting daily mean rate (value/s)  on the output file
  !!
  !!  history: Original code : J.M. Molines September, 21 2009
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 630 $
  !! $Date: 2013-08-26 10:39:54 +0200 (Mon, 26 Aug 2013) $
  !! $Id: decumul_yearly.f90 630 2013-08-26 08:39:54Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: ji, jj, jt, jdays     !: some loop index
  INTEGER :: npi, npj, npt, npdays !: dimension of the fields
  INTEGER :: iargc, narg, ijarg    !: command line variables
  INTEGER :: ipos, ipos2, natt
  INTEGER :: inoon, inight         !: index at 12h and 24 h in the current day

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: value1, value2

  CHARACTER(LEN=80) :: cfilename  !: name of input file
  CHARACTER(LEN=80) :: cfileout   !: name of output file
  CHARACTER(LEN=80) :: catt       !: attribute name
  CHARACTER(LEN=80) :: cunit      !: output units
  CHARACTER(LEN=80) :: cvar       !: to hold variable name
  CHARACTER(LEN=80) :: cdum       !: dummy character variable

  CHARACTER(LEN=80) :: c_dimlon='lon'
  CHARACTER(LEN=80) :: c_dimlat='lat'
  CHARACTER(LEN=80) :: c_dimtim='time'

  CHARACTER(LEN=80) :: cv_lon='lon'
  CHARACTER(LEN=80) :: cv_lat='lat'
  CHARACTER(LEN=80) :: cv_tim='time'
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable

  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg == 0 ) THEN
     PRINT *, ' '
     PRINT *, ' USAGE : dft_decumul_daily  -f CUMUL-file -v CUMUL-var -u OUT-units '
     PRINT *, '         [-lon DIM-lon] [-lat DIM-lat] [-tim DIM-time]'
     PRINT *, ' '
     PRINT *, '   PURPOSE:'
     PRINT *, '       Process the 3-hourly cumulated files (e.g precip, snow, ssrd, strd)'
     PRINT *, '       in order to build an un-cumulated daily file (units are changed).'
     PRINT *, ' '
     PRINT *, '   METHOD:'
     PRINT *, '       For each day, in the 3-hourly ERAinterim files, value at 12h is the'
     PRINT *, '       cumulated values from 0h to 12h, and the value at 24h are the cumulated'
     PRINT *, '       values from 12h to 24h. Hence, the daily cumul is the sum of value at'
     PRINT *, '       12h + value at 24h. The daily mean rate is obtained by dividing this'
     PRINT *, '       sum by the number of seconds in a day (86400).'
     PRINT *, ' '
     PRINT *, '   WARNING :'
     PRINT *, '       Strong assumptions about the cumulating process are done for this code.'
     PRINT *, '       (see method above). Be sure that your data follow the same pattern.'
     PRINT *, ' '
     PRINT *, '   ARGUMENTS:'
     PRINT *, '       -f CUMUL-file : name of the yearly file with 3h cumulated values.'
     PRINT *, '       -v CUMUL-var : name of the variable to process in CUMUL-file.'
     PRINT *, '       -u OUT-units : units of the output field (rate).'
     PRINT *, ' '
     PRINT *, '   OPTIONS:'
     PRINT *, '       -lon DIM-lon ; give name of longitude dimension, '
     PRINT *, '                  default :[',TRIM(c_dimlon),']'
     PRINT *, '       -lon DIM-lat ; give name of latitude dimension, '
     PRINT *, '                  default :[',TRIM(c_dimlat),']'
     PRINT *, '       -tim DIM-time ; give name of time dimension, '
     PRINT *, '                  default :[',TRIM(c_dimtim),']'
     PRINT *, ' '
     PRINT *, '   OUTPUT:'
     PRINT *, '       netcdf file : <CUMUL_file>_daily '
     PRINT *, '       variable    : <CUMUL_var> '
     PRINT *, '       units       : Original units/ s'
     PRINT *, ' '
     STOP 
  ENDIF

  ijarg=1
  cunit='none'
  DO WHILE ( ijarg <= narg )
     CALL getarg(ijarg, cdum ) ; ijarg=ijarg+1
     SELECT CASE ( cdum )
     CASE ( '-f'   ) ; CALL getarg(ijarg,cfilename ) ;  ijarg=ijarg+1
     CASE ( '-v'   ) ; CALL getarg(ijarg,cvar      ) ;  ijarg=ijarg+1
     CASE ( '-u'   ) ; CALL getarg(ijarg,cunit     ) ;  ijarg=ijarg+1 
     ! options
     CASE ( '-lon' ) ; CALL getarg(ijarg,c_dimlon  ) ;  ijarg=ijarg+1
     CASE ( '-lat' ) ; CALL getarg(ijarg,c_dimlat  ) ;  ijarg=ijarg+1
     CASE ( '-tim' ) ; CALL getarg(ijarg,c_dimtim  ) ;  ijarg=ijarg+1
     CASE DEFAULT    ; PRINT *, ' UNKNOWN options : ',TRIM(cdum),' !' ; STOP 10
     END SELECT
  ENDDO

  IF ( cunit == 'none') THEN
      PRINT *, ' ERROR : you must specify a unit with -u option !'
      STOP 10
  ENDIF

  ! 2) build output file name
  cfileout=TRIM(cfilename)//'_daily'

  ! 3) Open input file
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! The file does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions
  istatus=NF90_INQ_DIMID(ncid,c_dimlon ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,c_dimlat ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,c_dimtim ,idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  npdays=npt/8   ! number of days in the file, assuming time step of 3 h (FC file)

  ! 4.1  allocate space
  ALLOCATE (time(npt), value1( npi,npj), value2( npi,npj), rlon(npi), rlat(npj) )

  ! 5) Read invariant data
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,cv_tim,idv) ; istatus=NF90_GET_VAR(ncid,idv,time)
  istatus=NF90_INQ_VARID(ncid,cv_lon,idv) ; istatus=NF90_GET_VAR(ncid,idv,rlon)
  istatus=NF90_INQ_VARID(ncid,cv_lat,idv) ; istatus=NF90_GET_VAR(ncid,idv,rlat)

  !  5.2 check varid for the input variable
  istatus=NF90_INQ_VARID(ncid,cvar,idv)

  ! 6) create output file
  !  6.1 creation
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)

  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'lon',npi,idx)
  istatus=NF90_DEF_DIM(ncout,'lat',npj,idy)
  istatus=NF90_DEF_DIM(ncout,'time',NF90_UNLIMITED,idt)

  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'lon',NF90_DOUBLE,(/idx/),idvx)
  istatus=NF90_DEF_VAR(ncout,'lat',NF90_DOUBLE,(/idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time',NF90_DOUBLE,(/idt/),idvt)

  ! 6.3.2: cumulated value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idx,idy,idt/),idv)

  ! 6.3.3 copy attributes
  ! lon :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvx,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvx,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvx,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvy,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvy,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvy,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvt,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvt,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvt,catt,ncout,idvt)
  ENDDO
  ! field :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idv,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idv,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idv,catt,ncout,idv)
  ENDDO
  ! set new unit
  istatus=NF90_PUT_ATT(ncout,idv,'units',cunit )

  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  ! 6.4 : save invariant variable
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)

  ! 7) Loop on days
  DO jdays=1,npdays
     inoon  =  (jdays -1 ) * 8 + 4   ! index of hour 12:00 in the current day
     inight =   jdays      * 8       ! index of hour 24:00 in the current day
     ! 7.1 : check that time(inoon)=12 and time(inight)=24 ...
     IF ( time(inoon) /= (jdays-1)*24 + 12 .OR. time(inight) /= jdays *24 ) THEN
        PRINT *, ' incoherent time in this file .... sorry :( '
        STOP
     ENDIF
     ! 7.2 read values at 12h ( record 4 )
     istatus=NF90_GET_VAR(ncid,idv,value1, start=(/1,1,inoon/), count=(/npi,npj,1/) )

     ! 7.3 read values at 24h (record 8 )
     istatus=NF90_GET_VAR(ncid,idv,value2, start=(/1,1,inight/), count=(/npi,npj,1/) )

     ! 7.4 cumulate both values
     value1(:,:) = value1(:,:) + value2(:,:)

     ! 7.5 computes daily mean :
     value1(:,:)=value1(:,:)/ 86400.  ! seconds in a day

     ! 7.6 : write variables
     istatus=NF90_PUT_VAR(ncout,idvt,time(inoon:inoon),start=(/jdays/), count=(/1/) )

     istatus=NF90_PUT_VAR(ncout,idv,value1(:,:), start=(/1,1,jdays/), count=(/npi,npj,1/) )

  ENDDO

  ! 8) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)

END PROGRAM dft_decumul_daily

