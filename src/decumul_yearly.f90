PROGRAM decumul_yearly
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM decumul_yearly  ***
  !!
  !!  Purpose : take a ERAinterim FC file as input ( where variables are cumulated)
  !!            and return a file with daily mean value.
  !!
  !!  Method : Cumulated file normaly hold 8 time steps per day( 3 6 9 12 15 18 21 24 h)
  !!           values at 3 6 9 12 are cumulated since 0h, values at 15 18 21 24 are
  !!           cumulated since 12h. 
  !!           (1) take the file name as input and look for the variable name
  !!           (2) open the file and check that there are 8 frames in it
  !!           (3) read field at 12h then add it to frame at 24h
  !!           (4) divide the resulting 24h cumulated values by the number of seconds per day
  !!           (5) write the resulting daily mean value on the output file
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
  INTEGER :: iargc, narg           !: command line variables
  INTEGER :: ipos, ipos2, natt
  INTEGER :: inoon, inight         !: index at 12h and 24 h in the current day

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: value1, value2

  CHARACTER(LEN=80) :: cfilename  !: name of input file
  CHARACTER(LEN=80) :: cfileout   !: name of output file
  CHARACTER(LEN=80) :: catt       !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable

  CHARACTER(LEN=80) :: cvar           !: to hold variable name
  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : decumul_yearly filename variable_name'
     PRINT *, '    filename : name of the ncdf input file to be de-cumulated '
     PRINT *, '    output file name will  have FC (or AN) changed to FCdaily (resp. ANdaily) '
     STOP 
  ENDIF

  CALL getarg(1, cfilename)
  CALL getarg(2, cvar     )

  ! 2) build output file name
  ! 2.1 look if the filename contains FC or AN
  ipos=INDEX(cfilename,'FC')
  IF ( ipos == 0 ) THEN
     ! Look for 'AN'
     ipos=INDEX(cfilename,'AN')
     IF ( ipos == 0 ) THEN
        ! weird ! neither an FC nor AN file
        PRINT *,' Error: input file must be either a FC or AN file '
        STOP
     ENDIF
  ENDIF
  ! 2.2 change append daily to FC or AN
  cfileout=cfilename(1:ipos+1)//'daily'//cfilename(ipos+2:)


  ! 3) Open input file
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! The file does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions
  istatus=NF90_INQ_DIMID(ncid,'lon' ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat' ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  npdays=npt/8   ! number of days in the file, assuming time step of 3 h (FC file)

  ! 4.1  allocate space
  ALLOCATE (time(npt), value1( npi,npj), value2( npi,npj), rlon(npi), rlat(npj) )

  ! 5) Read invariant data
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time',idv) ; istatus=NF90_GET_VAR(ncid,idv,time)
  istatus=NF90_INQ_VARID(ncid,'lon',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlon)
  istatus=NF90_INQ_VARID(ncid,'lat',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlat)

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
  ! correct the unit attribute for field (erase tailing "s " )
  istatus=NF90_GET_ATT(ncid,idv,'units',catt)
  ipos=LEN(TRIM(catt)) 
  catt=catt(1:ipos-2)
  istatus=NF90_PUT_ATT(ncout,idv,'units',catt)

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

END PROGRAM decumul_yearly

