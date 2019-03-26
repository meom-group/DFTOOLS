PROGRAM decumul
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM decumul  ***
  !!
  !!  Purpose : take a ERAinterim FC file as input ( where variables are cumulated)
  !!            and return a file with daily mean value.
  !!
  !!  Method : Cumulated file normaly hold 8 time steps ( 3 6 9 12 15 18 21 24 h)
  !!           values at 3 6 9 12 are cumulated since 0h, values at 15 18 21 24 are
  !!           cumulated since 12h. 
  !!           (1) take the file name as input and look for the variable name
  !!           (2) open the file and check that there are 8 frames in it
  !!           (3) read field at 12h then add it to frame at 24h
  !!           (4) divide the resulting 24h cumulated values by the number of seconds per day
  !!           (5) write the resultinf daily mean value on the output file
  !!
  !!  history: Original code : J.M. Molines September, 21 2009
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 304 $
  !! $Date: 2011-09-15 10:20:24 +0200 (Thu, 15 Sep 2011) $
  !! $Id: decumul.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npt  !: dimension of the fields
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, ipos2, natt

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

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 1 ) THEN
     PRINT *, ' USAGE : decumul filename'
     PRINT *, '    filename : name of the ncdf input file to be de-cumulated '
     PRINT *, '    output file name will  have FC (or AN) changed to FCdaily (resp. ANdaily) '
     STOP 
  ENDIF

  CALL getarg(1, cfilename)

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

  ! 2.3 try to guess variable name from cfilename ...
  ipos2=INDEX(cfilename,'.nc')
  cvar=cfilename(ipos+3:ipos2-1)
  PRINT *, TRIM(cvar)


  ! 3) Open input file
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! The file does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions
  istatus=NF90_INQ_DIMID(ncid,'lon',idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat',idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  ! 4.1 check if there are 8 time frames in the file ...
  IF ( npt /= 8 ) THEN
     PRINT *, 'for decumul, input file are supposed to hold 8 time frames'
     PRINT *, ' This file only have ', npt, 'time frames ...'
     PRINT *, ' Sorry :( '
     STOP
  ENDIF

  ! 4.2 file OK allocate space
  ALLOCATE (time(npt), value1( npi,npj), value2( npi,npj), rlon(npi), rlat(npj) )

  ! 5) Read data and compute daily mean
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time',idv) ; istatus=NF90_GET_VAR(ncid,idv,time)
  ! check that time(4)=12 and time(8)=24 ...
  IF ( time(4) /= 12 .OR. time(8) /= 24 ) THEN
     PRINT *, ' incoherent time in this file .... sorry :( '
     STOP
  ENDIF
  istatus=NF90_INQ_VARID(ncid,'lon',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlon)
  istatus=NF90_INQ_VARID(ncid,'lat',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlat)

  !  5.2 read values at 12h ( record 4 )
  istatus=NF90_INQ_VARID(ncid,cvar,idv)
  istatus=NF90_GET_VAR(ncid,idv,value1, start=(/1,1,4/), count=(/npi,npj,1/) )

  !  5.3 read values at 24h (record 8 )
  istatus=NF90_GET_VAR(ncid,idv,value2, start=(/1,1,8/), count=(/npi,npj,1/) )

  !  5.4 cumulate both values
  value1(:,:) = value1(:,:) + value2(:,:)

  !  5.5 computes daily mean :
  value1(:,:)=value1(:,:)/ 86400.  ! seconds in a day

  ! 6) write output file
  ! 6.1 create output data set
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

  ! 6.3.2: mean value
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

  !6.4 : write variables
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time(4))

  istatus=NF90_PUT_VAR(ncout,idv,value1(:,:))

  ! 7) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)

END PROGRAM decumul

