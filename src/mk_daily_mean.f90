PROGRAM mk_daily_mean
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM mk_daily_mean  ***
  !!
  !!  Purpose : take a ERAinterim FC file as input (with data every 3 h)
  !!            and return a file with daily mean value.
  !!
  !!  Method : Cumulated file normaly hold 8 time steps ( 3 6 9 12 15 18 21 24 h)
  !!           Determine the number of days in the file
  !!           Assume 8 values per day
  !!           Read data by block of 8 values
  !!           compute the mean of the 8 values
  !!           Write the daily mean on the output file
  !!
  !!  history: Original code : J.M. Molines February, 28, 2013 (From decumul.f90)
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 606 $
  !! $Date: 2013-03-01 10:52:13 +0100 (Fri, 01 Mar 2013) $
  !! $Id: mk_daily_mean.f90 606 2013-03-01 09:52:13Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: ji, jj, jt, jdays     !: some loop index
  INTEGER :: npi, npj, npt, npdays !: dimension of the fields
  INTEGER :: iargc, narg           !: command line variables
  INTEGER :: ipos, ipos2, natt     !: file name management and attributes
  INTEGER :: ifrst                 !: index of the first value of the current day

  REAL(KIND=8), DIMENSION(:)    , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=4), DIMENSION(:,:)  , ALLOCATABLE :: value2
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: value1   ! 3rd dimension is 8 

  CHARACTER(LEN=80) :: cfilename  !: name of input file
  CHARACTER(LEN=80) :: cfileout   !: name of output file
  CHARACTER(LEN=80) :: catt       !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable
  INTEGER :: idv1,idvx1, idvy1, idvt1 !: ncdf id for variable

  CHARACTER(LEN=80) :: cvar           !: to hold variable name
  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : mk_daily_mean filename variable_name'
     PRINT *, '    filename : name of the ncdf input file with 3 h data '
     PRINT *, '    variable_name : name of the variable to be processed in the file '
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
  cfileout=cfilename(1:ipos+1)//'dailymean'//cfilename(ipos+2:)


  ! 3) Open input file
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! The file does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Allocate working space
  !  4.1  Look for dimensions
  istatus=NF90_INQ_DIMID(ncid,'lon' ,idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat' ,idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  npdays=npt/8    ! number of days in the file

  ! 4.2   allocate space
  ALLOCATE (time(npt), value1( npi,npj,8), value2( npi,npj), rlon(npi), rlat(npj) )

  ! 5) Proceed with invariant data
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time',idvt1) ; istatus=NF90_GET_VAR(ncid,idvt1,time)
  istatus=NF90_INQ_VARID(ncid,'lon' ,idvx1) ; istatus=NF90_GET_VAR(ncid,idvx1,rlon)
  istatus=NF90_INQ_VARID(ncid,'lat' ,idvy1) ; istatus=NF90_GET_VAR(ncid,idvy1,rlat)

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
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)

  ! 7 ) Loop on days in the file
  DO jdays=1,npdays
     !  7.1 : index of the first value in the current day
     ifrst  =  (jdays -1 ) * 8 + 1

     !  7.2 read  8 values per day
     istatus=NF90_GET_VAR(ncid,idv1,value1, start=(/1,1,ifrst/), count=(/npi,npj,8/) )

     !  7.3 cumulate 8 values, on the time dimension
     value2(:,:) = SUM(value1, dim=3)/8.

     !  7.4 : write variables (time and variable
     istatus=NF90_PUT_VAR(ncout,idvt,time(ifrst+3:ifrst+3),start=(/jdays/), count=(/1/) )
     istatus=NF90_PUT_VAR(ncout,idv,value2(:,:), start=(/1,1,jdays/), count=(/npi,npj,1/) )

  ENDDO

  ! 8) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)

END PROGRAM mk_daily_mean

