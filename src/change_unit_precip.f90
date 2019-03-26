PROGRAM change_unit_precip 
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM change_unit_precip  ***
  !!
  !!  Purpose : take ERAinterim precip files as input (in m/s) 
  !!            and return a file with precip values in mm/s.
  !!
  !!  Method : (1) take the file name as input to make output filename
  !!           (2) open the files and check that ther is 12 frames (12 months) in it
  !!           (3) read precip field and divide by 1000
  !!           (5) write the resulting value on the output file
  !!
  !!  history: Original code : J.M. Molines September, 21 2009
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 304 $
  !! $Date: 2011-09-15 10:20:24 +0200 (Thu, 15 Sep 2011) $
  !! $Id: change_unit_precip.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npt  !: dimension of the fields
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE ::  rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: precipvalue, precipunitvalue

  CHARACTER(LEN=80) :: cfilename  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout    !: name of output file
  CHARACTER(LEN=80) :: catt        !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable
  INTEGER :: idvin, idvout            !:  

  CHARACTER(LEN=80) :: cvar           !: to hold variable name

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 1 ) THEN
     PRINT *, ' USAGE : change_unit_precip filename'
     PRINT *, ' filename : name of the ncdf input file containing the precip value'
     PRINT *, ' output file name will  have the variable name changed punit'
     STOP 
  ENDIF

  CALL getarg(1, cfilename)

  ! 2) build output file name
  ! 2.1 look if the filename contain precip
  ipos=INDEX(cfilename,'precip')
  IF ( ipos == 0 ) THEN
     ! weird ! not a precip file
     PRINT *,' Error: first input file must be precip file '
     STOP
  ENDIF
  ! 2.2 change append precip to punit
  cfileout=cfilename(1:ipos-1)//'punit'//cfilename(ipos+6:)

  ! 2.3 variable name ...
  cvar='precip'
  
  PRINT *, TRIM(cvar)


  ! 3) Open input files
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncid,'x',idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'y',idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time_counter',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  
  ! 4.1 check if there are 12 time frames in the file ...
     PRINT *, ' This file have ', npt, 'time frames ...'


  ! 4.2 file OK allocate space
  ALLOCATE (time(npt), precipvalue( npi,npj), precipunitvalue( npi,npj), rlon(npi,npj), rlat(npi,npj) )
  
  ! 5) Read data and compute Q2
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time_counter',idv) ; istatus=NF90_GET_VAR(ncid,idv,time)
  istatus=NF90_INQ_VARID(ncid,'nav_lon',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlon)
  istatus=NF90_INQ_VARID(ncid,'nav_lat',idv) ; istatus=NF90_GET_VAR(ncid,idv,rlat)

  istatus=NF90_INQ_VARID(ncid,'precip',idvin)
  ! prepare the output file
  ! 6) write output file
  ! 6.1 create output data set
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)
  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'x',npi,idx)
  istatus=NF90_DEF_DIM(ncout,'y',npj,idy)
  istatus=NF90_DEF_DIM(ncout,'time_counter',NF90_UNLIMITED,idt)
  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'nav_lon',NF90_DOUBLE,(/idx,idy/),idvx)
  istatus=NF90_DEF_VAR(ncout,'nav_lat',NF90_DOUBLE,(/idx,idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time_counter',NF90_DOUBLE,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idx,idy,idt/),idvout)

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
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvin,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvin,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvin,catt,ncout,idvout)
  ENDDO
  ! Unit for precip is already mm/s : do not change anything
  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  !6.4 : write variables
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time)

  DO jt = 1 , npt 
  !  5.2 read precip values
  istatus=NF90_GET_VAR(ncid,idvin,precipvalue, start=(/1,1,jt/), count=(/npi,npj,1/) )


  !  5.3 change unit multiplying by 1000
  precipunitvalue(:,:)=precipvalue(:,:)*1000

  istatus=NF90_PUT_VAR(ncout,idvout,precipunitvalue(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  ENDDO

  ! 7) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)
END PROGRAM change_unit_precip 

