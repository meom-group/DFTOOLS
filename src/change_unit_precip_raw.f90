PROGRAM change_unit_precip_raw 
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM change_unit_precip_raw  ***
  !!
  !!  Purpose : take ERAinterim precip files as input (in m/s) 
  !!            and return a file with precip values in mm/s aka kg/m2/s
  !!
  !!  Method : (1) take the file name as input to make output filename
  !!           (2) open the files and check that ther is 12 frames (12 months) in it
  !!           (3) read precip field and multiply by 1000
  !!           (5) write the resulting value on the output file
  !!
  !!  history: Original code : J.M. Molines September, 21 2009
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 629 $
  !! $Date: 2013-08-26 10:06:09 +0200 (Mon, 26 Aug 2013) $
  !! $Id: change_unit_precip_raw.f90 629 2013-08-26 08:06:09Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npt  !: dimension of the fields
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(KIND=8), DIMENSION(:),   ALLOCATABLE :: time
  REAL(KIND=8), DIMENSION(:),   ALLOCATABLE :: rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: precipvalue, precipunitvalue

  CHARACTER(LEN=80) :: cfilename  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout    !: name of output file
  CHARACTER(LEN=80) :: catt        !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable
  INTEGER :: idvx1, idvy1, idvt1      !: ncdf id for variable
  INTEGER :: idvin, idvout            !:  

  CHARACTER(LEN=80) :: cvar           !: to hold variable name
  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : change_unit_precip_raw filename  variable_name'
     PRINT *, '    filename : name of the ncdf input file containing the precip value'
     PRINT *, '    variable_name : name of the precip in the file ( can be snow too ! )'
     PRINT *, '    output file name will  have the variable name changed punit'
     STOP 
  ENDIF

  CALL getarg(1, cfilename)
  CALL getarg(2, cvar     )

  ! 2) build output file name
  ! 2.1 look if the filename contain precip
  ipos=INDEX(cfilename,'precip')
  IF ( ipos == 0 ) THEN  ! look for snow ...
     ipos=INDEX(cfilename,'snow')
     IF ( ipos == 0 ) THEN  
        ! weird ! not a precip nor snow file
        PRINT *,' Error: input file must be precip/snow  file '
        STOP
     ENDIF
  ENDIF

  ! 2.2 change append precip to punit
  cfileout=cfilename(1:ipos-1)//'punit'//cfilename(ipos+6:)

  ! 3) Open input files
  istatus= NF90_OPEN(cfilename,NF90_NOWRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Allocate working space
  !  4.1 : Look for dimensions
  istatus=NF90_INQ_DIMID(ncid,'lon', idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat', idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  PRINT *, ' This file have ', npt, 'time frames ...'

  ! 4.2 allocate space
  ALLOCATE (time(npt), precipvalue( npi,npj), precipunitvalue( npi,npj), rlon(npi), rlat(npj) )

  ! 5) Read invariant data
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncid,'time',idvt1) ; istatus=NF90_GET_VAR(ncid,idvt1,time)
  istatus=NF90_INQ_VARID(ncid,'lon', idvx1) ; istatus=NF90_GET_VAR(ncid,idvx1,rlon)
  istatus=NF90_INQ_VARID(ncid,'lat', idvy1) ; istatus=NF90_GET_VAR(ncid,idvy1,rlat)

  !  5.2 Look for varid of the working variable
  istatus=NF90_INQ_VARID(ncid, cvar, idvin)
  IF ( istatus /= NF90_NOERR ) THEN
     PRINT *,' It seems that variable ',TRIM(cvar),' is not in the file ',TRIM(cfilename)
     STOP
  ENDIF

  ! 6 )Prepare the output file
  ! 6.1 create output data set
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)

  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'lon', npi,            idx)
  istatus=NF90_DEF_DIM(ncout,'lat', npj,            idy)
  istatus=NF90_DEF_DIM(ncout,'time',NF90_UNLIMITED, idt)

  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'lon', NF90_DOUBLE,(/idx/),idvx)
  istatus=NF90_DEF_VAR(ncout,'lat', NF90_DOUBLE,(/idy/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time',NF90_DOUBLE,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idx,idy,idt/),idvout)

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
  istatus=NF90_INQUIRE_VARIABLE(ncid,idvin,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idvin,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idvin,catt,ncout,idvout)
  ENDDO
  ! Unit for precip is already mm/s but NEMO better lokes kg/m2/s ...
  istatus=NF90_PUT_ATT(ncout,idvout,'units','kg/m2/s')

  !6.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  !6.5 : write invariant variables
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time)

  ! 7) Loop on time
  DO jt = 1 , npt 
     !  7.1 read precip values
     istatus=NF90_GET_VAR(ncid,idvin,precipvalue, start=(/1,1,jt/), count=(/npi,npj,1/) )

     !  7.2 change unit multiplying by 1000 where >0, reset to 0 if < 0
     WHERE (precipvalue >= 0 )
        precipunitvalue=precipvalue*1000
     ELSEWHERE
        precipunitvalue=0.
     END WHERE

     ! 7.2 write rescaled values
     istatus=NF90_PUT_VAR(ncout,idvout,precipunitvalue(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  ENDDO

  ! 8) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncid)

END PROGRAM change_unit_precip_raw

