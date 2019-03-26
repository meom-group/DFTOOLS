PROGRAM change_unit_precip_jmm
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM change_unit_precip_jmm  ***
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
  !! $Id: change_unit_precip_jmm.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npt  !: dimension of the fields
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time
  REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE ::  rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: precipvalue

  CHARACTER(LEN=80) :: cfilename  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout    !: name of output file
  CHARACTER(LEN=80) :: catt        !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt   !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt     !: ncdf id for variable
  INTEGER :: idvin                    !:  

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

  ! 2.3 variable name ...
  ! build variable name from filename supposed to be {varname}_ERAinterim_y{year}.nc
  ipos=INDEX(cfilename,'_')
  cvar=cfilename(1:ipos-1)
  
  PRINT *, TRIM(cvar)

  ! 3) Open input files
  istatus= NF90_OPEN(cfilename,NF90_WRITE,ncid) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncid,'lon',idx) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat',idy) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',idt) ; istatus=NF90_INQUIRE_DIMENSION(ncid,idt,len=npt)

  
  ! 4.1 check if there are 12 time frames in the file ...
     PRINT *, ' This file have ', npt, 'time frames ...'


  ! 4.2 file OK allocate space
  ALLOCATE (time(npt), precipvalue( npi,npj), rlon(npi,npj), rlat(npi,npj) )
  
  istatus=NF90_INQ_VARID(ncid,TRIM(cvar),idvin)
  istatus=NF90_GET_ATT(ncid,idvin,'units',catt)
  IF ( catt == 'kg/m2/s' ) THEN
   PRINT *, 'Unit already changed to kg/m2/s : nothing to do !'
  ELSE

  DO jt = 1 , npt 
  !  5.2 read precip values
  istatus=NF90_GET_VAR(ncid,idvin,precipvalue, start=(/1,1,jt/), count=(/npi,npj,1/) )
  !  5.3 change unit multiplying by 1000
  precipvalue(:,:)=precipvalue(:,:)*1000.
! precipvalue(:,:)=precipvalue(:,:)/1000.

  istatus=NF90_PUT_VAR(ncid,idvin,precipvalue(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  PRINT *,NF90_STRERROR(istatus)
  ENDDO
  istatus=NF90_REDEF(ncid)
  istatus=NF90_PUT_ATT(ncid,idvin,'units','kg/m2/s') 

  ENDIF
  ! 7) : close the files
  istatus=NF90_CLOSE(ncid)
END PROGRAM change_unit_precip_jmm

