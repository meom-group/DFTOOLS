PROGRAM mk_lsm_from_raw_file
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM mk_lsm_from_raw_file  ***
  !!
  !!  Purpose : take ERAinterim LSM file from the net ( 1 on land 0 on the ocean)
  !!            and build a lsm file suitable for oceanographers (1 on sea 0 on land)
  !!
  !!  Method : (1) take the file name as input to make output filename
  !!           (3) read lsm and set to 1 all value below or equal to the threshold
  !!           (5) write the ocean LSM
  !!
  !!  history: Original code : J.M. Molines February, 28 2013
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 607 $
  !! $Date: 2013-03-01 11:08:14 +0100 (Fri, 01 Mar 2013) $
  !! $Id: mk_lsm_from_rawfile.f90 607 2013-03-01 10:08:14Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: npi, npj
  INTEGER :: iargc, narg    !: command line variables

  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: LSMvalue, MVARvalue
  REAL(KIND=4)                              :: threshold


  CHARACTER(LEN=80) :: cfilenameLSM  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout      !: name of output file
  CHARACTER(LEN=80) :: cdum          !: dummy variable
  CHARACTER(LEN=80) :: cmd           !: command string
 
  ! netcdf stuff
  INTEGER :: ncout                            !: ncdf logical unit for input and output file
  INTEGER :: istatus, id
  !!----------------------------------------------------------------------------

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : mk_lsm_var filenameLSM treshold_value'
     PRINT *, '      filenameLSM : name of the ncdf input file containing the land-sea mask from web'
     PRINT *, '      treshhold value is the minimum value in the ocean'
     STOP 
  ENDIF

  CALL getarg(1, cfilenameLSM)
  CALL getarg(2, cdum) ; READ(cdum,*) threshold

  ! 2 ) Create output file by duplicating the input file
  cfileout='lsm_out.nc'
  ! copy input file to output file and work on the copy
  WRITE(cmd,'("cp ",a,1x,a)') TRIM(cfilenameLSM), TRIM(cfileout)

  PRINT *,' Executing ', TRIM(cmd)
  CALL system ( cmd)

  ! 3) Open output files
  istatus= NF90_OPEN(cfileout,NF90_WRITE,ncout) 
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Allocate working space
  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncout,'lon',id) ; istatus=NF90_INQUIRE_DIMENSION(ncout,id,len=npi)
  istatus=NF90_INQ_DIMID(ncout,'lat',id) ; istatus=NF90_INQUIRE_DIMENSION(ncout,id,len=npj)

  PRINT *,' NPI= ', npi
  PRINT *,' NPJ= ', npj
  
  ! 4.2 allocate space
  ALLOCATE (LSMvalue( npi,npj), MVARvalue( npi,npj))
  
  ! 5) build ocean mask
  istatus=NF90_INQ_VARID(ncout,'LSM',id)
  IF ( istatus /= NF90_NOERR) THEN 
     PRINT *, NF90_STRERROR(istatus) ,' in inq_varid for LSM'
  ENDIF
  istatus=NF90_GET_VAR(ncout,id,LSMvalue)
  IF ( istatus /= NF90_NOERR) THEN 
     PRINT *, NF90_STRERROR(istatus) ,' in get_var for LSM'
  ENDIF

  MVARvalue=0. ! set mask to 0 every where

  WHERE ( LSMvalue <= threshold  ) MVARvalue=1
  
  ! 6) write ocean mask and clse file
  istatus=NF90_PUT_VAR(ncout,id,MVARvalue)
  istatus=NF90_CLOSE(ncout)

END PROGRAM mk_lsm_from_raw_file

