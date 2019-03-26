PROGRAM mk_lsm_var
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM mk_lsm_var  ***
  !!
  !!  Purpose : take ERAinterim variable file and ERAinterim land-sea mask as input 
  !!            and return a variable file masked.
  !!
  !!  Method : (1) take the file name as input to make output filename
  !!           (2) open the files and check that ther is 12 frames (12 months) in it
  !!           (3) read T2 and D2 fields and compute q2
  !!           (5) write the resultinf daily mean value on the output file
  !!
  !!  history: Original code : J.M. Molines September, 21 2009
  !!
  !!----------------------------------------------------------------------------
  !! $Rev: 304 $
  !! $Date: 2011-09-15 10:20:24 +0200 (Thu, 15 Sep 2011) $
  !! $Id: mk_lsm_var.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npiLSM, npjLSM, npt  !: dimension of the fields
  INTEGER :: nptT, nptVAR
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: VARvalue, LSMvalue, MVARvalue

  CHARACTER(LEN=80) :: cfilenameVAR  !: name of dewpoint input file
  CHARACTER(LEN=80) :: cfilenameLSM  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout      !: name of output file
  CHARACTER(LEN=80) :: catt          !: attribute name
  CHARACTER(LEN=80) :: cvar          !: variable name
 
  ! netcdf stuff
  INTEGER :: ncid, ncidVAR, ncidLSM, ncout                            !: ncdf logical unit for input and output file
  INTEGER :: istatus, idxVAR, idyVAR, idxLSM, idyLSM, idt, idtVAR     !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt                                     !: ncdf id for variable
  INTEGER :: idvVAR, idvLSM


  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : mk_lsm_var filenameVAR filenameLSM'
     PRINT *, ' filenameVAR : name of the ncdf input file containing the variable value'
     PRINT *, ' filenameLSM : name of the ncdf input file containing the land-sea mask value'
     PRINT *, ' output file name will  have the variable name changed to mVAR'
     STOP 
  ENDIF

  CALL getarg(1, cfilenameVAR)
  CALL getarg(2, cfilenameLSM)

  ! 2) build output file name
  ! 2.1 look if the filenameD contain 2D
  ipos=INDEX(cfilenameVAR,'_ERAinterim')
  IF ( ipos == 0 ) THEN
     ! weird ! not a dewpoint file
     PRINT *,' Error: first input file must be an ERAinterim variable file '
     STOP
  ENDIF
  ! 2.2 change append 2Q to 2D
  cfileout=cfilenameVAR(1:ipos-1)//'masked'//cfilenameVAR(ipos:)

  ! 2.3 variable name ...
  cvar=cfilenameVAR(1:ipos-1)
  
  PRINT *, TRIM(cvar)


  ! 3) Open input files
  istatus= NF90_OPEN(cfilenameVAR,NF90_NOWRITE,ncidVAR) ! open read-only
  istatus= NF90_OPEN(cfilenameLSM,NF90_NOWRITE,ncidLSM) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncidVAR,'lon',idxVAR) ; istatus=NF90_INQUIRE_DIMENSION(ncidVAR,idxVAR,len=npi)
  istatus=NF90_INQ_DIMID(ncidVAR,'lat',idyVAR) ; istatus=NF90_INQUIRE_DIMENSION(ncidVAR,idyVAR,len=npj)
  istatus=NF90_INQ_DIMID(ncidVAR,'time',idtVAR) ; istatus=NF90_INQUIRE_DIMENSION(ncidVAR,idtVAR,len=nptVAR)
  istatus=NF90_INQ_DIMID(ncidLSM,'x',idxLSM) ; istatus=NF90_INQUIRE_DIMENSION(ncidLSM,idxLSM,len=npiLSM)
  istatus=NF90_INQ_DIMID(ncidLSM,'y',idyLSM) ; istatus=NF90_INQUIRE_DIMENSION(ncidLSM,idyLSM,len=npjLSM)
  
  ! 4.1 check if there are 12 time frames in the variable file and that the mask has the same x y dimensions that the variable..
     PRINT *, ' input file have ', nptVAR, ' time frames'

  IF ( npiLSM /= npi ) THEN
     PRINT *, ' input files are supposed to have the same x/longitude dimension'
     PRINT *, ' The mask file have ', npiLSM, 'longitude points vs', npi, 'longitude points for the variable file...'
     PRINT *, ' Sorry :( '
     STOP
  ELSE IF ( npjLSM /= npj ) THEN
     PRINT *, ' input files are supposed to have the same y/latitude dimension'
     PRINT *, ' The mask file have ', npjLSM, 'latitude points vs', npj, 'latitude points for the variable file...'
     PRINT *, ' Sorry :( '
     STOP

  ENDIF


  ! 4.2 file OK allocate space
  ALLOCATE (time(nptVAR), VARvalue( npi,npj), LSMvalue( npi,npj), MVARvalue( npi,npj), rlon(npi), rlat(npj) )
  
  ! 5) Read data and compute masked variable
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncidVAR,'time',idv) ; istatus=NF90_GET_VAR(ncidVAR,idv,time)
  istatus=NF90_INQ_VARID(ncidVAR,'lon',idv) ; istatus=NF90_GET_VAR(ncidVAR,idv,rlon)
  istatus=NF90_INQ_VARID(ncidVAR,'lat',idv) ; istatus=NF90_GET_VAR(ncidVAR,idv,rlat)

  istatus=NF90_INQ_VARID(ncidVAR,cvar,idvVAR)
  istatus=NF90_INQ_VARID(ncidLSM,'lsm',idvLSM)
  
  ! prepare the output file
  ! 6) write output file
  ! 6.1 create output data set
  istatus=NF90_CREATE(cfileout,NF90_CLOBBER, ncout)
  ! 6.2 define dimensions
  istatus=NF90_DEF_DIM(ncout,'lon',npi,idxVAR)
  istatus=NF90_DEF_DIM(ncout,'lat',npj,idyVAR)
  istatus=NF90_DEF_DIM(ncout,'time',NF90_UNLIMITED,idt)
  ! 6.3 define variables
  ! 6.3.1 : map variables (lon, lat)
  istatus=NF90_DEF_VAR(ncout,'lon',NF90_DOUBLE,(/idxVAR/),idvx)
  istatus=NF90_DEF_VAR(ncout,'lat',NF90_DOUBLE,(/idyVAR/),idvy)
  istatus=NF90_DEF_VAR(ncout,'time',NF90_DOUBLE,(/idt/),idvt)

  ! 6.3.2: mean value
  istatus=NF90_DEF_VAR(ncout,cvar,NF90_FLOAT,(/idxVAR,idyVAR,idt/),idv)

  ! 6.3.3 copy attributes
  ! lon :
  istatus=NF90_INQUIRE_VARIABLE(ncidVAR,idvx,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidVAR,idvx,ji,catt)
     istatus=NF90_COPY_ATT(ncidVAR,idvx,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQUIRE_VARIABLE(ncidVAR,idvy,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidVAR,idvy,ji,catt)
     istatus=NF90_COPY_ATT(ncidVAR,idvy,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQUIRE_VARIABLE(ncidVAR,idvt,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidVAR,idvt,ji,catt)
     istatus=NF90_COPY_ATT(ncidVAR,idvt,catt,ncout,idvt)
  ENDDO
  ! field :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idv,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idv,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idv,catt,ncout,idv)
  ENDDO
  
  ! output field attribute
  istatus=NF90_GET_ATT(ncid,idv,'missing_value',catt)
  catt="0.000000"
  istatus=NF90_PUT_ATT(ncout,idv,'missing_value',catt)
  
  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  !6.4 : write variables
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time)

  DO jt = 1 , nptVAR 
  !  5.2 read variable values
  istatus=NF90_GET_VAR(ncidVAR,idvVAR,VARvalue, start=(/1,1,jt/), count=(/npi,npj,1/) )

  !  5.3 read land-sea mask values
  istatus=NF90_GET_VAR(ncidLSM,idvLSM,LSMvalue)

  !  5.4 compute masked file (after have "returned" mask values)
  



  MVARvalue(:,:)=VARvalue(:,:) * LSMvalue(:,:)
  
  istatus=NF90_PUT_VAR(ncout,idv,MVARvalue(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  ENDDO

  ! 7) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncidLSM)
  istatus=NF90_CLOSE(ncidVAR)
END PROGRAM mk_lsm_var

