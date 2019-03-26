PROGRAM calc_q2_6H
  !!----------------------------------------------------------------------------
  !!              *** PROGRAM calc_q2_6H  ***
  !!
  !!  Purpose : take ERAinterim T2 and D2 files as input 
  !!            and return a file with q2 values.
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
  !! $Id: calc_q2_6H.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  INTEGER :: ji, jj, jt     !: some loop index
  INTEGER :: npi, npj, npt  !: dimension of the fields
  INTEGER :: nptT, nptD
  INTEGER :: iargc, narg    !: command line variables
  INTEGER :: ipos, natt

  REAL(KIND=8), DIMENSION(:)  , ALLOCATABLE :: time, rlon, rlat
  REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: D2value, T2value, Q2value, psat, HR

  CHARACTER(LEN=80) :: cfilenameD  !: name of dewpoint input file
  CHARACTER(LEN=80) :: cfilenameT  !: name of temperature input file
  CHARACTER(LEN=80) :: cfileout    !: name of output file
  CHARACTER(LEN=80) :: catt        !: attribute name
  ! netcdf stuff
  INTEGER :: ncid, ncidD, ncidT, ncout              !: ncdf logical unit for input and output file
  INTEGER :: istatus, idx, idy, idt, idtD, idtT    !: ncdf id for dimension
  INTEGER :: idv,idvx, idvy, idvt             !: ncdf id for variable
  INTEGER :: idvD, idvT2

  CHARACTER(LEN=80) :: cvar           !: to hold variable name

  ! 1) Read command line
  narg = iargc()   ! How many arguments on the command line ?
  IF ( narg /= 2 ) THEN
     PRINT *, ' USAGE : calc_q2_6H filenameD filnameT'
     PRINT *, ' filenameD : name of the ncdf input file containing the dewpoint value'
     PRINT *, ' filenameT : name of the ncdf input file containing the temperature value'
     PRINT *, ' output file name will  have the variable name changed to Q2'
     STOP 
  ENDIF

  CALL getarg(1, cfilenameD)
  CALL getarg(2, cfilenameT)

  ! 2) build output file name
  ! 2.1 look if the filenameD contain 2D
  ipos=INDEX(cfilenameD,'2D.nc')
  IF ( ipos == 0 ) THEN
     ! weird ! not a dewpoint file
     PRINT *,' Error: first input file must be dewpoint file '
     STOP
  ENDIF
  ! 2.2 change append 2Q to 2D
  cfileout=cfilenameD(4:ipos-1)//'2Q.nc'
  
  PRINT *, ' new name : ', cfileout, '.'

  ! 2.3 variable name ...
  cvar='q2'
  
  PRINT *, TRIM(cvar)


  ! 3) Open input files
  istatus= NF90_OPEN(cfilenameD,NF90_NOWRITE,ncidD) ! open read-only
  istatus= NF90_OPEN(cfilenameT,NF90_NOWRITE,ncidT) ! open read-only
  IF ( istatus /= NF90_NOERR) THEN
     ! One or two of the files does not exist probably ...
     PRINT *, NF90_STRERROR(istatus)    ! interpret error status
     STOP                               ! abort program
  ENDIF

  ! 4) Look for dimensions of input files
  istatus=NF90_INQ_DIMID(ncidD,'lon',idx) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idx,len=npi)
  istatus=NF90_INQ_DIMID(ncidD,'lat',idy) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idy,len=npj)
  istatus=NF90_INQ_DIMID(ncidD,'time',idtD) ; istatus=NF90_INQUIRE_DIMENSION(ncidD,idtD,len=nptD)
  istatus=NF90_INQ_DIMID(ncidT,'time',idtT) ; istatus=NF90_INQUIRE_DIMENSION(ncidT,idtT,len=nptT)

  
  ! 4.1 check if there are 12 time frames in the file ...
  IF ( nptD /= nptT ) THEN
     PRINT *, ' input files are both supposed to hold the same number of time frames(1460 or 1464)'
     PRINT *, ' These files have ', nptD, 'and', nptT, 'time frames ...'
     PRINT *, ' Sorry :( '
     STOP
  ENDIF


  ! 4.2 file OK allocate space
  ALLOCATE (time(nptD), D2value( npi,npj), T2value( npi,npj), Q2value( npi,npj), rlon(npi), rlat(npj) )
  ALLOCATE (psat( npi,npj), HR( npi,npj))
  
  ! 5) Read data and compute Q2
  !  5.1 time , lon, lat:
  istatus=NF90_INQ_VARID(ncidD,'time',idv) ; istatus=NF90_GET_VAR(ncidD,idv,time)
  istatus=NF90_INQ_VARID(ncidD,'lon',idv) ; istatus=NF90_GET_VAR(ncidD,idv,rlon)
  istatus=NF90_INQ_VARID(ncidD,'lat',idv) ; istatus=NF90_GET_VAR(ncidD,idv,rlat)

  istatus=NF90_INQ_VARID(ncidD,'D2M',idvD)
  !istatus=NF90_INQ_VARID(ncidT,'T2M',idvT2)
  istatus=NF90_INQ_VARID(ncidT,'T2M',idvT2)
  ! prepare the output file
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
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvx,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvx,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvx,catt,ncout,idvx)
  ENDDO
  ! lat :
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvy,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvy,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvy,catt,ncout,idvy)
  ENDDO
  ! time :
  istatus=NF90_INQUIRE_VARIABLE(ncidD,idvt,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncidD,idvt,ji,catt)
     istatus=NF90_COPY_ATT(ncidD,idvt,catt,ncout,idvt)
  ENDDO
  ! field :
  istatus=NF90_INQUIRE_VARIABLE(ncid,idv,natts=natt)
  DO ji=1,natt
     istatus=NF90_INQ_ATTNAME(ncid,idv,ji,catt)
     istatus=NF90_COPY_ATT(ncid,idv,catt,ncout,idv)
  ENDDO
  !en chantier
  ! correct the unit attribute for field (erase tailing "s " )
  istatus=NF90_GET_ATT(ncid,idv,'long_name',catt)
  catt="Specific humidity"
  istatus=NF90_PUT_ATT(ncout,idv,'long_name',catt)

  istatus=NF90_GET_ATT(ncid,idv,'units',catt)
  catt="kg/kg"
  istatus=NF90_PUT_ATT(ncout,idv,'units',catt)

  istatus=NF90_GET_ATT(ncid,idv,'code',catt)
  catt="9999"
  istatus=NF90_PUT_ATT(ncout,idv,'code',catt)
  ! 6.3.4 : leave def mode
  istatus=NF90_ENDDEF(ncout)

  !6.4 : write variables
  istatus=NF90_PUT_VAR(ncout,idvx,rlon)
  istatus=NF90_PUT_VAR(ncout,idvy,rlat)
  istatus=NF90_PUT_VAR(ncout,idvt,time)

  DO jt = 1 , nptD 
  !  5.2 read dewpoint values
  istatus=NF90_GET_VAR(ncidD,idvD,D2value, start=(/1,1,jt/), count=(/npi,npj,1/) )

  !  5.3 read temperature values
  istatus=NF90_GET_VAR(ncidT,idvT2,T2value, start=(/1,1,jt/), count=(/npi,npj,1/) )

  !  5.4 compute q2 (with intermediate values psat and HR)
  T2value(:,:)=T2value(:,:) - 273.18
  D2value(:,:)=D2value(:,:) - 273.18
  
  psat(:,:)=exp(23.3265-3802.7/(T2value(:,:)+273.18)-(472.68/(T2value(:,:)+273.18))**2)
  
  HR(:,:)=100*(( (D2value(:,:)-0.1*T2value(:,:)+112)/(112+0.9*T2value(:,:)))**8 )

  Q2value(:,:) =(0.622*psat(:,:)*HR(:,:))/(10132500-psat(:,:)*HR(:,:))

  istatus=NF90_PUT_VAR(ncout,idv,Q2value(:,:),start=(/1,1,jt/), count=(/npi,npj,1/) )
  

  ENDDO
  
  
  ! 7) : close the files
  istatus=NF90_CLOSE(ncout)
  istatus=NF90_CLOSE(ncidT)
  istatus=NF90_CLOSE(ncidD)
END PROGRAM calc_q2_6H

