PROGRAM upside_down_var
  !!-----------------------------------------------------------------------------
  !!                 *** Program upside_down_var  ***
  !!
  !!   Purpose : produce a netcdf files which is just the symetric of the
  !!            input file.
  !!
  !!   Method : copy the original file and work on the copy.
  !!            read the field and write it upside down.
  !!            Eventually correct latitude variable (if any)
  !!
  !!  history:  Original, J.M. Molines (Septembre 2009)
  !!----------------------------------------------------------------------------
  !! $Rev: 644 $
  !! $Date: 2013-12-20 11:23:39 +0100 (Fri, 20 Dec 2013) $
  !! $Id: upside_down_var.f90 644 2013-12-20 10:23:39Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  INTEGER :: iargc, narg, ipos
  INTEGER :: npi, npj, npt
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlat, zrlat

  REAL(KIND=4), DIMENSION(:,:,:) , ALLOCATABLE :: var, ivar
  CHARACTER(LEN=80) :: cfilin, cfilout, cmd, cvar, cdum, cvarlat, cnavlat


  ! netcdf stuff
  INTEGER :: istatus, ncid,id

  cvarlat='lat'
  cnavlat='nav_lat'

  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE: upside_down_var -f file -v var [-l latvar] [-l2 nav_latvar] '
     PRINT *,'      produce a file (file.sym), symetric to the input file '
     PRINT *,'    OPTIONS: '
     PRINT *,'      -l latvar : give the name of the latitude 1D variable if not lat '
     PRINT *,'      -l2 navlatvar : give the name of the latitude r2D variable if not nav_lat '
     STOP
  ENDIF

  ijarg=1
  DO WHILE ( ijarg <= narg ) 
  
    CALL getarg(ijarg,cdum) ; ijarg=ijarg+1
    SELECT CASE ( cdum )
    CASE ( '-f' ) ; CALL getarg(ijarg,cfilin   ) ; ijarg=ijarg+1
    CASE ( '-v' ) ; CALL getarg(ijarg,cvar     ) ; ijarg=ijarg+1
    CASE ( '-l' ) ; CALL getarg(ijarg,cvarlat  ) ; ijarg=ijarg+1
    CASE ( '-l2') ; CALL getarg(ijarg,cnavlat  ) ; ijarg=ijarg+1
    CASE DEFAULT ; PRINT * , TRIM(cdum),' : unknown option '
    END SELECT
  ENDDO
  cfilout=TRIM(cfilin)//'.sym'

  PRINT *,'CVAR=', TRIM(cvar)

  ! copy original file to cfilout
  cmd='cp '//TRIM(cfilin)//' '//TRIM(cfilout)
  CALL system(cmd)

  ! open and work on cfilout
  istatus=NF90_OPEN(cfilout,NF90_WRITE,ncid)  ! open write !

  istatus=NF90_INQ_DIMID(ncid,'lon',id)  ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'lat',id)  ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npj)
  istatus=NF90_INQ_DIMID(ncid,'time',id) ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npt) 

  ALLOCATE (rlat(npj), var(npi,npj,npt) )
  ALLOCATE (zrlat(npj), ivar(npi,npj,npt) )

  istatus=NF90_INQ_VARID(ncid,cvarlat,id) ;
  istatus=NF90_GET_VAR(ncid,id,rlat)
  zrlat(:)=rlat(npj:1:-1)
  istatus=NF90_PUT_VAR(ncid,id,zrlat)

  istatus=NF90_INQ_VARID(ncid,cnavlat,id)
  IF ( istatus == NF90_NOERR ) THEN  ! there is a 2D nav_lat var (or pseudo )
    istatus=NF90_GET_VAR(ncid,id,var(:,:,1) )
    var(:,:,2) = var(:,npj:1:-1,1)
    istatus=NF90_PUT_VAR(ncid,id,var(:,:,2) )

  ELSE
     PRINT *,' No nav_lat 2D found ...'
  ENDIF

  istatus=NF90_INQ_VARID(ncid,cvar,id) ;
  print *,NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,id,var)
  print *,NF90_STRERROR(istatus)
  ivar(:,:,:)=var(:,npj:1:-1,:)
  istatus=NF90_PUT_VAR(ncid,id,ivar)
  print *,NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

END PROGRAM upside_down_var
