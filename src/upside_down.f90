PROGRAM upside_down
  !!-----------------------------------------------------------------------------
  !!                 *** Program upside_down  ***
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
  !! $Rev: 304 $
  !! $Date: 2011-09-15 10:20:24 +0200 (Thu, 15 Sep 2011) $
  !! $Id: upside_down.f90 304 2011-09-15 08:20:24Z molines $
  !!----------------------------------------------------------------------------
  USE netcdf
  INTEGER :: iargc, narg
  INTEGER :: npi, npj
  REAL(KIND=4), DIMENSION(:), ALLOCATABLE :: rlat, zrlat
  INTEGER, DIMENSION(:,:) , ALLOCATABLE :: mask, imask
  CHARACTER(LEN=80) :: cfilin, cfilout, cmd


  ! netcdf stuff
  INTEGER :: istatus, ncid,id

  narg=iargc()
  IF ( narg /= 1 ) THEN
     PRINT *,' USAGE: upside_down file'
     PRINT *,'      produce a file (file.sym), symetric to the input file '
     PRINT *,'      with same variables and attributes '
     STOP
  ENDIF
  CALL getarg(1,cfilin)
  cfilout=TRIM(cfilin)//'.sym'

  ! copy original file to cfilout
  cmd='cp '//TRIM(cfilin)//' '//TRIM(cfilout)
  CALL system(cmd)

  ! open and work on cfilout
  istatus=NF90_OPEN(cfilout,NF90_WRITE,ncid)  ! open write !

  istatus=NF90_INQ_DIMID(ncid,'x',id) ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npi)
  istatus=NF90_INQ_DIMID(ncid,'y',id) ; istatus=NF90_INQUIRE_DIMENSION(ncid,id,len=npj)

  ALLOCATE (rlat(npj), mask(npi,npj) )
  ALLOCATE (zrlat(npj), imask(npi,npj) )
  istatus=NF90_INQ_VARID(ncid,'lat',id) ; istatus=NF90_GET_VAR(ncid,id,rlat)
  zrlat(:)=rlat(npj:1:-1)
  istatus=NF90_PUT_VAR(ncid,id,zrlat)

  istatus=NF90_INQ_VARID(ncid,'lsm',id) ; istatus=NF90_GET_VAR(ncid,id,mask)
  imask(:,:)=mask(:,npj:1:-1)
  istatus=NF90_PUT_VAR(ncid,id,imask)

  istatus=NF90_CLOSE(ncid)

END PROGRAM upside_down
