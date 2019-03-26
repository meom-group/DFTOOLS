PROGRAM add_feb_29
  !!======================================================================
  !!                     ***  PROGRAM  add_feb_29  ***
  !!=====================================================================
  !!  ** Purpose : add february 29 for leap years, when original file
  !!               does not have it ...
  !!
  !!  ** Method  : Linear interpolation between the last field of
  !!               Feb, 28 and the first field of March, 1st
  !!
  !! History : 1.0  : 03/2014  : J.M. Molines : Original code
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE
  
  INTEGER(KIND=4)    :: jvar, jdim, jt
  INTEGER(KIND=4)    :: nx, ny, nt

  INTEGER(KIND=4)    :: narg, iargc, ipos, iyear
  INTEGER(KIND=4)    :: ncid, ierr, idum, idv, idx, idy, ncid2, idvt
  INTEGER(KIND=4)    :: ndim, nvar, natt, idunlim
  INTEGER(KIND=4), DIMENSION(:),   ALLOCATABLE :: ilen, ivarid, nvardim
  INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE :: ndimids
  
  REAL(KIND=4), DIMENSION(:),     ALLOCATABLE :: time, tim29
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: v29_02

  CHARACTER(LEN=256) :: cf_in, cf_ou
  CHARACTER(LEN=256) :: cmd
  CHARACTER(LEN=256), DIMENSION(:), ALLOCATABLE :: cdimnm, cvarnm

  !!----------------------------------------------------------------------
  !! FORCING_TOOLS, MEOM 2014
  !! $Id: add_feb_29.f90 651 2014-03-22 08:09:47Z molines $
  !! Copyright (c) 2014, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/FORCING_CeCILL.txt)
  !!----------------------------------------------------------------------
  narg = iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :  add_feb_29 IN_file'
     PRINT *,'      '
     PRINT *,'     PURPOSE : '
     PRINT *,'          Add 29 february into leap year files which do not have it '
     PRINT *,'          This is done by linear interpolation between last field of Feb, 28th'
     PRINT *,'          and first field of March, 1st.'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        IN_file : input forcing file (e.g. q2_DFS4.2_y1958.nc)' 
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'        '
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'         none'
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : IN_file.29feb'
     PRINT *,'      '
     PRINT *,'       '
     PRINT *,'      '
     STOP
  ENDIF

  CALL getarg( 1, cf_in)
  ipos=INDEX(cf_in,'_y') 
  READ(cf_in(ipos+2:ipos+6),*) iyear

  IF ( MOD(iyear,4)  /= 0 ) THEN
    PRINT *,' This year (',iyear,') is not leap year. Nothing to do'
    STOP
  ENDIF
  
  PRINT *,'COPYING FILE  : ', TRIM(cf_in) 
  cf_ou=TRIM(cf_in)//".29feb"
  cmd=' cp '//TRIM(cf_in)//' '//TRIM(cf_ou)
  CALL SYSTEM(cmd)
  PRINT *,'WORKING FILE  : ', TRIM(cf_ou) 

  ! open in file
  ierr = NF90_OPEN(cf_in, NF90_NOWRITE, ncid  )
  ierr = NF90_OPEN(cf_ou, NF90_WRITE,   ncid2 )
  ! look file informations
  ierr = NF90_Inquire( ncid, nDimensions=ndim, nVariables=nvar, nAttributes=natt, &
       &                unlimitedDimId=idunlim)
  ALLOCATE( ilen(ndim), cdimnm(ndim), ivarid(nvar), cvarnm(nvar), ndimids(nvar,4) )
  ALLOCATE( nvardim(nvar)  )

  ! look for dims
  DO jdim =1, ndim
    ierr= NF90_INQUIRE_DIMENSION(ncid, jdim, name=cdimnm(jdim), len=ilen(jdim) )
  ENDDO

  PRINT *, ' ** DIMENSIONS **'
  PRINT *, '   Number of dimensions    :', ndim
  IF ( idunlim == -1 ) THEN
     PRINT *, '   Unlimited dimension     : none '
  ELSE
     PRINT *, '   Unlimited dimension  is :', TRIM(cdimnm(idunlim) )
  ENDIF

  DO jdim = 1, ndim
     PRINT *,'    Dimension ', jdim,' is ',TRIM(cdimnm(jdim)), ' : ', ilen(jdim) 
  ENDDO

  PRINT *
  PRINT *, ' ** VARIABLES **'

  DO jvar = 1, nvar
    ierr = NF90_INQUIRE_VARIABLE( ncid, jvar, cvarnm(jvar), ndims=nvardim(jvar), dimids=ndimids(jvar,:) )
    PRINT *,'    Variable ', jvar,' is ',TRIM(cvarnm(jvar)),' with ',nvardim(jvar),' dimensions'
  ENDDO

  IF (idunlim < 0 ) THEN
   STOP ' no time dimension in file'
  ENDIF
  ! look for time variable :
  DO jvar =  1, nvar
     IF ( ndimids(jvar,1) == idunlim .AND. nvardim(jvar) == 1 ) THEN
        idvt=jvar
        PRINT *, 'Name of time variable is :', TRIM(cvarnm(idvt) )
        EXIT
     ENDIF
  ENDDO

! check if adjustment is necessary, ie if unlim dim is a multiple of 366
  IF ( MOD( ilen(idunlim), 366 ) /= 0 ) THEN
    IF ( MOD(ilen(idunlim), 365) == 0 ) THEN
      PRINT *, ' LEAP year, 29 feb is missing ...'
      ! look for 3D variable with time in last position
      DO jvar = 1, nvar
        IF ( nvardim(jvar) == 3 )  THEN
          idv=jvar
          IF ( ndimids(idv,3) == idunlim ) THEN
             idx=ndimids(idv,1)
             idy=ndimids(idv,2)
             nx=ilen(idx)
             ny=ilen(idy)
             nt=ilen(idunlim)
             PRINT *, 'NX =', nx
             PRINT *, 'NY =', ny
             PRINT *, 'NT =', nt
             ALLOCATE (time(nt))
             ierr = NF90_GET_VAR(ncid,idvt,time)
             EXIT
          ENDIF
        ENDIF
       ENDDO
      PRINT *, ' correction for 29/02'
      CALL add_29_02
    ELSE
      PRINT *, ' Leap Year but missing more than 1 day '
    ENDIF
  ENDIF

CONTAINS
  SUBROUTINE add_29_02
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE add_29_02  ***
    !!
    !! ** Purpose :  add 29/02 when missing in a leap year file
    !!       
    !! ** Method  :  linear interpolation 
    !!
    !!----------------------------------------------------------------------
    INTEGER (KIND=4) :: ival_per_day, irec28_2, irec01_3
    INTEGER (KIND=4) :: ih, jt

    REAL(KIND=4), DIMENSION(:,:),   ALLOCATABLE :: z28_02, z01_03, zcur
    REAL(KIND=4) :: zcoef
    !!----------------------------------------------------------------------

    ival_per_day= ilen(idunlim)/365
    ih = 24/ival_per_day
 

    ALLOCATE ( z28_02(nx, ny), z01_03(nx, ny) , v29_02(nx, ny, ival_per_day) )
    ALLOCATE ( zcur(nx, ny), tim29(ival_per_day) )

    irec28_2=(31+28)*ival_per_day
    irec01_3=(31+28)*ival_per_day + 1
    tim29(:) = (/ (time(irec28_2)+jt/24. ,jt=ih,24,ih) /)
    time(irec01_3:) = time(irec01_3:) + 1.

    ierr=NF90_GET_VAR(ncid,idv,z28_02,start=(/1,1,irec28_2/),count=(/nx,ny,1/) )
    ierr=NF90_GET_VAR(ncid,idv,z01_03,start=(/1,1,irec01_3/),count=(/nx,ny,1/) )
    PRINT *, ' ..interpolation between ', irec28_2 ,' and ', irec01_3
    DO jt=1, ival_per_day
       zcoef=1./( ival_per_day +1 ) *jt
       PRINT *, 'ZCOEF', 1. -zcoef,  zcoef
       v29_02(:,:,jt) = (1. - zcoef)*z28_02 + zcoef*z01_03 
       !   l    |    |   |    |    |    |   |   |   f  
       !        *    *   *    *    *    *   *   *
       !   0    *    *   *    *    *    *   *   *   1    : zcoef
    ENDDO
    DEALLOCATE ( z28_02,  z01_03 )
    DO jt=1, irec28_2
       ierr=NF90_GET_VAR(ncid,  idv, zcur, start=(/1,1,jt/),count=(/nx,ny,1/) )
       ierr=NF90_PUT_VAR(ncid2, idv, zcur, start=(/1,1,jt/),count=(/nx,ny,1/) )
       IF (ierr /= NF90_NOERR) THEN
          PRINT *,"ERROR1.1 : ", NF90_STRERROR(ierr), jt
       ENDIF
       ierr=NF90_PUT_VAR(ncid2, idvt, time(jt:jt), start=(/jt/), count=(/1/) )
       IF (ierr /= NF90_NOERR) THEN
          PRINT *,"ERROR1.2 : ", NF90_STRERROR(ierr), jt
       ENDIF
    ENDDO
   
    ! write missing 29 Feb
    ierr = NF90_PUT_VAR(ncid2, idv, v29_02,  start=(/1,1,irec28_2+1/),count=(/nx,ny,ival_per_day/) )
    ierr = NF90_PUT_VAR(ncid2, idvt, tim29,  start=(/irec28_2+1/)    ,count=(/ival_per_day/)       )

    DO jt=irec01_3,  nt
       ierr=NF90_GET_VAR(ncid,  idv, zcur, start=(/1,1,jt/),count=(/nx,ny,1/) )
       ierr=NF90_PUT_VAR(ncid2, idv, zcur, start=(/1,1,jt+ival_per_day/),count=(/nx,ny,1/) )
       IF (ierr /= NF90_NOERR) THEN
          PRINT *,"ERROR2.1 : ", NF90_STRERROR(ierr), jt
       ENDIF
       ierr=NF90_PUT_VAR(ncid2, idvt, time(jt:jt), start=(/jt+ival_per_day/), count=(/1/) )
       IF (ierr /= NF90_NOERR) THEN
          PRINT *,"ERROR2.2 : ", NF90_STRERROR(ierr), jt
       ENDIF
    ENDDO

    ierr=NF90_CLOSE(ncid)
    ierr=NF90_CLOSE(ncid2)


  END SUBROUTINE add_29_02
  
END PROGRAM add_feb_29
