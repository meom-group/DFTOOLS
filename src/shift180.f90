PROGRAM shift180
   !!======================================================================
   !!                     ***  PROGRAM  shift180  ***
   !!=====================================================================
   !!  ** Purpose : Shift 0-360 deg files to -180 180
   !!
   !!  ** Method  : read, shift write. Work on a copy
   !!
   !! History : 1.0  : 11/2012  : J.M. Molines : original
   !!----------------------------------------------------------------------

   !!----------------------------------------------------------------------
   !! FORCING_TOOLS , MEOM 2012
   !! $Id$
   !! Copyright (c) 2012, J.-M. Molines
   !! Software governed by the CeCILL licence (Licence/FORCINGTOOLSCeCILL.txt)
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------------
   USE netcdf
   IMPLICIT NONE
   INTEGER(KIND=4), PARAMETER :: jpguess=3
   INTEGER(KIND=4) :: jv, jg, jt
   INTEGER(KIND=4) :: iargc, narg
   INTEGER(KIND=4) :: npi, npj, npt, i1
   INTEGER(KIND=4) :: istatus, ncid,id, nvar, ndim, idv, idx, idy, idt, idlon
   INTEGER(KIND=4), DIMENSION(:,:) , ALLOCATABLE :: mask, imask
   INTEGER(KIND=4), DIMENSION(4)  :: idimids

   REAL(KIND=4), DIMENSION(:),   ALLOCATABLE :: rlon
   REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE :: rval

   CHARACTER(LEN=256) :: cf_in, cf_out, cmd
   CHARACTER(LEN=256) :: cv_dum, cv_nam, cv_lon
   CHARACTER(LEN=256), DIMENSION(jpguess) :: cv_guess=(/'lon','longitude','lon0'/)
   
   LOGICAL  :: lnotfound=.true.
   !!----------------------------------------------------------------------------

   narg=iargc()
   IF ( narg == 0 ) THEN
      PRINT *,' usage :  shift180 IN-file '
      PRINT *,'      '
      PRINT *,'     PURPOSE :' 
      PRINT *,'          Shift forcing field 180 deg to the west, hence'
      PRINT *,'          transforming 0-360 deg files into -180 180 deg file'
      PRINT *,'      '
      PRINT *,'     ARGUMENTS :'
      PRINT *,'          IN-file : file to be shifted.'
      PRINT *,'      '
      PRINT *,'      '
      PRINT *,'     OUTPUT : '
      PRINT *,'       netcdf file :  IN_file.shift '
      PRINT *,'         variables :  same as input file'
      PRINT *,'      '
      PRINT *,'     SEE ALSO :'
      PRINT *,'       upside_down'
      PRINT *,'      '
      STOP
   ENDIF

   CALL getarg(1,cf_in)
   cf_out=TRIM(cf_in)//'.shift'
   PRINT *, TRIM(cf_out)

   ! copy original file to cf_out
   cmd='cp '//TRIM(cf_in)//' '//TRIM(cf_out)
   CALL system(cmd)

   ! open and work on cf_out
   istatus=NF90_OPEN(cf_out,NF90_WRITE, ncid)  ! open write !
   ! figure out the longitude variable ...
   istatus=NF90_INQUIRE( ncid, nVariables=nvar)

   DO jv = 1, nvar 
      istatus=NF90_INQUIRE_VARIABLE( ncid, jv, name=cv_dum, ndims=ndim, dimids=idimids )
      IF ( ndim == 1 .AND. lnotfound ) THEN
         DO jg = 1, jpguess
            PRINT *, ' CHECK ', TRIM(cv_dum) ,' against ', TRIM( cv_guess(jg) )
            IF ( cv_dum == cv_guess(jg) ) THEN
               lnotfound=.false.
               cv_lon = cv_dum
               idlon  = jv
               EXIT
            ENDIF
         ENDDO
      ELSEIF ( ndim == 3 ) THEN
         idv    = jv
         cv_nam = cv_dum
         idx = idimids(1)
         idy = idimids(2)
         idt = idimids(3)
      ENDIF
   ENDDO
   IF ( lnotfound  ) THEN
       PRINT *, ' No longitude variable found from guess ...'  ; STOP
   ENDIF

   PRINT *, 'Longitude is supposed to be :', TRIM(cv_lon)
   PRINT *, 'variable is supposed to be :', TRIM(cv_nam)
   ! guess dimensions from variable
   istatus = NF90_INQUIRE_DIMENSION( ncid, idx, len=npi)
   istatus = NF90_INQUIRE_DIMENSION( ncid, idy, len=npj)
   istatus = NF90_INQUIRE_DIMENSION( ncid, idt, len=npt)
   PRINT *, ' NPI = ', npi
   PRINT *, ' NPJ = ', npj
   PRINT *, ' NPT = ', npt

   ALLOCATE (rlon(npi), rval(npi,npj) )
   i1 = npi/2+1

   istatus = NF90_GET_VAR(ncid, idlon, rlon, start=(/1/), count=(/npi/) )

   istatus = NF90_PUT_VAR(ncid, idlon, rlon(i1:npi) -360. , start=(/1/), count=(/npi-i1+1/) )
   istatus = NF90_PUT_VAR(ncid, idlon, rlon(1:i1-1)       , start=(/npi-i1+2/), count=(/i1-1/) )

   DO jt = 1, npt
     istatus = NF90_GET_VAR(ncid, idv, rval, start=(/1,1,jt/), count=(/npi,npj,1/) )

     istatus = NF90_PUT_VAR(ncid, idv, rval(i1:npi,:), start=(/1,1,jt/), count=(/npi-i1+1,npj,1/) )
     istatus = NF90_PUT_VAR(ncid, idv, rval(1:i1-1,:), start=(/npi-i1+2,1,jt/), count=(/i1-1,npj,1/) )
   ENDDO

   istatus=NF90_CLOSE(ncid)

END PROGRAM shift180
