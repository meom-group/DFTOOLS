PROGRAM fill_time_counter
  USE netcdf

  IMPLICIT NONE
  INTEGER(KIND=4) :: jy, jd
  INTEGER(KIND=4) :: it
  INTEGER(KIND=4) :: iyear=1950
  INTEGER(KIND=4) :: nday

  REAL(KIND=8) :: dtime
  REAL(KIND=8), DIMENSION(:), ALLOCATABLE  :: dtime_array
  REAL(KIND=8) :: dt=03.d0

  CHARACTER(LEN=255) :: cunits="hours since  1950-01-01 00:00:00"


  dtime=0.d0 - dt
  DO jy=iyear,2020
     it = 0
     nday=365
     IF ( MOD(jy,4) == 0 ) nday=366
     IF ( MOD(jy,100) == 0 ) nday=365
     IF ( MOD(jy,400) == 0 ) nday=366
     ALLOCATE (dtime_array (INT(nday*24/dt)) )
     DO jd=1, nday*24/dt
        it=it+1
        dtime=dtime + dt
        dtime_array(it) = dtime
        IF ( jy == 1992 ) THEN
        PRINT *, jy, it, dtime
        ENDIF
     ENDDO
     print *, it, jy
     CALL OutputTime(dtime_array, it, jy )
     DEALLOCATE (dtime_array )
  ENDDO

CONTAINS
  SUBROUTINE OutputTime ( dd_time, knday, ky )
    REAL(KIND=8) ,DIMENSION(:) , INTENT(in) :: dd_time
    INTEGER(KIND=4),             INTENT(in) :: knday
    INTEGER(KIND=4),             INTENT(in) :: ky

    INTEGER(KIND=4) :: ncid, ierr, id, idt
    CHARACTER(LEN=155) :: cl_fname

    WRITE(cl_fname,'("time_counter_",I4.4,"_",I2.2,"h.nc")') ky, INT(dt)
    PRINT *, TRIM(cl_fname)

    ierr = NF90_CREATE(TRIM(cl_fname),NF90_NETCDF4,ncid)
    ierr = NF90_DEF_DIM(ncid,"time",NF90_UNLIMITED,idt)

    ierr = NF90_DEF_VAR(ncid,"time",NF90_DOUBLE,(/idt/), id )
    ierr = NF90_PUT_ATT(ncid,id,"units",cunits)
    ierr = NF90_ENDDEF(ncid) 
    ierr = NF90_PUT_VAR(ncid, id, dd_time(1:knday) )
    ierr = NF90_CLOSE(ncid)
  END SUBROUTINE OutputTime

END PROGRAM fill_time_counter
