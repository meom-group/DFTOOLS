PROGRAM dft_timeseries
  !!======================================================================
  !!                     ***  PROGRAM  dft_timeseries  ***
  !!=====================================================================
  !!  ** Purpose : Compute spatially weighted average of forcing fields
  !!               available on geographical grid (e.g. ERA40, ERAinterim)
  !!
  !!  ** Method  : The weights ( area of data cell in the input file) are
  !!               estimated from longitude and latitude of the points.
  !!
  !! History : 1.0  : 02/2012  : Raphael Dussin in FARC tools
  !!           2.0  : 06/2014  : J.M. Molines : CDFTOOLS style 
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!    check        : error wrapper for netcdf functions
  !!----------------------------------------------------------------------

USE netcdf

   IMPLICIT NONE

   INTEGER(KIND=4)            :: narg, iargc                      ! command line arguments
   INTEGER(KIND=4)            :: ncid_in, ncid_msk               ! netcdf id's
   INTEGER(KIND=4)            :: ncid_ts, ncid_ym, ncid_gm        ! netcdf id's

   INTEGER(KIND=4)            :: idd_lonin,  idd_latin
   INTEGER(KIND=4)            :: idd_timein, idd_timeout
   INTEGER(KIND=4)            :: idd_t                     ! netcdf dimensions id's
   INTEGER(KIND=4)            :: idv_lonin,  idv_latin
   INTEGER(KIND=4)            :: idv_in,     idv_msk
   INTEGER(KIND=4)            :: idv_timeout
   INTEGER(KIND=4)            :: idv_ts, idv_ym, idv_gm
   INTEGER(KIND=4)            :: npx, npy, npt                     ! dimensions in rad and wgt
   INTEGER(KIND=4)            :: jj, ji, jarg
   INTEGER(KIND=4)            :: ijarg, nfil, jt, ifr
   INTEGER(KIND=4)            :: nftot=0
   INTEGER(KIND=4)            :: ierr1, ierr2
   INTEGER(KIND=4), DIMENSION(:,:), ALLOCATABLE :: icount

   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dvarin, dmask
   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: dlon2,  dlat2
   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: de1,    de2
   REAL(KIND=8), DIMENSION(:),   ALLOCATABLE :: dlon1d, dlat1d 
   REAL(KIND=8), DIMENSION(:),   ALLOCATABLE :: dts, dym, dgm, dtimout, dtim
   REAL(KIND=8)                              :: dlta_lon, dlta_lat, dconv, dearth_radius
   REAL(KIND=8)                              :: dfyear=0.d0, dlyear=0.d0 , dratio=1.d0
   REAL(KIND=8)                              :: dsf, dao                          ! scale factor / add offset
   REAL(KIND=8)                              :: darea              ! area (m2) of the region of interest

   CHARACTER(LEN=256)                            :: cv_in, cv_out
   CHARACTER(LEN=256)                            :: cf_tsout
   CHARACTER(LEN=256)                            :: cf_ymout
   CHARACTER(LEN=256)                            :: cf_gmout
   CHARACTER(LEN=256), DIMENSION(:), ALLOCATABLE :: cf_inlist                      ! list of input files
   CHARACTER(LEN=256)                            :: cldum                        ! dummy string argument

   CHARACTER(LEN=256)                            :: cf_msk="mask.nc" , cv_msk="lsm"
   CHARACTER(LEN=256)                            :: cv_altlon="lon0" , cv_altlat="lat0"
   CHARACTER(LEN=256)                            :: c_dimlon='lon' , cv_lon='lon'
   CHARACTER(LEN=256)                            :: c_dimlat='lat' , cv_lat='lat'
   CHARACTER(LEN=256)                            :: c_dimtim='time', cv_tim='time'
   CHARACTER(LEN=256)                            :: cl_frstyr="0", cl_lastyr="0"
   CHARACTER(LEN=256)                            :: cl_zone='global'
   CHARACTER(LEN=256)                            :: cl_dtaset='unknown'
   CHARACTER(LEN=256)                            :: cl_ratio='1.d0'
   CHARACTER(LEN=256)                            :: cl_dirout='./'
  !!----------------------------------------------------------------------
  !! FORCING_TOOLS , MEOM 2014
  !! $Id$
  !! Copyright (c) 2014, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/FORCINGTOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  narg= iargc()

  IF ( narg == 0 ) THEN
     PRINT *,' usage :  global_timeserie list_of_files -var variable_name ...'
     PRINT *,'         [-fyear first_year] [-lyear last_year] [-area region_of_interest] ...'
     PRINT *,'         [-dataset dataset_name ] [-diroutput output_directory ] ... '
     PRINT *,'         [-mask path_to_mask] [-ratio multiplicative_ratio ] ...'
     PRINT *,'         [ -dim_lon LON-dim ] [-dim_lat LAT-dim ] [-dim_tim TIME-dim] ...'
     PRINT *,'         [ -var_lon LON-var ] [-var_lat LAT-var ] [-var_tim TIME-var]' 
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       This program aims at computing timeseries of spatially averaged'
     PRINT *,'       variables for data files, on geographical grid.' 
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       list_of_files : list of data files to process. Data files must share'
     PRINT *,'                   the same variables, they only differ for the period they'
     PRINT *,'                   cover.'
     PRINT *,'       -var variable_name : name of the variable to process'
     PRINT *,'       '
     PRINT *,'     OPTIONS (default values are given in [ ... ] :'
     PRINT *,'       -fyear first_year : specify the first year to process, [', TRIM(cl_frstyr),']'
     PRINT *,'       -lyear last_year : specify the last year to process, [', TRIM(cl_lastyr),']'
     PRINT *,'       -dataset dataset_name : name of data set to use in output file name,'
     PRINT *,'              default is :[',TRIM(cl_dtaset),']'
     PRINT *,'         REM : these three options are only used for building the prefix '
     PRINT *,'            of the output files.( see OUTPUT below)'
     PRINT *,'       -area region of interest : indicate latitude predefined regions of '
     PRINT *,'              interest. Available regions are : '
     PRINT *,'              Polar_South [-90S, -70S]'
     PRINT *,'              Subpolar_South [-70S, -45S]'
     PRINT *,'              Subtropical_South [-45S, -25S],'
     PRINT *,'              Tropical_South [-25S, -10S]'
     PRINT *,'              Equatorial_Band [ -10S, 10N ]'
     PRINT *,'              Tropical_North [10N, 25N]'
     PRINT *,'              Subtropical_North [25N, 45N]'
     PRINT *,'              Subpolar_North [45N, 70N]'
     PRINT *,'              Polar_North [70N, 90 N]'
     PRINT *,'              global [all the file ]'
     PRINT *,'              Default is [',TRIM(cl_zone),']'
     PRINT *,'       -diroutput : output directory [',TRIM(cl_dirout),']'
     PRINT *,'       -mask path_to_mask : indicate the name of the land/sea mask. [', TRIM(cf_msk),']'
     PRINT *,'             mask variable is supposed to be named ',TRIM(cv_msk),'.'
     PRINT *,'       -ratio : multiplicative facteur used for scaling the output [',TRIM(cl_ratio),']'
     PRINT *,'       '
     PRINT *,'       -dim_lon LON-dim : give name of longitude dimension [',TRIM(c_dimlon),']'
     PRINT *,'       -dim_lat LAT-dim : give name of latitude dimension [',TRIM(c_dimlat),']'
     PRINT *,'       -dim_tim TIME-dim : give name of time dimension [',TRIM(c_dimtim),']'
     PRINT *,'       -var_lon LON-var : give name of longitude variable [',TRIM(cv_lon),']'
     PRINT *,'       -var_lat LAT-var : give name of latitude variable [',TRIM(cv_lat),']'
     PRINT *,'       -var_tim TIME-var : give name of time variable [',TRIM(cv_tim),']'
     PRINT *,'        '
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none ' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'      netcdf file : <prefix>_full_timeserie.nc   : same freq. than input files'
     PRINT *,'                    <prefix>_yearly_timeserie.nc : yearly means for each year'
     PRINT *,'                    <prefix>_mean_of_timeserie.nc: global mean over the period'
     PRINT *,'              where <prefix> is :'
     PRINT *,'                    <zone>_<variable>_Dataset_<dataset>_<first_yr>-<last_yr>'
     PRINT *,'       variables : same as specified as input variable.'
     PRINT *,'      '
     PRINT *,'     SEE ALSO :'
     PRINT *,'      ' 
     PRINT *,'      '
     STOP
  ENDIF

  !! Set list of files
  ALLOCATE ( cf_inlist(narg) )
  ijarg = 1
  nfil = 0
  DO WHILE ( ijarg <= narg )
     CALL getarg (ijarg, cldum) ; ijarg=ijarg + 1
     SELECT CASE ( cldum )
     CASE ( "-dataset" )
        CALL getarg (ijarg, cl_dtaset ) ; ijarg=ijarg+1
     CASE ( "-area" )
        CALL getarg (ijarg, cl_zone   ) ; ijarg=ijarg+1
     CASE ( "-fyear" )
        CALL getarg (ijarg, cl_frstyr ) ; ijarg=ijarg+1 ; READ(cl_frstyr,*) dfyear
     CASE ( "-lyear" )
        CALL getarg (ijarg, cl_lastyr ) ; ijarg=ijarg+1 ; READ(cl_lastyr,*) dlyear
     CASE ( "-var" )
        CALL getarg (ijarg, cv_in     ) ; ijarg=ijarg+1
     CASE ( "-diroutput" )
        CALL getarg (ijarg, cl_dirout ) ; ijarg=ijarg+1
     CASE ( "-mask" )
        CALL getarg (ijarg, cf_msk    ) ; ijarg=ijarg+1
     CASE ( "-ratio" )
        CALL getarg (ijarg, cl_ratio  ) ; ijarg=ijarg+1 ; READ(cl_ratio,*) dratio 
     CASE ( "-dim_lon" )
        CALL getarg (ijarg, c_dimlon  ) ; ijarg=ijarg+1
     CASE ( "-dim_lat" )
        CALL getarg (ijarg, c_dimlat  ) ; ijarg=ijarg+1
     CASE ( "-dim_tim" )
        CALL getarg (ijarg, c_dimtim  ) ; ijarg=ijarg+1
     CASE ( "-var_lon" )
        CALL getarg (ijarg, cv_lon    ) ; ijarg=ijarg+1
     CASE ( "-var_lat" )
        CALL getarg (ijarg, cv_lat    ) ; ijarg=ijarg+1
     CASE ( "-var_tim" )
        CALL getarg (ijarg, cv_tim    ) ; ijarg=ijarg+1

     CASE DEFAULT         ! then the argument is a file
        nfil          = nfil + 1
        cf_inlist(nfil) = TRIM(cldum)
     END SELECT
  END DO


  !! We need to known the total number of frames
  DO jarg=1,nfil
     CALL check( NF90_OPEN(TRIM(cf_inlist(jarg)), NF90_NOWRITE, ncid_in) )
     CALL check( NF90_INQ_DIMID(ncid_in, c_dimtim, idd_t) )
     CALL check( NF90_INQUIRE_DIMENSION(ncid_in, idd_t, len=npt) )
     CALL check( NF90_CLOSE(ncid_in) )
     nftot = nftot + npt
  ENDDO

  PRINT *, '-----------------------------------------------------------'
  PRINT *, 'Variable : ', TRIM(cv_in) , ' , starting in ', TRIM(cl_frstyr) , ' and ending in ', TRIM(cl_lastyr)
  PRINT *, 'Region = ', TRIM(cl_zone)
  PRINT *, nfil, ' files to proceed'
  PRINT '(a,i7.0,a)', ' allocate for ', nftot , ' frames'
  PRINT *, '-----------------------------------------------------------'

  ALLOCATE ( dts(nftot), dym(nftot), dgm(nftot), dtimout(nftot) )
  ALLOCATE ( dtim(1) )

  !! Read Lon/Lat and Mask
  CALL check( NF90_OPEN(TRIM(cf_msk), NF90_NOWRITE, ncid_msk) )

  ! first we try with regular lon/lat
  ierr1 = NF90_INQ_DIMID(ncid_msk, cv_lon, idd_lonin)
  ierr2 = NF90_INQ_DIMID(ncid_msk, cv_lat, idd_latin)
  
  ! if it fails, try the alternative lon0/lat0
  IF ( (ierr1 /= NF90_NOERR).OR.(ierr2 /= NF90_NOERR) ) THEN
    PRINT *, ' ERROR: bad name for lon lat dimension: use -dim_lon, -dim_lat options!'
    STOP 1
  ENDIF

  CALL check( NF90_INQUIRE_DIMENSION(ncid_msk, idd_lonin, len=npx) )
  CALL check( NF90_INQUIRE_DIMENSION(ncid_msk, idd_latin, len=npy) )

  ALLOCATE ( dlon1d(npx) , dlat1d(npy) )
  ALLOCATE ( dlon2(npx,npy) , dlat2(npx,npy) )
  ALLOCATE ( dmask(npx,npy) )

  ! first we try with regular lon/lat
  ierr1 = NF90_INQ_VARID(ncid_msk, cv_lon, idv_lonin)
  ierr2 = NF90_INQ_VARID(ncid_msk, cv_lat, idv_latin)
  
  ! if it fails, try the alternative lon0/lat0
  IF ( (ierr1 /= NF90_NOERR).OR.(ierr2 /= NF90_NOERR) ) THEN
    PRINT *, ' ERROR: bad name for lon lat variables: use -dim_lon, -dim_lat options!'
    STOP 1
  ENDIF

  CALL check( NF90_INQ_VARID(ncid_msk, cv_msk, idv_msk) )

  CALL check( NF90_GET_VAR(ncid_msk, idv_lonin, dlon1d, start = (/1/),     count = (/npx/) )       )
  CALL check( NF90_GET_VAR(ncid_msk, idv_latin, dlat1d, start = (/1/),     count = (/npy/) )       )
  CALL check( NF90_GET_VAR(ncid_msk, idv_msk, dmask, start = (/1,1,1/), count=  (/npx, npy,1/)) )
  CALL check( NF90_CLOSE(ncid_msk) )

  !! Create regular metrics
  dconv         = ACOS(-1.d0)/180.d0  ! Pi/180.  (degres --> radians)
  dearth_radius = 6371229.d0   ! meters

  ALLOCATE( de1(npx,npy) , de2(npx,npy) )
  ALLOCATE( dvarin(npx,npy) )
  ALLOCATE( icount(npx,npy) )

  de1 = -1.d0
  de2 = -1.d0

  DO jj=2,npy-1
     DO ji=2,npx-1
     
        dlta_lon = 0.5 * ABS( dlon1d(ji+1) - dlon1d(ji-1) ) * dconv
        dlta_lat = 0.5 * ABS( dlat1d(jj+1) - dlat1d(jj-1) ) * dconv
        de1(ji,jj) = dearth_radius * dlta_lon * COS( dlat1d(jj) *dconv )
        de2(ji,jj) = dearth_radius * dlta_lat

     ENDDO
  ENDDO
!test JMM
de1=1
de2=1

  !! boundaries
  de1(:,1)   = de1(:,2)
  de1(:,npy) = de1(:,npy-1)
  de1(1,:)   = de1(2,:)
  de1(npx,:) = de1(npx-1,:)

  de2(:,1)   = de2(:,2)
  de2(:,npy) = de2(:,npy-1)
  de2(1,:)   = de2(2,:)
  de2(npx,:) = de2(npx-1,:)


  !! Create 2d array for lon and lat
  DO ji=1,npx
     dlat2(ji,:) = dlat1d
  ENDDO

  DO jj=1,npy
     dlon2(:,jj) = dlon1d
  ENDDO

  !! Convention : we set e1 to zero where we want to mask
  SELECT CASE ( cl_zone )

     CASE ('Polar_South')
          WHERE( dlat2 > -70. ) de1(:,:) = 0.d0

     CASE ('Subpolar_South')
          WHERE( dlat2 < -70. ) de1(:,:) = 0.d0
          WHERE( dlat2 > -45. ) de1(:,:) = 0.d0

     CASE ('Subtropical_South')
          WHERE( dlat2 < -45. ) de1(:,:) = 0.d0
          WHERE( dlat2 > -25. ) de1(:,:) = 0.d0

     CASE ('Tropical_South')
          WHERE( dlat2 < -25. ) de1(:,:) = 0.d0
          WHERE( dlat2 > -10. ) de1(:,:) = 0.d0

     CASE ('Equatorial_Band')
          WHERE( dlat2 < -10. ) de1(:,:) = 0.d0
          WHERE( dlat2 >  10. ) de1(:,:) = 0.d0

     CASE ('Tropical_North')
          WHERE( dlat2 < 10. ) de1(:,:) = 0.d0
          WHERE( dlat2 > 25. ) de1(:,:) = 0.d0
     
     CASE ('Subtropical_North')
          WHERE( dlat2 < 25. ) de1(:,:) = 0.d0
          WHERE( dlat2 > 45. ) de1(:,:) = 0.d0

     CASE ('Subpolar_North')
          WHERE( dlat2 < 45. ) de1(:,:) = 0.d0
          WHERE( dlat2 > 70. ) de1(:,:) = 0.d0

     CASE ('Polar_North')
          WHERE( dlat2 < 70. ) de1(:,:) = 0.d0

     CASE ('global')
          ! Nothing to do
     CASE DEFAULT
          PRINT *, 'Uncorrect area'; STOP
  END SELECT

  !! We start the computation
  ifr = 0
  darea = SUM( de1 * de2 * dmask )

  DO jarg=1, nfil

     PRINT *, 'working on '//TRIM(cf_inlist(jarg))
     ! get number of frames
     CALL check( NF90_OPEN(TRIM(cf_inlist(jarg)), NF90_NOWRITE, ncid_in) )
     CALL check( NF90_INQ_DIMID(ncid_in, c_dimtim, idd_timein) )
     CALL check( NF90_INQUIRE_DIMENSION(ncid_in, idd_timein,len=npt) )

     CALL check( NF90_INQ_VARID(ncid_in, TRIM(cv_in), idv_in) )  

     ierr1 = NF90_GET_ATT(ncid_in, idv_in, 'scale_factor', dsf)
     ierr2 = NF90_GET_ATT(ncid_in, idv_in, 'add_offset',   dao)
     !!
     IF ( (ierr1 /= NF90_NOERR) .OR. (ierr2 /= NF90_NOERR) ) THEN
        dsf = 1.d0      ;   dao = 0.d0
     ENDIF

     DO jt=1,npt

        icount=0
        ifr = ifr + 1

        CALL check( NF90_GET_VAR(ncid_in, idv_in, dvarin, start = (/1,1,jt/), count = (/npx, npy, 1 /) ) )

        ! scale factor / offset
        dvarin = (dsf * dvarin) + dao
        dvarin = dratio * dvarin
!       WHERE (dvarin == 0.d0 ) icount=1
!       PRINT *, 'ICOUNT',SUM(icount)

        dts(ifr) = SUM( dvarin * de1 * de2 * dmask ) / darea 
        dtimout(ifr) = dfyear + (jarg-1) + (1. * jt / npt )

     ENDDO

     CALL check( NF90_CLOSE(ncid_in) )

     dym(ifr-npt+1:ifr) = SUM( dts(ifr-npt+1:ifr) ) / npt

  ENDDO

  dgm(:) = SUM( dts(:) ) / nftot


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!  create the netcdf output file

cv_out = TRIM(cv_in)//"_"//TRIM(cl_dtaset)
cf_tsout=TRIM(cl_zone)//"_"//TRIM(cv_in)//"_Dataset_"//TRIM(cl_dtaset)//"_"//TRIM(cl_frstyr)//"-"//TRIM(cl_lastyr)//"_full_timeserie.nc"
cf_ymout=TRIM(cl_zone)//"_"//TRIM(cv_in)//"_Dataset_"//TRIM(cl_dtaset)//"_"//TRIM(cl_frstyr)//"-"//TRIM(cl_lastyr)//"_yearly_timeserie.nc"
cf_gmout=TRIM(cl_zone)//"_"//TRIM(cv_in)//"_Dataset_"//TRIM(cl_dtaset)//"_"//TRIM(cl_frstyr)//"-"//TRIM(cl_lastyr)//"_mean_of_timeserie.nc"

cf_tsout=TRIM(cl_dirout)//"/"//TRIM(cf_tsout)
cf_ymout=TRIM(cl_dirout)//"/"//TRIM(cf_ymout)
cf_gmout=TRIM(cl_dirout)//"/"//TRIM(cf_gmout)

  !! timeserie
  CALL check( NF90_CREATE(TRIM(cf_tsout), NF90_CLOBBER, ncid_ts) )
  CALL check( NF90_DEF_DIM(ncid_ts, 'time', NF90_UNLIMITED, idd_timeout) )
  CALL check( NF90_DEF_VAR(ncid_ts, 'time', NF90_REAL, (/ idd_timeout /), idv_timeout) )
  CALL check( NF90_DEF_VAR(ncid_ts, TRIM(cv_out), NF90_DOUBLE, (/ idd_timeout /), idv_ts) )
  CALL check( NF90_ENDDEF(ncid_ts) )

  CALL check( NF90_PUT_VAR(ncid_ts, idv_timeout, dtimout) )
  CALL check( NF90_PUT_VAR(ncid_ts, idv_ts, dts) )
  CALL check( NF90_CLOSE(ncid_ts) )

  !! yearly means
  CALL check( NF90_CREATE(TRIM(cf_ymout), NF90_CLOBBER, ncid_ym) )
  CALL check( NF90_DEF_DIM(ncid_ym, 'time', NF90_UNLIMITED, idd_timeout) )
  CALL check( NF90_DEF_VAR(ncid_ym, 'time', NF90_REAL, (/ idd_timeout /), idv_timeout) )
  CALL check( NF90_DEF_VAR(ncid_ym, TRIM(cv_out), NF90_DOUBLE, (/ idd_timeout /), idv_ym) )
  CALL check( NF90_ENDDEF(ncid_ym) )

  CALL check( NF90_PUT_VAR(ncid_ym, idv_timeout, dtimout) )
  CALL check( NF90_PUT_VAR(ncid_ym, idv_ym, dym) )
  CALL check( NF90_CLOSE(ncid_ym) )

  !! global mean
  CALL check( NF90_CREATE(TRIM(cf_gmout), NF90_CLOBBER, ncid_gm) )
  CALL check( NF90_DEF_DIM(ncid_gm, 'time', NF90_UNLIMITED, idd_timeout) )
  CALL check( NF90_DEF_VAR(ncid_gm, 'time', NF90_REAL, (/ idd_timeout /), idv_timeout) )
  CALL check( NF90_DEF_VAR(ncid_gm, TRIM(cv_out), NF90_DOUBLE, (/ idd_timeout /), idv_gm) )
  CALL check( NF90_ENDDEF(ncid_gm) )

  CALL check( NF90_PUT_VAR(ncid_gm, idv_timeout, dtimout) )
  CALL check( NF90_PUT_VAR(ncid_gm, idv_gm, dgm) )
  CALL check( NF90_CLOSE(ncid_gm) )


  PRINT *, 'done ! '

CONTAINS

      SUBROUTINE check(status)
        INTEGER(KIND=4), INTENT(in) :: status

        IF(status /= NF90_NOERR) THEN
          PRINT *, TRIM(NF90_STRERROR(status))
          STOP 2
        ENDIF
      END SUBROUTINE check


END PROGRAM dft_timeseries
