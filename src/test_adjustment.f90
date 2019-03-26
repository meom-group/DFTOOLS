PROGRAM adjustqt
  implicit none
  REAL(KIND=4) :: tw, ta,ez,vv, zv,zt
  REAL(kind=4) :: vpr,tpr,epr
  tw=0.    !  SST  (Celsius)
  ta=3.    !  Air temperature  (Celsius)
  ez=0.0033 !  Air specifiv humidity
  vv=12.    !  m/sec

  zv = 10.  ! m
  zt = 2.   ! m

  CALL adjv(tw,ta,ez,vv, zv,zt, vpr,tpr,epr)

  PRINT *, 'before vv ta ez', vv, ta, ez
  PRINT *, 'after vpr ta ez', vpr, tpr, epr
CONTAINS

  SUBROUTINE adjv(ptw,pta,pez,pvv, pzv,pzt, pvpr,ptpr,pepr)
    REAL(kind=4), INTENT(in ) :: ptw, pta,pez,pvv,pzv,pzt
    REAL(kind=4), INTENT(out) :: pvpr, ptpr, pepr
    !******************************************************************
    !          ptw   - sea surface temperature            input        *
    !          pta   - air temperature                     -"-         *
    !          pez   - air humidity                        -"-         *
    !          pvv   - wind velocity                       -"-         *
    !          pzv   - height of wind measurements         -"-         *
    !          pzt   - height of pta/pez measurements        -"-         *
    !          pvpr  - wind speed adjusted               output        *
    !          ptpr  - surface air temperature adjusted    -"-         *
    !          pepr  - humidity adjusted                   -"-         *
    !******************************************************************
    ! local variables :
    REAL(kind=4) :: zgl, zrudin, zrct, zre0
    !******************************************************************

    CALL pot1 (ptw,pta,pez,pvv,    zgl,zrudin,zrct, zre0)
    !---------------------------------------------------------------          
    CALL priv (ptw,pta,pez,pvv,zre0,zgl,zrudin,zrct,pzv,pzt,ptpr,pepr,pvpr) 

    IF (pvpr == 0.) pvpr=0.2
    IF (pvpr <  0.) pvpr=-pvpr
    IF (pvpr > pvv) pvpr=pvv*0.95

  END SUBROUTINE adjv

  !******************************************************************
  SUBROUTINE priv(ptw,pta,pez,pvv,pre0,pgl,prudin,prct,  pzv,pzt,ptpr,pepr,pvpr) 

    REAL(kind=4), INTENT(in ) :: ptw, pta,pez,pvv,pzv,pzt, pgl, prudin, prct, pre0
    REAL(kind=4), INTENT(out) :: pepr,ptpr,pvpr

    ! local variables
    REAL(kind=4) :: zgll, zgll0, zgllt, zgllt0, zpu, zpt, zpe
    REAL(kind=4) :: zfv,zfvp,zft, zftp,zdv,zdtt,zdee
    !******************************************************************
    !***************************************
    !     s t a n d a r d    h e i g h t s   *
    !***************************************
    pepr=pez
    ptpr=pta
    IF(pzt == 10.) RETURN   ! nothing to do already at good level !

    zgll=pzv/pgl
    zgll0=10./pgl
    zgllt=pzt/pgl
    zgllt0=10./pgl 

    zpu=prudin/0.4 
    zpt=(ptw-pta)/(prudin*0.4)*prct
    zpe=((pre0-pez)/(prudin*0.4))*prct

    IF ( zgll > 0. ) THEN
       !*********************************************
       !     universal    function         z/l>0    *
       !*********************************************
       zfv= LOG(zgll)   +10.*zgll
       zfvp=LOG(zgll0)  +10.*zgll0
       zft= LOG(zgllt)  +10.*zgllt
       zftp=LOG(zgllt0) +10.*zgllt0
    ELSE IF ( zgll <= -0.07 ) THEN
       !************************************************
       !     universal     function        z/l<-0.07   *
       !************************************************
       zfv=  0.25-1.2*(ABS(zgll))**(-0.3333333)
       zfvp= 0.25-1.2*(ABS(zgll0))**(-0.3333333)
       zft=  0.25-1.2*(ABS(zgllt))**(-0.3333333)
       zftp= 0.25-1.2*(ABS(zgllt0))**(-0.3333333)
       IF(zpt.LT.0.)zpt=-zpt
       IF(zpe.LT.0.)zpe=-zpe
    ELSE
       !***************************************************
       !     universal     function        -0.07<z/l<0.   *
       !***************************************************
       zfv= LOG(ABS(zgll))
       zfvp=LOG(ABS(zgll0))  
       zft= LOG(ABS(zgllt))
       zftp=LOG(ABS(zgllt0))
    ENDIF

    zdv=(zfv-zfvp)*zpu
    pvpr=pvv-zdv
    zpt=-zpt*pvpr
    zpe=-zpe*pvpr

    zdtt=(zft-zftp)*zpt
    zdee=(zft-zftp)*zpe

    ptpr=pta-zdtt
    pepr=pez-zdee

  END SUBROUTINE priv

  !*****************************************************************
  SUBROUTINE  pot1 (ptw,pta,pez,pvv,  pgl,prudin,prct,pre0)
    !******************************************************************
    !        heat,moisture and momentum fluxes estimates              *
    !              (for individual values)                            *
    !******************************************************************
    !          ptw   - sea surface temperature            input        *
    !          pta   - air temperature                     -"-         *
    !          pez   - air absolute humidity               -"-         *
    !          pvv   - wind velocity                       -"-         *
    !          prct  - heat exchange coefficient           -"-         *
    !          pre0  - humidity near sea surface           -"-         *
    !******************************************************************
    REAL(kind=4), INTENT(in ) :: ptw,pta,pez,pvv
    REAL(kind=4), INTENT(out) :: pgl,prudin,prct,pre0
    ! local variables
    INTEGER, PARAMETER :: jp16=16, jp19=19, jp13=13
    INTEGER, DIMENSION(jp16,jp19) :: iuu
    INTEGER, DIMENSION(jp16,jp13) :: itt
    INTEGER :: ji,jj, jv, jt

    REAL(kind=4), DIMENSION(jp16,jp19) ::  zciuu
    REAL(kind=4), DIMENSION(jp16,jp13) ::  zcitt
    REAL(kind=4), DIMENSION(jp19)      ::  zyvv
    REAL(kind=4), DIMENSION(jp13)      ::  zyvt
    REAL(kind=4), DIMENSION(jp16)      ::  zxdt

    REAL(kind=4) :: zvel, zt1,zt2, ze0log, ztef, zrcu1, zrcu2, zrct1, zrct2, zrcu
    !******************************************************************
    !
    DATA zxdt/-5.,-4.,-3.,-2.,-1.,0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10./
    DATA zyvv/2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14.,15.,16.,         &
         & 17.,18.,19.,20./
    DATA zyvt/2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12.,13.,14./
    DATA iuu/                                                               &
         & 195,197,197,200,201,203,205,206,208,209,210,211,211,212,213,214, &
         & 189,191,193,194,196,198,199,201,202,203,204,205,206,207,208,209, &
         & 182,184,186,188,191,192,194,195,196,197,198,199,200,201,202,203, &
         & 174,177,179,182,184,185,187,190,191,192,193,194,195,196,197,198, &
         & 168,170,173,175,177,179,181,183,185,187,188,189,190,192,193,194, &
         & 161,164,167,170,172,174,176,178,179,181,182,184,186,187,188,189, &
         & 155,158,162,164,167,169,171,173,174,176,178,179,181,182,183,185, &
         & 151,152,156,159,162,164,166,169,171,173,174,176,177,178,180,181, &
         & 144,145,149,153,157,160,162,164,166,169,171,172,173,174,176,179, &
         & 137,136,141,146,151,154,158,161,162,164,166,168,170,172,173,174, &
         & 123,123,132,138,143,148,152,154,158,161,162,163,165,167,169,172, &
         & 109,109,117,125,132,140,143,147,151,153,155,157,159,161,162,164, &
         & 093,094,102,110,118,125,132,137,141,144,145,148,150,153,154,156, &
         & 078,079,086,095,105,114,120,126,129,132,136,138,141,143,145,147, &
         & 065,068,076,085,096,107,115,120,124,127,128,131,133,135,137,139, &
         & 055,057,066,076,089,105,115,121,124,127,128,131,133,134,137,138, &
         & 041,042,054,066,080,105,118,124,127,130,133,136,138,140,142,144, &
         & 029,030,031,051,070,105,125,131,135,140,144,147,150,151,152,153, &
         & 026,027,028,040,058,105,134,143,150,151,152,153,153,154,154,154  &
         & /
    DATA itt/                                                               &
         & 122,124,127,129,131,132,134,136,138,139,140,141,142,142,143,143, &
         & 120,122,124,127,130,131,133,134,136,138,139,140,142,142,143,144, &
         & 114,119,122,124,127,130,132,133,135,138,138,140,142,142,143,144, &
         & 110,113,118,122,125,128,131,132,134,137,138,140,141,142,143,144, &
         & 100,106,112,118,122,126,129,131,133,136,138,139,141,143,144,145, &
         & 091,097,103,112,118,123,127,130,132,135,137,139,141,143,144,145, &
         & 078,086,094,101,112,119,125,128,132,135,137,139,141,143,144,146, &
         & 064,072,082,092,106,116,123,127,131,135,138,139,141,143,145,147, &
         & 052,060,070,085,099,115,123,128,132,136,139,141,143,145,147,148, &
         & 040,048,059,073,092,116,126,132,136,140,143,145,147,149,149,150, &
         & 025,033,044,058,083,117,132,139,143,148,152,154,156,158,158,160, &
         & 018,019,025,040,066,121,141,152,158,164,167,169,172,173,176,177, &
         & 017,018,019,030,045,128,157,170,181,182,181,182,182,182,183,184  &
         & /
    !******************************************************************
    DO   jj=1,jp19
       DO   ji=1,jp16
          zciuu(ji,jj)=float(iuu(ji,jp19+1-jj))/100.
       ENDDO
    ENDDO

    DO    jj=1,jp13
       DO   ji=1,jp16
          zcitt(ji,jj)=float(itt(ji,jp13+1-jj))/100.
       ENDDO
    ENDDO

    zvel=pvv
    !************************************************************
    !     pre0    -    estimation  (wmo - formulae)              *
    !************************************************************
    zt1=273.16/(273.15+ptw)
    zt2=(273.15+ptw)/273.16
    ze0log=10.79574*(1.-zt1)-5.028*log10(zt2)+(1.50475/10000.)*(1-(10. &
         &                  **(-8.2969*(zt2-1.))))+(0.42873/1000.)*((10.**(4.76955*(1.-zt1)))  &
         &                  -1.)+0.78614
    pre0=(10.**ze0log)*0.9815
    !**********************************************************************
    !     effective temperature estimation       *
    !**********************************************************************
    ztef=(ptw-pta)+0.108*(pre0-pez)
    !*********************************
    !     exchange coefficients      *
    !*********************************
    IF(zvel > 30.)  zvel=30.
    IF(zvel <  2.)  zvel=2.
    IF(ztef < -5.)  ztef=-5.
    IF(ztef > 10.)  ztef=10.

    IF ( zvel > 25. ) THEN
       zrcu=(2.67/1000.)-((30.-zvel)*(0.33/1000.))/5.
       prct=(3.53/1000.)-((30.-zvel)*(0.27/1000.))/5.
    ELSEIF ( 25. >= zvel .AND. zvel > 20 ) THEN
       zrcu=(2.37/1000.)-((25.-zvel)*(0.33/1000.))/5.
       prct=(3.26/1000.)-((25.-zvel)*(0.42/1000.))/5.
    ELSE   ! zvel <= 20
       !*****************************************
       !     search of  coefficients  values    *
       !*****************************************
       loop445: DO  jv=1,18
          DO  jt=1,15
             IF (zvel >= zyvv(jv).AND.zvel <= zyvv(jv+1).AND.ztef >= zxdt(jt).AND.ztef <= zxdt(jt+1))  THEN
                zrcu1=(ztef-zxdt(jt))*(zciuu(jt+1,jv)-zciuu(jt,jv))+zciuu(jt,jv)
                zrcu2=(ztef-zxdt(jt))*(zciuu(jt+1,jv+1)-zciuu(jt,jv+1))+zciuu(jt,jv+1)
                zrcu=((zvel-zyvv(jv))*(zrcu2-zrcu1)+zrcu1)/1000.
                EXIT loop445
             ENDIF
          ENDDO
       ENDDO loop445
       IF (  zvel > 14. ) THEN
          !*********************************
          !     storms  conditions         *
          !*********************************
          prct=(2.84/1000.)-((20.-zvel)*1.18/1000.)/5.
       ELSE
          loop446 :DO  jv=1,12
             DO  jt=1,15
                IF (zvel >= zyvt(jv).AND.zvel <= zyvt(jv+1).AND.ztef >= zxdt(jt).AND.ztef <= zxdt(jt+1)) THEN
                   zrct1=(ztef-zxdt(jt))*(zcitt(jt+1,jv)-zcitt(jt,jv))+zcitt(jt,jv)
                   zrct2=(ztef-zxdt(jt))*(zcitt(jt+1,jv+1)-zcitt(jt,jv+1))+zcitt(jt,jv+1)
                   prct=((zvel-zyvt(jv))*(zrct2-zrct1)+zrct1)/1000.
                   EXIT loop446
                ENDIF
             ENDDO
          ENDDO loop446
       ENDIF
    ENDIF

    IF (ztef == 0.) ztef=0.01
    pgl=((273.15+(ptw+pta)/2.)/(3.92*ztef))*(-1.)*SQRT(ABS(zrcu))*(zrcu/prct)*(pvv*pvv)
    prudin=pvv*SQRT(ABS(zrcu))

  END SUBROUTINE pot1
END PROGRAM adjustqt

