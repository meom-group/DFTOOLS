# How to retrieve ERAinterim reanalysis from ECMWF server ?

# How to prepare ECMWF reanalysis fields for being DRAKKAR-NEMO complient ?
## 1. Rename variables to DRAKKAR standards:
  * use [dft_renamevar.sh](../scripts/dft_renamevar.sh) script.

   ```

   USAGE : dft_renamevar.sh go 

     PURPOSE:
       Rename ECMWF variable for atmospheric fields to DRAKKAR used names.
       This script scans all the files downloaded from ECMWF  and changes 
       the variable names for : d2 msl radlw radsw snow precip t2 tcc u10 v10
       This script must be used in the directory where the files are.

     ARGUMENT:
       In fact any character does the job. Without arg, display this message.

   ```

## 2. Variables u10 v10 t2 msl
  * These variables are now ready to use.
 
## 3. Compute q2 from dewpoint temperature and mean sea level pressure  
  * Need to compute q2 (humidity) from d2 and msl: use [dft_q2_comp](../src/dft_q2_comp.f90)

    ```
    USAGE : dft_q2_comp -d2 DEWPOINT-file -msl MSL-file [-d2v D2-var]
          [-mslv MSL-var] [-o Q2-file ] [-lon DIM-lon] [-lat DIM-lat]
          [-time DIM-time]
 
      PURPOSE: 
        Compute humidity at 2m (q2) from the dewpoint temperature (d2) and 
        corresponding mean sea level pressure (msl).
 
      ARGUMENTS:
         -d2 DEWPOINT-file : name of dewpoint temperature file
         -msl MSL-file     : name of mean sea level pressure file
 
      OPTIONS:
         -d2v D2-var   : give name of dewpoint variable [default: d2]
         -mslv MSL-var : give name of mean sea level pressure variable.
                                  [default: msl]
         -o Q2-file    : give name of the output file.
                                  [default deduced from d2 file]
         -lon DIM-lon  : give name of longitude dimension in the input files. 
                                 [default: lon ]
         -lat DIM-lat  : give name of latitude dimension in the input files. 
                                 [default: lat ]
         -time DIM-time : give name of time dimension in the input files. 
                                 [default: time ]
 
      OUTPUT:
        netcdf file Q2-file (like DEWPOINT-file, with d2 changed to q2)
               variable :  q2 
               units    :  kg/kg 

    ```

## 4. Variables precip and snow , radiative heat fluxes (radlw and radsw)
  * Need to decumulate   and build daily file using [dft_decumul_daily](../src/dft_decumul_daily.f90)  
    See also this [dft_decumul_daily.sh](../scripts/dft_decumul_daily.sh) script for automatic treatment.

    ``` 
  
  USAGE : dft_decumul_daily  -f CUMUL-file -v CUMUL-var -u OUT-units 
          [-lon DIM-lon] [-lat DIM-lat] [-tim DIM-time]
  
    PURPOSE:
        Process the 3-hourly cumulated files (e.g precip, snow, ssrd, strd)
        in order to build an un-cumulated daily file (units are changed).
  
    METHOD:
        For each day, in the 3-hourly ERAinterim files, value at 12h is the
        cumulated values from 0h to 12h, and the value at 24h are the cumulated
        values from 12h to 24h. Hence, the daily cumul is the sum of value at
        12h + value at 24h. The daily mean rate is obtained by dividing this
        sum by the number of seconds in a day (86400).
  
    WARNING :
        Strong assumptions about the cumulating process are done for this code.
        (see method above). Be sure that your data follow the same pattern.
  
    ARGUMENTS:
        -f CUMUL-file : name of the yearly file with 3h cumulated values.
        -v CUMUL-var : name of the variable to process in CUMUL-file.
        -u OUT-units : units of the output field (rate).
  
    OPTIONS:
        -lon DIM-lon ; give name of longitude dimension, 
                   default :[lon]
        -lon DIM-lat ; give name of latitude dimension, 
                   default :[lat]
        -tim DIM-time ; give name of time dimension, 
                   default :[time]
  
    OUTPUT:
        netcdf file : <CUMUL_file>_daily 
        variable    : <CUMUL_var> 
        units       : Original units/ s
    ```

  * Need to change units only for `precip` and `snow`( from `m/s` to `kg/m2/s`)   
    This is just a division by 1000. Script [dft_ch_unit_precip.sh](../scripts/dft_ch_unit_precip.sh) can be used for this easy task (use ncap2 and ncatted)
  
