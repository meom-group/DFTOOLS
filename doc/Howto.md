# How to retrieve reanalysis from ECMWF server ?
## ERA interim
## ERA5

# How to prepare ECMWF reanalysis fields for being DRAKKAR-NEMO complient ?
## ERA interim
### 1. Rename variables to DRAKKAR standards:
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

### 2. Variables u10 v10 t2 msl
  * These variables are now ready to use.
 
### 3. Compute q2 from dewpoint temperature and mean sea level pressure  
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

### 4. Variables precip and snow :
  * Need to decumulate   
  * Need to change units ( from `m` to `kg/m2/s`)
  * Need to produce daily files instead of 3-hourly (why ? )

### 5. Radiative fluxes
  * Need to decumulate   
  * Need to change units ( from `J/m2` to `W/m2`)
  * Need to produce daily files instead of 3-hourly (why ? )
  
## ERA5
