# How to retrieve reanalysis from ECMWF server ?
## ERA interim
## ERA5

# How to prepare ECMWF reanalysis fields for being DRAKKAR-NEMO complient ?
## ERA interim
###  u10 v10 t2 msl
 Just rename the variables in the files:
 
 ```
      U10M --> u10
      V10M --> v10
      T2M  --> t2
      MSL  --> msl
 ```
 
### d2 msl
  Need to compute q2 (humidity) from d2 and msl
### precip snow :
  Need to decumulate   
  Need to change units ( from `m` to `kg/m2/s`)
### radiative fluxes
  Need to decumulate   
  Need to change units ( from `J/m2` to `W/m2`)
  Need to rename variables:  
  
  ```
      SSRD --> radsw
      STRD --> radlw
  ```
  
## msl
## ERA5
