#!/usr/bin/env python
#
# (C) Copyright 2012-2013 ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.
#
# To run this example, you need an API key 
# available from https://api.ecmwf.int/v1/key/
#
# R.Dussin 2014 - adapt script to download ERAinterim
# J.M. Molines 2019 - adapt Raphael script for DFTOOLS
#
"""
  dft_get_ERAinterim_grib.py
  Get ERAinterim grib files from ECMMF server for the ad hoc variables used in NEMO.
     R.Dussin 2014 - adapt script to download ERAinterim
    J.M. Molines 2019 - adapt Raphael script for DFTOOLS
"""

from ecmwfapi import ECMWFDataServer
import numpy as np
import ConfigParser
import os 
import sys, getopt

def usage(name):
   print ' '
   print 'USAGE: '+name+' -h  -f <first_year> -l <last_year> [ -h ] [-g <GRIB-directory> ]'
   print ' '
   print '  PURPOSE:'
   print '     This python script ends up with a set of grib files, downloaded from the '
   print '     ECMWF MARS server. The downloaded files, corresponding to suitable variables'
   print '     for NEMO forcing, will be processed later by DFTOOLS, in order to obtain the' 
   print '     netcdf set of files used by NEMO.'
   print ' '
   print '  ARGUMENTS:'
   print '     -f < first_year> : indicate the first_year to download.'
   print '     -l < last_year> : indicate the last _year to download.'
   print ' '
   print '  OPTIONS:'
   print '     -h : Display this help message'
   print '     -g <GRIB-directory> : define the GRIB directory where the grib files will'
   print '              be written. Default: ', gribdir
   print ' '
   sys.exit()

def set_default():
   global gribdir
   global fyear
   global lyear
   # set default
   gribdir = './'
   fyear = -1
   lyear = -1


def parse(argv,name):

   try:
      opts, args = getopt.getopt(argv,"hf:l:g:",["help","first_year=","last_year=","grib_directory="])
   except getopt.GetoptError:
      usage(name)

   for opt, arg in opts:
      if opt in ("-h", "--help"):
         usage(name)
      elif opt in ("-f", "--first_year"):
         fyear = arg
      elif opt in ("-l", "--last_year"):
         lyear = arg
      elif opt in ("-g", "--grib_directory"):
         gribdir = arg
    
   if fyear == -1 :
       usage(name)
   if lyear == -1 :
       usage(name)

def get_era_interim(fyear,lyear,gribdir):
   # Define the list of interesting variables with their associated parameter on MARS server
   tab_ref='128' 
   ecmwf_param = {'u10' : '165', 'v10'  : '166', 'd2'    : '168', 't2'    : '167', \
                  'msl' : '151', 'snow' : '144', 'radsw' : '169', 'radlw' : '175',  \
               'precip' : '228', 'tcc'  : '164'}

   server = ECMWFDataServer()

   for year in np.arange(int(fyear),int(lyear)+1):

        for key in ecmwf_param.keys():

                print 'working on variable', key, ' for year ', str(year)
                filegrib = gribdir + '/' + key + '_ERAinterim_y' + str(year) + '.grib'

                server.retrieve({
                    'stream'    : "oper",
                    'levtype'   : "sfc",
                    'resol'     : "av",
                    'param'     : ecmwf_param[key]+'.'+tab_ref ,
                    'dataset'   : "interim",
                    'step'      : "3/6/9/12",
                    'time'      : "00/12",
                    'date'      : str(year) + "-01-01/to/" + str(year) + "-12-31",
                    'type'      : "fc",
                    'class'     : "ei",
                    'target'    : filegrib
                })

if __name__ == "__main__":
   set_default()
   if len(sys.argv) == 1:
      usage(sys.argv[0])
   parse(sys.argv[1:],os.path.basename(sys.argv[0]) )
   get_era_interim(fyear,lyear,gribdir)
