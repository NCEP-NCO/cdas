--#%Module#####################################################
--#############################################################
--##                                 wesley.ebisuzaki@noaa.gov
--##                                 CPC/NCEP
--##                                 CPC/NCEP
--##  cdas.v1.2.0
--#############################################################
--proc ModulesHelp { } {
--  puts stderr "Set environment variables for compiling cdas"
--  puts stderr "This module initializes the users "
--  puts stderr " environment to compile on WCOSS2 machine at NCEP"
--}

-- Load Intel Compiler

load("intel/"..os.getenv("intel_ver"))

-- Load Supporting Software Libraries

load("bufr/"..os.getenv("bufr_ver"))
load("bacio/"..os.getenv("bacio_ver"))
load("w3nco/"..os.getenv("w3nco_ver"))
load("w3emc/"..os.getenv("w3emc_ver"))

