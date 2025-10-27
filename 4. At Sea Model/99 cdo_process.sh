#!/bin/bash

# set working directory prior to script to folder containing desired files e.g.
cd Downloads/ACCESS-ESM1-5

# resample to 1x1 degree grid 
for i in *.nc; do  echo $i; cdo remapycon,global_1 "$i" "${i%.nc}_bil_1x1.nc"; done

# ! IMPORTANT ! #
# check the output and delete the original files manually in file explorer before next step
# ! IMPORTANT ! #

# limit to 30-years for historical files
for i in *historical*.nc; do echo $i; cdo seldate,1985-01-01,2014-12-31 "$i" "${i%.nc}_30yr.nc"; done

# do the same for SSP126 and SSP585 files
for i in *ssp*.nc; do echo $i; cdo seldate,2070-01-01,2099-12-31 "$i" "${i%.nc}_30yr.nc"; done

# calculate climatologies for 30-year files
for i in *30yr*.nc; do echo $i; cdo ymonmean "$i" "${i%bil_1x1_30yr.nc}_climatology.nc"; done

# remove 30yr files
for i in *30yr*.nc; do echo $i; rm $i; done

# calculate deltas for SSP126 - might need to change the output file name based on download names
for i in *ssp126**climatology*.nc; do echo $i; var=$(basename "$i" | cut -d'_' -f1); echo $var; hist_file=$(ls *historical*climatology*.nc 2>/dev/null | grep "^${var}_" || true); echo $hist_file; cdo sub $i $hist_file "${i%_gn_201501-210012__climatology.nc}_delta.nc"; done

# repeat for SSP585
for i in *ssp585**climatology*.nc; do echo $i; var=$(basename "$i" | cut -d'_' -f1); echo $var; hist_file=$(ls *historical*climatology*.nc 2>/dev/null | grep "^${var}_" || true); echo $hist_file; cdo sub $i $hist_file "${i%_gn_201501-210012__climatology.nc}_delta.nc"; done

# remove climatology files
for i in *climatology*.nc; do echo $i; rm $i; done

# refer to the R script (move_cmip_files) to organise output files into folders


#------------------------------------------
# Additional Functions
#------------------------------------------

# remove an extra variable (e.g. "area" in IPSL models)
for i in *.nc; do  echo $i; cdo delname,area "$i" "${i%.nc}_del.nc"; done

# show available information (including depth levels)
for i in *.nc; do  echo $i; cdo sinfo $i; done

# select depth levels (e.g. 500cm in CESM2-WACCM)
for i in *.nc; do  echo $i; cdo sellevel,1027.22 "$i" "${i%.nc}_sel.nc"; done

# combining north-south and east-west vectors (e.g. uo and vo)
for i in *.nc; do echo $i; cdo mul "$i" "$i" "${i%.nc}_squared.nc"; done # square
for uo in *uo*squared*.nc; do echo $uo; vo="${uo/uo/vo}"; echo $vo; cdo add "$uo" "$vo" "${uo/uo/curr}"; done # add
for curr in *curr*.nc; do echo $curr; cdo sqrt "$curr" "${curr%_squared.nc}.nc"; done # square root

# combining files from multiple decades into one file (change dates as necessary)
for i in *206101-207012*.nc; do echo $i; second="${i/206101-207012/207101-208012}"; echo $second; third=${i/206101-207012/208101-209012}; echo $third; fourth="${i/206101-207012/209101-210012}"; echo $fourth; cdo mergetime $i $second $third $fourth "${i/206101-207012/206101-210012}"; done

for i in *198101-199012*.nc; do echo $i; second="${i/198101-199012/199101-200012}"; echo $second; third=${i/198101-199012/200101-201012}; echo $third; fourth="${i/198101-199012/201101-201412}"; echo $fourth; cdo mergetime $i $second $third $fourth "${i/198101-199012/198101-201412}"; done

# UKESM combine decades
for i in *195001-199912*.nc; do echo $i; second="${i/195001-199912/200001-201412}"; echo $second; cdo mergetime $i $second "${i/195001-199912/195001-201412}"; done