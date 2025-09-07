# -*- coding: utf-8 -*-
"""
Created on Fri Jul  4 09:59:16 2025

@author: jcw2g17
"""

# load packages
import os
import requests
from siphon.catalog import TDSCatalog

# define climate model
gcm = "IPSL-CM6A-LR"

# %% List all files for this Global Climate Model under SSP585

# set working directory
os.chdir('E:\cmip6_data\CMIP6\waves_meucci\hs')

# base url for downloads
base_url = "https://data-cbr.csiro.au/thredds/fileServer/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/ssp585/" + gcm + "/CDFAC1/monmean/"

# link catalogue
cat = TDSCatalog("https://data-cbr.csiro.au/thredds/catalog/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/ssp585/" + gcm + "/CDFAC1/monmean/catalog.xml")

# get dataset keys
keys = cat.datasets.keys()

# filter to significant wave height keys
hs_keys = [key for key in keys if '_hs_' in key]

# create paths for each file
path = [base_url+f for f in hs_keys]

# sort files
path.sort()

# local directory to save files
save_dir = "ssp585/" + gcm + "/"
os.makedirs(save_dir, exist_ok=True)


# %% Download files

# loop over all files
for file in path:

    # combine directory with filename
    filepath = os.path.join(save_dir, os.path.basename(file))

    # print download
    print(f"Downloading {filepath}")
    
    # prep download
    r = requests.get(file, stream = True)
    
    # write netCDF    
    with open(filepath, 'wb') as f: 
        for chunk in r.iter_content(chunk_size=10000):
                    f.write(chunk)

# %% List all files for this GCM under SSP126

# set working directory 
os.chdir('E:\cmip6_data\CMIP6\waves_meucci\hs')

# base url for downloads
base_url = "https://data-cbr.csiro.au/thredds/fileServer/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/ssp126/" + gcm + "/CDFAC1/monmean/"

# link catalogue
cat = TDSCatalog("https://data-cbr.csiro.au/thredds/catalog/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/ssp126/" + gcm + "/CDFAC1/monmean/catalog.xml")

# get dataset keys
keys = cat.datasets.keys()

# filter to significant wave height keys
hs_keys = [key for key in keys if '_hs_' in key]

# create paths for each file
path = [base_url+f for f in hs_keys]

# sort files
path.sort()

# local directory to save files
save_dir = "ssp126/" + gcm + "/"
os.makedirs(save_dir, exist_ok=True)

# %% Download files

# loop over all files
for file in path[170:360]:

    # combine directory with filename
    filepath = os.path.join(save_dir, os.path.basename(file))

    # print download
    print(f"Downloading {filepath}")
    
    # prep download
    r = requests.get(file, stream = True)
    
    # write netCDF    
    with open(filepath, 'wb') as f: 
        for chunk in r.iter_content(chunk_size=10000):
                    f.write(chunk)
                    
# %% List all files for this GCM under the historical scenario

# set working directory 
os.chdir('E:\cmip6_data\CMIP6\waves_meucci\hs')

# base url for downloads
base_url = "https://data-cbr.csiro.au/thredds/fileServer/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/historical/" + gcm + "/CDFAC1/monmean/"

# link catalogue
cat = TDSCatalog("https://data-cbr.csiro.au/thredds/catalog/catch_all/oa-cmip6-wave/UniMelb-CSIRO_CMIP6_projections/historical/" + gcm + "/CDFAC1/monmean/catalog.xml")

# get dataset keys
keys = cat.datasets.keys()

# filter to significant wave height keys
hs_keys = [key for key in keys if '_hs_' in key]

# create paths for each file
path = [base_url+f for f in hs_keys]

# sort files
path.sort()

# local directory to save files
save_dir = "historical/" + gcm + "/"
os.makedirs(save_dir, exist_ok=True)

# %% Download files

# loop over all files
for file in path:

    # combine directory with filename
    filepath = os.path.join(save_dir, os.path.basename(file))

    # print download
    print(f"Downloading {filepath}")
    
    # prep download
    r = requests.get(file, stream = True)
    
    # write netCDF    
    with open(filepath, 'wb') as f: 
        for chunk in r.iter_content(chunk_size=10000):
                    f.write(chunk)