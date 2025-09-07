# -*- coding: utf-8 -*-
"""
Created on Mon Jul  7 12:16:43 2025

@author: jcw2g17
"""

# load packages
import xarray as xr
import os
import matplotlib.pyplot as plt


# define global climate model
gcm = "IPSL-CM6A-LR"

# set working directory
os.chdir('E:\cmip6_data\CMIP6\waves_meucci\hs')

# %% Calculate 30-year mean climatologies for this GCM

# 1. Historical

# historical folder path
path = "historical/" + gcm + "/"

# list all files in historical folder
files = os.listdir(path)

# append to folder path
filepaths = [path + f for f in files]

# open all data
full_data = xr.open_mfdataset(filepaths, combine = 'by_coords', parallel = True, engine = "netcdf4")

# group by month and calculate mean for each month across all years
monthly_climatology = full_data['hs'].groupby('time.month').mean(dim='time')

# plot January to check 
monthly_climatology[0].plot()

# export monthly climatology
monthly_climatology.to_netcdf(path = "historical/" + gcm + "_hs_historical_means.nc")


# %% 2. SSP126

# ssp126 folder path
path = "ssp126/" + gcm + "/"

# list all files in historical folder
files = os.listdir(path)

# append to folder path
filepaths = [path + f for f in files]

# open all data
full_data = xr.open_mfdataset(filepaths, combine = 'by_coords', parallel = True, engine = "netcdf4")

# group by month and calculate mean for each month across all years
monthly_climatology = full_data['hs'].groupby('time.month').mean(dim='time')

# plot January to check 
monthly_climatology[0].plot()

# export monthly climatology
monthly_climatology.to_netcdf(path = "ssp126/" + gcm + "_hs_ssp126_means.nc")


# %% 3. SSP585

# ssp126 folder path
path = "ssp585/" + gcm + "/"

# list all files in historical folder
files = os.listdir(path)

# append to folder path
filepaths = [path + f for f in files]

# open all data
full_data = xr.open_mfdataset(filepaths, combine = 'by_coords', parallel = True, engine = "netcdf4")

# group by month and calculate mean for each month across all years
monthly_climatology = full_data['hs'].groupby('time.month').mean(dim='time')

# plot January to check 
monthly_climatology[0].plot()

# export monthly climatology
monthly_climatology.to_netcdf(path = "ssp585/" + gcm + "_hs_ssp585_means.nc")
