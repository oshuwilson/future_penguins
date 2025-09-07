# -*- coding: utf-8 -*-
"""
Created on Wed Jul 16 15:29:21 2025

@author: jcw2g17
"""

# load libraries
import intake
import xarray as xr
import os
import cmip6_preprocessing
import pandas

# List GCM and CMIP varname
institute = "NOAA-GFDL"
gcm = "GFDL-ESM4"
cmip_var = "tos"

# Load the CMIP6 Pangeo intake catalog
col_url = "https://storage.googleapis.com/cmip6/pangeo-cmip6.json"
col = intake.open_esm_datastore(col_url)
col.df.columns

# list available GCMs
col.df["source_id"].unique()

# %% Historical Search

# define search query
query = dict(
    source_id=gcm,
    experiment_id="historical",
    table_id="Omon",        
    variable_id=cmip_var,
    grid_label="gn"
)

# Rrun search query
cat = col.search(**query)

# %% Historical Checks

# available entries
print(f"Number of datasets found: {len(cat.df)}")

# check the member ID
print(cat.df["member_id"])

# check the grid label
print(cat.df["grid_label"])

# %% Historical Data

# limit to the smallest member ID available
cat = cat.search(member_id = "r1i1p1f1")

# convert the catalogue to a dictionary of datasets
dsets = cat.to_dataset_dict(zarr_kwargs={"consolidated": True}, storage_options={"token": "anon"})

# bank the catalogue while checking SSPs
hist_dsets = dsets


# %% SSP126 Search

# define search query
query = dict(
    source_id=gcm,
    experiment_id="ssp126",
    table_id="Omon",        
    variable_id=cmip_var,
    grid_label="gn"
)

# run search query
cat = col.search(**query)

# %% SSP126 Checks

# available entries
print(f"Number of datasets found: {len(cat.df)}")

# check the member ID
print(cat.df["member_id"])

# check the grid label
print(cat.df["grid_label"])

# %% SSP126 Data

# limit to the smallest member ID available
cat = cat.search(member_id = "r1i1p1f1")

# convert the catalogue to a dictionary of datasets
dsets = cat.to_dataset_dict(zarr_kwargs={"consolidated": True}, storage_options={"token": "anon"})

# bank the catalogue
ssp126_dsets = dsets


# %% SSP585 Search

# define search query
query = dict(
    source_id=gcm,
    experiment_id="ssp585",
    table_id="Omon",        
    variable_id=cmip_var,
    grid_label="gn"
)

# run search query
cat = col.search(**query)

# %% SSP585 Checks

# available entries
print(f"Number of datasets found: {len(cat.df)}")

# check the member ID
print(cat.df["member_id"])

# check the grid label
print(cat.df["grid_label"])

# %% SSP585 Data

# limit to the smallest member ID available
cat = cat.search(member_id = "r1i1p1f1")

# convert the catalogue to a dictionary of datasets
dsets = cat.to_dataset_dict(zarr_kwargs={"consolidated": True}, storage_options={"token": "anon"})

# bank the catalogue
ssp585_dsets = dsets


# %% Download Historical

# define output directory
output_dir = "E:/cmip6_data/CMIP6/CMIP/" + institute + "/" + gcm + "/historical/r1i1p1f1/Omon/" + cmip_var 

# create directory
os.makedirs(output_dir, exist_ok = True)

for key, ds in hist_dsets.items():
    print(f"Saving dataset: {key}")
    ds = ds.squeeze()  # remove length-1 dimensions
    filename = f"{output_dir}/{key.replace('/', '_')}.nc"
    ds.to_netcdf(filename)
    print(f"Saved to: {filename}")
    
    
# %% Download SSP126

# define output directory
output_dir = "E:/cmip6_data/CMIP6/ScenarioMIP/" + institute + "/" + gcm + "/ssp126/r1i1p1f1/Omon/" + cmip_var 

# create directory
os.makedirs(output_dir, exist_ok = True)

for key, ds in ssp126_dsets.items():
    print(f"Saving dataset: {key}")
    ds = ds.squeeze()  # remove length-1 dimensions
    filename = f"{output_dir}/{key.replace('/', '_')}.nc"
    ds.to_netcdf(filename)
    print(f"Saved to: {filename}")
    

# %% Download SSP585

# define output directory
output_dir = "E:/cmip6_data/CMIP6/ScenarioMIP/" + institute + "/" + gcm + "/ssp585/r1i1p1f1/Omon/" + cmip_var 

# create directory
os.makedirs(output_dir, exist_ok = True)

for key, ds in ssp585_dsets.items():
    print(f"Saving dataset: {key}")
    ds = ds.squeeze()  # remove length-1 dimensions
    filename = f"{output_dir}/{key.replace('/', '_')}.nc"
    ds.to_netcdf(filename)
    print(f"Saved to: {filename}")