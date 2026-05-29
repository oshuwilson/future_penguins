# -*- coding: utf-8 -*-
"""
Created on Mon Jul 14 10:38:33 2025

@author: jcw2g17
"""

# load packages
import rasterio
from rasterio import windows
from rasterio.features import rasterize
import geopandas as gpd
import numpy as np
from scipy.ndimage import distance_transform_edt
from tqdm import tqdm
import os

# change working directory
os.chdir('E:\Satellite_Data\static\DEM\REMA_100m_cropped')

# file paths
dem_path = "REMA_100m_elevation_cropped.tif"
coastline_path = "coastline.shp"
output_path = "dist2coast.tif"

# Open DEM and coastline
with rasterio.open(dem_path) as src:
    meta = src.meta.copy()
    transform = src.transform
    crs = src.crs
    width = src.width
    height = src.height
    res = src.res[0]  # assumes square pixels

# Load and reproject coastline
coastline = gpd.read_file(coastline_path).to_crs(crs)

# Tile size and buffer in pixels
tile_size = 1000

# %% Computation

# function to create rasters
def process_quadrant(name, row_start, row_end, col_start, col_end, src, meta, buffer=200):
    output_path = f'distance_to_coast_{name}.tif'

    height = row_end - row_start
    width = col_end - col_start
    transform = src.window_transform(windows.Window(col_start, row_start, width, height))

    meta.update({
        'height': height,
        'width': width,
        'transform': transform,
        'nodata': -1,
        'dtype': 'float32',
        'compress': 'lzw'
    })

    with rasterio.open(output_path, 'w', **meta) as dst:
        for row_off in tqdm(range(row_start, row_end, tile_size), desc=f"Quadrant {name} rows"):
            for col_off in range(col_start, col_end, tile_size):
                win_width = min(tile_size, col_end - col_off)
                win_height = min(tile_size, row_end - row_off)

                # Extended window with buffer
                ext_row_off = max(0, row_off - buffer)
                ext_col_off = max(0, col_off - buffer)
                ext_row_end = min(src.height, row_off + win_height + buffer)
                ext_col_end = min(src.width, col_off + win_width + buffer)
                ext_win = windows.Window(
                    ext_col_off,
                    ext_row_off,
                    ext_col_end - ext_col_off,
                    ext_row_end - ext_row_off
                )

                # Compute transform for extended window
                ext_transform = windows.transform(ext_win, src.transform)
                ext_shape = (int(ext_win.height), int(ext_win.width))

                # Rasterize coastline into extended buffer
                coast_mask_ext = rasterize(
                    [(geom, 1) for geom in coastline.geometry],
                    out_shape=ext_shape,
                    transform=ext_transform,
                    fill=0,
                    dtype='uint8',
                    all_touched=True
                )

                # Distance transform
                dist_pixels_ext = distance_transform_edt(coast_mask_ext == 0)
                dist_meters_ext = dist_pixels_ext * 100  # 100m resolution

                # Crop out the center tile from the buffered result
                row_start_in_buffer = row_off - ext_row_off
                col_start_in_buffer = col_off - ext_col_off
                dist_crop = dist_meters_ext[
                    row_start_in_buffer:row_start_in_buffer + win_height,
                    col_start_in_buffer:col_start_in_buffer + win_width
                ]

                # Write the cropped distance values to the destination raster
                dst.write(dist_crop.astype('float32'), 1,
                          window=windows.Window(col_off - col_start, row_off - row_start, win_width, win_height))
                
                
# %% Process each quadrant one at a time
with rasterio.open(dem_path) as src:
    meta = src.meta.copy()
    height = src.height
    width = src.width

    mid_row = height // 2
    mid_col = width // 2

    # Define quadrant bounds and names
    quadrants = {
        'NW': (0, mid_row, 0, mid_col),
        'NE': (0, mid_row, mid_col, width),
        'SW': (mid_row, height, 0, mid_col),
        'SE': (mid_row, height, mid_col, width)
    }

    for name, (r0, r1, c0, c1) in quadrants.items():
        process_quadrant(name, r0, r1, c0, c1, src, meta)