#-------------------------------------------------------------------------------
# Create mosaic of different Gentoo predictions
#-------------------------------------------------------------------------------

rm(list=ls())
setwd("~/OneDrive - University of Southampton/Documents/Chapter 03")

library(tidyverse)
library(terra)
library(tidyterra)

#-------------------------------------------------------------------------------
# Present-day predictions
#-------------------------------------------------------------------------------

# load rasters
northern <- rast("output/climatic model/predictions/GEPE_northernmost_simple_ensemble.tif")
crozet <- rast("output/climatic model/predictions/GEPE_crozet_simple_ensemble.tif")
kerguelen <- rast("output/climatic model/predictions/GEPE_kerguelen_simple_ensemble.tif")
midrange <- rast("output/climatic model/predictions/GEPE_midrange_simple_ensemble.tif")
southern <- rast("output/climatic model/predictions/GEPE_southern_simple_ensemble.tif")

# crop crozet raster
crozet <- crozet %>% crop(ext(45, 60, -50, -40))
plot(crozet)

# crop kerguelen raster
kerguelen <- kerguelen %>% crop(ext(60, 75, -51, -40))
plot(kerguelen)

# crop northern raster to include marion, gough, and nz subantarctic islands
marion <- northern %>% crop(ext(-15, 45, -50, -40))
plot(marion)
gough <- northern %>% crop(ext(-15, -5, -42, -40))
plot(gough)
nz_subantarctic <- northern %>% crop(ext(163, 180, -59, -40))
plot(nz_subantarctic)

# crop midrange raster around South Georgia, Macquarie, Chilean Islands and Falklands
falklands <- midrange %>% crop(ext(-65, -55, -55, -40))
plot(falklands)
chile <- midrange %>% crop(ext(-80, -65, -58, -40))
plot(chile)
south_georgia <- midrange %>% crop(ext(-48, -30, -59, -50))
plot(south_georgia)
macquarie <- midrange %>% crop(ext(155, 163, -59, -50))
plot(macquarie)

# crop southern raster around Antarctica, South Sandwich, Heard, and Bouvet
antarctica <- southern %>% crop(ext(-180, 180, -80, -59))
plot(antarctica)
south_sandwich <- southern %>% crop(ext(-30, -20, -62, -55))
plot(south_sandwich)
heard <- southern %>% crop(ext(70, 80, -59, -51))
plot(heard)
bouvet <- southern %>% crop(ext(0, 10, -55, -53))
plot(bouvet)

# create mosaic
gentoos <- mosaic(crozet, kerguelen, marion, gough, nz_subantarctic,
                  falklands, chile, south_georgia, macquarie,
                  antarctica, south_sandwich, heard, bouvet, southern,
                  fun = "max")
plot(gentoos)

# save raster
writeRaster(gentoos, "output/climatic model/predictions/GEPE_chick-rearing_simple_ensemble.tif",
            overwrite=TRUE)
plot(gentoos %>% project("epsg:6932"))


#-------------------------------------------------------------------------------
# Future Predictions
#-------------------------------------------------------------------------------

# cleanup
rm(list=ls())

# list of all GCMs
gcms <- c("ACCESS-ESM1-5", "CanESM5", "CESM2-WACCM", "HadGEM3-GC31-LL", 
          "IPSL-CM6A-LR", "MRI-ESM2-0", "NorESM2-MM", "UKESM1-0-LL")

# loop over gcms
for(gcm in gcms){
  print(gcm)
  
  # loop over scenarios
  for(scenario in c("ssp126", "ssp585")){
    print(scenario)
    
    # load rasters
    northern <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_northernmost_simple_ensemble.tif"))
    crozet <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_crozet_simple_ensemble.tif"))
    kerguelen <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_kerguelen_simple_ensemble.tif"))
    midrange <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_midrange_simple_ensemble.tif"))
    southern <- rast(paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_southern_simple_ensemble.tif"))
    
    # crop crozet raster
    crozet <- crozet %>% crop(ext(45, 60, -50, -40))
    plot(crozet)
    
    # crop kerguelen raster
    kerguelen <- kerguelen %>% crop(ext(60, 75, -51, -40))
    plot(kerguelen)
    
    # crop northern raster to include marion, gough, and nz subantarctic islands
    marion <- northern %>% crop(ext(-15, 45, -50, -40))
    plot(marion)
    gough <- northern %>% crop(ext(-15, -5, -42, -40))
    plot(gough)
    nz_subantarctic <- northern %>% crop(ext(163, 180, -59, -40))
    plot(nz_subantarctic)
    
    # crop midrange raster around South Georgia, Macquarie, Chilean Islands and Falklands
    falklands <- midrange %>% crop(ext(-65, -55, -55, -40))
    plot(falklands)
    chile <- midrange %>% crop(ext(-80, -65, -58, -40))
    plot(chile)
    south_georgia <- midrange %>% crop(ext(-48, -30, -59, -50))
    plot(south_georgia)
    macquarie <- midrange %>% crop(ext(155, 163, -59, -50))
    plot(macquarie)
    
    # crop southern raster around Antarctica, South Sandwich, Heard, and Bouvet
    antarctica <- southern %>% crop(ext(-180, 180, -80, -59))
    plot(antarctica)
    south_sandwich <- southern %>% crop(ext(-30, -20, -62, -55))
    plot(south_sandwich)
    heard <- southern %>% crop(ext(70, 80, -59, -51))
    plot(heard)
    bouvet <- southern %>% crop(ext(0, 10, -55, -53))
    plot(bouvet)
    
    # create mosaic
    gentoos <- mosaic(crozet, kerguelen, marion, gough, nz_subantarctic,
                      falklands, chile, south_georgia, macquarie,
                      antarctica, south_sandwich, heard, bouvet, southern,
                      fun = "max")
    plot(gentoos)
    
    # write raster
    writeRaster(gentoos, paste0("output/climatic model/projections/", scenario, "/", gcm, "/GEPE_", gcm, "_", scenario, "_simple_ensemble.tif"),
                overwrite=TRUE)
  }
}
