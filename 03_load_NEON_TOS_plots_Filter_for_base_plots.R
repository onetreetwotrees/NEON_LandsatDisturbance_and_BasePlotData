library(rgdal)
library(raster)
library(sp)
library(here)
library(dplyr)

setwd("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\plot_data")
options(stringsAsFactor=F)

# Read in plot centroids from NEON plot data
plots <- readOGR("C:\\Users\\janer\\Dropbox\\funding\\Hardiman_2018\\All_NEON_TOS_Plots_V5",layer="All_Neon_TOS_Centroids_V5")
plotsv4 <- read.csv("All_Neon_TOS_Centroid_V4.csv",header=T)

# Look at data
head(plots)

sites <- sort(unique(plots$siteID))
plotTypes <- sort(unique(paste(plots$plotType,plots$subtype,sep="-")))
psites <- c("BART","HARV","SCBI","BLAN","OSBS","JERC","GUAN",
            "UNDE","STEI","UKFS","GRSM","TALL","ABBY","SOAP")
# Select some sites to work with as test sites, based on disturbance history.
tsites <- c("SCBI","OSBS","GRSM")
table(plots$siteID)
plotsdf <- plots@data

# subset data frame and plots file to test sites
tplotsdf <- plotsdf %>% filter(siteID %in%  tsites & plotType == "tower" & subtype == "basePlot")
tplots <- plots[which(plots$siteID %in% tsites & plots$plotType == "tower" & plots$subtype == "basePlot"),]
pplotsdf <- plotsdf %>% filter(siteID %in%  psites & plotType == "tower" & subtype == "basePlot")
dim(ptplots)

# Write to shape file to download Landsat data from GEE
writeOGR(tplots,dsn="gis", layer="NEON_tower_base_plots_test_sites_LL", driver="ESRI Shapefile")
writeOGR(tplots,dsn="gis\\NEON_tower_base_plots_test_sites_LL_kml",layer="NEON_tower_base_plots_test_sites_LL_kml",driver="KML")