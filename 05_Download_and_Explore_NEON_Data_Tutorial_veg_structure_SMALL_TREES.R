## ----packages, eval=FALSE-----------------------------------------------------------------------------------------------------
## 
## install.packages("devtools")
## install.packages("neonUtilities")
## install.packages("raster")
## devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
## install.packages("BiocManager")
## BiocManager::install("rhdf5")
## 


## ----setup, results='hide', message=FALSE, warning=FALSE----------------------------------------------------------------------

# load packages
library(rgdal)
library(neonUtilities)
#install.packages("devtools")
#library(devtools)
#install_github("NEONScience/NEON-geolocation/geoNEON")
#install.packages('geoNEON')
library(geoNEON)
library(raster)
#library(rhdf5)
library(hdf5r)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(alluvial)


# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)

makepdf <- "yes" # or "no"
# Modify the file path to match the path to your zip file - Run this again if you want to update your download for new data
#stackByTable("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant.zip")

# Set a save path to match path for downloaded files
save_path <- "C:/Users/janer/Dropbox/code/Rcode/NEON_tutorials/data"
setwd(save_path)
#Here, we'll download one tile of Ecosystem structure (Canopy Height Model) (DP3.30015.001) from WREF in 2017.

#We'll read in the vst_mappingandtagging and vst_apparentindividual files:

vegmap0 <- readTableNEON("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\vst_mappingandtagging.csv",
                        "C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vegmap0)

# Try excluding rows with pointID == NA to get getLosTOS to work
#vegmap <- vegmap0[which(!(is.na(vegmap0$pointID))),]
# The above line subsamples down to only mapped individuals. But we want all individual records for plot summaries!

vegind <- readTableNEON("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\vst_apparentindividual.csv",
                        "C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vegind)
vstvar <- read.csv("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vstvar)
vstval <- read.csv("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\validation_10098.csv")
#View(vstval)
vplot0 <- read.csv("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\vst_perplotperyear.csv")
#First, use the geoNEON package to calculate stem locations:

names(vegmap)
# If you want to update locations with updated data, run line below
#vegmap <- geoNEON::getLocTOS(vegmap, "vst_mappingandtagging")
# Otherwise, read in from saved file
vegmap <-  read.csv("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", header=T)
names(vegmap)
vegmapOut <- vegmap
# Stored vegmap is only for mapped individuals. Use all for plot-level summaries
vegmap <- vegmap0

# Need to save a version of vegmap with locations so you don't have to re-run above lengthly step each time.
dim(vegmap)
#write.csv(vegmap, "NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", row.names=F)

#And now merge the mapping data with the individual measurements. individualID is the linking variable, the others are included to avoid having duplicate columns.
veg <- merge(vegind, vegmap, by=c("individualID","namedLocation",
                                  "domainID","siteID","plotID"))

# OR don't worry about mapping right now, select data for a particular site
#veg <- veg %>% dplyr::filter(siteID == "GRSM")

#Using the merged data, now we can map the stems in plot 85 (plot chosen at random). Note that the coordinates are in meters but stem diameters are in cm.
list_plant_status <- levels(as.factor(vegind$plantStatus))
#col_plant_status <- as.numeric(as.factor(veg$plantStatus))
table(veg$plantStatus)

plant_status_symbol <- c(17,3,21,21,10,21,21,21,8,13,12,9,7,4,5,17)
plant_status_bg <- c("gray",1,"green","orange","olivedrab","purple","red","gray",1,1,1,1,1,1,1,"red")
plant_status_vis <- data.frame(plant_status = list_plant_status,plant_status_symbol=plant_status_symbol,plant_status_bg=plant_status_bg)

veg0 <- veg

# Create species codes from scientific names
names(veg0)
# Create a list of distinct species to generate 8 digits species codes
spp_distinct <- veg0 %>% dplyr::select(taxonID, scientificName) %>% distinct() %>% arrange(taxonID)

splist <- strsplit(spp_distinct$scientificName, " ")
spplist <- c()
for (j in 1:length(splist)) {
  spj <- unlist(splist[j])
  spj <- paste(substr(spj[1],1,4), substr(spj[2],1,4), sep = "")
  spj <- tolower(spj)
  spplist <- c(spplist, spj)
}

spp_distinct$spp <- spplist

veg0 <- veg0 %>% inner_join(spp_distinct)
veg <- veg0 %>% inner_join(plant_status_vis,by=c("plantStatus"="plant_status"))

col_plant_status <- veg$plant_status_bg

# Quick tabulation of number of distinct years vegetation structure was measured by plotID
#plot_date <- vegind %>% group_by(plotID, plotType) %>% select(date) %>% distinct()
plot_date <- vegind %>% group_by(plotID) %>% dplyr::select(date) %>% distinct()
plot_date$year <- format(as.Date(plot_date$date, format = '%m/%d/%Y'), '%Y')
plot_year <- plot_date %>% group_by(plotID) %>% dplyr::select(year) %>% distinct()
n_years_meas <- table(plot_year$plotID) %>% data.frame()


#########################################################################################################
# Determine number of sampling events for a site
sitei <- "UNDE"
events <- vegind %>% dplyr::filter(siteID == sitei) %>% dplyr::select(eventID) %>% distinct() %>% unlist()
events

sitei_nYearsMeas <- n_years_meas[grep(sitei, n_years_meas$Var1),] %>% arrange(Freq)
towerPlotsi <- vplot0 %>% dplyr::filter(siteID == sitei & plotType == "tower") %>% dplyr::select(plotID) %>% distinct()
plotsi_remeasured <- sitei_nYearsMeas %>% dplyr::filter(Freq > 2)  %>% inner_join(towerPlotsi, by = c("Var1" = "plotID")) %>% 
                      dplyr::select(Var1) %>% unlist() %>% unname() %>% as.character()
# Summarize tree data to basal area by species in live and dead categories
vegi <- veg %>% dplyr::filter(siteID == sitei)
vplot <- vplot0 %>% dplyr::filter(siteID == sitei)
eventsi <- sort(unique(vegi$eventID.x)) # Note, there are 2 eventID's after join vegind to vegmapping, because mapping coordinates were collected in a particular year.

treeSppFreq <- vegi %>% dplyr::filter(stemDiameter < 10  & grepl("tree", growthForm)) %>% dplyr::select(spp) %>% table() %>% sort()
treeSppFreq
eventsi

siteTreeSpp <- names(treeSppFreq)

# Create a color palette for observed overstory tree species for the site
set.seed(5489)
conColors <- brewer.pal(9, "Greens")
siteTreeColors <- siteTreeColors <- c(brewer.pal(11,"Spectral"), brewer.pal(12, "Set3"), brewer.pal(8, "Accent"),
                                      brewer.pal(11, "PuOr"), brewer.pal(9, "Greys")[1:6])
# Reshuffle to randomize
siteTreeColors <- sample(siteTreeColors, length(siteTreeSpp))

# Try giving conifers dark green colors
conIndices <- c(grep("abie", siteTreeSpp), grep("pinu", siteTreeSpp), grep("pice", siteTreeSpp), 
              grep("tsug", siteTreeSpp), grep("lari", siteTreeSpp))
siteTreeColors[conIndices] <- conColors[(length(conColors)-length(conIndices)+1):length(conColors)]
sppColors <- data.frame(spp = siteTreeSpp, color = siteTreeColors)

# Determine species observed in overstory tree data

# Read in plot locations:
if (!(exists("plots_poly"))) {
  plotsxy <- readOGR(dsn="C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\All_NEON_TOS_Plots_V7",layer="All_NEON_TOS_Plot_Points")
  plots_poly <- readOGR(dsn="C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\All_NEON_TOS_Plots_V7",layer="All_NEON_TOS_Plot_Polygons")
}
plotsi <- plotsxy[which(plotsxy$siteID == sitei & plotsxy$subtype == "basePlot"),]
polysi <- plots_poly[which(plots_poly$siteID == sitei & plots_poly$subtype == "basePlot"),]
plotsi_df <- plotsi@data
# Filter vegind data to one sampling event at site i # DONT USE THIS ANYWHERE
#treeij <- vegind %>% dplyr::filter(siteID == sitei & eventID == max(events))
#treeij <- treeij %>% mutate(ba = stemDiameter/2^2 * pi) %>% inner_join(plotsi_df, by=c("plotID"="plotID","siteID"="siteID"))#,"domainID"="domainID"))

# Download more plot-specific AOP tiles of canopy height for ploti
years <- as.numeric(substr(eventsi, nchar(eventsi)-3, nchar(eventsi)))
print(years)

###########################################################################################################
# Subset site plots to a specific one
for (p in 1:length(plotsi_remeasured)) {
  #plot_numi <- "051"
  #plot_numi <- plotsi_remeasured[p]
  #ploti_name <- paste(sitei,"_",plot_numi,sep="")
  ploti_name <- plotsi_remeasured[p]
  print(ploti_name)

veg %>% dplyr::filter(plotID == ploti_name) %>% dplyr::select(subplotID.y, nestedSubplotID) %>% distinct() %>% print()

# Clean up for new run
if (exists('lastEventYear')) {
  rm(list = c("lastEventYear", "spbaAll", "vegpliAll", grep("spba2",ls(), value=T), "plotMapMade",
              grep("vegyrpli", ls(), value=T)))
}

# Extract spatial polygon data for this plot
ploti <- polysi[which(polysi$plotID == ploti_name),]
crs_ploti <- paste("+proj=","utm ","+zone=",ploti$utmZone," +datum=",ploti$datum," +units=m +no_defs", sep="")
ploti <- spTransform(ploti, crs_ploti)
sitei <- ploti$siteID
ext.ploti <- extent(ploti)
zoomi <- extent(ext.ploti)
buff_m <- 10
zoomi <- zoomi + c(-buff_m,buff_m,-buff_m,buff_m)

# Create a spp code that is a Factor
vegpli0 <- vegi %>% dplyr::filter(plotID == ploti_name) %>% mutate(sppFactor = as.factor(spp),
                  growthForm = as.factor(growthForm), plantStatus = as.factor(plantStatus))
# Clean data for any weird records
if (sitei == "HARV") {
  vegpli0 <- vegpli0 %>% dplyr::filter(stemDiameter < 180 | (is.na(stemDiameter)))
}

vegpli <- vegpli0 %>% dplyr::filter(stemDiameter < 10 & !(growthForm %in% c("sapling","single shrub"))) #dplyr::filter(stemDiameter >= 10) # Change for small trees
vegpliLt10cm <- vegpli0 %>% dplyr::filter(stemDiameter < 10)
plantStatusTab <- vegpli %>% dplyr::select(plantStatus) %>% group_by(plantStatus) %>% count() %>% mutate(freq_all = n) %>% dplyr::select(plantStatus, freq_all)
plantStatusLevels <- levels(vegpli$plantStatus)

if (makepdf == "yes") {
pdf(file = paste("..//figures//",sitei, "_", ploti_name, "_small_tree_composition_change.pdf",sep=""), onefile = T, width = 6, height = 4)
}
for (j in 1:length(years)) {
    yeari <- years[j]
  
  #js <- 1:length(eventsi)
  #yeari <- 2015
  # Start a counter for year, eventually use a loop
  #j = grep(yeari, eventsi)

  
  # Check if there was a measurement event for this plot in yeari. If not move on to next year.
  if (nrow(vegi %>% dplyr::filter(eventID.x == eventsi[grep(yeari, eventsi)]  & plotID == ploti_name)) > 0 ) {
      
    # Create a raster to map out areas of ploti that were sampled at the nested subplot level 40x40m plots
    subplots0.r <- raster(ncol=2, nrow=2, xmn = ext.ploti[1], xmx = ext.ploti[2], ymn=ext.ploti[3], ymx=ext.ploti[4], crs = crs_ploti )
    subplots.r <- raster(ncol=4, nrow=4, xmn = ext.ploti[1], xmx = ext.ploti[2], ymn=ext.ploti[3], ymx=ext.ploti[4], crs = crs_ploti)
    # Set values to the codes explained in the NEON_vegStructure_userGuide_vA.pdf
    subplot0IDs <- c(39,41,21,23)
    subplotIDs <- c(rep(c(rep(39,2),rep(41,2)),2), rep(c(rep(21,2),rep(23,2)),2))
    values(subplots.r) <- subplotIDs
    nsubplots.r <- raster(ncol=4, nrow=4, xmn = ext.ploti[1], xmx = ext.ploti[2], ymn=ext.ploti[3], ymx=ext.ploti[4], crs = crs_ploti)
    nsubplotIDs <- rep(c(rep(3:4, 2), rep(1:2,2)),2)
    values(nsubplots.r) <- nsubplotIDs
    values(subplots0.r) <- subplot0IDs
    # Later, you can use these to mask each-other based on data from plots
    
    vegyri <- vegpli %>% dplyr::filter(eventID.x == eventsi[grep(yeari, eventsi)])
    
    # Look at observation frequency by growthForm
    table(vegyri$growthForm)
    #multi-bole tree single bole tree     single shrub       small tree 
    # Look at vars related to DBH
    #names(vegyri)[grep('Diameter', names(vegyri))]
    #[1] "stemDiameter"                  "breakDiameter"                 "maxCrownDiameter"              "ninetyCrownDiameter"          
    #[5] "basalStemDiameter"             "basalStemDiameterMsrmntHeight" "maxBaseCrownDiameter"          "ninetyBaseCrownDiameter"      
    #[9] "initialBandStemDiameter"       "bandStemDiameter"
    
    totalSampledAreaTreesi <- vplot %>% dplyr::filter(plotID == ploti_name & eventID == eventsi[j]) %>%
      dplyr::select(totalSampledAreaTrees) %>% unname() %>% unlist() %>% unique()
    
    if (nrow(vegyri) > 0 & length(totalSampledAreaTreesi) == 0) {
      print("There is a problem with event record in vplot dataset, using 800 m  plot size as default")
      #totalSampledAreaTreesi <- 800
      # Advance to next year. Require a valid record for tree sampling in vplot table.
      next()
    } else {print(paste("totalSampledAreaTrees", yeari, "=", totalSampledAreaTreesi, "m2/ha"))}
    
    # Use above to convert basalArea to a per area basis for trees > 10 cm DBH
    # Narrow it down to one plot for practice, select overstory trees > 10 cm DBH
    vegyrpli <- vegyri %>% dplyr::filter(plotID == ploti_name) %>% 
      dplyr::select((order(colnames(vegyri)))) %>% mutate(basalAreaM2 = pi*(stemDiameter/100/2)^2, 
                                                          basalAreaM2ha = basalAreaM2 * 10000/totalSampledAreaTreesi)
    
    # Need to also weed out duplicate observations by individual if any occur. Use the most recent date.
    n_individuals <- vegyrpli %>% group_by(individualID) %>% arrange(individualID) %>% dplyr::select(individualID) %>% n_distinct(., na.rm=T)#slice(which.max(mydates))
    n_individuals
    # Check if there are duplicate records for individual trees. But, multibole stems??
    if (n_individuals < nrow(vegyrpli)) {
      n_individuals <- vegyrpli %>% group_by(individualID) %>% arrange(individualID) %>% 
        dplyr::select(individualID) %>% n_distinct(., na.rm=T)#slice(which.max(mydates))
      
      # There can still be duplicate records where only difference is field "uid.y". No knowledge yet no how to sort for most recent uid.
      # Select first record for now. Ask NEON how to determine most recent record when everything else is identical.
      vegyrpli <- vegyrpli %>% group_by(individualID) %>% arrange(individualID) %>% 
        dplyr::filter(uid.y == max(uid.y)) %>% 
        distinct() %>% ungroup()
      
    }
    print(paste('# individuals = ',n_individuals, yeari)) 
    # Check if there are duplicate records - Want 0 rows duplicated
    vegyrpli %>% group_by(individualID) %>% filter(n() > 1) %>% as.data.frame()
    
    #Look at growth form and plat status
    vegyrpli %>% dplyr::select(growthForm) %>% table()
    vegyrpli %>% dplyr::select(plantStatus) %>% table()
    vegyrpli %>% dplyr::select(remarks.x) %>% table()
    
    #vegyrpliL <- vegyrpli %>% dplyr::filter(growthForm %in% c("multi-bole tree","single bole tree"),
    vegyrpliL <- vegyrpli %>% dplyr::filter(growthForm %in% c("small tree"),
                                        grepl("Live", plantStatus))
    # Found case where "growthForm" was left NA in first year. In this case, use stemDiameter >= 10 to filter to overstory trees.
    if (nrow(vegyrpliL) == 0) {
      vegyrpliL <- vegyrpli %>% dplyr::filter(stemDiameter < 10 &
                                              grepl("Live", plantStatus))
    }
    
    #vegyrpliL <- vegyrpliL %>% inner_join(vplot, by = c("siteID"="siteID", "eventID.x" = "eventID", "plotID"="plotID"))
    # Look at live & dead trees per species
    vegyrpli %>% dplyr::select(sppFactor) %>% table()
    
    # Look at live trees per species
    vegyrpliL %>% dplyr::select(sppFactor) %>% table()
    #subplotsi <- as.numeric(unique(vegyrpli$subplotID.y))
    #nsubplotsi <- as.numeric(unique(vegyrpli$nestedSubplotID))
  
    
  #################### Section to Plot Subplots Sampled in Eventi yeari for Overstory trees DBH > 10 cm ########
  ##############################################################################################################
    ## Only create this plot one time in loop for now. Use a counter
    if (!(exists("plotMapMade"))) {
    # mask subplot raster to only show sampled subplots
    plotIDs.tab <- data.frame(cbind(as.character(subplotIDs), as.character(nsubplotIDs)))
    names(plotIDs.tab) <- c("subplotIDs","nsubplotIDs")
    # Determine distinct combo of subplot and nestedsubplot IDs observed in the plot data
    nsubplots.sampled <- vegyrpli %>% dplyr::select(subplotID.y, nestedSubplotID) %>% distinct()
    names(nsubplots.sampled) <- names(plotIDs.tab)
    print(nsubplots.sampled)
    
    
    # Now determine which rows correspond to NOT sampled subplots
    # If nestedSubPlots were not recorded, use recorded subplots instead
    if (all(is.na(nsubplots.sampled$nsubplotIDs)) |
        length(which(paste0(plotIDs.tab$subplotIDs, plotIDs.tab$nsubplotIDs) %in% 
                     paste0(nsubplots.sampled$subplotIDs, nsubplots.sampled$nsubplotIDs))) == 0) {
      NArowIDs <- which(!(paste0(plotIDs.tab$subplotIDs) %in% paste0(nsubplots.sampled$subplotIDs)))
    } else {NArowIDs <- which(!(paste0(plotIDs.tab$subplotIDs, plotIDs.tab$nsubplotIDs) %in% 
                                  paste0(nsubplots.sampled$subplotIDs, nsubplots.sampled$nsubplotIDs)))}
    
    plotIDs.tab[NArowIDs,] <- NA
    
    
    # Plot label coordinates for subplots (plot quarters)
    subplot.coords <- data.frame(rbind(c("39",ext.ploti[1] + 10, ext.ploti[3] + 30),
                            c("41",ext.ploti[1] + 30, ext.ploti[3] + 30),
                            c("21",ext.ploti[1] + 10, ext.ploti[3] + 10),
                            c("23",ext.ploti[1] + 30, ext.ploti[3] + 10)))
    
    
    #subplotIDs[which(!(subplotIDs %in% subplotsi))] <- NA
    subplotsVector <- rasterToPolygons(subplots0.r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
    nsubplotsVector <- rasterToPolygons(nsubplots.r, fun=NULL, n=4, na.rm=TRUE, digits=12, dissolve=FALSE)
    
    subplots.r <- raster::setValues(subplots.r, as.numeric(plotIDs.tab$subplotIDs))
    nsubplots.r <- raster::setValues(subplots.r, as.numeric(plotIDs.tab$nsubplotIDs))
    plot(nsubplots.r)
    plot(ploti, add=T)
    raster::text(nsubplots.r, col=1)
    
    # Now overlay vector outlines
    plot(nsubplotsVector, add=T)
    raster::text(subplots0.r, col = "darkgrey")
    mtext(ploti_name)
    
    # Now add mapped stems if available
    datai <- vegmapOut[which(vegmapOut$plotID==ploti_name),]
    if (nrow(datai) > 0) {
    points(datai$adjEasting[which(datai$plotID==ploti_name)], 
         datai$adjNorthing[which(datai$plotID==ploti_name)], 
         cex=datai$stemDiameter[which(datai$plotID==ploti_name)]/20,pch= plant_status_symbol,bg=plant_status_bg, 
         xlab="Easting", ylab="Northing")
    #legend("topright",list_plant_status,pt.bg=plant_status_bg,pch=plant_status_symbol,cex=0.8)
      }
      plotMapMade = "yes"
    }
    #################### END Section to Plot Subplots Sampled in Eventi yeari for Overstory trees DBH > 10 cm ####
    ##############################################################################################################  
    
    assign(paste('vegyrpliL',yeari, sep=""), vegyrpliL)
    assign(paste('vegyrpli',yeari, sep=""), vegyrpli)
    
    spbaYeari <- vegyrpliL %>% group_by(sppFactor) %>% mutate(basalAreaM2ha = basalAreaM2 * 10000/totalSampledAreaTreesi) %>% 
      summarize_at(vars(basalAreaM2ha), list(sum = sum))
    totalBasalAreaLiveYeari <- sum(spbaYeari$sum)
    spbaYeari <- spbaYeari %>% mutate(ba = sum, rba = ba / totalBasalAreaLiveYeari) %>% 
      dplyr::select(sppFactor, ba, rba)
  
    assign(paste('spba',yeari, sep=""), spbaYeari)
    # rename vars for a merge spba dataframe
    names(spbaYeari)[2:3] <- paste(names(spbaYeari)[2:3], yeari, sep="_")
    
    if (!(exists('spbaAll'))) {
      spbaAll <- spbaYeari
      plantStatus2 <- vegyrpliL %>% dplyr::select(plantStatus) %>% count() %>% mutate(year = yeari)
    } else { spbaAll <- merge(spbaAll, spbaYeari, by.x='sppFactor', by.y='sppFactor', all=T)
      plantStatusi <- vegyrpliL %>% dplyr::select(plantStatus) %>% count() %>% mutate(year = yeari)
      plantStatus2 <- rbind(plantStatus2, plantStatusi)
    }
    spbaAll <- spbaAll %>% replace(is.na(.), 0)
    yeariPlantStatus <- vegyrpliL %>% dplyr::select(plantStatus) %>% group_by(plantStatus) %>% count() 
    names(yeariPlantStatus)[2] <- paste('freq',yeari,sep="_")
    plantStatusTab <- merge(plantStatusTab, yeariPlantStatus, by.x = 'plantStatus', by.y = 'plantStatus', all=T)
    plantStatusTab <- plantStatusTab %>% replace(is.na(.), 0)
    
    # Create a barplot of basal area by species for current measurement year
    rangeBA <- c(-0.5* max(spbaAll[,2:ncol(spbaAll)]), max(spbaAll[,2:ncol(spbaAll)]))
    sppColorsi <- spbaYeari %>% dplyr::select(sppFactor) %>% left_join(sppColors, by = c("sppFactor" = "spp"))
    
    baYear1BarPlot <- with(spbaYeari, barplot(unlist(spbaYeari[,2]), names.arg = sppFactor, las = 2, col = sppColorsi$color,
                                            space = 0.1, axisnames = FALSE, ylab = "BA m2/ha", ylim = rangeBA))
    abline(h=0)
    # Now use mtext() for the axis labels
    text(baYear1BarPlot, rep(rangeBA[1] * 0.1,length(baYear1BarPlot)),spbaYeari$sppFactor, srt = 90, cex = 1.5)
    mtext(paste(ploti_name, yeari))
    #########################################################################################################
    
    # If yeari Measurement year is not the first, calculate a change over time
    if (exists('lastEventYear')) {
      yearsSinceMeasurement <- yeari - lastEventYear
      spbaLastEventYear <- get(paste("spba", lastEventYear, sep=""))
      liveBALastEventYear <- sum(spbaLastEventYear$ba)
      
      #spbaChange <- spbaYeari %>% group_by(sppFactor, .drop=F) %>% full_join(spbaLastEventYear, by = c("sppFactor" = "sppFactor")) %>% replace(is.na(.), 0) %>%
      #   mutate(baM2Change = (ba.x - ba.y) / yearsSinceMeasurement, baPercentChange = (ba.x - ba.y)/totalBasalAreaLiveLastEventYear/ yearsSinceMeasurement)
       
      spbaChange <- spbaAll %>% mutate(baM2Change = (get(paste('ba',yeari, sep="_")) - get(paste('ba',lastEventYear, sep="_"))) / yearsSinceMeasurement,
                                       baPercentChange = (get(paste('ba',yeari, sep="_")) - get(paste('ba',lastEventYear, sep="_")))/
                                         (get(paste('ba',lastEventYear, sep="_")) + 0.000001))
      
      rangeChange <- with(spbaChange, range(baM2Change))
      rangeChangePercent <- with(spbaChange, range(baPercentChange))
      rangeChangePercent[1] <- max(-1,rangeChangePercent[1])
      rangeChangePercent[2] <- min(1,rangeChangePercent[2])
      # Instead hardcode range for % change plots to -+130%
      rangeChangePercent <- c(-1.3, 1.3)
      
      # Now place labels closer to the x axis
      # set 'axisnames' to FALSE so the default
      # labels are not drawn. Also note that barplot() 
      # returns the bar midpoints, so set 'mp' to the return
      # values
    
        
      baChangeBarPlot <- with(spbaChange, barplot(baM2Change, names.arg = sppFactor, las = 2, 
                                                  space = 0.1, axisnames = FALSE, ylab = "BA m2/ha Change yr-1"))
      abline(h=0)
      # Now use mtext() for the axis labels
      text(baChangeBarPlot, rep(rangeChange[which.max(abs(rangeChange))] * 0.10,length(baChangeBarPlot)),spbaChange$sppFactor, srt = 90, cex = 1.5)
      mtext(paste(ploti_name, yeari,'-', lastEventYear))
      # Repeat for percent Change
      varChangeBarPlot <- with(spbaChange, graphics::barplot(baPercentChange, ylim = c(-1,1), names.arg = sppFactor, 
                                                             las = 2, 
                                                   space = 0.1, axisnames = FALSE, ylab = "BA % Change"))
      abline(h=0)
      # Now use mtext() for the axis labels
      text(varChangeBarPlot, rep(rangeChangePercent[which.max(abs(rangeChangePercent))] * 0.20,length(varChangeBarPlot)),spbaChange$sppFactor, srt = 90, cex = 1.5)
      mtext(paste(ploti_name, yeari,'-', lastEventYear))
    }
    

    
    lastEventYear <- yeari
  }
}



# Try to create Alluvial plot of plantStatus Transitions
# Combine all the byYear dataframes that have been screened for duplicate records etc.
# Find all the byYear dataframes to bind into one for summaries - include live and dead trees for transitions
listVegyrpli <- grep("vegyrpli", ls(), value=T)
listVegyrpli <- listVegyrpli[-grep("vegyrpliL",listVegyrpli)]
listVegyrpli <- listVegyrpli[-1]

vegpliAll <- get(listVegyrpli[1])

for (k in 2:length(listVegyrpli)) {
  vegpliAll <- rbind(vegpliAll, get(listVegyrpli[k]))
}

# Add a level to capture NAs in plantStatus
#levels(vegpliAll$plantStatus) <- c(levels(vegpliAll$plantStatus),"No standing record")
levelsPli <- levels(vegpliAll$plantStatus)
# Reorder levels for use in Alluvial plots
vegpliAll$plantStatus <- factor(vegpliAll$plantStatus, 
                                c("No standing record",levelsPli[c(grep("Dead", levelsPli),
                                  grep("dead",levelsPli),grep("Live, ", levelsPli, fixed=T))],"Live")) # levels(vegpliAll$plantStatus)[c(2:6,1,7)]

# First, have to take care of any duplicate measurements of individuals in the same eventID. Documentation suggests using the
# Most recent record, but there is not field indicating how recent a record is (only differ by uid, which is an alpha-numeric code, not time)
vegpliAll <- vegpliAll %>% group_by(individualID) %>% mutate(meanDBH = mean(stemDiameter), diffMeanDBH = abs(stemDiameter - meanDBH)) %>% 
                    arrange(individualID, eventID.x)
# Find and filter our duplicates of individualID and eventID.x. 
vegpliAll <- vegpliAll %>% group_by(individualID, eventID.x) %>%  dplyr::arrange(individualID, eventID.x, diffMeanDBH) %>%
            mutate(dup_DBH = duplicated(individualID, eventID.x)) %>% 
            dplyr::filter(!(dup_DBH))

# Use pivot_wider and grouping to spread the data frame out to plantStatus by measurement year
plantStatus3 <- vegpliAll %>% group_by(eventID.x, plantStatus) %>% dplyr::select(individualID, eventID.x, plantStatus) %>% 
  unique() %>% pivot_wider(., names_from = eventID.x, values_from = plantStatus) %>%  as.data.frame()
stemDBH3 <- vegpliAll %>% group_by(eventID.x) %>% dplyr::select(individualID, eventID.x, stemDiameter) %>% 
  unique() %>% pivot_wider(., names_from = eventID.x, values_from = stemDiameter) %>% dplyr::arrange(ncol(.)-2, ncol(.)-1) %>% as.data.frame()
plantStatus3b <- vegpliAll %>% group_by(individualID) %>% mutate(maxBasalAreaM2ha = max(basalAreaM2ha, na.rm=T)) %>%
                group_by(eventID.x, plantStatus) %>% dplyr::select(individualID, eventID.x, plantStatus, maxBasalAreaM2ha) %>% 
             unique() %>% pivot_wider(., names_from = eventID.x, values_from = plantStatus)


# Now Count the frequency of each combination of plantStatus transitions across measurement events
listEvents <- sort(unique(vegpliAll$eventID.x))

plantStatus4 <- plantStatus3 %>% dplyr::select(-individualID) %>% 
  group_by_at(vars(one_of(listEvents))) %>% count()


plantStatus4 <- plantStatus4 %>% replace(is.na(.), "No standing record")
plantStatus4 <- plantStatus4 %>% dplyr::arrange(vars(listEvents)) %>% as.data.frame()
# Filter for now, Option to remove rows for observations that were measured only in first year, not others
# May represent changes in protocol
#plantStatus4 <- plantStatus4[-which(apply(plantStatus4[,c(2:4)], 1, function(x){all(x=="No standing record")})),]

# Alternative, summarize based on sum of max basal area for each plant status sequence grouping
plantStatusBa <- plantStatus3b %>% dplyr::select(-individualID) %>% 
  group_by_at(vars(one_of(listEvents))) %>% summarize_at(vars(maxBasalAreaM2ha), sum, na.rm=T )
plantStatusBa <- plantStatusBa %>% replace(is.na(.), "No standing record")
plantStatusBa <- plantStatusBa %>% dplyr::arrange(vars(listEvents)) %>% mutate(n = round(maxBasalAreaM2ha * 100)) %>%  as.data.frame()


# Create a color palette for plantStatus
statusColors <- rep("grey", nrow(plantStatus4))
statusColors[grep("Live", plantStatus4[,1])] <- rgb(0.9,0.9,0,0.5)
statusColors[grep("Live", plantStatus4[,ncol(plantStatus4)-1])] <- "orange"
liveAfterYear1_rows <- c()
for (i in 2:(ncol(plantStatus4)-1)) {
  liveAfterYear1_rows <- c(liveAfterYear1_rows, grep("Live", plantStatus4[,i]))
}

liveAfterYear1_rows <- sort(unique(liveAfterYear1_rows))
statusColors[liveAfterYear1_rows[which(liveAfterYear1_rows %in% grep("No standing record", plantStatus4[,1]))]] <- "green"

events_labels <- names(plantStatusBa)[which(names(plantStatusBa) %in% listEvents)]
events_labels <- substr(events_labels, nchar(events_labels) - 3, nchar(events_labels)) # Doesn't work without above line if event is excluded because no trees were measured


# Plot alluvial plot using frequency of stems in each observed sequence of plantStatus
alluvial(plantStatus4[,which(names(plantStatus4) %in% listEvents)], freq=plantStatus4$n,
         col = statusColors,
         border = "white", axis_labels = events_labels, cex = 0.8)
#         hide = plantStatus4[,2] == "No standing record" & plantStatus4[,3] == "No standing record" & plantStatus4[,4] == "No standing record")
mtext(paste("Stems","   ",ploti_name), line = 2)


# Plot alluvial plot using sum of max BA of stems in each observed sequence of plantStatus
alluvial(plantStatusBa[,which(names(plantStatusBa) %in% listEvents)], freq=plantStatusBa$n,
         col = statusColors,
         border = "white", axis_labels = events_labels, cex = 0.8)
#         hide = plantStatus4[,2] == "No standing record" & plantStatus4[,3] == "No standing record" & plantStatus4[,4] == "No standing record")
mtext(paste("Basal Area","   ",ploti_name), line = 2)

if (makepdf == "yes") {
dev.off()
  }

}