####################################################################################################
#
#    --- Last updated: 02.18.2021 By Jane R. Foster <jane.foster@uvm.edu>
####################################################################################################


#---------------- Close all devices and delete all variables. -------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
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
## ----packages, eval=FALSE-----------------------------------------------------------------------------------------------------
## 
## install.packages("devtools")
## install.packages("neonUtilities")
## install.packages("raster")
## devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
## install.packages("BiocManager")
## BiocManager::install("rhdf5")
## 

#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)

makepdf <- "yes" # or "no"
# Set a save path to match path for downloaded files
save_path <- "data"
setwd(save_path)

# Create functions
# not in
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#
# Modify the file path to match the path to your zip file - Run this again if you want to update your download for new data
#stackByTable("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant.zip")

#We'll read in the vst_mappingandtagging and vst_apparentindividual files:
vegmap0 <- readTableNEON("NEON_struct-woody-plant\\stackedFiles\\vst_mappingandtagging.csv",
                        "NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
# Read in plot locations:
if (!(exists("plots_poly"))) {
  plotsxy <- readOGR(dsn="All_NEON_TOS_Plots_V7",layer="All_NEON_TOS_Plot_Points")
  plots_poly <- readOGR(dsn="All_NEON_TOS_Plots_V7",layer="All_NEON_TOS_Plot_Polygons")
}

# Try excluding rows with pointID == NA to get getLosTOS to work
#vegmap <- vegmap0[which(!(is.na(vegmap0$pointID))),]
# The above line subsamples down to only mapped individuals. But we want all individual records for plot summaries!

vegind <- readTableNEON("NEON_struct-woody-plant\\stackedFiles\\vst_apparentindividual.csv",
                        "NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vegind)
vstvar <- read.csv("NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vstvar)
vstval <- read.csv("NEON_struct-woody-plant\\stackedFiles\\validation_10098.csv")
#View(vstval)
vplot0 <- read.csv("NEON_struct-woody-plant\\stackedFiles\\vst_perplotperyear.csv")
#First, use the geoNEON package to calculate stem locations:

names(vegmap0)
# If you want to update locations with updated data, run line below
#vegmap <- geoNEON::getLocTOS(vegmap, "vst_mappingandtagging")
# Otherwise, read in from saved file
vegmapOut <-  read.csv("NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", header=T)
names(vegmapOut)
# Stored vegmapOut is only for mapped individuals. Use all for plot-level summaries

# vegmapOut has some duplicate measurements of individualID (trees). Need to slice to use only the most recent record for each tree using date.
vegmapOut <- vegmapOut %>%
  arrange(plotID, individualID, desc(date)) %>%
  group_by(individualID) %>%
  slice(1) %>%
  ungroup()

# Repeat for vegmap0
# Need to save a version of vegmap with locations so you don't have to re-run above 
# lengthly step each time. Code below sorts individual records and takes the most recent (date).
vegmap <- vegmap0 %>%
  arrange(plotID, individualID, desc(date)) %>%
  group_by(individualID) %>%
  slice(1) %>%
  ungroup()

dim(vegmap)
#write.csv(vegmap, "NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", row.names=F)

# add variable "year", then make a copy
vegind <- vegind %>% group_by(plotID) %>% mutate(year = format(as.Date(date, format = '%m/%d/%Y'), '%Y'))
vegind0 <- vegind
# Thin out vegind data to EXCLUDE non-tree growth forms shrubs and saplings, lianas for now.
vegind <- vegind0 %>% dplyr::filter(grepl("tree", growthForm))

# And now merge the un-duplicated mapping data with the individual measurements. A subset of trees are mapped.
# individualID is the linking variable, the others are included to avoid having duplicate columns.
veg <- merge(vegind, vegmap, by=c("individualID","namedLocation",
                                  "domainID","siteID","plotID"), all.x = T)

# Create a table of all possible plantStatus from vegind table.
list_plant_status <- levels(as.factor(vegind0$plantStatus))
table(veg$plantStatus)

plant_status_symbol <- c(17,3,21,21,10,21,21,21,8,13,12,9,7,4,5,17)
plant_status_bg <- c("gray",1,"green","orange","olivedrab","purple","red","gray",1,1,1,1,1,1,1,"red")
plant_status_vis <- data.frame(plant_status = list_plant_status,
                               plant_status_symbol=plant_status_symbol,
                               plant_status_bg=plant_status_bg)

veg0 <- veg

# Create species codes from scientific names
names(veg0)
# Create a list of distinct species to generate 8 digits species codes. Keep taxonID as some 8-char sppcodes are duplicated (not distinct)
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

#col_plant_status <- veg$plant_status_bg

# Quick tabulation of number of distinct years vegetation structure was measured by plotID
plot_year <- veg %>% group_by(plotID) %>% dplyr::select(year) %>% distinct()
# Exclude data from 2014 for now
plot_year <- plot_year %>% dplyr::filter(year > 2014)
n_years_meas <- table(plot_year$plotID) %>% data.frame()
print(n_years_meas)

# Try a different method, using vplot0 (from vstperplotyear)
#vplot0 %>% dplyr::filter(siteID == sitei) %>% dplyr::select(plotID) %>% distinct() %>% arrange(plotID)
#########################################################################################################
# Determine number of sampling events for a site
#########################################################################################################
sitei <- "UKFS"
plot_type <- "tower" #"tower" # "distributed"
#events <- vegind %>% dplyr::filter(siteID == sitei) %>% dplyr::select(eventID) %>% distinct() %>% unlist()
# Alternate method, use vplot table to determine "official" measurement events
events <- vplot0 %>% dplyr::filter(siteID == sitei) %>% dplyr::select(eventID) %>% distinct() %>% unlist()

# Subset tree and plot data tables to site level
vegi <- veg %>% dplyr::filter(siteID == sitei)
vplot <- vplot0 %>% dplyr::filter(siteID == sitei)
eventsi <- sort(unique(vegi$eventID.x)) # Note, there are 2 eventID's after join vegind to vegmapping, because mapping coordinates were collected in a particular year.
# Exclude data from 2014 for now
if (length(grep("2014", eventsi)) > 0) {
  eventsi <- eventsi[-grepl("2014", eventsi)]
} 

# Determine how often plots on site were remeasured
sitei_nYearsMeas <- n_years_meas[grep(sitei, n_years_meas$Var1),] %>% arrange(Freq)
print(sitei_nYearsMeas)
typePlotsi <- vplot0 %>% dplyr::filter(siteID == sitei & plotType == plot_type) %>% dplyr::select(plotID) %>% distinct()
plotsi_remeasured <- sitei_nYearsMeas %>% dplyr::filter(Freq > 1)  %>% inner_join(typePlotsi, by = c("Var1" = "plotID")) %>% 
                      dplyr::select(Var1) %>% unlist() %>% unname() %>% as.character()

# Look at frequency of tree species measured at this site
treeSppFreq <- vegi %>% dplyr::filter(stemDiameter >= 10  & grepl("tree", growthForm)) %>% 
  dplyr::select(taxonID) %>% table() %>% sort(., decreasing = T)
treeSppFreqSm <- vegi %>% dplyr::filter(stemDiameter < 10  & grepl("tree", growthForm)) %>% 
  dplyr::select(taxonID) %>% table() %>% sort(., decreasing = T)

print(treeSppFreq)
print(treeSppFreqSm)
eventsi

siteTreeSpp <- sort(unique(c(names(treeSppFreq), names(treeSppFreqSm))))

# Create a color palette for observed overstory tree species for the site
set.seed(5489)
conColors <- brewer.pal(9, "Greens")
siteTreeColors <- siteTreeColors <- c(brewer.pal(11,"Spectral"), brewer.pal(12, "Set3"), brewer.pal(8, "Accent"),
                                      brewer.pal(11, "PuOr"), brewer.pal(9, "Greys")[1:6])
# Reshuffle to randomize
siteTreeColors <- sample(siteTreeColors, length(siteTreeSpp), replace = T)

# First, create siteTreeSpp that uses species codes
siteTreeTaxonID <- siteTreeSpp
siteTreeSpp <- siteTreeTaxonID %>% data.frame(taxonID = .) %>% inner_join(spp_distinct) %>% 
  select(spp) %>% unname() %>% unlist()

# Try giving conifers dark green colors
conIndices <- c(grep("abie", siteTreeSpp), grep("pinu", siteTreeSpp), grep("pice", siteTreeSpp), 
              grep("tsug", siteTreeSpp), grep("lari", siteTreeSpp))
siteTreeColors[conIndices] <- conColors[(length(conColors)-length(conIndices)+1):length(conColors)]
sppColors <- data.frame(spp = siteTreeSpp, color = siteTreeColors, taxonID = siteTreeTaxonID)

# Subset plots to NEON site of interest, sitei
plotsi <- plotsxy[which(plotsxy$siteID == sitei & plotsxy$subtype == "basePlot"),]
polysi <- plots_poly[which(plots_poly$siteID == sitei & plots_poly$subtype == "basePlot"),]
plotsi_df <- plotsi@data

years <- as.numeric(substr(eventsi, nchar(eventsi)-3, nchar(eventsi)))
print(years)

# Set up empty dataframe to compile plot-level totals for live and dead BA by year
ba_tot_sitei <- data.frame(matrix(nrow=0, ncol=5))
names(ba_tot_sitei) <- c("plot", "year", "live_ba_tot", "dead_ba_tot", "live_ba_small_tot")

###########################################################################################################
# Subset site plots to a specific one
for (p in 1:length(plotsi_remeasured)) {
  #plot_numi <- "051"
  #plot_numi <- plotsi_remeasured[p]
  #ploti_name <- paste(sitei,"_",plot_numi,sep="")
  ploti_name <- plotsi_remeasured[p]
  print(ploti_name)
  yearsp <- c()

  veg %>% dplyr::filter(plotID == ploti_name) %>% dplyr::select(subplotID.y, nestedSubplotID) %>% 
    distinct() %>% print()
  
  # Clean up for new run with new plot
  if (exists('lastEventYear')) {
    rm(list = c("lastEventYear", "spbaAll", "vegpliAll", grep("spba2",ls(), value=T), "plotMapMade",
                grep("vegpliYrj", ls(), value=T), grep("live_ba", ls(), value=T), grep("dead_ba", ls(), value=T),
                "sppColorsDeadi","sppColorsi","sppColorsLegendi", grep("spbaA",ls(), value=T),grep('plantStatus', ls(), value=T)))
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

  # Subset veg data to plot, Create a spp code that is a Factor. Using taxonID for this step as it is unique (8-char sppcode is not always)
  vegpli0 <- vegi %>% dplyr::filter(plotID == ploti_name & !(plantStatus == "No longer qualifies")) %>% 
                    mutate(sppFactor = as.factor(taxonID), growthForm = as.factor(growthForm), 
                           plantStatus = as.factor(plantStatus))
  # Add in a factor level for NAs in tree data, then transform NAs in sppFactor to that label
  levels(vegpli0$sppFactor) <- c(levels(vegpli0$sppFactor), 'NAtaxonID')
  vegpli0$sppFactor[which(is.na(vegpli0$sppFactor))] <- 'NAtaxonID'
  
  # Clean data for any weird records
  if (sitei == "HARV") {
    vegpli0 <- vegpli0 %>% dplyr::filter(stemDiameter < 180 | (is.na(stemDiameter)))
  }
  
  # Subset data to trees with stemDiameter > 10 cm (tagged), or less than (small trees)
  # Change 2021-04-13 - Split data to large and small trees later
  #vegpli <- vegpli0 %>% dplyr::filter(stemDiameter >= 10)
  #vegpliLt10cm <- vegpli0 %>% dplyr::filter(stemDiameter < 10 | is.na(stemDiameter))
  vegpli <- vegpli0
  
  # Check why you may have veg observations with no stemDiameter (should generally be saplings, but could be "Downed" or "burned")
  vegpli0 %>% dplyr::filter(is.na(stemDiameter)) %>% dplyr::select(growthForm, plantStatus) %>% 
    table(useNA = "ifany")
  
  # Calculate frequency of plantStatus observed for ploti across all years
  plantStatusTab <- vegpli %>% dplyr::select(plantStatus) %>% group_by(plantStatus) %>% count() %>%
                    mutate(freq_all = n) %>% dplyr::select(plantStatus, freq_all) %>% data.frame()
  plantStatusTab$plantStatusLevels <- levels(plantStatusTab$plantStatus)[plantStatusTab$plantStatus]
  plantStatusTab <- plantStatusTab %>% inner_join(plant_status_vis, by = c("plantStatus" = "plant_status"))

  if (makepdf == "yes") {
  pdf(file = paste("..//figures//", ploti_name, "_tree_composition_change.pdf",sep=""), onefile = T, width = 6, height = 4)
  }
  for (j in 1:length(years)) {
      yearj <- years[j]
    
    # Skip problem plot - fix later
      if (ploti_name == "HARV_043" & yearj == 2015) {next()}
      
    # Check if there was a measurement event for this plot in yearj. If not move on to next year.
    if (nrow(vegpli0 %>% dplyr::filter(eventID.x == eventsi[grep(yearj, eventsi)])) > 0 ) {
        
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
      vegpliYrj <- vegpli %>% dplyr::filter(eventID.x == eventsi[grep(yearj, eventsi)])
   
          # Look at observation frequency by growthForm
      table(vegpliYrj$growthForm)
      #multi-bole tree single bole tree     single shrub       small tree 
      # Look at vars related to DBH
      #names(vegpliYrj)[grep('Diameter', names(vegpliYrj))]
      #[1] "stemDiameter"                  "breakDiameter"                 "maxCrownDiameter"              "ninetyCrownDiameter"          
      #[5] "basalStemDiameter"             "basalStemDiameterMsrmntHeight" "maxBaseCrownDiameter"          "ninetyBaseCrownDiameter"      
      #[9] "initialBandStemDiameter"       "bandStemDiameter"
      
      totalSampledAreaTreesij <- vplot %>% dplyr::filter(plotID == ploti_name & eventID == eventsi[j]) %>%
        dplyr::select(totalSampledAreaTrees) %>% unname() %>% unlist() %>% unique()
      
      if (nrow(vegpliYrj) > 0 & length(totalSampledAreaTreesij) == 0) {
        print("There is a problem with event record in vplot dataset, using 800 m  plot size as default")
        totalSampledAreaTreesij <- 800
        # Advance to next year. Require a valid record for tree sampling in vplot table.
        next()
      } else {print(paste("totalSampledAreaTrees", yearj, "=", totalSampledAreaTreesij, "m2/ha"))}
      
      # Convert basalArea to a per area basis for trees > 10 cm DBH
      vegpliYrj <- vegpliYrj %>% dplyr::select((order(colnames(vegpliYrj)))) %>% 
                      mutate(basalAreaM2 = pi*(stemDiameter/100/2)^2, 
                      basalAreaM2ha = basalAreaM2 * 10000/totalSampledAreaTreesij)
      
      # Need to also weed out duplicate observations by individual if any occur. Use the most recent date.
      n_individuals <- vegpliYrj %>% group_by(individualID) %>% arrange(individualID) %>% dplyr::select(individualID) %>% 
                        n_distinct(., na.rm=T)#slice(which.max(mydates))
      n_individuals
      # Check if there are duplicate records for individual trees. But, multibole stems??
      if (n_individuals < nrow(vegpliYrj)) {
        n_individuals <- vegpliYrj %>% group_by(individualID) %>% arrange(individualID) %>% 
          dplyr::select(individualID) %>% n_distinct(., na.rm=T)#slice(which.max(mydates))
        
        # There can still be duplicate records where only difference is field "uid.y". No knowledge yet no how to sort for most recent uid.
        # Select first record for now. Ask NEON how to determine most recent record when everything else is identical.
        # I guess it doesn't matter, unless it is multi-stemmed small tree with identical DBH.
        vegpliYrj <- vegpliYrj %>% group_by(individualID) %>% arrange(individualID) %>% 
          dplyr::filter(uid.y == max(uid.y)) %>% 
          distinct() %>% ungroup()
        
      }
      print(paste('# individuals = ',n_individuals, yearj)) 
      # Check if there are duplicate records - Want 0 rows duplicated
      vegpliYrj %>% group_by(individualID) %>% filter(n() > 1) %>% as.data.frame()
      
      #Look at growth form and plat status
      vegpliYrj %>% dplyr::select(growthForm) %>% table(useNA = "ifany")
      vegpliYrj %>% dplyr::select(plantStatus) %>% table(useNA = "ifany")
      vegpliYrj %>% dplyr::select(remarks.x) %>% table(useNA = "ifany")
      
      # Subset data to Live over story stems >= 10 cm DBH
      vegpliYrjL <- vegpliYrj %>% dplyr::filter((growthForm %in% c("multi-bole tree","single bole tree",NA)) |
                                                stemDiameter >= 10,
                                                grepl("Live", plantStatus))
      # New 2021-04-13: Subset to Live small trees DBH < 10 cm DBH
      vegpliYrjSmallL <- vegpliYrj %>% dplyr::filter((grepl("tree", growthForm)) &
                                                        stemDiameter < 10,
                                                      grepl("Live", plantStatus))
      
      # Found case where "growthForm" was left NA in first year. In this case, use stemDiameter >= 10 to filter to overstory trees.
      if (nrow(vegpliYrjL) == 0) {
        #vegpliYrjL <- vegpliYrj %>% dplyr::filter(stemDiameter >= 10 &
        #                                        grepl("Live", plantStatus))
        print(paste('Problem with All overstory growthForm == NA or No live overstory trees', yearj)) 
      }
      
       # Look at live & dead trees per species
      vegpliYrj %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      
      # Look at live trees per species
      vegpliYrjL %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      vegpliYrjSmallL  %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      #subplotsi <- as.numeric(unique(vegpliYrj$subplotID.y))
      #nsubplotsi <- as.numeric(unique(vegpliYrj$nestedSubplotID))
    
      
    #################### Section to Plot Subplots Sampled in Eventi yearj for Overstory trees DBH > 10 cm ########
    ##############################################################################################################
      ## Only create this plot one time in loop for now. Use a counter
      if (!(exists("plotMapMade"))) {
      # mask subplot raster to only show sampled subplots
      plotIDs.tab <- data.frame(cbind(as.character(subplotIDs), as.character(nsubplotIDs)))
      names(plotIDs.tab) <- c("subplotIDs","nsubplotIDs")
      # Determine distinct combo of subplot and nestedsubplot IDs observed in the plot data
      nsubplots.sampled <- vegpliYrj %>% dplyr::select(subplotID.y, nestedSubplotID) %>% distinct()
      names(nsubplots.sampled) <- names(plotIDs.tab)
      print(nsubplots.sampled)
      
      # Determine empty subplots to place legend in empty space
      legendInd <- 0
      if (41 %notin% nsubplots.sampled$subplotIDs) {
        legendPos <- "topright"
        legendInd <- 1
      }
      if (39 %notin% nsubplots.sampled$subplotIDs & legendInd == 0) {
        legendPos <- "topleft"
        legendInd <- 1
      }
      if (23 %notin% nsubplots.sampled$subplotIDs & legendInd == 0) {
        legendPos <- "bottomright"
        legendInd <- 1
      }
      if (21 %notin% nsubplots.sampled$subplotIDs & legendInd == 0) {
        legendPos <- "bottomleft"
        legendInd <- 1
      }
      
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
      
      # Now prepare mapped stems if available
      datai <- vegmapOut[which(vegmapOut$plotID==ploti_name),]
      # Clunky - rewrite at some point
      datai <-datai %>% dplyr:: select(individualID, eventID, utmZone:adjElevationUncertainty) %>%
        inner_join(subset(vegpli, select = c(individualID, stemDiameter, eventID.y, spp:sppFactor))) %>% 
        group_by(individualID) %>% arrange(desc(eventID.y, stemDiameter)) %>% slice(1) %>% ungroup()
      # Determine max extent of mapped stems to compare to designed plot coordinates
      if (any(!(is.na(datai$adjEasting)))) {
      extent_stems <- datai %>% dplyr::select(adjEasting, adjNorthing) %>% apply(., 2, range, na.rm=T) %>% data.frame()
      extent_stems <- with(extent_stems, extent(adjEasting[1], adjEasting[2], adjNorthing[1], adjNorthing[2]))
      extent_union <- raster::union(extent(nsubplots.r), extent_stems)
      } else {extent_union <- extent(nsubplots.r)}
      # Now plot raster of sampled, nested subplots
      plot(nsubplots.r, ext = extent_union)
      plot(ploti, add=T)
      if (length(NArowIDs) < 16) {
        raster::text(nsubplots.r, col=1)
      # Now overlay vector outlines
      plot(nsubplotsVector, add=T)
      raster::text(subplots0.r, col = "darkgrey")
      mtext(ploti_name)
      }
 
      # Now add mapped stems if available
      if (nrow(datai) > 0) {
        with(datai, points(adjEasting, adjNorthing, cex = stemDiameter/20, pch = plant_status_symbol,
                           bg = plant_status_bg, xlab = "Easting", ylab = "Northing"))  

        with(plantStatusTab, legend(legendPos, plantStatusLevels, pt.bg = plant_status_bg, pch = plant_status_symbol,
                                    cex = 0.8, bg = rgb(1, 1, 1, 0.8)))
      }
        plotMapMade = "yes"
      }
      #################### END Section to Plot Subplots Sampled in Eventi yearj for Overstory trees DBH > 10 cm ####
      ##############################################################################################################  
      
      assign(paste('vegpliYrjL',yearj, sep=""), vegpliYrjL)
      assign(paste('vegpliYrjSmallL',yearj, sep=""), vegpliYrjSmallL)
      assign(paste('vegpliYrj',yearj, sep=""), vegpliYrj)
      
      # Compute basal area by species for live and dead trees
      spbaYeari <- vegpliYrjL %>% group_by(sppFactor) %>% 
        summarize_at(vars(basalAreaM2ha), list(sum = sum, n = length))
      totalBasalAreaLiveYeari <- sum(spbaYeari$sum)
      spbaYeari <- spbaYeari %>% mutate(ba = sum, rba = ba / totalBasalAreaLiveYeari) %>% 
        dplyr::select(sppFactor, ba, rba, n)
  
      # Compute basal area by species for dead trees - large and small
      deadSpbaYeari <- vegpliYrj %>% dplyr::filter(plantStatus %in% c("Standing dead", "Dead, broken bole")) %>% 
        group_by(sppFactor) %>% mutate(basalAreaM2ha = basalAreaM2 * 10000/totalSampledAreaTreesij) %>% 
        summarize_at(vars(basalAreaM2ha), list(ba = sum, n = length))
      
      # New Compute basal area by species for small trees
      spbaYeariSmall <- vegpliYrjSmallL %>% group_by(sppFactor) %>% 
        summarize_at(vars(basalAreaM2ha), list(sum = sum, n = length))
      totalBasalAreaLiveYeariSmall <- sum(spbaYeariSmall$sum)
      spbaYeariSmall <- spbaYeariSmall %>% mutate(ba = sum, rba = ba / totalBasalAreaLiveYeariSmall) %>% 
        dplyr::select(sppFactor, ba, rba, n)
      
    
      assign(paste('spba',yearj, sep=""), spbaYeari)
      assign(paste('spbaSmall',yearj, sep=""), spbaYeariSmall)
      assign(paste('deadspba',yearj, sep=""), deadSpbaYeari)
      # rename vars for a merge spba dataframe
      names(spbaYeari)[2:ncol(spbaYeari)] <- paste(names(spbaYeari)[2:ncol(spbaYeari)], yearj, sep="_")
      names(deadSpbaYeari)[2:ncol(deadSpbaYeari)] <- paste(names(deadSpbaYeari)[2:ncol(deadSpbaYeari)], yearj, sep="_")
      names(spbaYeariSmall)[2:ncol(spbaYeariSmall)] <- paste(names(spbaYeariSmall)[2:ncol(spbaYeariSmall)], yearj, sep="_")
      
      if (!(exists('spbaAll'))) {
        allSpp <- spbaYeari %>% dplyr::select(sppFactor) %>% full_join(select(spbaYeariSmall, sppFactor)) %>%
          full_join(select(deadSpbaYeari, sppFactor))
        spbaAll <- spbaYeari %>% full_join(allSpp) %>% replace(is.na(.), 0)
        spbaAllSmall <- spbaYeariSmall %>% full_join(allSpp) %>% replace(is.na(.), 0)
        deadSpbaAll <- deadSpbaYeari %>% full_join(allSpp) %>% replace(is.na(.), 0)
        plantStatus2 <- vegpliYrjL %>% dplyr::select(plantStatus) %>% 
          group_by(plantStatus) %>% count() %>% mutate(year = yearj)
      } else { 
        spbaAll <- merge(spbaAll, spbaYeari, by.x='sppFactor', by.y='sppFactor', all=T) %>%
          replace(is.na(.), 0)
        deadSpbaAll <- merge(deadSpbaAll, deadSpbaYeari, by.x="sppFactor", 
                             by.y="sppFactor", all=T) %>% replace(is.na(.), 0)
        spbaAllSmall <- merge(spbaAllSmall, spbaYeariSmall, by.x='sppFactor', 
                              by.y='sppFactor', all=T) %>% replace(is.na(.), 0)
        plantStatusi <- vegpliYrjL %>% dplyr::select(plantStatus) %>% count() %>% mutate(year = yearj)
        plantStatus2 <- rbind(plantStatus2, plantStatusi)
      }
      
      # Create a copy of base plantStatusTab (from all trees observed in ploti yearj)
      plantStatusTab0 <- plantStatusTab
      # Now summarize for current year's measurements
      yearjPlantStatus <- vegpliYrjL %>% dplyr::select(plantStatus) %>% 
        group_by(plantStatus) %>% count() 
      names(yearjPlantStatus)[2] <- paste('freq',yearj,sep="_")
      plantStatusTab <- merge(plantStatusTab, yearjPlantStatus, by.x = 'plantStatus', 
                              by.y = 'plantStatus', all=T)
      plantStatusTab <- plantStatusTab %>% replace(is.na(.), 0)
      
      # Repeat plantStatus summary for small live trees
      yearjPlantStatusSmall <- vegpliYrjSmallL %>% dplyr::select(plantStatus) %>% 
        group_by(plantStatus) %>% count() 
      names(yearjPlantStatusSmall)[2] <- paste('freq',yearj,sep="_")
      plantStatusTabSmall <- merge(plantStatusTab0, yearjPlantStatusSmall, 
                                   by.x = 'plantStatus', by.y = 'plantStatus', all=T)
      plantStatusTabSmall <- plantStatusTabSmall %>% replace(is.na(.), 0)
      plantStatusTabSmall$plantStatus <- plantStatusTabSmall$plantStatusLevels <- paste(
            "Small:", plantStatusTabSmall$plantStatus, sep="")
      
      ##########################################################################
      # Create a barplot of basal area by species for current measurement year
      rangeBA <- c(-0.5* max(spbaAll[,2:ncol(spbaAll)], spbaAllSmall[,2:ncol(spbaAllSmall)]), 
                   max(spbaAll[,2:ncol(spbaAll)], spbaAllSmall[,2:ncol(spbaAllSmall)]))
      sppColorsi <- spbaAll %>% dplyr::select(sppFactor) %>% left_join(sppColors, by = c("sppFactor" = "taxonID"))
      
      #baYear1BarPlot <- with(spbaYeari, barplot(unlist(spbaYeari[,2]), names.arg = sppFactor, las = 2, col = sppColorsi$color,
      #                                        space = 0.1, axisnames = FALSE, ylab = "BA m2/ha", ylim = rangeBA))
      #abline(h=0)
      # Now use mtext() for the axis labels
      #text(baYear1BarPlot, rep(rangeBA[1] * 0.1,length(baYear1BarPlot)),spbaYeari$sppFactor, srt = 90, cex = 1.0)
      #mtext(paste(ploti_name, "Overstory", yearj))
      #########################################################################################################
      # Try barplot of overstory and small trees side by side
      spbaYeariL <- spbaYeari %>% full_join(spbaYeariSmall, by = c("sppFactor" = "sppFactor"), 
                                            suffix = c("", ".sm")) %>% replace(is.na(.), 0)
      # Or
      spbaYeariSmall2 <- spbaYeariSmall %>% dplyr::select(1:2) %>% mutate(size = "sm")
      spbaYeariLong <- spbaYeariL %>% dplyr::select(1:2) %>% mutate(size = "over") %>% 
        rbind(spbaYeariSmall2)
      spbaYeariWide <- spbaYeariLong %>% pivot_wider(., names_from = sppFactor, 
                      id_cols = size, values_from = 2, values_fill = 0) %>%
                      data.frame(.)
      row.names(spbaYeariWide) <- spbaYeariWide$size
      #spbaYeariWide <- spbaYeariWide[,-1]
      spbaYeariWide <- spbaYeariWide %>% dplyr::select(-size)
      
      
      sppColorsi <- data.frame(sppFactor = names(spbaYeariWide)) %>%  
        inner_join(sppColors, by = c("sppFactor" = "taxonID"))
      
      baYear1BarPlot <- with(spbaYeariWide, barplot(as.matrix(spbaYeariWide), beside=T,
                          las = 2, col = c("darkseagreen","lightgrey"),
                          space = c(0,0.1), axisnames = FALSE, ylab = "BA m2/ha", ylim = rangeBA))
      abline(h=0)
      legend("right", legend = c("Overstory","Small"), 
             fill =c("darkseagreen", "lightgrey"), box.lty=0)
      # Now use mtext() for the axis labels
      text(baYear1BarPlot, rep(rangeBA[1] * 0.1,ncol(spbaYeariWide)),
           unlist(strsplit(paste(names(spbaYeariWide)," "), " ")), srt = 90, cex = 1.0)
      mtext(paste(ploti_name, yearj))
      #########################################################################################################
      
      # If yearj Measurement year is not the first, calculate a change over time
      if (exists('lastEventYear')) {
        yearsSinceMeasurement <- yearj - lastEventYear
        spbaLastEventYear <- get(paste("spba", lastEventYear, sep=""))
        liveBALastEventYear <- sum(spbaLastEventYear$ba)
        
        #spbaChange <- spbaYeari %>% group_by(sppFactor, .drop=F) %>% full_join(spbaLastEventYear, by = c("sppFactor" = "sppFactor")) %>% replace(is.na(.), 0) %>%
        #   mutate(baM2Change = (ba.x - ba.y) / yearsSinceMeasurement, baPercentChange = (ba.x - ba.y)/totalBasalAreaLiveLastEventYear/ yearsSinceMeasurement)
         
        #spbaChange <- spbaAll %>% mutate(baM2Change = (get(paste('ba',yearj, sep="_")) - get(paste('ba',lastEventYear, sep="_"))) / yearsSinceMeasurement,
        #                                 baPercentChange = (get(paste('ba',yearj, sep="_")) - get(paste('ba',lastEventYear, sep="_")))/
        #                                   (get(paste('ba',lastEventYear, sep="_")) + 0.000001))
        
        #rangeChange <- with(spbaChange, range(baM2Change))
        #rangeChangePercent <- with(spbaChange, range(baPercentChange))
        #rangeChangePercent[1] <- max(-1,rangeChangePercent[1])
        #rangeChangePercent[2] <- min(1,rangeChangePercent[2])
        # Instead hardcode range for % change plots to -+130%
        #rangeChangePercent <- c(-1.3, 1.3)
        
        # Now place labels closer to the x axis
        # set 'axisnames' to FALSE so the default
        # labels are not drawn. Also note that barplot() 
        # returns the bar midpoints, so set 'mp' to the return
        # values
      
          
        #baChangeBarPlot <- with(spbaChange, barplot(baM2Change, names.arg = sppFactor, las = 2, 
        #                                            space = 0.1, axisnames = FALSE, ylab = "BA m2/ha Change yr-1"))
        #abline(h=0)
        # Now use mtext() for the axis labels
        #text(baChangeBarPlot, rep(rangeChange[which.max(abs(rangeChange))] * 0.10,length(baChangeBarPlot)),spbaChange$sppFactor, 
        #     srt = 90, cex = 1.0)
        #mtext(paste(ploti_name, yearj,'-', lastEventYear))
        # Repeat for percent Change
        #varChangeBarPlot <- with(spbaChange, graphics::barplot(baPercentChange, ylim = c(-1,1), names.arg = sppFactor, 
        #                         las = 2, space = 0.1, axisnames = FALSE, ylab = "BA % Change"))
        #abline(h=0)
        # Now use mtext() for the axis labels
        #text(varChangeBarPlot, rep(rangeChangePercent[which.max(abs(rangeChangePercent))] * 0.20,length(varChangeBarPlot)),spbaChange$sppFactor, 
        #     srt = 90, cex = 1.0)
        #mtext(paste(ploti_name, yearj,'-', lastEventYear))
      }
      
  
      
      lastEventYear <- yearj
      yearsp <- c(yearsp, yearj)
    } 
}



# Try to create Alluvial plot of plantStatus Transitions
# Combine all the byYear dataframes that have been screened for duplicate records etc.
# Find all the byYear dataframes to bind into one for summaries - include live and dead trees for transitions
listVegyrpli <- grep("vegpliYrj", ls(), value=T)
listVegyrpli <- listVegyrpli[-grep("L",listVegyrpli)]
listVegyrpli <- listVegyrpli[-1]

vegpliAll <- get(listVegyrpli[1])

# First, calculate totals numbers of Live and Dead Stems per year by Species
live_n <- spbaAll %>% dplyr::select(sppFactor, grep("n_", names(spbaAll))) %>% arrange(sppFactor)
live_n_small <- spbaAllSmall %>% dplyr::select(sppFactor, grep("n_", names(spbaAllSmall))) %>% arrange(sppFactor)
dead_n <- deadSpbaAll %>% dplyr::select(sppFactor, grep("n_", names(deadSpbaAll))) %>% arrange(sppFactor)
dead_tot <- dead_n %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))
live_tot <- live_n %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))
live_small_tot <- live_n_small %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))

# Also calculate totals by BA for later plotting
live_ba <- spbaAll %>% dplyr::select(sppFactor, grep("ba_", names(spbaAll))) %>% arrange(sppFactor)
live_ba <- live_ba %>% dplyr::select(-grep("rba_", names(live_ba)))
live_ba_small <- spbaAllSmall %>% dplyr::select(sppFactor, grep("ba_", names(spbaAllSmall))) %>% arrange(sppFactor)
live_ba_small <- live_ba_small %>% dplyr::select(-grep("rba_", names(live_ba_small)))
dead_ba <- deadSpbaAll %>% dplyr::select(sppFactor, grep("ba_", names(deadSpbaAll))) %>% arrange(sppFactor)
dead_ba_tot <- dead_ba %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))
live_ba_tot <- live_ba %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))
live_ba_small_tot <- live_ba_small %>% dplyr::select(-sppFactor) %>% summarize_all(list(sum = sum))


# Join live and dead tables to include all observed species
if (nrow(dead_n) > 0) {
  live_n <- live_n %>% full_join(dplyr::select(dead_n, sppFactor), 
                                 by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  live_ba <- live_ba %>% full_join(dplyr::select(dead_n, sppFactor), 
                                   by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  
}
if (nrow(live_n_small) > 0) {
  live_n_small <- live_n_small %>% full_join(dplyr::select(live_n, sppFactor), 
                                             by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  live_ba_small <- live_ba_small %>% full_join(dplyr::select(live_n, sppFactor), 
                                               by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  live_n <- live_n %>% full_join(dplyr::select(live_n_small, sppFactor), 
                                 by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  live_ba <- live_ba %>% full_join(dplyr::select(live_ba_small, sppFactor), 
                                   by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  
}
if (nrow(live_n) > 0) {
  dead_n <- dead_n %>% full_join(dplyr::select(live_n, sppFactor), 
                                 by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
  dead_ba <- dead_ba %>% full_join(dplyr::select(live_n, sppFactor), 
                                   by=c("sppFactor" = "sppFactor"), fill= 0 ) %>% 
    replace(is.na(.), 0) %>% arrange(sppFactor)
}


# Make alluvial plots and time series of live and dead BA, only if remeasured data found.
if (length(listVegyrpli) > 1) {

  for (k in 2:length(listVegyrpli)) {
    vegpliAll <- rbind(vegpliAll, get(listVegyrpli[k]))
  }
  
# Add a level to capture NAs in plantStatus. Remap plantStatus for small trees.
# Categorize small trees as live or dead for simplified alluvial plot
vegpliAll$plantStatus2 <- levels(vegpliAll$plantStatus)[vegpliAll$plantStatus]
vegpliAll$plantStatus2[which(vegpliAll$stemDiameter < 10 & 
                       grepl('Live', vegpliAll$plantStatus, ignore.case=T))] <- "Live, _small"
vegpliAll$plantStatus2[which(vegpliAll$stemDiameter < 10 & 
                       grepl('Dead', vegpliAll$plantStatus, ignore.case=T))] <- "Dead, small"


levelsPli <- levels(vegpliAll$plantStatus)
levelsPli2 <- levels(as.factor(vegpliAll$plantStatus2))
# Reorder levels for use in Alluvial plots
vegpliAll$plantStatus <- factor(vegpliAll$plantStatus, 
                                c("No standing record",levelsPli[c(grep("Dead", levelsPli),
                                  grep("dead",levelsPli),grep("Live, ", levelsPli, fixed=T))],"Live")) # levels(vegpliAll$plantStatus)[c(2:6,1,7)]
# Reorder levels for new PlantStatus2 that specifies small Live or Dead trees
vegpliAll$plantStatus2 <- factor(vegpliAll$plantStatus2, 
                                c("No standing record",levelsPli2[c(grep("Dead", levelsPli2),
                                grep("dead",levelsPli2),grep("Live, ", levelsPli2, fixed=T))],"Live")) # levels(vegpliAll$plantStatus)[c(2:6,1,7)]



# First, have to take care of any duplicate measurements of individuals in the same eventID. Documentation suggests using the
# Most recent record, but there is not field indicating how recent a record is (only differ by uid, which is an alpha-numeric code, not time)
# Not sure how to handle multistem, small trees...
print(paste("nrows vegpliAll = ", nrow(vegpliAll)))
vegpliAll <- vegpliAll %>% group_by(individualID) %>% mutate(meanDBH = mean(stemDiameter), diffMeanDBH = abs(stemDiameter - meanDBH)) %>% 
                    arrange(individualID, eventID.x)
# Find and filter our duplicates of individualID and eventID.x. 
vegpliAll <- vegpliAll %>% group_by(individualID, eventID.x) %>%  dplyr::arrange(individualID, eventID.x, diffMeanDBH) %>%
            mutate(dup_DBH = duplicated(individualID, eventID.x)) %>% 
            dplyr::filter(!(dup_DBH))
print(paste("nrows vegpliAll after filter duplicate records = ", nrow(vegpliAll)))

# Use pivot_wider and grouping to spread the data frame out to plantStatus by measurement year
# Encountered problem grouping by individualID once adding in small stems. Unclear if there can
# be multistemmed small individuals with same individualID. Try grouping by uid.x instead.
# Switch back. For now, select first of small trees with same individualID. Impact should be small.
plantStatus3 <- vegpliAll %>% group_by(individualID, eventID.x) %>% dplyr::select(individualID, eventID.x, plantStatus) %>% 
  slice(1) %>% arrange(eventID.x, individualID) %>% pivot_wider(., names_from = eventID.x, values_from = plantStatus) %>%  
  arrange(individualID) %>% as.data.frame()
stemDBH3 <- vegpliAll %>% group_by(individualID, eventID.x) %>% dplyr::select(individualID, eventID.x, stemDiameter) %>% 
  slice(1) %>% arrange(eventID.x, individualID) %>% pivot_wider(., names_from = eventID.x, values_from = stemDiameter) %>% dplyr::arrange(ncol(.)-2, ncol(.)-1) %>% as.data.frame()
plantStatus3b <- vegpliAll %>% group_by(individualID, eventID.x) %>% slice(1) %>% 
    group_by(individualID) %>% mutate(maxBasalAreaM2ha = max(basalAreaM2ha, na.rm=T)) %>%
    group_by(eventID.x, plantStatus) %>% dplyr::select(individualID, eventID.x, plantStatus, maxBasalAreaM2ha) %>% 
    unique() %>% arrange(eventID.x, individualID) %>% pivot_wider(., names_from = eventID.x, values_from = plantStatus)%>%
  arrange(individualID)

# Repeat for new plantStatus2 that addes in small trees as there own grouping
plantStatus23 <- vegpliAll %>% group_by(individualID, eventID.x) %>% dplyr::select(individualID, eventID.x, plantStatus2) %>% 
  slice(1) %>% arrange(eventID.x, individualID) %>% 
  pivot_wider(., names_from = eventID.x, values_from = plantStatus2) %>% arrange(individualID) %>%  as.data.frame()
stemDBH3 <- vegpliAll %>% group_by(individualID, eventID.x) %>% dplyr::select(individualID, eventID.x, stemDiameter) %>% 
  slice(1)  %>% arrange(eventID.x, individualID) %>% pivot_wider(., names_from = eventID.x, values_from = stemDiameter) %>% dplyr::arrange(ncol(.)-2, ncol(.)-1) %>% as.data.frame()
plantStatus23b <- vegpliAll %>% group_by(individualID, eventID.x) %>% slice(1) %>% 
  group_by(individualID) %>% mutate(maxBasalAreaM2ha = max(basalAreaM2ha, na.rm=T)) %>% 
  group_by(eventID.x, plantStatus2) %>% dplyr::select(individualID, eventID.x, plantStatus2, maxBasalAreaM2ha) %>% 
  unique() %>% arrange(eventID.x, individualID) %>% pivot_wider(., names_from = eventID.x, values_from = plantStatus2) %>%
  arrange(individualID)

# Find any records that have no DBH or Basal area recorded and remove from plantStatus
noDBH <- which(is.infinite(plantStatus23b$maxBasalAreaM2ha))
plantStatus23 <- plantStatus23 %>% dplyr::filter(!(is.infinite(plantStatus23b$maxBasalAreaM2ha)))
plantStatus23b <- plantStatus23b %>% dplyr::filter(!(is.infinite(plantStatus23b$maxBasalAreaM2ha)))

print(paste("Number of trees with no DBH = ", length(noDBH)))
# Now Count the frequency of each combination of plantStatus or plantStatus2 transitions across measurement events
listEvents <- sort(unique(vegpliAll$eventID.x))

plantStatus4 <- plantStatus23 %>% dplyr::select(-individualID) %>% 
  group_by_at(vars(one_of(listEvents))) %>% count()
plantStatus4 <- plantStatus4 %>% replace(is.na(.), "No standing record")
plantStatus4 <- plantStatus4 %>% dplyr::arrange(vars(listEvents)) %>% as.data.frame()
# Filter for now, Option to remove rows for observations that were measured only in first year, not others
# May represent changes in protocol
# Alternative, summarize based on sum of max basal area for each plant status sequence grouping
plantStatusBa <- plantStatus23b %>% dplyr::select(-individualID) %>% 
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

# Plot Total numbers of Live and Dead Stems per year by Species

# Update colors to include live and dead species
sppColorsi <- live_n %>% dplyr::select(sppFactor) %>% left_join(sppColors, by = c("sppFactor" = "taxonID"))
sppColorsi$color[which(sppColorsi$sppFactor == "NAtaxonID")] <- "thistle"
sppColorsDeadi <- dead_n %>% dplyr::select(sppFactor) %>% left_join(sppColors, by = c("sppFactor" = "taxonID"))
sppColorsLegendi <- rbind(sppColorsi, sppColorsDeadi) %>% distinct()
sppColorsLegendi <- sppColorsLegendi %>% group_by(sppFactor) %>% slice(1) %>% arrange(spp)

# Set margins with par
par(mar = c(5.1, 4.1, 2.1, 2.1))
#
plot(range(yearsp), c(0, max(live_n[,2:ncol(live_n)], dead_n[,2:ncol(dead_n)], live_n_small[,2:ncol(live_n_small)])),
     type = "n", xlab = "year", ylab = "n stems", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()
 for (m in 1:nrow(live_n)) {
   lines(yearsp, live_n[m,2:ncol(live_n)], col = sppColorsi$color[m], lwd = 1.5)
   points(yearsp, live_n[m,2:ncol(live_n)], pch = 21, bg = sppColorsi$color[m], cex = 1.5)
   # Plot dead stems as well
   lines(yearsp, dead_n[m,2:ncol(live_n)], col = sppColorsi$color[m], lwd = 1.5, lty = 2)
   points(yearsp, dead_n[m,2:ncol(live_n)], pch = 23, col = "grey", bg = sppColorsi$color[m], cex = 1.5, lty =2)
   # Plot small live trees as well
   lines(yearsp, live_n_small[m,2:ncol(live_n_small)], col = sppColorsi$color[m],lty=3,  lwd = 1.5)
   points(yearsp, live_n_small[m,2:ncol(live_n_small)], pch = "+", col = sppColorsi$color[m], cex = 1.5)
   
   }
mtext(paste("N Stems","   ",ploti_name), line = 0)
legend("topright", sppColorsi$sppFactor, bty = "n", pt.bg = sppColorsi$color, pch = 21, pt.cex = 1)
legend("topleft", c("live","live_small","dead"), bty="n", pch = c(21,3,23), lty=c(1,3,2), 
       lwd=c(1.5,1.5,1.5), pt.cex = 1.5, 
       col = c(1,"darkgrey"))

# Plot BA Change of Live and Dead Stems per year by Species
plot(range(yearsp), c(0, max(live_ba[,2:ncol(live_ba)], dead_ba[,2:ncol(dead_ba)], live_ba_small[,2:ncol(live_ba_small)])),
     type = "n", xlab = "year", 
     ylab = "basal area (m2/ha)", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()
for (m in 1:nrow(live_ba)) {
  lines(yearsp, live_ba[m,2:ncol(live_ba)], col = sppColorsi$color[m], lwd = 1.5)
  points(yearsp, live_ba[m,2:ncol(live_ba)], pch = 21, bg = sppColorsi$color[m], cex = 1.5)
  # Plot dead stems as well
  lines(yearsp, dead_ba[m,2:ncol(dead_ba)], col = sppColorsi$color[m], lwd = 1.5, lty = 2)
  points(yearsp, dead_ba[m,2:ncol(dead_ba)], pch = 23, col = "grey", bg = sppColorsi$color[m], cex = 1.5, lty =2)
  # Plot small live trees as well
  lines(yearsp, live_ba_small[m,2:ncol(live_ba_small)], col = sppColorsi$color[m],lty=3,  lwd = 1.5)
  points(yearsp, live_ba_small[m,2:ncol(live_ba_small)], pch = "+", col = sppColorsi$color[m], cex = 1.5)
  
}
mtext(paste("Basal Area","   ",ploti_name), line = 0)
legend("topright", sppColorsi$sppFactor, bty = "n", pt.bg = sppColorsi$color, pch = 21, pt.cex = 1)
legend("topleft", c("live","live_small","dead"), bty="n", pch = c(21,3,23), lty=c(1,3,2), 
       lwd=c(1.5,1.5,1.5), pt.cex = 1.5, 
       col = c(1,"darkgrey"))

# Plot live, dead totals
plot(range(yearsp), c(0, max(live_ba_tot, dead_ba_tot, live_ba_small_tot)),type = "n", xlab = "year", 
     ylab = "basal area (m2/ha)", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()
lines(yearsp, live_ba_tot, lwd = 2)
points(yearsp, live_ba_tot, pch = 21, cex=2, bg = "darkgrey")
lines(yearsp, live_ba_small_tot, lwd = 2, lty=3)
points(yearsp, live_ba_small_tot, pch = 3, cex=2, bg = "darkgrey")

lines(yearsp, dead_ba_tot, lwd = 2, lty = 2, col = "darkgrey")
points(yearsp, dead_ba_tot, pch = 23, cex=2, bg = "grey")
mtext(paste("Basal Area","   ",ploti_name), line = 0)
# Reset margins with par
par(mar = c(5.1, 4.1, 4.1, 2.1))
    }

if (makepdf == "yes") {
dev.off()
  }

# Reformat plot-level summaries and merge into site level dataframes
ba_tot_longi <- data.frame(plot = ploti_name, year = yearsp, 
                                live_ba_tot = unlist(unname(live_ba_tot)),
                           dead_ba_tot = unlist(unname(dead_ba_tot)),
                           live_ba_small_tot = unlist(unname(live_ba_small_tot)))
ba_tot_sitei <- rbind(ba_tot_sitei, ba_tot_longi)
rm(ba_tot_longi)

}

# Clean up any outliers
ba_tot_sitei <- ba_tot_sitei %>% dplyr::filter(live_ba_tot < 150)

# Add variables for change in each plot-level annual summary of ba
ba_tot_sitei <- ba_tot_sitei %>% group_by(plot) %>% 
  mutate(live_ba_ch = live_ba_tot - lag(live_ba_tot, n=1),
         dead_ba_ch = dead_ba_tot - lag(dead_ba_tot, n=1),
         live_ba_small_ch = live_ba_small_tot - lag(live_ba_small_tot, n=1))


# Load some Landsat data to compare
#ndwi <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\Landsat_processed\\ee_Landsat_time_series_out_NDWI_sim_STEI_NEON_2020-10-03.csv", header=T)
#ndwi <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\Landsat_processed\\ee_Landsat_time_series_out_NDWI_sim_GRSM_NEON_2021-04-07_v3.csv", header=T)
#ndwi <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\Landsat_processed\\ee_Landsat_time_series_out_NDWI_sim_BART_NEON_2021-04-19_v3.csv", header=T)
ndwi <- read.csv("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\Landsat_processed\\ee_Landsat_time_series_out_NDWI_sim_HARV_NEON_2021-04-19_v3.csv", header=T)


# Add some lagged and leading variables
ndwi <- ndwi %>% group_by(plot) %>% arrange(plot, year) %>% 
  mutate(fbmi_med_lead_1 = lead(fbmi_med, 1), fbmi_med_lag_1 = lag(fbmi_med, 1),
         fbmi_med_lag_2 = lag(fbmi_med, 2), fbmi_med_lead_2 = lead(fbmi_med, 2),
         fbmi_med_lead_3 = lead(fbmi_med, 3), fbmi_med_lag_3 = lag(fbmi_med, 3),
         resids_med_lead_1 = lead(resids_median, 1), resids_med_lag_1 = lag(resids_median, 1),
         ndwi_med_lead_1 = lead(r_obs_median, 1), ndwi_med_lag_1 = lag(r_obs_median, 1))

bandwi <- ba_tot_sitei %>% inner_join(ndwi)

# Set plotting variable name
plot_x <- "spline_median"
plot_y <- "dead_ba_ch"

with(bandwi, plot(get(plot_x), get(plot_y), pch=21, bg=as.factor(plot), cex=1.5,
                  xlab = paste(plot_x), ylab = paste(plot_y)))
with(bandwi, text(get(plot_x), get(plot_y), labels = substr(plot, nchar(plot)-2, nchar(plot)), pos = 4))

# Try simple OLS linear models to predict plot-level basal area and change from Landsat
with(bandwi, summary(lm(dead_ba_tot ~ fbmi_med + fbmi_med_lag_1 + fbmi_med_lag_2 + fbmi_med_lag_3 + spline_median)))