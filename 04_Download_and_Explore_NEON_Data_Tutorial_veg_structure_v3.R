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
library(httr)
library(jsonlite)
library(geoNEON)
library(raster)
#library(rhdf5)
library(hdf5r)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(tidyr)
library(alluvial)
library(ggplot2)

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

makepdf <- "no" # or "no"
writeShapeFile <- "no"
# Set a save path to match path for downloaded files
save_path <- "data"
setwd(save_path)

# Create functions
# not in
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#
# Modify the file path to match the path to your zip file - Run this again if you want to update your download for new data
#stackByTable("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-woody-plant.zip")
#stackByTable("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\NEON_struct-plant", folder=T)

#stackByTable("C:\\Users\\janer\\Dropbox\\Projects\\NEON_Macro\\data\\NEON_litterfall.zip")
# New, run stackByTable for non-structural perennial veg as well
#stackByTable("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_struct-non-herb-perennial-veg.zip")
#stackByTable("C:\\Users\\janer\\Dropbox\\code\\Rcode\\NEON_tutorials\\data\\NEON_survey-downed-wood.zip")

# Create a vector of NEON sites for this study - disturbed sites...
studySites <- c('ABBY','BART','BLAN','GRSM','GUAN','HARV','JERC','MLBS','OSBS','SCBI',
                'SOAP','STEI','TALL','TEAK','UKFS','UNDE')

#We'll read in the vst_mappingandtagging and vst_apparentindividual files:
vegmap0 <- readTableNEON("NEON_struct-woody-plant\\stackedFiles\\vst_mappingandtagging.csv",
                        "NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
# Read in plot locations:
if (!(exists("plots_poly"))) {
#  plotsxy <- readOGR(dsn="All_NEON_TOS_Plots_V8",layer="All_NEON_TOS_Plot_Points")
#  plots_poly <- readOGR(dsn="All_NEON_TOS_Plots_V8",layer="All_NEON_TOS_Plot_Polygons")
  plotsxy <- readOGR(dsn="All_NEON_TOS_Plots_V8",layer="All_NEON_TOS_Plot_Points_V8")
  plots_poly <- readOGR(dsn="All_NEON_TOS_Plots_V8",layer="All_NEON_TOS_Plot_Polygons_V8")
  
}

# Try excluding rows with pointID == NA to get getLosTOS to work
#vegmap <- vegmap0[which(!(is.na(vegmap0$pointID))),]
# The above line subsamples down to only mapped individuals. But we want all individual records for plot summaries!
# NEW - 2021-06-12 - there is a new TempStemID field that should help with prior errors
# the eventID x individualID x tempStemID fields should be unique within this table 
vegind0 <- readTableNEON("NEON_struct-woody-plant\\stackedFiles\\vst_apparentindividual.csv",
                        "NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vegind)
vstvar <- read.csv("NEON_struct-woody-plant\\stackedFiles\\variables_10098.csv")
#View(vstvar)
vstval <- read.csv("NEON_struct-woody-plant\\stackedFiles\\validation_10098.csv")
#View(vstval)
vplot0 <- read.csv("NEON_struct-woody-plant\\stackedFiles\\vst_perplotperyear.csv")

# Create a table of all possible plantStatus from vegind table.
list_plant_status <- levels(as.factor(vegind0$plantStatus))
table(vegind0$plantStatus)

plant_status_symbol <- c(17,3,21,21,10,21,21,21,8,13,12,9,7,4,5,17)
plant_status_bg <- c("gray",1,"green","orange","olivedrab","purple","red","gray",1,1,1,1,1,1,1,"red")
plant_status_vis <- data.frame(plant_status = list_plant_status,
                               plant_status_symbol=plant_status_symbol,
                               plant_status_bg=plant_status_bg)
rm(plant_status_symbol, plant_status_bg)

# Thin data to study area sites
vplot0 <- vplot0 %>% dplyr::filter(siteID %in% studySites)
vegind0 <- vegind0 %>% dplyr::filter(siteID %in% studySites)
vegmap0 <- vegmap0 %>% dplyr::filter(siteID %in% studySites)

# New 2021-07-13 - Observe that there are multiple rows per event id for plots in vst_perplotperyear
# Subset to just tree or shrub measurement info and most recent publication date
# vplotT <- vplot0 %>% dplyr::filter((treesPresent == "Y" | shrubsPresent == "Y")) # This misses good data. CHANGE
# Because plot area records sometimes differ, we may need to come back to vplot0 when most recent obs has error
vplotT <- vplot0
print(paste('Nrows vplotT before slicing for duplicate records', nrow(vplotT)))
vplotT <- vplotT %>% mutate(year = as.numeric(substr(eventID,nchar(eventID) - 3, 
  nchar(eventID)))) %>% #mutate(year = format(as.Date(date), format = '%Y')) %>%
  arrange(plotID, year, desc(date), desc(publicationDate)) %>%
  group_by(plotID, year, date) %>%
  slice(1) %>%
  ungroup()

print(paste('Nrows vplotT AFTER slicing for duplicate records', nrow(vplotT)))
# Repeat, slicing most recent date for earlier data with multiple samples per year
vplotT <- vplotT %>% 
  arrange(plotID, year, desc(date)) %>%
  group_by(plotID, year) %>%
  slice(1) %>%
  ungroup()

print(paste('Nrows vplotT AFTER slicing again for duplicate records', nrow(vplotT)))

# Use vplot0 in case some record rows are missing sampledArea observations
plotAreaYear <- vplot0 %>% dplyr::filter(siteID %in% studySites) %>% 
  mutate(year = substr(eventID, nchar(eventID) - 3, nchar(eventID))) %>% 
  group_by(plotID, eventID) %>% dplyr::select(plotID, eventID, year,
                                              totalSampledAreaTrees, totalSampledAreaShrubSapling, 
                                              totalSampledAreaLiana, uid, release) %>% 
  arrange(plotID, eventID) %>% data.frame()

#First, use the geoNEON package to calculate stem locations:
# New, read in perennial vegetation data as well. Not used.
#nstper <- readTableNEON("NEON_struct-non-herb-perennial-veg\\stackedFiles\\nst_perindividual.csv",
#                            "NEON_struct-non-herb-perennial-veg\\stackedFiles\\variables_10045.csv")
#vplotper <- read.csv("NEON_struct-non-herb-perennial-veg\\stackedFiles\\vst_perplotperyear.csv")

# New, follow steps in perennial veg user guide to get finer grain spatial info. Not used.
# New result is smaller than vegind, but we only use vplot0 data for trees and shrubs here...
vst_join <- vegind0 %>% mutate(date = as.character(date)) %>% 
  dplyr::inner_join(vplotT, by=c("namedLocation","domainID","siteID","plotID","eventID"))

# Inspect totalSampledAreaShrubSapling
vst_join %>% dplyr::select(totalSampledAreaShrubSapling) %>% table()

# New, read in CWD downed wood data to check for dead individuals in prior live surveys.
cdw <- readTableNEON("NEON_survey-downed-wood\\stackedFiles\\cdw_fieldtally.csv",
                    "NEON_survey-downed-wood\\stackedFiles\\variables_10010.csv")
cdw <- cdw %>% dplyr::filter(siteID %in% studySites)
# New 2021-08-15, read in fine litter data
litt <- readTableNEON("NEON_litterfall\\stackedFiles\\ltr_massdata.csv",
                      "NEON_litterfall\\stackedFiles\\variables_10033.csv")
# Add a year variable from collectDate and thin to study area sites
# Consider changing "year" to come from eventID if planning to use with other data.
litt <- litt %>% mutate(year = format(as.Date(collectDate), format = '%Y'),
                        mo = format(as.Date(collectDate), format = '%m')) %>%
  dplyr::filter(siteID %in% studySites)
# Read in "ltr_pertrap" file independently
lplot0 <- read.csv("NEON_litterfall\\stackedFiles\\ltr_pertrap.csv", header=T)

# Try using geoNEON package to get locations for litter traps. New 2021-12-06
# littmap <- geoNEON::getLocTOS(litt, "ltr_pertrap")
# Above produced thousands of errors [1] "Subplot  and clip cell 778 is not a valid location"
# Try manual approach instead...Also filter to study area sites
lplot <- lplot0 %>% mutate(namedLocationSubplot = paste(namedLocation, ".", subplotID, sep = ""),
                           clipCellNumber = substr(trapID, nchar(trapID)-2, nchar(trapID)),
                           utmEPlusOffset = NA, utmNPlusOffset = NA, utmUncert = NA, utmZone = NA) %>%
  dplyr::filter(siteID %in% studySites)

# 1st Read in clip cell lookup table that you downloaded via link in manual. This is a fixed table.
clip.offset <- read.csv("NEON_litterfall\\stackedFiles\\clipCellNumber_lookup.csv", header=T)

names(vegmap0)
# If you want to update locations with updated data, run line below
#vegmap <- geoNEON::getLocTOS(vegmap0, "vst_mappingandtagging") # Last run, 2021-12-19
# write.csv(vegmap, "NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", row.names=F)
# Otherwise, read in from saved file
vegmapOut <-  read.csv("NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", header=T)
names(vegmapOut)
# Stored vegmapOut is only for mapped individuals. Use all for plot-level summaries

# Thin for data only from study sites
vegmapOut <-  vegmapOut %>% dplyr::filter(siteID %in% studySites)

# vegmapOut has some duplicate measurements of individualID (trees). 
#Need to slice to use only the most recent record for each tree using date.
print(paste('Nrows vegmapOut before slicing for duplicate individualID', nrow(vegmapOut)))
vegmapOut <- vegmapOut %>%
  arrange(plotID, individualID, desc(date)) %>%
  group_by(individualID) %>%
  slice(1) %>%
  ungroup() %>% 

print(paste('Nrows vegmapOut AFTER slicing for duplicate individualID', nrow(vegmapOut)))

# Repeat for vegmap0
# Need to save a version of vegmap with locations so you don't have to re-run above 
# lengthly step each time. Code below sorts individual records and takes the most recent (date).
print(paste('Nrows vegmap0 before slicing for duplicate individualID', nrow(vegmap0)))
vegmap <- vegmap0 %>%
  arrange(plotID, individualID, desc(date)) %>%
  group_by(individualID) %>%
  slice(1) %>%
  ungroup()

print(paste('Nrows vegmap= AFTER slicing for duplicate individualID', nrow(vegmap)))

#write.csv(vegmap, "NEON_struct-woody-plant\\stackedFiles\\vst_vegmap.csv", row.names=F)

# add variable "year", then make a copy.
# 2021-07-06 Changed to vst_join here to propagate correct plot area through.
vegind1 <- vst_join %>% group_by(plotID) #%>% 
  #mutate(year.y = year, year = format(as.Date(date.x), format = '%Y'))
# Thin out vegind data to EXCLUDE non-tree growth forms shrubs and saplings, lianas for now.
vegind <- vegind1 %>% dplyr::filter(grepl("tree", growthForm))

# The following produces table of extent 0. This shows that no growth forms with 
# The word "tree" are associated with a nestedSubplotAreaShrubSapling. This means
# That "small tree" is sampled at the whole plot scale. Why is the User Guide ambiguous!!!?
#vegind %>% ungroup() %>% dplyr::select(nestedSubplotAreaShrubSapling) %>% table()
# 2021-07-13 - have this working now, I think.
vegind %>% ungroup() %>% dplyr::select(totalSampledAreaShrubSapling) %>% table()
vegind %>% ungroup() %>% dplyr::select(totalSampledAreaTrees) %>% table()


# And now merge the un-duplicated mapping data with the individual measurements. A subset of trees are mapped.
# individualID is the linking variable, the others are included to avoid having duplicate columns.
veg <- merge(vegind, vegmap, by=c("individualID","namedLocation",
                                  "domainID","siteID","plotID"), all.x = T)

# Make a copy of current veg table
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
rm(spj, j, splist, spplist)

# merge veg table with species and plant_status_vis
veg0 <- veg0 %>% inner_join(spp_distinct)
veg <- veg0 %>% inner_join(plant_status_vis,by=c("plantStatus"="plant_status"))

# Check for individuals that have NA in taxonID in some year but have taxonID in other year, assign that to all years
veg <- veg %>% group_by(individualID) %>% mutate(taxonID2 = case_when(is.na(taxonID) & 
  !(is.na(lead(taxonID, n=1, order=year))) ~ lead(taxonID, n=1, order=year),
  is.na(taxonID) & !(is.na(lead(taxonID, n=2, order=year))) ~ lead(taxonID, n=2, order=year),
  is.na(taxonID) & !(is.na(lead(taxonID, n=3, order=year))) ~ lead(taxonID, n=3, order=year),
  is.na(taxonID) & !(is.na(lead(taxonID, n=4, order=year))) ~ lead(taxonID, n=4, order=year),
  is.na(taxonID) & !(is.na(lead(taxonID, n=5, order=year))) ~ lead(taxonID, n=5, order=year),
  is.na(taxonID) & !(is.na(lag(taxonID, n=1, order=year))) ~ lag(taxonID, n=1, order=year),
  is.na(taxonID) & !(is.na(lag(taxonID, n=2, order=year))) ~ lag(taxonID, n=2, order=year),
  is.na(taxonID) & !(is.na(lag(taxonID, n=3, order=year))) ~ lag(taxonID, n=3, order=year),
  is.na(taxonID) & !(is.na(lag(taxonID, n=4, order=year))) ~ lag(taxonID, n=4, order=year),
  is.na(taxonID) & !(is.na(lag(taxonID, n=5, order=year))) ~ lag(taxonID, n=5, order=year),
  TRUE ~ taxonID),
  taxonIDraw = taxonID, taxonID = taxonID2)

# Quick tabulation of number of distinct years vegetation structure was measured by plotID
plot_year <- veg %>% group_by(plotID) %>% dplyr::select(year) %>% distinct()
# Exclude data from 2014 for now
#plot_year <- plot_year %>% dplyr::filter(year > 2014)
n_years_meas <- table(plot_year$plotID) %>% data.frame()
print(n_years_meas)

# View remeasurement frequency for given site
n_years_meas[grep("ABBY",n_years_meas$Var1),] %>% dplyr::arrange(desc(Freq))

# Try a different method, using vplot0 (from vstperplotyear)
#vplot0 %>% dplyr::filter(siteID == sitei) %>% dplyr::select(plotID) %>% distinct() %>% arrange(plotID)
#########################################################################################################
# Determine number of sampling events for a site
#########################################################################################################
print(studySites)
# Exclude sites with no re-measurement of tower plots
#studySites <- studySites[-c(11,14)]
for (i in 1:length(studySites)) {
sitei <- studySites[i]#"ABBY"
plot_type <- "tower" #"tower" # "distributed"

# When looking for remeasured plots, only analyze plots with at least 1 reameasurement
# Relax to zero for sites/plots that haven't been remeasured yet...
if (plot_type == "tower" & !(sitei %in% c("SOAP","TEAK","GUAN"))) {
  min_times_remeasured <- 1 } else {min_times_remeasured <- 0}

# Alternate method, use vplot table to determine "official" measurement events
events <- vplotT %>% dplyr::filter(siteID == sitei) %>% 
  dplyr::select(eventID) %>% distinct() %>% unlist()

# Subset tree, litter and plot data tables to site level
vegi <- veg %>% dplyr::filter(siteID == sitei)
vplot <- vplotT %>% dplyr::filter(siteID == sitei)
eventsi <- sort(unique(vegi$eventID.x)) # Note, there are 2 eventID's after join vegind to vegmapping, because mapping coordinates were collected in a particular year.
litti <- litt %>% dplyr::filter(siteID == sitei)

# New, subset Coarse Woody Debris tally table to track downed individuals
cdwi0 <- cdw %>% dplyr::filter(siteID == sitei)

# Create a linking variable from individualID to join to cdwi table
vegi$vstTagID <- as.character(as.numeric(substr(vegi$individualID, 19, nchar(vegi$individualID))))
vstTagIDList <- sort(unique(vegi$vstTagID))
# For this analysis, limit Coarse Woody Debris to decayClass 1 (freshly fallen)
# To get recently disturbed tree information.
cdwi <- cdwi0 %>% dplyr::filter(vstTagID %in% vstTagIDList | grepl(1, decayClass))

# Now add a new variable, recreating the individualID of formally live tree stem.
cdwi <- cdwi %>% mutate(vstIndividualID = paste("NEON","PLA",domainID,siteID,
                              sprintf("%05d", as.numeric(vstTagID)), sep="." ))

# Exclude data from 2014 for now
if (length(grep("2014", eventsi)) > 0) {
  eventsi <- eventsi[-grepl("2014", eventsi)]
} 

# Determine how often plots on site were remeasured
sitei_nYearsMeas <- n_years_meas[grep(sitei, n_years_meas$Var1),] %>% arrange(Freq)
print(sitei_nYearsMeas)

# Why is below vplot0? Should it just be vplot? Or vplotT?
typePlotsi <- vplot0 %>% dplyr::filter(siteID == sitei & plotType == plot_type) %>% 
  dplyr::select(plotID) %>% distinct()
plotsi_remeasured <- sitei_nYearsMeas %>% dplyr::filter(Freq > min_times_remeasured)  %>% 
  inner_join(typePlotsi, by = c("Var1" = "plotID")) %>% 
  dplyr::select(Var1) %>% unlist() %>% unname() %>% 
  as.character() %>% sort()
print(plotsi_remeasured)

# Look at frequency of tree species measured at this site
treeSppFreq <- vegi %>% ungroup() %>% dplyr::filter(stemDiameter >= 10  & grepl("tree", growthForm)) %>% 
  dplyr::select(taxonID) %>% table() %>% sort(., decreasing = T)
treeSppFreqSm <- vegi %>% ungroup() %>% dplyr::filter(stemDiameter < 10  & grepl("tree", growthForm)) %>% 
  dplyr::select(taxonID) %>% table() %>% sort(., decreasing = T)

print(treeSppFreq)
print(treeSppFreqSm)
print(eventsi)

siteTreeTaxonID <- sort(unique(c(names(treeSppFreq), names(treeSppFreqSm))))
siteTreeSppSciName <- vegi %>% ungroup() %>% dplyr::select(taxonID, scientificName) %>% 
  distinct

# Create a color palette for observed overstory tree species for the site
set.seed(5489)
conColors <- brewer.pal(9, "Greens")
siteTreeColors <- siteTreeColors <- c(brewer.pal(11,"Spectral"), 
                                      brewer.pal(12, "Set3"), 
                                      brewer.pal(8, "Accent"),
                                      brewer.pal(11, "PuOr"), 
                                      brewer.pal(9, "Greys")[1:6])
# Reshuffle to randomize
siteTreeColors <- sample(siteTreeColors, length(siteTreeTaxonID), replace = T)

# First, create siteTreeSpp table that uses species codes and taxonID
siteTreeSpp <- siteTreeTaxonID %>% data.frame(taxonID = .) %>% inner_join(spp_distinct) %>% 
  dplyr::select(spp) %>% unname() %>% unlist()

# Assign dark green colors to conifer genus
conIndices <- c(grep("abie", siteTreeSpp), grep("pinu", siteTreeSpp), grep("pice", siteTreeSpp), 
              grep("tsug", siteTreeSpp), grep("juni",siteTreeSpp), grep("lari", siteTreeSpp), 
              grep("thuj", siteTreeSpp),grep("pseumenz", siteTreeSpp))
siteTreeColors[conIndices] <- conColors[(length(conColors)-length(conIndices)+1):length(conColors)]
sppColors <- data.frame(spp = siteTreeSpp, color = siteTreeColors, taxonID = siteTreeTaxonID)
sppColors$isConifer <- 0
sppColors$isConifer[conIndices] <- 1
# Check colors with barplot if desired
#with(sppColors, barplot(rep(1, nrow(sppColors)), col= color, names.arg = taxonID, las=2))

# Subset plots to NEON site of interest, sitei
plotsi <- plotsxy[which(plotsxy$siteID == sitei & plotsxy$subtype == "basePlot"),]
polysi <- plots_poly[which(plots_poly$siteID == sitei & plots_poly$subtype == "basePlot"),]
plotsi_df <- plotsi@data

years <- as.numeric(substr(eventsi, nchar(eventsi)-3, nchar(eventsi)))
print(years)

# Subset veg data to site, change taxonID to factor for this step as 
# it is unique (8-char sppcode is not always unique)
# Moved 2022-03-24, convert categories to factors so levels will be the same across a site, rather than within plots.
# Filter data to remove records with "No longer qualifies" as plantStatus.
vegi0 <- vegi %>% dplyr::filter(!(plantStatus == "No longer qualifies")) %>% 
  mutate(sppFactor = as.factor(taxonID), growthForm = as.factor(growthForm), 
         plantStatus = as.factor(plantStatus))

# Add in a factor level for NAs in tree data, then transform NAs in sppFactor to that label
levels(vegi0$sppFactor) <- c(levels(vegi0$sppFactor), 'NAtaxonID')
vegi0$sppFactor[which(is.na(vegi0$sppFactor))] <- 'NAtaxonID'

# Add a level to capture NAs in plantStatus. Remap plantStatus for small trees.
# Categorize small trees as live or dead for simplified alluvial plot
vegi0$plantStatus2 <- levels(vegi0$plantStatus)[vegi0$plantStatus]
# New 2021-12-30, simplify to single "damaged" category. Ignore (insects, disease or physical)
vegi0$plantStatus2[grepl('damage', vegi0$plantStatus, ignore.case = T)] <- "Live, damaged"
vegi0$plantStatus2[grepl('Live, broken', vegi0$plantStatus, ignore.case = T)] <- "Live, damaged" 
# New 2022-01-03, simplify to single "Dead" category. Ignore (broken bole)
vegi0$plantStatus2[grepl('Dead', vegi0$plantStatus, ignore.case = T)] <- "Dead"  
vegi0$plantStatus2[which(vegi0$stemDiameter < 10 & 
                            grepl('Live', vegi0$plantStatus, ignore.case=T) &
                            !(grepl('damaged', vegi0$plantStatus, ignore.case=T)))] <- "Live, _small"
vegi0$plantStatus2[which(vegi0$stemDiameter < 10 & 
                            grepl('Live', vegi0$plantStatus, ignore.case=T) &
                            (grepl('damaged', vegi0$plantStatus, ignore.case=T) |
                     grepl('other damage', vegi0$plantStatus, ignore.case=T) |
                       grepl('Live, broken', vegi0$plantStatus, ignore.case=T)))] <- "Live, damaged, _small"
vegi0$plantStatus2[which(vegi0$stemDiameter < 10 & 
                            grepl('Dead', vegi0$plantStatus, ignore.case=T))] <- "Dead, _small"

# New 2022-04-12 Catch instances when a dead status is followed by live and change to the live status.
# On Review - this is not worth it, as some stems may be marked as dead for 3 years followed by
# one live status, in general, just accept the uncertainty around these status assignments.
# Calculated that for veg, 692 / 10944 = 0.063 observations of dead trees were followed by a live status the next year. Study sites excluding SOAP & TEAK.
#vegi0 <- vegi0 %>% group_by(individualID) %>% arrange(plotID, individualID, year) %>% 
#  mutate(plantStatus2 = case_when(grepl("Dead", plantStatus2) & 
#         grepl("Live", lead(plantStatus2, n=1, order=year)) ~ lead(plantStatus2, n=1, order=year),
#         TRUE ~ plantStatus2))

levelsPli <- levels(vegi0$plantStatus)
levelsPli2 <- levels(as.factor(vegi0$plantStatus2))
# Reorder levels for use in Alluvial plots
vegi0$plantStatus <- factor(vegi0$plantStatus, 
                      c("No standing record",levelsPli[c(grep("Dead", levelsPli),
                      grep("dead",levelsPli),grep("Live, ", levelsPli, fixed=T))],"Live"))
# Reorder levels for new PlantStatus2 that specifies small Live or Dead trees
vegi0$plantStatus2 <- factor(vegi0$plantStatus2, 
                       c("No standing record",levelsPli2[c(grep("Dead", levelsPli2),
                       grep("dead",levelsPli2))],"Live, _small", "Live, damaged, _small",
                       "Live, damaged","Live"))

#vegi0 %>% dplyr::filter(is.na(plantStatus2)) %>% select(where(~!is.na(.)))
# BART has plantStatus Removed, Downed, Lost, fate unknown, which we ignore...

# Join with sppColors
vegi0 <- vegi0 %>% full_join(sppColors)

################################################################################
# Subset site plots to a specific one
################################################################################
for (p in 1:length(plotsi_remeasured)) {
  ploti_name <- plotsi_remeasured[p]
  print(ploti_name)
  yearsp <- c()
  
  # Skip problem plot - fix later if needed
  if (ploti_name %in%  c("BLAN_055","GUAN_020","HARV_024","OSBS_014")) {
    next()
    }

  veg %>% dplyr::filter(plotID == ploti_name) %>% dplyr::select(subplotID.y, nestedSubplotID) %>% 
    distinct() %>% print()
  
  # Clean up for new run with new plot
  if (exists('lastEventYear')) {
    try(rm(list = c("lastEventYear", "spbaAll", "vegpliAll", grep("spba2",ls(), value=T), 
                "plotMapMade",grep("vegpliYrj", ls(), value=T), grep("live_ba", ls(), value=T),
                grep("dead_ba", ls(), value=T),"sppColorsDeadi","sppColorsi",
                "sppColorsLegendi", grep("spbaA",ls(), value=T),
                grep('plantStatus', ls(), value=T),"littpli")), silent=T)
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
  
  # Subset veg data down to current plot p
  vegpli0 <- vegi0 %>% dplyr::filter(plotID == ploti_name)
  
  # Clean data for any weird records
  if (sitei == "HARV") {
    vegpli0 <- vegpli0 %>% dplyr::filter(stemDiameter < 180 | (is.na(stemDiameter)))
  }
  
  
  # Check why you may have veg observations with no stemDiameter (should generally be saplings, 
  # but could be "Downed" or "burned"). How should we treat these two categories?
  vegpli0 %>% dplyr::filter(is.na(stemDiameter)) %>% dplyr::select(growthForm, plantStatus) %>% 
    table(useNA = "ifany")
  
  # # Create a copy of site-level veg structure data to work with 
  # If tree records have no stemDiameter and no plantStatus, delete that record
  vegpli <- vegpli0
  # Maybe skip below step for now. See if we can use Downed, Removed. or burned in imputation
  #vegpli <- vegpli0 %>% dplyr::filter(!(is.na(stemDiameter) & is.na(plantStatus)))
  
  # New, subset cdwi data to plot. Create a list of vstIndividualID that are new dead.
  if (any(cdwi$plotID %in% vegpli$individualID)) {
    cdwpli <- cdwi$vstIndividualID[which(cdwi$plotID %in% vegpli$plotID)]
  }
  # New 2021-08-16, subset litter data to plot, and plot details to plot
  littpli <- litti %>% dplyr::filter(plotID == ploti_name)
  
  # Determine location of individual litter traps using NEON API method
  if (nrow(littpli) > 0) {
    lploti <- lplot[which(lplot$plotID == ploti_name),]
    littpli <- lploti %>% dplyr::select(trapID, trapType, trapSize, targetTaxaPresent) %>%
      inner_join(littpli) %>% 
      mutate(dryMassGm2 = dryMass / trapSize, # Convert drymass to grams/m2
             durationDays = collectDate - setDate, # Calculate number of days trap was set out
             dryMassGm2Day = dryMassGm2/as.numeric(durationDays)) # Calculate the mean daily rate of litter
    
    littpli_sum <- littpli %>% dplyr::group_by(trapID,functionalGroup, year) %>% 
      dplyr::select(trapID, functionalGroup, year, starts_with("dryMass"), starts_with("duration")) %>%
      summarise(across(c(starts_with("dryMass"), starts_with("duration")), ~sum(.x, na.rm=T))) 
    #littpli_sumMo <- littpli %>% dplyr::group_by(trapID,functionalGroup, year, mo) %>% 
    #  summarise(across(starts_with("dryMass"), ~sum(.x, na.rm=T))) 
    ggplot(data = littpli_sum %>% dplyr::filter(durationDays > 300), 
           aes(x = year, y = dryMassGm2Day, group = functionalGroup, colour = functionalGroup)) +
           geom_line(size = 2) + facet_wrap(~trapID)
  
  # Loop through litter plot trapIDs and calculate the correct coordinates using NEON API utility
  for (s in 1:nrow(lploti)) {
    # Try example from https://www.neonscience.org/resources/learning-hub/tutorials/neon-api-usage
    # To query single location information as described in User Manual
    namedLocationi <- lploti$namedLocationSubplot[s]
    req.loc <- GET(paste("http://data.neonscience.org/api/v0/locations/", 
                         namedLocationi, sep=""))
    req.loc.df <- jsonlite::fromJSON(content(req.loc, as="text"))
    elevi <- req.loc.df$data$locationElevation
    utmE <- req.loc.df$data$locationUtmEasting
    utmN <- req.loc.df$data$locationUtmNorthing
    utmZone <- req.loc.df$data$locationUtmZone
    locProperties <- req.loc.df$data$locationProperties
    utmUncert <- req.loc.df$data$coordinateUncertainty
    utmE.plus.offset <- utmE + clip.offset$offsetEasting[which(clip.offset$pointID == lplot$subplotID[s] &
                                                                 clip.offset$clipCellNumber == lplot$clipCellNumber[s])]
    utmN.plus.offset <- utmN + clip.offset$offsetNorthing[which(clip.offset$pointID == lplot$subplotID[s] &
                                                                  clip.offset$clipCellNumber == lplot$clipCellNumber[s])]
    # NOT DONE... FINISH SOON. Maybe move to inside the nested site/plot loop to only run 
    # for plots for this study...
    # Now update and set the values that you have calculated in the lplot table for each row
    # May not work if error$detail == 400, "Location not found."
    if (length(utmE.plus.offset > 0)) {
      lploti$utmEPlusOffset[s] <- utmE.plus.offset
      lploti$utmNPlusOffset[s] <- utmN.plus.offset 
      lploti$utmZone[s] <- utmZone 
      if (length(utmUncert) > 0) {
        lploti$utmUncert[s] <- utmUncert 
      }
    }
    rm(namedLocationi, req.loc, req.loc.df, elevi, utmE, utmN, utmZone, locProperties,
       utmUncert, utmE.plus.offset, utmN.plus.offset)
  }
  
  if (exists("lplotLoc")) {
    lplotLoc <- lplotLoc %>% bind_rows(lploti)
  } else {lplotLoc <- lploti}
}

  ##############################################################################
  # Calculate frequency of plantStatus observed for ploti across all years
  plantStatusTab0 <- vegpli %>% dplyr::select(plantStatus) %>% group_by(plantStatus) %>% count() %>%
                    mutate(freq_all = n) %>% dplyr::select(plantStatus, freq_all) %>% data.frame()
  plantStatusTab0$plantStatusLevels <- levels(plantStatusTab0$plantStatus)[plantStatusTab0$plantStatus]
  plantStatusTab0 <- plantStatusTab0 %>% inner_join(plant_status_vis, by = c("plantStatus" = "plant_status"))

## PlantStatus2 Code was here. Try moving outside of plot loop.

  # First, have to take care of any duplicate measurements of individuals in the same eventID. 
  # Documentation suggests using the
  # Most recent record, but there is not field indicating how recent a record is 
  # (only differ by uid, which is an alpha-numeric code, not time)
  # Not sure how to handle multistem, small trees...
  # Also create some variables, mean DBH by individual over time and deviation,
  # 2022-04-22 add same approach for height data.
  print(paste("nrows vegpli before filter duplicate records = ", nrow(vegpli)))
  vegpli <- vegpli %>% group_by(individualID) %>% mutate(meanDBH = mean(stemDiameter), 
    diffMeanDBH = abs(stemDiameter - meanDBH),
    meanHt = mean(height),
    diffMeanHt = abs(height - meanHt)) %>% 
    arrange(individualID, eventID.x)
  # Find and filter out duplicates of individualID and eventID.x.
  # Update to release 2021-06 - var tempStemID should distinguish multistem. 
  # But 1st plot at GRSM still showing duplicates and NA for tempStemID.
  vegpli <- vegpli %>% group_by(individualID, eventID.x,tempStemID) %>%  
    dplyr::arrange(individualID, eventID.x, diffMeanDBH) %>%
    mutate(dup_DBH = duplicated(individualID, eventID.x)) %>% 
    dplyr::filter(!(dup_DBH))
  print(paste("nrows vegpli after filter duplicate records = ", nrow(vegpli)))
  
  # Need a method to compare DBH measurements by individual tree and edit errors
  # Add tempStemID to this when it seems populated correctly. NA in 1st plot GRSM, also ABBY_063
  # 2022-04-23 Add variables to deal with height values with high deviation, but only for unbroken stems.
  vegpli <- vegpli %>% group_by(individualID) %>% 
            mutate(stemDiameter.med = median(stemDiameter, na.rm = T),
            stemDBHdev = stemDiameter - stemDiameter.med,
            height.med = median(height, na.rm=T),
            heightDev = height - height.med,
            eventYear = as.numeric(substr(eventID.x, nchar(eventID.x)- 3, nchar(eventID.x)))) %>%
            arrange(individualID, eventYear)
  
  # Vizualize DBH measurements with high temporaldeviation, histogram
  with(vegpli, hist(stemDBHdev, nclass=30,col=1, main = paste(ploti_name, "stemDBHdev")))
  with(vegpli, hist(heightDev, nclass=30,col=1, main = paste(ploti_name, "stemDBHdev")))
  
  # List individuals with a high absolute deviation from median DBH
  vegpli %>% dplyr::filter(abs(stemDBHdev) > 5) %>% dplyr::select(individualID) %>%
    distinct() %>% inner_join(vegpli, by = c("individualID" = "individualID")) %>%
                  dplyr::select(individualID,eventYear, sppFactor, height, 
                  growthForm, contains('stem')) %>% data.frame()
  # Create a new stemDBH variable and set outlier stemDiameter values to NA. Outlier if > 5cm deviation from median.
  vegpli <- vegpli %>% mutate(stemDBH = ifelse(abs(stemDBHdev) > 5, NA, stemDiameter))
  
  # Replace outlier stemDiameter with NA, then impute a diameter from other meas.
  # First pass will replace outliers marked as NA in stemDBH with average of previous
  # and following year DBH. Second pass will replace remaining NA's with the closest
  # value (either lagging or following value). Cases where there are no lagging or
  # following DBH return Inf and are recast to NA.
  # Wrapped in if-else 2022-03-24 to allow single year measurement summary
  if (min_times_remeasured >= 1) {
    vegpli <- vegpli %>% group_by(individualID) %>% arrange(individualID, eventYear) %>% 
      mutate(lagStemDBH = lag(stemDBH, 1), leadStemDBH = lead(stemDBH, 1)) %>% 
      ungroup() %>% mutate(imputeStemDBH = stemDBH) %>% group_by(uid.x) %>%
      mutate(imputeStemDBH = ifelse(is.na(imputeStemDBH), (lagStemDBH + leadStemDBH)/ 2 , stemDBH)) %>% 
      mutate(imputeStemDBH2 = replace_na(imputeStemDBH, max(lagStemDBH, leadStemDBH, na.rm=T))) %>%
      mutate(imputeStemDBH2 = na_if(imputeStemDBH2, "Inf" ), imputeStemDBH2 = na_if(imputeStemDBH2, "-Inf"))
  } else {
    vegpli <- vegpli %>% group_by(individualID) %>% arrange(individualID, eventYear) %>% 
      mutate(lagStemDBH = NA, leadStemDBH = NA) %>% 
      ungroup() %>% mutate(imputeStemDBH = stemDBH) %>% group_by(uid.x) %>%
      mutate(imputeStemDBH2 = imputeStemDBH)
  }
  
  # New 2021-06-25, some DBH errors look like transcriptions errors of the decimal.
  # Try seeing if dividing by 10 or 100 gets to a correct measure (see UNDE_044 example)
  # New 2021-07-02, some errors look like numbers were swapped around decimal (see GRSM_051 examp)
  vegpli <- vegpli %>% group_by(individualID) %>% arrange(individualID, eventYear) %>% 
    mutate(imputeStemDBHDiv10 = ifelse(abs(stemDBHdev) > 5, stemDiameter/10 , NA),
           imputeStemDBHDiv100 = ifelse(abs(stemDBHdev) > 5, stemDiameter/100 , NA))

  # Check for cases where a DBH could not be imputed
  vegpli %>% dplyr::filter(is.na(imputeStemDBH2)) %>% dplyr::select(individualID) %>%
         distinct() %>% inner_join(vegpli, by = c("individualID" = "individualID")) %>%
         dplyr::select(individualID,eventYear, sppFactor, height, 
         growthForm, plantStatus, contains('stem')) %>% data.frame()
  with(vegpli, hist(imputeStemDBH2 - stemDiameter.med, nclass=30,col=1))
  
  # Correct specific transcriptional errors
  vegpli <- vegpli %>% mutate(imputeStemDBH2 = case_when(uid.x == "0bf2e6a2-c0d4-422d-8f82-c15abec39fa8" ~ 8.0,
                              uid.x == "291b0301-3874-40a9-b44d-cb0c14effffe" ~ stemDiameter.med,
                              TRUE ~ imputeStemDBH2))
  with(vegpli, hist(imputeStemDBH2 - stemDiameter.med, nclass=30,col=1))
  
  # Need to check here for rows where plotArea was not recorded. Impute from other years if possible
  vegTally <- vegpli %>% group_by(plotID, year) %>% add_tally() %>%
    summarize_at(vars(totalSampledAreaTrees, n), funs(mean))
  totalSampledAreaTreesi <- vegTally %>% ungroup() %>% 
    dplyr::select(totalSampledAreaTrees) %>% table() %>% sort(., decreasing=T)
  totalSampledAreaTreesi <- totalSampledAreaTreesi[1] %>% names() %>% as.numeric() %>% as.integer()
  
  
  # Store original totalSampledAreaTrees in new variable and impute likely area 
  # in original column. If totalSampledAreaTrees is NA, replace with the most commonly sampled area
  vegpli <- vegpli %>% mutate(totalSampledAreaTreesOrig = totalSampledAreaTrees,
             totalSampledAreaTrees = case_when(is.na(totalSampledAreaTrees) ~ totalSampledAreaTreesi,
             TRUE ~ totalSampledAreaTrees))
  
  # Convert DBH to basalArea to a per area basis for trees
  vegpli <- vegpli %>% dplyr::select((order(colnames(vegpli)))) %>% 
    mutate(basalAreaM2 = pi*(imputeStemDBH2/100/2)^2) 
  
  # Add the totalSampledAreaTrees from vplot table to calculate BA on area basis
  # New, added nestedSubplotAreaShrubSapling as well for small trees - Now ignore. 
  # None of small trees use this.
  # NEW 2022-03-29 Combine former lines of code into one using case_when construction...
  # Reminder, small trees are sampled over same area as totalSampledAreaTrees. Redo all after 2022-04-01
  vegpli <- vegpli %>% mutate(basalAreaM2ha =  case_when(!(growthForm == 'small tree') ~ 
                        basalAreaM2 * 10000/totalSampledAreaTrees,
                        growthForm == 'small tree' ~ basalAreaM2 * 10000/totalSampledAreaTrees),
                        n = 1, tph = case_when(!(growthForm == 'small tree') ~ 
                        n * 10000/totalSampledAreaTrees,
                        growthForm == 'small tree' ~ n * 10000/totalSampledAreaTrees))
  # Update basalAreaM2ha for correct area sampled for small tree data
  # What the fudge, "noneSelected" in the nestedSubplotAreaShrubSapling var makes no sense. Brutal.

  # New 2022-03-27 Try to screen event years for incomplete sampling (eg2015 in many plots at OSBS)
  # Hardcode for now.
  print(vegpli %>% ungroup() %>% dplyr::select(year) %>% table())
  
  if (ploti_name %in% c("OSBS_026","OSBS_027","OSBS_028","OSBS_031","OSBS_032","OSBS_042")) {
    vegpli <- vegpli %>% dplyr::filter(year > 2015)
  }
 
  # Use pivot_wider and grouping to spread the data frame out to plantStatus by measurement year
  # Encountered problem grouping by individualID once adding in small stems. Unclear if there can
  # be multistemmed small individuals with same individualID. Try grouping by uid.x instead.
  # Switch back. For now, select first of small trees with same individualID. Impact should be small.
  # TO DO: Add in tempStemID variable to distinguish small multi-stemmed trees. 
  # Test plot has this as NA, so can't implement yet.
  stemDBHtab <- vegpli %>% group_by(individualID, eventID.x) %>% 
    dplyr::select(individualID, eventID.x, imputeStemDBH2) %>% 
    slice(1) %>% arrange(eventID.x, individualID) %>% 
    pivot_wider(., names_from = eventID.x, values_from = imputeStemDBH2) %>% 
    dplyr::arrange(ncol(.)-2, ncol(.)-1) %>% as.data.frame()
  
  # Repeat for new plantStatus2 that addes in small trees as there own grouping
  # TO DO: Combine two tables into one, simplify. Seems plantStatus2b could work for both...
  # library(arsenal)
  # comparedf(plantStatus2, plantStatus2b)
  plantStatus2 <- vegpli %>% group_by(individualID, eventID.x) %>% slice(1) %>% 
    group_by(individualID) %>% mutate(maxBasalAreaM2ha = max(basalAreaM2ha, na.rm=T)) %>% 
    group_by(eventID.x, plantStatus2) %>% 
    dplyr::select(individualID, eventID.x, plantStatus2, maxBasalAreaM2ha) %>% 
    unique() %>% arrange(eventID.x, individualID) %>% 
    pivot_wider(., names_from = eventID.x, values_from = plantStatus2) %>%
    arrange(individualID)
  
  # Try summarizing basal area, tph, n by species, year, and plantStatus2 for this plot here...
  # This should replace all the tables created below for Live, live small, dead, dead small above...2022-04-01
  spbai <- vegpli %>% group_by(individualID, year) %>% 
    dplyr::select(plotID, individualID, year, plantStatus2, sppFactor, 
                  basalAreaM2ha, tph, n) %>% slice(1) %>% 
    dplyr::group_by(plotID,sppFactor, year, plantStatus2) %>% 
    summarize_at(vars(basalAreaM2ha, tph, n), list(sum = sum), na.rm=T)
  
  # Spread each summary table out by species
  spbaiWide <- spbai %>% dplyr::select(-tph_sum, -n_sum) %>% spread(., key = sppFactor, value = basalAreaM2ha_sum, fill = 0)
  tphiWide <- spbai %>% dplyr::select(-basalAreaM2ha_sum, -n_sum) %>% spread(., key = sppFactor, value = tph_sum, fill = 0)
  niWide <- spbai %>% dplyr::select(-basalAreaM2ha_sum, -tph_sum) %>%spread(., key = sppFactor, value = n_sum, fill = 0)
  
  ## Set up multi-page PDF for output figures by plot ##########################
  if (makepdf == "yes") {
  pdf(file = paste("..//figures//", ploti_name, "_tree_composition_change.pdf",sep=""), 
      onefile = T, width = 6, height = 4)
  }
  ##############################################################################
  ## Loop through each measurement year in eventsi, summarize and plot tree data.
  ##############################################################################
  for (j in 1:length(years)) {
      yearj <- years[j]
    
    # Skip problem plot - fix later
    if ((ploti_name == "HARV_043" & yearj == 2015) |
        (ploti_name == "OSBS_030" & yearj == 2019) |
        (ploti_name == "GUAN_020" & yearj == 2021) |
        (ploti_name == "OSBS_014" & yearj == 2020))
        {next()}
      
    # Check if there was a measurement event for this plot in yearj. If not move on to next year.
    if (nrow(vegpli %>% dplyr::filter(eventID.x == eventsi[grep(yearj, eventsi)])) > 0 ) {
      
      # Subset plot-level data for current year  
      vegpliYrj <- vegpli %>% dplyr::filter(eventID.x == eventsi[grep(yearj, eventsi)])
   
      # Look at observation frequency by growthForm
      table(vegpliYrj$growthForm)
      
      # Extract the area sampled for trees for year j (should be 800 or 400)
      totalSampledAreaTreesij <- plotAreaYear %>% dplyr::filter(plotID == ploti_name & 
             year == yearj) %>% dplyr::select(totalSampledAreaTrees)
      totalSampledAreaShrubSaplingij <- plotAreaYear %>% dplyr::filter(plotID == ploti_name & 
             year == yearj) %>% dplyr::select(totalSampledAreaShrubSapling)
        
      #totalSampledAreaTreesij <- vplotT %>% dplyr::filter(plotID == ploti_name & eventID == eventsi[j]) %>%
      #  dplyr::select(totalSampledAreaTrees) %>% unname() %>% unlist() %>% unique()
      # Extract the area sampled for small trees, shrubs, saplings year j
      #totalSampledAreaShrubSaplingij <- vplotT %>% dplyr::filter(plotID == ploti_name & eventID == eventsi[j]) %>%
      #  dplyr::select(totalSampledAreaShrubSapling) %>% unname() %>% unlist() %>% 
      #  unique() %>% as.numeric()
      
      # New 2022-01-03. Catch case where vplot table shows more than one area. Take most recent.
      # Should be doing this above outside of loop...
      #if (length(totalSampledAreaShrubSaplingij) > 1) {
      #  totalSampledAreaShrubSaplingij <- vplotT %>% dplyr::filter(plotID == ploti_name & eventID == eventsi[j]) %>% 
      #    arrange(desc(publicationDate)) %>% slice(1) %>%
      #    dplyr::select(totalSampledAreaShrubSapling) %>% unname() %>% unlist() %>% 
      #    unique() %>% as.numeric()
      #}

      # Check for cases where there is tree data but no record of the area sampled
      # This should all be handled above outside of by-year loop, but leaving in for now...
      if (nrow(vegpliYrj) > 0 & is.na(totalSampledAreaTreesij)) {
        # New 2021-12-30 Use plot size from any year if this event did not record area...
        # Revised 2022-04-01 after checking NAs in totalSampledArea fields for tree growthForm
        # Now skip any plot/year combo with NA in totalSampledArea field...
        #totalSampledAreaTreesij <- vplotT %>% dplyr::filter(plotID == ploti_name) %>% 
        #  dplyr::select(totalSampledAreaTrees) %>% unname() %>% unlist() %>% unique()
        #plotAreaYear %>% dplyr::filter(plotID == ploti_name) %>% dplyr::select(totalSampledAreaTrees)
        #if (length(totalSampledAreaTreesij) == 0) {
        #  print("There is a problem with event record in vplot dataset, using 800 m  plot size as default")
        #  totalSampledAreaTreesij <- 800 # REVISIT this approach...
        #} else {print("There is a problem with event record in vplot dataset, using plot size from other event")}
        # Advance to next year. Require a valid record for tree sampling in vplot table.
        if (is.na(totalSampledAreaTreesi)) {
          next()
        } else {
          totalSampledAreaTreesij <- totalSampledAreaTreesi
        }
      }
      print(paste("totalSampledAreaTrees", yearj, "=", totalSampledAreaTreesij, "m2/ha"))
      # Note, not using totalSampledAreaShrubSapling yet!...
      print(paste("totalSampledAreaShrubSapling", yearj, "=", totalSampledAreaShrubSaplingij, "m2/ha"))
        
      # Weed out duplicate observations by individual if any occur. 
      # First calculate how many distinct individualIDs Use the most recent date.
      n_individuals <- vegpliYrj %>% group_by(individualID) %>% arrange(individualID) %>% 
        dplyr::select(individualID) %>% 
        n_distinct(., na.rm=T)#slice(which.max(mydates))
      n_individuals
      
      # Check if there are duplicate records for individual trees. 
      #But, multibole stems?? Ignoring small tree problem for now.
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
      print(paste(ploti_name, '# individuals = ',n_individuals, yearj)) 
      # Check if there are duplicate records - Want 0 rows duplicated
      vegpliYrj %>% group_by(individualID) %>% filter(n() > 1) %>% as.data.frame()
      
      #Look at growth form and plant status
      vegpliYrj %>% dplyr::select(growthForm) %>% table(useNA = "ifany")
      vegpliYrj %>% dplyr::select(plantStatus) %>% table(useNA = "ifany")
      vegpliYrj %>% dplyr::select(remarks.x) %>% table(useNA = "ifany")
      
      # Subset data to Live over story stems >= 10 cm DBH
      # Change 2021-07-13 - Trusting NEON labeling as growthForm for plot types.
      # Should use spbai above and delete all of this 2022-04-01
       # New 2021-04-13: Subset to Live Small trees DBH < 10 cm DBH
      # New 2021-06-26: Subset to Dead Small trees DBH < 10 cm DBH and correct BAm2ha
  
      # New 2021-07-02: Subset Dead Overstory trees >= 10 cm DBH

      # Found case where "growthForm" was left NA in first year. In this case, use stemDiameter >= 10 to filter to overstory trees.
 
       # Look at live & dead trees per species together
      vegpliYrj %>% ungroup() %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      
      # Look at live trees per species
      #vegpliYrjL %>% ungroup() %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      #vegpliYrjSmallL  %>% ungroup() %>% dplyr::select(sppFactor) %>% table(useNA = "ifany")
      niWide %>% dplyr::filter(year == yearj & plantStatus2 %in% c("Live", "Live, damaged")) %>% 
        ungroup() %>% dplyr::select(-plotID, -year, -plantStatus2) #%>% colSums()
      niWide %>% dplyr::filter(year == yearj & plantStatus2 %in% c("Live, _small")) %>% 
        ungroup() %>% dplyr::select(-plotID, -year, -plantStatus2)
      
      # Subset cleaned tree-level veg structure data from plot for export
      vegpli_out <- vegpli %>% dplyr::select(uid, plotID, year, subplotID.y, 
                    individualID, plantStatus, plantStatus2, taxonID, 
                    scientificName, sppFactor, spp, totalSampledAreaTrees,
                    totalSampledAreaTreesOrig, growthForm, 
                    stemDiameter, stemDBHdev, stemDiameter.med, lagStemDBH,
                    imputeStemDBH2, basalAreaM2, basalAreaM2ha, n, tph, height, remarks.x)
      # Write cleaned up plot_by_year_tables to a named dataframe
      assign(paste("spbai_",ploti_name,sep=""), spbai)
      assign(paste("vegpli_",ploti_name, sep=""), vegpli_out)
              
    #################### Section to Plot Subplots Sampled in Eventi yearj for Overstory trees DBH > 10 cm ########
    ##############################################################################################################
      ## Only create this plot one time in loop for now. Use a counter
      if (!(exists("plotMapMade"))) {
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
        
      # mask subplot raster to only show sampled subplots
      plotIDs.tab <- data.frame(cbind(as.character(subplotIDs), as.character(nsubplotIDs)))
      names(plotIDs.tab) <- c("subplotIDs","nsubplotIDs")
      # Determine distinct combo of subplot and nestedsubplot IDs observed in the plot data
      nsubplots.sampled <- vegpliYrj %>% ungroup() %>% dplyr::select(subplotID.y, nestedSubplotID) %>% distinct()
      names(nsubplots.sampled) <- names(plotIDs.tab)
      print(nsubplots.sampled)
      
      # Capture cases where early NEON plot design is not consistent with current
      if (!(any(nsubplots.sampled$subplotIDs %in% subplotIDs))){
        print('subplot #s sampled not in current NEON design')
        next()
      }
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
      
      # Create a new polygon shape for individual subplots that are sampled
      subplots0.r.out <- subplots0.r
      subplotVals <- as.numeric(unique(plotIDs.tab$subplotIDs))
      # Remove NAs
      subplotVals <- subplotVals[-which(is.na(subplotVals))]
      subplots0.r.out[which(!(subplots0.r.out[] %in% subplotVals))] <- NA
      subplots.r.poly <- rasterToPolygons(subplots0.r.out, fun = function(x) { x > 0}, dissolve = T)
      names(subplots.r.poly) <- "subPlotID"
      subplots.r.poly <- spTransform(subplots.r.poly, crs(polysi))      
      subplots.r.poly@data <- cbind(subplots.r.poly@data, ploti@data)
      # Reproject it back to general Long-Lat projection for joint storage

      if (exists("subplotsAll.poly")) {
          subplotsAll.poly <- raster::bind(subplotsAll.poly, subplots.r.poly)
      } else {subplotsAll.poly <- subplots.r.poly}
      
      # Now add mapped stems if available
      if (nrow(datai) > 0) {
        # first join with plant statys vis variables from vegpli
        datai <- vegpli %>% dplyr::select(individualID, eventID.x, contains("plant_status")) %>% 
          right_join(datai)
        with(datai, points(adjEasting, adjNorthing, cex = stemDiameter/20, pch = plant_status_symbol,
                           bg = plant_status_bg, xlab = "Easting", ylab = "Northing"))  

        with(plantStatusTab0, legend(legendPos, plantStatusLevels, pt.bg = plant_status_bg, pch = plant_status_symbol,
                                    cex = 0.8, bg = rgb(1, 1, 1, 0.8)))
      }
      
      # Now add litter Traps if available
      if (exists('lploti')) {
        if (nrow(lploti) > 0) {
          with(lploti, points(utmEPlusOffset, utmNPlusOffset, pch=3, cex = 1, col = "blue"))
        }
      }
        plotMapMade = "yes"
      }
      #################### END Section to Plot Subplots Sampled in Eventi yearj for Overstory trees DBH > 10 cm ####
      ##############################################################################################################  
      
      # Compute basal area by species for live and dead trees

      # Rewrite 2022-04-01
      totalsLi <-  spbai %>% dplyr::filter(grepl("Live", plantStatus2) & year == yearj) %>% 
        ungroup() %>% summarize_at(vars(grep("sum",names(spbai), value=T)), funs(sum))
      totalsDi <- spbai %>% dplyr::filter(grepl("Dead", plantStatus2) & year == yearj) %>% 
        ungroup() %>% summarize_at(vars(grep("sum",names(spbai), value=T)), funs(sum))
      
      # Compute basal area by species for dead trees - large and small 
      # Check and rewrite to separate large & small
      # New 2021-06-26 Add plantStatus "Downed" to dead wood pool
      # Start with overstory tree dead wood (DBH or largest DBH of multibole stem > 10 cm)
  
      # New Compute basal area by species for small trees
 
      # New Compute basal area by species for small Dead trees
      # rename vars for a merge spba dataframe
      
      ##########################################################################
      # Create a barplot of basal area by species for current measurement year
      rangeBA <- c(-0.5, 1) * max(spbai$basalAreaM2ha_sum)
      sppColorsi <- sppColors %>% dplyr::filter(taxonID %in% spbai$sppFactor)
      sppColorsi$sppFactor <- sppColorsi$taxonID
      
      #########################################################################################################
      # Try barplot of overstory and small trees side by side
       # Or
      spbaStatusCol <- data.frame(plantStatus2 = c("Live, _small","Live","Live, damaged"),
                                  color = c("lightgrey","darkseagreen","gold"))
      # Rewrite 2022-04-01 - Proceed and delete as much above as unneeded
      spbaYeariWide <- spbaiWide %>% dplyr::filter(year == yearj & grepl("Live", plantStatus2))
      
      spbaYeariWide <- spbaYeariWide %>% ungroup() %>% dplyr::select(-plotID, -year)
      
      spbaYeariWide <- spbaStatusCol %>% inner_join(spbaYeariWide)
      
      if (nrow(spbaYeariWide) > 0) {
      sppColorsi <- data.frame(sppFactor = names(spbaYeariWide)) %>%  
        inner_join(sppColors, by = c("sppFactor" = "taxonID"))
      # Write figure to output, PNG
      #png(filename=paste("..//figures//FIA_",this_state,"_Mean_Change_AGB-C_Understory_DBHlt5in_Ecotone_Mtn_Group_",mtni,".png",sep=""),width=6.22,height=4.61,
      #    units="in",pointsize=14,res=300)
      baYear1BarPlot <-   barplot(as.matrix(spbaYeariWide[,3:ncol(spbaYeariWide)]), beside=T,
                          las = 2, col = spbaYeariWide$color,
                          space = c(0,0.1), axisnames = FALSE, ylab = "Live BA m2/ha", ylim = rangeBA)
      abline(h=0)
      legend("right", legend = c("Overstory","Overstory, damaged","Small"), 
             fill =c("darkseagreen","gold" ,"lightgrey"), box.lty=0)
      # Now use mtext() for the axis labels
      text(baYear1BarPlot[1,], rep(rangeBA[1] * 0.1, ncol(spbaYeariWide)-2),
           names(spbaYeariWide)[-(1:2)], srt = 90, cex = 1.0)
      mtext(paste(ploti_name, yearj))
      }
      #########################################################################################################
      
      # If yearj Measurement year is not the first, calculate a change over time
      # Need to rewrite, calculate from new spbai table
      #if (exists('lastEventYear')) {
      #  yearsSinceMeasurement <- yearj - lastEventYear
      #  spbaLastEventYear <- get(paste("spba", lastEventYear, sep=""))
      #  liveBALastEventYear <- sum(spbaLastEventYear$ba)
        # Deleted
       }
      
      lastEventYear <- yearj
      yearsp <- c(yearsp, yearj)
    } 

  #}


# Try to create Alluvial plot of plantStatus Transitions
  
# First, calculate totals density of Live and Dead Stems per year by Species

# Also calculate totals by BA for later plotting

# Join live and dead tables to include all observed species

# Make alluvial plots and time series of live and dead BA, only if remeasured data found.

# Find any records that have no DBH or Basal area recorded and remove from plantStatus
noDBH <- which(is.infinite(plantStatus2$maxBasalAreaM2ha))
plantStatus2 <- plantStatus2 %>% dplyr::filter(!(is.infinite(plantStatus2$maxBasalAreaM2ha)))

print(paste("Number of trees with no DBH = ", length(noDBH)))
# Now Count the frequency of each combination of plantStatus or plantStatus2 transitions across measurement events
listEvents <- sort(unique(vegpli$eventID.x))

plantStatus4 <- plantStatus2 %>% dplyr::select(-individualID) %>% 
  group_by_at(vars(one_of(listEvents))) %>% count()
plantStatus4 <- plantStatus4 %>% replace(is.na(.), "No standing record")
plantStatus4 <- plantStatus4 %>% dplyr::arrange(vars(listEvents)) %>% as.data.frame()
# Filter for now, Option to remove rows for observations that were measured only in first year, not others
# May represent changes in protocol
# Alternative, summarize based on sum of max basal area for each plant status sequence grouping
plantStatusBa <- plantStatus2 %>% dplyr::select(-individualID) %>% mutate(Count = 1) %>% 
  group_by_at(vars(one_of(listEvents))) %>% summarize_at(vars(maxBasalAreaM2ha,Count), sum, na.rm=T )
plantStatusBa <- plantStatusBa %>% replace(is.na(.), "No standing record")
plantStatusBa <- plantStatusBa %>% dplyr::arrange(vars(listEvents)) %>% mutate(n = round(maxBasalAreaM2ha)) %>%  as.data.frame()


# Create a color palette for plantStatus
statusColors <- rep("grey", nrow(plantStatus4))
statusColors[grep("Live", plantStatus4[,1])] <- rgb(0.9,0.9,0,0.5)
statusColors[grep("Live", plantStatus4[,ncol(plantStatus4)-1])] <- "orange"
liveAfterYear1_rows <- c()
for (k in 2:(ncol(plantStatus4)-1)) {
  liveAfterYear1_rows <- c(liveAfterYear1_rows, grep("Live", plantStatus4[,k]))
}

liveAfterYear1_rows <- sort(unique(liveAfterYear1_rows))
statusColors[liveAfterYear1_rows[which(liveAfterYear1_rows %in% grep("No standing record", plantStatus4[,1]))]] <- "green"

events_labels <- names(plantStatusBa)[which(names(plantStatusBa) %in% listEvents)]
events_labels <- substr(events_labels, nchar(events_labels) - 3, nchar(events_labels)) # Doesn't work without above line if event is excluded because no trees were measured


# Plot alluvial plot using frequency of stems in each observed sequence of plantStatus
# Only make these alluvial plots for plots with re-measured data, e.g. multiple years
if (min_times_remeasured > 0) {
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
}

# Plot Total numbers of Live and Dead Stems per year by Species
liveDamaged_n <-  niWide %>% dplyr::filter(grepl("damaged", plantStatus2)) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

liveUndamaged_n <- niWide %>% dplyr::filter(grepl("Live", plantStatus2) & 
                                              !(grepl("damaged", plantStatus2))) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

dead_n <- niWide %>% dplyr::filter(grepl("Dead", plantStatus2)) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

# Catch when a category does not occur to allow plots below to work
if (nrow(dead_n) == 0) {
  dead_n <- liveUndamaged_n
  dead_n[,2:ncol(dead_n)] <- 0
}

if (nrow(liveUndamaged_n) == 0) {
  liveUndamaged_n <- liveDamaged_n
  liveUndamaged_n[,2:ncol(liveUndamaged_n)] <- 0
}

if (nrow(liveDamaged_n) == 0) {
  liveDamaged_n <- liveUndamaged_n
  liveDamaged_n[,2:ncol(liveDamaged_n)] <- 0
}

# Update colors to include live and dead species
sppColorsi <- sppColorsi %>% rbind(c("NAtaxonID",NA,"thistle",NA))
sppColorsLegendi <- sppColorsi$sppFactor

# Set margins with par
par(mar = c(5.1, 4.1, 2.1, 2.1))
#

# First plot number of trees per species
plot(c(2015,2021), c(0, max(liveUndamaged_n[,-1], liveDamaged_n[,-1], dead_n[,-1])),
     type = "n", xlab = "year", ylab = "n stems", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()

  for (m in 2:ncol(liveDamaged_n)) {
   lines(liveUndamaged_n$year, unlist(liveUndamaged_n[,m]), col = sppColorsi$color[m-1], lwd = 1.5)
   points(liveUndamaged_n$year, unlist(liveUndamaged_n[,m]), pch = 21, bg = sppColorsi$color[m-1], cex = 1.5)
   # Plot dead stems as well
   lines(dead_n$year, unlist(dead_n[,m]), col = sppColorsi$color[m-1], lwd = 1.5, lty = 2)
   points(dead_n$year, unlist(dead_n[,m]), pch = 23, col = "grey", bg = sppColorsi$color[m-1], cex = 1.5, lty =2)
   # Plot small live trees as well
   lines(liveDamaged_n$year, unlist(liveDamaged_n[,m]), col = sppColorsi$color[m-1],lty=3,  lwd = 1.5)
   points(liveDamaged_n$year, unlist(liveDamaged_n[,m]), pch = "+", col = sppColorsi$color[m-1], cex = 1.5)
   points(liveDamaged_n$year, unlist(liveDamaged_n[,m]), pch = 21, col = sppColorsi$color[m-1], cex = 1.5)
   }
 
mtext(paste("N Stems","   ",ploti_name), line = 0)
legend("topright", sppColorsi$sppFactor, bty = "n", 
       pt.bg = sppColorsi$color, pch = 21, pt.cex = 1, cex = 0.5)
legend("topleft", c("live","live_damaged","dead"), bty="n", pch = c(21,3,23), lty=c(1,3,2), 
       lwd=c(1.5,1.5,1.5), pt.cex = 1.5, 
       col = c(1,"darkgrey"))

# Plot BA Change of Live and Dead Stems per year by Species
# Calculate annual summaries by status class
liveDamaged_ba <-  spbaiWide %>% dplyr::filter(grepl("damaged", plantStatus2)) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

liveUndamaged_ba <- spbaiWide %>% dplyr::filter(grepl("Live", plantStatus2) & 
                                              !(grepl("damaged", plantStatus2))) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

dead_ba <- spbaiWide %>% dplyr::filter(grepl("Dead", plantStatus2)) %>% 
  ungroup() %>% dplyr::select(-plotID, -plantStatus2) %>% group_by(year) %>% 
  summarize_at(vars(any_of(sppColorsi$sppFactor)), sum)

# Catch when a category does not occur to allow plots below to work
if (nrow(dead_ba) == 0) {
  dead_ba <- liveUndamaged_ba
  dead_ba[,2:ncol(dead_ba)] <- 0
}
if (nrow(liveUndamaged_ba) == 0) {
  liveUndamaged_ba <- liveDamaged_ba
  liveUndamaged_ba[,2:ncol(liveUndamaged_ba)] <- 0
}
if (nrow(liveDamaged_ba) == 0) {
  liveDamaged_ba <- liveUndamaged_ba
  liveDamaged_ba[,2:ncol(liveDamaged_ba)] <- 0
}

plot(c(2015,2021), c(0, max(liveUndamaged_ba[,-1], liveDamaged_ba[,-1], dead_ba[,-1])),
     type = "n", xlab = "year", 
     ylab = "basal area (m2/ha)", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()
for (m in 2:ncol(liveDamaged_ba)) {
  lines(liveUndamaged_ba$year, unlist(liveUndamaged_ba[,m]), col = sppColorsi$color[m-1], lwd = 1.5)
  points(liveUndamaged_ba$year, unlist(liveUndamaged_ba[,m]), pch = 21, bg = sppColorsi$color[m-1], cex = 1.5)
  # Plot dead stems as well
  lines(dead_ba$year, unlist(dead_ba[,m]), col = sppColorsi$color[m-1], lwd = 1.5, lty = 2)
  points(dead_ba$year, unlist(dead_ba[,m]), pch = 23, col = "grey", bg = sppColorsi$color[m-1], cex = 1.5, lty =2)
  # Plot small live trees as well
  lines(liveDamaged_ba$year, unlist(liveDamaged_ba[,m]), col = sppColorsi$color[m-1],lty=3,  lwd = 1.5)
  points(liveDamaged_ba$year, unlist(liveDamaged_ba[,m]), pch = "+", col = sppColorsi$color[m-1], cex = 1.5)
  points(liveDamaged_ba$year, unlist(liveDamaged_ba[,m]), pch = 21, col = sppColorsi$color[m-1], cex = 1.5)
}

mtext(paste("Basal Area","   ",ploti_name), line = 0)
legend("topright", sppColorsi$sppFactor, bty = "n", pt.bg = sppColorsi$color, 
       pch = 21, pt.cex = 1, cex = 0.5)
legend("topleft", c("live","live_damaged","dead"), bty="n", pch = c(21,3,23), lty=c(1,3,2), 
       lwd=c(1.5,1.5,1.5), pt.cex = 1.5, 
       col = c(1,"darkgrey"))

# Plot live, dead totals
# Calculate annual summaries by status class
liveDamaged_ba_tot <-  spbai %>% dplyr::filter(grepl("damaged", plantStatus2)) %>% 
  group_by(year) %>% summarize_at(vars(basalAreaM2ha_sum, tph_sum, n_sum), sum)

liveUndamaged_ba_tot <- spbai %>% dplyr::filter(grepl("Live", plantStatus2) & 
                                  !(grepl("damaged", plantStatus2))) %>% 
  group_by(year) %>% summarize_at(vars(basalAreaM2ha_sum, tph_sum, n_sum), sum)

dead_ba_tot <-  spbai %>% dplyr::filter(grepl("Dead", plantStatus2) & 
                                          !(grepl("damaged", plantStatus2))) %>% 
  group_by(year) %>% summarize_at(vars(basalAreaM2ha_sum, tph_sum, n_sum), sum)

# Catch when a category does not occur to allow plots below to work
if (nrow(dead_ba_tot) == 0) {
  dead_ba_tot <- liveUndamaged_ba_tot
  dead_ba_tot[,2:ncol(dead_ba_tot)] <- 0
}
if (nrow(liveUndamaged_ba_tot) == 0) {
  liveUndamaged_ba_tot <- liveDamaged_ba_tot
  liveUndamaged_ba_tot[,2:ncol(liveUndamaged_ba)] <- 0
}
if (nrow(liveDamaged_ba_tot) == 0) {
  liveDamaged_ba_tot <- liveUndamaged_ba_tot
  liveDamaged_ba_tot[,2:ncol(liveDamaged_ba_tot)] <- 0
}

plot(range(liveUndamaged_ba_tot$year), c(0, max(max(liveDamaged_ba_tot$basalAreaM2ha_sum),
     max(liveUndamaged_ba_tot$basalAreaM2ha_sum), max(dead_ba_tot$basalAreaM2ha_sum))),type = "n", xlab = "year", 
     ylab = "basal area (m2/ha)", axes = F)
abline(h = 0, lty = 3)
axis(side = 1, at = yearsp)
axis(side = 2)
box()
with(liveUndamaged_ba_tot, lines(year, basalAreaM2ha_sum, lwd = 2))
with(liveUndamaged_ba_tot, points(year, basalAreaM2ha_sum, pch = 21, cex=2, bg = "darkgrey"))
with(liveDamaged_ba_tot,lines(year, basalAreaM2ha_sum, lwd = 2, lty=3))
with(liveDamaged_ba_tot,points(year, basalAreaM2ha_sum, pch = 3, cex=2, bg = "darkgrey"))
with(liveDamaged_ba_tot,points(year, basalAreaM2ha_sum, pch = 1, cex=2, bg = "darkgrey"))
with(dead_ba_tot,lines(year, basalAreaM2ha_sum,  lwd = 2, lty = 2, col = "darkgrey"))
with(dead_ba_tot,points(year, basalAreaM2ha_sum, pch = 23, cex=2, bg = "grey"))
mtext(paste("Basal Area","   ",ploti_name), line = 0)

legend("topleft", c("live","live_damaged","dead"), bty="n", pch = c(21,3,23), lty=c(1,3,2), 
       lwd=c(1.5,1.5,1.5), pt.cex = 1.5, 
       col = c(1,"darkgrey"))

# Plot viz of litter trap data by year
if (exists("littpli_sum")) {
  try(print(ggplot(data = littpli_sum %>% dplyr::filter(durationDays > 300), 
         aes(x = year, y = dryMassGm2Day, group = functionalGroup, colour = functionalGroup)) +
    geom_line(size = 2) + facet_wrap(~trapID)), silent = T)
}

# Plot stemDBH vs height for this plot by species
test <- vegpli_out %>% group_by(sppFactor, individualID) %>% 
  summarize_at(vars(imputeStemDBH2, height), median, na.rm=T)
test <- test %>% inner_join(sppColorsi)

with(test, plot(c(min(imputeStemDBH2, na.rm=T),max(imputeStemDBH2, na.rm=T)), 
                c(0,max(height, na.rm=T)), type="n",
                xlab = "stem DBH (cm)", 
                ylab = "height (m)"))
     abline(h = 0, lty = 3)
     with(test, text(imputeStemDBH2, height, label = sppFactor, col=color, cex=0.7))

# Reset margins with par
par(mar = c(5.1, 4.1, 4.1, 2.1))

if (makepdf == "yes") {
dev.off()
  }

# Reformat plot-level summaries and merge into site level dataframes
ba_tot_longi <- liveUndamaged_ba_tot %>% 
  full_join(liveDamaged_ba_tot, by = c("year" = "year"), suffix=c(".liv",".dam")) %>%
  full_join(dead_ba_tot, by = c("year" = "year"), suffix=c(".dead","")) %>%
  mutate(plot = ploti_name)

if (p == 1) {
  ba_tot_sitei <- ba_tot_longi
} else {
  ba_tot_sitei <- rbind(ba_tot_sitei, ba_tot_longi)
}
rm(ba_tot_longi)

if (!(exists("littpli_sumAll")) & exists("littpli_sum")) {
  littpli_sumAll <- littpli_sum
  rm(littpli_sum)
  } else if (exists("littpli_sumAll") & exists("littpli_sum")) {
    littpli_sumAll <- littpli_sumAll %>% bind_rows(littpli_sum)
        rm(littpli_sum)
  }

}


# Write subplot plot polygons to shapefile and kml
if (writeShapeFile == "yes") {
outShapeName <- paste(sitei,"_NEON", "_", plot_type, "_base_plots_LL", sep="")
outKMLName <- paste(sitei,"_NEON", "_", plot_type, "_base_plots_LL_kml", sep="")
# Write to shape file to download Landsat data from GEE
writeOGR(subplotsAll.poly, dsn="..//gis", layer= outShapeName, driver="ESRI Shapefile")
writeOGR(subplotsAll.poly,dsn=paste("..//gis", outKMLName, sep=""),layer=outKMLName,driver="KML")
}

# Clean up any outliers and replace NA with zero
ba_tot_sitei <- ba_tot_sitei %>% replace(is.na(.), 0) %>% 
dplyr::filter((basalAreaM2ha_sum.liv + basalAreaM2ha_sum.dam) < 150) %>%
  rename(baM2ha.live = basalAreaM2ha_sum.liv, tph.live = tph_sum.liv, n.live = n_sum.liv,
         baM2ha.damage = basalAreaM2ha_sum.dam, tph.damage = tph_sum.dam, n.damage = n_sum.dam,
         baM2ha.dead = basalAreaM2ha_sum, tph.dead = tph_sum, n.dead = n_sum) %>%
  mutate(baM2ha.live.tot = baM2ha.live + baM2ha.damage, tph.live.tot = tph.live + tph.damage,
         n.live.tot = n.live + n.damage, year = as.numeric(year))

# Add variables for change in each plot-level annual summary of ba
ba_tot_sitei <- ba_tot_sitei %>% group_by(plot) %>% arrange(plot,year) %>% 
  mutate(ba.change.live = (baM2ha.live - lag(baM2ha.live, n=1))/ (year - lag(year, n=1)),
         ba.change.damage = (baM2ha.damage - lag(baM2ha.damage, n=1))/ (year - lag(year, n=1)),
         ba.change.live.tot = (baM2ha.live.tot - lag(baM2ha.live.tot, n=1))/ (year - lag(year, n=1)),
         ba.change.dead = (baM2ha.dead - lag(baM2ha.dead, n=1))/ (year - lag(year, n=1)))


# Combine plot-level spba long tables into one table per site.
spbaListi <- grep(paste("spbai_",sitei,sep=""), ls(), value=T)
for (j in 1:length(spbaListi)) {
  if (j == 1) {
    spbaAll <- get(spbaListi[j])
  } else {
    spbaAll <- spbaAll %>% bind_rows(get(spbaListi[j]))
  }
}

# Combine plot-level tree long tables into one table per site.
vegpliListi <- grep(paste("vegpli_",sitei,sep=""), ls(), value=T)
for (j in 1:length(vegpliListi)) {
  if (j == 1) {
    vegpliAll <- get(vegpliListi[j])
  } else {
    vegpliAll <- vegpliAll %>% bind_rows(get(vegpliListi[j]))
  }
}

# 2022-03-26 Add column attributes from plot polygon data (nlcd_class)
ba_tot_sitei <- ba_tot_sitei %>% 
  inner_join(polysi@data %>% dplyr::select(plotID, nlcdClass, elevation, minElev, maxElev,
                                           slope, aspect), by = c("plot" = "plotID"))

# Need to modify this if-statement for distributed plot data at the same sites...
if (length(grep(sitei, dir("processed"))) == 0) {
  write.csv(ba_tot_sitei, paste("processed//",sitei,"_",plot_type,"_ba_tph_annual_plot_totals.csv", sep=""), row.names=F)
  write.csv(littpli_sumAll, paste("processed//",sitei,"_",plot_type,"_litt_annual_trap_sums.csv", sep=""), row.names=F)
  write.csv(lplotLoc, paste("processed//", sitei,"_",plot_type, "_litt_plot_trap_locations.csv", sep=""), row.names=F)
  write.csv(polysi@data, paste("processed//", sitei,"_",plot_type, "_plot_poly_data.csv", sep=""), row.names=F)
  write.csv(spbaAll, paste("processed//", sitei,"_",plot_type, "_spba_plantStatus2_data.csv", sep=""), row.names=F)
  write.csv(vegpliAll, paste("processed//",sitei,"_",plot_type, "_NEON_tree.csv",sep=""), row.names=F)
}

rm(ba_tot_sitei, littpli_sumAll, lplotLoc, polysi, spbaAll, vegpliAll)
}