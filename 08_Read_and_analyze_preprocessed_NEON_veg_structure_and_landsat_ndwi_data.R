library(dplyr)
library(viridis)

setwd("data")
# Load some Landsat data to compare
vegDir <- "processed\\v2022-04-12\\"
landsatDir <- "Landsat_processed\\"

# Set makePng to "yes" if you want to output figures to PNG files
makePng <- "yes"

# Try to read in all the processed files for NDWI and ba plot summaries...
if (!(exists('procFiles'))) {
  procFiles <- dir(vegDir)
  landsatFiles <- dir(landsatDir)
  # Filter out litter data summaries for now
  #procFiles <- procFiles[-(c(grep("litt", procFiles), grep("v9",procFiles)))]
  landsatFiles <- grep("time_series_out", landsatFiles, value=T)
  #procFiles <- procFiles[grep("ba_tph", procFiles)]
  #sitesP <- sort(unique(substr(procFiles, 1, 4)))
  sitesP <- c('ABBY','BART','BLAN','GRSM','GUAN','HARV','JERC','MLBS','OSBS','SCBI',
    'SOAP','STEI','TALL','TEAK','UKFS','UNDE')
  sitesP <- sitesP[-5] # For now, until Landsat reprocessed for GUAN
  # paste(sitesP, collapse="\',\'")
  
  for (p in 1:length(sitesP)) {
    filesP <- sort(grep(sitesP[p], procFiles, value=T))
    landsatFilesP <- grep(sitesP[p], landsatFiles, value=T)
    print(filesP)
    print(landsatFilesP)
    baP <- read.csv(paste(vegDir,grep("_ba_tph_",filesP, value=T), sep=""), header=T)
    treeP <- read.csv(paste(vegDir,grep("_NEON_tree",filesP, value=T), sep=""), header=T)
    littP <- read.csv(paste(vegDir, grep("_litt_annual_",filesP, value=T), sep=""), header=T)
    ndwiP <- read.csv(paste(landsatDir, landsatFilesP, sep=""), header=T)
    #filesP <- filesP[-grep("NDWI",filesP, value=F)]
    #baP <- read.csv(paste("processed//",grep("_ba", filesP, value=T), sep=""), header=T)
    if (p == 1) {
      baAll <- baP
      ndwiAll <- ndwiP
      treeAll <- treeP
      littAll <- littP
    } else {
      baAll <- baAll %>% bind_rows(baP)
      ndwiAll <- ndwiAll %>% bind_rows(ndwiP)
      treeAll <- treeAll %>% bind_rows(treeP)
      littAll <- littAll %>% bind_rows(littP)
    }
    try(rm(filesP, baP, ndwiP, treeP, littP), silent=T)
  }
  
}

# Add a variable for different symbols by nlcd class
baAll <- baAll %>% mutate(nlcdPch = case_when(nlcdClass == "deciduousForest" ~ 21,
                    nlcdClass == "evergreenForest" ~ 24,
                    nlcdClass == "grasslandHerbaceous" ~ 25,
                    nlcdClass == "mixedForest" ~ 23,
                    nlcdClass == "shrubScrub" ~ 25,
                    nlcdClass == "woodyWetlands" ~ 22),
                    siteID = substr(plot, 1,4))# %>% 

treeAll <- treeAll %>% mutate(siteID = substr(plotID, 1, 4))
littAll <- littAll %>% mutate(plotID = substr(trapID, 1, 8), siteID = substr(trapID, 1,4))

littSub <- littAll %>% group_by(siteID, plotID, functionalGroup) %>% dplyr::filter(!(all(dryMass == 0)))
littYear <- littSub %>% group_by(siteID, plotID,year) %>% 
  dplyr::filter(functionalGroup %in% c("Leaves","Mixed","Needles") & durationDays >= 300) %>% 
  summarize_at(vars(dryMassGm2Day, durationDays), funs(mean))
  #dplyr::filter(!(nlcdClass %in% c("grasslandHerbaceous"))) # Keep, as some ABBY sites classified as this

# Simple plot of litter by site and year
sitei <- sitesP[15]#"GRSM"
with(littYear %>% dplyr::filter(siteID == sitei), plot(year, dryMassGm2Day))
with(littYear %>% dplyr::filter(siteID == sitei) %>% group_by(year) %>% 
       summarize_at(vars(dryMassGm2Day),funs(mean)), lines(year, dryMassGm2Day))
mtext(sitei)
##############################################################################################

# Subset ndwi table and rename variables for easier use and sharing

# Add some lagged and leading variables
ndwi <- ndwiAll %>% group_by(plot) %>% arrange(plot, year) %>% 
  dplyr::select(plot, site, year, ndwi_min, r_obs_median, r_obs_mean, r_obs_q2.5,
                r_obs_q97.5, fbmi_med, fbmi_q2.5, fbmi_q97.5, rx_median, rx_mean,
                rx_q2.5, rx_q97.5, spline_median, q2.5_median, q97.5_median) %>% 
  # Correct an errant plot-year
  mutate(fbmi_med = case_when(plot == "HARV_041" & year == 2000 ~ mean(fbmi_med, na.rm=T),
         TRUE ~ fbmi_med), 
         rx_median = case_when(plot == "HARV_041" & year == 2000 ~ mean(rx_median, na.rm=T),
         TRUE ~ rx_median), r_obs_median = case_when(plot == "HARV_041" & year == 2000 ~ mean(r_obs_median, na.rm=T),
         TRUE ~ r_obs_median),
         ndwi_min_all_years = min(r_obs_median, na.rm=T),
         fbmi_min_all_years = min(fbmi_med, na.rm=T),
         resid_min_all_years = min(rx_median, na.rm=T)) %>% 
      mutate(fbmi_med_lead_1 = lead(fbmi_med, 1), fbmi_med_lead_2 = lead(fbmi_med, 2),
         fbmi_med_lead_3 = lead(fbmi_med, 3, order_by = year),
         fbmi_med_lead_4 = lead(fbmi_med, 4, order_by = year),
         fbmi_med_lead_5 = lead(fbmi_med, 5, order_by = year),
         fbmi_med_lag_1 = lag(fbmi_med, 1, order_by = year), 
         fbmi_med_lag_2 = lag(fbmi_med, 2, order_by = year),
         fbmi_med_lag_3 = lag(fbmi_med, 3, order_by = year),
         fbmi_med_lag_4 = lag(fbmi_med, 4, order_by = year),
         fbmi_med_lag_5 = lag(fbmi_med, 5),
         fbmi_med_lag_mean5 = (fbmi_med_lag_1 + fbmi_med_lag_2 + fbmi_med_lag_3 + 
                                 fbmi_med_lag_4 + fbmi_med_lag_5)/5,
         resids_med_lead_1 = lead(rx_median, 1, order_by = year),
         resids_med_lead_2 = lead(rx_median, 2, order_by = year),
         resids_med_lead_3 = lead(rx_median, 3, order_by = year),
         resids_med_lead_4 = lead(rx_median, 4, order_by = year),
         resids_med_lead_5 = lead(rx_median, 5, order_by = year),
         resids_med_lag_1 = lag(rx_median, 1, order_by = year),
         resids_med_lag_2 = lag(rx_median, 2, order_by = year),
         resids_med_lag_3 = lag(rx_median, 3, order_by = year),
         resids_med_lag_4 = lag(rx_median, 4, order_by = year),
         resids_med_lag_5 = lag(rx_median, 5),
         resids_med_lag_mean5 = (resids_med_lag_1 + resids_med_lag_2 + resids_med_lag_3 + 
                                   resids_med_lag_4 + resids_med_lag_5)/5,
         ndwi_med_lead_1 = lead(r_obs_median, 1, order_by = year),
         ndwi_med_lead_2 = lead(r_obs_median, 2, order_by = year),
         ndwi_med_lead_3 = lead(r_obs_median, 3, order_by = year),
         ndwi_med_lead_4 = lead(r_obs_median, 4, order_by = year),
         ndwi_med_lead_5 = lead(r_obs_median, 5, order_by = year),
         ndwi_med_lag_1 = lag(r_obs_median, 1, order_by = year),
         ndwi_med_lag_2 = lag(r_obs_median, 2, order_by = year),
         ndwi_med_lag_3 = lag(r_obs_median, 3, order_by = year),
         ndwi_med_lag_4 = lag(r_obs_median, 4, order_by = year),
         ndwi_med_lag_5 = lag(r_obs_median, 5, order_by = year),
         ndwi_med_lag_mean5_plot = (ndwi_med_lag_1 + ndwi_med_lag_2 + ndwi_med_lag_3 + 
                                 ndwi_med_lag_4 + ndwi_med_lag_5)/5,
         fbmi_min_lag_5_plot = min(fbmi_med, fbmi_med_lag_1, fbmi_med_lag_2, 
                              fbmi_med_lag_3, fbmi_med_lag_4, fbmi_med_lag_5, na.rm=T),
         ndwi_min_lag_5_plot = min(r_obs_median, ndwi_med_lag_1, ndwi_med_lag_2, 
                              ndwi_med_lag_3, ndwi_med_lag_4, ndwi_med_lag_5, na.rm=T),
         resids_min_lag_5_plot = min(rx_median, resids_med_lag_1, resids_med_lag_2, 
                                resids_med_lag_3, resids_med_lag_4, resids_med_lag_5, na.rm=T),
         ndwi_change = r_obs_median - ndwi_med_lag_1,
         fbmi_change = fbmi_med - fbmi_med_lag_1,
         resids_change = rx_median - resids_med_lag_1,
         #ndwi_change_neg_cum = ndwi_change + lag(ndwi_change, 1, order_by = year) * (lag(ndwi_change, 1, order_by = year) < 0) +
         #  lag(ndwi_change, 2, order_by = year) * (lag(ndwi_change, 2, order_by = year) < 0) + 
         #  lag(ndwi_change, 3, order_by = year) * (lag(ndwi_change, 3, order_by = year) < 0) +
         #  lag(ndwi_change, 4, order_by = year) * (lag(ndwi_change, 4, order_by = year) < 0) +
         #  lag(ndwi_change, 5, order_by = year) * (lag(ndwi_change, 5, order_by = year) < 0),
         site = substr(site, 1,4)) %>% ungroup() %>% group_by(plot, year) %>%
          mutate(ndwi_med_lag_mean5 = (ndwi_med_lag_1 + ndwi_med_lag_2 + ndwi_med_lag_3 + 
                                      ndwi_med_lag_4 + ndwi_med_lag_5)/5,
              fbmi_min_lag_5 = min(fbmi_med, fbmi_med_lag_1, fbmi_med_lag_2, 
                                   fbmi_med_lag_3, fbmi_med_lag_4, fbmi_med_lag_5),
              ndwi_min_lag_5 = min(r_obs_median, ndwi_med_lag_1, ndwi_med_lag_2, 
                                   ndwi_med_lag_3, ndwi_med_lag_4, ndwi_med_lag_5),
              resids_min_lag_5 = min(rx_median, resids_med_lag_1, resids_med_lag_2, 
                                     resids_med_lag_3, resids_med_lag_4, resids_med_lag_5),
              ndwi_min_lag_10 = min(r_obs_median, ndwi_med_lag_1, ndwi_med_lag_2, 
                                   ndwi_med_lag_3, ndwi_med_lag_4, ndwi_med_lag_5,
                                   lag(r_obs_median, 6),lag(r_obs_median, 7),
                                   lag(r_obs_median, 8),lag(r_obs_median, 9),
                                   lag(r_obs_median, 10), na.rm=T)) %>%
  rename(ndwi_med = r_obs_median, ndwi_mean = r_obs_mean, ndwi_q2.5 = r_obs_q2.5,
         ndwi_q97.5 = r_obs_q97.5, spline_med = spline_median, 
         spline_q2.5 = q2.5_median, spline_q97.5 = q97.5_median,
         resids_med = rx_median, resids_mean = rx_mean, resids_q2.5 = rx_q2.5, 
         resids_q97.5 = rx_q97.5)


# Calculate some minima for 5-year intervals
ndwiTmp <- ndwi %>% ungroup() %>% group_by(plot) %>% dplyr::filter(year %in% 2010:2014) %>% 
  dplyr::select(plot, ndwi_med) %>%
  summarize_at(vars(ndwi_med), min, na.rm=T) %>% rename(ndwiMin_2010_2014 = ndwi_med)
ndwiTmp2 <- ndwi %>% ungroup() %>% group_by(plot) %>% dplyr::filter(year %in% 2005:2009) %>% 
  dplyr::select(plot, ndwi_med) %>%
  summarize_at(vars(ndwi_med), min, na.rm=T) %>% rename(ndwiMin_2005_2009 = ndwi_med)
ndwiTmp3 <- ndwi %>% ungroup() %>% group_by(plot) %>% dplyr::filter(year %in% 2000:2004) %>% 
  dplyr::select(plot, ndwi_med) %>%
  summarize_at(vars(ndwi_med), min, na.rm=T) %>% rename(ndwiMin_2000_2004 = ndwi_med)

ndwi <- ndwi %>% full_join(ndwiTmp) %>% full_join(ndwiTmp2) %>% full_join(ndwiTmp3) %>%
  # Subset to plots co-occurring in baAll
  dplyr::filter(plot %in% baAll$plot) %>% 
  mutate(ndwiMin_2005_2014 = min(ndwiMin_2005_2009, ndwiMin_2010_2014, na.rm=T),
    ndwiMin_2000_2014 = min(ndwiMin_2000_2004,ndwiMin_2005_2009, 
                            ndwiMin_2010_2014, na.rm=T))


# Create colors for NEON sites
siteColors <- data.frame(site = sitesP, color = c(viridis(8)[-1], magma(9)[-2]))

# Calculate a site-level mean and visualize time series by site
siteMean <- ndwi %>% group_by(site, year) %>% summarize_at(vars(ndwi_med, fbmi_med), mean)

if (makePng == "yes") {
  png(filename=paste("..//figures//","NEON_Site_Mean_NDWI_Med_Tower_Plots.png",sep=""),width=6.22,height=4.61,
      units="in",pointsize=12,res=300)
  par(mar = c(5.1, 4.1, 1.1, 1.1))
}
for (i in c(1:2,4:length(sitesP))) {
  if (i == 1) {
    with(siteMean[grep(sitesP[i], siteMean$site),], plot(year, ndwi_med, col=siteColors$color[i], type="l"))
  } else {
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, ndwi_med, col=siteColors$color[i]))
  }
  with(siteMean[grep(sitesP[i], siteMean$site),], points(year, ndwi_med, col=siteColors$color[i],
                                                         pch=21, bg=siteColors$color[i]))
}
legend("topleft", siteColors$site, pch=21, cex = 1.0, pt.bg = siteColors$color, bty ="n")
if (makePng == "yes") {
  dev.off()
}

siteMeanBa <- baAll %>% group_by(siteID, year) %>% summarize_at(vars(baM2ha.live, baM2ha.dead,
               baM2ha.damage, baM2ha.live.tot), mean, na.rm=T)

if (makePng == "yes") {
  png(filename=paste("..//figures//","NEON_Site_Mean_FBMI_Med_Tower_Plots.png",sep=""),width=6.22,height=4.61,
      units="in",pointsize=12,res=300)
  par(mar = c(5.1, 4.1, 1.1, 1.1))
}
# Repeat vis with FBMI - visualize percent change within sites
for (i in c(1:2,4:length(sitesP))) {
  if (i == 1) {
    with(siteMean[grep(sitesP[i], siteMean$site),], plot(year, fbmi_med, col=siteColors$color[i], type="l"))
  } else {
    with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, fbmi_med, col=siteColors$color[i]))
  }
  with(siteMean[grep(sitesP[i], siteMean$site),], points(year, fbmi_med, col=siteColors$color[i],
                                                         pch=21, bg=siteColors$color[i]))
}
legend("topleft", siteColors$site, pch=21, cex = 1.5, pt.bg = siteColors$color, bty ="n")
if (makePng == "yes") {
  dev.off()
}


# Make line plots by site with individual time-series by plot
for (i in c(1:length(sitesP))) {
  plotsi <- ndwi %>% ungroup() %>% dplyr::filter(site == sitesP[i]) %>% 
  dplyr::select(plot) %>% distinct() %>% unlist() %>% unname()
  
  if (makePng == "yes") {
    png(filename=paste("..//figures//","NEON_Plot_Mean_NDWI_Med_Tower_Plots_",sitesP[i], ".png",sep=""),width=6.22,height=4.61,
        units="in",pointsize=12,res=300)
    par(mar = c(5.1, 4.1, 1.1, 1.1))
  }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], plot(year, ndwi_med, 
     col=siteColors$color[i], type="n", lwd=1, ylim=c(-0.2,0.55)))
  abline(v=seq(1980,2030, by=5), lty=2, col="grey")

    for (j in 1:length(plotsi)) {
    with(ndwi[grep(plotsi[j], ndwi$plot),], lines(year, ndwi_med, col=siteColors$color[i]))
    with(ndwi[grep(plotsi[j], ndwi$plot),], points(year, ndwi_med, col=siteColors$color[i],
          pch=21, bg=siteColors$color[i], cex=0.5))
    }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, ndwi_med, 
                                                        col="lightgrey", 
                                                        lty=1, lwd = 3))
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, ndwi_med, 
    col="black", lty=2, lwd = 2))
  
  mtext(paste(sitesP[i]))
  if (makePng == "yes") {
    dev.off()
  }
}



# Make line plots by site with individual time-series by plot FBMI
for (i in c(1:length(sitesP))) {
  plotsi <- ndwi %>% ungroup() %>% dplyr::filter(site == sitesP[i]) %>% 
    dplyr::select(plot) %>% distinct() %>% unlist() %>% unname()
  
  if (makePng == "yes") {
    png(filename=paste("..//figures//","NEON_Plot_Mean_FBMI_Med_Tower_Plots_",sitesP[i], ".png",sep=""),width=6.22,height=4.61,
        units="in",pointsize=12,res=300)
    par(mar = c(5.1, 4.1, 1.1, 1.1))
  }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], plot(year, fbmi_med, 
       col=siteColors$color[i], lwd=2, ylim=c(0,1.2), type="n"))
  abline(v=seq(1980,2030, by=5), lty=2, col="grey")
  
  for (j in 1:length(plotsi)) {
    with(ndwi[grep(plotsi[j], ndwi$plot),], lines(year, fbmi_med, col=siteColors$color[i]))
    with(ndwi[grep(plotsi[j], ndwi$plot),], points(year, fbmi_med, col=siteColors$color[i],
                                                   pch=21, bg=siteColors$color[i], cex=0.5))
  }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, fbmi_med, 
                                                        col="lightgrey", 
                                                        lty=1, lwd = 3))
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, fbmi_med, 
                                                        col="black", lty=2, lwd = 2))
  mtext(paste(sitesP[i]))
  if (makePng == "yes") {
    dev.off()
  }
  
}


addBa <- "yes"
# 2010-2021 Make line plots by site with individual time-series by plot FBMI add BA
for (i in c(1:length(sitesP))) {
  plotsi <- ndwi %>% ungroup() %>% dplyr::filter(site == sitesP[i]) %>% 
    dplyr::select(plot) %>% distinct() %>% unlist() %>% unname()
  
  if (makePng == "yes") {
    png(filename=paste("..//figures//","NEON_Plot_Mean_FBMI_And_BA_Damage_Med_Tower_Plots_",sitesP[i], ".png",sep=""),width=6.22,height=4.61,
        units="in",pointsize=12,res=300)
    par(mar = c(5.1, 4.1, 1.1, 3.1))
  }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], plot(year, fbmi_med, 
      col=siteColors$color[i], lwd=2, ylim=c(0,1.2), type="n", xlim=c(2010,2021)))
  abline(v=seq(1980,2030, by=5), lty=2, col="grey")
  
  for (j in 1:length(plotsi)) {
    with(ndwi[grep(plotsi[j], ndwi$plot),], lines(year, fbmi_med, col=siteColors$color[i]))
    with(ndwi[grep(plotsi[j], ndwi$plot),], points(year, fbmi_med, col=siteColors$color[i],
                                                   pch=21, bg=siteColors$color[i], cex=0.5))
  }
  
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, fbmi_med, 
                                                        col="lightgrey", 
                                                        lty=1, lwd = 3))
  with(siteMean[grep(sitesP[i], siteMean$site),], lines(year, fbmi_med, 
                                                        col="black", lty=2, lwd = 2))
  mtext(paste(sitesP[i]))
  
  if (addBa == "yes") {
    par(new = T)
    with(siteMeanBa[grep(sitesP[i], siteMeanBa$siteID),], 
         base::plot(year, baM2ha.damage, 
                    col="darkgrey", lwd=2, xlim=c(2010,2021), 
                    ylim=c(0,max(baAll$baM2ha.damage[grep(sitesP[i], baAll$siteID)])), type="l",
                    xlab = "", ylab = "", axes=F))
    
    
    for (j in 1:length(plotsi)) {
      with(baAll[grep(plotsi[j], baAll$plot),], lines(year, baM2ha.damage, col="darkgrey"))
      with(baAll[grep(plotsi[j], baAll$plot),], points(year, baM2ha.damage, col="darkgrey",
                                                       pch=21, bg=siteColors$color[i], cex=0.5))
    }
    axis(side=4)
    mtext("baM2ha.damage", side=4, line=2)
  }
  if (makePng == "yes") {
    dev.off()
  }
}


 
#write.csv(baNdwi,  paste("processed//",sitei,"_ba_tph_NDWI_annual_plot_totals.csv", sep=""), row.names=F)

# New 2022-01-04 - Rank lagged NDWI for models so that order of disturbance years doesn't matter.
# Rank = low NDWI value (1) to high (6+)
#Ndwi_rank <- baNdwi %>% group_by(plot) %>% arrange(desc(year)) %>% 
#  dplyr::filter(year <= max(ba_tot_sitei$year))%>% slice(1:6) %>% 
#  mutate(rank = rank(r_obs_median, ties.method = "first")) %>% dplyr::select(plot, year, rank, r_obs_median) %>% data.frame()
#Ndwi_rank_wide <- Ndwi_rank %>% arrange(rank) %>% dplyr::select(-year) %>% 
#  pivot_wider(names_from = rank, 
#              values_from = r_obs_median,
#              names_glue = "{'ndwiRank'}_{rank}"
#  )


baNdwi <- baAll %>% mutate(site = substr(plot, 1, 4)) %>% 
  inner_join(siteColors, by = c("site" = "site")) %>% inner_join(ndwi) #%>% 
  #dplyr::filter(!(site %in% c("SOAP","TEAK")))

# Make some summary tables before excluding SOAP and TEAK sites. (No remeasurement, but disturbance in Landsat)
# Examine plot-years with the most negative change in NDWI
plotMaxYear <- baAll %>% group_by(plot) %>% summarize_at(vars(year), max)

ndwiCh <- ndwi %>% group_by(plot) %>% full_join(plotMaxYear, by = c("plot" = "plot"),suffix = c("",".max")) %>% 
  dplyr::filter(year >= 2015 & year <= year.max) %>% 
  # dplyr::arrange(plot, year) %>% mutate(ndwi_change_cum = cumsum(ndwi_change),
  #         ndwi_change_masked = case_when(ndwi_change < 0 ~ ndwi_change,
  #         TRUE ~ 0), ndwi_change_cum_masked = cumsum(ndwi_change_masked)) %>% dplyr::filter(ndwi_change < 0)
  dplyr::arrange(ndwi_change) %>% dplyr::slice_head(., n=1)
ndwiCh %>% group_by(site) %>% summarize_at(vars(ndwi_change, fbmi_change),funs(min, max), na.rm=T)
ndwiCh <- ndwiCh %>% group_by(site) %>% arrange(site,ndwi_change)

ndwiCh2021 <- ndwi %>% group_by(plot) %>% full_join(plotMaxYear, by = c("plot" = "plot"),suffix = c("",".max")) %>% 
  dplyr::filter(year >= 2015) %>% 
  # dplyr::arrange(plot, year) %>% mutate(ndwi_change_cum = cumsum(ndwi_change),
  #         ndwi_change_masked = case_when(ndwi_change < 0 ~ ndwi_change,
  #         TRUE ~ 0), ndwi_change_cum_masked = cumsum(ndwi_change_masked)) %>% dplyr::filter(ndwi_change < 0)
  dplyr::arrange(ndwi_change) %>% dplyr::slice_head(., n=1)
ndwiCh2021 %>% group_by(site) %>% summarize_at(vars(ndwi_change, fbmi_change),funs(min, max), na.rm=T)
ndwiCh2021 <- ndwiCh2021 %>% group_by(site) %>% arrange(site,ndwi_change)



plotAreas <- treeAll %>% mutate(siteID = substr(plotID, 1,4)) %>% 
  dplyr::filter(!(siteID %in% c("SOAP", "TEAK")))%>% group_by(plotID) %>% 
  summarize_at(vars(totalSampledAreaTrees, totalSampledAreaTreesOrig), unique ) 
plotAreas %>% ungroup %>% dplyr::select(totalSampledAreaTrees) %>% table()

# Tabulate number of plots sampled by eventYear bouts per year and site
plotYear <- baAll %>% mutate(plotID = plot, siteID = substr(plot, 1,4)) %>% 
  group_by(siteID, plotID) %>% dplyr::filter((n.live.tot + n.dead) > 0) %>% 
  dplyr::select(year) %>% mutate(count = 1)
plotYearTab <- plotYear %>% pivot_wider(names_from = year, values_from = count) %>% group_by(siteID) %>%
  dplyr::select(-plotID) %>% summarize_at(vars('2015','2016','2017','2018','2019','2020'), sum, na.rm=T)

plotYears <- baAll %>% mutate(plotID = plot, siteID = substr(plot, 1,4)) %>% 
  group_by(siteID, plotID) %>% dplyr::filter((n.live.tot + n.dead) > 0) %>% 
  dplyr::select(year) %>% count() %>% group_by(siteID) %>% summarize_at(vars(n), funs(min,max))
plotYears <- baAll %>% mutate(plotID = plot, siteID = substr(plot, 1,4)) %>% 
  group_by(siteID, plotID) %>% dplyr::filter((n.live.tot + n.dead) > 0) %>% 
  group_by(siteID) %>% summarize_at(vars(year), funs(min,max)) %>% 
  inner_join(plotYears, by = c("siteID" = "siteID"), suffix = c(".yr",".yrs"))

yearDisturbed <- ndwiCh %>% group_by(site) %>% dplyr::select(year) %>% table()
yearDisturbed <- yearDisturbed %>% data.frame() %>% 
  tidyr::pivot_wider(names_from = year, values_from = Freq)
# For the 19 plots remeasured in 2021, none had the most negative change in NDWI that year, so have to re-add the column here.
#yearDisturbed$'2021' <- 0
yearDisturbed$"n.plots" <- apply(yearDisturbed[,2:ncol(yearDisturbed)], 1, sum)

yearDistAllYears <- ndwiCh2021 %>% group_by(site) %>% dplyr::select(year) %>% table()
yearDistAllYears <- yearDistAllYears %>% data.frame() %>% 
  tidyr::pivot_wider(names_from = year, values_from = Freq)
# For the 19 plots remeasured in 2021, none had the most negative change in NDWI that year, so have to re-add the column here.
#yearDistAllYears$'2021' <- 0
yearDistAllYears$"n.plots" <- apply(yearDistAllYears[,2:ncol(yearDistAllYears)], 1, sum) 
# Examine plot-years with the most positive increase in tree damage. Shift to highest
# live ba in damaged class, as we can't calculate change in the first year for plots...
baDam <- baNdwi %>% group_by(plot) %>% dplyr::filter(year >= 2015) %>%
  dplyr::arrange(desc(baM2ha.damage)) %>% dplyr::slice_head(., n=1) #baM2ha.damage ba.change.damage

yearDist2 <- baDam %>% group_by(site) %>% dplyr::select(year) %>% table()
yearDist2 <- yearDist2 %>% data.frame() %>% 
  tidyr::pivot_wider(names_from = year, values_from = Freq)
#yearDist2$'2021' <- NA

yearDist3 <- mapply(function(x, y){paste(x,y, sep = "|")}, yearDisturbed[,-c(1,8)], yearDist2[,-c(1)]) %>% as.data.frame()
yearDist3$siteID <- yearDisturbed$site

table1 <- plotYears %>% inner_join(yearDisturbed %>% dplyr::select(site, n.plots), by=c("siteID"="site")) %>% 
  inner_join(yearDist3, by=c("siteID" = "siteID"))

# Check plantStatus key
treeAll %>% dplyr::select(plantStatus, plantStatus2) %>% distinct() %>% arrange(plantStatus2)

datenow = as.character(Sys.Date())
# Create output table with stargazer
stargazer(table1, title = "Table 1. Frequency and range of years that plots were remeasured by site, and year of recent disturbance onset by plot (most negative change in Landsat NDWI from the preceding year).",
          summary=F, out = paste("tables\\", "Table_1_", datenow,".doc", sep=""), type="html", rownames=F)

# Now exclude SOAP and TEAK for models of remeasured sites...
baNdwi <- baAll %>% mutate(site = substr(plot, 1, 4)) %>% 
  inner_join(siteColors, by = c("site" = "site")) %>% inner_join(ndwi) %>% 
dplyr::filter(!(site %in% c("SOAP","TEAK"))) #%>% dplyr::filter(site == "UNDE")

# Set plotting variable name
plot_y <- "ba.change.live.prop"       #"baM2ha.damage"#"baM2ha.dead"#"baM2ha.live.tot"
#"year,baM2ha.live,tph.live,n.live,baM2ha.damage,tph.damage,n.damage,baM2ha.dead,
#tph.dead,n.dead,plot,baM2ha.live.tot,tph.live.tot,n.live.tot,ba.change.live,
#ba.change.damage,ba.change.live.tot,ba.change.dead,nlcdClass,elevation,
#minElev,maxElev,slope,aspect,nlcdPch"

# Try simple OLS linear models to predict plot-level basal area and change from Landsat
with(baNdwi, summary(lm(get(plot_y) ~ fbmi_med + fbmi_med_lag_1 + fbmi_med_lag_2 + 
                          fbmi_med_lag_3 + fbmi_med_lag_4 + fbmi_med_lag_5 + spline_med))) #fbmi_lag123_mn + spline_median)))
with(baNdwi, summary(lm(get(plot_y) ~ ndwi_med + ndwi_med_lag_1 + ndwi_med_lag_2 + 
                          ndwi_med_lag_3 + ndwi_med_lag_4 + ndwi_med_lag_5))) #fbmi_lag123_mn + spline_median)))
#with(baNdwi, summary(lm(get(plot_y) ~ resids_med + resids_med_lag_1 + resids_med_lag_2 + 
#                          resids_med_lag_3 + resids_med_lag_4 + resids_med_lag_5))) #fbmi_lag123_mn + spline_median)))

plot_x <- "fbmi_change"       #"ndwi_min_all_years"#"ndwiMin_2000_2014"
#c(grep("ndwi", names(baNdwi), value=T), grep("fbmi", names(baNdwi), value=T),
#grep("resids", names(baNdwi), value=T))
#[1] "ndwi_min"     "ndwi_med"  "ndwi_mean" "ndwi_q2.5" "ndwi_q97.5"        
#[6] "ndwi_med_lead_1" "ndwi_med_lead_2"  "ndwi_med_lead_3"  "ndwi_med_lead_4"  "ndwi_med_lead_5"   
#[11] "ndwi_med_lag_1" "ndwi_med_lag_2" "ndwi_med_lag_3" "ndwi_med_lag_4" "ndwi_med_lag_5"    
#[16] "ndwi_med_lag_mean5" "ndwi_min_lag_5" "ndwiMin_2010_2014" "ndwiMin_2005_2009" "ndwiMin_2000_2004"
#[21] "fbmi_med"             "fbmi_q2.5"            "fbmi_q97.5"           "fbmi_med_lead_1"     
#[25] "fbmi_med_lead_2"      "fbmi_med_lead_3"      "fbmi_med_lead_4"      "fbmi_med_lead_5"     
#[29] "fbmi_med_lag_1"       "fbmi_med_lag_2"       "fbmi_med_lag_3"       "fbmi_med_lag_4"      
#[33] "fbmi_med_lag_5"       "fbmi_med_lag_mean5"   "fbmi_min_lag_5"       "resids_med"          
#[37] "resids_mean"          "resids_q2.5"          "resids_q97.5"         "resids_med_lead_1"   
#[41] "resids_med_lead_2"    "resids_med_lead_3"    "resids_med_lead_4"    "resids_med_lead_5"   
#[45] "resids_med_lag_1"     "resids_med_lag_2"     "resids_med_lag_3"     "resids_med_lag_4"    
#[49] "resids_med_lag_5"     "resids_med_lag_mean5" "resids_min_lag_5" 

#pch_bgs <- c(viridis(length(plotsi_remeasured)/2), plasma(length(plotsi_remeasured)/2))
#pch_bgs <- c(1:10,viridis(length(plotsi_remeasured)/2))
with(baNdwi, plot(get(plot_y) ~ get(plot_x), pch=21, bg=color, cex=1.5,
                  xlab = paste(plot_x), ylab = paste(plot_y)))
with(baNdwi, text(get(plot_y) ~ get(plot_x), labels = substr(plot, 
                            nchar(plot)-2, nchar(plot)), pos = 4, cex=0.5))
mtext(paste(plot_x, "vs.",plot_y))
with(baNdwi, abline(lm(get(plot_y) ~ get(plot_x))))
with(baNdwi, summary(lm(get(plot_y) ~ get(plot_x))))
legend("topleft", siteColors$site, pch=21, cex = 1.5, pt.bg = siteColors$color,bty  ="n")

# Predict landsat variable from demographic categories...
with(baNdwi, summary(lm(get(plot_x) ~ baM2ha.live  + baM2ha.damage + baM2ha.dead)))


with(baNdwi, summary(lm(get(plot_y) ~ get(plot_x) + fbmi_min_lag_5 + 
                          fbmi_med_lag_mean5 + spline_med))) 

# Create subset dataset with just most recent observations to avoid repeated measures
#if (!(exists("baNdwiLast"))) {
  baNdwiLast <- baNdwi %>% group_by(site, plot) %>% dplyr::arrange(desc(year)) %>% 
    dplyr::slice_head(., n=1)
  # join with Ndwi_rank_wide
  #baNdwiLast <- baNdwiLast %>% inner_join(Ndwi_rank_wide)
#}

  baNdwiLast <- baNdwi %>% group_by(site, plot) %>% dplyr::arrange(ndwi_change) %>% 
    dplyr::slice_head(., n=1)
  
# Repeat some plots and models with subset data
with(baNdwiLast, plot(get(plot_y) ~ get(plot_x), pch=nlcdPch,#21, 
                      bg=color, cex=2.5,cex.axis = 1.75,
                      cex.lab = 2,
                      xlab = paste(plot_x, "(unitless)"), 
                      ylab = paste(plot_y, "(m2/ha)")))
                      #xlab = "minimum NDWI, prior 5 years (unitless)", 
                      #ylab = "Live overstory basal area (m2/ha)"))

with(baNdwiLast, text(get(plot_y) ~ get(plot_x), 
                      labels = substr(plot, nchar(plot)-2, nchar(plot)), pos = 4,
                      cex = 0.8))
#mtext(paste(plot_x, "vs.",plot_y))
with(baNdwiLast[!(baNdwiLast$site %in% c("TEAK","SOAP")),], abline(lm(get(plot_y) ~ get(plot_x))))
with(baNdwiLast[!(baNdwiLast$site %in% c("TEAK","SOAP")),], summary(lm(get(plot_y) ~ get(plot_x))))
legend("topleft", siteColors$site, pch=21, cex = 1.5, pt.bg = siteColors$color, bty ="n")

# Try simple OLS linear models to predict plot-level basal area and change from Landsat
with(baNdwiLast, summary(lm(get(plot_x) ~ baM2ha.live + baM2ha.damage + baM2ha.dead)))
with(baNdwiLast, summary(lm(get(plot_x) ~ baM2ha.live.tot * baM2ha.dead)))


with(baNdwiLast, summary(lm(get(plot_y) ~ fbmi_med + fbmi_med_lag_1 + fbmi_med_lag_2 + 
                              fbmi_med_lag_3 + fbmi_med_lag_4 + fbmi_med_lag_5 + spline_med))) #fbmi_lag123_mn + spline_median)))
with(baNdwiLast, summary(lm(get(plot_y) ~ ndwi_med + ndwi_med_lag_1 + ndwi_med_lag_2 + 
                              ndwi_med_lag_3 + ndwi_med_lag_4 + ndwi_med_lag_5))) #fbmi_lag123_mn + spline_median)))
#with(baNdwiLast, summary(lm(get(plot_y) ~ resids_med + resids_med_lag_1 + resids_med_lag_2 + 
#                              resids_med_lag_3 + resids_med_lag_4 + resids_med_lag_5))) #fbmi_lag123_mn + spline_median)))

with(baNdwiLast, summary(lm(get(plot_y) ~ get(plot_x) + fbmi_min_lag_5 + 
                              ndwi_med_lag_mean5 + spline_med))) 

#with(baNdwiLast, summary(lm(get(plot_y) ~ get(plot_x) + ndwiRank_1 + ndwiRank_2 + ndwiRank_3 + ndwiRank_4 + ndwiRank_5))) 
with(baNdwiLast, summary(lm(get(plot_y) ~ get(plot_x) + ndwiMin_2010_2014 + 
                              ndwiMin_2005_2009 + ndwiMin_2000_2004)))

