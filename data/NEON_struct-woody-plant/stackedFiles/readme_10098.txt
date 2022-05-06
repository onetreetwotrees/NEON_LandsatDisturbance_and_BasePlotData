###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
All files used during stacking are listed at the bottom of this document, which includes the data publication dates.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.

DATA PRODUCT INFORMATION
------------------------

ID: NEON.DOM.SITE.DP1.10098.001

Name: Woody plant vegetation structure

Description: Structure measurements, including height, crown diameter, and stem diameter, as well as mapped position of individual woody plants

NEON Science Team Supplier: Terrestrial Observation System

Abstract: This data product contains the quality-controlled, native sampling resolution data from in-situ measurements of live and standing dead woody individuals and shrub groups, from all terrestrial NEON sites with qualifying woody vegetation. The exact measurements collected per individual depend on growth form, and these measurements are focused on enabling biomass and productivity estimation, estimation of shrub volume and biomass, and calibration / validation of multiple NEON airborne remote-sensing data products. In general, comparatively large individuals that are visible to remote-sensing instruments are mapped, tagged and measured, and other smaller individuals are tagged and measured but not mapped. Smaller individuals may be subsampled according to a nested subplot approach in order to standardize the per plot sampling effort. Structure and mapping data are reported per individual per plot; sampling metadata, such as per growth form sampling area, are reported per plot. For additional details, see the user guide, protocols, and science design listed in the Documentation section in this data product's details webpage.

Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.

vst_apparentindividual:  90

vst_mappingandtagging:  90

vst_perplotperyear:  300

vst_shrubgroup:  90

Brief Design Description: Woody Plant Vegetation Structure data are collected from distributed and tower plots. Each distributed plot is sampled in a given bout if at least one tree with Diameter at Breast Height (DBH) ≥ 10 cm is present; if trees with DBH ≥ 10 cm are absent, distributed plots are sampled if smaller woody individuals comprise ≥ 10% aerial cover of the plot. Tower plots are sampled if at least one tree with DBH ≥ 10 cm is present in ≥ 10% of tower plots, or if smaller woody individuals comprise ≥ 10% of aerial cover averaged across all tower plots. Within both distributed and tower plots, all individuals with DBH ≥ 10 cm are mapped and measured throughout the plot sampling area. Individuals with DBH < 10 cm are mapped if a) individuals with DBH ≥ 10 cm are absent from the plot, and b) they are visible to airborne remote-sensing instruments. If stem density thresholds are met, individuals with DBH < 10 cm may be measured within nested subplots in order to standardize the sampling effort across plots.

At all sites at which qualifying woody vegetation is present, distributed plots are sampled every 5 years. At most sites with qualifying woody vegetation, a spatially-balanced subset of n=5 tower plots are sampled annually, and the full complement of tower plots is sampled every 5 years. At slow-growth-increment sites (RMNP, YELL, NIWO, WREF, SJER, SOAP, TEAK, BONA, DEJU, HEAL), dendrometer bands are measured annually on a subset of individuals within tower plots. At desert sites with sensitive vegetation and soils (i.e., MOAB, JORN, SRER, ONAQ), there is no annual tower plot measurement and the full complement of tower plots is measured every 5 years only.

At all sites, distributed plot sampling and ‘full’ tower plot sampling is scheduled in a staggered manner such that sites generate data either from all distributed plots or all tower plots every 2-3 years. Sites that do not have qualifying woody vegetation are surveyed every 5 years to determine whether ingrowth of qualifying woody vegetation has occurred.

At sites with seasonal senescence, the onset of sampling in a given year is triggered by senescence of canopy or understory individuals. Sampling must be completed before growth begins the following season and within 4 months of sampling onset. At sites with no distinct season, sampling begins within ± 2 weeks of the same date, and must be completed within 60 days of sampling onset. See NEON.DOC.000987 for more details.

Brief Study Area Description: Woody plant vegetation structure data are collected at all NEON terrestrial sites at which woody individuals presence or percent cover criteria. Functionally, sampling occurs at forested sites and sites with shrub/scrub vegetation. See the ‘Brief Design Description’ above for criteria.

Keywords: woody plants, biomass, saplings, shrubs, vegetation, net primary productivity (NPP), tree height, lianas, plant productivity, productivity, plants, trees, vegetation structure, production, carbon cycle, canopy height, annual net primary productivity (ANPP)


DATA PACKAGE CONTENTS
---------------------

This data product contains up to 4 data tables:

vst_mappingandtagging - Mapping, identifying and tagging of individual stems for remeasurement
vst_perplotperyear - Per plot sampling metadata, including presence/absence of each growthForm
vst_apparentindividual - Biomass and productivity measurements of apparent individuals
vst_shrubgroup - Biomass and productivity measurements of groups of shrubs
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic data package contains all measurements. An expanded data package is not available for this data product.

FILE NAMING CONVENTIONS
-----------------------

NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.

ISSUE LOG
---------

This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.10098.001.

Issue Date: 2020-10-01
Issue: Annual sampling of tower plots at desert sites caused unacceptable damage to sensitive soils and/or vegetation – e.g., soil biocrust damage, broken vegetation when measuring brittle, multi-stem desert shrubs.
       Date Range: 2014-01-01 to 2017-03-09
       Location(s) Affected: MOAB, JORN, SRER, ONAQ
Resolution Date: 
Resolution: Annual sampling of tower plots discontinued at affected sites; all tower plots sampled on a multi-year time interval. Sampling interval was 3-years between 2017-03-09 and 2018-07-26, and was changed to every 5-years after 2018-07-26.

Issue Date: 2020-10-01
Issue: Low quality annual stem diameter increment data from slow-growth-increment sites.
       Date Range: 2014-01-01 to 2017-03-09
       Location(s) Affected: RMNP, YELL, NIWO, MOAB, SRER, JORN, ONAQ, WREF, SJER, SOAP, TEAK, BONA, DEJU, HEAL
Resolution Date: 
Resolution: Annual sampling of tower plots discontinued at slow-growth increment sites. Sampling interval for tower plots at slow-growth-increment sites was changed to every 3-years between 2017-03-09 and 2018-07-26, and was changed to every 5-years between 2018-07-26 and 2019-08-30.

Issue Date: 2020-10-01
Issue: Lack of annual stem diameter data from slow-growth-increment sites and related inability to calculate above-ground annual net primary production at affected sites.
       Date Range: 2017-03-09 to 2019-08-30
       Location(s) Affected: RMNP, YELL, NIWO, MOAB, SRER, JORN, ONAQ, WREF, SJER, SOAP, TEAK, BONA, DEJU, HEAL
Resolution Date: 
Resolution: Installed dendrometer bands to enable high-resolution annual stem diameter measurement in a subset of tower plots at RMNP, YELL, NIWO, WREF, SJER, SOAP, TEAK, BONA, DEJU, and HEAL. Dendrometer bands not installed at slow-growth-increment desert sites due to lack of suitable vegetation (MOAB, SRER, JORN, ONAQ); these sites do not support annual stem diameter measurement.

Issue Date: 2021-01-06
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or eliminated sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
       Date Range: 2020-03-23 to 2021-06-01
       Location(s) Affected: All
Resolution Date: 
Resolution: 

Issue Date: 2019-03-05
Issue: Prior to the resolution date, multi-stem individuals with growth form of small tree, sapling, single shrub, or small shrub generated multiple records with the same individualID that could appear to be duplicates if stem diameter data were identical.
       Date Range: 2014-01-01 to 2020-07-31
       Location(s) Affected: All sites for which woody vegetation structure data have been published.
Resolution Date: 2020-07-31
Resolution: The `tempStemID` field was added to the vst_apparentindividual table. From the resolution date onward, the combination of the `eventID` x `individualID` x `tempStemID` fields should be unique within this table and multi-stem individuals with growth form of small tree, sapling, single shrub, and small shrub should no longer appear to be duplicates.

Issue Date: 2020-10-01
Issue: In the vst_perboutperyear table, data collected in the following fields were not published to the Data Portal: treesAbsentList, shrubsAbsentList, lianasAbsentList, cactiAbsentList, fernsAbsentList, yuccasAbsentList, palmsAbsentList, ocotillosAbsentList, xerophyllumAbsentList.
       Date Range: 2017-01-01 to 2020-07-31
       Location(s) Affected: All sites for which woody vegetation structure data have been published.
Resolution Date: 2020-07-31
Resolution: Missing data were located, L0 edits were performed to add missing data to the NEON database, and missing data were published to the NEON Data Portal.

Issue Date: 2020-03-10
Issue: Before 2020, distributed plot sampling would generate up to 20 vst_perboutperyear records per site, and tower plot sampling would generate up to 20 or 30 vst_perboutperyear records per site, depending on the total number of tower plots established at the site. In cases where fewer than the maximum number of records were generated per sampling event, the reduced number of records reflects lower levels of sampling effort and additional records should not be expected.
       Date Range: 2014-01-01 to 2020-03-10
       Location(s) Affected: All sites for which woody vegetation structure data have been published.
Resolution Date: 2020-03-10
Resolution: NEON added the ‘samplingImpractical’ quality flag to the vst_perboutperyear table to assist end-users in understanding when data for this product are temporarily missing versus permanently unavailable. Beginning in 2020, the number of records in the vst_perboutperyear table always represents the scheduled sampling effort, regardless of whether sampling was completed.

ADDITIONAL INFORMATION
----------------------

Queries for this data product return data from the user-specified date range for the `vst_perplotperyear`, `vst_apparentindividual` and `vst_shrubgroup` tables. For the `vst_mappingandtagging` table, queries ignore the user-specified date range and return all records for each user-selected site regardless of the user-specified date, due to the fact that individuals may be tagged and mapped in a year prior to the user-selected vegetation structure sampling event. Data are provided in monthly download files; queries including any part of a month will return data from the entire month. In the `vst_perplotperyear` table, there should be one record per plotID per eventID, and data in this table describe the presence/absence of woody growth forms, as well as the sampling area utilized for each growth form. The `vst_mappingandtagging` table contains at least one record per individualID, and provides data that are invariant through time, including tagID, taxonID, and mapped location (if applicable). Duplicates in `vst_mappingandtagging` may exist at the individualID level if errors have been corrected after ingest of the original record; in this instance, users are advised to use the most recent record for a given individualID. The `vst_apparentindividual` table contains one record per individualID per eventID, and includes growth form, structure and plant status data that may be linked to `vst_mappingandtagging` records via individualID; records may also be linked to `vst_perplotperyear` via the plotID and eventID fields in order to generate plot-level estimates of biomass and productivity. For vegetation structure data collected from tree palms, tree ferns, ferns, and other relatively large, perennial, non-woody individuals, users must download the related ‘Non-herbaceous perennial vegetation structure’ data product (DP1.10045.001). The `vst_shrubgroup` table contains a minimum of one record per groupID per plotID per eventID; multiple records with the same groupID may exist if a given shrub group is comprised of more than one taxonID. Data provided in the `vst_shrubgroup` table allow calculation of live and dead volume per taxonID within each shrub group, and records may be linked with `vst_perplotperyear` via the plotID and eventID fields.
 
For all tables, duplicates may exist where protocol and/or data entry aberrations have occurred; users should check data carefully for anomalies before joining tables. For the `vst_apparentindividual` table, the combination of the `eventID` x `individualID` x `tempStemID` fields should be unique. The `tempStemID` field is used to uniquely identify the stems within a multi-stem individual within a sampling event, but the identity of these stems is not tracked from year-to-year; individuals with a single stem are assigned a `tempStemID` of 1. Taxonomic IDs of species of concern have been 'fuzzed'; see data package readme files for more information.

Protection of species of concern: At most sites, taxonomic IDs of species of concern have been 'fuzzed', i.e., reported at a higher taxonomic rank than the raw data, to avoid publishing locations of sensitive species. For a few sites with stricter regulations (e.g., Great Smoky Mountains National Park (GRSM)), records for species of concern are not published. 

NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------

A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.10098.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.

DATA QUALITY AND VERSIONING
---------------------------

NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI.

To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.

POST STACKING README DOCUMENTATION
----------------------------------

Each row contains the readme filename used during stackByTable

NEON.D01.BART.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D01.BART.DP1.10098.001.readme.20210429T165211Z.txt
NEON.D01.HARV.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D01.HARV.DP1.10098.001.readme.20210429T164225Z.txt
NEON.D01.HARV.DP1.10098.001.readme.20210429T165250Z.txt
NEON.D01.HARV.DP1.10098.001.readme.20210429T164048Z.txt
NEON.D02.BLAN.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D02.BLAN.DP1.10098.001.readme.20210429T165941Z.txt
NEON.D02.SCBI.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D02.SCBI.DP1.10098.001.readme.20210429T170926Z.txt
NEON.D02.SCBI.DP1.10098.001.readme.20210504T165005Z.txt
NEON.D02.SCBI.DP1.10098.001.readme.20210602T165355Z.txt
NEON.D02.SERC.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D02.SERC.DP1.10098.001.readme.20210602T175716Z.txt
NEON.D03.DSNY.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D03.JERC.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D03.JERC.DP1.10098.001.readme.20210429T164209Z.txt
NEON.D03.JERC.DP1.10098.001.readme.20210804T160134Z.txt
NEON.D03.JERC.DP1.10098.001.readme.20210830T180451Z.txt
NEON.D03.JERC.DP1.10098.001.readme.20211004T165636Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20210429T170049Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20210429T164537Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20210804T151620Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20210830T172000Z.txt
NEON.D03.OSBS.DP1.10098.001.readme.20211004T181610Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210429T165624Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210429T163956Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210429T165909Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210804T175340Z.txt
NEON.D04.GUAN.DP1.10098.001.readme.20210830T164609Z.txt
NEON.D04.LAJA.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D05.STEI.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D05.STEI.DP1.10098.001.readme.20210429T164542Z.txt
NEON.D05.STEI.DP1.10098.001.readme.20211116T145101Z.txt
NEON.D05.TREE.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D05.TREE.DP1.10098.001.readme.20210429T164410Z.txt
NEON.D05.TREE.DP1.10098.001.readme.20210429T164724Z.txt
NEON.D05.TREE.DP1.10098.001.readme.20210510T143547Z.txt
NEON.D05.UNDE.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D05.UNDE.DP1.10098.001.readme.20210429T170329Z.txt
NEON.D05.UNDE.DP1.10098.001.readme.20210504T180407Z.txt
NEON.D06.KONZ.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D06.KONZ.DP1.10098.001.readme.20210429T165523Z.txt
NEON.D06.KONZ.DP1.10098.001.readme.20210504T162603Z.txt
NEON.D06.KONZ.DP1.10098.001.readme.20210602T165149Z.txt
NEON.D06.UKFS.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D06.UKFS.DP1.10098.001.readme.20210602T154027Z.txt
NEON.D07.GRSM.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D07.GRSM.DP1.10098.001.readme.20210429T164255Z.txt
NEON.D07.GRSM.DP1.10098.001.readme.20210602T161606Z.txt
NEON.D07.GRSM.DP1.10098.001.readme.20211004T184629Z.txt
NEON.D07.GRSM.DP1.10098.001.readme.20211102T184704Z.txt
NEON.D07.MLBS.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D07.MLBS.DP1.10098.001.readme.20211004T172624Z.txt
NEON.D07.MLBS.DP1.10098.001.readme.20211102T180746Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20210429T165109Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20210429T165949Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20210706T215401Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20210830T164144Z.txt
NEON.D07.ORNL.DP1.10098.001.readme.20211004T182505Z.txt
NEON.D08.DELA.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D08.DELA.DP1.10098.001.readme.20210504T165201Z.txt
NEON.D08.DELA.DP1.10098.001.readme.20210706T214223Z.txt
NEON.D08.LENO.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D08.TALL.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D08.TALL.DP1.10098.001.readme.20210504T181244Z.txt
NEON.D08.TALL.DP1.10098.001.readme.20210804T152610Z.txt
NEON.D09.DCFS.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D09.NOGP.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D09.WOOD.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D10.CPER.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D10.RMNP.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D10.RMNP.DP1.10098.001.readme.20210429T165930Z.txt
NEON.D10.RMNP.DP1.10098.001.readme.20210510T142256Z.txt
NEON.D11.CLBJ.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D11.CLBJ.DP1.10098.001.readme.20210429T170419Z.txt
NEON.D11.CLBJ.DP1.10098.001.readme.20210706T213603Z.txt
NEON.D11.CLBJ.DP1.10098.001.readme.20211004T181515Z.txt
NEON.D12.YELL.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D12.YELL.DP1.10098.001.readme.20210510T144856Z.txt
NEON.D12.YELL.DP1.10098.001.readme.20211208T145815Z.txt
NEON.D13.MOAB.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D13.NIWO.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D13.NIWO.DP1.10098.001.readme.20210429T170132Z.txt
NEON.D13.NIWO.DP1.10098.001.readme.20210504T173633Z.txt
NEON.D14.JORN.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D14.JORN.DP1.10098.001.readme.20211004T190106Z.txt
NEON.D14.SRER.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D14.SRER.DP1.10098.001.readme.20211102T184219Z.txt
NEON.D14.SRER.DP1.10098.001.readme.20211129T154634Z.txt
NEON.D15.ONAQ.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D16.ABBY.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D16.WREF.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D16.WREF.DP1.10098.001.readme.20210429T170454Z.txt
NEON.D16.WREF.DP1.10098.001.readme.20211102T185828Z.txt
NEON.D17.SJER.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D17.SOAP.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D17.SOAP.DP1.10098.001.readme.20210429T165739Z.txt
NEON.D17.SOAP.DP1.10098.001.readme.20210429T171120Z.txt
NEON.D17.TEAK.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D19.BONA.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D19.BONA.DP1.10098.001.readme.20210429T164606Z.txt
NEON.D19.BONA.DP1.10098.001.readme.20210429T170522Z.txt
NEON.D19.BONA.DP1.10098.001.readme.20210429T164422Z.txt
NEON.D19.BONA.DP1.10098.001.readme.20210510T143430Z.txt
NEON.D19.DEJU.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D19.DEJU.DP1.10098.001.readme.20210429T170029Z.txt
NEON.D19.HEAL.DP1.10098.001.readme.20210123T023002Z.txt
NEON.D19.HEAL.DP1.10098.001.readme.20210429T170607Z.txt
NEON.D19.HEAL.DP1.10098.001.readme.20210429T170518Z.txt
NEON.D19.HEAL.DP1.10098.001.readme.20210429T164516Z.txt
NEON.D19.HEAL.DP1.10098.001.readme.20211129T154024Z.txt
NEON.D20.PUUM.DP1.10098.001.readme.20210123T023002Z.txt
