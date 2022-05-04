# NEON API Tutorial

# Load the necessary libraries
library(httr)
library(jsonlite)

# Request data using the GET function & the API call Test getting a product with the woody veg data
#req <- GET("http://data.neonscience.org/api/v0/products/DP1.10098.001")
#req

locSites <- GET("https://data.neonscience.org/api/v0/locations/sites")
harv <- GET("https://data.neonscience.org/api/v0/locations/HARV")

## Response [https://data.neonscience.org/api/v0/products/DP1.10098.001]
##   Date: 2021-06-16 01:03
##   Status: 200
##   Content-Type: application/json;charset=UTF-8
##   Size: 70.1 kB

# Make the data readable by jsonlite
#req.text <- content(req, as="text")
locs <- content(locSites, as="text")
harvTxt <- content(harv, as="text")

# Flatten json into a nested list
avail <- jsonlite::fromJSON(locs, 
                            simplifyDataFrame=T, 
                            flatten=T)
harvTab <- jsonlite::fromJSON(harvTxt, simplifyDataFrame = T, flatten = T)

# View description of data product
#avail$data$productDescription
names(avail$data)
str(harvTab)

# Look at the first list element for siteCode
#avail$data$siteCodes$siteCode[[1]]
avail$data$siteCode[[29]] # HARV = 29

# Get complete list of available data URLs
#wood.urls <- unlist(avail$data$siteCodes$availableDataUrls)
urls <- unlist(harvTab$data$locationChildrenUrls)

# Total number of URLs
#length(wood.urls)
length(urls)

## [1] 535

# Show first 10 URLs available
#wood.urls[1:10] 
urls[1:10]