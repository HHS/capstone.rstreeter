####  HHS CoLab Capstone Project
#       Last update:  July 29, 2018
#       Prepared by:  hrsa/bhw/nchwa, rstreeter (rstreeter@hrsa.gov)

####  Background
#       Objective:  Assess current location for CHGME graduates
#       Data:  Create .csv in SAS 
#         SAS required because of large size of source data
#       Steps performed in R:
#           Clean zipcodes
#           Link to county
#           By county fips code, link to selected data from ahrf
#           Describe counties where CHGME graduates practice
#           Compare to US counties overall (median income, US)
#           Illustrate with selected maps
#           Evaluate factors influencing counts (log-linear regression)


# Resource for large data:
# https://www.datacamp.com/community/tutorials/importing-data-r-part-two

# Resource for contiguous counties
# https://www.census.gov/geo/reference/county-adjacency.html


####  cleaning zip codes

setwd("C:/Users/rstreeter/Documents/hhsCoLab" )

library(zipcode)
library(tidyverse)
library(noncensus)

npi <- read.csv (file = "link.csv" , header = TRUE , stringsAsFactors = FALSE )

npi$zip_clean <- clean.zipcodes(npi$VAR33)

npi$zip_five <- substr(npi$zip_clean, 1, 5)

head(npi$zip_five)

data("zip_codes")
data("counties")

# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
# http://stat545.com/bit001_dplyr-cheatsheet.html

str(zip_codes)
str(npi)

npi_fips <- left_join(npi, zip_codes , by = c("zip_five" = "zip") )

str(npi_fips)

npi_fips_cts <- npi_fips %>%
  group_by(fips) %>%
  summarise(count=n())

head(npi_fips_cts)
str(npi_fips_cts)

####  Import data from AHRF

ahrf <- read.csv("ahrf_07292018.csv" , header = TRUE , stringsAsFactors = FALSE)
str(ahrf)
ahrf$fips <- as.numeric(ahrf$fips)

#### Link AHRF and npi_fips_count by fips codes

npi_ahrf <- left_join(ahrf, npi_fips_cts , by = c("fips" = "fips") )

str(npi_ahrf)
head(npi_ahrf)

####  Let's look at counties with CHGME graduates
# https://dplyr.tidyverse.org/

npi_ahrf_chgme <- npi_ahrf %>%
  select( abbrev , count , peds_gen_tot_2015 ) %>% 
  filter (count >= 0 , peds_gen_tot_2015 >= 0 ) %>%
  group_by(abbrev) %>%
  summarise(total = sum(count), peds = sum(peds_gen_tot_2015)) %>%
  
  arrange(desc (total))
  

head(npi_ahrf_chgme)

####  clean up the counties

str(npi_ahrf)

table(npi_ahrf$pchpsa_2015 , npi_ahrf$region)


npi_ahrf_2015 <- npi_ahrf[npi_ahrf$pchpsa_2015 >= 0 & npi_ahrf$region != "" , ]

table (npi_ahrf_2015$pchpsa_2015 , npi_ahrf_2015$region)

161+245+649
34+183
230+444+749
19+115+314

str(npi_ahrf_2015)

table(npi_ahrf_2015$state)

table(npi_ahrf_2015$pchpsa_2015 , npi_ahrf_2015$mhhpsa_2015)

npi_ahrf_2015c <- npi_ahrf_2015[complete.cases(npi_ahrf_2015 [ ,12 ]) , ]
                               
str(npi_ahrf_2015c)

####  Let's make maps
##       Start with primary care shortage areas to show limitation of using HPSAs

library(dplyr)
library(readr)
library(reshape)

library(ggplot2)
library(mapproj)
library(maps)

# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

png("pcsa.png" , width = 3600, height = 3000, res = 400) # 3600/400 -> (8,10)

# increase height to help move legend down
# create color buckets and add colorBuckets column to data to be mapped


colors = c( "gray85" , "steelblue4" , "darkmagenta")


npi_ahrf_2015c$colorBuckets <- as.numeric(cut(npi_ahrf_2015c$pchpsa_2015  , c(-1 , 0 , 1 , 2 ), right = TRUE))

leg.txt <- c("Not an SA County" , "Whole SA County" , "Partial SA County" )

# align data with map definitions by (partial) matching county names (names include multiple polygons for some states)
co.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
colorsmatched <- npi_ahrf_2015c$colorBuckets [match(co.fips, npi_ahrf_2015c$fips)]  
# note that map shows 48 contiguous states only  

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("county", col = "lightgoldenrod1", fill = FALSE, add = TRUE, lty = 1, lwd = 0.6, projection="polyconic")
map("state", col = "lightgoldenrod2", fill = FALSE, add = TRUE, lty = 1, lwd = 2.0, projection="polyconic")
title(adj = 0, line = 1, main = "Primary Care Provider Shortage Area (SA) Counties" , 
      sub = "HPSA:  HRSA-designated Health Professional Shortage Area (primary care)" , cex.main = 0.9, cex.sub = 0.6)	
legend(title = "Shortage Area Indicator (whole or partial county HPSA)" , 
       "top", leg.txt, horiz = TRUE, fill = colors, cex = 0.7)

dev.off()
graphics.off()

####  Map where chgme participants are

summary(npi_ahrf_2015c$count)

##  of counties with >= 1 chgme participants
##  min = 1, max = 820, q1 = 1, med = 2, mean = 20, q3 = 7
## library(norm)  #  ways to deal with NAs and missing values

npi_ahrf_2015c$x <- npi_ahrf_2015c$count %>% replace_na(0) 

summary(npi_ahrf_2015c$x)

png("chgme_partic.png" , width = 3600, height = 3000, res = 400) # 3600/400 -> (8,10)

# increase height to help move legend down
# create color buckets and add colorBuckets column to data to be mapped


colors = c( "gray85" , "steelblue4" , "darkmagenta")


npi_ahrf_2015c$colorBuckets <- as.numeric(cut(npi_ahrf_2015c$x  , c(-1 , 0 , 7 , 1000 ), right = TRUE))

leg.txt <- c("None" , "1 to 7" , "Over 7" )

# align data with map definitions by (partial) matching county names (names include multiple polygons for some states)
co.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
colorsmatched <- npi_ahrf_2015c$colorBuckets [match(co.fips, npi_ahrf_2015c$fips)]  
# note that map shows 48 contiguous states only  

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("county", col = "lightgoldenrod1", fill = FALSE, add = TRUE, lty = 1, lwd = 0.6, projection="polyconic")
map("state", col = "lightgoldenrod2", fill = FALSE, add = TRUE, lty = 1, lwd = 2.0, projection="polyconic")
title(adj = 0, line = 1, main = "Number of CHGME Participants, by County" , 
      sub = "2015" , cex.main = 0.9, cex.sub = 0.6)	
legend(title = "CHGME Participants" , 
       "top", leg.txt, horiz = TRUE, fill = colors, cex = 0.7)

dev.off()
graphics.off()

####  examine income

##  median income, US, 2015 = 56516
##  https://www.census.gov/library/publications/2016/demo/p60-256.html

summary(npi_ahrf_2015c$median_hh_income_2015)

npi_ahrf_2015c$med_inc_indic <- ifelse(npi_ahrf_2015c$median_hh_income_2015 >= 56516, 0, 1)

table( npi_ahrf_2015c$med_inc_indic )

##  note that two counties do not have a median household income reported

####  prelim cross tabs

##      create a smaller dataframe, with only the counties having chgme participants

npi_ahrf_2015c_small <- npi_ahrf_2015c[npi_ahrf_2015c$count >=1 , ]

table(npi_ahrf_2015c_small$pchpsa_2015, npi_ahrf_2015c_small$low_edu_code_2015)

table(npi_ahrf_2015c_small$pchpsa_2015, npi_ahrf_2015c_small$med_inc_indic)

table(npi_ahrf_2015c_small$pchpsa_2015, npi_ahrf_2015c_small$ruca)


#### Prelim regressions
##  need to ensure that any "old" county entities have been removed






