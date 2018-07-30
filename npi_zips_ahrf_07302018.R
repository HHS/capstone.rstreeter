####  HHS CoLab Capstone Project
#       Last update:  July 30, 2018
#       Prepared by:  hrsa/bhw/nchwa, rstreeter (rstreeter@hrsa.gov)

####  Background
#       Objective:  Examine current location for CHGME participants
#       Data:  Create .csv files with npi and ahrf county-level data in sas 
#         (sas used due to size of underlying source files from CMS and HRSA)
#       Analysis performed in R:
#           Set the stage (set working directory, load libraries)
#           Clean NPI provider location zipcodes 
#           Link cleaned zip codes to county
#           By county fips code, link to selected data from ahrf
#           Describe counties where CHGME graduates practice
#           Compare to US counties overall (e.g., median national-level household income)
#           Illustrate with selected maps
#           Evaluate factors influencing counts (log-linear regression, classification)
#           Explore ways to create service areas, using contiguous census tracts

# resource for large data:
# https://www.datacamp.com/community/tutorials/importing-data-r-part-two

# resource for contiguous counties
# https://www.census.gov/geo/reference/county-adjacency.html

####  set the stage

setwd("C:/Users/rstreeter/Documents/hhsCoLab" )

library(zipcode)
library(tidyverse)
library(noncensus)

####  clean npi provider location zip codes

npi <- read.csv (file = "link.csv" , header = TRUE , stringsAsFactors = FALSE )

npi$zip_clean <- clean.zipcodes(npi$VAR33)

npi$zip_five <- substr(npi$zip_clean, 1, 5)

head(npi$zip_five)

data("zip_codes")
data("counties")

# resources
# https://stackoverflow.com/questions/13316185/r-convert-zipcode-or-lat-long-to-county
# http://stat545.com/bit001_dplyr-cheatsheet.html

str(zip_codes)
str(npi)

npi_fips <- left_join(npi, zip_codes , by = c("zip_five" = "zip") )

str(npi_fips)

npi_fips_cts <- npi_fips %>%
  group_by(fips) %>%
  summarise(count=n())

head(npi_fips_cts, 20)
str(npi_fips_cts)

####  import dataset with selected variables from AHRF (.csv created in sas)

ahrf <- read.csv("ahrf_07292018.csv" , header = TRUE , stringsAsFactors = FALSE)
ahrf$fips <- as.numeric(ahrf$fips)

#### Link AHRF and npi_fips_count by fips codes

npi_ahrf <- left_join(ahrf, npi_fips_cts , by = c("fips" = "fips") )

str(npi_ahrf)

head(npi_ahrf, 10)

####  Let's look at counties with CHGME participants

npi_ahrf_chgme <- npi_ahrf %>%
  select( abbrev , count , peds_gen_tot_2015 ) %>% 
  filter (count >= 0 , peds_gen_tot_2015 >= 0 ) %>%
  group_by(abbrev) %>%
  summarise(total = sum(count), peds = sum(peds_gen_tot_2015)) %>%
  
  arrange(desc (total))
  
head(npi_ahrf_chgme, 15)

tail(npi_ahrf_chgme, 15)

##  note that chgme participants' practice location includes sites in PR

####  clean up counties (remove territories, historic county designations)

str(npi_ahrf)

table(npi_ahrf$pchpsa_2015 , npi_ahrf$region)

npi_ahrf_2015 <- npi_ahrf[npi_ahrf$pchpsa_2015 >= 0 & npi_ahrf$region != "" , ]

table (npi_ahrf_2015$pchpsa_2015 , npi_ahrf_2015$region)

table(npi_ahrf_2015$state)

table(npi_ahrf_2015$pchpsa_2015 , npi_ahrf_2015$mhhpsa_2015)

# use pchpsa_2015 variable as proxy for valid counties in 2015
npi_ahrf_2015c <- npi_ahrf_2015[complete.cases(npi_ahrf_2015 [ ,12 ]) , ]
                               
str(npi_ahrf_2015c)

####  Let's make maps
##       Start with primary care shortage areas to show limitation of using HPSAs

## library(dplyr)
library(readr)
library(reshape)

##  library(ggplot2)
library(mapproj)
library(maps)

# resource for color selection
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

png("pcsa.png" , width = 3600, height = 3000, res = 400) # 3600/400 -> (8,10)

# increase height to help move legend down
# create color buckets and add colorBuckets column to data to be mapped

colors = c( "lavenderblush" , "steelblue4" , "darkmagenta")

npi_ahrf_2015c$colorBuckets <- as.numeric(cut(npi_ahrf_2015c$pchpsa_2015  , c(-1 , 0 , 1 , 2 ), right = TRUE))

leg.txt <- c("Not an SA County" , "Whole SA County" , "Partial SA County" )

# align data with map definitions by (partial) matching county names (names include multiple polygons for some states)
co.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
colorsmatched <- npi_ahrf_2015c$colorBuckets [match(co.fips, npi_ahrf_2015c$fips)]  
# note that map shows 48 contiguous states only  

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("county", col = "lightgoldenrod2", fill = FALSE, add = TRUE, lty = 1, lwd = 0.6, projection="polyconic")
map("state", col = "lightgoldenrod1", fill = FALSE, add = TRUE, lty = 1, lwd = 2.0, projection="polyconic")
title(adj = 0, line = 1, main = "Primary Care Provider Shortage Area (SA) Counties" , 
      sub = "HPSA:  HRSA-designated Health Professional Shortage Area (primary care)" , cex.main = 1.4, cex.sub = 0.6)	
legend(title = "Shortage Area Indicator (whole or partial county HPSA)" , 
       "top", leg.txt, horiz = TRUE, fill = colors, cex = 0.7)

dev.off()
graphics.off()

####  Map where chgme participants are

summary(npi_ahrf_2015c$count)

##  of counties with >= 1 chgme participants
##  min = 1, max = 820, q1 = 1, med = 2, mean = 20, q3 = 7
##  use cut points of -1, 0, 7, 1000 to create buckets (7 is q3 of counties, excluding null counties )
##  replace na with 0 to create buckets
##  library(norm)  #  more ways to deal with NAs and missing values

npi_ahrf_2015c$x <- npi_ahrf_2015c$count %>% replace_na(0) 

summary(npi_ahrf_2015c$x)

png("chgme_partic.png" , width = 3600, height = 3000, res = 400) # 3600/400 -> (8,10)

# increase height to help move legend down
# create color buckets and add colorBuckets column to data to be mapped


colors = c( "lavenderblush" , "steelblue4" , "darkmagenta")

npi_ahrf_2015c$colorBuckets <- as.numeric(cut(npi_ahrf_2015c$x  , c(-1 , 0 , 7 , 1000 ), right = TRUE))

leg.txt <- c("None" , "1 to 7" , "Over 7" )

# align data with map definitions by (partial) matching county names (names include multiple polygons for some states)
co.fips <- county.fips$fips[match(map("county", plot=FALSE)$names, county.fips$polyname)]
colorsmatched <- npi_ahrf_2015c$colorBuckets [match(co.fips, npi_ahrf_2015c$fips)]  
# note that map shows 48 contiguous states only  

# draw map
map("county", col = colors[colorsmatched], fill = TRUE, resolution = 0,
    lty = 0, projection = "polyconic")
map("county", col = "lightgoldenrod2", fill = FALSE, add = TRUE, lty = 1, lwd = 0.6, projection="polyconic")
map("state", col = "lightgoldenrod1", fill = FALSE, add = TRUE, lty = 1, lwd = 2.0, projection="polyconic")
title(adj = 0, line = 1, main = "Number of CHGME Participants, by County" , 
      sub = "2016-2017" , cex.main = 1.4, cex.sub = 0.6)	
legend(title = "CHGME Participants" , 
       "top", leg.txt, horiz = TRUE, fill = colors, cex = 0.7)

dev.off()
graphics.off()


####  prelim summaries for vulnerabilities:  
##           economic, education, rural, diversity, provider:population ratios, combined  

table(npi_ahrf_2015c$count >= 1 )  # 2015 counties with chgme participants = 519 (excl PR, other? )

##  examine income

##  median income, US, 2015 = 56516
##  https://www.census.gov/library/publications/2016/demo/p60-256.html

summary(npi_ahrf_2015c$median_hh_income_2015)
npi_ahrf_2015c$med_inc_indic <- ifelse(npi_ahrf_2015c$median_hh_income_2015 >= 56516, 0, 1)
table( npi_ahrf_2015c$med_inc_indic )
table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$med_inc_indic)

##  note that two counties do not have a median household income reported

##  other economic indicators (usda/ers low employment codes, snap recipients)

table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$low_empl_code_2015)

npi_ahrf_2015c$perc_snap <- (100 * npi_ahrf_2015c$snap_recip_2014 / npi_ahrf_2015c$pop_2015)
summary(npi_ahrf_2015c$perc_snap)
npi_ahrf_2015c$med_snap_indic <- ifelse(npi_ahrf_2015c$perc_snap >= 14, 1, 0)
table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$med_snap_indic)

##  education indicator (usda/ers)
table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$low_edu_code_2015)

##  rural indicator (usda/ers)
table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$ruca)

####  diversity

table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$perc_white_2010 < 50 ) 
# proxy for majority minority counties


# after July 30, 2018:  explore diversity indices from ecology literature, elsewhere


##  provider:population ratios
table(npi_ahrf_2015c$pchpsa_2015)

table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$pchpsa_2015)

summary(npi_ahrf_2015c$peds_gen_tot_2015)
summary(npi_ahrf_2015c$peds_gen_tot_pcnf_2015)
summary(npi_ahrf_2015c$medicaid_eligibles_children_2012)

npi_ahrf_2015c$peds_med_children <- 
  10000 * npi_ahrf_2015c$peds_gen_tot_pcnf_2015/npi_ahrf_2015c$medicaid_eligibles_children_2012

summary(npi_ahrf_2015c$peds_med_children)

npi_ahrf_2015c$prov_med_indic <- ifelse(npi_ahrf_2015c$peds_med_children < 3.3, 1 , 0)

table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$prov_med_indic )

##  combination

npi_ahrf_2015c$comb_indic <- ifelse ( npi_ahrf_2015c$med_inc_indic == 1 | 
                                      npi_ahrf_2015c$low_empl_code_2015 ==  1 |
                                      npi_ahrf_2015c$med_snap_indic == 1 |
                                      
                                      npi_ahrf_2015c$low_edu_code_2015 ==  1 |
                                      
                                      npi_ahrf_2015c$ruca == 7 |
                                      npi_ahrf_2015c$ruca == 8 |
                                      npi_ahrf_2015c$ruca == 9 |
                                      
                                      npi_ahrf_2015c$pchpsa_2015 == 1 |
                                      npi_ahrf_2015c$pchpsa_2015 == 2 |
                                      
                                      npi_ahrf_2015c$prov_med_indic == 1 , 1 , 0 )
                                    

table(npi_ahrf_2015c$count >= 1 , npi_ahrf_2015c$comb_indic)

#### Prelim loglinear regressions (counts, with population offset, provider offset)
##  need to ensure that any "old" county entities have been removed

    # after July 30, 2018

#### Clustering

    # after July 30, 2018

#### Classification 

    # after July 30, 2018

#### PCA

    # after July 30, 2018




