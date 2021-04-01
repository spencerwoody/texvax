##############################################################################
# Convert CDC SVI estimates from census tracts to ZIPs
# Requires HUD crosswalk using the percentage of residential address in ZIP
##############################################################################
library(tidyverse)

#### DATA USED FOR SVI CONVERSION ####
# convert between zip code and census tract as of 09-2020 # https://www.huduser.gov/portal/datasets/usps_crosswalk.html#codebook
ctract_zip_crosswalk = read_csv("SVI/ZIP_TRACT_092020.csv") 
#msa_crosswalk = subset(ctract_zip_crosswalk, ZIP %in% zips_arr_msa$ZIP)
ctract_svi_tx = read_csv("SVI/CDC_SVI_Texas_2018.csv") %>% # CDC SVI metrics for the state of Texas
  rename(TRACT = FIPS) # rename for join
ctract_zip_crosswalk_svi = ctract_zip_crosswalk %>% 
  left_join(ctract_svi_tx, by="TRACT") %>% # join SVI data to the ZIP to TRACT conversion table
  select(ZIP, TRACT, RES_RATIO, RPL_THEMES, everything())

# Calculate the weighted sum of SVI and it's percentage components for each ZIP with residential ratio in each TRACT
zip_svi_tx = ctract_zip_crosswalk_svi %>%
  group_by(ZIP) %>%
  filter(RPL_THEMES>=0) %>%
  summarise(ZIP_COVERED = sum(RES_RATIO),      # proportion of ZIP used in residential areas of Tract
            SVI = sum(RES_RATIO*RPL_THEMES),   # SVI
            POV = sum(RES_RATIO*EP_POV),       # percent below poverty line
            UNEMP = sum(RES_RATIO*EP_UNEMP),   # unemployment rate estimate
            PCI = sum(RES_RATIO*EP_PCI),       # per capita income
            NOHSDP = sum(RES_RATIO*EP_NOHSDP), # no high school diploma
            AGE65 = sum(RES_RATIO*EP_AGE65),   # percent over age 65
            AGE17 = sum(RES_RATIO*EP_AGE17),   # percent 17 and below
            DISABL = sum(RES_RATIO*EP_DISABL), # percent non-institutionalized disabled
            SNGPNT = sum(RES_RATIO*EP_SNGPNT), # percent single parent house hold children under 18
            MINRTY = sum(RES_RATIO*EP_MINRTY), # percent all minorities, excludes white+non-hispanic
            LIMENG = sum(RES_RATIO*EP_LIMENG), # percent greater than 5 with limited english
            MUNIT = sum(RES_RATIO*EP_MUNIT),   # percent housing structures with 10 or more units
            MOBILE = sum(RES_RATIO*EP_MOBILE), # percent mobile homes
            CROWD = sum(RES_RATIO*EP_CROWD),   # percent occupied houses with more people than rooms
            NOVEH = sum(RES_RATIO*EP_NOVEH),   # percent households no vehicle
            GROUPQ = sum(RES_RATIO*EP_GROUPQ), # percent in institutionalized group quarters
            RPL_THEME1 = sum(RES_RATIO*RPL_THEME1),
            RPL_THEME2 = sum(RES_RATIO*RPL_THEME2),
            RPL_THEME3 = sum(RES_RATIO*RPL_THEME3),
            RPL_THEME4 = sum(RES_RATIO*RPL_THEME4)
  ) %>%
  mutate(SVI=ifelse(ZIP_COVERED==0, NA, SVI)) # if no residential addresses the SVI isn't 0 just NA

# Crosswalk used is for ZIP codes and not ZCTAs
# Some ZIPs listed we could get SVI but they don't have polygon geometry of a ZCTA
# Produces 2420 ZIP codes, but only about 1930 are listed in google search as "active"
write.csv(zip_svi_tx, "SVI/svi_per_zip_TX.csv", row.names = FALSE)

#########################################################################################################################
# Visual check of the ZIP codes produced
library(tigris)
census_api_key("3deb7c3e77d1747cf53071c077e276d05aa31407", install = TRUE, overwrite = TRUE) # Spencer Woody's API KEY

# Get ZCTA geometry, wasn't working for me in tidycensus, which is annoying
zcta_geom <- zctas(cb = TRUE) %>%
  select(ZCTA5CE10, geometry) %>%
  rename(ZIP = ZCTA5CE10)
zip_geom = subset(zcta_geom, ZIP %in% zip_svi_tx$ZIP) # Get only ZIPs listed in SVI ZIP dataframe
full_zip_df = zip_geom %>%
  left_join(zip_svi_tx, by="ZIP") # join ZIPs to the 2010 polygons of ZCTAs

# Plot to see what SVI looks like across the state
tx_svi_map = ggplot()+
  geom_sf(data=full_zip_df, mapping=aes(geometry=geometry, fill=SVI), 
        size = 0.05, color="black")+
  scale_fill_gradient2(low = "steelblue", mid = "lightgoldenrod1", high = "firebrick", 
                       na.value = "white", midpoint =0.5, labels=c(0, 0.25, 0.5, 0.75, 1))+
  theme_bw()



