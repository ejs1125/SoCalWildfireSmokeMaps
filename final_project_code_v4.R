###############################

#Evan Serge
#EPID 592 Final Project Code

###############################

#### Packages ####
library(tidyverse)
library(sf)
library(sp)
library(tigris)
library(tidycensus)
library(lubridate)
library(rgdal)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(osmdata)
library(rosm)

setwd("~/YourWorkingDirectory") #your working directory here

#### Geography and Smoke Exposure Setup ####
#SoCal CSA County FIPs
county_fip <- c('037', '059', '111', '071', '065')

#Get SoCal CSA tracts
socal_tracts <- tracts(state = '06',
                       county_fip,
                       cb = F,
                       year = 2019)

#remove water-only tracts
socal_tracts <- socal_tracts %>% filter(ALAND > 0)

#Get County Borders (the ones in tigiris include the water-area along the coasts)
#source (download data from here): https://gis.data.ca.gov/datasets/8713ced9b78a4abb97dc130a691a8695_0/explore?location=34.128216%2C-117.872003%2C9.32
socal_counties <-
  read_sf('cnty19_1.shp') %>% filter(COUNTY_FIP %in% county_fip) %>%
  filter(is.na(ISLAND))

# Load smokePM predictions on smoke days
# Download file below from Source: https://github.com/echolab-stanford/daily-10km-smokePM
preds <-
  readRDS("smokePM2pt5_predictions_daily_tract_20060101-20201231.rds")

#Load full set of dates - 2016-2020
dates <- seq.Date(ymd("20160101"), ymd("20201231"), by = "day")

#get full combination of tract-days
out <- expand.grid(GEOID = socal_tracts$GEOID, date = dates) %>%
  # Match smokePM predictions on smoke days to tract-days
  left_join(preds, by = c("GEOID", "date")) %>%
  # Predict 0 for remaining tract-days, which are non-smoke days
  mutate(smokePM_pred = replace_na(smokePM_pred, 0)) %>%
  #Get county names
  inner_join(socal_tracts, by = 'GEOID')

#remove from environment to save on memory
rm(preds)
gc()

#sum up smokePM by tract
for_maps <- out %>%
  group_by(GEOID, geometry, COUNTYFP) %>% summarize(sum_smoke = sum(smokePM_pred), n = n())

for_maps$avg_smoke_day <- for_maps$sum_smoke / for_maps$n

#### get ACS data ####

#verify ACS variables I want
acs5profile_colnames <- load_variables(2019, 'acs5/profile')
acs5names <- load_variables(2019, 'acs5')

#declare variable names (I got more than I needed here in case I expand on this project later on)
my_acs_var <- c(
  'total_hh' = 'DP02_0001',
  'edu_total_pop_a25_over' = 'DP02_0059',
  'edu_bach_deg_a25_over' = 'DP02_0068',
  'frgn_total_pop' = 'DP02_0087',
  'frgn_frgnborn_non_citizen' = 'DP02_0096',
  'pov_totalhh_inc_bens' = 'DP03_0051',
  'pov_med_inc_bens_hh' = 'DP03_0062',
  'pov_cash_pub_ast_hhi' = 'DP03_0072',
  'pov_foodstamp_SNAP_hhi' = 'DP03_0074',
  'gend_age_race_total_pop' = 'DP05_0001',
  'gend_total_male_pop' = 'DP05_0002',
  'age_median_age' = 'DP05_0018',
  'age_total_pop_under_18' = 'DP05_0019',
  'age_total_pop_65_over' = 'DP05_0024',
  'race_white_pop' = 'DP05_0077',
  'race_black_pop' = 'DP05_0078',
  'race_AIAN_pop' = 'DP05_0079',
  'race_asian_pop' = 'DP05_0080',
  'race_two_more_races_pop' = 'DP05_0082',
  'race_HIPI_pop' = 'DP05_0081',
  'race_other_race_pop' = 'DP05_0085',
  'hisp_total_pop' = 'DP05_0070',
  'hisp_pop' = 'DP05_0071',
  'pov2_total_hh_inc_to_pov' = 'C17002_001',
  'pov2_inc_to_pov_less_than_0.5' = 'C17002_002',
  'pov2_inc_to_pov_0.5_0.99' = 'C17002_003',
  'pov2_inc_to_pov_1_1.24' = 'C17002_004',
  'pov2_inc_to_pov_1.25_1.49' = 'C17002_005',
  'pov2_inc_to_pov_1.5_1.84' = 'C17002_006',
  'pov2_inc_to_pov_1.85_1.99' = 'C17002_007'
)

#get data
ACS_raw <- get_acs(
  geography = 'tract',
  variables = my_acs_var,
  year = 2019,
  output = 'wide',
  state = 'California',
  county = county_fip,
  geometry = F,
  #key = CENSUS_API_KEY,
  survey = 'acs5',
  moe_level = 95
)

#create custom variables and get relevant demographic variables

ACS_raw$HH_less_than_pov <-
  ACS_raw$pov2_inc_to_pov_less_than_0.5E + ACS_raw$pov2_inc_to_pov_0.5_0.99E

ACS_refined <- ACS_raw %>% select(
  GEOID,
  gend_age_race_total_popE,
  gend_age_race_total_popM,
  HH_less_than_pov,
  pov2_inc_to_pov_less_than_0.5M,
  pov2_inc_to_pov_0.5_0.99M,
  pov2_total_hh_inc_to_povE,
  pov2_total_hh_inc_to_povM,
  hisp_popE,
  hisp_popM,
  race_black_popE,
  race_black_popM,
  race_asian_popE,
  race_asian_popM,
  race_white_popE,
  race_white_popM,
  race_AIAN_popE,
  race_AIAN_popM,
  race_two_more_races_popE,
  race_two_more_races_popM,
  race_HIPI_popE,
  race_HIPI_popM,
  race_other_race_popE,
  race_other_race_popM,
  pov_med_inc_bens_hhE,
  pov_med_inc_bens_hhM
)
#Calculate percentages
ACS_refined$HH_less_than_pov_P <-
  (ACS_refined$HH_less_than_pov / ACS_refined$pov2_total_hh_inc_to_povE) *
  100
ACS_refined$hisp_p <-
  (ACS_refined$hisp_popE / ACS_refined$gend_age_race_total_popE) * 100
ACS_refined$black_p <-
  (ACS_refined$race_black_popE / ACS_refined$gend_age_race_total_popE) *
  100
ACS_refined$asian_p <-
  (ACS_refined$race_asian_popE / ACS_refined$gend_age_race_total_popE) *
  100
ACS_refined$white_p <-
  (ACS_refined$race_white_popE / ACS_refined$gend_age_race_total_popE) *
  100
ACS_refined$AIAN_p <-
  (ACS_refined$race_AIAN_popE / ACS_refined$gend_age_race_total_popE) * 100
ACS_refined$two_more_races_p <-
  (ACS_refined$race_two_more_races_popE / ACS_refined$gend_age_race_total_popE) *
  100
ACS_refined$HIPI_p <-
  (ACS_refined$race_HIPI_popE / ACS_refined$gend_age_race_total_popE) * 100
ACS_refined$other_race_p <-
  (ACS_refined$race_other_race_popE / ACS_refined$gend_age_race_total_popE) *
  100

#### Making Maps ####

#In one data table
for_maps <- for_maps %>% left_join(ACS_refined, by = 'GEOID')

#convert to sf & align projections between tract map and county borders
for_maps <- st_as_sf(for_maps)
socal_counties <-
  st_as_sf(socal_counties) %>% st_transform(crs = st_crs(for_maps))

#background map for context
osm_socal <-
  read_osm(socal_tracts, type = 'apple-iphoto') %>% st_transform(crs = st_crs(for_maps))

#### Smoke by Tract Map ####

#move county labels around for clarity
socal_counties$xmod <- c(0.4, 0, 0, 0, 0.2)
#dummy values to trick tm_text bellow for label color
socal_counties$color <- c(1, 4, 5, 2, 2)

#taking out water portions in the shape
tmap_options(check.and.fix = T)
for_maps <-
  for_maps %>% erase_water(area_threshold = 0.95, year = 2019)

Smoke_SoCal <- tm_shape(osm_socal) + tm_rgb() + tm_shape(for_maps) +
  tm_polygons(
    col = 'avg_smoke_day',
    palette = 'RdPu',
    alpha = 0.9,
    border.alpha = 0,
    breaks = quantile(for_maps$avg_smoke_day),
    colorNA = 'grey',
    legend.format = list(digits = 2),
    title = expression(paste('Wildfire PM2.5 ug/m' ^ '3'))
  ) +
  tm_shape(socal_counties) +
  tm_borders(col = 'black') +
  tm_fill(
    'color',
    palette = c('gray21', 'gray21', 'gray21', 'gray', 'gray'),
    legend.show = F,
    alpha = 0
  ) +
  tm_text(
    'COUNTY_NAM',
    size = 0.75,
    fontface = 'bold',
    xmod = socal_counties$xmod
  ) +
  tm_credits(
    'Source data from Childs et al (2022)',
    bg.color = 'white',
    bg.alpha = 0.9,
    fontface = 'italic'
  ) +
  tm_layout(
    main.title = 'Avg Daily Wildfire PM2.5 by Census Tract, 2016-2020',
    main.title.fontface = 'bold',
    legend.frame = 'black',
    legend.bg.color = 'grey92',
    legend.title.size = 1,
    legend.text.size = 0.8
  )


#### Hispanic by Tract Map ####

#Move labels around
socal_counties$xmod <- c(0.4, 0.5, 0.9, 0, 0.2)
socal_counties$ymod <- c(0.2,-0.3, 0, 0, 0.5)

Hisp_SoCal <- tm_shape(osm_socal) + tm_rgb() + tm_shape(for_maps) +
  tm_polygons(
    col = 'hisp_p',
    palette = 'Reds',
    alpha = 0.9,
    border.alpha = 0,
    breaks = quantile(for_maps$hisp_p, na.rm = T),
    colorNA = 'grey',
    legend.format = list(
      fun = function(x)
        paste0(formatC(x, digits = 0, format = 'f'), '%')
    ),
    title = 'Hispanic Percent'
  ) +
  tm_shape(socal_counties) +
  tm_borders(col = 'black') +
  tm_text(
    'COUNTY_NAM',
    size = 0.75,
    fontface = 'bold',
    xmod = socal_counties$xmod,
    ymod = socal_counties$ymod,
    bg.color = 'white',
    bg.alpha = 0.5,
    just = 'top'
  ) +
  tm_credits(
    'Source data from American Community Survey',
    bg.color = 'white',
    bg.alpha = 0.9,
    just = 'top',
    fontface = 'italic'
  ) +
  tm_layout(
    main.title = '% Hispanic by Census Tract 2015-2019',
    main.title.fontface = 'bold',
    legend.frame = 'black',
    legend.bg.color = 'grey92',
    legend.title.size = 1,
    legend.text.size = 0.8
  )

#### Income by Tract Map ####

Inc_SoCal <- tm_shape(osm_socal) + tm_rgb() + tm_shape(for_maps) +
  tm_polygons(
    col = 'pov_med_inc_bens_hhE',
    palette = 'Greens',
    alpha = 0.9,
    border.alpha = 0,
    breaks = quantile(for_maps$pov_med_inc_bens_hhE, na.rm = T),
    colorNA = 'grey',
    legend.format = list(
      prefix = '$',
      text.separator = ' - ',
      digits = 0,
      big.mark = ','
    ),
    title = 'HH Income and Earnings'
  ) +
  tm_shape(socal_counties) +
  tm_borders(col = 'black') +
  tm_text(
    'COUNTY_NAM',
    size = 0.75,
    fontface = 'bold',
    xmod = socal_counties$xmod,
    ymod = socal_counties$ymod,
    bg.color = 'white',
    bg.alpha = 0.5,
    just = 'top'
  ) +
  tm_credits(
    'Source data from American Community Survey',
    bg.color = 'white',
    bg.alpha = 0.9,
    just = 'top',
    fontface = 'italic'
  ) +
  tm_layout(
    main.title = 'HH Income & Earnings by Census Tract 2015-2019',
    main.title.fontface = 'bold',
    legend.frame = 'black',
    legend.bg.color = 'grey92',
    legend.title.size = 1,
    legend.text.size = 0.8
  )

#### Export Maps ####
tmap_save(tm = Smoke_SoCal,
          filename = "smoke.tiff")

tmap_save(tm = Inc_SoCal,
          filename = "income.tiff")

tmap_save(tm = Hisp_SoCal,
          filename = "hispanic.tiff")

#### Data Summary ####

#assign quartile avg smoke values to tracts
for_maps <-
  within(
    for_maps,
    avg_smoke_quartile <-
      cut(
        avg_smoke_day,
        quantile(avg_smoke_day, probs = 0:4 / 4),
        include.lowest = T,
        labels = F
      )
  )

#join quartile values to ACS_refined table
ACS_refined <-
  for_maps %>% select(GEOID, avg_smoke_quartile) %>% right_join(ACS_refined, by =
                                                                  'GEOID')
ACS_refined <- st_drop_geometry(ACS_refined)

#estimates table
smoke_sum_table <-
  as_tibble(ACS_refined) %>% select(
    avg_smoke_quartile,
    gend_age_race_total_popE,
    hisp_popE,
    race_black_popE,
    race_asian_popE,
    HH_less_than_pov,
    race_white_popE,
    race_AIAN_popE,
    race_two_more_races_popE,
    race_HIPI_popE,
    race_other_race_popE,
    pov2_total_hh_inc_to_povE
  ) %>%
  group_by(avg_smoke_quartile) %>%
  summarize_all(sum, na.rm = T) %>%
  filter(!is.na(avg_smoke_quartile))

#helper function
z_sq <- function(z) {
  result <- z ^ 2
}

#Margin of Error Table
smoke_moe_table <-
  as_tibble(ACS_refined) %>% select(
    avg_smoke_quartile,
    gend_age_race_total_popM,
    hisp_popM,
    race_black_popM,
    race_asian_popM,
    pov2_inc_to_pov_less_than_0.5M,
    pov2_inc_to_pov_0.5_0.99M,
    race_white_popM,
    race_AIAN_popM,
    race_two_more_races_popM,
    race_HIPI_popM,
    race_other_race_popM,
    pov2_total_hh_inc_to_povM
  ) %>%
  transmute_all(funs(z_sq)) %>%
  group_by(avg_smoke_quartile) %>%
  summarize_all(sum, na.rm = T) %>%
  filter(!is.na(avg_smoke_quartile)) %>%
  sqrt()

smoke_moe_table$avg_smoke_quartile <- c(1, 2, 3, 4)

#Create table of derived percentage estimates
smoke_percentage_table <- smoke_sum_table %>%
  mutate(
    hispanic_percent = hisp_popE / gend_age_race_total_popE,
    black_percent = race_black_popE / gend_age_race_total_popE,
    asian_percent = race_asian_popE / gend_age_race_total_popE,
    white_percent = race_white_popE / gend_age_race_total_popE,
    AIAN_percent = race_AIAN_popE / gend_age_race_total_popE,
    tworaces_percent = race_two_more_races_popE / gend_age_race_total_popE,
    HIPI_percent = race_HIPI_popE / gend_age_race_total_popE,
    otherrace_percent = race_other_race_popE / gend_age_race_total_popE,
    HH_poverty_percent = HH_less_than_pov / pov2_total_hh_inc_to_povE
  ) %>%
  rename(total_pop = gend_age_race_total_popE) %>%
  select(-ends_with('E', ignore.case = F)) %>% select(-ends_with('pov'))

#weighted mean of median income
med_income_table <- ACS_refined %>%
  group_by(avg_smoke_quartile) %>%
  summarize(median_hh_income = weighted.mean(pov_med_inc_bens_hhE,
                                             pov2_total_hh_inc_to_povE,
                                             na.rm = T))

#putting together summary table
smoke_percentage_table <-
  smoke_percentage_table %>%
  left_join(med_income_table, by = 'avg_smoke_quartile')

#margins of error for summary table
smoke_percentage_table$total_pop_margin <-
  smoke_moe_table$gend_age_race_total_popM

smoke_percentage_table$hisp_margin <-
  moe_prop(
    smoke_sum_table$hisp_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$hisp_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$black_margin <-
  moe_prop(
    smoke_sum_table$race_black_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_black_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$asian_margin <-
  moe_prop(
    smoke_sum_table$race_asian_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_asian_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$white_margin <-
  moe_prop(
    smoke_sum_table$race_white_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_white_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$AIAN_margin <-
  moe_prop(
    smoke_sum_table$race_AIAN_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_AIAN_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$tworaces_margin <-
  moe_prop(
    smoke_sum_table$race_two_more_races_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_two_more_races_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$HIPI_margin <-
  moe_prop(
    smoke_sum_table$race_HIPI_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_HIPI_popM,
    smoke_moe_table$gend_age_race_total_popM
  )

smoke_percentage_table$otherrace_margin <-
  moe_prop(
    smoke_sum_table$race_other_race_popE,
    smoke_sum_table$gend_age_race_total_popE,
    smoke_moe_table$race_other_race_popM,
    smoke_moe_table$gend_age_race_total_popM
  )
#combine two poverty subgroup margins into one
smoke_moe_table$HH_poverty_margin <-
  ACS_refined %>% select(avg_smoke_quartile,
                         pov2_inc_to_pov_less_than_0.5M,
                         pov2_inc_to_pov_0.5_0.99M) %>%
  transmute(
    less_than_0.5_sq = pov2_inc_to_pov_less_than_0.5M ^ 2,
    half_to_one_sq = pov2_inc_to_pov_0.5_0.99M ^ 2,
    avg_smoke_quartile = avg_smoke_quartile
  ) %>%
  mutate(hh_under_pov_m = less_than_0.5_sq + half_to_one_sq) %>%
  select(-ends_with('sq')) %>%
  group_by(avg_smoke_quartile) %>%
  summarize_all(sum, na.rm = T) %>%
  transmute(avg_smoke_quartile = avg_smoke_quartile,
            hh_under_pov_m = sqrt(hh_under_pov_m)) %>%
  filter(!is.na(avg_smoke_quartile))

smoke_percentage_table$HH_poverty_margin <-
  moe_prop(
    smoke_sum_table$HH_less_than_pov,
    smoke_sum_table$pov2_total_hh_inc_to_povE,
    smoke_moe_table$HH_poverty_margin,
    smoke_moe_table$pov2_total_hh_inc_to_povM
  )


#transposing table
smoke_percentage_table$avg_smoke_quartile <-
  as.factor(smoke_percentage_table$avg_smoke_quartile)
smoke_percentage_table <-
  as.data.frame(t(smoke_percentage_table))

#### Export Data Table ####

write.csv(smoke_percentage_table, "results.csv")
