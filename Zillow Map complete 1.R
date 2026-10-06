
########################## Load the packages and data
library(tidyverse)

zori <- read_csv("Metro_zori_uc_mfr_sm_month.csv")
##########################

# Goal 1: convert to panel data

########################## check
glimpse(zori)
##########################

########################## Take multiple columns and stack them into rows. Convert data from wide format → long format.
zori_long <- zori %>%
  pivot_longer(
    cols = `2015-01-31`:`2026-08-31`,
    names_to = "Month",
    values_to = "ZORI"
  )
########################## Assign rent values to new veriable ZORI


########################## Take the values in Month and convert them into R’s date data type.
zori_long <- zori_long %>%
  mutate(Month = as.Date(Month))
##########################

########################## Check
glimpse(zori_long)
head(zori_long)
dim(zori_long)
nrow(zori_long)
##########################



########################## what geographic types Zillow included
table(zori_long$RegionType)
##########################

########################## basic distribution of rent values
summary(zori_long$ZORI)
##########################

########################## how many of those 89,320 metro-month slots don’t actually have an observed ZORI value
sum(is.na(zori_long$ZORI))
##########################


##########################
#Observations:

# 89,320 rows, consisting of:
# 140 country-month observations — the U.S. national series
# 89,180 MSA-month observations
# 51,584 missing ZORI values
# observations that actually contain ZORI values is about 42.2%
# Zillow appears to not have multifamily ZORI estimates for every metro going all the way back to 2015
# The mean is above the median which suggests a right-skewed distribution: expensive rental markets may pull the average upward.


#Goal 2: Check missingness and how many metros are represented
##########################

########################## Take Panel Zillow dataset, create a year variable from Month, separate the observations by year, for each year, count the total observations, count the nonmissing ZORI observations, count the missing ZORI observations, and calculate the percentage that are missing.
zori_long %>%
  mutate(Year = lubridate::year(Month)) %>%
  group_by(Year) %>%
  summarise(
    Total = n(),
    Observed = sum(!is.na(ZORI)),
    Missing = sum(is.na(ZORI)),
    Percent_Missing = round(mean(is.na(ZORI)) * 100, 1)
  )
########################## 

##########################
# geographic coverage expands substantially over the sample period
##########################


########################## remove national series count the number of MSAs with at least one observed ZORI value each year.
zori_long %>%
  filter(RegionType == "msa") %>%
  mutate(Year = lubridate::year(Month)) %>%
  filter(!is.na(ZORI)) %>%
  group_by(Year) %>%
  summarise(
    Metros = n_distinct(RegionID)
)
########################## Zillow’s multifamily ZORI coverage increases from 111 metros in 2015 to 637 metros in 2026


##########################
# Goal 3: What metros have ZORI data throughout the entire period?
##########################

########################## For each metro, count number of months in which ZORI is not missing. keep only metros that have observations in all 140 months.
consistent_metros <- zori_long %>%
  filter(RegionType == "msa") %>%
  group_by(RegionID, RegionName, SizeRank) %>%
  summarise(
    months_observed = sum(!is.na(ZORI)),
    .groups = "drop"
  ) %>%
  filter(months_observed == 140)
##########################

########################## check
nrow(consistent_metros)
View(consistent_metros)
##########################


##########################
# 91 metros from 2015–2026

# create sample with 91 metros
##########################

########################## Take the full long Zillow dataset, keeping only MSAs, and keeping only those MSAs whose RegionID appears in my list of 91 complete metros.
zori_balanced <- zori_long %>%
  filter(
    RegionType == "msa",
    RegionID %in% consistent_metros$RegionID
  )
##########################

########################## check
dim(zori_balanced)
sum(is.na(zori_balanced$ZORI))
##########################

##########################
# zori_balanced = 2015–2026 rent trend, with 91 metros that stay exactly the same.
##########################

########################## which metros are in the balanced sample
sort(unique(zori_balanced$RegionName))
##########################


########################## Pick metros of interest
selected_metros <- zori_balanced %>%
  filter(RegionName %in% c(
    "Chicago, IL",
    "New York, NY",
    "Los Angeles, CA",
    "	Minneapolis, MN",
    "Philadelphia, PA"
  ))
##########################

# Graph

########################## 
ggplot(selected_metros,
       aes(x = Month, y = ZORI, color = RegionName)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Multifamily Rent Trends in Selected U.S. Metropolitan Areas",
    subtitle = "January 2015–August 2026",
    x = NULL,
    y = "Typical Monthly Rent ($)",
    color = "Metro"
  ) +
  theme_minimal()
##########################

##########################
# New York is the most expensive of these four metros throughout the period and shows strong growth after 2021.
# Los Angeles is second, although its growth flattens after 2021–2022 increase.
# Chicago accelerates after 2021 and continues climbing through 2026.
# Philadelphia is smoother, lower trajectory, though it also accelerates after 2021.
# There is seasonality in the monthly series the repeated waves are very visible for New York and Chicago.
# The 2020–2021 movements are also very visible, mainly for New York as it decline followed by a sharp rebound.
##########################

##########################
# Goal 4: calculate growth
# percentage-growth measure makes metros comparable due to scale
##########################


########################## start with 2019, it gives us a pre-pandemic reference point and, unlike 2015, there are 200 metros around then
growth_2019 <- zori_balanced %>%
  group_by(RegionID, RegionName, StateName) %>%
  summarise(
    ZORI_2019 = ZORI[Month == as.Date("2019-08-31")],
    ZORI_2026 = ZORI[Month == as.Date("2026-08-31")],
    .groups = "drop"
  ) %>%
  mutate(
    Rent_Growth = ((ZORI_2026 / ZORI_2019) - 1) * 100
  )
########################## for each metro, find rent index in Aug 2019 and Aug 2026, calculate percentage change between two dates.

# Calculating (Aug 2026 Zori(i))/(Aug2019 Zori(i)) x 100 = Percentage change in Growth

########################## check what had the largest growth what had the smallest
summary(growth_2019$Rent_Growth)

growth_2019 %>%
  arrange(desc(Rent_Growth)) %>%
  select(RegionName, ZORI_2019, ZORI_2026, Rent_Growth) %>%
  head(10)

growth_2019 %>%
  arrange(Rent_Growth) %>%
  select(RegionName, ZORI_2019, ZORI_2026, Rent_Growth) %>%
  head(10)
########################## this is useful for mapping where multifamily rents grown the most since 2019? as opposed to just where are rents expensive?

##########################
# top growth metros include Providence (+62.8%), Fresno (+59.3%), Poughkeepsie (+59.0%), and New Haven (+58.8%). At the bottom, Austin increased only about 7.0%, followed by San Antonio (+9.3%), Denver (+13.3%), and Houston (+13.7%). there is a large amount of geographic variation in rent growth across the sample
# minimum, 6.97%, means the metro with the smallest rent increase experienced about 7% growth from August 2019 to August 2026 which is Austin.
# The max, 62.81%, is metro with the largest increase experienced about 62.8% growth which is Providence.
# median, 38.32%, half of the 91 metros experienced rent growth below 38.3%, and half experienced growth above 38.3
# mean, 37.09%, this is unweighted average across metros. the typical U.S. renter’s rent hasnt actually increased 37.1%, as New York and a smaller metro each will contribute one observation to the calculation
# middle 50% of metros experienced approximately 28.6%–44.8% rent growth
# for mapping check if some had 50–60% growth and if others had 10–25%. Could would suggest a geographic pattern.
##########################

##########################
# Map question: Are metros with high rent growth geographically located near other metros with high rent growth, and are low-growth metros located near other low-growth metros?
# Map time:
##########################

########################## load simple features and obtain geographic boundaries from U.S. Census Bureau TIGER/Line data from tigris
install.packages(c("sf", "tigris"))
library(sf)
library(tigris)
##########################

########################## Download metro boundaries
metro_shapes <- core_based_statistical_areas(
  year = 2024,
  cb = TRUE
)
##########################

##########################
#CBSAs include metropolitan and micropolitan statistical areas from 2024 using cartographic boundaries. boundaries are simplified for mapping, which makes it easier for the descriptive map.
##########################



########################## view info on metro shapes
metro_shapes
names(metro_shapes)
##########################

##########################
# Simple feature collection assigned metro_shapes 935 geographic areas. In spatial data, a feature is one geographic object. one feature = one Core Based Statistical Area.
# I have more than 91 because this file contains both metropolitan and micropolitan areas across the U.S. The Zillow balanced sample contains only 91 metros.
# GEOID — the Census identifier for the CBSA. Census’s version of an ID number.
# NAME — the geographic area’s name.
# NAMELSAD — a more complete name that also tells whether it’s a metro or micro area
# LSAD — says kind of statistical area it is. M1 identifies metropolitan statistical areas and M2 identifies micropolitan statistical areas.
# I already have RegionType == "msa" for Zillow metropolitan so drop micropolitan
# ALAND and AWATER represent land area and water area.
# geometry - set of coordinates telling R the physical boundary of a geographic area.
# dataset knows the shape, but not a state’s rent growth
# Goal 5: Clean metro_shapes and join with Zillow
##########################

########################## replacing metro_shapes with filtered version that removes M2 Micropolitan areas
metro_shapes <- metro_shapes %>%
  filter(LSAD == "M1")
nrow(metro_shapes)
########################## check number of rows left after filter: 935 -> 393

# metro_shapes contains 393 metro-area shapes, while growth_2019 has 91 Zillow metros.


########################## take NAME column from metro_shapes and put in alphabetical order and compare to ZORI
sort(metro_shapes$NAME)
sort(unique(zori_balanced$RegionName))
##########################

##########################
# Zillow data uses shorter names Census uses the official metropolitan area names
# Chicago, IL 
# Chicago-Naperville-Elgin, IL-IN
# cannot join on names directly using left_join(growth_2019, metro_shapes, by = c("RegionName" = "NAME"))
# R will determine "Chicago, IL" == "Chicago-Naperville-Elgin, IL-IN" is FALSE and not match
# create a crosswalk between the Zillow metro and the Census GEOID
##########################

#metro_shapes
#names(metro_shapes)

########################## create simplified names using the first city name from each dataset
zillow_names <- growth_2019 %>%
  mutate(
    MatchName = sub(",.*", "", RegionName)
  ) %>%
  select(RegionID, RegionName, MatchName)

census_names <- metro_shapes %>%
  st_drop_geometry() %>%
  mutate(
    MatchName = sub("-.*|,.*", "", NAME)
  ) %>%
  select(GEOID, NAME, MatchName)
########################## 

########################## 
# sub() replaces part of a text string.
# Find the first comma and everything after it, replace it with nothing
# Chicago, IL -> Chicago
# st_drop_geometry() = Temporarily remove the spatial geometry and treat like a normal dataframe
# sf object geometries are sticky as they persist through dplyr manipulations like select() or summarize()), its important before performing pure tabular operations or saving a clean dataset without spatial data.
# it strips the spatial attributes and sets the geometry column to NULL
# sub("-.*|,.*", "", NAME) removes everything beginning with either the first hyphen or comma
########################## 




########################## join and match by name
name_matches <- zillow_names %>%
  left_join(
   census_names,
   by = "MatchName"
)
########################## does not run because at least one name on the Zillow side matches more than one row on the Census side.




########################## count how many times each MatchName occurs. shortcut for grouping by MatchName and counting the rows.
zillow_names %>%
  count(MatchName) %>%
  filter(n > 1)

census_names %>%
  count(MatchName) %>%
  filter(n > 1)
##########################

##########################
# filter(n > 1)
# only show names appearing more than once.
# Zillow side, only duplicate simplified name is Portland because sample contained both Portland, Oregon and Portland, Maine
# Census side, many first-city names repeat across states. City join not precise enough
# join by city + state 
##########################


########################## New abbreviation
census_names <- metro_shapes %>%
  st_drop_geometry() %>%
  mutate(
    FirstCity = sub("-.*|,.*", "", NAME),
    State = sub(".*, ([A-Z]{2}).*", "\\1", NAME),
    MatchName = paste0(FirstCity, ", ", State)
  ) %>%
  select(GEOID, NAME, MatchName)
########################## 



##########################
# State = sub(".*, ([A-Z]{2}).*", "\\1", NAME)
# extracts the first state abbreviation after the comma
# Chicago-Naperville-Elgin, IL-IN -> IL
# paste0(FirstCity, ", ", State) 
# combines pieces of text
# FirstCity = "Chicago"
# State     = "IL"
# MatchName = "Chicago, IL"
##########################



########################## recreate zillow names and join
zillow_names <- growth_2019 %>%
  select(RegionID, RegionName) %>%
  mutate(MatchName = RegionName)

name_matches <- zillow_names %>%
  left_join(
    census_names,
    by = "MatchName"
  )
##########################


########################## check which of the 91 Zillow metros still didn’t match
name_matches %>%
  filter(is.na(GEOID)) %>%
  select(RegionName, MatchName)
##########################

##########################
# fix Louisville and Poughkeepsie
##########################

########################## locate census names
metro_shapes %>%
  filter(
    grepl("Louisville|Poughkeepsie", NAME)
  ) %>%
  select(GEOID, NAME)
########################## 

########################## 
# grepl("Louisville|Poughkeepsie", NAME)
# asks if this text contain a particular pattern
# grepl("Chicago", "Chicago-Naperville-Elgin, IL-IN")
# would return TRUE as "Chicago" is somewhere inside the longer name.
# | = or
# filter() keeps keeps Census rows where its true
# select(GEOID, NAME)
# shows only the Census ID and official name.
########################## 


########################## Manually match
manual_matches <- tibble(
  RegionName = c("Louisville, KY", "Poughkeepsie, NY"),
  GEOID = c("31140", "28880")
)
##########################

##########################
# tibble(), manually creates a small data table
# c("Louisville, KY", "Poughkeepsie, NY")
# Make a collection containing these two names.
# And the first name corresponds to the first GEOID, second name to second GEOID.
##########################


########################## take the automatic matches and remove the two that failed
auto_matches <- name_matches %>%
  filter(!is.na(GEOID)) %>%
  select(RegionName, GEOID)
########################## 

########################## 
# filter(!is.na(GEOID)) keep only rows where Census GEOID successfully found, select() keeps just the two columns needed.
########################## 

########################## combine auto matches with the two manual matches
metro_crosswalk <- bind_rows(
  auto_matches,
  manual_matches
)
########################## 

########################## 
# bind_rows() Stack one dataset underneath another dataset
# adds two rows underneath
# 89 + 2 = 91
########################## 





########################## verify if its all 91
nrow(metro_crosswalk)
n_distinct(metro_crosswalk$RegionName)
n_distinct(metro_crosswalk$GEOID)
########################## 

########################## 
# if # of rows = 91 it means 91 matches total
# if Unique RegionNames = 91 it means no Zillow metro accidentally appears twice
# Unique GEOIDs = 91 means I havent assigned two Zillow metros to the same Census metro.
########################## 

########################## Join growth and GEOID
growth_with_geoid <- growth_2019 %>%
  left_join(
    metro_crosswalk,
    by = "RegionName"
  )
########################## 



########################## verify
growth_with_geoid %>%
  select(RegionName, GEOID, Rent_Growth) %>%
  head(10)
##########################


########################## check if all 91 Zillow metros has a Census geographic identifier
sum(is.na(growth_with_geoid$GEOID))
##########################

##########################  connect rent-growth data to geographic shapes
growth_map <- metro_shapes %>%
  inner_join(
    growth_with_geoid,
    by = "GEOID"
  )
########################## 

########################## 
# inner_join()
# Keep only observations that exist in both datasets.
########################## 


########################## check
nrow(growth_map)
class(growth_map)
##########################


########################## simple map
ggplot(growth_map) +
  geom_sf(aes(fill = Rent_Growth))
##########################

##########################
# geom_sf()
# Draw the geographic shapes stored in the geometry column.
# growth_map is an sf object, R already knows that the geometry column contains the coordinates so no need for aes(x,y)
# aes(fill = Rent_Growth) fills each metro’s geographic shape according to its value of Rent_Growth
# polygon -> growth -> shading
##########################

########################## focus on the contiguous U.S.
ggplot(growth_map) +
  geom_sf(aes(fill = Rent_Growth)) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  )
##########################



##########################
#coord_sf(
#  xlim = c(-125, -66),
#  ylim = c(24, 50)
#)
# essentially changing the camera
# xlim controls longitude
# ylim controls latitude
# increasing x means moving east
# increasing y means moving north
# doesn’t delete anything from growth_map. focuses on which areas to display
# states out of view are still there

# Observation: rent-growth values arent identical across regions. 
# the Northeast has several lighter-shaded metros, while several Texas and western metros appear darker. matches top/bottom growth table. however not evidence of clustering.
##########################



########################## add state boundaries
state_shapes <- states(
  year = 2024,
  cb = TRUE
)
state_shapes
##########################


##########################
# states()
# downloads state boundaries instead of CBSA boundaries
# state polygons provide geographic context
# layering the map like State boundaries -> Metro boundaries -> Rent-growth shading
##########################

########################## put state outlines behind metro rent-growth polygons
ggplot() +
  geom_sf(
    data = state_shapes,
    fill = "white",
    color = "gray70"
  ) +
  geom_sf(
    data = growth_map,
    aes(fill = Rent_Growth)
  ) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  )
##########################

##########################
# use ggplot() instead of ggplot(growth_map)
# due to use of two different spatial datasets so tell R to layer
# geom_sf(
#data = state_shapes,
#fill = "white",
#color = "gray70"
#)
# Tells R Draw the states first, fill with white, then draw borders in gray. 
# not inside aes(). because every state gets the same white fill and gray border.
# geom_sf(
#data = growth_map,
#aes(fill = Rent_Growth)
#)
# tells R: On top of the states, draw the 91 metro areas and vary their fill according to rent growth.
# Coord then focuses map
##########################

########################## Make map cleaner
ggplot() +
  geom_sf(
    data = state_shapes,
    fill = "white",
    color = "gray75",
    linewidth = 0.3
  ) +
  geom_sf(
    data = growth_map,
    aes(fill = Rent_Growth),
    color = "gray30",
    linewidth = 0.2
  ) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  ) +
  labs(
    title = "Multifamily Rent Growth Across U.S. Metropolitan Areas",
    subtitle = "Percent change in Zillow Observed Rent Index, August 2019–August 2026",
    fill = "Rent Growth (%)",
    caption = "Source: Zillow Research, Multifamily ZORI"
  ) +
  theme_minimal()
##########################

##########################
# linewidth = 0.3
# controls thickness of border
# Use color = "gray30" to give each metro a thin outline
# fill = inside of a polygon
# color = border of a polygon
# Labs is labels
# fill changes legend titles
# theme_minimal() changes overall appearance
##########################

##########################
# map note: uses only the 91 metros with complete ZORI observations from January 2015–August 2026, even though the growth calculation itself is August 2019–August 2026.
##########################

# new question: Is the geographic pattern stronger than we’d expect if rent growth were spatially random?
# Goal 6: Moran I

##########################
# Moran’s I asks whether metros with similar rent growth tend to be geographically near one another
# x(i) = rent growth in metro i.
# x(bar) = average rent growth across the 91 metros. 
# (x(i)-x(bar)) Deviation of the variable value at location i from its mean 
# w(ij) = whether/how metro j is considered a neighbor of metro i. Spatial weight between feature i and feature j based on proximity or adjacency
# N=91
# First question: what counts as a neighbor?
# Typically two areas are neighbors when their polygons touch.
# My metro areas are scattered
# My definition of neighbor: Two metros are neighbors if they’re geographically close enough to one another.
##########################

# Goal 7: examine distances between 91 metros and choose way to define spatial neighbors.
# project coordinate system where distances have physical units.

########################## 
growth_map_proj <- growth_map %>%
  st_transform(5070)
##########################

##########################
# st_transform() changes the coordinate reference system (CRS) of spatial data
# census shapes are in NAD83
# with coords that are longitude and latitude which are angles not distance
# st_transform(5070) converts geometry to NAD83/Conus Albers, EPSG 5070. a projected coordinate system designed for the contiguous United States.
# changes how those locations are represented mathematically.
# longitude/latitude -> projected x/y coordinates
##########################

########################## inspect projection details.
st_crs(growth_map_proj)
growth_map_proj
##########################

##########################
# bounding-box numbers are now different from the longitude/latitude 
##########################

# Goal 6: create one point for each metro
# each metro is a MULTIPOLYGON area with boundaries. For distance-based neighbors, it is easier to represent each metro with one point.

########################## change geometry representation
metro_points <- growth_map_proj %>%
  st_point_on_surface()
##########################

##########################
# st_point_on_surface() creates a point guaranteed to lie somewhere on that metro’s geometry.
# 91 metro polygons -> 91 representative points 
# each point still includes other variables
# Future project note: st_centroid() is also commonly used for irregular or multipart polygons, a centroid can sometimes fall outside the actual polygon. st_point_on_surface() guarantees the representative point is on the feature, which is best for this project.
##########################


########################## Check
metro_points
##########################

##########################
# has changed from MULTIPOLYGON -> POINT
##########################

# Goal 7: For each metro, how far away is its nearest other metro. 


########################## Calculate all pairwise distances
metro_distances <- st_distance(metro_points)
dim(metro_distances)
##########################

##########################
# st_distance() calculates the distance from every metro to every other metro.
# I have 91 metros, this creates a 91x91 distance matrix
# diagonal would be 0 because Chicago dist from itself = 0
##########################


########################## check small section of the matrix. units in [m] meters
metro_distances[1:5, 1:5]
##########################

########################## Replace all 91 self-distances with missing values.
diag(metro_distances) <- NA
##########################

##########################
# To keep R from saying Chicago -> Chicago as nearest neighbor
##########################


########################## 
nearest_distance <- apply(
  metro_distances,
  1,
  min,
  na.rm = TRUE
)
##########################

##########################
# apply()
# Apply a function across the rows or columns of a matrix
# 1 means rows
# min, finds smallest value
# For each metro row, find smallest nonmissing distance
# nearestdistance(i) = min(ij), j does not equal i
##########################

########################## min, quart, med, mean, max nearest-neighbor distance
summary(nearest_distance)
##########################

##########################
# Min: 41 km
# 25th: 90 km
# Med: 131 km
# Mean: 219 km
# 75th: 189 km
# Max: 3,900 km
##########################

# max is very different from everything else. find out which metro produced that 3,900 km value. It is probably Hawai and Alaska

########################## 
which.max(nearest_distance)
##########################

##########################
# which.max() will show which of 91 metros has 3,900 km nearest neighbor distance
# x <- c(100, 250, 75, 900, 120) 
# which.max(x)
# returns 4 because the 4th observation is largest value
##########################


########################## Find row with largest nearest neighbor distance, show which metro it is.
metro_points[which.max(nearest_distance), ] %>%
  select(RegionName, StateName, Rent_Growth)
########################## Urban Honolulu is the 3,900 km max

##########################
# For this spatial analysis, I'm making Moran’s I analysis contiguous U.S. only. map already focuses on the contiguous U.S., including Honolulu makes a spatial-neighbor problem: either leave it isolated or use large distance threshold to give it a neighbor.
# solution: Create a new spatial-analysis dataset without HI
##########################


########################## Keep observations where StateName not equal to "HI".
metro_points_conus <- metro_points %>%
  filter(StateName != "HI")
nrow(metro_points_conus)
########################## 91 -> 90 rows

# recalculate distance

##########################
conus_distances <- st_distance(metro_points_conus)
diag(conus_distances) <- NA
nearest_conus <- apply(
  conus_distances,
  1,
  min,
  na.rm = TRUE
)
summary(nearest_conus)
########################## 

##########################
# Min: 41092
# 1st Qu: 87747 
# Median: 129081 
# Mean: 177864 
# 3rd Qu: 183909 
# Max:2406404
# Most metros have another sampled metro close
# one metro’s nearest neighbor is 2,406 km away
##########################

########################## check which area is highest
which.max(nearest_conus)
########################## 64

########################## Find row with largest nearest neighbor distance, show which metro it is.
metro_points_conus[which.max(nearest_conus), ] %>%
  select(RegionName, StateName, Rent_Growth)
########################## it is Alaska Anchorage

########################## Keep observations where StateName not equal to "HI" and "AK".
metro_points_conus <- metro_points %>%
  filter(!StateName %in% c("AK", "HI"))
##########################

########################## check
nrow(metro_points_conus)
########################## 91 -> 89

########################## Calculate distance
conus_distances <- st_distance(metro_points_conus)
diag(conus_distances) <- NA
nearest_conus <- apply(
  conus_distances,
  1,
  min,
  na.rm = TRUE
)
summary(nearest_conus)
########################## max is 448045 km 


########################## check. If it’s a reasonable western metro that’s simply far from the other metros, then the 448 km distance says something about the geography.
metro_points_conus[which.max(nearest_conus), ] %>%
  select(RegionName, StateName, Rent_Growth)
########################## El Paso is most isolated metro. 

# Goal 8: start defining neighbors

########################## install/load spdep
install.packages("spdep")
library(spdep)
##########################

# spdep used for spatial-neighbor structures and Moran’s I
# focused on spatial weights and autocorrelation statistics. 

########################## gather coords of 89 points
coords <- st_coordinates(metro_points_conus)
##########################

##########################
# st_coordinates() takes the spatial POINT geometry and extracts X and Y coordinates
##########################


########################## create distance based neighbor object
neighbors_448 <- dnearneigh(
  coords,
  d1 = 0,
  d2 = 448046
)
##########################

##########################
# dnearneigh() asks which observations are in a specified distance of one another
# d1 = 0 is minimum distance
# d2 = 448046 is max distance
# metro j is considered a neighbor of metro i when:  0 < d(ij) <= 448,046 meters
# using 448046 instead of 448045 to avoid a rounding-edge problem with El Paso
# neighbour object has 5 sub-graphs means the 89 metros form 5 separate connected groups when defining neighbors as metros within 448 km.
# Every metro can have at least one neighbor, but it doesn’t guarantee all 89 metros are connected into one network. A metro in one group might be connected to several nearby metros, which connect to others, but there may be no chain connecting that whole group to another group.
# 0 isolated observations = every metro has at least one neighbor.
# 1 subgraph = every metro is connected, directly or indirectly, into one overall network.
# I have 5 subgraphs.
##########################

########################## how many neighbors each metro gets
neighbor_counts <- card(neighbors_448)
##########################

##########################
# card() means cardinality. the number of neighbors associated with each observation.
# if Chicago has 8 metros within 448 km 
# and El Paso has 1
# then summary(neighbor_counts) will show min, med, max. 
##########################

##########################
summary(neighbor_counts)
sum(neighbor_counts == 0)
##########################

##########################
# Min: 1.00 
# 1st Qu: 4.00 
# Median: 7.00 
# Mean: 7.82  
# 3rd Qu: 11.00  
# Max: 20.00 
# all metros have at least one neighbor, typical metro has 7 neighbors, most connected has 20. The network still has 5 separate subgraphs.
##########################


########################## which metros have very few neighbors.
metro_points_conus %>%
  st_drop_geometry() %>%
  mutate(
    Neighbors = neighbor_counts
  ) %>%
  select(RegionName, StateName, Neighbors) %>%
  arrange(Neighbors) %>%
  head(10)
##########################

##########################
# mutate(Neighbors = neighbor_counts)
# neighbor_counts vector has 89 numbers in same order as the 89 rows of metro_points_conus. attaches each count to its corresponding metro
# arrange(Neighbors)
# sorts from fewest to most neighbors, then display 10 least connected metros
# Observation: the number of neighbors varies a lot from 1 to 20
# bring in k-nearest neighbors (KNN)
# instead of saying every metro within 448 km is a neighbor
# KNN says for every metro, find its k geographically closest metros.
# if k = 4 then each metro will get its 4 closest neighbors
# even though the physical distance to those neighbors differ.
##########################


# Goal 9: create 4 nearest neighbor network


########################## Fix amount of coordinate point
coords_conus <- st_coordinates(metro_points_conus)
##########################

##########################
# coords created before I removed Alaska and Hawaii. cant use old 91 point coordinates with current 89 points
# recreated coords_conus from the correct 89-metro dataset
##########################

########################## find nearest observations
knn4 <- knearneigh(
  coords_conus,
  k = 4
)
##########################

##########################
#knearneigh() finds nearest observations but doesn’t yet create same neighbor-list structure as dnearneigh()
# It returns a matrix containing the indices of these nearest neighbors.
# It helps build spatial weights and measure spatial patterns.
##########################

########################## turn results into a spatial neighbor object
neighbors_knn4 <- knn2nb(knn4)
##########################

##########################
# Transforms raw index matrices from knearneigh into spatial neighbor structures needed for spatial weights and autocorrelation analysis.
##########################

########################## check
summary(card(neighbors_knn4))
########################## each metro selected four other metros as nearest neighbor

########################## check for symmetry
is.symmetric.nb(neighbors_knn4)
########################## not symmetrical

##########################
# KNN is directional
# just because El Paso is Albuquerque’s nearest neighbor does not mean Albuquerque is in El Paso's four closest metros
##########################

########################## create a symmetric version
neighbors_knn4_sym <- make.sym.nb(neighbors_knn4)
##########################

##########################
# if either metro considers the other a neighbor, the connection is retained in both directions.
##########################

########################## check
summary(card(neighbors_knn4_sym))
##########################

##########################
# neighbors_knn4_sym answers who is a neighbor of who
# symmetric 4 nearest neighbors avoids having some metros represented by only one neighbor while others have twenty, and still keeps the network local. 
##########################

# Goal 10: create spatial weights

########################## 
weights_knn4 <- nb2listw(
  neighbors_knn4_sym,
  style = "W"
)
##########################

##########################
# Moran I needs a spatial weights matrix W to figure out how much weight should each neighbor receive?
# nb2listw() converts neighbor structure into weights
# style = "W" means row-standardized weights each metro’s neighbor weights add up to 1
# sum(w(ij)) = 1
##########################

########################## Check 
summary(weights_knn4)
##########################

##########################
# n = 89 confirms I'm still using 89 contiguous-U.S. metros.
# S0 = 89 Each metro’s weights sum to 1, so across 89 metros'
# S0 = Sum(i)Sum(j)(W(ij) = 89
# The S1, S2 used in calculating variance of Moran’s I
##########################

# calculate spatial lag

########################## create spatial lag variable
metro_points_conus <- metro_points_conus %>%
  mutate(
    Spatial_Lag = lag.listw(
      weights_knn4,
      Rent_Growth
    )
  )
##########################

##########################
# spatial lag - for each metro there is its own rent growth, x(i)
# spatial lag check what is rent growth like among this metro x(i)'s neighbors
# Wx(i) = sum(j)w(ij)x(j) - a weighted average of neighboring metros’ rent growth.
# if:
# Neighbor A = 20%
# Neighbor B = 30%
# Neighbor C = 40%
# Neighbor D = 50%
# Each receives weight 0.25
# spatial lag = (.25)(20) + .(25)(30) + ... = 35%
# For metros with 5, 6, or 7 neighbors after symmetrization, "W" weights adjust so their weights still sum to 1.
##########################

########################## display rent growth, region names, and spatial lag.
metro_points_conus %>%
  st_drop_geometry() %>%
  select(
    RegionName,
    Rent_Growth,
    Spatial_Lag
  ) %>%
  head(10)
##########################

##########################
# Rent_Growth asks how much did this metro’s multifamily ZORI increase
# Spatial_Lag asks how much did nearby metros’ rents increase
# Moran’s I is going to examine if metros with high rent growth tend to be surrounded by other high growth metros, and do low growth metros tend to be surrounded by other low growth metros
# Poughkeepsie had rent growth of about 59.0%, neighboring metros averaged about 47.0%. Both relatively high. Phoenix, on the other hand, had growth of about 27.5%, while its neighbors averaged 42.6%. Phoenix is relatively low compared with its surrounding metro network.
# Moran’s I summarizes this across all 89 metros simultaneously
##########################


########################## make Moran scatterplot
ggplot(
  st_drop_geometry(metro_points_conus),
  aes(
    x = Rent_Growth,
    y = Spatial_Lag
  )
) +
  geom_point() +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Moran Scatterplot of Multifamily Rent Growth",
    subtitle = "89 contiguous-U.S. metropolitan areas, August 2019–August 2026",
    x = "Metro Rent Growth (%)",
    y = "Spatial Lag of Rent Growth (%)",
    caption = "Spatial weights: symmetric 4-nearest-neighbor network"
  ) +
  theme_minimal()
##########################



##########################
# geom_point() gives one point for every metro
# x = metro's own rent growth 
# y = neighboring metros' rent growth
# geom_smooth(
# method = "lm",
# se = FALSE
# )
# adds a linear regression line through those points. 
# method = "lm" means linear model, while se = FALSE tells R not to draw the confidence band around the line.
# If metros with high rent growth tend to be near other high-growth metros and low growth metros near other low growth metros I'll see an upward relationship
# if high growth metros tended to be surrounded by low growth metros and vice versa, I'd see a downward relationship negative spatial autocorrelation
# a proper Moran scatterplot usually standardizes the variable and its spatial lag around zero. 
# This is an easier-to-interpret version in the original percentage-point units. just to see the underlying spatial relationship
# The regression line slopes upward, metros with higher rent growth tend to have neighbors with higher rent growth. But there is also a lot of scatter, can't describe the relationship as strong just from graph.
##########################

########################## calculate Global Moran’s I
moran.test(
  metro_points_conus$Rent_Growth,
  weights_knn4
)
##########################

##########################
# Moran’s I compares each metro’s deviation from average rent growth with the deviations among its neighbors
# N = 89 
# x(i) = Rent_Growth
# x(bar) = mean rent growth
# w(ij) = weights_knn4
# Wx: calculated as Spatial_Lag
# formally summarizes the spatial relationship already built
# Under spatial randomness, Moran’s I isn’t expected to be exactly zero. 
# expected value (E(i)) is:
# = (-1)/N-1
# = -.0114
# R compares observed Moran’s I against −0.011 not 0
# an observed value above that indicates positive spatial autocorrelation, below it indicates negative spatial autocorrelation. 
# whether it is statistically distinguishable from spatial randomness comes from the test statistic and p-value.
##########################


# Conclusion: The formal test supports the scatterplot: multifamily rent growth in 89-metro contiguous-U.S. sample show a positive spatial autocorrelation under the symmetric 4-nearest-neighbor specification.

##########################
# Moran’s I = 0.208 
# metros with high multifamily rent growth tend to be located near other metros with high growth, while low growth metros tend to be near other low growth metros.
# 0.208 does not mean 20.8% spatial correlation. Moran’s I isn’t a percentage and shouldn’t be interpreted like an ordinary correlation coefficient.
# Expected Moran’s I = −0.0114
# observed statistic is substantially more positive than its null expectation
# Standard deviate (z) = 3.269 
# observed Moran’s I is about 3.27 standard deviations above its expected value under the null
# p = 0.00054 
# test was one-sided because moran.test() defaults to is spatial autocorrelation greater than expected under spatial randomness
# strong statistical evidence against the null in favor of positive spatial autocorrelation
# Multifamily rent growth exhibited statistically significant positive spatial autocorrelation across the 89 contiguous-U.S. metropolitan areas in the balanced sample (Moran’s I = 0.208, p < 0.001), using row-standardized symmetric four-nearest-neighbor spatial weights.
# it doesn’t tell where the clusters are. Global Moran’s I gives one statistic for the entire sample.
##########################

# next step Local Moran’s I (LISA)
# where are the high-high, low-low, high-low, and low-high spatial patterns

########################## Run local Moran
local_moran <- localmoran(
  metro_points_conus$Rent_Growth,
  weights_knn4
)
##########################

##########################
# localmoran() will give a Local Moran statistic for every metro.
# 89 local statistics
##########################


########################## check
dim(local_moran)
head(local_moran)
##########################

##########################
# Local Moran’s I asks for metro(i), is its rent growth similar to or different from the rent growth of its neighbors?
# I = z(i) x sum(j)w(ij)z(j)
# z(i) = this metro. whether metro i’s rent growth is above or below the overall average
# z(i) = x(i)-x(bar)/s
# x(i) = metro i’s rent growth
# x(bar) = average rent growth across the 89 metros
# s = standard deviation of rent growth
# sum(j)w(ij)z(j) = neighbors. Are this metro’s neighbors generally above or below average?
# leads to 4 combinations: HH LL HL LH
# doing many local tests which means some metros can appear significant simply by chance.
##########################

##########################
# Ii — Local Moran’s I
# E.Ii — expected Local Moran’s I. what R expects for that local statistic under the null hypothesis of spatial randomness
# Var.Ii — variance
# Z.Ii — standardized test statistic. How far is the observed Local Moran statistic from what we’d expect under the null, measured in standard deviations?
# Pr(z != E(Ii)) — p-value 
  # None is below .05 can't label any of these six as statistically significant LISA clusters/outliers based on this test.
# Phoenix falls in the Low–High quadrant, but the Local Moran statistic is not statistically significant.
##########################

########################## how many of the 89 metros have significant Local Moran statistics
sum(local_moran[, "Pr(z != E(Ii))"] < 0.05)
########################## 15

##########################
# local_moran[, "Pr(z != E(Ii))"] provide all rows (,) from the column named "Pr(z != E(Ii))".
# p < 0.05 turns each p-value into true false and sum() counts trues
# how many of my 89 metros have a Local Moran p-value below .05
##########################

########################## creates a new columns in spatial dataset 
metro_points_conus$Local_I <- local_moran[, "Ii"]
metro_points_conus$Local_p <- local_moran[, "Pr(z != E(Ii))"]
##########################

##########################
# local_moran[, "Ii"] takes the 89 Local Moran statistics and creates a new column in spatial dataset called Local_I
# same for p values
##########################


########################## Show the 15 metros with p<.05, sorted from smallest to largest p-value.
metro_points_conus %>%
  st_drop_geometry() %>%
  filter(Local_p < 0.05) %>%
  select(
    RegionName,
    Rent_Growth,
    Spatial_Lag,
    Local_I,
    Local_p
  ) %>%
  arrange(Local_p)
##########################

########################## check dividing line is the mean rent growth for the 89 CONUS metros, not the median and not the earlier 91-metro mean
mean(
  metro_points_conus$Rent_Growth,
  na.rm = TRUE
)
##########################


##########################
# LISA quadrants are based on whether a metro and its spatial lag are above or below the mean.
##########################


########################## classify the LISA quadrants
metro_points_conus <- metro_points_conus %>%
  mutate(
    LISA_Cluster = case_when(
      Local_p >= 0.05 ~ "Not Significant",
      Rent_Growth > mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag > mean(Rent_Growth, na.rm = TRUE) ~ "High-High",
      Rent_Growth < mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag < mean(Rent_Growth, na.rm = TRUE) ~ "Low-Low",
      Rent_Growth > mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag < mean(Rent_Growth, na.rm = TRUE) ~ "High-Low",
      Rent_Growth < mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag > mean(Rent_Growth, na.rm = TRUE) ~ "Low-High"
    )
  )
##########################

##########################
# case_when() is useful when creating a categorical variable from several conditions.
# condition ~ "result" means If this condition is TRUE, assign this result.
# Local_p >= 0.05 ~ "Not Significant" 
# If the local p-value is at least .05, classify metro as "Not Significant".
# case_when() works from top to bottom, so those metros get removed from consideration before assigning HH/LL/HL/LH.
# & -> Both conditions must be true to assign High-High
# Because spatial weights are row-standardized spatial lag represents the weighted average of neighboring rent growth.I'm asking whether that neighbor average is above or below the same overall rent-growth mean.
##########################

########################## check
table(metro_points_conus$LISA_Cluster)
##########################

########################## see which metros are in each category
metro_points_conus %>%
  st_drop_geometry() %>%
  filter(LISA_Cluster != "Not Significant") %>%
  select(
    RegionName,
    Rent_Growth,
    Spatial_Lag,
    Local_I,
    Local_p,
    LISA_Cluster
  ) %>%
  arrange(LISA_Cluster, Local_p)
##########################

##########################
# LISA_Cluster != "Not Significant" Keep metros whose LISA classification is not "Not Significant".
##########################

########################## 
metro_points_conus <- metro_points_conus %>%
  mutate(
    Local_p_FDR = p.adjust(
      Local_p,
      method = "BH"
    )
  )
########################## 

##########################
# p.adjust() input is a vector of p-values like Local_p
# method = "BH"
# Benjamini–Hochberg procedure to control the false discovery rate
# because I performed 89 local tests
# even if there were no real local patterns, with a conventional a =.05, some small p-values occur just from chance. Benjamini–Hochberg adjusts the p-values to account for the fact that I am testing many metros.
# which local patterns remain convincing after accounting for the fact that I searched across 89 locations?
##########################

########################## counting adjusted p values
sum(metro_points_conus$Local_p_FDR < 0.05)
########################## 9 adjusted p values left

########################## which nine are left
metro_points_conus %>%
  st_drop_geometry() %>%
  filter(Local_p_FDR < 0.05) %>%
  select(
    RegionName,
    Rent_Growth,
    Spatial_Lag,
    Local_I,
    Local_p,
    Local_p_FDR,
    LISA_Cluster
  ) %>%
  arrange(Local_p_FDR)
##########################

########################## Create a second variable for adjusted p values
metro_points_conus <- metro_points_conus %>%
  mutate(
    LISA_FDR = case_when(
      Local_p_FDR >= 0.05 ~ "Not Significant",
      Rent_Growth > mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag > mean(Rent_Growth, na.rm = TRUE) ~ "High-High",
      Rent_Growth < mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag < mean(Rent_Growth, na.rm = TRUE) ~ "Low-Low",
      Rent_Growth > mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag < mean(Rent_Growth, na.rm = TRUE) ~ "High-Low",
      Rent_Growth < mean(Rent_Growth, na.rm = TRUE) &
        Spatial_Lag > mean(Rent_Growth, na.rm = TRUE) ~ "Low-High"
    )
  )
##########################

##########################
# what only changes is: Local_p >= 0.05 ~ "Not Significant" -> Local_p_FDR >= 0.05 ~ "Not Significant"
##########################

########################## verify
table(metro_points_conus$LISA_FDR)
########################## 4 High–High, 3 Low–Low, 1 High–Low, 1 Low–High, and 80 Not Significant


########################## match LISA results to the metro polygons
lisa_map <- growth_map %>%
  filter(!StateName %in% c("AK", "HI")) %>%
  left_join(
    metro_points_conus %>%
      st_drop_geometry() %>%
      select(GEOID, LISA_FDR),
    by = "GEOID"
  )
##########################

##########################
# polygon gets classified HH
##########################


########################## check
table(lisa_map$LISA_FDR)
##########################

########################## LISA cluster map
ggplot() +
  geom_sf(
    data = state_shapes,
    fill = "white",
    color = "gray75",
    linewidth = 0.3
  ) +
  geom_sf(
    data = lisa_map,
    aes(fill = LISA_FDR),
    color = "gray30",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "High-High" = "#d73027",
      "High-Low" = "#fc8d59",
      "Low-High" = "#91bfdb",
      "Low-Low" = "#4575b4",
      "Not Significant" = "gray85"
    )
  ) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  ) +
  labs(
    title = "Local Spatial Clusters of Multifamily Rent Growth",
    subtitle = "August 2019–August 2026",
    fill = "LISA Cluster",
    caption = "Source: Zillow Research, Multifamily ZORI; FDR-adjusted p < 0.05"
  ) +
  theme_minimal()
##########################

##########################
# changed: aes(fill = Rent_Growth) -> aes(fill = LISA_FDR)
# color each metro according to its numerical rent growth -> Color each metro according to its LISA category.
# added: scale_fill_manual()
# before, ggplot decided which colors represented each category. scale_fill_manual() lets me choose.
# because LISA_FDR is categorical, ggplot will automatically give separate legend entries
# where do we have statistically significant local spatial clusters or spatial outliers after FDR correction?
##########################









# ============================================================
# VARIABLE DEFINITIONS
# Zillow Multifamily ZORI Exploratory Analysis
# ============================================================


# ------------------------------------------------------------
# ORIGINAL ZILLOW VARIABLES
# ------------------------------------------------------------

# RegionID
# Zillow's numeric identifier for each geographic region.
# Used as a unique identifier for metropolitan areas.

# SizeRank
# Zillow's ranking of geographic areas by size.

# RegionName
# Name of the geographic region.
# Example: "Chicago, IL"

# RegionType
# Type of geographic unit represented by the observation.
# "msa" identifies Metropolitan Statistical Areas.

# StateName
# State abbreviation associated with the geographic region.

# ZORI
# Zillow Observed Rent Index value for a metro in a given month.
# In this analysis, ZORI refers to the multifamily ZORI series.


# ------------------------------------------------------------
# TIME VARIABLES
# ------------------------------------------------------------

# Month
# Calendar month associated with each ZORI observation.
# Created when the original wide Zillow dataset was reshaped
# into long format using pivot_longer().
# Stored as an R Date.

# Year
# Calendar year extracted from Month using lubridate::year().
# Used for annual summaries of missingness and data coverage.


# ------------------------------------------------------------
# DATA COVERAGE / MISSINGNESS VARIABLES
# ------------------------------------------------------------

# Total
# Total number of observations within a given year,
# including observations with missing ZORI values.

# Observed
# Number of observations within a year with a nonmissing
# ZORI value.

# Missing
# Number of observations within a year with a missing
# ZORI value.

# Percent_Missing
# Percentage of observations within a year with missing
# ZORI values.
#
# Formula:
# (Missing / Total) * 100

# Metros
# Number of distinct MSAs with at least one nonmissing
# ZORI observation during a given year.

# months_observed
# Number of months for which an MSA has a nonmissing
# ZORI observation between January 2015 and August 2026.
#
# There are 140 months in the complete study period.
# MSAs with months_observed == 140 were included in the
# balanced panel.


# ------------------------------------------------------------
# RENT GROWTH VARIABLES
# ------------------------------------------------------------

# ZORI_2019
# Multifamily ZORI value for an MSA in August 2019.
# Used as the baseline for the cumulative rent-growth measure.

# ZORI_2026
# Multifamily ZORI value for the same MSA in August 2026.
# Used as the ending value for the cumulative rent-growth measure.

# Rent_Growth
# Percentage change in multifamily ZORI between
# August 2019 and August 2026.
#
# Formula:
# ((ZORI_2026 / ZORI_2019) - 1) * 100
#
# This represents cumulative percentage growth over the
# period, NOT an annual growth rate.


# ------------------------------------------------------------
# GEOGRAPHIC VARIABLES
# ------------------------------------------------------------

# GEOID
# Census geographic identifier for each Metropolitan
# Statistical Area.
#
# Used to match Zillow metro observations to Census
# geographic boundaries for mapping and spatial analysis.


# ------------------------------------------------------------
# SPATIAL NEIGHBOR OBJECTS
# ------------------------------------------------------------

# coords_conus
# Projected X/Y coordinates representing the locations of
# the contiguous-U.S. metropolitan areas used in the
# spatial analysis.

# neighbors_knn4
# Four-nearest-neighbor (KNN) spatial network.
#
# Each MSA is initially connected to its four geographically
# nearest MSAs.

# neighbors_knn4_sym
# Symmetric version of the four-nearest-neighbor network.
#
# If Metro A identifies Metro B as a neighbor, the
# symmetrized network treats A and B as neighbors of
# one another.

# weights_knn4
# Row-standardized spatial weights object constructed from
# the symmetric four-nearest-neighbor network.
#
# style = "W" means the spatial weights associated with
# each metro's neighbors sum to 1.


# ------------------------------------------------------------
# SPATIAL ANALYSIS VARIABLES
# ------------------------------------------------------------

# Spatial_Lag
# Weighted average of Rent_Growth among an MSA's spatial
# neighbors, based on the row-standardized KNN weights.
#
# Conceptually:
# Spatial_Lag_i = sum(w_ij * Rent_Growth_j)
#
# A high Spatial_Lag means the metro is surrounded by
# metros that generally experienced relatively high
# rent growth.


# ------------------------------------------------------------
# LOCAL MORAN'S I / LISA VARIABLES
# ------------------------------------------------------------

# Local_I
# Local Moran's I statistic for each MSA.
#
# Measures whether the metro's rent-growth value is locally
# similar to or different from the values of its spatial
# neighbors.
#
# Positive Local_I:
# Tendency toward local similarity (High-High or Low-Low).
#
# Negative Local_I:
# Tendency toward local dissimilarity
# (High-Low or Low-High).
#
# Statistical significance must also be considered.

# Local_p
# Unadjusted two-sided p-value associated with each metro's
# Local Moran's I statistic.
#
# Because Local Moran's I was calculated separately for
# 89 metros, these p-values involve multiple hypothesis tests.

# LISA_Cluster
# Original LISA classification based on the unadjusted
# Local Moran p-value (Local_p < 0.05).
#
# Categories:
#
# High-High:
# Above-average rent growth and above-average neighboring
# rent growth.
#
# Low-Low:
# Below-average rent growth and below-average neighboring
# rent growth.
#
# High-Low:
# Above-average rent growth but below-average neighboring
# rent growth. Potential spatial outlier.
#
# Low-High:
# Below-average rent growth but above-average neighboring
# rent growth. Potential spatial outlier.
#
# Not Significant:
# Local Moran's I does not meet the specified significance
# threshold.


# ------------------------------------------------------------
# MULTIPLE-TESTING ADJUSTMENT
# ------------------------------------------------------------

# Local_p_FDR
# Local Moran p-value adjusted for multiple hypothesis
# testing using the Benjamini-Hochberg (BH) false discovery
# rate procedure.
#
# Created using:
# p.adjust(Local_p, method = "BH")
#
# This adjustment accounts for the fact that Local Moran's I
# was tested separately for 89 metropolitan areas.


# ------------------------------------------------------------
# FINAL LISA CLASSIFICATION
# ------------------------------------------------------------

# LISA_FDR
# Final LISA classification using the FDR-adjusted p-value.
#
# A metro is classified as a significant spatial cluster or
# spatial outlier only when Local_p_FDR < 0.05.
#
# Categories:
#
# High-High:
# Above-average rent growth surrounded by above-average
# rent growth.
#
# Low-Low:
# Below-average rent growth surrounded by below-average
# rent growth.
#
# High-Low:
# Above-average rent growth surrounded by below-average
# rent growth.
#
# Low-High:
# Below-average rent growth surrounded by above-average
# rent growth.
#
# Not Significant:
# Local_p_FDR >= 0.05.
#
# Final FDR-adjusted classifications:
# High-High        = 4
# High-Low         = 1
# Low-High         = 1
# Low-Low          = 3
# Not Significant  = 80
#
# Total CONUS spatial-analysis sample = 89 MSAs


# ============================================================
# ANALYSIS SAMPLE NOTES
# ============================================================

# Original Zillow file:
# 637 MSAs represented across the full dataset.

# Balanced panel:
# 91 MSAs with complete monthly ZORI observations from
# January 2015 through August 2026.

# Spatial-analysis sample:
# 89 MSAs after excluding Alaska and Hawaii from the
# balanced panel.

# Rent-growth period:
# August 2019 through August 2026.

# Global Moran's I:
# I = 0.208, p < 0.001
#
# Interpretation:
# Multifamily rent growth exhibited statistically significant
# positive spatial autocorrelation across the 89 CONUS MSAs
# under the symmetric four-nearest-neighbor spatial weights
# specification.

# IMPORTANT:
# Spatial autocorrelation identifies geographic association.
# It does not establish causality or prove that rent changes
# in one metro caused rent changes in neighboring metros.
# ============================================================