library(dplyr)
library(ggplot2)
# install.packages("sf") # https://github.com/r-spatial/sf/issues/2035
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)
library(tidyr)
library(purrr)
library(lwgeom)
library(stringr)
library(moderndive)
# devtools::install_github(“rstudio/gt”)
library(knitr)
library(kableExtra)
library(ggfortify)
library(performance)

## -------SOURCING DATA--------
# variables from https://api.census.gov/data/2020/acs/acs5/groups.html
bg_shapes <- get_acs(
  geography = "block group", 
  variables = c("B02001_001", "B02008_001", "B02009_001", "B02010_001", 
                "B02011_001", "B19013_001", "B03003_001", "B03003_003"),
  state = "CA", 
  year = 2020,
  cache_table = TRUE,
  geometry = TRUE
) 

# CSV is too large to push -- download from https://www.epa.gov/dwucmr/occurrence-data-unregulated-contaminant-monitoring-rule#4
UCMR <- read.csv("data/UCMR4_All.csv") %>% 
  filter(PWSName %in% c("Pala North", "Pauma", "Pechanga", "Redding Rancheria", 
                        "Viejas Community System") | State == "CA") 

govwatershapes <- st_read("data/SABL_Public_230403") %>% 
  rename("PWSID" = "SABL_PWSID") %>%
  filter(PWSID %in% unique(UCMR$PWSID)) %>% 
  st_transform(4269)

# .gpkg is too large to push -- download from https://github.com/SimpleLab-Inc/wsb
SLwatershapes <- st_read("data/temm.gpkg") %>% 
  # https://community.rstudio.com/t/how-to-open-a-geopackage-file/41027
  filter(pwsid %in% unique(UCMR$PWSID))


## -------COMBINING CA PWS AND SIMPLELAB GEOMETRIES--------
missingPWSs <- setdiff(unique(UCMR$PWSID), unique(govwatershapes$PWSID)) %>% 
  map(function (x) {ifelse(substr(x,1,1) == "9", paste0("0", x), x)}) 

missingshapes <- SLwatershapes %>% 
  filter(pwsid %in% missingPWSs) %>% 
  rename("PWSID" = "pwsid",
         "WATER_SY_1" = "pws_name",
         "geometry" = "geom") %>% 
  select(PWSID, WATER_SY_1, geometry) %>% 
  st_transform(4269)

PWS <- bind_rows(missingshapes, govwatershapes)

## ACS variable codes 
# B02001_001 Race total B02001_002 White alone\
# B02009_001 Black alone or in combination 
# B02010_001 Native American alone or in combination 
# B02011_001 Asian alone or in combination 
# Hispanic total: B03003_001 
# B03003_003 yes hispanic or latino

## -------TRANSFORMING DEMOGRAPHICS DATA--------
bg_shapes_wide <- bg_shapes %>% 
  select(-moe) %>% 
  pivot_wider(names_from = "variable", values_from = "estimate") %>% 
  rename(Total = B02001_001,	
         White = B02008_001,	
         Black = B02009_001,
         Indigenous = B02010_001,	
         Asian = B02011_001,
         Total_Hisp = B03003_001,	
         Hisp_Lat = B03003_003,
         Med_Income = B19013_001) #%>% 
# mutate(White = White / Total,
#        Black = Black / Total,
#        Indigenous = Indigenous / Total,
#        Asian = Asian / Total,
#        Hisp_Lat = Hisp_Lat / Total_Hisp
#        )

# Save until after having joined since weighted averages won't work with props
# Want to use counts

# block_shapes_tall_props <- block_shapes_wide %>% 
#   pivot_longer(c(White, Black, Asian, Indigenous, Hisp_Lat), 
#                names_to = "race", values_to = "prop")
# 
# ggplot(block_shapes_tall2, aes(x = race, y = prop, color = race)) + geom_col()
# 
# block_shapes_vals <- block_shapes %>% 
#   filter(!(Total == 0 & Total_Hisp == 0))

## -------JOINING AND AVERAGING DEMOGRAPHICS--------
# below line sourced from https://github.com/r-spatial/sf/issues/493
sf_use_s2(FALSE) # flattens shapes

# join <- watershapes %>% 
#   inner_join(UCMR_CA, by = c("pwsid" = "PWSID")) %>% 
#   select(pwsid, PWSName, primacy_agency_code, primary_source_code, tier, FacilityWaterType, CollectionDate, Contaminant, MRL, AnalyticalResultsSign, AnalyticalResultValue.µg.L.) %>% 
#   st_intersection(bg_shapes) %>% 
#   mutate(pwi_county = paste0(pwsid, "-", GEOID))


# PWS_demos_joined <- st_join(govwatershapes, bg_shapes_wide)

shape_intersections <- st_intersection(PWS, bg_shapes_wide) %>% 
  mutate(PWS_bg = paste0(PWSID, "-", GEOID))

PWS_aw_demographics <- shape_intersections %>% 
  #--- get area ---#
  mutate(area = as.numeric(st_area(.))) %>% 
  #--- get area-weight by HUC unit ---#
  group_by(PWSID) %>% 
  mutate(weight = area / sum(area)) %>% 
  #--- calculate area-weighted corn acreage by HUC unit ---#
  # 'aw' stands for area-weighted 
  summarize(Total_aw = sum(weight * Total),
            White_aw = sum(weight * White),
            Black_aw = sum(weight * Black),
            Indig_aw = sum(weight * Indigenous),
            Asian_aw = sum(weight * Asian),
            Total_HispLat_aw = sum(weight * Total_Hisp),
            HispLat_aw = sum(weight * Hisp_Lat),
            MedIncome_aw = sum(weight * Med_Income)
  )

# saving for posterity (to not have to rerun code)
# st_write(PWS_aw_demographics, dsn = 'data/joinedPWSdemos/joinedPWSdemos.shp')

## -------WRANGLING DEMOGRAPHICS--------
PWSdemog <- PWS_aw_demographics %>% 
  mutate(White_prop = White_aw / Total_aw,
         Black_prop = Black_aw / Total_aw,
         Indig_prop = Indig_aw / Total_aw,
         Asian_prop = Asian_aw / Total_aw,
         HispLat_prop = HispLat_aw / Total_HispLat_aw)

PWSdemog <- PWSdemog %>% 
  st_drop_geometry() %>% # max.col() couldn't work with 
  mutate(Predom = colnames(.[10:13])[max.col(.[10:13])]) %>% 
  # above line courtesy of https://stackoverflow.com/questions/17735859/for-each-row-return-the-column-name-of-the-largest-value
  right_join(PWSdemog) %>% 
  select(PWSID, MedIncome_aw, White_prop, Black_prop,
         Indig_prop, Asian_prop, HispLat_prop, Predom)

## -------JOINING W UCMR--------
contamsOfInterest <- c("germanium", "manganese", "HAA6Br", "HAA9")

full <- UCMR %>% 
  select(PWSID, PWSName, Contaminant, AnalyticalResultsSign,
         AnalyticalResultValue.µg.L.) %>% 
  filter(Contaminant %in% contamsOfInterest) %>% 
  inner_join(PWSdemog) %>% 
  pivot_longer(c(White_prop, Black_prop, Asian_prop, Indig_prop), 
               names_to = "RaceEth", values_to = "Prop") %>% 
  rename(ContamLevel = AnalyticalResultValue.µg.L.,
         RelativetoMRL = AnalyticalResultsSign,
         MedIncome = MedIncome_aw) %>% 
  filter(ContamLevel != 0) %>% 
  mutate(Predom = substr(Predom, 1, nchar(Predom)-5),
         RaceEth = substr(RaceEth, 1, nchar(RaceEth)-5),
         RaceEth = factor(RaceEth),
         Contaminant = factor(Contaminant, 
                              levels = c("HAA6Br", "HAA9", "manganese", "germanium")),
         LogLevel = log10(ContamLevel),
         HLFiftyPer = ifelse(HispLat_prop >= median(HispLat_prop), "Above", "Below"),
         HLFiftyPer = factor(HLFiftyPer, levels = c("Above", "Below"))) 

write.csv(full, "full.csv", row.names=FALSE)
