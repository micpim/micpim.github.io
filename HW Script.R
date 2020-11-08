library(tidyverse)
library(plotly)
library(sf)
library(tigris)
library(leaflet)
# year <- 2017          #Start running from here if loading in gas/elect data
# quarters <- 1:14
# type <- "Gas"        #Change this to Gas to read in gas data
# 
# pge_gas <- NULL         #Change this to Gas to read in gas data
# quarter = 1
# 
# for(run  in quarters) {
#   if ((run == 5) | (run == 9) | (run == 13)){
#     year = year + 1
#     quarter = quarter - 4
#   }
#   if ((quarter == 3) & (year == 2020)){
#     break
#   }
#   
#   filename <- 
#     paste0(
#       "PGE_",
#       year,
#       "_Q",
#       quarter,
#       "_",
#       type,
#       "UsageByZip.csv"
#     )
#   print(filename)
#   
#   temp <- read_csv(filename)
#   
#   pge_gas <- rbind(pge_gas,temp)     #Change this to Gas to read in gas data
#   quarter = quarter + 1
# }
# year <- 2017          =
# quarters <- 1:14
# type <- "Electric"        #Change this to Gas to read in gas data
# 
# pge_electric <- NULL         #Change this to Gas to read in gas data
# quarter = 1
# 
# for(run  in quarters) {
#   if ((run == 5) | (run == 9) | (run == 13)){
#     year = year + 1
#     quarter = quarter - 4
#   }
#   if ((quarter == 3) & (year == 2020)){
#     break
#   }
#   
#   filename <- 
#     paste0(
#       "PGE_",
#       year,
#       "_Q",
#       quarter,
#       "_",
#       type,
#       "UsageByZip.csv"
#     )
#   temp <- read_csv(filename)
#   
#   pge_electric <- rbind(pge_electric,temp)     #Change this to Gas to read in gas data
#   quarter = quarter + 1
# }
# #saveRDS(pge_gas, "pge_gas.rds")      #Change this to Gas to read in gas data
pge_elec <- readRDS("pge_elec.rds")
pge_gas <- readRDS("pge_gas.rds")
pge_filter_elec <- filter(pge_elec, CUSTOMERCLASS %in% c("Elec- Residential","Elec- Commercial"))
pge_filter_gas <- filter(pge_gas, CUSTOMERCLASS %in% c("Gas- Residential","Gas- Commercial"))
pge_filter_elecNEI <- filter(pge_elec, CUSTOMERCLASS %in% c("Elec- Residential"))
pge_filter_elecNEI <- filter(pge_filter_elecNEI, YEAR %in% c("2019", "2020"))
ElectMonth6 <- filter(pge_filter_elecNEI , MONTH %in% c("6"))

pge_select_elec <- mutate(pge_filter_elec, MONTH = MONTH + (12*(YEAR - 2017)) )
pge_select_gas <- mutate(pge_filter_gas, MONTH = MONTH + (12*(YEAR - 2017)) )


pge_select_elec <-select(pge_select_elec  ,!c( COMBINED, AVERAGEKWH))
pge_select_gas <-select(pge_select_gas ,!c( COMBINED, AVERAGETHM))
ElectMonth6  <-select(ElectMonth6  ,!c( COMBINED, AVERAGEKWH,CUSTOMERCLASS, MONTH))

rm(pge_filter_elec, pge_filter_gas,pge_filter_elecNEI)

pge_group_elec <- group_by( pge_select_elec, MONTH, CUSTOMERCLASS, YEAR)
pge_group_gas <- group_by( pge_select_gas, MONTH, CUSTOMERCLASS, YEAR)
ElectMonth6Group <- group_by( ElectMonth6, YEAR, ZIPCODE)

rm(pge_select_elec,pge_select_gas,ElectMonth6)

pge_summarize_elec <- summarize( pge_group_elec, TOTALKWH = sum( TOTALKWH, na.rm = T),
                                 TOTALCUSTOMERS = sum(TOTALCUSTOMERS, na.rm = T))
pge_summarize_gas <- summarize( pge_group_gas, TOTALTHM = sum( TOTALTHM, na.rm = T),
                                TOTALCUSTOMERS = sum(TOTALCUSTOMERS, na.rm = T))

ElectMonth6Sum <- summarize( ElectMonth6Group, TOTALKWH = sum( TOTALKWH, na.rm = T),
                                 TOTALCUSTOMERS = sum(TOTALCUSTOMERS, na.rm = T))

Customers <- ElectMonth6Sum$TOTALCUSTOMERS
zeroCustomers<- which(Customers %in% 0)
ElectMonth6Sum <-ElectMonth6Sum[-c(zeroCustomers),]

rm(pge_group_elec,pge_group_gas,Customers,zeroCustomers,ElectMonth6Group)

pge_mutate_elec <- mutate(pge_summarize_elec, AVERAGEkBTU = (TOTALKWH * 3412.14 ) /TOTALCUSTOMERS)
pge_mutate_gas  <- mutate(pge_summarize_gas , AVERAGEkBTU = (TOTALTHM * 99976.1 )/TOTALCUSTOMERS)
ElecMutate<- mutate(ElectMonth6Sum, AVERAGEKWH = TOTALKWH /TOTALCUSTOMERS)

rm(pge_summarize_elec,pge_summarize_gas,ElectMonth6Sum )

pgeELECT <-select(pge_mutate_elec  ,-TOTALKWH)
pgeGAS <-select(pge_mutate_gas  , -TOTALTHM )
pge_final<- rbind(pgeELECT,pgeGAS)
rm(pge_mutate_elec,pge_mutate_gas,pgeELECT,pgeGAS)

#saveRDS(pge_final, "pge_final.rds") 
#pge_final<- readRDS("pge_final.rds")

library(plotly)

pge_chart <-
  pge_final %>% 
  ggplot() +
  geom_bar(
    aes(
      x = MONTH %>% factor(),
      y = AVERAGEkBTU,
      fill = CUSTOMERCLASS
    ),
    stat = "identity",
    position = "stack"
  ) +
  labs(
    x = "Months Starting Jan 2017",
    y = "kBTU",
    title = "PG&E Territory Monthly Electricity and Gas Usage, 2017-Present",
    fill = "Electricity Type"
  )
pge_chart


library("sf")
library(tigris)
library(leaflet)



ca_counties <- counties("CA", cb = T, progress_bar = F)
st_crs(ca_counties)
projection <- "+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=ft +no_defs"

# ca_counties_transformed <-
#   ca_counties %>%
#   st_transform(4326) %>%
#   st_transform(26910) %>%
#   st_transform(projection) %>%
#   st_transform(st_crs(ca_counties))
# ggplot(ca_counties) + geom_sf()

bay_county_names <-
  c(
    "Alameda",
    "Contra Costa",
    "Marin",
    "Napa",
    "San Francisco",
    "San Mateo",
    "Santa Clara",
    "Solano",
    "Sonoma"
  )
bay_counties <-
  ca_counties %>%
  filter(NAME %in% bay_county_names)

# ggplot(bay_counties) + geom_sf()

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(
#     data = bay_counties %>%
#       st_transform(4326)
#   ) %>%
#   addMarkers(
#     data = bay_counties %>%
#       st_centroid() %>%
#       st_transform(4326)
#   )


# ca_cities <- places("CA", cb = T, progress_bar = FALSE)
# bay_cities <- ca_cities[bay_counties, ]
# 
# bay_cities_within <-
#   ca_cities %>%
#   st_centroid() %>%
#   .[bay_counties, ] %>%
#   st_set_geometry(NULL) %>%
#   left_join(ca_cities %>% select(GEOID)) %>%
#   st_as_sf()

bay_cbgs <- block_groups("CA", bay_county_names[1:9], cb = T, progress_bar = F)

ggplot(bay_cbgs)+geom_sf()

usa_zips <- zctas(cb = T, progress_bar = F)


bay_zips <-
  usa_zips %>%
  st_centroid() %>%
  .[bay_counties, ] %>%
  st_set_geometry(NULL) %>%
  left_join(usa_zips %>% select(GEOID10)) %>%
  st_as_sf()

Elec2019 <- filter(ElecMutate, YEAR %in% c("2019"))
Elec2020 <- filter(ElecMutate, YEAR %in% c("2020"))

Elec2019 <-     #2020
  Elec2019 %>%  #2020
  mutate(
    ZIPCODE = ZIPCODE %>% as.character()
  ) %>%
  mutate(
    ZIPCODE = ZIPCODE %>% as.character()
  ) %>%
  group_by(ZIPCODE) %>%
  summarize(
    TOTALKWH = sum(TOTALKWH, na.rm = T)
  ) %>%
  right_join(
    bay_zips %>% select(GEOID10),
    by = c("ZIPCODE" = "GEOID10")
  ) %>%
  st_as_sf() %>%
  st_transform(4326)

res_pal <- colorNumeric(
  palette = "Blues",
  domain =
    Elec2019$TOTALKWH   #2020
)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = Elec2019,        #2020
    fillColor = ~res_pal(TOTALKWH),
    color = "white",
    opacity = 0.5,
    fillOpacity = 0.5,
    weight = 1,
    label = ~paste0(
      round(TOTALKWH),
      " kWh total in ",
      ZIPCODE
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      opacity = 1
    )
  ) %>%
  addLegend(
    data = Elec2019,   #2020
    pal = res_pal,
    values = ~TOTALKWH,
    title = "Elctricy usage <br>kWh, June 2019"
  )

# pge_final  <-select(pge_final  ,!c( YEAR, TOTALCUSTOMERS))
# pge_final1  <-select(pge_final  ,!MONTH)
# plot_ly() %>% 
#   add_trace(
#     data = filter(pge_final,CUSTOMERCLASS == "Elec- Residential"),
#     x = factor(~MONTH),
#     y = AVERAGEkBTU,
#     type = "bar",
#     name = "EResidential"
#   ) %>% 
#   add_trace(
#     data = filter(pge_final,CUSTOMERCLASS == "Gas- Residential"),
#     x = factor(~MONTH),
#     y = AVERAGEkBTU,
#     type = "bar",
#     name = "GResidential"
#   ) %>% 
#   add_trace(
#     data = filter(pge_final,CUSTOMERCLASS == "Gas- RCommercial"),
#     x = factor(~MONTH),
#     y = AVERAGEkBTU,
#     type = "bar",
#     name = "GCommercial"
#   )%>%
#   add_trace(
#     data = filter(pge_final,CUSTOMERCLASS == "Elec- Commercial"),
#     x = factor(~MONTH),
#     y = AVERAGEkBTU,
#     type = "bar",
#     name = "ECommercial"
#   ) %>% 
#   layout(
#     xaxis = list(
#       title = "Month",
#       fixedrange = T
#     ),
#     yaxis = list(
#       title = "kBTU",
#       fixedrange = T
#     ),
#     barmode = "stack",
#     legend = list(title = list(text = "Electricity Type"))
#   ) %>% config(displayModeBar = F)

