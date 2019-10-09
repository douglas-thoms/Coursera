#https://mountainmath.github.io/cancensus/articles/Making_maps_with_cancensus.html

library(cancensus)
library(sf)
library(leaflet)
library(dplyr)
options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
options(cancensus.cache_path = getwd())

# retrieve sf dataframe

census_variables <- c("v_CA16_5051","v_CA16_5054","v_CA16_5057","v_CA16_5060","v_CA16_5063",
                      "v_CA16_5066","v_CA16_5069","v_CA16_5072","v_CA16_5075",
                      "v_CA16_5078","v_CA16_5081","v_CA16_5084","v_CA16_5087",
                      "v_CA16_5090","v_CA16_5093")

census_data <- get_census(dataset='CA16', regions=list(C="01"), labels = "short", 
                          vectors="v_CA16_5051", level='PR', geo_format='sf')

census_data <- census_data %>%
        mutate(map_data = (v_CA16_5081/Population)*100)

pal <- colorNumeric("RdYlBu", domain = census_data$map_data)



my_map <- leaflet(census_data) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(map_data),
                    color = "white",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.65) %>%
        addLegend("bottomright", pal = pal, values = ~map_data,
                  title = "Population",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1
        )

my_map
