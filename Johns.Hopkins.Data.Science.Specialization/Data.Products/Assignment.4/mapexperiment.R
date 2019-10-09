#https://mountainmath.github.io/cancensus/articles/Making_maps_with_cancensus.html

library(cancensus)
library(sf)
library(leaflet)
options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
options(cancensus.cache_path = 'C:/Users/Douglas/Documents/Coursera/Johns.Hopkins.Data.Science.Specialization/Data.Products/Assignment.4')

# retrieve sf dataframe

census_variables <- c("v_CA16_5051","v_CA16_5054","v_CA16_5057","v_CA16_5060","v_CA16_5063",
                      "v_CA16_5066","v_CA16_5069","v_CA16_5072","v_CA16_5075",
                      "v_CA16_5078","v_CA16_5081","v_CA16_5084","v_CA16_5087",
                      "v_CA16_5090","v_CA16_5093")

census_data <- get_census(dataset='CA16', regions=list(C="01"), labels = "short", 
                          vectors=census_variables, level='CSD', geo_format='sf')

pal <- colorNumeric("RdYlBu", domain = census_data$v_CA16_5087)



my_map <- leaflet(census_data) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(v_CA16_5087),
                    color = "white",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.65) %>%
        addLegend("bottomright", pal = pal, values = ~v_CA16_5087,
                  title = "Population",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1
        )

my_map
