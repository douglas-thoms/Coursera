#https://mountainmath.github.io/cancensus/articles/Making_maps_with_cancensus.html

library(cancensus)
library(sf)
library(leaflet)
options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
# retrieve sf dataframe
toronto <- get_census(dataset='CA16', regions=list(CMA="35535"),
                      vectors=c("median_hh_income"="v_CA16_2397"), level='CSD', quiet = TRUE, 
                      geo_format = 'sf', labels = 'short')

bins <- c(0, 30000,40000, 50000,60000, 70000,80000, 90000,100000, 110000, Inf)
pal <- colorBin("RdYlBu", domain = toronto$v_CA16_2397, bins = bins)
#> Warning: Unknown or uninitialised column: 'v_CA16_2397'.
leaflet(toronto) %>% 
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(median_hh_income),
                    color = "white",
                    weight = 1,
                    opacity = 1,
                    fillOpacity = 0.65)
