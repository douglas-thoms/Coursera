# Define server logic required to draw a histogram
server <- function(input, output,session) {
        
        
        options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
        options(cancensus.cache_path = getwd())
        
        census_variables <- c("v_CA16_5051","v_CA16_5054","v_CA16_5057","v_CA16_5060","v_CA16_5063",
                              "v_CA16_5066","v_CA16_5069","v_CA16_5072","v_CA16_5075",
                              "v_CA16_5078","v_CA16_5081","v_CA16_5084","v_CA16_5087",
                              "v_CA16_5090","v_CA16_5093")

        
# CAN Select position
        
        output$map <- renderLeaflet({
                #base map on select input
                
                #calculate population that have this degree as their highest education
                
                census_data <- get_census(dataset='CA16', regions=list(C="01"), labels = "short", 
                                          vectors=census_variables, level=input$area, geo_format='sf')
                
                census_data <- census_data %>%
                        mutate(map_data = (!!as.symbol(input$variable)/Population)*100)
                
                pal <- colorNumeric("RdYlBu", domain = census_data$map_data)
                
                leaflet(census_data) %>% 
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
                
                # # generate bins based on input$bins from ui.R
                # x    <- faithful[, 2] 
                # bins <- seq(min(x), max(x), length.out = input$bins + 1)
                # 
                # # draw the histogram with the specified number of bins
                # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
}