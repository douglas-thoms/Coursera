# Define server logic required to draw a histogram
server <- function(input, output) {
        
        library(cancensus)
        options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
        census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')
        
        # census_api <- function(path) {
        #         url <- modify_url("https://censusmapper.ca/api", path = path)
        #         GET(url)
        # }
        # 
        #resp <- census_api("/repos/hadley/httr")
        #resp
        
        output$distPlot <- renderPlot({
                # generate bins based on input$bins from ui.R
                x    <- faithful[, 2] 
                bins <- seq(min(x), max(x), length.out = input$bins + 1)
                
                # draw the histogram with the specified number of bins
                hist(x, breaks = bins, col = 'darkgray', border = 'white')
        })
}