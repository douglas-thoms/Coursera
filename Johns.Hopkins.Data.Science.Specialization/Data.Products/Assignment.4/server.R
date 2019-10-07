# Define server logic required to draw a histogram
server <- function(input, output) {
        
        library(httr)
        census_api <- function(path) {
                url <- modify_url("https://censusmapper.ca/api", path = path)
                GET(url)
        }
        
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