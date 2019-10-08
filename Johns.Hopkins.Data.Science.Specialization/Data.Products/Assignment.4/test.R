library(cancensus)
options(cancensus.api_key = "CensusMapper_7dd2122753cfb78fec890b61d909bdc4")
census_data <- get_census(dataset='CA16', regions=list(CMA="59933"), 
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"), level='CSD')
x <- list_census_datasets()
y <- list_census_regions("CA16")
z <- list_census_vectors("CA16")