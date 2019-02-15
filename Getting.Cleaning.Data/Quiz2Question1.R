
get_API <- function(){

library(httpuv)
library(httr)
library(dplyr)

#set up end point with object and credentials
myapp <- oauth_app("github", 
                   key = "083391989edaa9839101",
                   secret = "0e94e4d868072f464e117fb5f16c7571ab8360ac"
                   )

#get )authcredentials
github_token <- oauth1.0_token(oauth_endpoints("github"), myapp)


#Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(req)

json1 <- content(req)
json2 <- jsonlite::fromJSON(jsonlite::toJSON(json1))

json2 <- filter(json2, name == "datasharing")

return(json2)
}