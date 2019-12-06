#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ngram)
library(stringr)
library(purrr)
library(dplyr)
library(DT)
library(quanteda)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # url <- a("Google Homepage", href="https://www.google.com/")
  # output$tab <- renderUI({
  #               tagList("URL link:", url)
  #               })
        
  output$final.results <- DT::renderDataTable({
          
          #if loop that waits for complete words based on space
          if(str_sub(input$entry.sentence, start = -1) == " "){
                #need a loop in here so only reacts on blank white space
          
                  #prep search terms
                  entry.cleaned <- input$entry.sentence %>%
                          tokens(remove_punct = TRUE,
                                 remove_numbers = TRUE,
                                 remove_symbols = TRUE,
                                 remove_url = TRUE,
                                 remove_twitter = TRUE
                          ) %>%
                          tokens_tolower()
                  #tokens_select(stopwords('english'),selection='remove')
                  entry.cleaned <- paste(entry.cleaned[[1]],collapse=" ")
                  print(entry.cleaned)
                  
                  #determine string length
                  entry.cleaned.length <- wordcount(entry.cleaned)
                  
                  #if over 4, truncuate to last 4 words
                  if (entry.cleaned.length >4) {
                          entry.cleaned <- word(entry.cleaned, start = entry.cleaned.length-3, end = entry.cleaned.length)
                          entry.cleaned.length <- 4
                  }
                  
                  #changes blank spaces to hyphen to match look up tables
                  entry.cleaned <- gsub(" ", "_", entry.cleaned)
                  
                  #create loop to break entry sentence into ngram to unigram
                  output <- NULL
                  
                  for(i in 1:entry.cleaned.length) {
                          #generate ngrams from blank to pentagram
                          output <- c(output, word(entry.cleaned, start = i, end = entry.cleaned.length, sep = "_"))
                  }
                  
                  output <- c(output,NA)
                  
                  
                  predictions <- NULL
                  for(i in 1:entry.cleaned.length+1){
                          
                          df <- final.values[.(output[i]), nomatch = 0L]
                          predictions <- rbind(predictions,df)
                          
                  }
                  
                  
                  #get last word
                  predictions <- predictions %>%
                          mutate(new.word = map_chr(name,word, start = -1, sep = "_")) %>%
                          group_by(new.word) %>%
                          summarise(score = sum(score)) %>%
                          ungroup %>%
                          #remove stop words
                          filter(!(new.word %in% stopwords("english"))) %>%
                          arrange(desc(score)) %>%
                          mutate(predicted.sentence = paste(input$entry.sentence,new.word)) %>%
                          slice(1:input$number.prediction) %>%
                          transform(score = round(score,4)) %>%
                          select(predicted.sentence,score) %>%
                          rename(Predicted.Sentences = predicted.sentence,
                                 Score = score)
          
          }else{
                  predictions <- data.frame(Predicted.Sentences = "Please put in space after last word",
                                            "Score" = NA)
          }
  })

})
