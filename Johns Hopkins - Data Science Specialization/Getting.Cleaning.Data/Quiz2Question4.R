reading_http <- function(){
  
  con <- url("http://biostat.jhsph.edu/~jleek/contact.html")
  htmlCode = readLines(con)
  close(con)
  return(htmlCode)
  
}

get_character_length <- function(line_number){
  
  content <- reading_http()
  
  return(nchar(content[line_number]))
  
  
}

10 - 45
20- 31
30 - 7
100 - 25