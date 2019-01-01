setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

library('rvest')

url <- 'http://www.skysports.com/rugby-union/exeter-vs-saracens/87012'

#Reading the HTML code from the website
webpage <- read_html(url)

#Get only the sections with the scores in
data_html <- html_nodes(webpage, '.match-head__scorers')

Results <- html_text(data_html)

Results <- gsub("\u00A0", "~", Results, fixed = TRUE)

Results <- gsub("[\r\n ]", "", Results)

Results <- strsplit(Results, "[:()]")

Results <- unlist(Results)

NewResults <- c()

for (i in 1:length(Results))
{
  Results[i] <- paste(Results[i], "~", sep = "")
  if (length(unlist(strsplit(Results[i], "~"))) == 1)
  {
    Results[i] <- substr(Results[i], 1, nchar(Results[i]) - 1)
    NewResults <- append(NewResults, Results[i])
  }
}

Results <- NewResults