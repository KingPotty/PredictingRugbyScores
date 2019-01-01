setwd("C:/Users/tompo/OneDrive/Documents/Tom Potter/EMC 3")

#Loading the rvest package
library('rvest')

#Specifying the url for desired website to be scrapped
url <- 'https://www.bbc.co.uk/sport/rugby-union/english-premiership/results'

#Reading the HTML code from the website
webpage <- read_html(url)

#Get only the sections with the scores in
data_html <- html_nodes(webpage, '.gel-pb-')

Results <- html_text(data_html)

Results <- gsub("\u00A0", "~", Results, fixed = TRUE)

for (i in 1:length(Results))
{
  Results[i] <- gsub('  ', '~', Results[i])
  Results[i] <- strsplit(as.character(Results[i]), '~')
  remove <- ' '
  Results[i] <- setdiff(Results[i], remove)
}