## CDSS Getting and Cleaning Data
## Course Project check script

## Written by: Michael Gregory
## Date: 21-Jun-2015

##This script downloads the submitted data to confirm that the link works and the table is as expected.

fileURL <- "https://s3.amazonaws.com/coursera-uploads/user-caa8d9800f15027a97f05688/973502/asst-3/dd2d3220185d11e5ba5691b6c2cccabe.txt"
fileURL <- sub("^https", "http", fileURL)
projectOutput <- read.table(url(fileURL), header = TRUE)
View(projectOutput)
