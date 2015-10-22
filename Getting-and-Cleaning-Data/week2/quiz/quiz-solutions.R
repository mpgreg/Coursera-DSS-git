## CDSS Getting and Cleaning Data
## Week 2 Quiz

##Question 1
##solution from tutorial at https://github.com/hadley/httr/blob/master/demo/oauth2-github.r
question1 <- function() {
        library(httr)
        library(jsonlite)
        endpoints <- oauth_endpoints("github")
        #keys from app created at https://github.com/settings/applications/210496
        myapp <- oauth_app("github", key = "c987b04e9e54d7850e9c", secret = "a9ca776328afedcdb2c6fae5ffaad3cf5fdd7c95")
        github_token <- oauth2.0_token(endpoints, myapp,cache=TRUE)
        gtoken <- config(token = github_token)
        url <- "https://api.github.com/users/jtleek/repos"
        req <- GET(url, gtoken)
        # OR:
        #req <- with_config(gtoken, GET(url))
        stop_for_status(req)
        jsonData <- content(req)
        jsonDF <- fromJSON(toJSON(jsonData))
        print(jsonDF[jsonDF$name=="datasharing","url"])
        print("created at: ")
        print(jsonDF[jsonDF$name=="datasharing","created_at"])
}

question2 <- function() {
        library(sqldf)
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
        destFile <- "ss06pid.csv"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        }
        acs <- read.csv(destFile)
        
        ##Which command will select only the data for the probability weights pwgtp1 with 
        ##ages less than 50?
        sqldf("select pwgtp1 from acs where AGEP < 50")
}

question3 <- function() {
        questions2()
        ##what is the equivalent function to unique(acs$AGEP)
        sqldf("select distinct AGEP from acs")
}

question4 <- function() {
        fileURL <- "http://biostat.jhsph.edu/~jleek/contact.html"
        destFile <- "contact.html"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        }
        htmlcode <- readLines(destFile)
        nchar(htmlcode[c(10,20,30,100)])        
}

question5 <- function() {
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
        destFile <- "wksst8110.for"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        }
        
        fwFile <- read.fwf(file=destFile, widths=c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)
        sum(fwFile[4])
}

