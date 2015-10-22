## CDSS Getting and Cleaning Data
## Week 4 Quiz

setwd("~/Documents/School/coursera/data science/getting and cleaning data/week 4/quiz")

question1 <- function(printme = TRUE) {
        
        ##Apply strsplit() to split all the names of the data frame on the characters 
        ##"wgtp". What is the value of the 123 element of the resulting list?

        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        destFile <- "ss06hid.csv"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        } 
        
        acs <- read.csv(destFile)
        
        if(printme) {
                print(strsplit(names(acs),"wgtp")[[123]])
        }else return(acs)        
}

question2 <- function(printme = TRUE) {
        
        ##Remove the commas from the GDP numbers in millions of dollars and 
        ##average them. What is the average? 

        ##Original data sources: http://data.worldbank.org/data-catalog/GDP-ranking-table

        library(dplyr)

        gdpURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        gdpFile <- "gdp.csv"
        
        if(!file.exists(gdpFile)) {
                download.file(gdpURL,destfile=gdpFile,method="curl")
        } 
        
        ##read in GDP table initially...its pretty ugly
        gdp <- read.table(gdpFile, sep=",", quote="\"", header=TRUE,skip=3,na.strings="")
        ##rename the country code column
        gdp <- rename(gdp,CountryCode=X,USD=US.dollars.)
        ##retain only rows with ranking and GDP
        gdp <- filter(gdp, !is.na(Ranking) & !is.na(USD))
        
        gdp$USD <- as.numeric(gsub(",","",gdp$USD))
        
        if(printme) {
                print(mean(gdp$USD))
        } else return(gdp)
        
}

question3 <- function(printme=TRUE) {
        ##In the data set from Question 2 what is a regular expression that would allow 
        ## you to count the number of countries whose name begins with "United"? 
        ##Assume that the variable with the country names in it is named countryNames. 
        ##How many countries begin with United?

        gdp <- question2(printme=FALSE)
        countryNames <- gdp$Economy
        count <- length(grep("^United", countryNames, useBytes=TRUE))
        
        if(printme) {
                cat(sprintf("grep(^United, countryNames, useBytes=TRUE, %d",count))
        } else return(gdp)
        
}

question4 <- function(printme=TRUE) {
        ##Load the Gross Domestic Product data for the 190 ranked countries in this 
        ##data set: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
        ##Load the educational data from this data set: 
        ##https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
        ##
        ##Match the data based on the country shortcode. Of the countries for which 
        ##the end of the fiscal year is available, how many end in June? 

        ##Original data sources: 
                ##http://data.worldbank.org/data-catalog/GDP-ranking-table 
                ##http://data.worldbank.org/data-catalog/ed-stats

        library(dplyr)
        mergeDF <- question3(printme=FALSE)
        
        edStatsURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        edStatsFile <- "EDSTATS_Country.csv"
         
        if(!file.exists(edStatsFile)) {
                download.file(edStatsURL,destfile=edStatsFile,method="curl")
        } 

        edStats <- read.csv(edStatsFile)
        mergeDF <- merge(gdp,edStats,by="CountryCode")

        fyReport <- mergeDF[grep("fiscal year end", mergeDF$Special.Notes, ignore.case = T),12]
        juneReport <- grep("June",fyReport,value = T)

        if(printme) {
                print(length(juneReport))
        } else return(mergeDF)
}

question5 <- function(printme=TRUE) {
        ##You can use the quantmod (http://www.quantmod.com/) package to get historical 
        ##stock prices for publicly traded companies on the NASDAQ and NYSE.
        
        ##How many values were collected in 2012? How many values were collected on 
        ##Mondays in 2012?
        
        ##install.packages("quantmod")
        library(quantmod)
        amzn = getSymbols("AMZN",auto.assign=FALSE)
        sampleTimes = index(amzn)
        
        year2012 <- sampleTimes[lapply(sampleTimes,format,"%Y")=="2012"]
        monday2012 <-  year2012[lapply(year2012,weekdays)=="Monday"]
        
        if(printme) {
                cat(sprintf("%d, %d", length(year2012), length(monday2012)))
        } 
        else return(sampleTimes)
        
}