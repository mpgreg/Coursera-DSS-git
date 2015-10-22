## CDSS Getting and Cleaning Data
## Week 3 Quiz

setwd("~/Documents/School/coursera/data science/getting and cleaning data/week 3/quiz")

##Question 1
question1 <- function(printme = TRUE) {
        
        ##Create a logical vector that identifies the households on greater than 
        ##10 acres who sold more than $10,000 worth of agriculture products. Assign 
        ##that logical vector to the variable agricultureLogical. Apply the which() 
        ##function like this to identify the rows of the data frame where the logical 
        ##vector is TRUE. which(agricultureLogical) What are the first 3 values that 
        ##result?
        
        ##library(dplyr)
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
        destFile <- "ss06pid.csv"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        } 
        
        acs <- read.csv(destFile)
        acs$agricultureLogical <- ifelse(acs$ACR==3 & acs$AGS==6,TRUE,FALSE)
        
        if(printme) {
                print(head(row.names(acs[which(acs$agricultureLogical),]),3))
        }else return(acs)
        
}
question2 <- function(printme = TRUE) {
        
        ##Using the jpeg package read in the following picture of your instructor into R.
        ##Use the parameter native=TRUE. What are the 30th and 80th quantiles of the resulting 
        ##data? (some Linux systems may produce an answer 638 different for the 30th quantile)

        ## install.packages("jpeg")
        library(jpeg)
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
        destFile <- "jeff.jpg"
        if(!file.exists(destFile)) {
                download.file(fileURL,destfile=destFile,method="curl")
        } 
        
        jpegArray <- readJPEG(destFile,native=TRUE)
        if(printme) {
                print(quantile(jpegArray,probs=c(.3,.8)))
        } else return(jpegArray)
        
}

question3 <- function(printme=TRUE) {
        ##Load the Gross Domestic Product data for the 190 ranked countries in this data set: 
        ##      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv 
        ##Load the educational data from this data set: 
        ##      https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv 
        ##Match the data based on the country shortcode. How many of the IDs match? Sort the 
        ##data frame in descending order by GDP rank (so United States is last). What is the 
        ##13th country in the resulting data frame? 
        
        ##Original data sources: 
        ##      http://data.worldbank.org/data-catalog/GDP-ranking-table 
        ##      http://data.worldbank.org/data-catalog/ed-stats
        
        ## install.packages("dplyr")

        library(dplyr)

        gdpURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
        edStatsURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
        gdpFile <- "gdp.csv"
        edStatsFile <- "EDSTATS_Country.csv"
        
        if(!file.exists(gdpFile)) {
                download.file(gdpURL,destfile=gdpFile,method="curl")
        } 
        if(!file.exists(edStatsFile)) {
                download.file(edStatsURL,destfile=edStatsFile,method="curl")
        } 

        ##read in GDP table initially...its pretty ugly
        gdp <- read.table(gdpFile, sep=",", quote="\"", header=TRUE,skip=3,na.strings="",as.is=TRUE)
        ##remove the NA columns
        gdp <- select(gdp,-3,-(6:10))
        ##rename the country code column
        gdp <- rename(gdp,CountryCode=X,USD=US.dollars.)
        ##remove empty rows and needless comments
        gdp <- filter(gdp, !is.na(CountryCode))
        ##remove rows withtout ranking
        gdp <- filter(gdp, !is.na(Ranking))
        ##recast USD and Ranking columns as integers
        gdp <- mutate(gdp,USD=as.integer(gsub(",", "",USD)), Ranking=as.integer(Ranking))

        edStats <- read.csv(edStatsFile)
        
        mergeDF <- merge(gdp,edStats,by="CountryCode")
        mergeDF <- arrange(mergeDF,USD)
        if(printme) {
                print(nrow(mergeDF))
                print(mergeDF[13,"Economy"])
        }
        else return(mergeDF)
        
}

question4 <- function(printme=TRUE) {
        ##What is the average GDP ranking for the "High income: OECD" and "High income: nonOECD" group?
        library(dplyr)
        mergeDF <- question3(printme=FALSE)
        
        highIncomeDF <- filter(mergeDF, (Income.Group=="High income: OECD" | Income.Group=="High income: nonOECD"))
        highIncomeDF <- group_by(highIncomeDF,Income.Group)
        
        if(printme) {
                print(summarize(highIncomeDF,Mean=mean(Ranking)))
        }
        else return(mergeDF)
        
        
}

question5 <- function(printme=TRUE) {
        ##Cut the GDP ranking into 5 separate quantile groups. Make a table versus Income.
        ##Group. How many countries are Lower middle income but among the 38 nations with highest GDP?
        
        library(dplyr)
        mergeDF <- question3(printme=FALSE)
        ##add quantile column
        mergeDF$USD.quantile <- cut(mergeDF$USD, breaks=quantile(mergeDF$USD,probs=c(0,.2,.4,.6,.8,1)),include.lowest=TRUE)
        mergeDF$USD.qnum <- factor(mergeDF$USD.quantile,labels=c("1st","2nd","3rd","4th","5th"))
        
        ##grap the LMI countries with GDP ranking less than 38.  Could use 5th quantile but the question specifically
        ##asks for highest 38.
        lmIncome38 <- filter(mergeDF, Ranking <= 38 & Income.Group == "Lower middle income")
        
        if(printme) {
                print(table(mergeDF$USD.qnum, mergeDF$Income.Group))
                cat(sprintf("The following %d countries have a \"Lower middle income\" but are among the 38 highest GDP: \n\t%s", 
                            nrow(lmIncome38), paste(lmIncome38$Long.Name,collapse="\n\t")))
        } 
        else return(mergeDF)
        
}