## Coursera Data Science Specialization Reproducible Research
## Project 2 code


## Written by: Michael Gregory
## Date: 22-Oct-2015

## Original data file at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
##This assignment makes use of data from a NOAA study on storm events from 1950 to 2011.  
##Documentation is available at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf with 
##an FAQ at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf


setwd("~/Documents/School/coursera/data science/reproducible research/project2/")

##fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
##tempFile <- tempfile()
##if(capabilities("libcurl")) {
##        download.file(fileURL, tempFile, method = "libcurl")        
##} else {
##        download.file(fileURL, tempFile)
##}

##--------------------------------SWITCH THIS TO LIVE DOWNLOAD------------------------------------------
tempFile <- "StormData.csv.bz2"

#To cut down on processing and cleaning time import only relevant columns of data. 
#Grab the headers list.
headers <- read.csv(tempFile, nrows = 1)
#Ignore all columns except the ones we specify.
headers[,] <- c(rep("NULL", ncol(headers)))
headers[c("FATALITIES", "INJURIES", "PROPDMG", "CROPDMG")] <- "numeric"
headers[c("BGN_DATE", "PROPDMGEXP", "CROPDMGEXP")] <- "factor"
headers[c("EVTYPE")] <- "character"
#Read in the specified columns
##--------------------------------SWITCH THIS TO LIVE DOWNLOAD------------------------------------------
##StormDF <- read.table(tempFile, header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "", colClasses = headers)
##unlink(tempFile)

StormDF <- read.table(tempFile, header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "", colClasses = headers, nrows = 902298)

##preprocess the data
##Assign better column names
names(StormDF)[names(StormDF) == "EVTYPE"] <- "Event.Type"
names(StormDF)[names(StormDF) == "BGN_DATE"] <- "Begin.Date"
names(StormDF)[names(StormDF) == "FATALITIES"] <- "Fatalities"
names(StormDF)[names(StormDF) == "INJURIES"] <- "Injuries"
names(StormDF)[names(StormDF) == "PROPDMG"] <- "Property.Damage"
names(StormDF)[names(StormDF) == "PROPDMGEXP"] <- "Property.Damage.Exponent"
names(StormDF)[names(StormDF) == "CROPDMG"] <- "Crop.Damage"
names(StormDF)[names(StormDF) == "CROPDMGEXP"] <- "Crop.Damage.Exponent"

#Drop all rows with no Health or Economic loss
StormDF <- StormDF[!rowSums(StormDF[c("Fatalities","Injuries","Property.Damage","Crop.Damage")])==0,]

##Check if there NAs
anyNA(StormDF)

##Are the Property Damage and Crop Damage exponent fields accurate?  If not do the incorrect ones represent a statistically relevant set?
allowedExp <- c("k", "K", "M", "m", "B", "b", "0", "0.0", "0.00", NA, "")
percErrors <- round(nrow(subset(StormDF, !Property.Damage.Exponent %in% allowedExp | !Crop.Damage.Exponent %in% allowedExp)) / nrow(StormDF), 5)*100
cat(sprintf("Events with erroneous exponent fields represent %s percent of the data.\n", percErrors))


##Calculate the full property damage cost by multiplying the exponent
StormDF["Property.Damage.FULL"] <- StormDF["Property.Damage"]
StormDF[StormDF$Property.Damage.Exponent %in% c("k","K"),"Property.Damage.FULL"] <- StormDF[StormDF$Property.Damage.Exponent %in% c("k","K"),"Property.Damage"]*1000
StormDF[StormDF$Property.Damage.Exponent %in% c("m","M"),"Property.Damage.FULL"] <- StormDF[StormDF$Property.Damage.Exponent %in% c("m","M"),"Property.Damage"]*1000000
StormDF[StormDF$Property.Damage.Exponent %in% c("b","B"),"Property.Damage.FULL"] <- StormDF[StormDF$Property.Damage.Exponent %in% c("b","B"),"Property.Damage"]*1000000000
#Do the same for crop damage
StormDF["Crop.Damage.FULL"] <- StormDF["Crop.Damage"]
StormDF[StormDF$Crop.Damage.Exponent %in% c("k","K"),"Crop.Damage.FULL"] <- StormDF[StormDF$Crop.Damage.Exponent %in% c("k","K"),"Crop.Damage"]*1000
StormDF[StormDF$Crop.Damage.Exponent %in% c("m","M"),"Crop.Damage.FULL"] <- StormDF[StormDF$Crop.Damage.Exponent %in% c("m","M"),"Crop.Damage"]*1000000
StormDF[StormDF$Crop.Damage.Exponent %in% c("b","B"),"Crop.Damage.FULL"] <- StormDF[StormDF$Crop.Damage.Exponent %in% c("b","B"),"Crop.Damage"]*1000000000

#Add Property and Crop Damage
StormDF["Economic.Impact"] <- StormDF$Property.Damage.FULL + StormDF$Crop.Damage.FULL

#Add Fatalities and Injuries to summarize personal health impact.
StormDF["Health.Impact"] <- StormDF$Fatalities + StormDF$Injuries

##The dataset has three distinct collection periods.  
#From 1950 to 1954 only Tornado events were collected.  
#From 1955 through 1995, only tornado, thunderstorm wind and hail events were collected. From 
#From 1996 to present (2011), 48 event types are recorded as defined in NWS Directive 10-1605 (http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf) were collected.

#Reassign the Date column to allow for subsetting according to these collection periods.  We don't need to factor in the TIMEZONE column for the sake of subsetting at a rough level.
StormDF$Begin.Date <- as.Date(StormDF$Begin.Date, format = "%m/%d/%Y")

#Create factor variable for subsetting.
StormDF[which(StormDF$Begin.Date <= as.Date("1954-12-31")),"Reporting.Period"] <- "rp1950to1955"
StormDF[which(StormDF$Begin.Date > as.Date("1954-12-31") & StormDF$Begin.Date <= as.Date("1995-12-31")),"Reporting.Period"] <- "rp1955to1996"
StormDF[which(StormDF$Begin.Date > as.Date("1995-12-31")),"Reporting.Period"] <- "rp1996toPres"
StormDF$Reporting.Period <- as.factor(StormDF$Reporting.Period)



#Check the validity and consistency of the "Event Type" column.  According to the documentation there are 48 "correct" storm event types.
#Create a list of allowed (known) Event Types from page 6 of http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf
allowedEvents <- toupper(c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", 
                           "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", 
                           "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", 
                           "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", 
                           "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", 
                           "Waterspout", "Wildfire", "Winter Storm", "Winter Weather"))


##alternatively generate programatically
##library(tm)
##docFileURL <- "http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf"
##docTempFile <- tempfile()
##if(capabilities("libcurl")) {
##        download.file(docFileURL, docTempFile, method = "libcurl")        
##} else {
##        download.file(docFileURL, docTempFile)
##}
##
##docTempFile <- "pd01016005curr.pdf"
##docPDF <- readPDF(control = list(c(text = "-layout")))
##docPDF <- pdf(elem=list(uri=docTempFile),language="en")
##c(pdf$content[seq(397, 420)], pdf$content[seq(425, 448)])
##unlink(docTempFile)


#How many Event Types are listed in the dataset?
StormDF$Event.Type <- toupper(StormDF$Event.Type)
cat(sprintf("There are %s unique \"events\" in the data.\n", nrow(unique(StormDF$Event.Type))))

percErrors <- round(nrow(subset(StormDF, !Event.Type %in% allowedEvents)) / nrow(StormDF), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the data.\n", percErrors))

#What is the magnitude of the economic and health impact for these "non-standard" events?
skewEconomicImpact <- round(sum(subset(StormDF, !Event.Type %in% allowedEvents)$Economic.Impact) 
                            / sum(StormDF$Economic.Impact), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the total Economic Impact of all storm events.\n", skewEconomicImpact))

skewHealthImpact <- round(sum(subset(StormDF, !Event.Type %in% allowedEvents)$Health.Impact) 
                         / sum(StormDF$Health.Impact), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the total Health Impact of all storm events.\n", skewHealthImpact))


#Lets clean up some of the more egregious and obvious mis-labeled event types.
#NOTE: This needs to be done according to a specific order because of some events that have multiple event types in the name (ie. Thunderstorm / Hail).
#The below order is based on assesment of the direct vs indirect effects of Economic and Health impact in decreasing order of magnitude.
#1) Primary cause: The major event ("Blizzard", "Funnel Cloud", "Hurricane (Typhoon)", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", 
#"Wildfire", "Winter Storm") are the causal events for secondary events such as wind, rain, flood, etc.  Also marine events are a special subset to start with.

StormDF[grep("WATERS", StormDF$Event.Type),"Event.Type"] <- "WATERSPOUT"
StormDF[grepl("^\\s*TORNA", StormDF$Event.Type),"Event.Type"] <- "TORNADO"
StormDF[grep("(HUR|TYP)", StormDF$Event.Type),"Event.Type"] <- "HURRICANE (TYPHOON)"
StormDF[grep("TROPIC.*STORM", StormDF$Event.Type),"Event.Type"] <- "TROPICAL STORM"
StormDF[grep("BLIZ", StormDF$Event.Type),"Event.Type"] <- "BLIZZARD"
StormDF[grep("FUNNEL.*CLO", StormDF$Event.Type),"Event.Type"] <- "FUNNEL CLOUD"
StormDF[grep("TSU", StormDF$Event.Type),"Event.Type"] <- "TSUNAMI"
StormDF[grep("FIRE", StormDF$Event.Type),"Event.Type"] <- "WILDFIRE"
StormDF[grepl("WINTER", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("WINTER WEATHER"),"Event.Type"] <- "WINTER STORM"

#2) Secondary cause: A primary event may have multiple secondary causes for damage costal flood, flash flood, flood, hail, wind, storm surge, high surf, 
#high winds, etc that infer a precedance on the impact that they have.
#Cleanup the various flood types.
StormDF[grep("(COASTAL.*FLOOD|CSTL.*FLOOD|TID.*FLOOD)", StormDF$Event.Type),"Event.Type"] <- "COASTAL FLOOD"
StormDF[grep("BEACH.*FLOOD", StormDF$Event.Type),"Event.Type"] <- "COASTAL FLOOD"
StormDF[grep("(FLASH.*FLOOD|FLOOD.*FLASH)", StormDF$Event.Type),"Event.Type"] <- "FLASH FLOOD"
StormDF[grepl("LAKE.*FLOOD", StormDF$Event.Type),"Event.Type"] <- "LAKESHORE FLOOD"
StormDF[grepl("(FLOOD|FLD)", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("FLASH FLOOD", "COASTAL FLOOD", "LAKESHORE FLOOD"),"Event.Type"] <- "FLOOD"

#Cleanup others
StormDF[grepl("HAIL", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("MARINE HAIL"),"Event.Type"] <- "HAIL"
StormDF[grep("HEAVY RAIN", StormDF$Event.Type),"Event.Type"] <- "HEAVY RAIN"
StormDF[grep("LAKE.*SNOW", StormDF$Event.Type),"Event.Type"] <- "LAKE-EFFECT SNOW"
StormDF[grep("HEAVY SNOW", StormDF$Event.Type),"Event.Type"] <- "HEAVY SNOW"
StormDF[grep("MAR.*TSTM", StormDF$Event.Type),"Event.Type"] <- "MARINE THUNDERSTORM WIND"
StormDF[grepl("(THUNDERST|^\\s*TSTM)", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("MARINE THUNDERSTORM WIND"),"Event.Type"] <- "THUNDERSTORM WIND"
StormDF[grep("(EXT.*COLD|EXT.*WIND.*CHIL)", StormDF$Event.Type),"Event.Type"] <- "EXTREME COLD/WIND CHILL"
StormDF[grepl("WIND.*CHI", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("EXTREME COLD/WIND CHILL"),"Event.Type"] <- "COLD/WIND CHILL"
StormDF[grep("STRONG.*WIND", StormDF$Event.Type),"Event.Type"] <- "STRONG WIND"
StormDF[grep("HIGH.*WIND", StormDF$Event.Type),"Event.Type"] <- "HIGH WIND"
StormDF[grep("ICE FOG", StormDF$Event.Type),"Event.Type"] <- "FREEZING FOG"
StormDF[grepl("FOG", StormDF$Event.Type) & 
                !StormDF$Event.Type %in% c("FREEZING FOG"),"Event.Type"] <- "DENSE FOG"
StormDF[grep("^SNOW$", StormDF$Event.Type),"Event.Type"] <- "HEAVY SNOW"
StormDF[grep("^STORM.*SURGE$", StormDF$Event.Type),"Event.Type"] <- "STORM SURGE/TIDE"
StormDF[grep("^FREEZING RAIN$", StormDF$Event.Type),"Event.Type"] <- "ICE STORM"
StormDF[grep("^WIND$", StormDF$Event.Type),"Event.Type"] <- "HIGH WIND"
StormDF[grep("^LANDSLIDE$", StormDF$Event.Type),"Event.Type"] <- "HEAVY RAIN"
StormDF[grep("RIP.*CUR", StormDF$Event.Type),"Event.Type"] <- "RIP CURRENT"
StormDF[grep("WARM", StormDF$Event.Type),"Event.Type"] <- "HEAT"
StormDF[grep("HEAVY SURF/HIGH SURF", StormDF$Event.Type),"Event.Type"] <- "HIGH SURF"
StormDF[grep("HIGH TIDE", StormDF$Event.Type),"Event.Type"] <- "STORM SURGE/TIDE"

#After doing some cleanup what is the magnitude of mis-classified events?
cat(sprintf("There are %s unique \"events\" in the data.\n", nrow(unique(StormDF$Event.Type))))

percErrors <- round(nrow(subset(StormDF, !Event.Type %in% allowedEvents)) / nrow(StormDF), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the data.\n", percErrors))

skewEconomicImpact <- round(sum(subset(StormDF, !Event.Type %in% allowedEvents)$Economic.Impact) 
                            / sum(StormDF$Economic.Impact), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the total Economic Impact of all storm events.\n", skewEconomicImpact))

#What is the magnitude of the economic and health impact for these "non-standard" events?
skewHealthImpact <- round(sum(subset(StormDF, !Event.Type %in% allowedEvents)$Health.Impact) 
                          / sum(StormDF$Health.Impact), 3)*100
cat(sprintf("Events with \"non-standard\" event fields represent %s percent of the total Health Impact of all storm events.\n", skewHealthImpact))

###NEED to subset by collection period

#Build aggregates for cross-tabulation of fatalities, injuries, and total.
require(reshape2)
meltedDF <- melt(StormDF, id.vars = "Event.Type", 
                          measure.vars = c("Fatalities", "Injuries", "Health.Impact", "Property.Damage.FULL", "Crop.Damage.FULL", "Economic.Impact"))
summaryDF <- dcast(meltedDF, Event.Type ~ variable, sum)
rownames(summaryDF) <- summaryDF$Event.Type

barplot(head(sort(summaryDF[summaryDF$Economic.Impact > quantile(summaryDF$Economic.Impact)[[4]],"Economic.Impact"], decreasing = TRUE), 10))

maxEconomicStorm <- StormDF[which.max(StormDF$Economic.Impact),]
maxEconomicStormType <- summaryDF[which.max(summaryDF$Economic.Impact),]
maxHealthStorm <- StormDF[which.max(StormDF$Health.Impact),]
maxHelathStormType <- summaryDF[which.max(summaryDF$Health.Impact),]


