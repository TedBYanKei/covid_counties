#!/usr/bin/env Rscript
# Created by Ted Braun
# For academic purposes only
# Downloaded covid data by US County by day downloaded from here:
#   https://github.com/nytimes/covid-19-data
#   (could have got on the fly but would have to mess with Github password passing so just downloaded)

library(choroplethr)
library(choroplethrMaps)
library(tidyverse)
library(here)
data(county.regions)

CurrDir <- here::here()
print(CurrDir)
DataDirFile <- paste(CurrDir, "/nytimes_data_us-counties.csv", sep = "", collapse = "")
Covid_Data <- read_csv(DataDirFile)
str(Covid_Data)
Covid_Data_MostRecent <- Covid_Data %>% 
  filter(!is.na(fips)) %>% 
  filter(date == max(date)) 

Joined_Data <- inner_join(county.regions, Covid_Data_MostRecent, by = c("county.fips.character" = "fips"))

Joined_Data <- Joined_Data %>% 
  rename(value = deaths) %>% 
  select(region, value)

county_choropleth(Joined_Data,
                  title ="  COVID Fatalities by US County", 
                  legend = "Deaths", num_colors = 9)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#This isn't fair though, as the big counties will naturally have more fatalities.
#  What if we want to look at fatalities per 100,000 people per county to make more comparable?
#https://catalog.data.gov/dataset/population-estimates
#https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html
CensusData <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
CensusData$StateStr <- sprintf("%02d", CensusData$STATE)
CensusData$CountyStr <- sprintf("%03d", CensusData$COUNTY)
CensusData$FIPS_Str <- with(CensusData, paste(StateStr, CountyStr, sep = ""))
head(CensusData, 10)
CensusData_Needed_Fields <- CensusData %>% 
  select(FIPS_Str, POPESTIMATE2019) %>% 
  rename(County_Population = POPESTIMATE2019)

Per100kData <- inner_join(county.regions, Covid_Data_MostRecent, by = c("county.fips.character" = "fips"))
Per100kData <- inner_join(Per100kData, CensusData_Needed_Fields, by = c("county.fips.character" = "FIPS_Str"))

Per100kData <- Per100kData %>% 
  filter(County_Population > 0) %>% 
  mutate(DeathsPer100k = round((deaths / County_Population * 100000), digits = 1)) 

Per100kData <- Per100kData %>% 
  rename(value = DeathsPer100k) %>% 
  select(region, value)

county_choropleth(Per100kData,
                  title ="  Current Deaths per 100,000 People by US County", 
                  legend = "Deaths per 100,000 people", num_colors = 9)
OutputDirFile <- paste(CurrDir, "/covid_us_county_current12.png", sep = "", collapse = "")


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#What if we want to look at data over time....?
Covid_Data$YMD <- as.character(format(Covid_Data$date, "%Y-%m-%d"))

All_Joined_Data <- inner_join(county.regions, Covid_Data, by = c("county.fips.character" = "fips"))
All_Joined_Data <- All_Joined_Data %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(deaths))%>% 
  arrange(YMD,region) %>% 
  select(YMD, region, deaths)

All_Joined_Data <- All_Joined_Data %>% complete(YMD, nesting(region), fill = list(deaths = 0))

vecDates <- sort(unique(as.vector(All_Joined_Data$YMD))) 

CurrDirFBD <- paste(CurrDir, "/fbd", sep = "", collapse = "")
for (aDt in vecDates) {
  aDay_of_Covid_Data <- All_Joined_Data %>% 
    filter(YMD == aDt) %>% 
    select(region, deaths) %>% 
    rename(value = deaths)
    OutputDirFile <- paste(CurrDirFBD, "/Fatalities_by_day_", aDt, ".png", sep = "", collapse = "")
  str(aDt)
  strTitle <- paste("Date: ", aDt, sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 700)
  p <- county_choropleth(aDay_of_Covid_Data, title = strTitle, legend="Deaths", num_colors = 9)
  print(p)
  dev.off()
}

#Now to paste all of the files together into a .mp4 and a gif file.  
#Use ImageMagick application to do the stitching together
#Directory to output to, then issue an ImageMagick 'convert' command (non-R) to "paste" the .png files together
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_01.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_01.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#How about the same but doing by day per 100k...
Per100kData_AllDays  <- inner_join(county.regions, Covid_Data, by = c("county.fips.character" = "fips"))
Per100kData_AllDays <- inner_join(Per100kData_AllDays, CensusData_Needed_Fields, by = c("county.fips.character" = "FIPS_Str"))
Per100kData_AllDays <- Per100kData_AllDays %>% 
  filter(County_Population > 0) %>% 
  mutate(DeathsPer100k = round((deaths / County_Population * 100000), digits = 1)) 


Per100kData_AllDays <- Per100kData_AllDays %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(deaths))%>% 
  arrange(YMD, region) %>% 
  select(YMD, region, DeathsPer100k)

Per100kData_AllDays <- Per100kData_AllDays %>% complete(YMD, nesting(region), fill = list(DeathsPer100k = 0))

vecDates <- sort(unique(as.vector(Per100kData_AllDays$YMD))) 

CurrDirFBD <- paste(CurrDir, "/fbd_per100k", sep = "", collapse = "")
for (aDt in vecDates) {
  aDay_of_Covid_Data <- Per100kData_AllDays %>% 
    filter(YMD == aDt) %>% 
    select(region, DeathsPer100k) %>% 
    rename(value = DeathsPer100k)
  OutputDirFile <- paste(CurrDirFBD, "/Fatalities_by_day_Per_100k_", aDt, ".png", sep = "", collapse = "")
  str(aDt)
  strTitle <- paste("Date: ", aDt, sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 700)
  p <- county_choropleth(aDay_of_Covid_Data, title = strTitle, legend="Deaths", num_colors = 9)
  print(p)
  dev.off()
}


command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_PerCapita_01.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_PerCapita_01.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))

#To illustrate an example, playing around
vecDates_Subset <- tail(vecDates,20)

CurrDirFBD <- paste(CurrDir, "/just_clowning", sep = "", collapse = "")
for (aDt in vecDates_Subset) {
  aDay_of_Covid_Data <- Per100kData_AllDays %>% 
    filter(YMD == aDt) %>% 
    select(region, DeathsPer100k) %>% 
    rename(value = DeathsPer100k)
  OutputDirFile <- paste(CurrDirFBD, "/Fatalities_by_day_Per_100k_JC_", aDt, ".png", sep = "", collapse = "")
  str(aDt)
  strTitle <- paste("Date: ", aDt, sep = "", collapse = "")
  png(filename = OutputDirFile, width = 1100, height = 700)
  p <- county_choropleth(aDay_of_Covid_Data, title = strTitle, legend="Deaths", num_colors = 9)
  print(p)
  dev.off()
}

command <- paste("cd ", CurrDirFBD, " && convert -delay 25 *.png Fatalities_by_day_PerCapita_JC_01.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_PerCapita_JC_01.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
