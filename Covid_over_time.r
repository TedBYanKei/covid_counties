#!/usr/bin/env Rscript
#https://github.com/nytimes/covid-19-data
#

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

str(county.regions)
str(Covid_Data_MostRecent)

str(Joined_Data)

Joined_Data <- inner_join(county.regions, Covid_Data_MostRecent, by = c("county.fips.character" = "fips"))
tjd <- cbind(Joined_Data)
Joined_Data <- Joined_Data %>% 
  rename(value = deaths) %>% 
  select(region, value)

county_choropleth(Joined_Data,
                  title ="  COVID Fatalities by US County", 
                  legend = "Deaths", num_colors = 9)


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


Joined_Data <- inner_join(county.regions, Covid_Data_MostRecent, by = c("county.fips.character" = "fips"))
Joined_Data <- inner_join(Joined_Data, CensusData_Needed_Fields, by = c("county.fips.character" = "FIPS_Str"))
Joined_Data <- Joined_Data %>% 
  filter(CountyPopul > 0) %>% 
  mutate(DeathsPer100k = round((deaths / CountyPopul * 100000), digits = 1)) 

Joined_Data <- Joined_Data %>% 
  rename(value = DeathsPer100k) %>% 
  select(region, value)

str(Joined_Data)
tjd_b100k <- cbind(Joined_Data)
OutputDirFile <- paste(CurrDir, "/covid_us_county_current12.png", sep = "", collapse = "")
png(filename = OutputDirFile, width = 1100, height = 700)
p <- county_choropleth(Joined_Data,
                       title ="  Current Deaths per 100,000 People by US County", 
                       legend = "Deaths per 100,000 people", num_colors = 9)
print(p)
dev.off()


#What if we want to look at data over time....?

str(Covid_Data)
Covid_Data$YMD <- as.character(format(Covid_Data$date, "%Y-%m-%d"))

All_Joined_Data <- inner_join(county.regions, Covid_Data, by = c("county.fips.character" = "fips"))
All_Joined_Data <- All_Joined_Data %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(deaths))%>% 
  arrange(YMD,region) %>% 
  select(YMD, region, deaths)

All_Joined_Data <- All_Joined_Data %>% complete(YMD, nesting(region), fill = list(deaths = 0))

vecDates <- sort(unique(as.vector(All_Joined_Data$YMD))) 
#vecDates <- tail(vecDates,5)
str(All_Joined_Data)
str(vecDates)
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

str(CurrDir)


#Directory to output to, then issue an ImageMagick 'convert' command (non-R) to "paste" the .png files together
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_01.mp4", sep = "", collapse = "")
system(paste("cmd.exe /c", command))
command <- paste("cd ", CurrDirFBD, " && convert -delay 15 *.png Fatalities_by_day_01.gif", sep = "", collapse = "")
system(paste("cmd.exe /c", command))


