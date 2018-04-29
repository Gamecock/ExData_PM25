#-----------------------------------------------------------------------------
#  Copyright (C) 2018  Michael F. Finch
#
#  Distributed under the terms of the MIT License.  The full license is in
#  the file LISCENCE.txt, distributed as part of this software.
#-----------------------------------------------------------------------------

plot4 <-function (){
  library(dplyr)
  
  if (!dir.exists("data")){
    dir.create("data")
  } 
  if (!file.exists("data/NEI.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "data/NEI.zip")
    unzip("data/NEI.zip", exdir = "data")
  }
  if(file.exists("data/Source_Classification_Code.rds")){
    SCC <- as_tibble(readRDS("data/Source_Classification_Code.rds"))
  } else {
    print("SCC Missing")
  }
  if(file.exists("data/summarySCC_PM25.rds")){
    PM25 <- as_tibble(readRDS("data/summarySCC_PM25.rds"))
  } else {
    print("PM25 Missing")
  }
  #create file for output
  if (!dir.exists("charts")){
    dir.create("charts")
  }
  #Create Plot 4
  coals <- SCC %>% mutate(SCC = as.character(SCC)) %>% filter(grepl("Coal", EI.Sector)) %>% 
    select(c(SCC, EI.Sector))
  coal_emissions <- semi_join(PM25, coals,"SCC") %>% group_by(year) %>% 
    summarise(total_25 = sum(Emissions,na.rm = TRUE))
  ggplot(data = coal_emissions, aes(year, total_25)) + geom_point(size = 3) +
    geom_smooth(method = "lm", formula= y~x, se=FALSE) +
    ggtitle("PM2.5 Particulate Matter From Coal")
  ggsave("charts/plot4.png")
}
