#-----------------------------------------------------------------------------
#  Copyright (C) 2018  Michael F. Finch
#
#  Distributed under the terms of the MIT License.  The full license is in
#  the file LISCENCE.txt, distributed as part of this software.
#-----------------------------------------------------------------------------

plot2 <-function (){
  library(dplyr)
  
  if (!dir.exists("data")){
    dir.create("data")
  } 
  if (!file.exists("data/NEI.zip")){
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "data/NEI.zip")
    unzip("data/NEI.zip", exdir = "data")
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
  #Create Plot 2
  totals = PM25 %>% filter( fips =="24510") %>% group_by(year) %>% 
    summarise(total_25 = sum(Emissions, na.rm = TRUE))
  png("charts/Plot2.png", width = 400, height = 350)
  with(totals, plot(total_25~year))
  abline(lm(total_25~year, data = totals), col = "red")
  title(main = "Total PM 2.5 Emissions in Baltimore")
  dev.off()
}
