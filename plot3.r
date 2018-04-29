#-----------------------------------------------------------------------------
#  Copyright (C) 2018  Michael F. Finch
#
#  Distributed under the terms of the MIT License.  The full license is in
#  the file LISCENCE.txt, distributed as part of this software.
#-----------------------------------------------------------------------------

plot3 <-function (){
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
  #Create Plot 3
  totals = PM25 %>%mutate(type = factor(type)) %>% group_by(year, type)%>% 
    summarise(total_25 = sum(Emissions, na.rm = TRUE))
  ggplot(data = totals, aes(year, total_25, color = type)) + geom_point(size = 3) +
    geom_smooth(method = "lm", formula= y~x, se=FALSE) + 
    ggtitle("PM2.5 particulate matter by source type")
  ggsave("charts/plot3.png")
}
