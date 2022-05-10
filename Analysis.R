#The biological hypothesis that are being tested are as follows:
#1. The lower the depth the higher the abundance of plastics will be found.
#2. The higher the salinity the higher the abundance of plastics will be found.
#3. The higher the temperature the higher the abundance of plastics will be found.

#The dependent variable of this test is the plastic abundance 
#The independent variables of this test are environmental conditions being compared (e.g. sea temperature, depth and salinity)

#The statistical analysis being used in this test is the Kendall Rank Correlation Test which is a non-parametric test used to measure the correlation of two variables 
#The statistical null hypothesis is 
#The assumptions of the statistical analysis is 

install.packages(readr)
library(readr)
install.packages(ggpubr)
library(ggpubr)
install.packages(tidyverse)
library(tidyverse)

nettows_info <- read_csv("data/nettows_info.csv")
plastics_info <- read_csv("data/plastics_info.csv")

plastics_abund <- count(plastics_info, NetStation)

avtemp <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.ST = mean(SeaTemperature)) 

avdepth <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.depth = mean(Depth)) 

avsalinity <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.sal = mean(Salinity)) 

list <- list(plastics_abund, avtemp, avdepth, avsalinity)      
data <- Reduce(function(x, y) merge(x, y, all=TRUE), list) 

ggscatter(data, x = "averaged.ST", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "average sea temperature", ylab = "plastic abundance")

ggscatter(data, x = "averaged.depth", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "average depth", ylab = "plastic abundance")

ggscatter(data, x = "averaged.sal", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "average salinity", ylab = "plastic abundance")

shapiro.test(data$n)
shapiro.test(data$averaged.ST)
shapiro.test(data$averaged.depth)
shapiro.test(data$averaged.sal)

temperature_pearsons <- cor.test(data$n, data$averaged.ST, 
                method = "kendall")
depth_pearsons <- cor.test(data$n, data$averaged.depth, 
                method = "kendall")
salinity_pearsons <- cor.test(data$n, data$averaged.sal, 
                method = "kendall")

temperature_pearsons
depth_pearsons
salinity_pearsons
