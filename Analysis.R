#The biological hypothesis that are being tested are as follows:
#1. The lower the depth the higher the abundance of plastics will be found.
#2. The higher the salinity the higher the abundance of plastics will be found.
#3. The higher the temperature the higher the abundance of plastics will be found.

#The dependent variable of this test is the plastic abundance 
#The independent variables of this test are environmental conditions (sea temperature, depth and salinity)

#The statistical analysis being used in this test is the Kendall Rank Correlation Test which is a non-parametric test used to measure the correlation of two variables 
#The statistical null hypothesis is there is no statistically significant relationship between the median abundance of plastics and the median environmental variables (sea temperature, depth and salinity).
#The assumptions of the statistical analysis is:
#1. The data is continuous or ordinal
#2. The data is monotonic

help(ggscatter)
install.packages("readr")
tinytex::install_tinytex()
library(tinytex)
library(readr)
install.packages("ggpubr")
library(ggpubr)
install.packages("tidyverse")
library(tidyverse)

#Import datasets (nettows_info.csv and plastics_info.csv)
nettows_info <- read_csv("data/nettows_info.csv")
plastics_info <- read_csv("data/plastics_info.csv")

#We want to create a new dataset containing only the data we need and take the average from the three replicates of each site.
#We want to take the total plastic abunance at each site and the average sea temperature, depth and salinity at each site.
plastics_abund <- count(plastics_info, NetStation)
plastics_abund <- aggregate(Length~NetStation+Replicate,data=plastics_info,FUN=length)

data <- merge(plastics_abund,nettows_info)

avtemp <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.ST = mean(SeaTemperature)) 

avdepth <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.depth = mean(Depth)) 

avsalinity <- nettows_info %>% 
  group_by(NetStation) %>%
  summarize(averaged.sal = mean(Salinity)) 

#Compile all of the data into a single dataset.
list <- list(plastics_abund, avtemp, avdepth, avsalinity)      
data <- Reduce(function(x, y) merge(x, y, all=TRUE), list) 

#Now we can view the datasets 
#Table of Site, plastic abundance, average sea temperature, depth and sailinity
head(data, 5)

#All of the data is measured on a continuous scale 

#We also will test for normality of the datasets
shapiro.test(data$Length)
shapiro.test(data$averaged.ST)
shapiro.test(data$averaged.depth)
shapiro.test(data$averaged.sal)
hist(data$Length)

#From the output, the four p-values are less than the significance level 0.05 implying that the distribution of the data are significantly different from normal distribution. 
#In other words, we can assume the data is NOT normally distributed.

#Create scatter plots of the environmental conditions vs the plastic abundance.
#Scatter plot of plastic abundance and average sea temperature
ggscatter(data, x = "averaged.ST", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "average sea temperature", ylab = "plastic abundance")

#Scatter plot of plastic abundance and average depth
ggscatter(data, x = "averaged.depth", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "average depth", ylab = "plastic abundance")

#Scatter plot of plastic abundance and average salinity
ggscatter(data, x = "averaged.sal", y = "n", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "average salinity", ylab = "plastic abundance")


#From the above plots we can see that the above plots are linear and monotonic in relationship.

#Therefore, we can say that the data is not normally distributed, continuous in scale and have a monotonic relationship.
#Therefore, all assumptions have been met to use the Kendall Rank Correlation Test.
temperature_kendall <- cor.test(data$n, data$averaged.ST, 
                method = "kendall")
depth_kendall <- cor.test(data$n, data$averaged.depth, 
                method = "kendall")
salinity_kendall <- cor.test(data$n, data$averaged.sal, 
                method = "kendall")

temperature_kendall
#tau = 0.1086, p = 0.2963
depth_kendall
#tau = -0.1657, p = 0.08502
salinity_kendall
#tau = -0.06806, p = 0.5124

#All p-values obtained are greater than the significance level 0.05.
#Therefore, the null hypothesis is not rejected.
#Therefore, we can determine that there is no statistically significant relationship between plastics abundance and environmental conditions (sea temperature, depth and salinity)

shapiro.test(residuals(lm(log(n)~averaged.depth,data)))
shapiro.test(residuals(lm(log(n)~averaged.sal,data)))
shapiro.test(residuals(lm(log(n)~averaged.ST,data)))

lm(log(n)~averaged.depth,data)

data$logn <- log(data$n)

temperature_pearson <- cor.test(data$logn, data$averaged.ST, 
                                method = "pearson")
depth_pearson <- cor.test(data$logn, data$averaged.depth, 
                                method = "pearson")
salinity_pearson <- cor.test(data$logn, data$averaged.sal, 
                                method = "pearson")


lengthdata <- merge(plastics_info,nettows_info)
lengthdata$log.length <- log(plastics_info$Length)
mST <- lmer(log.length ~ SeaTemperature +  (1|NetStation/Replicate),lengthdata)
mdepth <- lmer(log.length ~ Depth +  (1|NetStation/Replicate),lengthdata)
msalinity <- lmer(log.length ~ Salinity +  (1|NetStation/Replicate),lengthdata)
plot(msalinity)
summary(mST)
summary(mdepth)
summary(msalinity)

hist(lengthdata$log.length)
hist(data$Length)

help(lmer)

R.Version()
          
lst(shapiro.test) %>% 
  map_df(~ data  %>% 
           summarise_all(list(~ list(.x(.)[c("statistic", "p.value")]))) %>% 
            .id = "test")
