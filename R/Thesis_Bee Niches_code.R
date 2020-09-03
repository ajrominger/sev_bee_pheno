##Title: Thesis_Bee Phenological Niches_code
##Author: JEM
##Date: 12/04/2019
##Revised: 07/02/2020
##Purpose: define sev bee niches
##Note: originally written on laptop


rm(list=ls(all=TRUE))

library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(lmerTest)
library(visreg)


setwd("C:/Users/Owner/Desktop/Bee_data")


SEVBeeData2002.2015 <- read.csv("C:/Users/Owner/Desktop/Bee_data/SEVBeeData2002-2015.csv")
SEVmonthlymetAJ_20Sep19 <- read.csv("C:/Users/Owner/Desktop/Bee_data/SEVmonthlymetAJ_20Sep19.csv")

##lowercase column names
SEVBeeData2002.2015$month <- SEVBeeData2002.2015$Month
SEVBeeData2002.2015$year <- SEVBeeData2002.2015$Year
SEVBeeData2002.2015$ecosystem <- SEVBeeData2002.2015$Ecosystem


##?merge brings up merge function
#merge 
BEEclimate <- merge(SEVBeeData2002.2015,SEVmonthlymetAJ_20Sep19, by=c("month","year","ecosystem"))
BEEclimate <- BEEclimate[,-which(names(BEEclimate) %in% c("Month","Year","Ecosystem"))] #this removes columns that you don't want in the new dataframe. It is not essential but will make it look cleaner.


#Turn data into long form. Long form data is easier to work with because more functions are made to handle long form data and those functions generally run faster in long form
#we can use the function "pivot_longer" from the "tidyr" package
##climate data for site B in 2002 was in complete
BC_long <- BEEclimate%>%
  pivot_longer( cols = c(7:309), names_to = "species", values_to ="individuals")##%>%subset(ecosystem="C")

month_year <- ddply(BC_long, c("year","month","ecosystem","species","mean.temp","mean.humidity","precip",
                               "SPEI_12","SPEI_6","SPEI_5","SPEI_4","SPEI_3","SPEI_2",
                               "accum.precip2","accum.precip3","accum.precip4","accum.precip5","accum.precip6",
                               "meanT2","meanT3","meanT4","meanT5","meanT6",
                               "minT2","minT3","minT4",
                               "maxT2","maxT3","maxT4","maxT5","maxT6"),
                            function(x)data.frame(
                              total_abundance = sum(x$individuals)
                            ))


mean.temp <- lmer(total_abundance~mean.temp+(1|ecosystem:species),month_year)
mean.humidity <- lmer(total_abundance~mean.humidity+(1|ecosystem:species),month_year)
precip <- lmer(total_abundance~precip+(1|ecosystem:species),month_year)
SPEI_12 <- lmer(total_abundance~SPEI_12+(1|ecosystem:species),month_year)
SPEI_6 <- lmer(total_abundance~SPEI_6+(1|ecosystem:species),month_year)
SPEI_5 <- lmer(total_abundance~SPEI_5+(1|ecosystem:species),month_year)
SPEI_4 <- lmer(total_abundance~SPEI_4+(1|ecosystem:species),month_year)
SPEI_3 <- lmer(total_abundance~SPEI_3+(1|ecosystem:species),month_year)
SPEI_2 <- lmer(total_abundance~SPEI_2+(1|ecosystem:species),month_year)
accum.precip2 <- lmer(total_abundance~accum.precip2+(1|ecosystem:species),month_year)
accum.precip3 <- lmer(total_abundance~accum.precip3+(1|ecosystem:species),month_year)
accum.precip4 <- lmer(total_abundance~accum.precip4+(1|ecosystem:species),month_year)
accum.precip5 <- lmer(total_abundance~accum.precip5+(1|ecosystem:species),month_year)
accum.precip6 <- lmer(total_abundance~accum.precip6+(1|ecosystem:species),month_year)
meanT2 <- lmer(total_abundance~meanT2+(1|ecosystem:species),month_year)
meanT3 <- lmer(total_abundance~meanT3+(1|ecosystem:species),month_year)
meanT4 <- lmer(total_abundance~meanT4+(1|ecosystem:species),month_year)
meanT5 <- lmer(total_abundance~meanT5+(1|ecosystem:species),month_year)
meanT6 <- lmer(total_abundance~meanT6+(1|ecosystem:species),month_year)
minT2 <- lmer(total_abundance~minT2+(1|ecosystem:species),month_year)
minT3 <- lmer(total_abundance~minT3+(1|ecosystem:species),month_year)
minT4 <- lmer(total_abundance~minT4+(1|ecosystem:species),month_year)
maxT2 <- lmer(total_abundance~maxT2+(1|ecosystem:species),month_year)
maxT3 <- lmer(total_abundance~maxT3+(1|ecosystem:species),month_year)
maxT4 <- lmer(total_abundance~maxT4+(1|ecosystem:species),month_year)
maxT5 <- lmer(total_abundance~maxT5+(1|ecosystem:species),month_year)
maxT6 <- lmer(total_abundance~maxT6+(1|ecosystem:species),month_year)
month <- lmer(total_abundance~month+(1|ecosystem:species),month_year)

anova(month,mean.temp,mean.humidity,precip,
      SPEI_12,SPEI_6,SPEI_5,SPEI_4,SPEI_3,SPEI_2,
      accum.precip2,accum.precip3,accum.precip4,accum.precip5,accum.precip6,
      meanT2,meanT3,meanT4,meanT5,meanT6,
      minT2,minT3,minT4,
      maxT2,maxT3,maxT4,maxT5,maxT6)

visreg(month,ylim = c(0,10))
summary(month)


month.drive <- lmer(total_abundance~month+(1|mean.temp)+(1|precip)+(1|species),month_year)
temp.drive <- lmer(total_abundance~mean.temp+(1|month)+(1|precip)+(1|species),month_year)
precip.drive <- lmer(total_abundance~precip+(1|mean.temp)+(1|month)+(1|species),month_year)

anova(month.drive, temp.drive, precip.drive)
summary(month.drive)
summary(temp.drive)
summary(precip.drive)

##this will sum the number of times a species was recorded in any given month over all years of the dataset
month_occurance <- BC_long%>%
  ddply(c("species","month"),
      function(x)data.frame(
        total_abundance=sum(x$individuals)
      ))%>%
  subset(total_abundance!=0)


ggplot(month_occurance, aes(x=species, y=month, colour=species))+
  geom_pointrange(aes(ymin= month-.5, ymax= month+.5), fatten = .1, size=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90),legend.position = "none")


##month_occurance$presence <- ifelse(month_occurance$total_abundance==0,0,1)


##########################reduce dataset to focus species#############################
##focus_bees were designated as any bee with an abundance_proportion greater than or equal to .001
focus_bees <- read.csv("C:/Users/Owner/Desktop/Bee_data/Focus_Bees.csv")

##rename 'species' column in BC_long(58th column if climate data is included)
colnames(BC_long)[58] <- c('code')

##combined focus_bees and BC_long
Bee_focus <- merge(focus_bees,BC_long, by=c("code"))
Bee_focus <- Bee_focus[,-which(names(Bee_focus) %in% c("mean_mass_genus","se_mass_genus","focalspecies"))]

write.csv(Bee_focus, "C:/Users/Owner/Desktop/Bee_data/Bee_focus.csv")
##check if 'species' should be changed to 'code'?????????
##what exactly is month_abundance?
month_year <- ddply(Bee_focus, c("year","month","ecosystem","code","mean.temp","mean.humidity","precip",
                               "SPEI_12","SPEI_6","SPEI_5","SPEI_4","SPEI_3","SPEI_2",
                               "accum.precip2","accum.precip3","accum.precip4","accum.precip5","accum.precip6",
                               "meanT2","meanT3","meanT4","meanT5","meanT6",
                               "minT2","minT3","minT4",
                               "maxT2","maxT3","maxT4","maxT5","maxT6"),
                    function(x)data.frame(
                      month_abundance = sum(x$individuals)
                    ))

##month_year2 <- ddply(Bee_focus, c("year","month","ecosystem","species","mean.temp","mean.humidity","precip",
                                 #"SPEI_12","SPEI_6","SPEI_5","SPEI_4","SPEI_3","SPEI_2",
                                 #"accum.precip2","accum.precip3","accum.precip4","accum.precip5","accum.precip6",
                                # "meanT2","meanT3","meanT4","meanT5","meanT6",
                                 #"minT2","minT3","minT4",
                                # "maxT2","maxT3","maxT4","maxT5","maxT6"),
                   #summarise,
                      #month_abundance = sum(individuals))

identical(month_year,month_year2)

mean.temp <- lmer(month_abundance~mean.temp+(1|ecosystem:species),month_year)
mean.humidity <- lmer(month_abundance~mean.humidity+(1|ecosystem:species),month_year)
precip <- lmer(month_abundance~precip+(1|ecosystem:species),month_year)
SPEI_12 <- lmer(month_abundance~SPEI_12+(1|ecosystem:species),month_year)
SPEI_6 <- lmer(month_abundance~SPEI_6+(1|ecosystem:species),month_year)
SPEI_5 <- lmer(month_abundance~SPEI_5+(1|ecosystem:species),month_year)
SPEI_4 <- lmer(month_abundance~SPEI_4+(1|ecosystem:species),month_year)
SPEI_3 <- lmer(month_abundance~SPEI_3+(1|ecosystem:species),month_year)
SPEI_2 <- lmer(month_abundance~SPEI_2+(1|ecosystem:species),month_year)
accum.precip2 <- lmer(month_abundance~accum.precip2+(1|ecosystem:species),month_year)
accum.precip3 <- lmer(month_abundance~accum.precip3+(1|ecosystem:species),month_year)
accum.precip4 <- lmer(month_abundance~accum.precip4+(1|ecosystem:species),month_year)
accum.precip5 <- lmer(month_abundance~accum.precip5+(1|ecosystem:species),month_year)
accum.precip6 <- lmer(month_abundance~accum.precip6+(1|ecosystem:species),month_year)
meanT2 <- lmer(month_abundance~meanT2+(1|ecosystem:species),month_year)
meanT3 <- lmer(month_abundance~meanT3+(1|ecosystem:species),month_year)
meanT4 <- lmer(month_abundance~meanT4+(1|ecosystem:species),month_year)
meanT5 <- lmer(month_abundance~meanT5+(1|ecosystem:species),month_year)
meanT6 <- lmer(month_abundance~meanT6+(1|ecosystem:species),month_year)
minT2 <- lmer(month_abundance~minT2+(1|ecosystem:species),month_year)
minT3 <- lmer(month_abundance~minT3+(1|ecosystem:species),month_year)
minT4 <- lmer(month_abundance~minT4+(1|ecosystem:species),month_year)
maxT2 <- lmer(month_abundance~maxT2+(1|ecosystem:species),month_year)
maxT3 <- lmer(month_abundance~maxT3+(1|ecosystem:species),month_year)
maxT4 <- lmer(month_abundance~maxT4+(1|ecosystem:species),month_year)
maxT5 <- lmer(month_abundance~maxT5+(1|ecosystem:species),month_year)
maxT6 <- lmer(month_abundance~maxT6+(1|ecosystem:species),month_year)
month <- lmer(month_abundance~month+(1|ecosystem:species),month_year)

anova(month,mean.temp,mean.humidity,precip,
      SPEI_12,SPEI_6,SPEI_5,SPEI_4,SPEI_3,SPEI_2,
      accum.precip2,accum.precip3,accum.precip4,accum.precip5,accum.precip6,
      meanT2,meanT3,meanT4,meanT5,meanT6,
      minT2,minT3,minT4,
      maxT2,maxT3,maxT4,maxT5,maxT6)

visreg(month,ylim = c(0,10))
summary(month)


month.drive <- lmer(total_abundance~month+(1|mean.temp)+(1|precip)+(1|species),month_year)
temp.drive <- lmer(total_abundance~mean.temp+(1|month)+(1|precip)+(1|species),month_year)
precip.drive <- lmer(total_abundance~precip+(1|mean.temp)+(1|month)+(1|species),month_year)

anova(month.drive, temp.drive, precip.drive)
summary(month.drive)
summary(temp.drive)
summary(precip.drive)

##this creates and graph, with just the focus bees, any month a species has been seen in over the course of the data set
month_occurance <- Bee_focus%>%
  ddply(c("code","month"),
        function(x)data.frame(
          month_abundance=sum(x$individuals)
        ))%>%
  subset(month_abundance!=0)


ggplot(month_occurance, aes(x=code, y=month, colour=code))+
  geom_pointrange(aes(ymin= month-.5, ymax= month+.5), fatten = .1, size=2)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=90),legend.position = "none")



#####group bees by seasonality: spring, summer?, fall, bivoltine, all year
##try this for focused bees and all bees
##if the bee occurs before or during June(Spring) after(Fall),if it occurs contnuously 
##make a seasonality column


####distribution curve: first, meadian, and last months seen, abundance for each date

# library
library(ggridges)
library(ggplot2)


# ridge plot, trying to look at distribution
ggplot(month_occurance, aes(x = month, y = code, fill = month_abundance, colour=code)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

Bee_focus <- merge(Bee_focus,month_occurance, by=c("code","month"))

Diadasia_rin <- subset(Bee_focus, code=="APDIARIN")
Diadasia_rin$year <- as.factor(Diadasia_rin$year)

ggplot(Diadasia_rin, aes(x = month, y = year, fill = month_abundance, colour=code)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

apeuclyc <- subset(Bee_focus, code=="APEUCLYC")
apeuclyc$year <- as.factor(apeuclyc$year)

ggplot(apeuclyc, aes(x = month, y = year, fill = month_abundance, colour=code)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

###ridge plot with fill as total_abundance over all years
###not accurate total_abundance is not recognizing month
##bee_curve <- Bee_focus%>% select(code,month,total_abundance)

##ggplot(bee_curve, aes(x = month, y = code, fill = total_abundance, colour=code)) +
  #geom_density_ridges() +
  #theme_ridges() + 
  #theme(legend.position = "none")

###ridge plot for each year
##merge Bee_focus with month_occurance
##something is wrong with month_abundance at this point....it's filling in the same abundance for every year
##I don't think month_abundance can be used for this calculation, I need to find the mon_abundances for each year not all years
Bee_focus <- merge(Bee_focus,month_occurance, by=c("code","month"))

bee_curve_years <- Bee_focus%>% select(code,month,month_abundance,year)

ggplot(bee_curve_years, aes(x = month, y = code, fill = month_abundance, colour=code)) +
  geom_density_ridges() +
  facet_wrap(facets = vars(year))+
  theme_ridges() + 
  theme(legend.position = "none")


##ddply for each species and year what is the peak abundance month, has that changed over time
##
