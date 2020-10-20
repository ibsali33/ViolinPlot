#The objective for this code is to create a voilin plot with the number of new cases per day
# for the states with the 15 largest populatios. This will help visualize how infection spreads
# over time in different states. First we load the packages required for the project.

library(dplyr)
library(ggplot2)
library(gplots)
library(tidyr)
library(tibble)

# COVID19 data is a bit messy so we are going to need to do some data manipulation. Load two 
# csv files the first containing the COVID19 data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa"

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",") # the read.csv command is used for importing csv data sets
statepop <- read.csv("LargeStates.csv", sep = ",")

# We are interested in the number of positive cases and the increase in positive cases 
# organized by state over time. We also need info about in state population. We need to 
# reorganize the data in a way that specificially selects for the states/regions we are 
# interested in (indicated by the LargeStates.csv file) and organizes the positive cases by date. 

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive, alldata$positiveIncrease)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

# Now we can merge the two tables based on their state abbrevations. Here I use the full_join command
# I tell full_join to align the data based on the "alldata.state" and "statepop.StateAbbrevaition"
# columns.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

# Now that the tables are merged lets divide the number of cases by the population to 
# get the number of cases per million (CPM) individuals, in the same step, lets remove 
# all territories that are not on our selected populations list.

casespermillion <- tablemerge %>%
  mutate(CPM=alldata.positive/statepop.Pop.millions) %>%
  mutate(log2increase=log2(alldata.positiveIncrease)) %>%
  filter(statepop.Pop.millions != "NA")

#Note that the number of observations reduces here, this shows that the removal of NAs worked. Now we can plot.

#ggplot2 works well for creating violin plots

ggplot(data = casespermillion, aes(alldata.state, log2increase))+
  geom_violin(scale = "area")+
  geom_dotplot(binaxis="y", stackdir = "center", binwidth = 0.25, dotsize = .5)

