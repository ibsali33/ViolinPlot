#The objective for this code is to plot the number of cases of COVID19 recorded per million people. This will help visualize rates of infection in different states. First we load the packages required for the project.

library(dplyr)
library(ggplot2)
library(gplots)
library(tidyr)
library(tibble)

#COVID19 data is a bit messy so we are going to need to do some data manipulation. Load two csv files the first containing the COVID19 data downloaded from "kaggle.com/sudalairajkumar/covid19-in-usa"

alldata <- read.csv("us_states_covid19_daily.csv", sep = ",")
statepop <- read.csv("States.csv", sep = ",")

#We are interested in the number of positive cases organized by state over time and are interested in state population (including DC and Puerto Rico). We need to reorganize the data in a way that specificially selects for the states/regions we are interested in (indicated by the States.csv file) and organizes the positive cases by date. 

allpositivecases <- data.frame(alldata$date, alldata$state, alldata$positive)
selectedpops <- data.frame(statepop$StateAbbreviation, statepop$Pop.millions)

#Now we can merge the two tables based on their state abbrevations.

tablemerge <- full_join(allpositivecases, selectedpops, by = c("alldata.state" = "statepop.StateAbbreviation"))

#Now that the tables are merged lets divide the number of cases by the population to get the number of cases per million (CPM) individuals, in the same step, lets remove all states that are not on our selected populations list. 

casespermillion <- tablemerge %>%
  mutate(CPM=alldata.positive/statepop.Pop.millions) %>%
  filter(statepop.Pop.millions != "NA")

#Subset the full table into two data frames, one containing the cases per million values, and two contianing the raw positive numbers.

forheatmap1 <- casespermillion %>%
  select(alldata.date, alldata.state, CPM)

#Note that the number of observations reduces here, this shows that the removal of NAs worked.