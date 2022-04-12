
# Installing packages
install.packages("tidyverse")

# Loading Libraries
library(tidyverse)

############################################################
## MASTER HAPPINESS AGGREGATED DATA SET
############################################################

### Data Exploration

# Reading in project data from CSV file
whr_15 <- read_csv("./data/whr15_21/2015.csv")
whr_16 <- read_csv("./data/whr15_21/2016.csv")
whr_17 <- read_csv("./data/whr15_21/2017.csv")
whr_18 <- read_csv("./data/whr15_21/2018.csv")
whr_19 <- read_csv("./data/whr15_21/2019.csv")
whr_20 <- read_csv("./data/whr15_21/2020.csv")
whr_21 <- read_csv("./data/whr15_21/2021.csv")


# Cleaning for columns of interest

col_names <- names(whr_18)
whr_15c <- whr_15 %>% select (col_names)


# Merging all dataframes
whr_full <- do.call("rbind", list(whr_15, whr_16, whr_17, whr_18, whr_19, whr_20, whr_21))





# Checking how many NA value each data column has
nrow(data_raw)
colSums(is.na(data_raw))

no_na <- na.omit(data_raw)


### Data Cleaning


############################################################
## MASTER HAPPINESS AGGREGATED DATA SET
############################################################