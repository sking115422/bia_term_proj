

# Installing packages
install.packages("tidyverse")

# Loading Libraries
library(tidyverse)


############################################################
## WHR AGGREGATED DATA SET 2015 - 2021 
############################################################

### Data Exploration

# Reading in project data from CSV file
whr_raw <- read_csv("./data/whr_15-21_mc.csv")

# Checking how many NA value each data column has
nrow(whr_raw)
colSums(is.na(whr_raw))

# Checking which countries are represented in all years
c_uniq <- unique(whr_raw$Country)
length(c_uniq)

c_occ <- list()

for (c1 in c_uniq) {
  
  inc = 0
  
  for (c2 in whr_raw$Country) {
    
    if (c1 == c2) {
      inc = inc + 1
    }
    
  }
  
  c_occ <- append(c_occ, inc)
  
}

c_occ 

# Cleaning countries that are not represented in all years

inc = 1
whr_raw_c <- whr_raw
c_occ_f <- list()

for (each in c_occ) {
  
  if (each != 7) {
    whr_raw_c <- subset(whr_raw_c, Country != c_uniq[inc])
  }
  
  if (each == 7) {
    c_occ_f <- append(c_occ_f, c_uniq[inc])
  }
  
  inc = inc + 1
  
}

# Checking to make sure cleaning was successful

length(c_occ_f)
length(unique(whr_raw_c$Country))

setdiff(c_occ_f, as.list(unique(whr_raw_c$Country)))


############################################################
## WHR Modeling and Analysis 
############################################################





















