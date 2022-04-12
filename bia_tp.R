
# Initializing renv for local dependency management
renv::init()

# Installing Packages
install.packages("tidyverse")

# Loading Libraries
library(tidyverse)

# Reading in project data from CSV file
data_raw <- read_csv("master_happiness_agg_data.csv")

# Checking how many NA value each data column has
nrow(data_raw)
colSums(is.na(data_raw))

no_na <- na.omit(data_raw)
