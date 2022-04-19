

# Installing packages
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("reshape2")
install.packages("ggplot2")

# Loading Libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(reshape2)
library(ggplot2)


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
whr_c <- whr_raw
c_occ_f <- list()

for (each in c_occ) {
  
  if (each != 7) {
    whr_c <- subset(whr_c, Country != c_uniq[inc])
  }
  
  if (each == 7) {
    c_occ_f <- append(c_occ_f, c_uniq[inc])
  }
  
  inc = inc + 1
  
}

# Checking to make sure cleaning was successful

length(c_occ_f)
length(unique(whr_c$Country))

setdiff(c_occ_f, as.list(unique(whr_c$Country)))

# Exporting final WHR data set to CSV

write_csv(whr_c, "data/whr_final.csv")

# Descriptive Statistics

write_csv(summary (whr_c), "sum.csv")

# Correlation Matrix

# Dropping Country column bc its non numeric
whr_c_d <- select(whr_c, -c(1, 2, 3))
whr_c_d 

cormat <- cor(whr_c_d, method = "pearson")

cormat

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  xlab("") + ylab("") +
  geom_tile() 

############################################################
## WHR Modeling and Analysis 
############################################################

### GENERAL ANALYSIS

# Finding top 5, middle 5, bottom 5 countries

c_rank <- whr_c %>% 
  group_by(Country) %>%
  summarize(avg_h_score = mean(`Happiness Score`)) %>%
  arrange((desc(avg_h_score)))

# Top 5
head(c_rank, 5)

# Middle 5
c_rank[67:71, ]

# Bottom 5
tail(c_rank, 5)

### K_MEANS CLUSTERING

# Checking if there might be better ways to cluster these countries
# Using K-means clustering

# First we need to aggregate the data by year so we can see the average of happiness scores off each country across the years
whr_clust <- whr_c %>% 
  group_by(Country) %>%
  summarize(avg_happiness_score = mean(`Happiness Score`), avg_economy = mean(Economy), avg_family = mean(Family), avg_health = mean(Health),
            avg_freedom = mean(Freedom), avg_trust = mean(Trust), avg_generosity = mean(Generosity)) %>%
  arrange((desc(avg_happiness_score)))

# Creating a data frame that store country names and their ranks
c_rank$CountryIndex <- 1:nrow(c_rank)

# Replacing the country names with there associated index value corresponding to their average rank
whr_clust$Country <- c_rank$CountryIndex


# Scaling our data
whr_clust_og <- whr_clust
whr_clust_scaled <- scale(whr_clust)

# Capturing std and mean in a list so we can de-scale later
mean_list <- list()
std_list <- list()

whr_clust

for (i in colnames(whr_clust)){
  
  m = mean(whr_clust[[i]])
  mean_list <- append(mean_list, m)
  
  std = sd(whr_clust[[i]])
  std_list <- append(std_list, std)
  
  # whr_clust[[i]] <- (whr_clust[[i]] - m) / std
  
  i = 0
  
}

mean_list
std_list
whr_clust_scaled

# Setting seed for reproducibility
set.seed(1) 

# Creating elbow plot to determine optimal number of clusters
fviz_nbclust(whr_clust_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2, color = "red") +
  labs(title = "Optimal Number of Clusters", subtitle = "Elbow method") +
  xlab("Number of Clusters, K")

# Clustering based on the optimal number of clusters
km_res <- kmeans(whr_clust_scaled, 4, nstart = 25)
print(km_res)

# Visualizing the clustering
fviz_cluster(km_res, data = whr_clust_scaled, main = "Cluster Plot", geom = c("point")) +
  ggthemes::theme_fivethirtyeight()

# Adding clustering results back as column to averaged WHR data
whr_clust$Cluster <- km_res$cluster
whr_clust$Country <- c_rank$Country
write_csv(whr_clust, "data/whr_15-21_clusters.csv")

# Creating and de-scaling cluster means table
cluster_means <- as_tibble(km_res$centers)
cluster_means_scaled <- as_tibble(km_res$centers)
cluster_means

# De-scaling
for (j in 1:ncol(cluster_means)) {
  cluster_means[ , j] <- cluster_means[ , j] * std_list[j] + mean_list[j]  
  j = 0
}

cluster_means$num_in_cluster <- km_res$size
cluster_means

# Exporting clustering means as CSV
write_csv(cluster_means, "visuals/clust_means.csv")

### EVALUATING COUNTRIES OF INTEREST OVER TIME

# Getting top 5 countries over all years
t5_names <- head(c_rank$Country, 5)
t5_names

top5 <- whr_c %>% filter (Country == t5_names[1] | Country == t5_names[2] | Country == t5_names[3] | 
                          Country == t5_names[4] | Country == t5_names[5] )

write_csv(top5, "data/top5.csv")

# Getting bottom 5 countries over all years
b5_names <- tail(c_rank$Country, 5)
b5_names

bot5 <- whr_c %>% filter (Country == b5_names[1] | Country == b5_names[2] | Country == b5_names[3] | 
                            Country == b5_names[4] | Country == b5_names[5] )

write_csv(bot5, "data/bot5.csv")

# Countries of interest

# Finding out which countries made the most improvement from 2015-2021
tmp1 <- whr_c %>% 
  filter(Year == 2015) %>%
  summarize(country = Country, hs = `Happiness Score`)

tmp2 <- whr_c %>% 
  filter(Year == 2021) %>%
  summarize(country = Country, hs = `Happiness Score`)

tmp1 <- tmp1 %>% arrange(country)
tmp1

tmp2 <- tmp2 %>% arrange(country)
tmp2

diff <- tmp2$hs - tmp1$hs

diff_df <- tibble(country = tmp1$country, diff = diff)

diff_df <- diff_df %>% arrange(desc(diff))

# Getting top 5 most improved 
mi_names <- head(diff_df$country, 5)

mi <- whr_c %>% filter (Country == mi_names[1] | Country == mi_names[2] | Country == mi_names[3] | 
                            Country == mi_names[4] | Country == mi_names[5] )

write_csv(mi, "data/mi.csv")

# Getting bottom 5 most improved
li_names <- tail(diff_df$country, 5)

li <- whr_c %>% filter (Country == li_names[1] | Country == li_names[2] | Country == li_names[3] | 
                          Country == li_names[4] | Country == li_names[5] )

write_csv(li, "data/li.csv")

# Countries that show showed significant improvement in the same region
# Wanted to further quantify
c_int <- whr_c %>% filter (Country == "Hungary" | Country == "Romania" | Country == "Bulgaria" | 
                            Country == "Ukraine" | Country == "Greece" )

write_csv(c_int, "data/c_int.csv")

