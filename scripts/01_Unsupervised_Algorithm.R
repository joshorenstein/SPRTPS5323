#Load some packages
Packages <- c("lubridate","tidyverse","RODBC")
lapply(Packages, library, character.only = TRUE)
# Download the data
u <- "https://projects.fivethirtyeight.com/nba-model/nba_elo.csv"
elo.df <- read.csv(u)

#Do some data cleaning. Re-organize the dataset into favored team and underdog
elo.df$date <- as.Date(elo.df$date)

elo <- elo.df %>% mutate(elo_prob1 = if_else(elo_prob1<.5,1-elo_prob1,elo_prob1))
elo <- elo %>% mutate(elo_favor_team=if_else(elo1_pre>elo2_pre,elo1_pre,elo2_pre)) %>% 
  mutate(elo_under_team=if_else(elo1_pre<elo2_pre,elo1_pre,elo2_pre),
         elo_avg=(elo1_pre+elo2_pre)/2) 

#Just review data since 2017 
elo_df <- elo %>% filter(season>=2017) %>% 
  dplyr::select(date,season,playoff,team1,team2,elo_favor_team,elo_under_team,elo_prob1,elo_avg) %>% 
  filter(playoff=="")

# Convert the features of the data
elo.data <- as.matrix(elo_df[6:9])

# Set the row names of elo.data
row.names(elo.data) <- elo_df$id

# Check column means and standard deviations
colMeans(elo.data)
summary(elo.data)
apply(elo.data,2,sd)

# Scale the data
elo.scaled <- scale(elo.data)
elo.scaled
# Calculate the (Euclidean) distances
elo.dist <- dist(elo.scaled)
# Create a hierarchical clustering model
elo.hclust <- hclust(d=elo.dist,method="complete")
plot(elo.hclust)
abline(h=5,lty=2)
abline(h=4,lty=2)
# Cut tree so that it has 5 clusters
elo.hclust.clusters <- cutree(elo.hclust,k=5)

elo_clust <- data.frame(elo.hclust.clusters)
elo_clust$elo.hclust.clusters <- as.character(elo_clust$elo.hclust.clusters)
# Compare cluster membership to actual team performance

summary <- elo_df %>% ungroup() %>%  bind_cols(elo_clust) %>% 
  dplyr::group_by(elo.hclust.clusters) %>% 
  dplyr::summarise(elo1=mean(elo_favor_team),
                   elo2=mean(elo_under_team),
                   elo_prob=mean(elo_prob1),
                   mean=mean(elo_avg),count=n())

#Join the data
class <- elo_df %>% bind_cols(elo_clust) 
class <- class %>% dplyr::rename(cluster=elo.hclust.clusters)

