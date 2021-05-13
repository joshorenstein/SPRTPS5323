#install.packages("tidyverse") #install the packages if need be
#install.packages("mdsr")
library(tidyverse)
library(mdsr)

df <- read_csv("data/nba_basic.csv") #nba data 
kp <- read_csv("data/kenpom.csv") #ncaa data from kenpom.com
names(kp) #look at the column names
kp <- kp %>% filter(OE!=0) %>% filter(TOPct_off!=100) #filter out teams that didn't play in 20-21 season
View(kp) #view the data
kp %>% distinct(Conference) #look at all of the conferences
main <- kp %>% filter(Conference %in% c("B10","P12","SEC","B12","ACC","BE","AAC","WCC")) #keep the major conferences in a sep dataframe

#Aesthetics
names(main)
g <- ggplot(data = main,aes(y=OE,x=FG3Pct))
g
g + geom_point(size = 3) #scatterplot

g + geom_point(aes(color = Conference), size = 3) #with color

g + geom_text(aes(label = TeamName, color = Conference), size = 3) #use team names as labels

g + geom_point(aes(color = Conference, size = Tempo)) #use the size aesthetic

#Facets

g + 
  geom_point(alpha = 0.9, aes(size = Tempo)) + 
  coord_trans(y = "log10") + 
  facet_wrap(~Conference, nrow = 1) +  #group by Conference
  theme(legend.position = "top")

#Layers
head(kp)
c <- kp %>% 
  group_by(Conference) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE) #summarize all numeric data by conf

conf <- 
  kp %>% 
  group_by(Conference) %>% 
  summarise(TOPct=mean(TOPct_off)) #group by conference and get average TO

#Bar graph for all conference by Turnover %
p <- ggplot(
  data = conf, 
  aes(x = reorder(Conference, TOPct), y = TOPct)
) +
  geom_col(fill = "gray") +
  ylab("Turnover %") + 
  xlab("Conference") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(1)))
p
names(main)

p + geom_point(data = conf, size = 1, alpha = 0.3)  #add a point


#Univariate displays
names(kp)
g <- ggplot(data = kp, aes(x = OE))

#histogram with binwidth of 1
g + geom_histogram(binwidth = 1) + labs(x = "Average Offensive Eff")

#histogram as density plot
g + geom_density(adjust = 0.3)

names(kp)
#keep pac 12 teams only
p12 <- kp %>% 
  filter(Conference=="P12")

#bar graph
ggplot(
  data = head(p12),#keep top 6 teams  
  aes(x = reorder(TeamName, -OE), y = OE)
) +
  geom_col() +
  labs(x = "Team", y = "Average Offensive Eff")



#Multivariate displays
names(main)
g <- ggplot(
  data = main, 
  aes(y = OE, x = FG3Pct)
) + 
  geom_point()

g #scatterplot

g <- g + 
  geom_smooth(method = "lm", se = FALSE) + 
  xlab("Average 3P%") +
  ylab("Average Offensive Rating")
g # add a regression line

g <- g + 
  geom_smooth(method = "lm", se = TRUE) + #add error bars to regression line
  xlab("Average 3P%") +
  ylab("Average Offensive Rating")
g 
summary(kp$FG3Pct)
kp <- kp %>%
  mutate(
    FG3Pct_rate = cut(
      FG3Pct, 
      breaks = c(25, 31.84, 35.45, 100), #cut the data at the quartiles
      labels = c("low", "medium", "high") #split into 3 categories
    )
  )

View(kp)
g <- g %+% kp # redo the plot with the new dataset

g + aes(color = FG3Pct_rate) # add color of new category

g + facet_wrap(~ FG3Pct_rate) #facet wrap

height <- read_csv('data/height.csv') #load height data
kp <- kp %>% 
  inner_join(height) #join int
summary(kp$Size)
kp <- kp %>%
  mutate(
    Size_rate = cut(
      Size, 
      breaks = c(70, 76.32, 77.52, 100), 
      labels = c("short", "medium", "tall")
    )
  )

#similar plot to above but with height and block %
ggplot(
  data = kp, 
  aes(x = Size, y = BlockPct, color = Size_rate)
) + 
  geom_point() + 
  geom_smooth() + 
  ylab("Block %") + 
  xlab("Average Height (in)") +
  labs(color = "Team Size") 

#boxplots of Tempo/Pace by Conference.
ggplot(
  data = main, 
  aes(
    x = Conference, 
    y = Tempo
  )
) + 
  geom_boxplot() +
  xlab("Conference") + 
  ylab("Tempo")

