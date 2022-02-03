# load essential libraries 
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)

# load this week's data
boardgame <- tidytuesdayR::tt_load('2022-01-25')

# get the two tables
boardgame_ratings <- boardgame$ratings
boardgame_details <- boardgame$details

# Ratings table has 200 more rows, but looks like these are all unique. 
# left join ratings with details table
boardgame <- boardgame_ratings %>% left_join(boardgame_details, by="id")

# Create a smaller subset and exclude zero values for year
boardgame_subset <- boardgame %>% select(id, name, year, rank, average, users_rated,minplayers, maxplayers,playingtime,boardgamecategory, owned) %>%
  filter(year!=0)

# Separate out board game categories and keep Science and Avdenture
boardgame_subset <- boardgame_subset %>% separate_rows(boardgamecategory) %>%
  filter(boardgamecategory=="Science"| boardgamecategory=="Adventure" | boardgamecategory=="Card")

## check how many players game are there: boardgame_subset %>% group_by(minplayers) %>% tally()
## 0-8 - 0 and 5-8 are quite small, so let's look at 1-4 players
boardgame_subset <- boardgame_subset %>% filter(minplayers!=0 & minplayers<=4 & between(year, 1980,2021))

# Group by year, category and minimum player number
boardgame_aggregate <- boardgame_subset %>% group_by(year, boardgamecategory, minplayers) %>% summarise(n=n(), avg_rating=mean(average))

# See how many games were released in each category for diff num of players
gamerelease<- ggplot(data = boardgame_aggregate , aes(year, n, fill=as.factor(minplayers))) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_brewer(palette="Spectral", direction = -1)+ 
  theme(text = element_text(size=16), legend.position = "top", panel.background = element_rect(fill = "White"), panel.grid=element_line(color = "gray94"), panel.border = element_rect(fill = NA, colour = "gray92")) +
  labs(title = "Number of boardgames released", x="Release year", y="Number of games", fill="Minimum number of players") + 
  facet_wrap(~boardgamecategory, scales = "free_y", ncol = 1) 

# Check average ratings for these games
game_rating <- ggplot(data = boardgame_aggregate, aes(year, avg_rating)) +
  geom_line(color = "darkgoldenrod1", size = 1) + 
  geom_point(color = "darkgoldenrod2")+
  theme(text = element_text(size=16), legend.position = "top", panel.background = element_rect(fill = "White"), panel.grid=element_line(color = "gray94"), panel.border = element_rect(fill = NA, colour = "gray92")) +
  labs(title = "Boardgame ratings in different category", x="Release year", y="Average rating", fill="Minimum number of players") + 
  facet_grid(minplayers~boardgamecategory) 

ggsave("gamerelase.jpeg", gamerelease, width = 20,height = 18, units="cm",dpi = 300 )
ggsave("gamerating.jpeg", game_rating, width = 24,height = 16, units="cm",dpi = 300 )
