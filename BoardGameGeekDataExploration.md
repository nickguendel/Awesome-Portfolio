Board Game Geek Exploration
================


# Exploring BBG Ratings
##### Creating 3 Graphs from a Dataset made availible by [Board Game Geek](https://boardgamegeek.com/). The dataset can be found [here](https://www.kaggle.com/datasets/jvanelteren/boardgamegeek-reviews?select=games_detailed_info.csv).
***
### Overview of Exploration
#### Relationships in the data that I am interested in understanding with visualizations:
1. Setting up the environment and reading the data.
2. Do more players increase the playtime?
3. How do user ratings compare with the year the game was published?
4. How do the average ratings of the games from a game designer created compare to other game designers?


### 1. Setting up the environment and reading the data.

##### Loading Packages and downloading data files
`
```{r Setting-up Environment and Inital Processing, collabse=FALSE, include=FALSE, results=FALSE}
library(tidyverse)
library(ggplot2)
library(dplyr)


ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')
```
##### Looking at the information inside the data
#####
Ratings Table
```{r looking at ratings table, results='markup'}
colnames(ratings)
```

Details Table
```{r looking at details table, results='markup'}
colnames(details)
```
##### Joining the Ratings and Details Tables 

I used the id column as the key in the two data sets.
```{r joining data}
join_rating_details <- ratings %>% 
  outer_join(details, by = "id")
glimpse(join_rating_details)
```


### 2. Do more players increase the playtime?
##### I filtered the visualization for modern games (published after 1950) that can be played in a day with breaks and with 10 players or less. 
```{r}
data_time_vs_players <- join_rating_details %>% 
  filter(maxplayers <= 10, maxplayers != 0) %>% 
  filter(playingtime <= 360, playingtime != 0) %>% 
  filter(yearpublished > 1950)
```

```{r players increase the playtime}

ggplot(data = data_time_vs_players) + 
  geom_smooth(mapping = aes(x=maxplayers,y=playingtime))+
  labs(x="Number of Players",y="Playing Time", title = "HOW LONG WILL A GAME TAKE BASED ON THE NUMBER OF PLAYERS?",
       subtitle = "Two to Three Player Games Typically Take Longer to Play Than Four to 10 Player Games",
       caption = "Created by Nick Guendel with data from boardgamegeeks.com") +
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10) )
# Notes: Rebuild with bar chart, average time to play a game vs the number of players
```


### 3. How do user ratings correlate with the date published?
Modern games have the highest ratings. The data shows users, on average, rate newly published games higher than past games. Some interesting factors on this are that newly published games have the advantage of being rated by user when the game is being published and not decades or centries afterwards.
```{r}
df_year_vs_rating <- join_rating_details %>% 
  filter(yearpublished >= 1950)
  
ggplot(data = df_year_vs_rating) +
  geom_jitter(mapping = aes(x = yearpublished, y = average, alpha = .25)) +
  geom_smooth(mapping = aes(x = yearpublished, y = average, color="User Average"))+
  labs(x="Year Published",y="Average Rating",title = "BBG Current Rating of Games Bases on the Year Published", 
       subtitle = "BBG ratings are highest for the most recently published games", 
       caption = "Created by Nick Guendel with data from ***")+
  theme(legend.position="none")
```


### 4. How do the average ratings of the games from a game designer compare to other game designers?
```{r ratings of the games from a game designer}

# Selecting Data
board_game_category <- join_rating_details %>% 
  select(id,name,yearpublished,boardgamecategory,boardgamedesigner,average,yearpublished,bayes_average)

# Removing brakets, parenthesis, and quotes from Names
cat_cleaned <- 
  data.frame(board_game_category = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", 
                                        as.character(board_game_category$boardgamecategory)))
designer_cleaned <- 
  data.frame(board_game_category = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", 
                                        as.character(board_game_category$boardgamedesigner)))

# Combining the cleaned rows with the original data set and removing unclean columns
board_game_category <- bind_cols(board_game_category, cat_cleaned, designer_cleaned) 

board_game_category <- board_game_category %>% 
  select(-boardgamecategory,-boardgamedesigner)

board_game_category <- board_game_category %>% 
  rename(category=board_game_category...8) %>% 
  rename(designer=board_game_category...9)

# Separating the list of categories into individual rows
board_game_designer <- separate_rows(board_game_category, designer, sep = ", ")

# Counts the number in the average
num_games_of_designer <- board_game_designer %>% 
  count(designer, sort = TRUE)

# average of all games by that designer
board_game_designer_avg <- board_game_designer %>% 
  group_by(designer) %>% 
  summarise(designer_avg=mean(average))

board_game_designer <- board_game_designer_avg %>% 
  inner_join(num_games_of_designer, by="designer")

board_game_designer_avg_filtered <- board_game_designer %>% 
  filter(n > 4) %>% 
  filter(designer_avg > (mean(designer_avg)+(2*sd(designer_avg))))

# Plotting designer averages
ggplot(data = board_game_designer_avg_filtered) +
  geom_col(aes(x = reorder(designer, designer_avg), y = designer_avg), fill = "brown2")+
  coord_flip()+
  labs(x="Game Designer",y="User Rating",title = "Rankings of Top Game Designers")+
  theme_minimal()
```


### 5. Which category of grams have the highest ratings?
```{r}

board_game_category <- join_rating_details %>% 
  select(name,boardgamecategory,average)
  
cat_cleaned <- 
  data.frame(board_game_category = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", 
                             as.character(board_game_category$boardgamecategory)))

board_game_category <- bind_cols(board_game_category, cat_cleaned) 
  
board_game_category <- board_game_category %>% 
  select(-boardgamecategory)

board_game_category <- board_game_category %>% 
  rename(category=board_game_category)

board_game_category <- separate_rows(board_game_category, category, sep = ", ")

num_games_per_category <- board_game_category %>% 
  count(category, sort = TRUE)

board_game_category <- board_game_category %>% 
  group_by(category) %>% 
  summarise(avg=mean(average))

board_game_category <- num_games_per_category %>% 
  inner_join(board_game_category, by="category")
  
graph_filter <- board_game_category %>% 
  filter(category != 'NA') %>% 
  filter(category!='Fan Expansion') %>% 
  filter(avg > mean(avg)) %>% 
  filter(n > 500)

ggplot(data = graph_filter) +
  geom_col(aes(x = reorder(category, avg), y = avg), fill = "deepskyblue4")+
  coord_flip()+
  labs(x="Category",y="User Rating",title = "Highest Rated Games By Category")+
  theme_minimal()

```