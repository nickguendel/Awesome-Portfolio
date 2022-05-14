library(tidyverse)
library(ggplot2)
library(dplyr)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')


# inner join with id number -----------------------------------------------
join_rating_details <- ratings %>% 
  inner_join(details, by = "id")

board_games <- join_rating_details %>% 
  select(-primary,-num.x,-num.y,-boardgameimplementation,-boardgamefamily)

regex_cleaned <- 
  data.frame(designer = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgamedesigner)), 
             category = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgamecategory)),
             publisher = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgamepublisher)),
             artist = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgameartist)),
             mechanics = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgamemechanic)),
             expansion = gsub("\\[|\\]|\\'|\\(|\\)|\"", "", as.character(board_games$boardgameexpansion)),
             id = board_games$id)

board_games <- board_games %>% 
  inner_join(regex_cleaned, by = "id") %>% 
  select(-boardgamedesigner,-boardgamecategory,-boardgamepublisher,
         -boardgameartist,-boardgamemechanic,-boardgameexpansion)

write.csv(board_games,"bggData.csv")

gamedesigner_separated <- 
  separate_rows(board_games, designer, sep = ", ")

write.csv(gamedesigner,"bggDataLongDesigner.csv")

gamecategory_separated <- 
  separate_rows(board_games, category, sep = ", ")

write.csv(gamecategory_separated,"bggDataLongCategory.csv")














