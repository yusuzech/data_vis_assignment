#--------------------------------------------
#setup ("setup" function in "setup.R)
setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats"),
      data_urls = "https://raw.githubusercontent.com/yusuzech/data_vis_assignment/master/movie.csv")

#-------------------------------------------
#data preparation
original_data <- movie
#(1)Change all column name to lower cases
#(2)Create avg_rating column (imdb_rating + movielens_rating)/2
colnames(original_data) <- tolower(colnames(original_data))
movie <- original_data %>%
  mutate(avg_rating = (imdb_rating + 2*movielens_rating)/2)


#-----------------------------------------
#data exploration
#(1) rating difference
#add rate_diff column
install.packages('ggrepel')
library(ggrepel)
d1 <- movie %>%
  mutate(imdb_minus_movielens = imdb_rating - 2*movielens_rating, 
         x = row_number())

#scatter plot for difference of two ratings
g1 <- d1 %>%
  ggplot(mapping = aes(x = x, y =imdb_minus_movielens)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE);g1
#We find that if two ratings are in same scale, for the same movies, imdb_ratings are slightly bigger than movielens_ratings

#find out movies that have big divergence between two ratings
d1_text <- d1 %>%
  filter(abs(imdb_minus_movielens) > 1)
g1_1 <- g1 + 
  geom_text_repel(mapping = aes(x = x, y = imdb_minus_movielens, label = movie_title), data = d1_text, nudge_x = 0.2,nudge_y = 0.15) +
  geom_hline(yintercept = 0, color = "red");g1_1

#find those exceptional movies and see these movies' performance
g1_2 <- d1_text %>%
  ggplot() +
  geom_point(mapping = aes(x = adjusted_gross, y = imdb_minus_movielens)) +
  geom_hline(yintercept = 0, color = "red");g1_1
#We can see that either imdb overestimate those movies, or movielens underestimate those movies
#I can't figure it out myself



#(2)--------------------------------------------------------------------------
#question 1 : Does higher rating movie also have a higher adjusted gross revenue?
#(2) - 1
g2 <- movie %>%
  ggplot(mapping = aes(x = avg_rating, y = adjusted_gross),alpha = 0.5) +
  geom_point();g2

d2_text <- tibble(x = -Inf, y = Inf, correlation = round(with(movie,cor(avg_rating,adjusted_gross)),6)) %>%
  mutate(text = str_c("Correaltio between adjusted gross revenue and average rating\n",correlation))

g2_text <- g2 +geom_text(mapping  = aes( x = x, y =y, label = text),data = d2_text, hjust = "left", vjust = "top")
#We can see no relavance, we see breakdown by studio and category as next step
g2_1 <- g2 + facet_wrap(~genre);g2_1
g2_1_text <- movie %>% 
  group_by(genre) %>%
  summarise(x = -Inf, y = Inf,
            correlation = cor(avg_rating, adjusted_gross),
            size = n(),
            text = str_c("Correlation is ",round(correlation,5),"\nn = ", size))

g2_2 <- g2 + 
  geom_text(mapping = aes(x = x, y = y, label = text),data = g2_1_text,vjust = "top", hjust = "left") + 
  facet_wrap(~genre);g2_2

