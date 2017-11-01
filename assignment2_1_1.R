#--------------------------------------------
#setup ("setup" function in "setup.R).
setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel"),
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


# demonstrate the distribution of rating
movie%>%
  ggplot(mapping = aes(x = avg_rating)) +
  geom_histogram() 

fivRate<-fivenum(movie$avg_rating)

movie_highRate<-movie%>%
  filter(avg_rating>fivRate[4])

movie_highRate%>%
  ggplot(mapping = aes(x = avg_rating, y =adjusted_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE)
# geom_text_repel(mapping = aes( label = movie_title), nudge_x = 0.2,nudge_y = 0.15) +
# geom_hline(yintercept = 0, color = "red")

movie_avgRate<-movie%>%
  filter(avg_rating>fivRate[2] & avg_rating<fivRate[4])

movie_avgRate%>%
  ggplot(mapping = aes(x = avg_rating, y =adjusted_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE)

movie_lowRate<-movie%>%
  filter(avg_rating<fivRate[2])

movie_lowRate%>%
  ggplot(mapping = aes(x = avg_rating, y =adjusted_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE)

rating_text <- tibble(x = c(6.75,8.25, 5), y = 2000, text = c("average rating", "high rating","low rating"))

final_graph <- ggplot() + 
  geom_point(data = movie_avgRate, mapping = aes(x = avg_rating, y =adjusted_gross), alpha = 0.5) +
  geom_smooth(data = movie_avgRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "blue", se = FALSE) +
  geom_point(data = movie_highRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "green", alpha = 0.5) +
  geom_smooth(data = movie_highRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "red", se = FALSE) +
  geom_point(data = movie_lowRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "red", alpha = 0.5) +
  geom_smooth(data = movie_lowRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "red", se = FALSE) +
  geom_text(data = rating_text, mapping = aes(x = x, y = y, label = text));final_graph
