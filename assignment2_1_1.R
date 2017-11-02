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
  geom_smooth(color = "blue", se = FALSE)+
  ylim(0,2000)
# geom_text_repel(mapping = aes( label = movie_title), nudge_x = 0.2,nudge_y = 0.15) +
# geom_hline(yintercept = 0, color = "red")

movie_avgRate<-movie%>%
  filter(avg_rating>fivRate[2] & avg_rating<fivRate[4])

movie_avgRate%>%
  ggplot(mapping = aes(x = avg_rating, y =adjusted_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE)+
  ylim(0,2000)

movie_lowRate<-movie%>%
  filter(avg_rating<fivRate[2])

movie_lowRate%>%
  ggplot(mapping = aes(x = avg_rating, y =adjusted_gross)) +
  geom_point(alpha = 0.5) +
  geom_smooth(color = "blue", se = FALSE)+
  ylim(0,2000)

rating_text <- tibble(x = c(6.75,8.25, 5), y = 1500, text = c("average rating", "high rating","low rating"))

final_graph <- ggplot() + 
  geom_point(data = movie_avgRate, mapping = aes(x = avg_rating, y =adjusted_gross), color="seashell4",alpha = 0.3, size=3) + ylim(0,1800)+
  geom_smooth(data = movie_avgRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "grey37", se = FALSE) +
  geom_point(data = movie_highRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "palegreen2", alpha = 0.5,size=3) +ylim(0,1800)+
  geom_smooth(data = movie_highRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "seagreen4", se = FALSE) +
  geom_point(data = movie_lowRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "maroon1", alpha = 0.5,size=3) +ylim(0,1800)+
  geom_smooth(data = movie_lowRate, mapping = aes(x = avg_rating, y =adjusted_gross), color = "indianred4", se = FALSE) +
  geom_text(data = rating_text, mapping = aes(x = x, y = y, label = text),size=5)+
  labs(title='Rating and Gross correlation for movies in Netflix', size=10)+
  theme_light();final_graph

ggsave(filename = "Rating & Gross.jpeg",path = "graph", width = 30, height = 30 * 0.618, units = "cm")



