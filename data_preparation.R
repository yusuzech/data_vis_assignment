#--------------------------------------------
#setup ("setup" function in "setup.R")
setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel","lubridate","RColorBrewer"))


#-------------------------------------------
#data preparation
original_data <- movie
#(1)Change all column name to lower cases
#(2)Create avg_rating column (imdb_rating + movielens_rating)/2
colnames(original_data) <- tolower(colnames(original_data))
movie <- original_data %>%
  mutate(avg_rating = (imdb_rating + 2*movielens_rating)/2,
         release_date = dmy(release_date))


###
mycolors = c(brewer.pal(name="Set3", n = 12), brewer.pal(name="Set3", n = 12),brewer.pal(name="Set3", n = 12))
movie %>%
  group_by(studio) %>%
  summarise(avg_gorss = mean(adjusted_gross)) %>%
  mutate(rank = min_rank(desc(avg_gorss))) %>%
  ggplot() +
  geom_col(mapping = aes(x = fct_reorder(studio,rank), y = avg_gorss,fill = studio)) +
  coord_flip() +
  theme_light() +
  scale_fill_manual(values = mycolors) +
  labs(y = "Average Gross Revenue", x = "Studio", title = "Studio Performance") +
  theme(legend.position="none")

ggsave("Studio_Performance.jpeg",path = "Time_Series_Analysis",width = 30, height = 30 *0.618, units = "cm")
  
movie %>%
  mutate(month_of_year = month(release_date)) %>%
  group_by(month_of_year) %>%
  summarise(avg_gross = mean(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = avg_gross)) +
  geom_line() +
  scale_x_continuous(breaks=c(1:12),labels=as.character(c(1:12)))
