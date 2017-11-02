#--------------------------------------------
#setup ("setup" function in "setup.R")
setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel","RColorBrewer"))

# dir.create("Time_Series_Analysis")
#Time series analysis

#Total-------------------------------------------------------------
g1 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year) %>%
  summarise(total_gross = sum(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = total_gross)) +
  geom_col(fill = "salmon") +
  labs(x = "Month", y = "Adjusted Total Gross Revenue", title = "Most Movies are released in May-July and Nov-Dec(Holiday seasons)") +
  theme_light();g1

ggsave("total_gross_to_month.jpeg",path = "Time_Series_Analysis",width = 30, height = 30 *0.618, units = "cm")

g2 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year) %>%
  summarise(average_gross = mean(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = average_gross)) +
  geom_col(fill = "salmon") +
  labs(x = "Month", y = "Average Gross Revenue", title = "Average Movie revenue are higher in Apr-Jul and Nov-Jan") +
  theme_light(); g2

ggsave("average_gross_to_month.jpeg",path = "Time_Series_Analysis",width = 30, height = 30 *0.618, units = "cm")

#By genre---------------------------------------------------
#total gross by genre
movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(genre,month_of_year) %>%
  summarise(total_gross = sum(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = genre)) +
  geom_tile(mapping = aes(fill = total_gross)) +
  scale_fill_gradient(low = "lightskyblue1", high = "deepskyblue3")
ggsave("total_gross_to_month_by_genre.jpeg",path = "Time_Series_Analysis",width = 30, height = 30 *0.618, units = "cm")

movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(genre,month_of_year) %>%
  summarise(avg_gross = mean(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = genre)) +
  geom_tile(mapping = aes(fill = avg_gross)) +
  scale_fill_gradient(low = "lightskyblue1", high = "deepskyblue3")

ggsave("average_gross_to_month_by_genre.jpeg",path = "Time_Series_Analysis",width = 30, height = 30 *0.618, units = "cm")