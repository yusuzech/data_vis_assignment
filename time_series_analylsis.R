#--------------------------------------------
#setup ("setup" function in "setup.R")
setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel","lubridate"))

# dir.create("Time_Series_Analysis")
#Time series analysis

#Total-------------------------------------------------------------
g1 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year) %>%
  summarise(total_gross = sum(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = total_gross)) +
  geom_col();g1

ggsave("total_gross_to_month.jpeg",path = "Time_Series_Analysis")

g2 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year) %>%
  summarise(average_gross = mean(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = average_gross)) +
  geom_col(); g2

ggsave("average_gross_to_month.jpeg",path = "Time_Series_Analysis")

#By genre---------------------------------------------------
#total gross by genre
g3 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year,genre) %>%
  summarise(total_gross = sum(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = total_gross)) +
  geom_col() +
  facet_wrap(~genre);g3
ggsave("total_gross_to_month_by_genre.jpeg",path = "Time_Series_Analysis")

#average gross by genre
g4 <- movie %>%
  mutate(month_of_year = as.factor(month(release_date))) %>%
  group_by(month_of_year,genre) %>%
  summarise(average_gross = mean(adjusted_gross)) %>%
  ggplot(mapping = aes(x = month_of_year, y = average_gross)) +
  geom_col() +
  facet_wrap(~genre); g4

ggsave("average_gross_to_month_by_genre.jpeg",path = "Time_Series_Analysis")

#-------------------------------------------------------------