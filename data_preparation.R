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
