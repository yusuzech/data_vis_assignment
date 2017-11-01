setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel"),
      data_urls = "https://raw.githubusercontent.com/yusuzech/data_vis_assignment/master/movie.csv")

#-----------------------------------------------------
#overall domestic and global revenue 
movie %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_text(mapping = aes(x = -Inf, y = Inf, 
                          label = str_c("correalation is ",round(cor(us_rev, overseas_rev),4))),
            vjust = "top", hjust = "left")

#------------------------------------------------------
#domestic and global revenue grouped by genre
g_cor_text <- movie %>%
  group_by(genre) %>%
  summarise(count_of_rows = n(),
            correlation = round(cor(us_rev,overseas_rev),4)) %>%
  mutate(x = -Inf,
         y = Inf, 
         text = str_c("correlation is ",correlation,"\nn = ",count_of_rows))

g <- movie %>% 
  left_join(g_cor_text, by = "genre") %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~fct_reorder(genre,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text, vjust = "top", hjust = "left");g

#interprete visually
invest_genre <- c("action", "animation", "drama", "adventure", "sci-fi", "fantasy")
 
#--------------------------------------------------------- 
#domestic and global revenue grouped by studio
g_cor_text1 <- movie %>%
  group_by(studio) %>%
  summarise(count_of_rows = n(),
            correlation = round(cor(us_rev,overseas_rev),4)) %>%
  mutate(x = -Inf,
         y = Inf, 
         text = str_c("correlation is ",correlation,"\nn = ",count_of_rows))

g1 <- movie %>% 
  left_join(g_cor_text1, by = "studio") %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~fct_reorder(studio,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text1, vjust = "top", hjust = "left");g1

#interprete visually
invest_studio <- c("Buena Vista Studios", "WB", "Fox", "Universal", "Sony", "Paramount Pictures", "New Line Cinema", "DreamWorks", "Lionsgate/Summit",
                   "Summit Entertainment")
#----------------------------------------------------------

    

