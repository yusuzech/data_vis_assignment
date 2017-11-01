setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel"))

# dir.create("domestic_overseas_analysis")
#-----------------------------------------------------
#overall domestic and global revenue 
g1 <- movie %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_text(mapping = aes(x = -Inf, y = Inf, 
                          label = str_c("correalation is ",round(cor(us_rev, overseas_rev),4))),
            vjust = "top", hjust = "left"); g1

ggsave("overall_domestic_overseas.jpeg",path = "domestic_overseas_analysis")
#------------------------------------------------------
#domestic and global revenue grouped by genre
g_cor_text <- movie %>%
  group_by(genre) %>%
  summarise(count_of_rows = n(),
            correlation = round(cor(us_rev,overseas_rev),4)) %>%
  mutate(x = -Inf,
         y = Inf, 
         text = str_c("correlation is ",correlation,"\nn = ",count_of_rows))

g2 <- movie %>% 
  left_join(g_cor_text, by = "genre") %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~fct_reorder(genre,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text, vjust = "top", hjust = "left");g2

ggsave("domestic_overseas_by_genre.jpeg",path = "domestic_overseas_analysis")
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

g3 <- movie %>% 
  left_join(g_cor_text1, by = "studio") %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm",se = FALSE) +
  facet_wrap(~fct_reorder(studio,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text1, vjust = "top", hjust = "left");g3

ggsave("domestic_overseas_by_studio.jpeg",path = "domestic_overseas_analysis")

#interprete visually
invest_studio <- c("Buena Vista Studios", "WB", "Fox", "Universal", "Sony", "Paramount Pictures", "New Line Cinema", "DreamWorks", "Lionsgate/Summit",
                   "Summit Entertainment")
#----------------------------------------------------------


