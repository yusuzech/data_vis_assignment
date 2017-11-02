setup(desired_working_file_name = "team_assignment_1",
      required_packages = c("tidyverse","stringr","forcats","ggrepel","RColorBrewer"))

# dir.create("domestic_overseas_analysis")
#-----------------------------------------------------
#overall domestic and global revenue 
g1 <- movie %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5, color = "lightskyblue3") +
  geom_smooth(se = FALSE, method = "lm",color = "steelblue4") +
  geom_text(mapping = aes(x = -Inf, y = Inf, 
                          label = str_c("correalation is ",round(cor(us_rev, overseas_rev),4))),
            vjust = "top", hjust = "left") +
  labs(x = "Movie Revenue in US", y = "Movie Revenue Overseas", title = "Movies Do Well in US also Do Well Overseas") +
  theme_light(); g1

ggsave("overall_domestic_overseas.jpeg",path = "domestic_overseas_analysis",width = 30, height = 30 *0.618, units = "cm")
#------------------------------------------------------
#domestic and global revenue grouped by genre
g_cor_text <- movie %>%
  group_by(genre) %>%
  filter(n() >= 10) %>%
  summarise(count_of_rows = n(),
            correlation = round(cor(us_rev,overseas_rev),4)) %>%
  mutate(x = -Inf,
         y = Inf, 
         text = str_c("correlation is ",correlation,"\nn = ",count_of_rows))

g2 <- movie %>% 
  left_join(g_cor_text, by = "genre") %>%
  group_by(genre) %>%
  filter(n() >= 10) %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5,mapping = aes(color = genre)) + 
  geom_smooth(method = "lm",se = FALSE,color = "grey32") +
  facet_wrap(~fct_reorder(genre,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text, vjust = "top", hjust = "left") +
  labs(x = "Revenue in US", y = "Revenue Overseas", title = "Comedy, Thriller, Biography and Crime Movies Do Well in US May Not Do Well Overseas",
       subtitle = "genre that have less than 10 movies are not displayed") +
  scale_colour_brewer(palette="Set3") +
  theme_light() +
  theme(legend.position="none") ;g2

ggsave("domestic_overseas_by_genre.jpeg",path = "domestic_overseas_analysis",width = 30, height = 30 *0.618, units = "cm")
#interprete visually
invest_genre <- c("action", "animation", "drama", "adventure", "sci-fi", "fantasy")
 
#--------------------------------------------------------- 
#domestic and global revenue grouped by studio
g_cor_text1 <- movie %>%
  group_by(studio) %>%
  filter(n() >= 10) %>%
  summarise(count_of_rows = n(),
            correlation = round(cor(us_rev,overseas_rev),4)) %>%
  mutate(x = -Inf,
         y = Inf, 
         text = str_c("correlation is ",correlation,"\nn = ",count_of_rows))

g3 <- movie %>% 
  left_join(g_cor_text1, by = "studio") %>%
  group_by(studio) %>%
  filter(n() >= 10) %>%
  ggplot(mapping = aes(x = us_rev, y = overseas_rev)) +
  geom_point(alpha = 0.5,mapping = aes(color = studio)) + 
  geom_smooth(method = "lm",se = FALSE,color = "grey32") +
  facet_wrap(~fct_reorder(studio,desc(count_of_rows))) +
  geom_text(mapping = aes(x = x, y = y, label = text), data = g_cor_text1, vjust = "top", hjust = "left") +
  labs(x = "Revenue in US", y = "Revenue Overseas", title = "Movies Made by Big Studios Usually Do Well Both in US and Overseas",
       subtitle = "studios that have less than 10 movies are not displayed") +
  scale_colour_brewer(palette="Set3") +
  theme_light() + 
  theme(legend.position="none");g3

ggsave("domestic_overseas_by_studio.jpeg",path = "domestic_overseas_analysis",width = 30, height = 30 *0.618, units = "cm")

#interprete visually
invest_studio <- c("Buena Vista Studios", "WB", "Fox", "Universal", "Sony", "Paramount Pictures", "New Line Cinema", "DreamWorks", "Lionsgate/Summit",
                   "Summit Entertainment")
#----------------------------------------------------------


