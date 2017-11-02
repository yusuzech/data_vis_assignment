#--------------------------------------------
#Some useful graphs
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
