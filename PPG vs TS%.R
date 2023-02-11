url <- "https://www.basketball-reference.com/leagues/NBA_2023_per_game.html"
html_content <- read_html(url)

per_game <- html_content %>% html_table(header = TRUE)
per_game <- per_game[[1]]

url2 <- "https://www.basketball-reference.com/leagues/NBA_2023_advanced.html"

html_content2 <- read_html(url2)

adv <- html_content2 %>%
  html_table(header = T)
adv <- adv[[1]]

col_to_add <- adv$`TS%`
per_game$`TS%` <- col_to_add
# Notice that within our per_game dataframe, almost all the values are set as characters
# we need to change them to numerics.
per_game$`TS%` <- as.numeric(per_game$`TS%`)
per_game$PTS <- as.numeric(per_game$PTS)
# We changed the important variables to numerics, now we can filter 
nba <- per_game %>%
  filter(PTS >= 20 & G >= 2)

str(nba)


graph <- nba %>%
  ggplot(aes(x=as.numeric(PTS), y = as.numeric(`TS%`), label = Player))+
  geom_point(aes(fill=Pos, color = after_scale(clr_darken(fill, 0.3))),
             shape = 21, 
             alpha = .75,
             size = 3)+
  geom_text_repel(size = 2, color = "black", min.segment.length = unit(0.1, "lines"))+
  scale_x_log10()+
  scale_y_log10()+
  scale_fill_discrete(limits = c("PG", "SG", "SF", "PF", "C"))+
  theme_jelly()+
  theme(
    axis.line = element_line(color="black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color ="black"),
    plot.title = element_text(color = "black", hjust = .5, face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray10", hjust = .5, face = "bold", size = 8))+
  labs(
    title = "PPG vs TS% amongst 20 PT Scorers",
    subtitle = "2022-23 Regular Season || Data:BBallRef\n",
    x = "Points Per Game",
    y = "True Shooting %"
  )

ggsave("PPG vs TS% (20PT Scorers).png", height = 6, width = 6, dpi = 300)
