library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

# http://www.census.gov/topics/population/genealogy/data/2000_surnames.html
surnames <- read.csv("./data/surnames.csv", stringsAsFactors = FALSE, 
                     header = TRUE, skip = 1)
head(surnames)
str(surnames)

surnames$letter <- str_sub(surnames$name, start = 1L, end = 1L)

agg <- surnames %>% 
  group_by(letter) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  data.frame

L <- toupper(letters)
grid <- data.frame(letter = L)

agg <- merge(grid, agg, all.x = TRUE, by = "letter")
agg$count[is.na(agg$count)] <- 0

agg$first <- sort(rep(c(0, 1), 13))

ggplot(agg, aes(x = letter, y = count)) + 
  geom_bar(stat = 'identity') + 
  scale_x_discrete("Letter") + 
  scale_y_continuous("Frequency", label = comma) + 
  ggtitle("") + 
  theme_bw()
ggsave("letter.png")

agg %>% 
  group_by(first) %>% 
  summarise(count = sum(count)) %>% 
  ungroup() %>% 
  mutate(first = ifelse(first == 0, "A - M", "N - Z")) %>% 
  data.frame %>% 
  ggplot(aes(x = first, y = count)) + 
  geom_bar(stat = 'identity') + 
  scale_x_discrete("Letter") + 
  scale_y_continuous("Frequency", label = comma) + 
  ggtitle("") + 
  theme_bw()
ggsave("first_or_last.png")
