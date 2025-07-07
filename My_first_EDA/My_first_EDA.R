library(tidyverse)

article.raw <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2025/2025-02-25/article_dat.csv")

model_dat <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2025/2025-02-25/model_dat.csv")

View(model_dat)

unique(model_dat$compare)


Description <- article.raw %>% 
  
  select(c("pmid", "title" , "year", "month", "day", "study_location", 13, 14, 15))

View(Description)

sampledat 

popn <- article.raw %>% 
  select(c("pmid", 17:32)) 

popn %>% 
  select(ends_with("ss")) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Race"
  )
paste0(floor(mean(Description$study_location == "USA", na.rm = T)*100), "%")


Description %>% 
  filter(!study_location %in% c("USA", "Unclear", "N/A")  & !is.na(study_location)) %>% 
  separate_longer_delim(study_location, delim = ",") %>% 
  separate_longer_delim(study_location, delim = ".") %>% 
  mutate(
    study_location = str_to_upper(study_location),
    study_location = str_remove_all(study_location, " "),
  ) %>% 
  group_by(study_location) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  )

geom_label_repel

Description %>% 
  filter(!study_location %in% c("USA", "Unclear", "N/A")  & !is.na(study_location)) %>% 
  separate_longer_delim(study_location, delim = ",") %>% 
  separate_longer_delim(study_location, delim = ".") %>% 
  mutate(
    study_location = str_to_upper(study_location),
    study_location = str_remove_all(study_location, " "),
  ) %>% 
  group_by(study_location) %>% 
  summarise(
    N = n(),
    .groups = "drop"
  ) %>%  
  filter(!study_location %in% c("CA", "N/A")) %>% 
  arrange(desc(N)) %>% 
  mutate(
    study_location = fct_reorder(study_location, N)
  ) %>% 
  ggplot(aes(study_location, N, fill = study_location)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none")+
  scale_y_continuous(breaks = seq(0, 30, 3))




View(Research_outcome)
