#### ANALYZE YCA SURVEY FOR MEETING TIMES ####
## Author(s): Clara Wang
## September 2019

## ----------------------------< SET UP >---------------------------------------
library(tidyverse)
library(ggthemes)

setwd("~/Desktop/YCA/yca_ds/yca_dstech")

## import data
df <- read.csv("logistics.csv", stringsAsFactors = FALSE)

write.csv(deid, "deid_logistics.csv")

deid <- df %>%
  dplyr::select(-c(Date.Created, IP.Address, Please.enter.your.name, Last, Please.enter.your.email.address))

## ----------------------------< CLEAN UP >-------------------------------------

# identify columns asking meeting times
meeting_cols <- seq(1, ncol(df))[grepl("When.would.you.be.free", names(df))]

# grab text
meeting_times <- map_chr(.x = df[, meeting_cols],
                         .f = function(x) {setdiff(unique(x), c(""))})

# set names of meeting time columns to actual times
names(df)[meeting_cols] <- meeting_times

# recode data frame
df_recode <- df %>%
  dplyr::mutate_at(.vars = meeting_cols,
                   .funs = function(x) {ifelse(x == "", 0, 1)})


## ------------------------< MEETING TIME PLOT >--------------------------------

meeting_plot <- df_recode[, meeting_times] %>%
  gather(time, response) %>%
  mutate("day" = factor(word(time, 1),
                        levels = c("Tuesdays", "Thursdays", "Fridays",
                                   "Saturdays", "Sundays")),
         "start_time" = word(time, 2),
         "end_time" = word(time, 4),
         "time_range" = paste0(start_time, "-", end_time)) %>%
  filter(response == 1) %>%
  group_by(time) %>%
  mutate(n_responses = sum(response)) %>%
  ungroup() %>%
  mutate(over_10 = ifelse(n_responses >= 10, "over", "under")) %>%
  ggplot() +
  geom_bar(aes(x = time_range, fill = over_10)) +
  facet_grid(. ~ day, scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks = seq(0, 12, 3),
                     labels = seq(0, 12, 3),
                     name = "Number of Respondents") +
  scale_fill_manual(values = c("over" = "darkgreen", "under" = "gray60"),
                    name = "# Respondents",
                    labels = c(">= 10", "<10")) +
  theme_hc() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  +
  labs(x = "Time Range",
       title = "Yenching DS & Tech Club Meeting Time Preferences",
       subtitle = paste0("N = ", nrow(df)))
meeting_plot
