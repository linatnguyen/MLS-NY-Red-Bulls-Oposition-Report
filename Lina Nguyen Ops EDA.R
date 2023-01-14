Lina Nguyen

# loading necessary packages
library(dplyr)
library(tidyr)
library(magrittr)
library(ggsoccer)
library(StatsBombR)
library(ggplot2)
library(e1071)
library(caret)
library(ggcorrplot)
library(fmsb)
library(caret)
library(reshape2)
library(corrplot)
library(imbalance)
library(randomForest)

# importing data
lu <- readRDS("/Users/linanguyen/Downloads/Data Scientist Test Code - Final/lineups_for_test.RDS")
me <- readRDS("/Users/linanguyen/Downloads/Data Scientist Test Code - Final/match_events_for_test.RDS")


# Feature Engineering
summary(me)

# looking at dataset dimensions
dim(me)

# counting number of distinct values in all columns
sapply(me, function(x) n_distinct(x))

#break dataset into their respective games
# CC: Columbus Crew, NYC: NYCFC, NER: New England Revolution, C: Charlotte, PU: Philadelphia Union
CC <- me[me$match_id == 3817375, ]
NYC <- me[me$match_id == 3817391, ]
NER <- me[me$match_id == 3817433, ]
C <- me[me$match_id == 3817468, ]
PU <- me[me$match_id == 3817604, ]

# EDA
# pressure heat map per game/team/player
heatall <- me %>% 
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

vCPress <- C %>%
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

vCCPress <- CC %>%
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

vNYCPress <- NYC %>%
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

vPUPress <- PU %>%
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

vNERPress <- NER %>%
  filter(type.name == 'Pressure') %>%
  filter(possession_team.name == "New York Red Bulls")

# Plotting Team Pressure Heat Maps
# Heat Map Overall
ggplot(me) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map of Last 5 Games in 2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# v. Charlotte 
ggplot(vCPress) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map",
       subtitle="NYRB v. Charlotte 10/9/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. Columbus
ggplot(vCCPress) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map",
       subtitle="NYRB v. Columbus 10/1/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. NYC 
ggplot(vNYCPress) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map",
       subtitle="NYRB v. NYC 9/17/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. Philidelphia Union
ggplot(vPUPress) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map",
       subtitle="NYRB v. Philadelphia 9/3/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. New England Revolution
ggplot(vNERPress) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB Pressure Heat Map",
       subtitle="NYRB v. New England 9/10/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# Pressure during 4231 tactic
vCPress4231 <- C %>%
  filter(type.name == 'Pressure') %>%
  filter(minute < 81) %>%
  filter(team.name == "New York Red Bulls")

vNYCPress4231 <- NYC %>%
  filter(type.name == 'Pressure') %>%
  filter(minute < 80) %>%
  filter(team.name == "New York Red Bulls")

vPUPress4231 <- PU %>%
  filter(type.name == 'Pressure') %>%
  filter(minute > 56) %>%
  filter(team.name == "New York Red Bulls")

all4231 <- me %>%
  filter(type.name == 'Pressure') %>%
  filter(team.name == "New York Red Bulls")

# 4231 Pressure Overall
ggplot(all4231) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 4231 Pressure Heat Map of Last 5 Games in 2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# v. Charlotte 
ggplot(vCPress4231) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 4231 Pressure Heat Map", 
       "NYRB v. Charlotte 10/9/2022")+ 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. NYC
ggplot(vNYCPress4231) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 4231 Pressure Heat Map",
       subtitle="NYRB v. NYC 9/17/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. PU 
ggplot(vPUPress4231) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 4231 Pressure Heat Map",
       subtitle="NYRB v. PU 9/3/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# pressure during 3412 tactic

vCPress3412 <- C %>%
  filter(type.name == 'Pressure') %>%
  filter(minute > 81) %>%
  filter(team.name == "New York Red Bulls")

vCCPress3412 <- CC %>%
  filter(type.name == 'Pressure') %>%
  filter(team.name == "New York Red Bulls")

vNYCPress3412 <- NYC %>%
  filter(type.name == 'Pressure') %>%
  filter(minute < 80) %>%
  filter(team.name == "New York Red Bulls")

vPUPress3412 <- PU %>%
  filter(type.name == 'Pressure') %>%
  filter(minute < 56) %>%
  filter(team.name == "New York Red Bulls")

all3412 <- me %>%
  filter(type.name == 'Pressure') %>%
  filter(team.name == "New York Red Bulls")

# 3412 Pressure Overall
ggplot(all3412) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 3412 Pressure Heat Map of Last 5 Games in 2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )
# v. Charlotte 
ggplot(vCPress3412) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 3412 Pressure Heat Map", 
       subtitle = 
         "NYRB v. Charlotte 10/9/2022")+ 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# v. Columbus Crew 
ggplot(vCCPress3412) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 3412 Pressure Heat Map", 
       subtitle = "NYRB v. Columbus Crew 10/1/2022")+ 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. NYC
ggplot(vNYCPress3412) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 3412 Pressure Heat Map",
       subtitle="NYRB v. NYC 9/17/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# vs. PU 
ggplot(vPUPress3412) +
  annotate_pitch(dimensions = pitch_statsbomb, fill='#021e3f', colour='#DDDDDD') +
  geom_density2d_filled(aes(location.x, location.y, fill=..level..), alpha=0.4, contour_var='ndensity') +
  scale_x_continuous(c(0, 120)) +
  scale_y_continuous(c(0, 80)) +
  labs(title="NYRB 3412 Pressure Heat Map",
       subtitle="NYRB v. PU 9/3/2022") + 
  theme_minimal() +
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    text = element_text(family="Geneva", color='white'),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  )

# shot map per game/team/player
shotmap <- me[c("match_id", "type.name", "shot.outcome.name", "location.x", "location.y", "shot.end_location.x", "shot.end_location.y", "shot.statsbomb_xg", "team.name")]

shotmap$shot.outcome.name[shotmap$shot.outcome.name != "Goal"] <- "Miss"

xshot <- shotmap %>%
  filter(type.name == 'Shot') %>%
  select(location.x)

mean(xshot$location.x, na.rm = TRUE)

yshot<- shotmap %>%
  filter(type.name == 'Shot') %>%
  select(location.y)

mean(yshot$location.y, na.rm = TRUE)

xshotG <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(shot.outcome.name == 'Goal') %>%
  select(location.x)

mean(xshotG$location.x, na.rm = TRUE)

yshotG <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(shot.outcome.name == 'Goal') %>%
  select(location.y)

mean(yshotG$location.y, na.rm = TRUE)

NYRBshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

CshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(match_id == 3817468) %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

CCshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(match_id == 3817375) %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

NYCshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(match_id == 3817391) %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

NERshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(match_id == 3817433) %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

PUshotAll <- shotmap %>%
  filter(type.name == 'Shot') %>%
  filter(match_id == 3817604) %>%
  filter(team.name == "New York Red Bulls") %>%
  select(location.x, location.y, shot.end_location.x, shot.end_location.y, shot.statsbomb_xg, shot.outcome.name)

# NYRB Shot Map for Last 5 Games of 2022"
ggplot(NYRBshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, fill = shot.outcome.name), shape = 21, size = 2) +
  labs(
    title="NYRB Shot Map Last 5 Games of 2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# v. Charlotte
ggplot(CshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, fill = shot.outcome.name), shape = 21, size = 2) + 
  labs(
    title="NYRB Shot Map vs. Charlotte",
    subtitle = "NYRB v. Charlotte 10/9/2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# v. Columbus Crew

ggplot(CCshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, fill = shot.outcome.name), shape = 21, size = 2) + 
  labs(
    title="NYRB Shot Map vs. Columbus Crew",
    subtitle = "NYRB v. Columbus Crew 10/1/2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# v. NYC
ggplot(NYCshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, colour = "darkturquoise", fill = "darkturquoise"), shape = 21, size = 2) + 
  labs(
    title="NYRB Shot Map vs. NYC",
    subtitle = "NYRB v. NYC 9/17/2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# PU
ggplot(PUshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, colour = "darkturquoise", fill = "darkturquoise"), shape = 21, size = 2) + 
  labs(
    title="NYRB Shot Map vs. PU",
    subtitle = "NYRB v. PU 9/3/2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )

# NE
ggplot(NERshotAll) +
  annotate_pitch(dimensions = pitch_statsbomb, colour='white', fill='#021e3f') +
  geom_point(aes(x=location.x, y=location.y, fill = shot.outcome.name), shape = 21, size = 2) + 
  labs(
    title="NYRB Shot Map vs. NER",
    subtitle = "NYRB v. NER 9/10/2022"
  ) + 
  theme(
    plot.background = element_rect(fill='#021e3f', color='#021e3f'),
    panel.background = element_rect(fill='#021e3f', color='#021e3f'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family="Geneva", color='white'),
    plot.title = element_text(hjust=0.5, vjust=0, size=14),
    plot.subtitle = element_text(hjust=0.5, vjust=0, size=8),
    plot.caption = element_text(hjust=0.5),
    plot.margin = margin(2, 2, 2, 2),
    legend.position = "none"
  )


# Expected Goals
xG <- me[c("shot.statsbomb_xg", "minute", "match_id", "possession_team.name")]

xG$minute <- as.numeric(as.character(xG$minute))

# charlotte
xGrbC <- xG %>%
  filter(match_id == 3817468) %>%
  filter(possession_team.name == "New York Red Bulls") %>%
  select(shot.statsbomb_xg, minute)

xGRBc <- xG %>%
  filter(match_id == 3817468) %>%
  filter(possession_team.name == "Charlotte") %>%
  select(shot.statsbomb_xg, minute)

# columbus crew
xGrbCC <- xG %>%
  filter(match_id == 3817375) %>%
  filter(possession_team.name == "New York Red Bulls") %>%
  select(shot.statsbomb_xg, minute)

xGRBcc <- xG %>%
  filter(match_id == 3817375) %>%
  filter(possession_team.name == "Columbus Crew") %>%
  select(shot.statsbomb_xg, minute)

# NER
xGrbNER <- xG %>%
  filter(match_id == 3817433) %>%
  filter(possession_team.name == "New York Red Bulls") %>%
  select(shot.statsbomb_xg, minute)

xGRBner <- xG %>%
  filter(match_id == 3817433) %>%
  filter(possession_team.name == "New England Revolution") %>%
  select(shot.statsbomb_xg, minute)

# NYC
xGrbNYC <- xG %>%
  filter(match_id == 3817391) %>%
  filter(possession_team.name == "New York Red Bulls") %>%
  select(shot.statsbomb_xg, minute)

xGRBnyc <- xG %>%
  filter(match_id == 3817391) %>%
  filter(possession_team.name == "New York City FC") %>%
  select(shot.statsbomb_xg, minute)

# PU
xGrbPU <- xG %>%
  filter(match_id == 3817604) %>%
  filter(possession_team.name == "New York Red Bulls") %>%
  select(shot.statsbomb_xg, minute)

xGRBpu <- xG %>%
  filter(match_id == 3817604) %>%
  filter(possession_team.name == "Philadelphia Union") %>%
  select(shot.statsbomb_xg, minute)

# Charlotte
plot(x = xGrbC$minute, y = xGrbC$shot.statsbomb_xg, frame = FALSE, type = "b", pch = 19, col = "red", xlab = "Minutes", ylab = "xG") + geom_line(aes(group = 2)) + geom_point() + theme_bw()

lines(x = xGRBc$minute, y = xGRBc$shot.statsbomb_xg, pch = 18, col = "blue", type = "b", lty = 2)
title("xG vs. Time NYRB v. Charlotte")
legend("topleft", legend=c("New York Red Bulls", "Charlotte"),
       col=c("red", "blue"), lty = 1:2, cex=.8)


# Columbus Crew
plot(x = xGrbCC$minute, y = xGrbCC$shot.statsbomb_xg, frame = FALSE, type = "b", pch = 19, col = "red", xlab = "Minutes", ylab = "xG") + geom_line(aes(group = 2)) + geom_point() + theme_bw()

lines(x = xGRBcc$minute, y = xGRBcc$shot.statsbomb_xg, pch = 18, col = "blue", type = "b", lty = 2)
title("xG vs. Time NYRB v. Columbus Crew")
legend("topleft", legend=c("New York Red Bulls", "Columbus Crew"),
       col=c("red", "blue"), lty = 1:2, cex=.8)

# NER
plot(x = xGrbNER$minute, y = xGrbNER$shot.statsbomb_xg, frame = FALSE, type = "b", pch = 19, col = "red", xlab = "Minutes", ylab = "xG") + geom_line(aes(group = 2)) + geom_point() + theme_bw()

lines(x = xGRBner$minute, y = xGRBner$shot.statsbomb_xg, pch = 18, col = "blue", type = "b", lty = 2)
title("xG vs. Time NYRB v. New England Revolution")
legend("topleft", legend=c("New York Red Bulls", "New England Revolution"),
       col=c("red", "blue"), lty = 1:2, cex=.8)

# NYC
plot(x = xGrbNYC$minute, y = xGrbNYC$shot.statsbomb_xg, frame = FALSE, type = "b", pch = 19, col = "red", xlab = "Minutes", ylab = "xG") + geom_line(aes(group = 2)) + geom_point() + theme_bw()

lines(x = xGRBnyc$minute, y = xGRBnyc$shot.statsbomb_xg, pch = 18, col = "blue", type = "b", lty = 2)
title("xG vs. Time NYRB v. New York City FC")
legend("topleft", legend=c("New York Red Bulls", "New York City FC"),
       col=c("red", "blue"), lty = 1:2, cex=.8)

# PU
plot(x = xGrbPU$minute, y = xGrbPU$shot.statsbomb_xg, frame = FALSE, type = "b", pch = 19, col = "red", xlab = "Minutes", ylab = "xG") + geom_line(aes(group = 2)) + geom_point() + theme_bw()

lines(x = xGRBpu$minute, y = xGRBpu$shot.statsbomb_xg, pch = 18, col = "blue", type = "b", lty = 2)
title("xG vs. Time NYRB v. Philadelphia Union")
legend("topleft", legend=c("New York Red Bulls", "Philadelphia Union"),
       col=c("red", "blue"), lty = 1:2, cex=.8)

unique(me$play_pattern.name)

setplays <- me[c("play_pattern.name", "match_id", "player.name", "team.name")]

# Corner kick
corner <- setplays %>%
  filter(play_pattern.name == "From Corner") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(corner$player.name)

# KO
KO <- setplays %>%
  filter(play_pattern.name == "From Kick Off") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(KO$player.name)

# GK
GK <- setplays %>%
  filter(play_pattern.name == "From Goal Kick") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(GK$player.name)

# Free Kicks
FK <- setplays %>%
  filter(play_pattern.name == "From Free Kick") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(FK$player.name)

# Counter
counter <- setplays %>%
  filter(play_pattern.name == "From Counter") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(counter$player.name)

# Throw In
TI <- setplays %>%
  filter(play_pattern.name == "From Throw In") %>%
  filter(team.name == "New York Red Bulls") %>%
  select(player.name)

table(TI$player.name)

# Data Science Question: Can we predict shot outcome based on shot and goalkeeper stats?
# subset dataset
df <- me[c("shot.statsbomb_xg", "shot.statsbomb_xg2", "shot.aerial_won", "shot.end_location.x", "shot.end_location.y", "shot.end_location.z", "shot.first_time", "goalkeeper.type.name", "shot.type.name", "shot.outcome.name", "location.x.GK", "location.y.GK", "DistToGoal", "DistToKeeper", "AngleToGoal", "AngleToKeeper", "AngleDeviation", "avevelocity", "DistSGK")]

# view data types
str(df)

# view number of distinct values
sapply(df, function(x) n_distinct(x))

# view number of missing values
(colMeans(is.na(df)))*100

# remove rows where the whole row is NA
df2 <- df[rowSums(is.na(df)) != ncol(df),]

# view number of missing values
(colMeans(is.na(df2)))*100

# remove rows where the whole row is NA
df2 <- df[rowSums(is.na(df)) != ncol(df),]

# view number of missing values
(colMeans(is.na(df2)))*100












