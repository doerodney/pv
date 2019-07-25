library(tidyverse)
library(lubridate) 

df <- read.csv("~/github/doerodney/pv/bloodyhell.csv")
glimpse(df)
df$timestamp = as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M")
df$date = as.Date(df$timestamp)
glimpse(df)

dfDaily <- df %>% 
          group_by(date) %>% 
          summarize(
            systolic = mean(systolic),
            diastolic = mean(diastolic),
            pulse = mean(pulse),
            dosemg = mean(dosemg)
          )
glimpse(dfDaily)

dfPhleb <- df %>% filter(phlebotomy > 0) %>% 
  group_by(date) %>%
  summarize( systolic = mean(systolic),
             diastolic = mean(diastolic),
             pulse = mean(pulse),
             dosemg = mean(dosemg))
glimpse(dfPhleb)

dfHematocrit <- df %>% filter(hematocrit > 0) %>% 
  group_by(date) %>%
  summarize(hematocrit = mean(hematocrit),
            systolic = mean(systolic),
            diastolic = mean(diastolic),
            pulse = mean(pulse),
            dosemg = mean(dosemg))
glimpse(dfHematocrit)

# Model the systolic pressure as a function of phlebotomy, hematocrit, and dosage.
model <- lm(systolic ~ phlebotomy + hematocrit + dosemg, data = dfDaily)
summary(model)

# Model hematocrit as a function of phlebotomy and dosage.
model <- lm(hematocrit ~ phlebotomy + dosemg, data = dfDaily)
summary(model)

# Plot strip charts of blood pressure values:
ggplot(dfDaily, aes(x=as.POSIXct(date, tz="GMT"), y=systolic, color=dosemg)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Daily mean systolic pressure by time and hydroxyurea dosage') +
  xlab('date time') +
  ylab('pressure [mm Hg]') 

# Plot strip charts of blood pressure values:
ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=systolic, color=dosemg)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Systolic pressure by time and hydroxyurea dosage') +
  xlab('date time') +
  ylab('pressure [mm Hg]') 

#------------------------------------------------------------------------------

# Plot strip charts of blood pressure values:
ggplot(dfPhleb, aes(x=as.POSIXct(date, tz="GMT"), y=systolic, color=dosemg)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Systolic pressure by time and hydroxyurea dosage pre-phlebotomy') +
  xlab('date time') +
  ylab('pressure [mm Hg]') 

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=diastolic, color=dosemg)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Diastolic pressure by time and hydroxyurea dosage') +
  xlab('date time') +
  ylab('pressure [mm Hg]')

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=pulse)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Heart Rate') +
  xlab('date time') +
  ylab('beats per minute')

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=hematocrit, color=dosemg)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Hematocrit') +
  xlab('date time') +
  ylab('percent red cell volume of total blood volume')

ggplot(dfHematocrit, aes(x=hematocrit, y=systolic)) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Systolic Pressure by Hematocrit') +
  xlab('hematocrit') +
  ylab('systolic [mm Hg]')

ggplot(df, aes(x=hematocrit, y=diastolic)) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Diastolic Pressure by Hematocrit') +
  xlab('hematocrit') +
  ylab('diastolic [mm Hg]')
