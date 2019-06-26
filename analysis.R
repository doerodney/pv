library(tidyverse)
library(lubridate) 

df <- read.csv("~/github/doerodney/pv/bloodyhell.csv")
glimpse(df)
df$timestamp = as.POSIXct(df$timestamp, format="%Y-%m-%d %H:%M")
glimpse(df)

# Plot strip charts of blood pressure values:
ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=systolic)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Systolic pressure') +
  xlab('date time') +
  ylab('pressure [mm Hg]') 

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=diastolic)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Diastolic pressure') +
  xlab('date time') +
  ylab('pressure [mm Hg]')

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=pulse)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Heart Rate') +
  xlab('date time') +
  ylab('beats per minute')

ggplot(df, aes(x=as.POSIXct(timestamp, tz="GMT"), y=hematocrit)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Hematocrit') +
  xlab('date time') +
  ylab('percent red cell volume of total blood volume')

ggplot(df, aes(x=hematocrit, y=systolic)) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Systolic Pressure by Hematocrit') +
  xlab('hematocrit') +
  ylab

ggplot(df, aes(x=hematocrit, y=diastolic)) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method="auto", se=TRUE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle('Diastolic Pressure by Hematocrit') +
  xlab('hematocrit') +
  ylab('diastolic [mm Hg]')
