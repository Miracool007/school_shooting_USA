#loading the libraries I'll be working with
library(tidyverse)
library(janitor)

#importing the dataset
school_shooting <- read.csv("school shootings USA.csv")

#cleaning the columns in the dataset
school_shooting <- rename_with(school_shooting, tolower)

#dropping null values
school_shooting <- school_shooting%>%
  distinct()%>%
  drop_na()

#formatting the date
school_shooting <- school_shooting%>%
  mutate(date = as.Date(incident.date, format = "%m/%d/%Y"))

#crating tables to show different matrices 
state_schooting_killed <- school_shooting%>%
  group_by(state)%>%
  summarise(students_killed = sum(victims.killed))

state_shooting_arrest <- school_shooting%>%
  group_by(state)%>%
  summarise(arrest_made = sum(suspects.arrested))

state_shooting_suspect_killed <- school_shooting%>%
  group_by(state)%>%
  summarise(suspect_killed = sum(suspects.killed))

state_shooting_suspect <- school_shooting%>%
  group_by(state)%>%
  summarise(suspects_involved = sum(suspects.killed,suspects.injured,
                                    suspects.arrested))

most_tragic_day <- school_shooting%>%
  group_by(incident.date)%>%
  summarise(number_of_victims = sum(victims.injured,victims.killed))

#formatting the date to show months
school_shooting$month <- format(as.Date(school_shooting$incident.date), "%m")

school_shooting<- school_shooting%>%
  mutate(months = case_when(month == "01" ~ "January",
                            month == "02" ~ "February",
                            month == "03" ~ "March",
                            month == "04" ~ "April",
                            month == "05" ~ "May",
                            month == "06" ~ "June",
                            month == "07" ~ "July",
                            month == "08" ~ "August",
                            month == "09" ~ "September",
                            month == "10" ~ "October",
                            month == "11" ~ "November",
                            month == "12" ~ "December"))


#grouping the months by seasons
school_shooting<- school_shooting%>%
  mutate(season = case_when(month == "01" ~ "Winter",
                            month == "02" ~ "Winter",
                            month == "03" ~ "Spring",
                            month == "04" ~ "Spring",
                            month == "05" ~ "Spring",
                            month == "06" ~ "Summer",
                            month == "07" ~ "Summer",
                            month == "08" ~ "Summer",
                            month == "09" ~ "Fall",
                            month == "10" ~ "Fall",
                            month == "11" ~ "Fall",
                            month == "12" ~ "Winter"))

#creating a table to show total shootings per season
most_tragic_season <- school_shooting%>%
  group_by(season)%>%
  summarise(number_of_victims = sum(victims.injured, victims.killed))

#creating a visual to show total shootings per season
ggplot(data = most_tragic_season)+
  geom_col(mapping = aes(x = season, y=number_of_victims), fill = "green")+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(title = "Number of Shootings in Each Season")

#creating a visual to show total shooting per month
ggplot(data = most_tragic_month)+
  geom_point(mapping = aes(x = months, y= number_of_victims,
                           color = number_of_victims))+
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank())+
  labs(title = "Number of victims per Month")

#creating a visual to show total suspects per state
ggplot(data = state_shooting_suspect)+
  geom_col(mapping = aes(x = state, y=suspects_involved), fill = "blue")+
  theme(axis.text.x = element_text(angle = 90, vjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())+
labs(title = "Number of Suspects per State")

#creating a visual to show total students killed per state
ggplot(data = state_schooting_killed)+
  geom_col(mapping = aes(x= state, y= students_killed), fill = "orange")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 1),
        panel.grid = element_blank())+
  labs(title = "Number of Students Killed in each State")

#exporting datasets for further visualisation with Power BI
write.csv(most_tragic_month, "tragic month.csv")
write.csv(school_shooting, "school_shooting.csv")
write.csv(state_shooting_arrest, "shooting aresst.csv")
