# ipl_data_analysis_project

rm(list = ls())
install.packages("tidyverse")
library(tidyverse) #helps wrangle data
install.packages("dplyr")
library(dplyr)

library(lubridate) #helps wrangle date attributes
install.packages("skimr")
library(skimr)
library(ggplot2)
install.packages("treemap")
library(treemap)
library(tidyr)
library(readr)

#STEP 1: COLLECT DATA
deliveries <- read.csv("deliveries.csv", header = TRUE)
matches <- read.csv("matches.csv", header = TRUE)

#STEP 2: WRANGLE DATA  
matches <- matches %>% 
  mutate(city = replace(city, city == ""  ,"dubai"))

matches <- matches %>% 
  mutate(winner = replace(winner, winner == ""  ,"No Result"))

#Matches played in different cities
ggplot(matches,aes(city,fill= city)) +geom_bar()+ ylab("Number of Matches Played") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ guides(fill=FALSE)

# IS WINNING TOSS IN IPL HAS ADVANTAGE 
matches$toss_match<- ifelse(as.character(matches$toss_winner)==as.character(matches$winner),"Won","Lost") # does toss winner won the match 
ggplot(matches,aes(toss_match, fill = toss_match))+ 
  geom_bar()+ xlab("Toss") +ylab("Number of matches won/lost after winning toss")+ ggtitle("How much of a advantage is winning the toss")

#NUMBER OF MATCHES PLAYED BY EACH TEAM
ggplot(as.data.frame(table(matches$team2) + table(matches$team1)),aes(reorder(Var1,-Freq),Freq,fill = Var1)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_bar(stat = "identity") + guides(fill=FALSE)

#NUMBER OF MATCHES WON BY EACH TEAMS
ggplot(matches,aes(winner),fill= winner) +geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Matches won")





Data<-matches[matches$season!="2009",]
Data$date<- as.Date(Data$date)
Data1<-Data[Data$date < as.Date("2014-04-16") | Data$date > as.Date("2014-04-30"),]
Data1$home_team[Data1$city=="Bangalore"]<- "Royal Challengers Bangalore"
Data1$home_team[Data1$city=="Chennai"]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Delhi"]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Chandigarh"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Jaipur"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Mumbai"]<- "Mumbai Indians"
Data1$home_team[Data1$city=="Kolkata"]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Kochi"]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season <=2012]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Hyderabad" & Data1$season >2012]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ahmedabad"]<- "Rajasthan Royals"
Data1$home_team[Data1$city=="Dharamsala"]<- "Kings XI Punjab"
Data1$home_team[Data1$city=="Visakhapatnam" & Data1$season== 2015]<- "Sunrisers Hyderabad"
Data1$home_team[Data1$city=="Ranchi" & Data1$season== 2013]<- "Kolkata Knight Riders"
Data1$home_team[Data1$city=="Ranchi" & Data1$season > 2013]<- "Chennai Super Kings"
Data1$home_team[Data1$city=="Rajkot" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Kanpur" ]<- "Gujarat Lions"
Data1$home_team[Data1$city=="Raipur" ]<- "Delhi Daredevils"
Data1$home_team[Data1$city=="Nagpur" ]<- "Deccan Chargers"
Data1$home_team[Data1$city=="Indore" ]<- "Kochi Tuskers Kerala"
Data1$home_team[Data1$city=="Pune" & Data1$season!= 2016]<- "Pune Warriors"
Data1$home_team[Data1$city=="Pune" & Data1$season== 2016]<- "Rising Pune Supergiants"
Data1<-Data1[ which(!is.na(Data1$home_team)),]
Data1$win_host <- ifelse(as.character(Data1$winner)==as.character(Data1$home_team),"Home","Away")

ggplot(Data1[which(!is.na(Data1$win_host)),],aes(win_host,fill= win_host))+geom_bar()+
  ggtitle("Is home advantage a real thing in IPL?")+
  xlab("Team")+
  ylab("Number of Matches won")+labs(aesthetic="Winner")

#WINNING PERCENTAGE OF EACH TEAM

matches_won<-as.data.frame(table(matches$winner))
colnames(matches_won)[1]<-"teams"
colnames(matches_won)[2]<-"Won"
matches_played<-as.data.frame(table(matches$team2) + table(matches$team1))
colnames(matches_played)[1]<-"teams"
colnames(matches_played)[2]<-"Played"

ggplot(left_join(matches_played,matches_won, by = "teams" ),aes(reorder(teams,-Won/Played),Won*100/Played,fill = teams)) +geom_bar(stat = "identity")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ xlab("Team")+
  ylab("Win Percentage") +  guides(fill=FALSE)+coord_cartesian(ylim = c(0, 100))

#TOP scoring BATSMAN IN IPL
highest_scoring_batsman<- deliveries %>% 
  group_by(batsman)%>%
  summarise(runs=sum(batsman_runs)) %>% 
  arrange(desc(runs))  %>%
  filter(runs > 3000) 

#batsman with highest strike rate 
strike_rate <- deliveries %>% group_by(batsman) %>% filter(length(total_runs)>500) %>% 
  summarise(strike_rate= mean(batsman_runs)*100) %>%  arrange(desc(strike_rate))  %>%
  filter(strike_rate > 100) 
