#*************
#Title: NBA 2018 Teams 
#Description: a short description of what the script is about
#input(s): nba2018.csv
#output(s):nba2018-teams.csv
#*************
library(readr)
library(dplyr)
nba2018<-read_csv("~/Desktop/hw-stat133/workout1/data/nba2018.csv")
nba2018$experience[nba2018$experience=="R"]<-0
nba2018$experience<-as.integer(nba2018$experience)
salary<-nba2018$salary/1000000
position<-factor(nba2018$position,levels=c("C","PF","PG","SF","SG"),labels = c("center","power_fwd","point_guard","small_fwd","shoot_guard"))
missed_fg<-nba2018$field_goals_atts - nba2018$field_goals
missed_ft<-nba2018$points1_atts-nba2018$points1
rebounds<-nba2018$off_rebounds + nba2018$def_rebounds
efficiency<- (nba2018$points+rebounds+nba2018$assists+nba2018$steals+nba2018$blocks-missed_fg-missed_ft-nba2018$turnovers)/nba2018$games
summary_efficiency<-summary(efficiency)
sink("../output/eficiency-summary.txt")
teams<-summarise(group_by(nba2018,team), experience=round(sum(experience)),salary=round(sum(salary)),points3=sum(points3),points2=sum(points2),points1=sum(points1),points=sum(points),off_rebounds=sum(off_rebounds),def_rebounds=sum(def_rebounds),assists=sum(assists),steals=sum(steals),blocks=sum(blocks),turnovers=sum(turnovers),fouls=sum(fouls),efficiency=sum(efficiency))
summary_teams<-summary(teams)
sink("../data/teams-summary.txt")



write.csv(summary_teams, "../data/nba2018-teams.csv")
library(ggplot2)
library(readr)
library(dplyr)
dat <- read_csv("../data/nba2018-teams.csv")

salary<-sum(as.double(salary))
salary_teams <- dat %>%
  group_by(team) %>% summarize(salaries)

ggplot(salary_teams, aes(x = reorder(team, +salaries), y = salaries)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  labs(y = "Salary(in millions)", x = "Team", title = "NBA Teams ranked by Total Salary") + 
  geom_hline(aes(yintercept = mean(salaries)), colour = "red", size = 1.5)

points_<-sum(teams$points)
points_teams <- dat %>%
  group_by(team) %>% 
  summarise(points_)
ggplot(points_teams, aes(x = reorder(team, +points_), y = points_)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  labs(y = "Points", x = "Team", title = "NBA Teams ranked by Total Points") + 
  geom_hline(aes(yintercept = mean(points)), colour = "red", size = 1.5)



eff <- sum(efficiency)
eff_teams <- dat %>%
  group_by(team) %>% 
  summarise(eff)

ggplot(eff_teams, aes(x = reorder(team, +eff), y = eff)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  labs(y = "Efficiency", x = "Team", title = "NBA Teams ranked by Total Efficiency") + 
  geom_hline(aes(yintercept = mean(eff)), colour = "red", size = 1.5)



#rank teams by total rebounds
reb = sum(teams$off_rebounds + teams$def_rebounds)
r_teams <- dat %>%
  group_by(team) %>% 
  summarise(reb)
ggplot(r_teams, aes(x = reorder(team, reb), y = reb)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() +
  labs(y = "Rebounds", x = "Team", title = "NBA Teams ranked by Total Rebounds") + 
  geom_hline(aes(yintercept = mean(reb)), colour = "red", size = 1.5)

nba2018_teams = read.csv("../data/nba2018-teams.csv")
teams
sal_teams <- nba2018_teams %>% group_by(nba2018_teams$X....team) %>% summarise(salaries=sum(nba2018$salary))
ggplot(sal_teams,aes(x=reorder(teams, salaries)))+geom_bar(stat='identity')+coord_flip()+labs(y="Salary(in millions)",x="Team",title="NBA Teams ranked by Total Salary")+geom_hline(aes(yintercept = mean(salaries)),colour="red",size=1.5)



###Comments
#Very difficult, spend over 7 hours doing it. 




