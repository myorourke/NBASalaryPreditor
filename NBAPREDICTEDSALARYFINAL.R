#Read the Data
data <- read.csv('NBA1920dataPER.csv', fileEncoding="UTF-8-BOM")

#Modeling
linearmod <- glm(formula = X2019.20.Salary ~ PER + DEFRTG, family = gaussian, data=data) #Generalized Linear Models (Linear regression)
#Formula - y variables vs. x variables, Family - normal/gaussian, Data
summary(linearmod)
######

##making a correlation heatmap
mydata <- data[, c(11,12,13,14,15,16,17,18,19,20,21,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67)]

head(mydata)

cormat <- round(cor(mydata),2)
head(cormat)

#Fixing NA's
cor(mydata, use = "complete.obs")

#correlation with P values

#install.packages("Hmisc")
library('Hmisc')

rcorr(x, type = c("pearson","spearman"))

library("Hmisc")
res2 <- rcorr(as.matrix(mydata))
res2

# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

#create profiles of players 
#Kmeans, group players
#unsupervised learning
#go deeper in each group, why are they grouped players

datacluster <- mydata[1:305, 1:53]
#Picking all players and columns all columns except name, team, and salary beyond 19-20 season
#to answer so our system does not crash

#cluster the data into positions
#1(PG),2(SG),3(SF),4(PF), 5(C)
#K-Means

#checking for NA's
is.na(datacluster)
is.finite('datacluster')
str(datacluster)

cluster <- kmeans(datacluster, centers = 5)
#it has done the clustering
#I now need to add the cluster information into the data

datacluster$position <- as.numeric(cluster$cluster)

#Made position numeric -- cluster cant have factor data
datacluster$position = as.numeric(datacluster$position)

#change position from factor to num?
str(datacluster)

as.numeric(datacluster$position)

#heatmap
#Corrplot
#install.packages('corrr')
library(corrr)

datacluster[,-1] = datacluster[,-1] * 0.99 #--> https://github.com/tidymodels/corrr/issues/86
#Provides Gaussian noise 

network_plot(correlate(datacluster), min_cor=0.4, colours = c("indianred2", "pink", "skyblue1", "blue"))
#If it does not run at first, please run line 72-77 again, then try again please! if not, maybe
#Try from the begginning again. If values do not show up, zoom in on the plot

#Visualize
#install.packages('ggplot2')
library('ggplot2')

ggplot(datacluster, aes(x= REB., y= AST, color = position)) +geom_point()
#This chart shows how the players are clustered up among the rebounding and ast categories. TO change the variables, change X and Y
#We chose REB and AST because they are arguably the most important stats aside from Points, and we assumed Big men would tend to 
#cluster more favorably towards rebounds, and smaller players will rack up more assists


#PREDICTING NBA SALARY
salary.table <- read.csv('NBA1920dataPER.csv', fileEncoding="UTF-8-BOM")

ss <- read.csv('NBA1920dataPER.csv', fileEncoding="UTF-8-BOM")

#Checking structure of data
str(salary.table)
str(ss)

#selecting Points, Min, TO, Reb, PER, Stl, Aast
salary.table <- salary.table[, c(1,2,3,4,7,23,24,35,36,37,38,39,68,69,77)]
ss <- ss[, c(1,2,3,4,7,23,24,35,36,37,38,39,68,69,77)]

#installing packages
#install.packages('data.table')
#install.packages('corrplot')
#install.packages('GGally')
#install.packages('tidyverse')
#install.packages('PerformanceAnalytics')
#install.packages('plotly')
library (data.table)
library(corrplot)
library(GGally)
library(tidyverse)
library(PerformanceAnalytics)
library(plotly)

#computing per/game stats of main stats
stats20 <- 
  ss %>% filter(Season >= 2020) %>% 
  select(PLAYER:Season) %>% 
  distinct(PLAYER, .keep_all = TRUE) %>% 
  mutate(MPG = MIN, PPG = PTS, APG = AST, 
         RPG = REB, TOPG = TOV, BPG = BLK, 
         SPG = STL)

#Merge datasets
stats_salary <- merge(stats20, salary.table, by.x = 'PLAYER', by.y = 'PLAYER')
names(stats_salary)[36]<-'X2019.20.Salary.x'
stats_salary <- stats_salary[-35]

#Check correlation between salary and player's performance
corrplot(cor(stats_salary %>% 
               select(X2019.20.Salary.x, MPG:SPG, 
                      AGE.x, PER.x, contains("%")), 
             use = "complete.obs"), 
         method = "circle",type = "upper")

str(stats_salary)
# As it can be viewed in the following chart that Salary 19_20 
# shows strong correlation with PPG(Point Per Game) and MPG(Minute Per Game)
# Besides that, the correlation between Salary and TOPG and PER is also high

stats_salary_cor <- 
  stats_salary %>% 
  select(X2019.20.Salary.x, PPG, MPG, TOPG, RPG, PER.x, SPG, APG)
ggpairs(stats_salary_cor)
# Below in the chart also shows the higher correlation between Salary and PPG,TOPG,MPG,PER.x
#Turnovers are POSITIVELY correlated. Probably because the more a players possesses the ball,
#The more turnovers the players produce.Despite being a negative stat, it correlates to the players
#aggressiveness and possession time, which are considered positive stats


cor(stats_salary_cor)[,"X2019.20.Salary.x"]
# Below shows the rank of the coefficient of each factors

#Salary x PPG
names(stats_salary)[23] <- "TEAM"
plot_ly(data = stats_salary, x = ~X2019.20.Salary.x, y = ~PPG, color = ~TEAM,
        hoverinfo = "text",
        text = ~paste("Player: ", PLAYER,
                      "<br>Salary: ", format(X2019.20.Salary.x, big.mark = ","),"$",
                      "<br>PPG: ", round(PPG, digits = 3),
                      "<br>Team: ", TEAM.x)) %>% 
  layout(
    title = "Salary vs Point Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Point per Game")
  )

#Salary X MPG
names(stats_salary)[23] <- "TEAM"
plot_ly(data = stats_salary, x = ~X2019.20.Salary.x, y = ~MPG, color = ~TEAM,
        hoverinfo = "text",
        text = ~paste("PLAYER: ", PLAYER,
                      "<br>Salary: ", format(X2019.20.Salary.x, big.mark = ","),"$",
                      "<br>MPG: ", round(MPG, digits = 3),
                      "<br>Team: ", TEAM)) %>% 
  layout(
    title = "Salary vs Minute Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Minute per Game")
  )
#Salary x TO
names(stats_salary)[23] <- "TEAM"
plot_ly(data = stats_salary, x = ~X2019.20.Salary.x, y = ~TOPG, color = ~TEAM,
        hoverinfo = "text",
        text = ~paste("Player: ", PLAYER,
                      "<br>Salary: ", format(X2019.20.Salary.x, big.mark = ","),"$",
                      "<br>TOPG: ", round(TOPG, digits = 3),
                      "<br>Team: ", TEAM)) %>% 
  layout(
    title = "Salary vs Turnover Per Game",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Turnover Per Game")
  )

#Linear Regression Model

stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = PPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = MPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = APG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = RPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = TOPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = BPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = SPG)) + 
  geom_point() + 
  geom_smooth(method = "lm")
stats_salary %>% 
  ggplot(aes(x = X2019.20.Salary.x, y = PER.x)) + 
  geom_point() + 
  geom_smooth(method = "lm")

stats_salary_regression <- 
  stats_salary %>% select(X2019.20.Salary.x, MPG:SPG, PER.x)
lm(X2019.20.Salary.x~., data=stats_salary_regression)

#Salary prediction
salary_prediction <- function(m, point, minutes, turn_over, blocks, assists, rebounds, steals, per){
  pre_new <- predict(m, data.frame(PPG = point, MPG = minutes, TOPG = turn_over, BPG = blocks, APG = assists, RPG = rebounds, SPG = steals, PER.x = per))
  msg <- paste("PPG:", point, ",MPG:", minutes, ",TOPG:", turn_over, "BPG:", blocks, "APG:", assists, "RPG:", rebounds, "SPG:", steals, "PER.x:", per, " ==> Expected Salary: $", format(round(pre_new), big.mark = ","), sep = "")
  print(msg)
}

#Testing prediction
#(input PPG + MPG + TOPG + BPG + APG + RPG + SPG + PER.x)
model <- lm(formula = X2019.20.Salary.x ~ PPG + MPG + TOPG + BPG + APG + RPG + SPG + PER.x, data = stats_salary_regression)
# Prediction on Salary of Anthony Davis
#Davis's Stats (PPG=26.1,MPG=34.4, TOPG=2.5)
salary_prediction(model,26.1,34.4,2.5,2.3,3.2,9.3,1.5,27.50)

# Prediction on Salary of Rui Hachimura
salary_prediction(model,13.5,30.1,1.1,0.2,1.8,6.1,0.8,13.56)
#END
#Showing you how to add the prediction column to the data
y_predict = predict(linearmod)

data$PredictedSal <- y_predict

write.csv(data, "NBA1920data.csv")

