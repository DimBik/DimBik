rm(list=ls())
library(xlsx)

#-----------------------------------------------Question 1-----------------------------------------------
data <- data.frame(read.xlsx("C:\\Users\\jimbi\\OneDrive\\Desktop\\Notes\\Intro to R for Data Science\\Assessment\\DATA8010_Assignment1_2021.xls",1))
str(data)

#-----------------------------------------------Question 2-----------------------------------------------
data$Date...Timestamp <- as.POSIXct(data$Date...Timestamp , tz="", format = "%d %b %Y %H:%M:%S")

#-----------------------------------------------Question 3-----------------------------------------------
library(tidyr)
#I used the for-loop to change the column names batch.1-8.pH to 1-8.
for (i in 2:9){  
  names(data)[i]=gsub("i",i-1,"i")
}
#Batch is the name of the column I passed the characters 1-8 in from the for-loop above
data=gather(data = data, Batch, pH, 2:9, na.rm=T) 

#-----------------------------------------------Question 4-----------------------------------------------
#I change the column Batch from string to integer
data$Batch=as.integer(data$Batch)
str(data)
#I delete the rows which have exact the same values with different value in column batch
data=data[c(-298,-404,-617),] 
View(data)
str(data)

#-----------------------------------------------Question 5-----------------------------------------------
#Function diff.Date gives the difference in secs in the data set. I divide it by 60 to get the time in minutes
data$diff.betw.2.obs=c(0,diff.Date(data$Date...Timestamp)/60)
#In dif vector we pass into two time values(The first observation that we want to start counting the time from and each other observation in the first column)
#I used the for-loop to take all the observations and I passed the difference into the vector matr. 
matr=NULL
for (i in 1:nrow(data)){
  dif=c(data$Date...Timestamp[1],data$Date...Timestamp[i])
  matr=c(matr,diff.Date(dif))
}
#The time is given in days so I multiply the days with 24 and 60 to get it in minutes
matr=matr*24*60
#I create a new column in my data set with the calculations I performed before 
data$dif.from.1st=matr

#-----------------------------------------------Question 6-----------------------------------------------
library(tidyverse)
library(gt)

#I create a table grouped by Batch
tabled=data %>%
  group_by(Batch) %>%
  summarise(Start_T_D = Date...Timestamp[1],
            MaxpH = max(pH), 
            MinpH = min(pH),
            MeanOfpH = mean(pH), 
            MedianOfpH = median(pH), 
            Std.DevOfpH = sd(pH), 
            Initial_pH = pH[1],
            Final_pH = pH[length(Batch)],
            ActiveDays = (dif.from.1st[length(Batch)]-dif.from.1st[1])/(24*60),
            NumberOfDatapoints = length(Batch),
            End_T_D = Date...Timestamp[length(Batch)],
           ) 
View(tabled)
tabled
#I put some colors in the table from red to green, red depicts the low values in each column and green the high values
#I also calculated the mean and standard deviation in each column
tabled %>% 
  gt() %>%
  tab_header(
    title = "Summary for each Batch") %>%
  fmt_number( columns = c(MaxpH,MinpH,MeanOfpH,MedianOfpH,Std.DevOfpH,Initial_pH,Final_pH,ActiveDays)) %>%
  fmt_integer(columns = NumberOfDatapoints) %>%
  data_color(columns = c(MaxpH,MinpH,MeanOfpH,MedianOfpH,Std.DevOfpH,Initial_pH,Final_pH,ActiveDays,NumberOfDatapoints), colors =c("red","green") ) %>%
  summary_rows(columns = c(MaxpH,MinpH,MeanOfpH,MedianOfpH,Std.DevOfpH,Initial_pH,Final_pH,ActiveDays,NumberOfDatapoints), fns = list(~mean(.),~sd(.)))
#The mean and the sd between 2 consecutive observations in minutes in each batch
mean(data$diff.betw.2.obs[data$Batch==1])    
sd(data$diff.betw.2.obs[data$Batch==1])

mean(data$diff.betw.2.obs[data$Batch==2][2:length(data$diff.betw.2.obs[data$Batch==2])])    
sd(data$diff.betw.2.obs[data$Batch==2][2:length(data$diff.betw.2.obs[data$Batch==2])])

mean(data$diff.betw.2.obs[data$Batch==3][2:length(data$diff.betw.2.obs[data$Batch==3])])    
sd(data$diff.betw.2.obs[data$Batch==3][2:length(data$diff.betw.2.obs[data$Batch==3])])

mean(data$diff.betw.2.obs[data$Batch==4][2:length(data$diff.betw.2.obs[data$Batch==4])])    
sd(data$diff.betw.2.obs[data$Batch==4][2:length(data$diff.betw.2.obs[data$Batch==4])])

mean(data$diff.betw.2.obs[data$Batch==5][2:length(data$diff.betw.2.obs[data$Batch==5])])    
sd(data$diff.betw.2.obs[data$Batch==5][2:length(data$diff.betw.2.obs[data$Batch==5])])

mean(data$diff.betw.2.obs[data$Batch==6][2:length(data$diff.betw.2.obs[data$Batch==6])])    
sd(data$diff.betw.2.obs[data$Batch==6][2:length(data$diff.betw.2.obs[data$Batch==6])])
#Examining if the pH follows the Normal Distribution
library(e1071)
hist(data$pH)
shapiro.test(data$pH)
skewness(data$pH)
kurtosis(data$pH)
#Function to calculate the standard error of skewness and kurtosis
se_of_Sk_ku=function(x){
  n=length(x)
  ses=sqrt(6*n*(n-1)/((n-2)*(n+1)*(n+3)))
  cat("standard error of the skewsness is=",ses,'\n')
  sek=2*ses*sqrt((n^2-1)/((n-3)*(n+5)))
  cat("standard error of the kurtosis is= ",sek)
  return(c(ses,sek))
}
rn=data$pH
se=se_of_Sk_ku(rn)
#Checking the normality
ifelse(abs(skewness(rn,na.rm=T))<2*se[1],yes="it appears symmetric",no="it isn't symmetric")
ifelse(abs(kurtosis(rn,na.rm=T))<2*se[2],yes="it doesnt deviate from normality",no="it deviates from normality")
t.test(data$pH)
plot(data$pH)

#-----------------------------------------------Question 7-----------------------------------------------

library(plotly)
#I plotted using the function ggplotly to get an interactive plot
#Time vs pH
plotingpH=ggplot(data = data) +
  geom_line(aes(x=Date...Timestamp,y=pH, color=as.factor(Batch)))

ggplotly(plotingpH)

#Time vs Conductivity
plotingCon=ggplot(data = data) +
  geom_point(aes(x=Date...Timestamp,y=Conductivity..mS.mL., color=as.factor(Batch)))

ggplotly(plotingCon)
hist(data$Conductivity..mS.mL.)
shapiro.test(data$Conductivity..mS.mL.)
t.test(data$Conductivity..mS.mL.)

#Time vs Pressure
plotingPress=ggplot(data = data) +
  geom_point(aes(x=Date...Timestamp,y=Pressure..bar., color=as.factor(Batch)))

ggplotly(plotingPress)
hist(data$Pressure..bar.)
shapiro.test(data$Pressure..bar.)
t.test(data$Pressure..bar.)

#Time vs Temperature
plotingTemp=ggplot(data = data) +
  geom_point(aes(x=Date...Timestamp,y=Temperature...C., color=as.factor(Batch)))

ggplotly(plotingTemp)
hist(data$Temperature...C.)
shapiro.test(data$Temperature...C.)

#Time vs Concentration
plotingConcent=ggplot(data = data) +
  geom_point(aes(x=Date...Timestamp,y=Concentration..mg.mL., color=as.factor(Batch)))

ggplotly(plotingConcent)
hist(data$Concentration..mg.mL.)
shapiro.test(data$Concentration..mg.mL.)

#-----------------------------------------------Question 8-----------------------------------------------
#I downloaded the packages below to use the animate function and anim_save function to display the animated time series and save it as a gif 
install.packages('gganimate')
library(gganimate)
install.packages("gifski")
library(gifski)
#I set my working directory to be able to save the gif file into this folder
setwd("C:\\Users\\jimbi\\OneDrive\\Desktop\\Notes\\Intro to R for Data Science\\Assessment")
#I created the time series graph (Time vs pH) coloring the points in different Batches 
anim_graph<-ggplot(data = data, aes(x=Date...Timestamp,y=pH, color= as.factor(Batch))) + 
  geom_line() + 
  transition_reveal(data$Date...Timestamp)
#I set my number of frames to 600 and fps to 20 to get an animated time series for 600/20=30 secs
anim_graph1=animate(anim_graph, fps = 20, nframes=600)
#I saved the aforementioned time series as a gif
anim_save("graph.gif", anim_graph1)

anim_graph1

#-----------------------------------------------Question 9-----------------------------------------------
#I created two functions to calculate the boarders for the mild outliers (top and bottom outliers)
#Using this values, I am going to create two ablines in each boxplot which will show which points are outliers
bottom_outliers=function(x){
  outliers=as.numeric(quantile(x,0.25))-1.5*IQR(x)
  return(outliers)
}

top_outliers=function(x){
  outliers=as.numeric(quantile(x,0.75))+1.5*IQR(x)
  return(outliers)
}
#Below, I created different boxplots for pH and Concentration in different values for each categorical variable, in these boxplots we also have a scatter plot for concentration vs pH and pH vs Concentration respectively.
#I created two data sets, the first column in each dataset will help us link the different intercepts which can be found in the second column( For example in the boxplot for pH in Batch 1 the intercept of the abline is in mat[1], in the boxplot for pH in Batch 2 the intercept is in mat[2] and so forth)
#The mat has the values of the intercepts for the bottom outliers and mat1 for the top outliers. In each abline the slope is equal to zero because it has to be parallel to X-axis
mat=NULL
for (i in 1:8){mat=c(mat,bottom_outliers(data$pH[data$Batch==i]))}
mat1=NULL
for (i in 1:8){mat1=c(mat1,top_outliers(data$pH[data$Batch==i]))}
outb1=data.frame(Batch=c(1,2,3,4,5,6,7,8),intercept1=mat)
outb2=data.frame(Batch=c(1,2,3,4,5,6,7,8),intercept2=mat1)
#I created the 8 different boxplots for each Batch and fit the ablines in each different boxplot 
b1=ggplot(data= data,aes(x=Concentration..mg.mL.,y=pH,fill=as.factor(Batch)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Batch)
b1 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )
#I did the same for the categorical variable Feed
outb1=data.frame(Feed=c(1,2),intercept1=c(bottom_outliers(data$pH[data$Feed==1]),bottom_outliers(data$pH[data$Feed==2])))
outb2=data.frame(Feed=c(1,2),intercept2=c(top_outliers(data$pH[data$Feed==1]),top_outliers(data$pH[data$Feed==2])))
#I created the boxplots and the ablines for the different values in column Feed
b2=ggplot(data= data,aes(x=Concentration..mg.mL.,y=pH,fill=as.factor(Feed)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Feed)
b2 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )
#I did the same for the Hose
outb1=data.frame(Hose=c("A","B","C","D"),intercept1=c(bottom_outliers(data$pH[data$Hose=="A"]),bottom_outliers(data$pH[data$Hose=="B"]),bottom_outliers(data$pH[data$Hose=="C"]),bottom_outliers(data$pH[data$Hose=="D"])))
outb2=data.frame(Hose=c("A","B","C","D"),intercept2=c(top_outliers(data$pH[data$Hose=="A"]),top_outliers(data$pH[data$Hose=="B"]),top_outliers(data$pH[data$Hose=="C"]),top_outliers(data$pH[data$Hose=="D"])))
#I created the aforementioned boxplots for the Hose
b3=ggplot(data= data,aes(x=Concentration..mg.mL.,y=pH,fill=as.factor(Hose)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Hose)
b3 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )
#In the aforementioned boxplots I had the boxplots for pH and the scatter plot for Concentration vs pH
#In the following lines I have the boxplots for Concentration and the scatter plot for pH vs Concentration
#Below is the code for the boxplot described in the line 173 in the different Batches
mat=NULL
for (i in 1:8){mat=c(mat,bottom_outliers(data$Concentration..mg.mL.[data$Batch==i]))}
mat1=NULL
for (i in 1:8){mat1=c(mat1,top_outliers(data$Concentration..mg.mL.[data$Batch==i]))}
outb1=data.frame(Batch=c(1,2,3,4,5,6,7,8),intercept1=mat)
outb2=data.frame(Batch=c(1,2,3,4,5,6,7,8),intercept2=mat1)
# Here I create the boxplots and fit the ablines
b4=ggplot(data= data,aes(x=pH,y=Concentration..mg.mL.,fill=as.factor(Batch)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Batch)
b4 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )
#I did the same for the different values in column Feed
outb1=data.frame(Feed=c(1,2),intercept1=c(bottom_outliers(data$Concentration..mg.mL.[data$Feed==1]),bottom_outliers(data$Concentration..mg.mL.[data$Feed==2])))
outb2=data.frame(Feed=c(1,2),intercept2=c(top_outliers(data$Concentration..mg.mL.[data$Feed==1]),top_outliers(data$Concentration..mg.mL.[data$Feed==2])))

b5=ggplot(data= data,aes(x=pH,y=Concentration..mg.mL.,fill=as.factor(Feed)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Feed)
b5 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )
#I did the same for the different values in column Hose
outb1=data.frame(Hose=c("A","B","C","D"),intercept1=c(bottom_outliers(data$Concentration..mg.mL.[data$Hose=="A"]),bottom_outliers(data$Concentration..mg.mL.[data$Hose=="B"]),bottom_outliers(data$Concentration..mg.mL.[data$Hose=="C"]),bottom_outliers(data$Concentration..mg.mL.[data$Hose=="D"])))
outb2=data.frame(Hose=c("A","B","C","D"),intercept2=c(top_outliers(data$Concentration..mg.mL.[data$Hose=="A"]),top_outliers(data$Concentration..mg.mL.[data$Hose=="B"]),top_outliers(data$Concentration..mg.mL.[data$Hose=="C"]),top_outliers(data$Concentration..mg.mL.[data$Hose=="D"])))

b6=ggplot(data= data,aes(x=pH,y=Concentration..mg.mL.,fill=as.factor(Hose)))+ 
  geom_boxplot()+
  geom_point()+
  facet_wrap(~Hose)
b6 + geom_hline(aes(yintercept =intercept2),outb2,color="red" ) + geom_hline(aes(yintercept =intercept1),outb1,col="green" )

#I split the initial dataset into 8 datasets for each Batch
dataB1= data %>% filter(Batch==1)
dataB2= data %>% filter(Batch==2)
dataB3= data %>% filter(Batch==3)
dataB4= data %>% filter(Batch==4)
dataB5= data %>% filter(Batch==5)
dataB6= data %>% filter(Batch==6)
dataB7= data %>% filter(Batch==7)
dataB8= data %>% filter(Batch==8)
summary(dataB1$pH)
summary(dataB2$pH)
summary(dataB3$pH)
summary(dataB4$pH)
summary(dataB5$pH)
summary(dataB6$pH)
summary(dataB7$pH)
summary(dataB8$pH)
#Shapiro test to check if in each batch the concentration follows the normal distribution
shapiro.test(dataB1$Concentration..mg.mL.)
shapiro.test(dataB2$Concentration..mg.mL.)
shapiro.test(dataB3$Concentration..mg.mL.)
shapiro.test(dataB4$Concentration..mg.mL.)
shapiro.test(dataB5$Concentration..mg.mL.)
shapiro.test(dataB6$Concentration..mg.mL.)

#-----------------------------------------------Question 10-----------------------------------------------
#I downloaded and set the library GGally to have access to the function ggpairs which will help us create the correlation coefficient matrix
install.packages("GGally")
library(GGally)
#The correlation coefficient matrix for the dataset
ggpairs(data[c(2:5,9)])
cor.test(data$Concentration..mg.mL.,data$Conductivity..mS.mL.)
cor.test(data$Concentration..mg.mL.,data$pH)

#I created 8 different correlation coefficient matrices for each dataset
ggpairs(dataB1[c(2:5,9)])
ggpairs(dataB2[c(2:5,9)])
ggpairs(dataB3[c(2:5,9)])
ggpairs(dataB4[c(2:5,9)])
ggpairs(dataB5[c(2:5,9)])
ggpairs(dataB6[c(2:5,9)])
ggpairs(dataB7[c(2:5,9)])
ggpairs(dataB8[c(2:5,9)])
#I created a scatter plot and I applied smoothers to each scatter plot in the initial dataset(Only for the Continuous Variables) 
qplot(x=Temperature...C.,y=pH, data = data, geom = c("point","smooth"))
qplot(x=Temperature...C.,y=Conductivity..mS.mL., data = data, geom = c("point","smooth"))
qplot(x=Temperature...C.,y=Pressure..bar., data = data, geom = c("point","smooth"))
qplot(x=Temperature...C.,y=Concentration..mg.mL., data = data, geom = c("point","smooth"))
qplot(x=Conductivity..mS.mL.,y=pH, data = data, geom = c("point","smooth"))
qplot(x=Conductivity..mS.mL.,y=Pressure..bar., data = data, geom = c("point","smooth"))
qplot(x=Conductivity..mS.mL.,y=Concentration..mg.mL., data = data, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = data, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Pressure..bar., data = data, geom = c("point","smooth"))
qplot(x=Pressure..bar.,y=pH, data = data, geom = c("point","smooth"))
#The following lines are the code for the scatter plots with smoothers only for the Variables that seem to have some "strong" correlation
#I created the scatter plots with smoothers in each dataset which are split in batches
qplot(x=Concentration..mg.mL.,y=pH, data = dataB1, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB1, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB2, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB2, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB3, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB3, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB4, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB4, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB5, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB5, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB6, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB6, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB7, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB7, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=pH, data = dataB8, geom = c("point","smooth"))
qplot(x=Concentration..mg.mL.,y=Conductivity..mS.mL., data = dataB8, geom = c("point","smooth"))

#-----------------------------------------------Question 11-----------------------------------------------
#I used the function lm(y~x) to find the slope and the intercept for the linear regression model 
#I used the abline to display the line in the scatter plot which is created by the function plot(x,y)
#I only used a scatter plot and fit the abline in the columns which have some strong correlation.
#I did the same for the different datasets split in Batches
plot(data$Conductivity..mS.mL.,data$Concentration..mg.mL.)
k=lm(data$Concentration..mg.mL.~data$Conductivity..mS.mL.)
abline(k)

plot(data$Concentration..mg.mL.,data$pH)
k=lm(data$pH~data$Concentration..mg.mL.)
abline(k)

plot(dataB1$Conductivity..mS.mL.,dataB1$Concentration..mg.mL.)
k=lm(dataB1$Concentration..mg.mL.~dataB1$Conductivity..mS.mL.)
abline(k)

plot(dataB1$Concentration..mg.mL.,dataB1$pH)
k=lm(dataB1$pH~dataB1$Concentration..mg.mL.)
abline(k)

plot(dataB2$Conductivity..mS.mL.,dataB2$Concentration..mg.mL.)
k=lm(dataB2$Concentration..mg.mL.~dataB2$Conductivity..mS.mL.)
abline(k)

plot(dataB2$Concentration..mg.mL.,dataB2$pH)
k=lm(dataB2$pH~dataB2$Concentration..mg.mL.)
abline(k)

plot(dataB3$Conductivity..mS.mL.,dataB3$Concentration..mg.mL.)
k=lm(dataB3$Concentration..mg.mL.~dataB3$Conductivity..mS.mL.)
abline(k)

plot(dataB3$Concentration..mg.mL.,dataB3$pH)
k=lm(dataB3$pH~dataB3$Concentration..mg.mL.)
abline(k)

plot(dataB4$Conductivity..mS.mL.,dataB4$Concentration..mg.mL.)
k=lm(dataB4$Concentration..mg.mL.~dataB4$Conductivity..mS.mL.)
abline(k)

plot(dataB4$Concentration..mg.mL.,dataB4$pH)
k=lm(dataB4$pH~dataB4$Concentration..mg.mL.)
abline(k)

plot(dataB5$Conductivity..mS.mL.,dataB5$Concentration..mg.mL.)
k=lm(dataB5$Concentration..mg.mL.~dataB5$Conductivity..mS.mL.)
abline(k)

plot(dataB5$Concentration..mg.mL.,dataB5$pH)
k=lm(dataB5$pH~dataB5$Concentration..mg.mL.)
abline(k)

plot(dataB6$Conductivity..mS.mL.,dataB6$Concentration..mg.mL.)
k=lm(dataB6$Concentration..mg.mL.~dataB6$Conductivity..mS.mL.)
abline(k)

plot(dataB6$Concentration..mg.mL.,dataB6$pH)
k=lm(dataB6$pH~dataB6$Concentration..mg.mL.)
abline(k)

plot(dataB7$Conductivity..mS.mL.,dataB7$Concentration..mg.mL.)
k=lm(dataB7$Concentration..mg.mL.~dataB7$Conductivity..mS.mL.)
abline(k)

plot(dataB7$Concentration..mg.mL.,dataB7$pH)
k=lm(dataB7$pH~dataB7$Concentration..mg.mL.)
abline(k)

plot(dataB8$Conductivity..mS.mL.,dataB8$Concentration..mg.mL.)
k=lm(dataB8$Concentration..mg.mL.~dataB8$Conductivity..mS.mL.)
abline(k)

plot(dataB8$Concentration..mg.mL.,dataB8$pH)
k=lm(dataB8$pH~dataB8$Concentration..mg.mL.)
abline(k)
k

a=lm(formula =  data$Concentration..mg.mL. ~ data$pH + data$Conductivity..mS.mL. + data$Pressure..bar.+data$Temperature...C.)
summary(a)
b=lm(formula =  data$Concentration..mg.mL. ~ data$pH + data$Conductivity..mS.mL.)
summary(b)
#For the 3D scatter plot and multivariable linear regression
install.packages("scatterplot3d")
library(scatterplot3d)
sca=scatterplot3d(data$pH,data$Conductivity..mS.mL.,data$Concentration..mg.mL.,color = 'blue')
sca$plane3d(b,col='red')
#End of code
