rm(list = ls())
library(xlsx)
data=read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Notes/Intro to R for Data Science/Assessment 2/STAT8010_2021_assignment2.xlsx',sheetIndex = 1,stringsAsFactors=T )
baseline=read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Notes/Intro to R for Data Science/Assessment 2/STAT8010_2021_assignment2.xlsx',sheetIndex = 2)
names(data)[4]="dailyO2"
View(data)
summary(data)
library(shiny)
library(tidyverse)
#---------------------------Question 1---------------------------
#We place the select bars on the side panel
ui=fluidPage(sidebarLayout(sidebarPanel(selectInput(inputId = 'var', #We ask for the user to choose on of the variables
                                                    'Select a variable',
                                                    choices = names(data)),
                                        selectInput(inputId = 'var2', #We ask for the user to choose the second variable as before
                                                    'Select a variable',
                                                    choices = names(data))),
                           #We create a tab called Plot and we plot the scatter plot, barplot or boxplot in the main panel tab Plot which is in the main panel
                           mainPanel(tabsetPanel(tabPanel("Plot",fluidRow(plotOutput(outputId = 'plot') #We plot the graphs in first tab
                           )
                           )
,
tabPanel("Summary",verbatimTextOutput(outputId = 'summ')),#We create second tab for the summary
tabPanel("Histogram A",#We create the third tab for the histogram of the first variable called var. When it is continuous we prot using textOutput
         plotOutput(outputId = 'hist1'),
         textOutput(outputId = 'text1')),
tabPanel("Histogram B",#We create the fourth tab for the histgogram of the second variable called var2 
         plotOutput(outputId = 'hist2'),
         textOutput(outputId = 'text2'))))))

server=function(input,output){
  x=reactive({ifelse(input$var=='vessel' | input$var=='feed' | input$var=='base' | input$var=='hoses',yes = 1,no = 0)})#For var we pass in x variable 1 or 0, it depends of the variable(continuous or categorical)
  y=reactive({ifelse(input$var2=='vessel' | input$var2=='feed' | input$var2=='base' | input$var2=='hoses',yes = 1,no = 0)})#The same as before for the var2
  k=reactive(paste('as.factor(',input$var2,')',sep = ''))#We create a string as.factor(input$var2) because in line 40 the fill should be string as we have put it in the aes_string
  output$plot=renderPlot({if( x()==0 & y()==0 ) {plot(as.formula(paste(input$var,"~",input$var2)),data = data) #We plot the scatter boxplot or barplot 
  }  else{ if (x()==1 & y()==0) {boxplot(as.formula(paste(input$var2,"~",input$var)),data = data) 
  }        else{ if (x()==0 & y()==1) {boxplot(as.formula(paste(input$var,"~",input$var2)),data = data)
  }else{ggplot(data = data)+
      geom_bar(mapping = aes_string(x=input$var,fill=k()),position = 'dodge')}}}})
  output$summ=renderPrint(summary(data))#We print the summary
  output$hist1=renderPlot({if (x()==0){hist(data[,input$var],xlab=input$var,main=paste('Histogram of',input$var))}})#We print the Histogram
  output$text1=renderText({if (x()==1){print("The histogram is not available as the variable is not continuous ")}})#We print the message when the variable is not continuous
  output$hist2=renderPlot({if (y()==0){hist(data[,input$var2],xlab=input$var2,main=paste('Histogram of',input$var2))}})#We print the Histogram
  output$text2=renderText(if (y()==1){print("The histogram is not available as the variable is not continuous ")})#We print the message when the variable is not continuous
  }

shinyApp(ui = ui,server = server)

#---------------------------Question 2---------------------------
ui1=fluidPage(sidebarLayout(sidebarPanel(selectInput(inputId = 'var',
                                                    'Select a variable',
                                                    choices = names(data)),
                                        selectInput(inputId = 'var2',
                                                    'Select a variable',
                                                    choices = names(data))),
                           mainPanel(tabsetPanel(tabPanel("Plot",splitLayout(verticalLayout(plotOutput(outputId = 'plot'),textOutput(outputId = 'appl'),verbatimTextOutput('R2')),verticalLayout(
                             hr(),#one line gap
                             wellPanel(checkboxInput(inputId = 'bool','Linear Regrassion Model')),#We create a checkbox in well panel in tab 1
                             hr(),#one line gap
                             wellPanel(selectInput(inputId = 'var3','Choose Categorical Variable',choices=names(data)[7:10])))#We create select button with the categorical variables as a choice
                           ))
                           ,
                           tabPanel("Summary",verbatimTextOutput(outputId = 'summ')),
                           tabPanel("Histogram A",
                                    plotOutput(outputId = 'hist1'),
                                    textOutput(outputId = 'text1')),
                           tabPanel("Histogram B",
                                    plotOutput(outputId = 'hist2'),
                                    textOutput(outputId = 'text2'))))))

server1=function(input,output){
  fctr=reactive(paste('as.factor(',input$var3,')',sep = ''))#to color the ggplot,like before aes_string recognizes only string arguments 
  o=reactive({lm(as.formula(paste(input$var2,'~',input$var)),data = data)})#We create this to check if its slope is negative by running o()$coefficients
  x=reactive({ifelse(input$var=='vessel' | input$var=='feed' | input$var=='base' | input$var=='hoses',yes = 1,no = 0)})
  y=reactive({ifelse(input$var2=='vessel' | input$var2=='feed' | input$var2=='base' | input$var2=='hoses',yes = 1,no = 0)})
  k=reactive(paste('as.factor(',input$var2,')',sep = ''))
  output$plot=renderPlot({if( x()==0 & y()==0 ) {ggplot(data)+geom_point(mapping=aes_string(x=input$var,input$var2,colour=fctr()))+if (input$bool){if (is.na(o()$coefficients[2])==T){geom_abline(slope = 1,intercept = 0)} 
 else{ geom_abline(slope=o()$coefficients[2],intercept = o()$coefficients[1])}}#Create the Scatter plot and fit the abline if the checkbox is ticked
                         }else{ if (x()==1 & y()==0) {boxplot(as.formula(paste(input$var2,"~",input$var)),data = data)#Boxplot
                               }else{ if (x()==0 & y()==1) {boxplot(as.formula(paste(input$var,"~",input$var2)),data = data)#Boxplot
                                     }else{ggplot(data = data)+
                                           geom_bar(mapping = aes_string(x=input$var,fill=k()),position = 'dodge')}}}})#barplot
  output$appl=renderText({if(input$bool & (x()!=0 | y()!=0)){print("The regression line is not applicable")}})#when we dont have scatter plot we return that the regression is not applicable
  #In lines 85-86 I print the coefficient of determination and the linear model
  output$R2=renderPrint({if(input$bool & x()==0 & y()==0 & input$var!=input$var2){cat(c("R^2=",round(cor(data[,input$var],data[,input$var2])^2,2),'\n',paste('y=',round(lm(as.formula(paste(input$var2,'~',input$var)),data = data)$coefficients[1],2),'+(',round(lm(as.formula(paste(input$var2,'~',input$var)),data = data)$coefficients[2],2),")x",sep = '')),sep = '')}
                         else{if(input$bool & x()==0 & y()==0 & input$var==input$var2){cat(c("R^2=",1,'\n','y=x'),sep='')}}})
  output$summ=renderPrint(summary(data))
  output$hist1=renderPlot({if (x()==0){hist(data[,input$var],xlab=input$var,main=paste('Histogram of',input$var))}})
  output$text1=renderText({if (x()==1){print("The histogram is not available as the variable is not continuous ")}})
  output$hist2=renderPlot({if (y()==0){hist(data[,input$var2],xlab=input$var2,main=paste('Histogram of',input$var2))}})
  output$text2=renderText(if (y()==1){print("The histogram is not available as the variable is not continuous ")})
}
#The lines that I have not commented are the same as the Question 1
shinyApp(ui = ui1,server = server1)
#---------------------------Question 3---------------------------

rm(list = ls())
rad=10 #The radious in mm
accur=8 #because 10-2=8 inaccuracy limits in mm

set.seed(1) #We set a set.seed to take the same results every time we execute the code
x=seq(-10,10,0.2)
plot(x=x,y=-sqrt(rad^2-x^2),xlim=c(-12,12),ylim=c(-12,12),type = 'l',col='red') #We plot the circle with radius 10 mm
lines(x=x,y=sqrt(rad^2-x^2),col='red')
x1=seq(-8,8,0.2)
lines(x=x1,y=sqrt(accur^2-x1^2),col='blue') #We plot the circle with radius 8 mm
lines(x=x1,y=-sqrt(accur^2-x1^2),col='blue')
missal=NULL
for (i in 1:1000){
      x.axis=sample(c(0,0.2),size = 500,replace=T,prob = c(19/20,1/20))#x.axis~Binomial(500,19/20)
      y.axis=runif(min = -0.05,max = 0.08,n=500)#y.axis[i]~U(-0.05,0.08)
      missal=c(missal,sqrt(sum(x.axis)^2+sum(y.axis)^2))
      points(sum(x.axis),sum(y.axis)) #We plot the points to see which of them are outside of the circles
}
hist(missal) #Histogram for the values of the missal
abline(v=c(10,8),col=c('red','blue')) #lines show the limits of fail and inaccuracy
shapiro.test(missal)
logic=NULL
logic1=NULL
for (i in 1:1000){
     logic[i]=ifelse(missal[i]<=10,'yes','no')
     logic1[i]=ifelse(missal[i]<=8,'yes','no')
}
table(logic)/1000 #Probability to go outside of the fail limits,87.2%
table(logic1)/1000 #Probability that the machine fails or gives inaccurate results,12.8%

set.seed(1)

missal=NULL
out10=NULL
out8=NULL
for (i in 1:1000){
  x.axis=0 
  y.axis=0
  index=500
  index1=500
  l=0
  la=0
  for (j in 1:500){
     xal=sample(c(0,0.2),size = 1,prob = c(19/20,1/20)) #it generates a random number {0, 0.2}
     yal=runif(min = -0.05,max = 0.08,n=1) #It gives a random number between -0.05 and 0.08 using uniform distribution
     x.axis=x.axis+xal
     y.axis=y.axis+yal
     if (sqrt(x.axis^2+y.axis^2)>8 & la==0){index1=j #It returns the first number that went outside of the circle that its boarders shows the inaccuracy
                                            la=1}
     if (sqrt(x.axis^2+y.axis^2)>10 & l==0){index=j #It returns the first number that went outside of the circle that shows the limits for failure
                                            l=1}}
  out10=c(out10,index) #We pass the numbers we found before
  missal=c(missal,sqrt(x.axis^2+y.axis^2))
  out8=c(out8,index1) 
}
out10
missal
out8
hist(out10) #Histogram shows the distribution for random variable that describes the number of tests conducted before the first failure 
hist(out8)#Histogram shows the distribution for random variable that describes the number of tests conducted before the first inaccuracy
abline(v=c(mean(out8),median(out8)),col=c('blue','red'))
summary(out8)
shapiro.test(out8) #Check for normality

#---------------------------Question 4---------------------------

strategy=function(n){ #It return the profit or cost for n tests
  x=0
  y=0
  o=0
  inac=0
  for (i in 1:n){
    x=x+sample(c(0,0.2),size = 1,prob = c(19/20,1/20))
    y=y+runif(min = -0.05,max = 0.08,n = 1)
    if (sqrt(x^2+y^2)>10){
      k=(n-i-1)*800+100000
      o=1
      break
    }else if(sqrt(x^2+y^2)>8){
     inac=inac+500
    }
  }
  if (o==1){
    return(250*(i-1)-k-inac)
  }else if(o==0){return(250*n-inac)}
}

profit=function(n,ma){ #It returns the plot with the average cost for different choices of test. n is the number of tests, ma is the number of days we perform n tests
  prof=NULL
  for (i in 1:n){
    plt=NULL
    for (j in 1:ma){
      plt=c(plt,strategy(i))# The profit-cost if we conduct n tests for ma different days
    }
    prof=c(prof,mean(plt)) #The average of profit-cost for the ma different days
  }
  plot(x=1:n,y=prof) #We plot the average of profit for different choice of tests
  abline(h=max(prof),col='blue') #line with the maximum
  abline(v=which(max(prof)==prof),col='green') #line with the index of maximum( How many test we perform to get the maximum)
  print(which(max(prof)==prof))#It returns the index of the maximum(the best strategy to optimize profit)
  print(max(prof))
}

set.seed(1)
profit(500,50) #It takes some minutes to be executed (2-3 minutes)

set.seed(1)
avg=0
for (I in 1:1000){
  avg=avg+strategy(448)
}
avg/1000 #Expected profit= 99736.9
#---------------------------End of Code--------------------------
