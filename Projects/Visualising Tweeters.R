rm(list=ls()) #We clean the environment
#Useful Libraries
library(ggplot2) #We load the libraries
library(xlsx)
library(openxlsx)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(maps)
#Preparing the data set
data=read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/Twitter+data+in+sheets.xlsx',1) #We pass into R the datasets
data1=read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/Twitter+data+in+sheets.xlsx',2) #In this dataset we have the locations for each tweet. data and data1 are linked each other with the variable LocationID
data2=read.xlsx('C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/Twitter+data+in+sheets.xlsx',3) #In this dataset we have the gender for each observation. data and data2 are linked each other with the variable UserID
data=data[-100001,]#We delete the last row because it has null values
#data3=read.csv('C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/countryContinent.csv')
k=world.cities #We pass the dataset world.cities which includes longitude and latitude for different cities around the world
#Maybe because of lack of RAM after 57000 observations the piece of code is not working properly, that's the reason I deleted 50000 observation
dataB=data[seq(50001,nrow(data)),] #We split the initial dataset into 2 datasets (with 50000 observations each) to make it lighter for R. If we don't split it, it will run out of RAM
data=data[-1*seq(50001,nrow(data)),]

#The following loop needs 5-10 minutes to run
for (i in seq(1,nrow(data))){ #we run a for loop to merge the datasets data,data1,data2
  data$country[i]=data1$Country[which(data$LocationID[i]==data1$LocationID)]#We pass 4 variables from dataset data1 into data
  data$state[i]=data1$State[which(data$LocationID[i]==data1$LocationID)]#and we merge them using the LocationID variable which is the one that the two datasets have in common
  data$statecode[i]=data1$StateCode[which(data$LocationID[i]==data1$LocationID)]
  data$city[i]=data1$City[which(data$LocationID[i]==data1$LocationID)]
  data$gender[i]=data2$Gender[which(data$UserID[i]==data2$UserID)]#We pass only one variable from data2 which shows the gender of each observation, userID is the variable that we joint the two datasets with.
}
data=data[,c(-13,-17)]#We delete irrelevant variables
data$country[data$country=='United States']='USA'#We change the name of United States and Kingdom to be able to link 
data$country[data$country=='United Kingdom']='UK'#it with the dataset k in which the two countries are writen USA and UK respectively
data$country[data$country=='Republic of the Congo']='Congo'

#Same here, the computational cost is high as we have 50000 observations. Approx 5-10 minutes
for (i in seq(1,nrow(dataB))){ #We run a for loop as before for the second dataset dataB following the same concept as before
  if (length(which(dataB$LocationID[i]==data1$LocationID)!=0)){#We use length. In which() the result will return either the index or integer(0), By using length(which()) the two possible variables will be 1 or 0. 
    dataB$country[i]=data1$Country[which(dataB$LocationID[i]==data1$LocationID)]#the for loop stops if we don't include else{} through which I pass zero into each variable
    dataB$state[i]=data1$State[which(dataB$LocationID[i]==data1$LocationID)]# when the length(which()) is zero 
    dataB$statecode[i]=data1$StateCode[which(dataB$LocationID[i]==data1$LocationID)]
    dataB$city[i]=data1$City[which(dataB$LocationID[i]==data1$LocationID)]
  }else{
    dataB$country[i]=0#We pass zero when the logical expresion inside if is not TRUE
    dataB$state[i]=0
    dataB$statecode[i]=0
    dataB$city[i]=0
  }
  if(length(which(dataB$UserID[i]==data2$UserID))!=0){#Same logic as in the first if-statement
    dataB$gender[i]=data2$Gender[which(dataB$UserID[i]==data2$UserID)]
  }else{
    dataB$gender[i]=0
  }
}
View(dataB)
dataB=dataB[,c(-13,-17)]#We delete the irrelevant variables
dataB$country[dataB$country=='United States']='USA'#Change the names as we did in dataset data
dataB$country[dataB$country=='United Kingdom']='UK'
#2
tail(data)#We change some names in order to be easier to link the datasets (k with data and k with dataB)
data$city[data$statecode=='US-DC']="Washington"
k$country.etc[k$country.etc=='Korea South']='South Korea'
data$city[data$city=='New York City']='New York'
data$city[data$city=="So Paulo"]='Sao Paulo'
data$city[data$city=="stanbul"]='Istanbul'
data$city[data$city=="zmir"]='Izmir'
k$name[k$name=="Roxas" & k$pop>100000]="Roxas City"
k$name[k$name=="Ni Dilli"]="New Delhi"
k$name[k$name=="Milan" & k$pop>1000000]="Milano"
k$name[k$name=='Navi Mumbai']='Mumbai'
k$country.etc[k$name=='Toronto']='USA'

tail(dataB)
dataB$city[dataB$statecode=='US-DC']="Washington"
dataB$city[dataB$city=='New York City']='New York'
dataB$city[dataB$city=="So Paulo"]='Sao Paulo'
dataB$city[dataB$city=="stanbul"]='Istanbul'
dataB$city[dataB$city=="zmir"]='Izmir'

#High computational cost again 2-3 minutes
for(i in seq(1,nrow(data))){#We merge the datasets k and data by linking the cities and countries.
  a=which((data$city[i]==k$name) & (data$country[i]==k$country.etc))#(Countries are needed because some cities in different countries have the same name)
  if (length(a)!=0){#Using this for-loop we get the longitude and latitude for each observation.
    data$long[i]=k$long[a]#The if statement has the same logic as before, the difference is that instead of passing zero(when the if statement is not true)
    data$lat[i]=k$lat[a]#we pass NA. Zero would be a bad idea as it would plot it on the map with longitude=0 and latitude=0
    data$pop[i]=k$pop[a]
  }else{
    data$long[i]=NA
    data$lat[i]=NA
    data$pop[i]=NA
  }
}

for(i in seq(1,nrow(dataB))){#Same as before for the dataset dataB
  a=which((dataB$city[i]==k$name) & (dataB$country[i]==k$country.etc))
  if (length(a)!=0){
    dataB$long[i]=k$long[a]
    dataB$lat[i]=k$lat[a]
    dataB$pop[i]=k$pop[a]
  }else{
    dataB$long[i]=NA
    dataB$lat[i]=NA
    dataB$pop[i]=NA
  }
}
summary(data)
summary(dataB)
#Longitude and Latitude manually from Google maps from 7000 missing values in variables long and lat in each dataset to 5000 with the following piece of code 
#I chose the cities with the most tweets in
data$long[data$city=="Seattle"]=-122.30
data$lat[data$city=="Seattle"]=47.55
data$long[data$city=="Oldsmar"]=-82.67
data$lat[data$city=="Oldsmar"]=28.05
data$long[data$city=="Ahmedabad"]=72.58
data$lat[data$city=="Ahmedabad"]=23.4
data$long[data$city=="Bogot"]=-74.12
data$lat[data$city=="Bogot"]=4.66
data$long[data$city=="Brooklyn"]=-73.86
data$lat[data$city=="Brooklyn"]=40.68
data$long[data$city=="Boise"]=-116.23
data$lat[data$city=="Boise"]=43.60
data$long[data$city=="Brixton"]=-0.11
data$lat[data$city=="Brixton"]=51.46
data$long[data$city=="Camberley"]=-0.74
data$lat[data$city=="Camberley"]=51.34
data$long[data$city=="Cebu City"]=123.88
data$lat[data$city=="Cebu City"]=10.33
data$long[data$city=="Deeside"]=-3.03
data$lat[data$city=="Deeside"]=53.20
data$long[data$city=="England"]=-91.97
data$lat[data$city=="England"]=34.55
data$long[data$city=="Espaa"]=118.75
data$lat[data$city=="Espaa"]=9.77
data$long[data$city=="Gotham"]=-1.21
data$lat[data$city=="Gotham"]=52.87
data$long[data$city=="Hannover"]=9.75
data$lat[data$city=="Hannover"]=52.37
data$long[data$city=="Herndon"]=-77.38
data$lat[data$city=="Herndon"]=38.97
data$long[data$city=="Kolkata"]=88.37
data$lat[data$city=="Kolkata"]=22.65
data$long[data$city=="Leavenworth"]=-120.66
data$lat[data$city=="Leavenworth"]=47.60
data$long[data$city=="London" & data$state=='California']=-119.44
data$lat[data$city=="London" & data$state=='California']=36.48
data$long[data$city=="Los Gatos"]=-121.97
data$lat[data$city=="Los Gatos"]=37.24
data$long[data$city=="Menlo Park"]=-122.15
data$lat[data$city=="Menlo Park"]=37.48
data$long[data$city=="Oak Grove"]=-87.41
data$lat[data$city=="Oak Grove"]=36.67
data$long[data$city=="Orroli"]=9.25
data$lat[data$city=="Orroli"]=39.70
data$long[data$city=="Ottawa" & data$state=='Ohio']=-84.04
data$lat[data$city=="Ottawa" & data$state=='Ohio']=41.02
data$long[data$city=="Piscataway"]=-74.48
data$lat[data$city=="Piscataway"]=40.54
data$long[data$city=="Regensburg"]=12.10
data$lat[data$city=="Regensburg"]=49.01
data$long[data$city=="Sammamish"]=-122.04
data$lat[data$city=="Sammamish"]=47.62
data$long[data$city=="Scotland"]=-77.59
data$lat[data$city=="Scotland"]=39.97
data$long[data$city=="Shinjuku"]=139.71
data$lat[data$city=="Shinjuku"]=35.70
data$long[data$city=="St. Louis"]=-90.23
data$lat[data$city=="St. Louis"]=38.64
data$long[data$city=="Surrey"]=-122.80
data$lat[data$city=="Surrey"]=49.16
data$long[data$city=="Weil am Rhein"]=7.62
data$lat[data$city=="Weil am Rhein"]=47.59
data$long[data$city=="Woodcliff Lake"]=-74.06
data$lat[data$city=="Woodcliff Lake"]=41.02
data$long[data$city=="Woodside"]=-122.25
data$lat[data$city=="Woodside"]=37.43
#We fix the dataset by changing the variables to factor, numeric and integer
data$Weekday=as.factor(data$Weekday)
data$Hour=as.integer(data$Hour)
data$Day=as.integer(data$Day)
data$Lang=as.factor(data$Lang)
data$IsReshare=as.factor(data$IsReshare)
data$Reach=as.integer(data$Reach)
data$RetweetCount=as.integer(data$RetweetCount)
data$Likes=as.integer(data$Likes)
data$Klout=as.integer(data$Klout)
data$Sentiment=as.numeric(data$Sentiment)
data$country=as.factor(data$country)
data$state=as.factor(data$state)
data$city=as.factor(data$city)
data$gender=as.factor(data$gender)
data$UserID=as.factor(data$UserID)
str(data)
summary(data)


# the same for dataB
dataB$long[dataB$city=="Seattle"]=-122.30
dataB$lat[dataB$city=="Seattle"]=47.55
dataB$long[dataB$city=="Oldsmar"]=-82.67
dataB$lat[dataB$city=="Oldsmar"]=28.05
dataB$long[dataB$city=="Ahmedabad"]=72.58
dataB$lat[dataB$city=="Ahmedabad"]=23.4
dataB$long[dataB$city=="Bogot"]=-74.12
dataB$lat[dataB$city=="Bogot"]=4.66
dataB$long[dataB$city=="Brooklyn"]=-73.86
dataB$lat[dataB$city=="Brooklyn"]=40.68
dataB$long[dataB$city=="Boise"]=-116.23
dataB$lat[dataB$city=="Boise"]=43.60
dataB$long[dataB$city=="Brixton"]=-0.11
dataB$lat[dataB$city=="Brixton"]=51.46
dataB$long[dataB$city=="Camberley"]=-0.74
dataB$lat[dataB$city=="Camberley"]=51.34
dataB$long[dataB$city=="Cebu City"]=123.88
dataB$lat[dataB$city=="Cebu City"]=10.33
dataB$long[dataB$city=="Deeside"]=-3.03
dataB$lat[dataB$city=="Deeside"]=53.20
dataB$long[dataB$city=="England"]=-91.97
dataB$lat[dataB$city=="England"]=34.55
dataB$long[dataB$city=="Espaa"]=118.75
dataB$lat[dataB$city=="Espaa"]=9.77
dataB$long[dataB$city=="Gotham"]=-1.21
dataB$lat[dataB$city=="Gotham"]=52.87
dataB$long[dataB$city=="Hannover"]=9.75
dataB$lat[dataB$city=="Hannover"]=52.37
dataB$long[dataB$city=="Herndon"]=-77.38
dataB$lat[dataB$city=="Herndon"]=38.97
dataB$long[dataB$city=="Kolkata"]=88.37
dataB$lat[dataB$city=="Kolkata"]=22.65
dataB$long[dataB$city=="Leavenworth"]=-120.66
dataB$lat[dataB$city=="Leavenworth"]=47.60
dataB$long[dataB$city=="London" & dataB$state=='California']=-119.44
dataB$lat[dataB$city=="London" & dataB$state=='California']=36.48
dataB$long[dataB$city=="Los Gatos"]=-121.97
dataB$lat[dataB$city=="Los Gatos"]=37.24
dataB$long[dataB$city=="Menlo Park"]=-122.15
dataB$lat[dataB$city=="Menlo Park"]=37.48
dataB$long[dataB$city=="Oak Grove"]=-87.41
dataB$lat[dataB$city=="Oak Grove"]=36.67
dataB$long[dataB$city=="Orroli"]=9.25
dataB$lat[dataB$city=="Orroli"]=39.70
dataB$long[dataB$city=="Ottawa" & dataB$state=='Ohio']=-84.04
dataB$lat[dataB$city=="Ottawa" & dataB$state=='Ohio']=41.02
dataB$long[dataB$city=="Piscataway"]=-74.48
dataB$lat[dataB$city=="Piscataway"]=40.54
dataB$long[dataB$city=="Regensburg"]=12.10
dataB$lat[dataB$city=="Regensburg"]=49.01
dataB$long[dataB$city=="Sammamish"]=-122.04
dataB$lat[dataB$city=="Sammamish"]=47.62
dataB$long[dataB$city=="Scotland"]=-77.59
dataB$lat[dataB$city=="Scotland"]=39.97
dataB$long[dataB$city=="Shinjuku"]=139.71
dataB$lat[dataB$city=="Shinjuku"]=35.70
dataB$long[dataB$city=="St. Louis"]=-90.23
dataB$lat[dataB$city=="St. Louis"]=38.64
dataB$long[dataB$city=="Surrey"]=-122.80
dataB$lat[dataB$city=="Surrey"]=49.16
dataB$long[dataB$city=="Weil am Rhein"]=7.62
dataB$lat[dataB$city=="Weil am Rhein"]=47.59
dataB$long[dataB$city=="Woodcliff Lake"]=-74.06
dataB$lat[dataB$city=="Woodcliff Lake"]=41.02
dataB$long[dataB$city=="Woodside"]=-122.25
dataB$lat[dataB$city=="Woodside"]=37.43

dataB$Weekday=as.factor(dataB$Weekday)
dataB$Hour=as.integer(dataB$Hour)
dataB$Day=as.integer(dataB$Day)
dataB$Lang=as.factor(dataB$Lang)
dataB$IsReshare=as.factor(dataB$IsReshare)
dataB$Reach=as.integer(dataB$Reach)
dataB$RetweetCount=as.integer(dataB$RetweetCount)
dataB$Likes=as.integer(dataB$Likes)
dataB$Klout=as.integer(dataB$Klout)
dataB$Sentiment=as.numeric(dataB$Sentiment)
dataB$country=as.factor(dataB$country)
dataB$state=as.factor(dataB$state)
dataB$city=as.factor(dataB$city)
dataB$gender=as.factor(dataB$gender)
dataB$UserID=as.factor(dataB$UserID)
str(dataB)
summary(dataB)

rm(list=c('k','data1','data2')) # We delete the datasets that we are not gonna use anymore
#We create 3 new datasets for data and 3 for dataB
set=data %>% group_by(country) %>% count()#It counts the number of tweets for each country. set and set1 will help us to create the polygon map
#Max,min,median,mean for sentiment and mean for Reach. The aforementioned are the variables in the set1 grouping by country
set1=data %>% group_by(country) %>% summarise(mean=mean(Sentiment),max=max(Sentiment),min=min(Sentiment),median=median(Sentiment),meanR=mean(Reach))
set2=data %>% group_by(country,gender) %>% count()#group by country and gender, we get the number of tweets for each gender for each country. It will help us
#to create a barchart inside the polygons.
View(set2)
#Same for dataB
setB=dataB %>% group_by(country) %>% count()
set1B=dataB %>% group_by(country) %>% summarise(mean=mean(Sentiment),max=max(Sentiment),min=min(Sentiment),median=median(Sentiment),meanR=mean(Reach))
set2B=dataB %>% group_by(country,gender) %>% count()
View(set1)
View(set)
str(set2)

View(set1B)
View(setB)
library(spData)
#the data world from spData library has the polygons for each country worldwide. with the following piece of code we pass the variables from set1 
polyg=world #to polyg dataset which is equal to world dataset. Doing this we will be able to create the polygon map and color it.
polyg$name_long[polyg$name_long=="United States"]="USA"
polyg$name_long[polyg$name_long=="United Kingdom"]="UK"
polyg$name_long[polyg$name_long=='Republic of Korea']="South Korea"
polyg$name_long[polyg$name_long=='Dem. Rep. Korea']="North Korea"
polyg$name_long[polyg$name_long=="Côte d'Ivoire"]="Ivory Coast"
polygB$name_long[polygB$name_long=="Republic of the Congo"]="Congo"
polyg$numTwe=NA
polyg$max=NA
polyg$min=NA
polyg$mean=NA
polyg$median=NA
polyg$meanR=NA
for ( i in seq(1,nrow(polyg))){#Same logic in the for-loop to merge the dataset.
  if (length(which(set$country==polyg$name_long[i]))!=0){  
    polyg$numTwe[i]=set$n[which(set$country==polyg$name_long[i])]
    polyg$max[i]=set1$max[which(set1$country==polyg$name_long[i])]
    polyg$min[i]=set1$min[which(set1$country==polyg$name_long[i])]
    polyg$mean[i]=set1$mean[which(set1$country==polyg$name_long[i])]
    polyg$median[i]=set1$median[which(set1$country==polyg$name_long[i])]
    polyg$meanR[i]=set1$meanR[which(set1$country==polyg$name_long[i])]
  }
}
View(polyg)

polygB=world#Do the same for set1B
polygB$name_long[polygB$name_long=="United States"]="USA"
polygB$name_long[polygB$name_long=="United Kingdom"]="UK"
polygB$name_long[polygB$name_long=='Republic of Korea']="South Korea"
polygB$name_long[polygB$name_long=='Dem. Rep. Korea']="North Korea"
polygB$name_long[polygB$name_long=="Côte d'Ivoire"]="Ivory Coast"
polygB$name_long[polygB$name_long=="Republic of the Congo"]="Congo"
polygB$numTwe=NA
polygB$max=NA
polygB$min=NA
polygB$mean=NA
polygB$median=NA
polygB$meanR=NA
for ( i in seq(1,nrow(polygB))){
  if (length(which(setB$country==polygB$name_long[i]))!=0){  
    polygB$numTwe[i]=setB$n[which(setB$country==polygB$name_long[i])]
    polygB$max[i]=set1B$max[which(set1B$country==polygB$name_long[i])]
    polygB$min[i]=set1B$min[which(set1B$country==polygB$name_long[i])]
    polygB$mean[i]=set1B$mean[which(set1B$country==polygB$name_long[i])]
    polygB$median[i]=set1B$median[which(set1B$country==polygB$name_long[i])]
    polygB$meanR[i]=set1B$meanR[which(set1B$country==polygB$name_long[i])]
  }
}
View(polygB)
#We create two maps. The first one with points and the second one with polygons. The map with the points will be created using the dataset data and the map with polygons using the polyg dataset
colcont=colorNumeric(palette = "Blues",domain =log(c(1:26426)))#We create a continuous palette for polyg, 1 is the minimum value and 26426 is the maximum value for number of tweets variable in polyg dataset, we use logarithm to bring them closer
#These palettes will colorize our polygons on the map 
colcontB=colorNumeric(palette = "Blues",domain =log(c(min(polygB$numTwe,na.rm=T):max(polygB$numTwe,na.rm=T))))#We create a continuous palette for the second dataset polygB

#We create a palette for gender. The palette will colorize our points on the map with points
coloring=colorFactor(palette = c("Red","Blue","Black","Green"),levels = c("Female","Male","Unisex","Unknown"))
#We create a function for the popup. On the popup will be able to see the minimum maximum and so forth for some variables.
labelmap=function(x,y,z,w,e){paste0("Sentiment","<br/>","Minimum:",x,"<br/>","Maximum:",y,"<br/>","Mean:",z,"<br/>","Median:",w,"<br/>",'Mean Reach:',e )}
map=leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addPolygons(data = polyg,
              weight = 1,#boarder = 1 pixel
              fillOpacity = 0.75,
              color= ~colcont(log(numTwe)),#we color by the log(number of tweets per country)
              label = paste('Number of Tweets',polyg$numTwe),#We see the exact number of tweets when we hover above each polygon
              popup = ~labelmap(min,max,mean,median,meanR),#popup using the function we created above
              highlight=highlightOptions(weight = 3,color = "green")) %>%#when we hover above polygons the boarders will be 3 pixels and green color
  addLegend(pal = colcont,#we add a legend for the colors
            values = log(polyg$numTwe),
            title = "#twts = e^lgnd")#as we used logarithm the values will be displayed on the legend will be different with the real values. Saying e^lgnd we show to the user that exp(values on the legend) will return the real value

mapB=leaflet() %>%#Same for polygB 
  addProviderTiles("CartoDB") %>% 
  addPolygons(data = polygB,
              weight = 1,
              fillOpacity = 0.75,
              color= ~colcontB(log(numTwe)),
              label = paste('Number of Tweets',polygB$numTwe),
              popup = ~labelmap(min,max,mean,median,meanR),
              highlight=highlightOptions(weight = 3,color = "green")) %>%
  addLegend(pal = colcontB,
            values = log(polygB$numTwe),
            title = "#twts = e^lgnd")

#In this section we will create the map with the points. Since longitude and latitude for each city were passed manually all the observations in a specific city 
#will have the same longitude and latitude. To overcome this issue we add a random value from -0.01 to 0.01 to the longitude and latitude.
#We have a lot of tweets from the same people. Observations-tweets shared from the same person should have the exact same longitude and latitude. That's the reason of using Unique(UserID)
for (i in unique(data$UserID)){#2-3mins
  while (TRUE){
    run=runif(1,-0.01,0.01)
    run1=runif(1,-0.01,0.01)
    if (run!=0 & run1!=0){#There is a small chance of getting zero when we run runif(). If we get zero, we will have the same location for different users 
      break#We include a while loop which breaks when both runif values are not zero.
    }
  }
  data$long[data$UserID==i]=data$long[data$UserID==i]+run #We add the values to longitude and latitude
  data$lat[data$UserID==i]=data$lat[data$UserID==i]+run1
}
#The very same code for the second dataset dataB
for (i in unique(dataB$UserID)){#2-3 mins
  while (TRUE){
    run=runif(1,-0.01,0.01)
    run1=runif(1,-0.01,0.01)
    if (run!=0 & run1!=0){
      break
    }
  }
  dataB$long[dataB$UserID==i]=dataB$long[dataB$UserID==i]+run #To reduce the probability getting the same number I call runif one more time
  dataB$lat[dataB$UserID==i]=dataB$lat[dataB$UserID==i]+run1
}

library(htmltools)
popup=function(x,y) {paste0("Language: ",x,"<br/>","Number of Retweets: ",y)}#Function for the popup and the label. popup will show the language and number of re-tweets
label= function(x) {paste("Klout: ",x)}#label will show the Klout
male=data %>% filter(gender=="Male")#We create four datasets(male,female,unisex,unknown) from the dataset called data
female=data %>% filter(gender=="Female")#We do it in order to create a layer control which will allow us to choose the gender we want to plot the points for
unisex=data %>% filter(gender=="Unisex")
unknown=data %>% filter(gender=="Unknown")
#We create the map with the points
map1=leaflet() %>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(data=male,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,#radius of the points(1.4 pixels)
                   clusterOptions = markerClusterOptions(),#we create clusters, this is necessary, otherwise it will crash as the observations are many
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Male" #This is linked with the checkbox on the map
                   ) %>%
  addCircleMarkers(data=female,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Female" 
                   ) %>%
  addCircleMarkers(data=unisex,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Unisex" 
                   ) %>%
  addCircleMarkers(data=unknown,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Unknown" 
                   ) %>%
  addLegend(pal = coloring,#we add legends and the user will be able to see the color that represents each gender
            values = c("Female","Male","Unisex","Unknown")) %>%
  addLayersControl(overlayGroups = c("Male","Female",'Unisex','Unknown')) #We create the checkbox. Inside the vector is the exact name of the group in the addCircles above
  
#The very same for the dataB
maleB=dataB %>% filter(gender=="Male")
femaleB=dataB %>% filter(gender=="Female")
unisexB=dataB %>% filter(gender=="Unisex")
unknownB=dataB %>% filter(gender=="Unknown")
map1B=leaflet() %>%
  addProviderTiles('CartoDB') %>%
  addCircleMarkers(data=maleB,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Male"
  ) %>%
  addCircleMarkers(data=femaleB,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Female" 
  ) %>%
  addCircleMarkers(data=unisexB,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Unisex" 
  ) %>%
  addCircleMarkers(data=unknownB,
                   lng = ~long,
                   lat = ~lat,
                   radius = 1.4,
                   clusterOptions = markerClusterOptions(),
                   label = ~label(Klout),
                   popup = ~popup(Lang,RetweetCount),
                   color = ~coloring(gender),
                   group = "Unknown" 
  ) %>%
  addLegend(pal = coloring,
            values = c("Female","Male","Unisex","Unknown")) %>%
  addLayersControl(overlayGroups = c("Male","Female",'Unisex','Unknown')) 

library(leaflet.minicharts)
library(rvest)
#The following piece of code creates a dataset taken from a website. The following website(URL variable) displays a table
#Web scraping
URL="https://developers.google.com/public-data/docs/canonical/countries_csv"
#We go to website
web=read_html(URL)
#We select the table and create a 1D vector with the each observation in the table
all=web %>%
  html_nodes("td") %>% html_text()
#We create a DataFrame using the vector all
df=data.frame(code=all[seq(1,length(all),4)],latitude=all[seq(2,length(all),4)],longtitude=all[seq(3,length(all),4)],country=all[seq(4,length(all),4)])
#We save the DataFrame as a csv file
write.csv(df,'C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/countries.csv')
#We read the dataset we just created.(Optional as we already have the dataset into df).
countries=read.csv("C:/Users/jimbi/OneDrive/Desktop/Semester 2/Data Analytics and Visualisation/Assignment 1/countries.csv")
countries$country[countries$country=="United States"]="USA" 
countries$country[countries$country=="United Kingdom"]="UK"
countries$country[countries$code=="CI"]="Ivory Coast"
countries$country[countries$country=="Congo [DRC]"]="Democratic Republic of the Congo"
countries$country[countries$country=="Congo [Republic]"]="Congo"
str(countries)
View(countries)
#The for loop will create bar charts within the polygons for each country.
for (i in unique(set2$country)){#in set2 dataset we have at most 4 observations for each country(different levels in gender) Unique country to get each country only once
  if(length(which(countries$country==i))==0){next}else{#Same logic as before using length(which) but now when it is equal to zero we skip one iteration 
    x=set2$n[(set2$country==i) & (set2$gender=="Male")]#x,y,z,w will have the values that we are going to create the barchart with
    y=set2$n[(set2$country==i) & (set2$gender=="Female")]
    z=set2$n[(set2$country==i) & (set2$gender=="Unisex")]
    w=set2$n[(set2$country==i) & (set2$gender=="Unknown")]
    if (length(x)==0){x=0}#We pass zero if we don't have tweets for a specific gender in a specific country 
    if (length(y)==0){y=0}
    if (length(z)==0){z=0}
    if (length(w)==0){w=0}
    map=map %>%#We update the map
      addMinicharts(lng = countries$longtitude[countries$country==i],#longitude and latitude from the countries dataset
                    lat = countries$latitude[countries$country==i],
                    chartdata =c(x,y,z,w),
                    type = 'bar',
                    opacity = 1,
                    width = 22,
                    height = 32)
  }
}
#Finally we add a legend and the map is ready
map=map %>% addLegend(position = 'bottomright',
                      pal = colorFactor(palette = c("blue",'orange','green','red'),
                                        levels = c("Male","Female","Unisex","Unknown")),
                      values = c("Male","Female","Unisex","Unknown"),
                      title = "Bar Colors")


for (i in unique(set2B$country)){#Same for the the second dataset
  if(length(which(countries$country==i))==0){next}else{
    x=set2B$n[(set2B$country==i) & (set2B$gender=="Male")]
    y=set2B$n[(set2B$country==i) & (set2B$gender=="Female")]
    z=set2B$n[(set2B$country==i) & (set2B$gender=="Unisex")]
    w=set2B$n[(set2B$country==i) & (set2B$gender=="Unknown")]
    if (length(x)==0){x=0}
    if (length(y)==0){y=0}
    if (length(z)==0){z=0}
    if (length(w)==0){w=0}
    mapB=mapB %>%
      addMinicharts(lng = countries$longtitude[countries$country==i],
                    lat = countries$latitude[countries$country==i],
                    chartdata =c(x,y,z,w),
                    type = 'bar',
                    opacity = 1,
                    width = 22,
                    height = 32)
  }
}
mapB=mapB %>% addLegend(position = 'bottomright',
                        pal = colorFactor(palette = c("blue",'orange','green','red'),
                                          levels = c("Male","Female","Unisex","Unknown")),
                        values = c("Male","Female","Unisex","Unknown"),
                        title = "Bar Colors")







library(shiny)
#We create some copies of our inital datasets as we are going to change some variables
dfA=data
dfB=dataB
str(dfA)
str(dfB)
View(dfB)

dfA$Sentiment[dfA$Sentiment>1]="Much Positive"
dfA$Sentiment[dfA$Sentiment>0 & dfA$Sentiment<=1]="Positive"
dfA$Sentiment[dfA$Sentiment==0]="Neutral"
dfA$Sentiment[dfA$Sentiment>=-1 & dfA$Sentiment<0]="Negative"
dfA$Sentiment[dfA$Sentiment< -1]="Much Negative"

dfB$Sentiment[dfB$Sentiment>1]="Much Positive"
dfB$Sentiment[dfB$Sentiment>0 & dfB$Sentiment<=1]="Positive"
dfB$Sentiment[dfB$Sentiment==0]="Neutral"
dfB$Sentiment[dfB$Sentiment>=-1 & dfB$Sentiment<0]="Negative"
dfB$Sentiment[dfB$Sentiment< -1]="Much Negative"
dfB$gender[dfB$gender==0]="Unknown"
View(dataB)

dframe=data[,-c(12:16)]#We exclude some columns
#In variable Day, we find numbers from 1 to 31 when it goes to 31 the next day will be 1. It will be wrong to group them by day as observations for different days will be grouped 
#To overcome this issue, we start from 1 and when the day changes we add 1 using the for loop.
date=c()
j=1
for (i in seq(1:nrow(dframe))){
  if (dframe$Day[i]==dframe$Day[i+1]){#when the current day and the next day have the same value means that both tweets shared on the same day
    date=c(date,j)
  }else{#When they are not equal, we have reached the last observation of the day. We pass the observation into the vector and we add 1 to j. j shows the day
    date=c(date,j)#From example the first day(j=1) we have x number of tweets. when we reach the last observation of the day, we pass it into vector and j becomes 2 
    j=j+1
  }
  if (i==49999){#Although when we reach the last observation i+1 will be out of bounds. So we break the loop when the i reach 49999
    break
  }
}
tail(dframe)
date=c(date,j)#We pass j into the vector one more time as we are missing one observation, we have to check the tail of the dataset though, because the last observation might be the next day
dframe$time=date#we create a new column
dframeA= dframe %>%#Create a dataset grouping by time and gender
  group_by(time,gender) %>% 
  summarise(Reach=mean(Reach),
            sentiment=mean(Sentiment),
            retw=mean(RetweetCount),
            likes=mean(Likes),
            klout=mean(Klout))



dfra= dframe %>%#One more to count the number of tweets per day for each gender
  group_by(time,gender) %>%
  count()

dframeA$count=dfra$n

males=dframeA %>% filter(gender=="Male")# For datasets for the timeseries
females=dframeA %>% filter(gender=="Female")
unknowns=dframeA %>% filter(gender=="Unknown")
unisexs=dframeA %>% filter(gender=="Unisex")

dframe1=dataB[,-c(12:16)]#for the second dataset
date=c()
j=1
for (i in seq(1:nrow(dframe1))){
  if (dframe1$Day[i]==dframe1$Day[i+1]){
    date=c(date,j)
  }else{
    date=c(date,j)
    j=j+1
  }
  if (i==49999){
    break
  }
}
date=c(date,j)
dframe1$time=date
dframe1A= dframe1 %>%
  group_by(time,gender) %>% 
  summarise(Reach=mean(Reach),
            sentiment=mean(Sentiment),
            retw=mean(RetweetCount),
            likes=mean(Likes),
            klout=mean(Klout))



dfra1= dframe1 %>%
  group_by(time,gender) %>%
  count()

dframe1A$count=dfra1$n

males1=dframe1A %>% filter(gender=="Male")
females1=dframe1A %>% filter(gender=="Female")
unknowns1=dframe1A %>% filter(gender=="Unknown")
unisexs1=dframe1A %>% filter(gender=="Unisex")



#Shiny App
ui=fluidPage(navlistPanel(tabPanel('Point Map',#First tab map
                                   leafletOutput(outputId = 'map1'),
                                   leafletOutput(outputId = 'map1B')),
                          tabPanel("Polygons Map",#Second Tab map with polygons
                                   leafletOutput(outputId = 'map'),
                                   leafletOutput('mapB')),
                          tabPanel("Graphs",#Third tab, we have created 4 boxes. Each one has some options. We can see the options in choices argument
                                   sidebarLayout(sidebarPanel(selectInput(inputId = 'facet1',#The aforementioned were placed in the side panel
                                                                          'Select a variable to split the scatter plot',
                                                                          choices = c('gender','IsReshare','No')),
                                                               selectInput(inputId = 'facet2',
                                                                           'Select a second variable to split the scatter plot',
                                                                           choices = c('gender','IsReshare','No')),
                                                               selectInput(inputId = 'size1',
                                                                           'Select a variable for size',
                                                                            choices = c("Likes", "RetweetCount",NULL)),
                                                              selectInput(inputId = 'Fun',
                                                                          "Select transformation",
                                                                          choices = c('Logarithm',"Arctan","Nothing"))# This one is for the transformation in x axis. Using log() we have a dense scatterplot so we cant distinguish the colors for the middle points. We want a sigmoid function. I am gonna elaborate myself in the report
                                                               ),
                                                 mainPanel(plotOutput(outputId = 'plt1'),#In main panel we plot the graphs
                                                           plotOutput(outputId = 'plt2')
                                                           )
                                                 )
                                   ),
                          tabPanel("Timeseries",#Fourth tab timeseries
                                   sidebarLayout(sidebarPanel(selectInput(inputId = 'time',#We ask for the user to choose one of the variables. The box has placed on side panel
                                                                          'Select variable for timeseries',
                                                                          choices = names(dframeA)[3:8]
                                                                          )),
                                                 mainPanel(plotOutput(outputId = 'plt3'),#We plot it on the main panel
                                                           plotOutput(outputId = 'plt4'))
                                                 )
                                   )
                          )
             ) 


server=function(input,output){
  transf=reactive({if (input$Fun=='Logarithm'){#for the transformation. if user chooses logarithm, we will transform the x axis using log(Reach)
    'log(Reach)'
  }else if (input$Fun=='Arctan'){#If user chooses Arctan, we will transform the x-axis using atan(Reach/548)
    'atan(log(Reach)-6.29)'
  }else {"Reach"}#If none, no transformation will happen
 })
  plt=reactive({ggplot(data = dfA,aes_string(x = transf(), y = "Klout", color = "Sentiment", size = input$size1))+  #We use aes_string() as the inputs are characters
      geom_point()+
      scale_colour_discrete(type = c('red','green','yellow','pink','blue'))+
      theme_dark()})
  plt1=reactive({ggplot(data = dfB,aes_string(x = transf(), y = "Klout", color = "Sentiment", size = input$size1))+  
      geom_point()+
      scale_colour_discrete(type = c('red','green','yellow','pink','blue'))+
      theme_dark()})
  output$map1=renderLeaflet(map1)#We plot map1
  output$map1B=renderLeaflet(map1B)#We plot map1B
  output$map=renderLeaflet(map)#We plot map
  output$mapB=renderLeaflet(mapB)#We plot mapB
  output$plt1=renderPlot({if (input$facet1=='No' & input$facet1=='No'){#We plot the ggplot. We have different occasions for each possible selection from the user. We cover them all using if-elseif-else 
    plt()}else if ((input$facet1=="No" & input$facet2=='gender') | (input$facet1=='gender' & input$facet2=='gender') | (input$facet2=="No" & input$facet1=='gender')){
    plt()+
      facet_wrap(~gender)
      }else if((input$facet1=="No" & input$facet2=='IsReshare') | (input$facet1=='IsReshare' & input$facet2=='IsReshare') | (input$facet2=="No" & input$facet1=='IsReshare')){
        plt()+
          facet_wrap(~IsReshare)
      }else{plt()+
             facet_wrap(~IsReshare+gender)}})
  output$plt2=renderPlot({if (input$facet1=='No' & input$facet1=='No'){#Same as before for the second dataset
    plt1()}else if ((input$facet1=="No" & input$facet2=='gender') | (input$facet1=='gender' & input$facet2=='gender') | (input$facet2=="No" & input$facet1=='gender')){
      plt1()+
        facet_wrap(~gender)
    }else if((input$facet1=="No" & input$facet2=='IsReshare') | (input$facet1=='IsReshare' & input$facet2=='IsReshare') | (input$facet2=="No" & input$facet1=='IsReshare')){
      plt1()+
        facet_wrap(~IsReshare)
    }else{plt1()+
             facet_wrap(~IsReshare+gender)}})
  output$plt3=renderPlot({ggplot(data = dframeA,aes(color=gender))+#We plot the timeseries, we use aes in ggplot only to get the legend
    geom_line(data=males,aes_string(x='time',y=input$time))+#The following lines plot the timeseries
    geom_line(data=females,aes_string(x='time',y=input$time))+
    geom_line(data=unknowns,aes_string(x='time',y=input$time))+
    geom_line(data=unisexs,aes_string(x='time',y=input$time))})
  output$plt4=renderPlot({ggplot(data= dframe1A,aes(color=gender))+#Same as before for the second dataset
    geom_line(data=males1,aes_string(x='time',y=input$time))+
    geom_line(data=females1,aes_string(x='time',y=input$time))+
    geom_line(data=unknowns1,aes_string(x='time',y=input$time))+
    geom_line(data=unisexs1,aes_string(x='time',y=input$time))})
}

shinyApp(ui = ui,server = server)#We run the application

#------------------end of code-------------------
