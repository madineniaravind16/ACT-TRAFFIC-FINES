library(tidyverse)
library(dplyr)
library("scatterplot3d")

#reading the files
traf_fines <- read.csv("Traffic_camera_offences_and_fines.csv",header =T, na.strings=c(""))
traf_loc <- read.csv("Traffic_speed_camera_locations.csv",header =T, na.strings=c(""))

head(traf_fines)
#view(traf_fines)

#chekcing for missing values
colSums(is.na(traf_fines))
#removing the columns that are unnecessary in traffic fines data set
traf_fines <- subset(traf_fines, select = -c(Sum_With_Amt,Sum_With_Count))

#chekcing for missing values in traffic location dataset and removing a column which has more
#than 70% missing values
colSums(is.na(traf_loc))
traf_loc <- traf_loc[,-8]
#omitting the missing values
traf_loc <- na.omit(traf_loc)

colSums(is.na(traf_fines))
colSums(is.na(traf_loc))

#CHEKCING the percentage of missing values for client category column
perc <- (sum(is.na(traf_fines$Clt_Catg))/nrow(traf_fines)) *100
perc

#omitting missing values
traf_fines <- na.omit(traf_fines)
colSums(is.na(traf_fines))
table(traf_fines$Rego_State)
table(traf_fines$Clt_Catg)

#seperating the offence month into month and years and we have also got time as 
# 12:00 which is due to the excel automatic formatting when csv file opened in excel
traf_fines <- traf_fines %>% 
  separate(ï..Offence_Month, sep = '/', into = c("month","date","year"))

#seeprating the year column into year and time 
traf_fines1 <- traf_fines %>% 
  separate(year, sep = ' ', into = c("year","time"))

#removing the date and time column as they arent that useful for us
traf_fines1 <- subset(traf_fines1, select = -c(date,time))

# finding the count of which state registration causes more issues
vehicle_rego <- traf_fines1 %>% group_by(Rego_State) %>% summarise(Count=sum(Sum_Inf_Count))

#plotting the infringement count by vehicle rego
ggplot(vehicle_rego,aes(x=reorder(Rego_State,Count),y=Count,fill=Rego_State))+
  geom_bar(stat = "identity",
           position = "stack")+
  xlab(label = "Rego State")+
  ylab(label = "Number of Infringements")+
  scale_y_continuous(breaks = seq(0, 400000, by = 40000))+
  theme_light()

# filtering the data by registrating state == ACT
df_act <- traf_fines1[traf_fines1$Rego_State == 'ACT',]

#finding the count of years and months
table(df_act$year)
table(df_act$month)

# finding the count of infringements grouping them by year
year_count_act <- as.data.frame(df_act %>% group_by(year) %>% 
                        summarise(Freq= sum(Sum_Inf_Count)))

# plotting the infringement count as grouped per year
ggplot(year_count_act,
       aes(x = year, y = Freq, color = year))+
  geom_bar(stat = 'identity')+
  geom_point()+
  ggtitle('Tot infringement count per year')+
  xlab(label = 'Year')+
  ylab(label = 'Infringement count')

# checking different values in offence description column and to check the levels we
#need to convert the categorical values into factors  as follows
table(df_act$Offence_Desc)
df_act$Offence_Desc <- as.factor(df_act$Offence_Desc)
levels(df_act$Offence_Desc) 

#so we can see that there are 30 levels and some of them are the same description
#just with some extra characters and we need to rename or recode them as follows and
#for making the function work we used the revalue function from plyr package and after
#using this plyr package is detached from packages

df_act$Offence_Desc <- revalue(df_act$Offence_Desc, c("20 Non-School Zone Exceed Speed Limit > 15 But <= 30 Km/H"="Non-School Zone Exceed Speed Limit > 15 But <= 30 Km/H",
                                              "20 Non-school zone exceed speed limit by <= 15km/h"="Non-school zone exceed speed limit by <= 15km/h",
                                              "20 Non-School Zone Exceed Speed Limit By > 30 But <= 45 Km/H"="Non-School Zone Exceed Speed Limit By > 30 But <= 45 Km/H",
                                              "Non-School Zone Exceed Speed Limit <= 15 Km/H"= "Non-school zone exceed speed limit by <= 15km/h",
                                              "20 Non-School Zone Exceed Speed Limit By > 45 Km/H"="Non-School Zone Exceed Speed Limit By > 45 Km/H",
                                              "20 School Zone  Exceed Speed Limit By <= 15 Km/H"="School Zone  Exceed Speed Limit By <= 15 Km/H",
                                              "20 School Zone Exceed Speed Limit By > 15 But <= 30 Km/H"="School Zone Exceed Speed Limit By > 15 But <= 30 Km/H",
                                              "20 School Zone Exceed Speed Limit By > 30 But <= 45 Km/H" = "School Zone Exceed Speed Limit By > 30 But <= 45 Km/H",
                                              "20 School Zone Exceed Speed Limit By > 45 Km/H" = "School Zone Exceed Speed Limit By > 45 Km/H",
                                              "59 (1) Enter Intersection or Marked Foot Crossing When Traffic Light Red"= "Enter Intersection or Marked Foot Crossing When Traffic Light Red",
                                              "60 Enter Intersection or Marked Foot Crossing When Traffic Arrow Red" = "Enter Intersection or Marked Foot Crossing When Traffic Arrow Red"))



#after renaming the cells we can see there are only 19 levels
table(df_act$Offence_Desc)
levels(df_act$Offence_Desc)


#finding the number of infringements grouping by offence offence description and plotting the top 5
offdes_count_act <- data.frame(df_act %>% group_by(Offence_Desc) %>% 
                                  summarise(Freq= sum(Sum_Inf_Count)))
offdes_count_act <- top_n(offdes_count_act, 5)

#plotting the top 5 offences with infringements
ggplot(offdes_count_act,
       aes(x = Offence_Desc, y = Freq))+ 
  geom_bar(stat = 'identity',width = 0.5,color = "blue")+
  scale_y_continuous(breaks = seq(0, 350000, by = 40000))+
  ggtitle('infringement type')+
  xlab(label = 'offence description')+
  ylab(label = 'Infringement count')+ 
  theme(axis.text.x = element_text(angle = 90) )


# counting the cameras in different locations
(data_cameralocation<- df_act %>% group_by(Location_Desc) %>%
    dplyr::summarise(count=n()) %>% 
    arrange(desc(count)) %>% slice(1:10))
#To plot the graph we have used ggplot
ggplot(data_cameralocation, aes(x=reorder(Location_Desc,-count), y=count,fill=Location_Desc)) +
  scale_x_discrete(labels = abbreviate)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("Location of fine")+ylab("Total number of fines")+
  ggtitle("Graph between Top Ten Locations and count")+
  geom_bar(stat="identity")+ geom_text(aes(label=count),position=position_dodge(width=0.9), 
                                       vjust=-0.25)

#This part is to get the graph of different camera types and the fines whihc came 
#through those cameras
(data_cameratype <- df_act %>% group_by(Camera_Type) %>%
    dplyr::summarise(count=n())) #This is to make another dataset whihc can be used
#to plot the graph

#ggplot is used to plot the data so that it is easy to visualize it rather than 
#reading the numbers
ggplot(data_cameratype, aes(x=reorder(Camera_Type,-count), y=count,fill=Camera_Type)) +
  scale_x_discrete(labels = abbreviate)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("Type of Cameras")+ylab("Total number of fines")+
  ggtitle("Graph between Camera Type and count")+
  geom_bar(stat="identity")+ geom_text(aes(label=count),position=position_dodge(width=0.9), 
                                       vjust=-0.25)

#This part is for the camera types and how are the fines whihc are captured by them
#from 2010 to 2021
#first part is to filter the data which we used for ploting
(data_cameratypey <- df_act %>% group_by(Camera_Type,year) %>%
    dplyr::summarise(count=n()))
#We have used line pot because weare showing the effect of fines over time
ggplot(data_cameratypey, aes(x=year, y=count, group=Camera_Type, size=Camera_Type,color=Camera_Type)) +
  geom_line(stat='identity', position='dodge')+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_size_manual( values = c(1,1,1,1))+xlab("Year")+ylab("Frequency/Count of Fines")+
  ggtitle("Graph between Year and number of fines for each type of camera")


#This part will deal with sum of penalities for top ten locations and how much they
#have earned from 2010 to 2021
#first i have filtered the data and made another dataset
(data_penalitysumd <- df_act %>% group_by(Location_Desc) %>%
    dplyr::summarise(count=sum(Sum_Pen_Amt)) %>%  arrange(desc(count)) %>%
    top_n(10))
#This plot is to visualize the locations and approximate sum whihc is earned by traffic
#cameras
ggplot(data_penalitysumd, aes(x=reorder(Location_Desc,-count), y=count,fill=Location_Desc)) +
  scale_x_discrete(labels = abbreviate)+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("Top Ten earning locations ")+ylab("Total Amount")+
  ggtitle("Graph between Top ten locations and earning")+
  scale_y_continuous(labels = scales::label_number_si())+
  scale_fill_discrete(name="Location Names") +
  geom_bar(stat="identity")+ geom_text(aes(label=paste(round(count/1e6,1),"M")),position=position_dodge(width=0.9), 
                                       vjust=-0.25)

#This part will deal with last 5 year data for top 5 locations to be more
#precise.
#I have filtered the data for top 5 locations first from main data and year from 2017 to 2021.
data_yandlocations <- df_act %>%
  filter(year == c(2017,2018,2019,2020,2021)) %>%
  filter(Location_Desc ==c("NORTHBOURNE AVENUE/LONDON CIRCUIT",
                           "MONARO HIGHWAY BETWEEN MUGGA LANE AND ISABELLA DRIVE",
                           "HINDMARSH DRIVE/BALL STREET",
                           "BARTON HIGHWAY BETWEEN CURRAN DRIVE AND GOLD CREEK ROAD",
                           "MONARO HIGHWAY BETWEEN LANYON DRIVE AND SHEPPARD STREET"))


#To get the sum of total money earned by those  locations
(data_penalitysumdy <- data_yandlocations %>% group_by(Location_Desc,year) %>%
    dplyr::summarise(count=sum(Sum_Pen_Amt)))

#to plot the data
ggplot(data_penalitysumdy, aes(x=year, y=count, group=Location_Desc, size=Location_Desc,color=Location_Desc)) +
  geom_line(stat='identity', position='dodge')+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_size_manual( values = c(1,1,1,1,1) )+xlab("Year")+ylab("Amount of Fines")+
  ggtitle("Graph between Year and Top five locations of fines")+
  scale_y_continuous(labels = scales::label_number_si())

#This part will deal with month vise fines fro last 5 years
data_yandlocations_2 <- df_act %>%
  filter(year == c(2017,2018,2019,2020,2021))%>% 
  group_by(year,month) %>%
  dplyr::summarise(Infringement_count=sum(Sum_Inf_Count))



ggplot(data_yandlocations_2, aes(x=month, y=Infringement_count, group=year,color=year)) +
  geom_line(stat='identity', position='dodge',size=1)+theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_size_manual( values = c(2,2,2,2,2) )+xlab("Months")+ylab("Number of fines")+
  ggtitle("Graph between Months for last five years and fines")+
  scale_y_continuous(labels = scales::label_number_si())



# classifying the infringements into less than 30 and greater than 30 for school zones 
#and non school zones
#non school zone offences
non_sch_less30<- filter(df_act, Offence_Desc == "Non-school zone exceed speed limit by <= 15km/h"|Offence_Desc == "Non-School Zone Exceed Speed Limit > 15 But <= 30 Km/H ")
non_sch_greater30<- filter(df_act, Offence_Desc=="Non-School Zone Exceed Speed Limit By > 30 But <= 45 Km/H"|Offence_Desc=="Non-School Zone Exceed Speed Limit By > 45 Km/H")




# non school zone greater than 30 infringements count
non_sch_zone_count_g30 <- as.data.frame(non_sch_greater30 %>% group_by(year,Offence_Desc) %>% summarise(Count=sum(Sum_Inf_Count)))

# plotting line graph of non school zone offences
non_sch_zone_count_g30 %>% 
  ggplot( aes(x=year, y=Count, group= Offence_Desc, color=Offence_Desc)) +
  geom_line()+
  geom_point()+
  xlab(label = "Year")+
  ylab(label = "Number of Infrngements")+
  ggtitle("Non-School Zone Infrigements caused with speeding greater than 30 Km/H")+
  theme_light()



#non school zone less than 30 infringements count
non_scl_zone_count_less30 <- as.data.frame(non_sch_less30 %>% group_by(year,Offence_Desc) %>% summarise(Count=sum(Sum_Inf_Count)))

# plotting line graph of non school zone offences
non_scl_zone_count_less30 %>% 
  ggplot( aes(x=year, y=Count, group=Offence_Desc, color=Offence_Desc)) +
  geom_line()+
  geom_point()+
  xlab(label = "Year")+
  ylab(label = "Number of Infrngements")+
  ggtitle("Non-School Zone Infrigements caused with speeding less than 30 Km/H")+
  theme_light()

#school zone offences
sch_less30 <- filter(df_act, Offence_Desc =="School Zone  Exceed Speed Limit By <= 15 Km/H" | Offence_Desc =="School Zone Exceed Speed Limit By > 15 But <= 30 Km/H" )
sch_greater30 <- filter(df_act, Offence_Desc=="School Zone Exceed Speed Limit By > 30 But <= 45 Km/H"|Offence_Desc=="School Zone Exceed Speed Limit By > 45 Km/H")


#school zone greater than 30 infringements count
sch_zone_count_g30 <- as.data.frame(sch_greater30 %>% group_by(year,Offence_Desc) %>% summarise(Count=sum(Sum_Inf_Count)))

# plotting line graph of school zone offences
sch_zone_count_g30 %>% 
  ggplot( aes(x=year, y=Count, group=Offence_Desc, color=Offence_Desc)) +
  geom_line()+
  geom_point()+
  xlab(label = "Year")+
  ylab(label = "Number of Infrngements")+
  ggtitle("School Zone Infrigements caused with speeding greater than 30 Km/H")+
  theme_light()

#school zone lesser than 30 infringements count
sz_count_less30 <- as.data.frame(sch_less30 %>% group_by(year,Offence_Desc) %>% summarise(Count=sum(Sum_Inf_Count)))

# plotting line graph of school zone offences
sz_count_less30 %>% 
  ggplot( aes(x=year, y=Count, group=Offence_Desc, color=Offence_Desc)) +
  geom_line()+
  geom_point()+
  xlab(label = "Year")+
  ylab(label = "Number of Infrngements")+
  ggtitle("School Zone Infrigements caused with speeding less than 30")+
  theme_light()

# to understand the which vehicle type causes more fine
vehicle_type <- df_act %>% group_by(Clt_Catg) %>% summarise(Count=sum(Sum_Inf_Count))
vehicle_type <- vehicle_type[order(vehicle_type$Count),]
#View(vehicle_type)

#bar graph
ggplot(vehicle_type,aes(x=reorder(Clt_Catg,-Count),y=Count,fill=Clt_Catg))+
  geom_bar(stat = "identity",
           position = "stack")+
  xlab(label = "Vehicle Category")+
  ylab(label = "Number of Infringements")+
  scale_y_continuous(breaks = seq(0,400000,by=50000))



# to separate the top 10 hot locations      

#typeof(df_act$Location_Code)

df_act$Location_Code <- as.integer(df_act$Location_Code) 
top_loc<- df_act %>% group_by(Location_Code,Location_Desc,Offence_Desc) %>% summarise(Count=sum(Sum_Inf_Count))

top_loc = top_loc[order(top_loc$Count,decreasing=TRUE),]

top10_inf <- top_loc[1:10,c(2,4)]
top10_inf
#View(top10)


# Now we can map these top locations for infringements in a map and for this we need the
# other data set traffic locations where we need longitude and latitude for the locations

traffic_location <- traf_loc %>%
  select(LOCATION_CODE,LATITUDE, LONGITUDE)
# renaming the location_code variable to merge the both datasets
names(df_act)[6] <- "LOCATION_CODE"

#merging the dataset by location code
df_act_new <- merge(df_act,traffic_location, by="LOCATION_CODE")

loc_new<- df_act_new %>% group_by(LOCATION_CODE,Location_Desc,Offence_Desc,LATITUDE,LONGITUDE) %>% summarise(Count=sum(Sum_Inf_Count))
loc_new <- loc_new[order(-loc_new$Count),]
loc_10 <- loc_new[1:10,]


library(ozmaps)
canberra <- ozmaps::abs_ste %>% 
  filter(NAME == "Australian Capital Territory") %>% 
  pull(geometry)    


ggplot(canberra) + 
  geom_sf() +
  geom_point(data = loc_10, aes(x=LONGITUDE,y=LATITUDE, size=Count, color=Location_Desc, alpha=Count), shape=20, stroke=FALSE)+
  geom_text(data= loc_new,aes(x=LONGITUDE, y=LATITUDE, label=Location_Desc),
            color = "dark blue", size=1,fontface = "bold", check_overlap = TRUE,) +
  coord_sf()

















