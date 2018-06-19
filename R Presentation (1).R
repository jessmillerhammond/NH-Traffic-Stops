Jon Bieniek

install.packages("tidyverse")
NH.Data <- read.csv("C:\\Users\\BieniekJon\\Desktop\\R\\NH_cleaned.csv")
Weather.data<- read.csv("C:\\Users\\BieniekJon\\Desktop\\R\\weather.csv")

library(dplyr)
library(Hmisc)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(plotly)

# creates tibble trables
nh.data <- tbl_df(NH.Data)

nh.data$num_stops<- mutate(nh.data,num_stops = 1) 

Stops <- nh.data%>%
  group_by(stop_date)%>%``
  summarise(n=n())

Weather.select <-select(Weather.data, DATE, PRCP,TMAX, SNOW)

weather.next <- Weather.select %>%
  group_by(DATE) %>%
  summarise(PRCP1 = max(PRCP, na.rm = TRUE), TMAX1 = max(TMAX, na.rm = TRUE), SNOW1 = max(SNOW, na.rm = TRUE))

Stops <- rename(Stops, DATE= Date)

joined <- left_join(weather.next, Stops, by="DATE")

ggplot(joined, aes(x = PRCP1, y = n))+
  xlim(0,4) +
  ylim(0,12000) +
  xlab("Inches of Rain")+
  ylab("# of Tickets")+
  ggtitle("Rain vs Number of Tickets")+
  geom_bar(stat = 'identity', fill="seagreen3")


ggplot(joined, aes(x = SNOW1, y = n))+
  xlim(0,20) +
  ylim(0,5500) +
  xlab("Inches of Snow")+
  ylab("# of Tickets")+
  theme_classic()+
  ggtitle("Snow vs Number of Tickets")+
  geom_bar(stat = 'identity', fill="steelblue")


ggplot(joined, aes(x = TMAX1, y = n))+
  xlim(0,100) +
  ylim(0,10000) +
  xlab("Temperature")+
  ylab("# of Tickets")+
  theme_classic()+
  ggtitle("Temperature vs Number of Tickets")+
  geom_bar(stat = 'identity', fill="palevioletred")


Manoj Virigineni

getwd()
library(dplyr)
library(lubridate)
library(tidyr)
library(plotly)
library(ggplot2)
library(gridExtra)

mydata <- read.csv("NH_cleaned.csv")

mydata$month_NH <- month(ymd(mydata$stop_date), label = TRUE)

NH <- tbl_df(mydata)

NH <- tidyr::separate(NH,stop_date, c("year", "month", "date"))

# Males with any kind of violation and the speed_outcome as ticket in 2014

M_violation_ticket <- NH %>%
  filter(driver_gender == "M" & year == 2014 & stop_outcome == "Ticket" ) %>%
  group_by(month_NH)

M_violation_ticket

M_violation_ticket_2014 <- M_violation_ticket %>%
  group_by(month_NH) %>%
  summarise(Tickets=n())

M_violation_ticket_2014

P <- ggplot(M_violation_ticket_2014, aes(x = month_NH, y = Tickets, group = 1)) + geom_line() + xlab("Month") + ylab("Tickets") + 
  ggtitle("Males_Violation_Tickets_2014") + theme_classic()


ggplotly(P)

# Males with any kind of violation and the speed_outcome as warnings in 2014

M_violation_warning <- NH %>%
  filter(driver_gender =="M" & year == 2014 & stop_outcome == "Warning") %>%
  group_by(month_NH)

M_violation_warning

M_violation_warning_2014 <- M_violation_warning %>%
  group_by(month_NH) %>%
  summarise(Warning=n())

M_violation_warning_2014


Q <- ggplot(M_violation_warning_2014, aes(x = month_NH, y = Warning, group = 1)) + geom_line() + xlab("Month") + ylab("Warnings") + 
  ggtitle("Males_Violation_warnings_2014") + theme_classic()

ggplotly(Q)


# Males with any kind of violation and the speed_outcome as Summons in 2014

M_violation_summons <- NH %>%
  filter(driver_gender =="M" & year == 2014 & stop_outcome == "Summons") %>%
  group_by(violation)

M_violation_summons

M_violation_Summons_2014 <- M_violation_summons %>%
  group_by(month_NH) %>%
  summarise(Summons=n())

M_violation_Summons_2014

R <- ggplot(M_violation_Summons_2014, aes(x = month_NH, y = Summons, group = 1)) + geom_line() + xlab("Month") + ylab("Summons") + 
  ggtitle("Males_Violation_summons_2014") + theme_classic()

ggplotly(R)

# Males with any kind of violation and the speed_outcome as Checkup in 2014

M_violation_checkup <- NH %>%
  filter(driver_gender =="M" & year == 2014 & stop_outcome == "Checkup") %>%
  group_by(violation)

M_violation_checkup

M_violation_Checkup_2014 <- M_violation_checkup %>%
  group_by(month_NH) %>%
  summarise(Checkup=n())

M_violation_Checkup_2014

S <- ggplot(M_violation_Checkup_2014, aes(x = month_NH, y = Checkup, group = 1)) + geom_line() + xlab("Month") + ylab("Check_up") + 
  ggtitle("Males_Violation_Checkup_2014") + theme_classic()

ggplotly(S)

# Males with any kind of violation and the speed_outcome as ticket in 2015

M_violation_ticket_2015 <- NH %>%
  filter(driver_gender == "M" & year == 2015 & stop_outcome == "Ticket" ) %>%
  group_by(month_NH)

M_violation_ticket_2015

M_violation_ticket_2015 <- M_violation_ticket_2015 %>%
  group_by(month_NH) %>%
  summarise(Tickets=n())

M_violation_ticket_2015

T <- ggplot(M_violation_ticket_2015, aes(x = month_NH, y = Tickets, group = 1)) + geom_line() + xlab("Month") + ylab("Tickets") + 
  ggtitle("Males_Violation_Tickets_2015") + theme_classic()

ggplotly(T)

# Males with any kind of violation and the speed_outcome as warnings in 2015

M_violation_warning_2015 <- NH %>%
  filter(driver_gender =="M" & year == 2015 & stop_outcome == "Warning") %>%
  group_by(month_NH)

M_violation_warning

M_violation_warning_2015 <- M_violation_warning_2015 %>%
  group_by(month_NH) %>%
  summarise(Warning=n())

M_violation_warning_2015


U <- ggplot(M_violation_warning_2015, aes(x = month_NH, y = Warning, group = 1)) + geom_line() + xlab("Month") + ylab("Warnings") + 
  ggtitle("Males_Violation_warning_2015") + theme_classic()

ggplotly(U)

# Males with any kind of violation and the speed_outcome as Summons in 2014

M_violation_summons_2015 <- NH %>%
  filter(driver_gender =="M" & year == 2015 & stop_outcome == "Summons") %>%
  group_by(violation)

M_violation_summons_2015

M_violation_Summons_2015 <- M_violation_summons_2015 %>%
  group_by(month_NH) %>%
  summarise(Summons=n())

M_violation_Summons_2015

V <- ggplot(M_violation_Summons_2015, aes(x = month_NH, y = Summons, group = 1)) + geom_line() + xlab("Month") + ylab("Summons") + 
  ggtitle("Males_Violation_summons_2015") + theme_classic()

ggplotly(V)

# Males with any kind of violation and the speed_outcome as Checkup in 2015

M_violation_checkup_2015 <- NH %>%
  filter(driver_gender =="M" & year == 2015 & stop_outcome == "Checkup") %>%
  group_by(violation)

M_violation_checkup

M_violation_Checkup_2015 <- M_violation_checkup_2015 %>%
  group_by(month_NH) %>%
  summarise(Checkup=n())

M_violation_Checkup_2014

W <- ggplot(M_violation_Checkup_2015, aes(x = month_NH, y = Checkup, group = 1)) + geom_line() + xlab("Month") + ylab("Check_up") + 
  ggtitle("Males_Violation_Checkups_2015") + theme_classic()



ggplotly(W)

###########################################################################################
###########################################################################################


# Females with any kind of violation and the speed_outcome as ticket in 2014

F_violation_ticket <- NH %>%
  filter(driver_gender == "F" & year == 2014 & stop_outcome == "Ticket" ) %>%
  group_by(month_NH)

F_violation_ticket

F_violation_ticket_2014 <- F_violation_ticket %>%
  group_by(month_NH) %>%
  summarise(Tickets=n())

F_violation_ticket_2014

p <- ggplot(F_violation_ticket_2014, aes(x = month_NH, y = Tickets, group = 1)) + geom_line() + xlab("Month") + ylab("Tickets") + 
  ggtitle("Females_Violation_Tickets_2014") + theme_classic()

ggplotly(p)

# Feales with any kind of violation and the speed_outcome as warnings in 2014

F_violation_warning <- NH %>%
  filter(driver_gender =="F" & year == 2014 & stop_outcome == "Warning") %>%
  group_by(month_NH)

F_violation_warning

F_violation_warning_2014 <- F_violation_warning %>%
  group_by(month_NH) %>%
  summarise(Warning=n())

M_violation_warning_2014


q <- ggplot(F_violation_warning_2014, aes(x = month_NH, y = Warning, group = 1)) + geom_line() + xlab("Month") + ylab("Warnings") + 
  ggtitle("Females_Violation_warning_2014") + theme_classic()

ggplotly(q)

# Females with any kind of violation and the speed_outcome as Summons in 2014

F_violation_summons <- NH %>%
  filter(driver_gender =="F" & year == 2014 & stop_outcome == "Summons") %>%
  group_by(violation)

F_violation_summons

F_violation_Summons_2014 <- F_violation_summons %>%
  group_by(month_NH) %>%
  summarise(Summons=n())

F_violation_Summons_2014

r <- ggplot(F_violation_Summons_2014, aes(x = month_NH, y = Summons, group = 1)) + geom_line() + xlab("Month") + ylab("Summons") + 
  ggtitle("Females_Violation_summons_2014") + theme_classic()

ggplotly(r)

# Females with any kind of violation and the speed_outcome as Checkup in 2014

F_violation_checkup <- NH %>%
  filter(driver_gender =="F" & year == 2014 & stop_outcome == "Checkup") %>%
  group_by(violation)

F_violation_checkup

F_violation_Checkup_2014 <- F_violation_checkup %>%
  group_by(month_NH) %>%
  summarise(Checkup=n())

F_violation_Checkup_2014

s <- ggplot(F_violation_Checkup_2014, aes(x = month_NH, y = Checkup, group = 1)) + geom_line() + xlab("Month") + ylab("Check_up") + 
  ggtitle("Females_Violation_checkup_2014") + theme_classic()



ggplotly(s)

# Females with any kind of violation and the speed_outcome as ticket in 2015

F_violation_ticket_2015 <- NH %>%
  filter(driver_gender == "F" & year == 2015 & stop_outcome == "Ticket" ) %>%
  group_by(month_NH)

F_violation_ticket_2015

F_violation_ticket_2015 <- F_violation_ticket_2015 %>%
  group_by(month_NH) %>%
  summarise(Tickets=n())

F_violation_ticket_2015

t <- ggplot(F_violation_ticket_2015, aes(x = month_NH, y = Tickets, group = 1)) + geom_line() + xlab("Month") + ylab("Tickets") + 
  ggtitle("Females_Violation_Tickets_2015") + theme_classic()

ggplotly(t)

# Females with any kind of violation and the speed_outcome as warnings in 2015

F_violation_warning_2015 <- NH %>%
  filter(driver_gender =="F" & year == 2015 & stop_outcome == "Warning") %>%
  group_by(month_NH)

F_violation_warning

F_violation_warning_2015 <- F_violation_warning_2015 %>%
  group_by(month_NH) %>%
  summarise(Warning=n())

F_violation_warning_2015


u <- ggplot(F_violation_warning_2015, aes(x = month_NH, y = Warning, group = 1)) + geom_line() + xlab("Month") + ylab("Warnings") + 
  ggtitle("Females_Violation_warning_2015") + theme_classic()

ggplotly(u)

# Females with any kind of violation and the speed_outcome as Summons in 2015

F_violation_summons_2015 <- NH %>%
  filter(driver_gender == "F" & year == 2015 & stop_outcome == "Summons") %>%
  group_by(violation)

F_violation_summons_2015

F_violation_Summons_2015 <- F_violation_summons_2015 %>%
  group_by(month_NH) %>%
  summarise(Summons=n())

F_violation_Summons_2015

v <- ggplot(F_violation_Summons_2015, aes(x = month_NH, y = Summons, group = 1)) + geom_line() + xlab("Month") + ylab("Summons") + 
  ggtitle("Females_Violation_summons_2015") + theme_classic()

ggplotly(v)

# Females with any kind of violation and the speed_outcome as Checkup in 2015

F_violation_checkup_2015 <- NH %>%
  filter(driver_gender =="F" & year == 2015 & stop_outcome == "Checkup") %>%
  group_by(violation)

F_violation_checkup

F_violation_Checkup_2015 <- F_violation_checkup_2015 %>%
  group_by(month_NH) %>%
  summarise(Checkup=n())

F_violation_Checkup_2014

w <- ggplot(F_violation_Checkup_2015, aes(x = month_NH, y = Checkup, group = 1)) + geom_line() + xlab("Month") + ylab("Check_up") + 
  ggtitle("Females_Violation_checkup_2015") + theme_classic()

ggplotly(w)

#plots for 2014 and 2015

multiplot(P, Q, R, S)
multiplot(T, U, V, W)
multiplot(p, q, r, s)
multiplot(t, u, v, w)


Jessica Hammond 

#This code separated the stop date into three categories.  FALSE is to keep source column
mydates<- tidyr::separate(tblnhcleaned,stop_date, c("year", "month", "day"),remove=FALSE)
mydates
class(mydates$stop_time)
as.numeric()
mydates%>%
  group_by(county_name)%>%
  select(year:day, stop_outcome)
mydates
mycounties<-select(mydates,one_of(c("county_name", "stop_date","year", "month", "day", "stop_outcome")))
RockinghamMonths<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning"))
#Create a vector listin the number of days in the month for each month of the year
monthDays<-c(31,28,31,30,31,30,31,31,30,31,30,31)
#Create 3 separate tables for months ending in 28, 30, and 31 days
RockinghamMonth30<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & monthDays[as.numeric(month)]==30)
RockinghamMonth31<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & monthDays[as.numeric(month)]==31)
RockinghamMonth28<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & monthDays[as.numeric(month)]==28)
RockinghamMonthDec<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & as.numeric(month)==12)
RockinghamMonthMay<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & as.numeric(month)==5)
RockinghamMonthSep<-filter(mycounties,county_name=="Rockingham County" & (stop_outcome=="Ticket"| stop_outcome=="Warning") & as.numeric(month)==9)
RockinghamMonth30$day <- as.numeric(RockinghamMonth30$day)
RockinghamMonth31$day <- as.numeric(RockinghamMonth31$day)
RockinghamMonth28$day <- as.numeric(RockinghamMonth28$day)
RockinghamMonthDec$day <- as.numeric(RockinghamMonthDec$day)
RockinghamMonthMay$day <- as.numeric(RockinghamMonthMay$day)
RockinghamMonthSep$day <- as.numeric(RockinghamMonthSep$day)
###MY Graph for R HW Project
install.packages("ggplot2")
library(ggplot2)
z30<- ggplot(RockinghamMonth30,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=30)
z30<- z30+ ggtitle("Stop Outcomes Rockingham County April,June,Sep, and Nov")  
z30 <- z30 + scale_x_discrete(name="Days", limits=c(1:30))
z30
z31<- ggplot(RockinghamMonth31,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=31)
z31<- z31+ ggtitle("Stop Outcomes Rockingham County Jan, Mar, May, July, Aug, Oct and Dec")  
z31 <- z31 + scale_x_discrete(name="Days", limits=c(1:31))
z31
z28<- ggplot(RockinghamMonth28,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=28)
z28<- z28+ ggtitle("Stop Outcomes Rockingham County Feb")  
z28 <- z28 + scale_x_discrete(name="Days", limits=c(1:28))
z28
zDec<- ggplot(RockinghamMonthDec,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=31)
zDec<- zDec+ ggtitle("Stop Outcomes Rockingham County Dec")  
zDec
zMay<- ggplot(RockinghamMonthMay,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=31)
zMay<- zMay+ ggtitle("Stop Outcomes Rockingham County May")  
zMay
zSep<- ggplot(RockinghamMonthSep,aes(x=day, fill=stop_outcome))+ geom_histogram(position="dodge", bins=30)
zSep<- zSep+ ggtitle("Stop Outcomes Rockingham County Sep")  
zSep


Jared fortier

# library(RCurl)
# library(xlsx)
# library(ggmap)
# library(zipcode)
# library(ggplot2)
# library(tidyr)
# library(tibble)
# library(dplyr)
# library(Hmisc)
# setwd("C:\\Users\\Dolomite\\Desktop\\Analytics\\801_R_programming")
# 
## Read data and remove empty columns
# nh <- read.csv("NH_cleaned.csv")
# nh <- tbl_df(nh)
# nh %>%
#   select(-police_department) %>%
#   select(-state) %>%
#   select(-is_arrested) %>%
#   select(-contraband_found) %>%
#   select(-search_type_raw) %>%
#   select(-search_type) %>%
#   select(-search_conducted) -> nhSubset



latlon <- nhSubset[c('lat', 'lon')]
names(latlon) <- c('latitude','longitude')

nhSubset$county <- nhSubset[c('location_raw')]
names(nhSubset$county) <- c('County')


# lookUpTableCounty <- c("BELKNAP" = 1, "CARROLL" = 2, "CHESHIRE" = 3, "COOS" = 4, "HILLSBOROUGH" = 6, "MERRIMACK" = 7, "ROCKINGHAM" = 8, "STRAFFORD" = 9, "SULLIVAN" = 10)
# nhSubset$county <- lookUpTableCounty[nhSubset$county]

# get geographical coordinates from zipcode
#require(zipcode)
# data(zipcode)
# census$Zip <- clean.zipcodes(census$Zip)
# census <- merge(census, zipcode, by.x='Zip', by.y='zip')

# get a Google map
#require(ggmap)
# map<-get_map(location= c(lon=-70.75, lat=43.04), zoom=12, maptype = "roadmap",
#              source='google',color='color')

######## PORTSMOUTH MAP
map<-get_map(location= c(lon=-70.75, lat=43.04), zoom=12, maptype = "roadmap",
             source='google',color='color')


####### NEW HAMPSHIRE MAP
myLocation <- c(-73, 42, -70, 45.7)

map<-get_map(location=myLocation, zoom=8, maptype = "roadmap",
             source='google', color='color')

###ORIGNAL NH MAP
# map<-get_map(location= c(lon=-71.5, lat=43.7), zoom=8, maptype = "roadmap",
# source='google', color='color')
#cbbPalette = c("#000000", "#E69F00", "#56B4E9", "#FF00FF", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#009933", "#CC79A7")
# plot it with ggplot2
#require("ggplot2")   

###############
#### Non-White Drivers 

driverRaceNonWhite <- filter(nhSubset, driver_race != "White" & driver_race!="")
latLonNonWhite <- driverRaceNonWhite[c('lat', 'lon')]
names(latLonNonWhite) <- c('latitude','longitude')


ggmap(map, extent = "panel") + geom_point(
  aes(x=longitude, y=latitude, colour=driverRaceNonWhite$driver_race),
  data=latLonNonWhite, alpha=.8, na.rm = F, size=4) +
  guides( colour = guide_legend(override.aes = list(size=4))) +
  labs(x="Longitude", y="Latitude", colour="NH Counties")

####qmplot(lon, lat, data = nhSubset, maptype = "toner-lite", color = I("red"))
# ###############
# ggmap(map, extent = "panel") + geom_point(
#   aes(x=longitude, y=latitude, colour=nhSubset$driver_race),
#   data=latlon, alpha=.8, na.rm = T, size=.2)
##############
# annotate('rect', xmin=-70, ymin=42.5, xmax=-73, ymax=45.5, col="red", fill="white")

#### All Drivers - Settings for NH level data
ggmap(map, extent = "panel") + geom_point(
  aes(x=longitude, y=latitude, colour="firebrick"),
  data=latlon, alpha=.8, na.rm = F, size=.6) + 
  guides( colour = guide_legend(override.aes = list(size=4))) +
  labs(x="Longitude", y="Latitude", colour="NH Counties")

#### Drivers by Gender  - Settings for NH level data
driverGender <- filter(nhSubset, driver_gender !="")
latLonGender <- driverGender[c('lat', 'lon')]
names(latLonGender) <- c('latitude','longitude')

ggmap(map, extent = "panel") + geom_point(
  aes(x=longitude, y=latitude, colour=driverGender$driver_gender),
  data=latLonGender, alpha=.8, na.rm = F, size=.6) + 
  guides( colour = guide_legend(override.aes = list(size=4))) +
  labs(x="Longitude", y="Latitude", colour="Gender")

####### Male drivers only
driverMale <- filter(nhSubset, driver_gender =="M")
latLonMale <- driverMale[c('lat', 'lon')]
names(latLonMale) <- c('latitude','longitude')

ggmap(map, extent = "panel") + geom_point(
  aes(x=longitude, y=latitude, color="Male"), color="blue",
  data=latLonMale, alpha=.8, na.rm = F, size=.6) + 
  guides( colour = guide_legend(override.aes = list(size=4))) +
  labs(x="Longitude", y="Latitude", colour="Male")

###### Female drivers only
driverFemale <- filter(nhSubset, driver_gender =="F")
latLonFemale <- driverFemale[c('lat', 'lon')]
names(latLonFemale) <- c('latitude','longitude')

ggmap(map, extent = "panel") + geom_point(
  aes(x=longitude, y=latitude, color="Female"), color="darkorchid3",
  data=latLonFemale, alpha=.8, na.rm = F, size=.6) + 
  guides( colour = guide_legend(override.aes = list(size=4))) +
  labs(x="Longitude", y="Latitude", colour="Female")

#####ORIGINAL
#ggmap(map, extent = "panel") + geom_point(
#   aes(x=longitude, y=latitude, show_guide = TRUE, colour=Median), 
#   data=latlon, alpha=.8, na.rm = T)  + 
#   scale_color_gradient(low="beige", high="blue")

####Tried to save the output map, but I don't think ggsave works with ggmap (just ggplot)
# ggsave("NHTrafficMale.png", dpi = 300)
# unlink("NHTrafficMale.png")
print("end")
####





