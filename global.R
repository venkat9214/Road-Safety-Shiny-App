library(dplyr)
library (ggplot2) 
library(ggthemes)
library (lubridate) 
library(labeling)
library(chron)
library(dygraphs)


###################################

# Import truncate data (vc is short for vehicle collision)

vc <- read.csv("NYPD_Motor_Vehicle_Collisions (10000 obs).csv")

#count number of accidents at the same location(latitude and longitude)
cleantable <- vc %>%
  group_by(LONGITUDE,LATITUDE) %>%
  mutate(count = n(),totalinjury = sum(NUMBER.OF.PERSONS.INJURED),
            totaldeath = sum(NUMBER.OF.PERSONS.KILLED))
cleantable <- cleantable[!duplicated(cleantable$LOCATION), ]

cleantable <- cleantable %>%
  select(
    Borough = BOROUGH,
    Zipcode = ZIP.CODE,
    Street1 = ON.STREET.NAME,
    Street2 = CROSS.STREET.NAME,
    Accidents = count,
    Injuries = totalinjury,
    Deaths = totaldeath,
    Lat = LATITUDE,
    Long = LONGITUDE)


#####################
vcr <-  read.csv("NYPD_Motor_Vehicle_Collisions (10000 obs).csv")

#count number of accidents at the same location(latitude and longitude)
cleant <- vcr %>%
  group_by(LONGITUDE,LATITUDE) %>%
  mutate(count = n(),totalinjury = sum(NUMBER.OF.PERSONS.INJURED),
         totaldeath = sum(NUMBER.OF.PERSONS.KILLED))
cleant <- cleant[!duplicated(cleant$LOCATION), ]



cleant$DATE = as.Date(cleant$DATE, "%m/%d/%Y")
cleant$Year = year(cleant$DATE)

tbl_NYPD = tbl_df(vcr)


#PRE-PROCESSING
grp_by_borough = cleant %>% filter(BOROUGH != "") %>% group_by(BOROUGH,Year, VEHICLE.TYPE.CODE.1)
#636,343 observations


Borough_fatalities_by_years = grp_by_borough %>% dplyr::summarise(total_num_fatalities = sum(NUMBER.OF.PERSONS.KILLED))


Borough_mot_ped_cyc_fatalities_by_years = grp_by_borough %>% dplyr::summarise(total_num_motorist_fatalities = sum(NUMBER.OF.MOTORIST.KILLED),total_num_ped_fatalities = sum(NUMBER.OF.PEDESTRIANS.KILLED), total_num_cyclist_fatalities = sum(NUMBER.OF.CYCLIST.KILLED)  ) 

grp_by_borough$upd_time = times(paste0(grp_by_borough$TIME,":00"))
#str(grp_by_borough) # check if upd_time is of time data type



grp_by_borough$Morning_fatalities <- ifelse(grp_by_borough$upd_time > times('06:00:00') & grp_by_borough$upd_time < times('12:00:00'),1,0)

grp_by_borough$Afternoon_fatalities <- ifelse(grp_by_borough$upd_time > times('12:00:00') & grp_by_borough$upd_time < times('17:00:00'),1,0)

grp_by_borough$Evening_fatalities <- ifelse(grp_by_borough$upd_time > times('17:00:00'),1,0)

grp_by_borough$Night_fatalities <- ifelse(grp_by_borough$upd_time > times('00:00:00') & grp_by_borough$upd_time < times('06:00:00'),1,0)


Time_of_day_fatalities <- grp_by_borough[,c(2,4,26,36,37,38,39)]

Time_of_day_fatalities$DATE <- substr(Time_of_day_fatalities$DATE,1,4)

Time_of_fatalities <- Time_of_day_fatalities %>% group_by(BOROUGH, DATE, VEHICLE.TYPE.CODE.1) %>% summarise(Morning_fatalities = sum(Morning_fatalities), Afternoon_fatalities = sum(Afternoon_fatalities), Evening_fatalities = sum(Evening_fatalities), Night_fatalities = sum(Night_fatalities))

############


tis <-  read.csv("NYPD_Motor_Vehicle_Collisions.csv")




tis$DATE = as.Date(tis$DATE, "%m/%d/%Y")

tr <- tis %>% 
  group_by(yr = year(DATE), mon = month(DATE)) %>% 
  summarise(mn_amt = sum(NUMBER.OF.PERSONS.INJURED))



myts <- ts(tr$mn_amt, start=c(2012, 7), end=c(2016, 11), frequency=12)



tr1 <- tis %>% 
  group_by(yr = year(DATE), mon = month(DATE)) %>% 
  summarise(mn_amt = sum(NUMBER.OF.PERSONS.KILLED))



myts1 <- ts(tr1$mn_amt, start=c(2012, 7), end=c(2016, 11), frequency=12)


