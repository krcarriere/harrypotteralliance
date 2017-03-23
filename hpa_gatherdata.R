library(Rfacebook)
library(devtools)
library(ggplot2)
library(plyr)
#Some of this is borrowed from the FacebookR github:
#https://github.com/pablobarbera/Rfacebook

#I use this auth to make authing easier
#See here http://thinktostart.com/analyzing-facebook-with-r/

source("facebook_appid.R")
source("hpa_functions.R")
#So, we need to protect our appid and appsecret.
#This is something just for me, so I have a file called "facebook_appid.R"
#And in it, there are two variables, appid and appsecret, with characters of my app id and app secret.
#like Appid <- "XXX" appsecret <- "XXXX"

fb_oauth <- fbOAuth(app_id= appid, app_secret=appsecret, extended_permissions=TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

#This will retrieve all posts by HPA - currently at 3940
HPA <- getPage("thehpalliance", token=fb_oauth, n = 5000)

HPA$datetime <- format.facebook.date(HPA$created_time)
attr(HPA$datetime, "tzone") <- "America/New_York"
HPA <- HPA[order(HPA$datetime),]

HPA$month <- format(HPA$datetime, "%Y-%m")
HPA$year <- format(HPA$datetime, "%Y")


#Find Out What Day a Given Post was Used
HPA$dayofweek <- as.POSIXlt(HPA$datetime)$wday
HPA$dayofweek <- as.factor(HPA$dayofweek)

HPA$dayofweek <- plyr::mapvalues(HPA$dayofweek, from = c("0","1","2","3","4","5","6"), to = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) 

#Find Out Which 4 Hour Time Frame Gets Best
HPA$timeofday <- substr(HPA$datetime, 12, 16)
HPA$hourofday <- as.numeric(substr(HPA$datetime, 12, 13))

#Other Variables We Can Create
HPA$fourhour <- 1
HPA$fourhour[HPA$hourofday==0 | HPA$hourofday==1 | HPA$hourofday==2 | HPA$hourofday==3] <- 0
HPA$fourhour[HPA$hourofday==4 | HPA$hourofday==5 | HPA$hourofday==6 | HPA$hourofday==7] <- 1
HPA$fourhour[HPA$hourofday==8 | HPA$hourofday==9 | HPA$hourofday==10 | HPA$hourofday==11] <- 2
HPA$fourhour[HPA$hourofday==12 | HPA$hourofday==13 | HPA$hourofday==14 | HPA$hourofday==15] <- 3
HPA$fourhour[HPA$hourofday==16 | HPA$hourofday==17 | HPA$hourofday==18 | HPA$hourofday==19] <- 4
HPA$fourhour[HPA$hourofday==20 | HPA$hourofday==21 | HPA$hourofday==22 | HPA$hourofday==23] <- 5
HPA$fourhour <- plyr::mapvalues(HPA$fourhour, from = c("0","1","2","3","4","5"), to = c("12AM to 3AM", "4AM to 7AM", "8AM to 11AM", "12PM to 3PM", "4PM to 7PM", "8PM to 11PM")) 
HPA$fourhour <- factor(HPA$fourhour, levels=c("12AM to 3AM", "4AM to 7AM", "8AM to 11AM", "12PM to 3PM", "4PM to 7PM", "8PM to 11PM"))

#Create Other Variables
HPA$hrs_squared <- (HPA$hourofday*HPA$hourofday)
HPA$hrs_cubed <- (HPA$hourofday*HPA$hourofday*HPA$hourofday)
HPA$factor_hours <- as.factor(HPA$hourofday)


write.csv(HPA, "facebook_hpa.csv")