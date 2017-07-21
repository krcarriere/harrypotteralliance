#HPA WAS Survey Analysis 2017-2018

#Welcome! My name is Kevin and I'll be leading you through how to analyze our data.

#First, read our data. 
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("psych")

setwd("~/Desktop/S17/HPA/WAS/")
#ggplot2 never likes being called. It always wants to be installed. 
library(ggplot2)
library(plyr)

data <- read.csv("data/HPA_Wizard_Activist_Survey 4.csv")
dwline <- data

#The way Qualtrics works, we need to delete the first line. But, the first line will be useful.
#So, we'll make a copy just for reference.
dwline <- data
data <- data[-1,]

#These are some functions just to clean up the code a bit. I use them a lot. They all basically do the same thing.
#We have some trouble with importing the legacy csv, so these functions clean up the data for us for whatever variable we choose.
factorclean <- function(input){
  input <- as.numeric(levels(input))[input]
  input <- as.factor(input)
  return(input)
}
factorclean_campaign <- function(input){
  input <- as.numeric(levels(input)[input])
  input <- factor(input, 
                  levels=c(0,1,2),
                  labels = c("Heavily Participated",
                                        "Participated in Some Way",
                                        "Heard But Didn't Participate"))
  input <- addNA(input)
  return(input)
}
characterclean <- function(input){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
}
characterclean_ones <- function(input, rename){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename))
  return(input)
}
characterclean_two <- function(input, rename1, rename2){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename1, "2"=rename2))
  return(input)
}
characterclean_three <- function(input, rename1, rename2, rename3){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename1, "2"=rename2, "3"=rename3))
  return(input)
}
characterclean_four <- function(input, rename1, rename2, rename3, rename4){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename1, "2"=rename2, "3"=rename3, "4"=rename4))
  return(input)
}
characterclean_six <- function(input, rename1, rename2, rename3, rename4, rename5, rename6){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename1, "2"=rename2, "3"=rename3, "4"=rename4, "5"=rename5, "6"=rename6))
  return(input)
}
characterclean_eight <- function(input, rename1, rename2, rename3, rename4, rename5, rename6, rename7, rename8){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("1"=rename1, "2"=rename2, "3"=rename3, "4"=rename4, "5"=rename5, "6"=rename6, "7"=rename7, "8"=rename8))
  return(input)
}

#Okay. So, data is our data. $ means variable. There are a lot of packages in R - functions to do things.
#When I want to call a specific function, I always include the package so R knows which one to draw from.
#(see plyr::revalue) while others might just write (revalue). If you need a package, it's:
#install.packages("plyr") and then if you don't want to have to constantly call it, it's "library(plyr)".

data$pick_house <- factorclean(data$pick_house)
data$pick_house <- plyr::revalue(data$pick_house, c("1"="Gryffindor",
                                                    "2"="Hufflepuff",
                                                    "3"="Ravenclaw",
                                                    "4"="Slytherin"))

data$state <- factorclean(data$state)
data$state <- plyr::revalue(data$state, c("1"="Alabama", "2"="Alaska", "3"="Arizona",
                                          "4"="Arkansas", "5"="California", "6"="Colorado",
                                          "7"="Connecticut", "8"="Delaware", "9"="District of Columbia", 
                                          "10"="Florida", "11"="Georgia", "12"="Hawaii",
                                          "13"="Idaho", "14"="Illinois", "15"="Indiana",
                                          "16"="Iowa", "17"="Kansas", "18"="Kentucky",
                                          "19"="Louisiana", "20"="Maine", "21"="Maryland",
                                          "22"="Massachusetts", "23"="Michigan", "24"="Minnesota",
                                          "25"="Mississippi", "26"="Missouri", "27"="Montana",
                                          "28"="Nebraska", "29"="Nevada", "30"="New Hampshire",
                                          "31"="New Jersey", "32"="New Mexico", "33"="New York",
                                          "34"="North Carolina", "35"="North Dakota", "36"="Ohio",
                                          "37"="Oklahoma", "38"="Oregon", "39"="Pennsylvania",
                                          "40"="Rhode Island", "41"="South Carolina", "42"="South Dakota",
                                          "43"="Tennessee", "44"="Texas", "45"="Utah",
                                          "46"="Vermont", "47"="Virginia", "48"="Washington",
                                          "49"="West Virginia", "50"="Wisconsin", "51"="Wyoming"
))

#R has this handy function to do this for us. 
data$stateabb <- state.abb[match(data$state,state.name)]
table(data$stateabb, data$state)

#We have to do this a lot due to that first row that got read in. It's annoying, but needed.
data$age <- as.numeric(levels(data$age))[data$age]

#Basically, in order to allow people to express themselves fully, we needed to allow multiple respones.
#This is not the best practice data collection wise. But - it is important for many of our questions (which eventS did you go to, for instance).
#So, what Qualtrics does is generate a column for each option - and gives a 1 if that option was chosen, blank otherwise.

data$gender_1 <- characterclean_ones(data$gender_1, "Cis man")
data$gender_2 <- characterclean_ones(data$gender_2, "Cis woman")
data$gender_3 <- characterclean_ones(data$gender_3, "Man of trans experience")
data$gender_4 <- characterclean_ones(data$gender_4, "Woman of trans experience")
data$gender_5 <- characterclean_ones(data$gender_5, "Genderqueer")
data$gender_6 <- characterclean_ones(data$gender_6, "Non-binary")
data$gender_7 <- characterclean_ones(data$gender_7, "Non-listed")

data$sex_o_1 <- characterclean_ones(data$sex_o_1, "Attracted to men")
data$sex_o_2 <- characterclean_ones(data$sex_o_2, "Attracted to women")
data$sex_o_3 <- characterclean_ones(data$sex_o_3, "Attracted to agender")
data$sex_o_4 <- characterclean_ones(data$sex_o_4, "Attracted to fill in")
data$sex_o_5 <- characterclean_ones(data$sex_o_5, "Attracted to many genders")
data$sex_o_6 <- characterclean_ones(data$sex_o_6, "Attracted to people regardless")
data$sex_o_7 <- characterclean_ones(data$sex_o_7, "Not sexually attracted to anyone")
data$sex_o_8 <- characterclean_ones(data$sex_o_8, "Not sexually attracted to anyone until emotion")

data$female <- 0
data$female[data$gender_2=="Cis woman"] <- 1
data$female[data$gender_4=="Woman of trans experience"] <- 1
data$female <- factor(data$female,
                      levels = c(0, 1),
                      labels = c("Not Female", "Female"))

data$rom_o_1 <- characterclean_ones(data$rom_o_1, "Attracted to men")
data$rom_o_2 <- characterclean_ones(data$rom_o_2, "Attracted to women")
data$rom_o_3 <- characterclean_ones(data$rom_o_3, "Attracted to agender")
data$rom_o_4 <- characterclean_ones(data$rom_o_4, "Attracted to fill in")
data$rom_o_5 <- characterclean_ones(data$rom_o_5, "Attracted to many genders")
data$rom_o_6 <- characterclean_ones(data$rom_o_6, "Attracted to people regardless")
data$rom_o_7 <- characterclean_ones(data$rom_o_7, "Not romantically attracted to anyone")
data$rom_o_8 <- characterclean_ones(data$rom_o_8, "Not romantically attracted to anyone until emotion")

data$education_r2_1 <- characterclean_ones(data$education_r2_1, "Elementary School")
data$education_r2_2 <- characterclean_ones(data$education_r2_2, "Middle School")
data$education_r2_3 <- characterclean_ones(data$education_r2_3, "High School")
data$education_r2_4 <- characterclean_ones(data$education_r2_4, "Drop Out HS")
data$education_r2_5 <- characterclean_ones(data$education_r2_5, "H.S. Grad")
data$education_r2_6 <- characterclean_ones(data$education_r2_6, "Homeschooled")
data$education_r2_7 <- characterclean_ones(data$education_r2_7, "Homeschool Grad")
data$education_r2_8 <- characterclean_ones(data$education_r2_8, "GED")
data$education_r2_9 <- characterclean_ones(data$education_r2_9, "In 2 Year Degree")
data$education_r2_10 <- characterclean_ones(data$education_r2_10, "In 4 Year Degree")
data$education_r2_11 <- characterclean_ones(data$education_r2_11, "Drop out College")
data$education_r2_12 <- characterclean_ones(data$education_r2_12, "Associates Degree")
data$education_r2_13 <- characterclean_ones(data$education_r2_13, "Bachelor's Degree")
data$education_r2_14 <- characterclean_ones(data$education_r2_14, "In Grad For Masters")
data$education_r2_15 <- characterclean_ones(data$education_r2_15, "In Grad for Terminal")
data$education_r2_16 <- characterclean_ones(data$education_r2_16, "In Medical School")
data$education_r2_17 <- characterclean_ones(data$education_r2_17, "Finished Grad for Masters")
data$education_r2_18 <- characterclean_ones(data$education_r2_18, "Finished Grad for PhD")
data$education_r2_19 <- characterclean_ones(data$education_r2_19, "Finished Medical")

data$years_ed <- as.numeric(levels(data$years_ed))[data$years_ed]

data$newest_sweatshirt <- characterclean_three(data$newest_sweatshirt, "My own money", "Parents no Expectations", "Parents w Expectations")
data$order_food <-  characterclean_three(data$order_food, "My own money","Parents no Expectation", "Parents w Expectation")

data$selfsufficient <- 0
data$selfsufficient[data$order_food=="My own money" & data$newest_sweatshirt=="My own money"] <- 1

data$employed <- characterclean_two(data$employed, "Yes", "No")

data$yearly_income <- characterclean_six(data$yearly_income, "Less than $20000",
                                         "$20000-$34999",
                                         "$35000-$49999",
                                         "$50000-$74999",
                                         "$75000-$99999",
                                         "More than $100,000")

data$parents_income <- characterclean_six(data$parents_income, "Less than $20000",
                                         "$20000-$34999",
                                         "$35000-$49999",
                                         "$50000-$74999",
                                         "$75000-$99999",
                                         "More than $100,000")

data$nerdfighter <- characterclean_four(data$nerdfighter, "Yes DFTBA", "No I am not", "No but I used to be", "I don't know what that is")
data$listen_wrock <- characterclean_four(data$listen_wrock, "Yes I do", "No I do not", "No but i want to look into it", "I used to")

data$length_active_hpa <- as.numeric(levels(data$active_hpa))[data$active_hpa]

data$hpa_fandom_events_1 <- characterclean_ones(data$hpa_fandom_events_1, "Granger Leadership Academy")
data$hpa_fandom_events_2 <- characterclean_ones(data$hpa_fandom_events_2, "LeakyCon")
data$hpa_fandom_events_3 <- characterclean_ones(data$hpa_fandom_events_3, "GeekyCon")
data$hpa_fandom_events_4 <- characterclean_ones(data$hpa_fandom_events_4, "HPEF")
data$hpa_fandom_events_5 <- characterclean_ones(data$hpa_fandom_events_5, "Terminus")
data$hpa_fandom_events_6 <- characterclean_ones(data$hpa_fandom_events_6, "BroadwayCon")
data$hpa_fandom_events_7 <- characterclean_ones(data$hpa_fandom_events_7, "VidCon")
data$hpa_fandom_events_8 <- characterclean_ones(data$hpa_fandom_events_8, "NerdCon: Stories")
data$hpa_fandom_events_9 <- characterclean_ones(data$hpa_fandom_events_9, "NerdCon: Nerdfighteria")
data$hpa_fandom_events_10 <- characterclean_ones(data$hpa_fandom_events_10, "The Yule Ball")
data$hpa_fandom_events_11 <- characterclean_ones(data$hpa_fandom_events_11, "The Quidditch World Cup")
data$hpa_fandom_events_12 <- characterclean_ones(data$hpa_fandom_events_12, "MISTI Con")
data$hpa_fandom_events_13 <- characterclean_ones(data$hpa_fandom_events_13, "NYCC")
data$hpa_fandom_events_14 <- characterclean_ones(data$hpa_fandom_events_14, "SDCC")
data$hpa_fandom_events_15 <- characterclean_ones(data$hpa_fandom_events_15, "Geek Girl Con")
data$hpa_fandom_events_16 <- characterclean_ones(data$hpa_fandom_events_16, "Fill in 1")
data$hpa_fandom_events_17 <- characterclean_ones(data$hpa_fandom_events_17, "Fill in 2")
data$hpa_fandom_events_18 <- characterclean_ones(data$hpa_fandom_events_18, "Fill in 3")
data$hpa_fandom_events_19 <- characterclean_ones(data$hpa_fandom_events_19, "Fill in 4")
data$hpa_fandom_events_20 <- characterclean_ones(data$hpa_fandom_events_20, "Fill in 5")
data$hpa_fandom_events_21 <- characterclean_ones(data$hpa_fandom_events_21, "Never Attended")

data$fandom_fav_event <- as.numeric(levels(data$fandom_fav_event))[data$fandom_fav_event]
data$fandom_fav_event <- as.character(data$fandom_fav_event)
data$fandom_fav_event <- plyr::revalue(data$fandom_fav_event, c("1"="GLA", "2"="LeakyCon", "3"="GeekyCon",
                                                                "4"="HPEF", "5"="Terminus", "6"="BroadwayCon",
                                                                "7"="VidCon", "8"="NerdCon: Stories", "9"="NerdCon: Nerdfighteria",
                                                                "10"="The Yule Ball", "11"="The Quidditch World Cup", "12"="MISTI Con",
                                                                "13"="NYCC", "14"="SDCC", "15"="Geek Girl Con",
                                                                "16"="FillIn1","17"="Fillin2", "18"="Fillin3",
                                                                "19"="Fillin4", "20"="Fillin5", "21"="NeverAttended"))

data$neverattended_1 <- characterclean_ones(data$neverattended_1, "Cost too High")
data$neverattended_2 <- characterclean_ones(data$neverattended_2, "Accessibility")
data$neverattended_3 <- characterclean_ones(data$neverattended_3, "Travel Plans Failed")
data$neverattended_4 <- characterclean_ones(data$neverattended_4, "Did Not Know Events Exist")
data$neverattended_5 <- characterclean_ones(data$neverattended_5, "See Text - Other")

data$chaptermember_1 <- characterclean_ones(data$chaptermember_1,"Yes, as member")
data$chaptermember_2 <- characterclean_ones(data$chaptermember_2,"Yes, as CO")
data$chaptermember_3 <- characterclean_ones(data$chaptermember_3,"No, but I used to be")
data$chaptermember_4 <- characterclean_ones(data$chaptermember_4,"No")
data$chaptermember_5 <- characterclean_ones(data$chaptermember_5,"What's a chapter?")

data$chaptermember <- 0
data$chaptermember[data$chaptermember_1=="Yes, as member"] <- 1
data$chaptermember[data$chaptermember_2=="Yes, as CO"] <- 1
data$chaptermember <- as.factor(data$chaptermember)
data$chaptermember <- factor(data$chaptermember, levels = c(0,1), labels = c("No", "Yes"))

data$volunteerstaff_1 <- characterclean_ones(data$volunteerstaff_1, "Yes")
data$volunteerstaff_2 <- characterclean_ones(data$volunteerstaff_2, "No but interested")
data$volunteerstaff_3 <- characterclean_ones(data$volunteerstaff_3, "No too busy")
data$volunteerstaff_4 <- characterclean_ones(data$volunteerstaff_4, "No, other reasons")
data$volunteerstaff_5 <- characterclean_ones(data$volunteerstaff_5, "You take volunteers?")

data$volunteerstaff <- 0 
data$volunteerstaff[data$volunteerstaff_1=="Yes"] <- 1
data$volunteerstaff <- as.factor(data$volunteerstaff)
data$volunteerstaff <- factor(data$volunteerstaff, levels = c(0,1), labels = c("No", "Yes"))

data$consider_activist_1 <- characterclean_ones(data$consider_activist_1,"Yes")
data$consider_activist_2 <- characterclean_ones(data$consider_activist_2, "No")
data$consider_activist_3 <- characterclean_ones(data$consider_activist_3, "Other")

data$consider_activist <- 0
data$consider_activist[data$consider_activist_1=="Yes"] <- 1
data$consider_activist <- factor(data$consider_activist, levels = c(0,1), labels = c("No", "Yes"))


data$consider_wactivist_1 <- characterclean_ones(data$consider_wactivist_1, "Yes")
data$consider_wactivist_2 <- characterclean_ones(data$consider_wactivist_2, "No")
data$consider_wactivist_3 <- characterclean_ones(data$consider_wactivist_3, "Dont Understand")
data$consider_wactivist_4 <- characterclean_ones(data$consider_wactivist_4, "Other")

data$consider_wactivist <- 0
data$consider_wactivist[data$consider_wactivist_1=="Yes"] <- 1
data$consider_wactivist <- factor(data$consider_wactivist, levels = c(0,1), labels = c("No", "Yes"))

#Comfortable Being Activist
data$wactivist_scale_1 <- as.numeric(levels(data$wactivist_scale_1))[data$wactivist_scale_1]
#Increased Activism Due to HPA
data$wactivist_scale_2 <- as.numeric(levels(data$wactivist_scale_2))[data$wactivist_scale_2]
#Learned About Social Justice HPA
data$wactivist_scale_3 <- as.numeric(levels(data$wactivist_scale_3))[data$wactivist_scale_3]
#New Friends due to HPA
data$wactivist_scale_4 <- as.numeric(levels(data$wactivist_scale_4))[data$wactivist_scale_4]
#Protested Something Wouldnt Have due to HPA
data$wactivist_scale_15 <- as.numeric(levels(data$wactivist_scale_15))[data$wactivist_scale_15]
#Made a Difference HPA
data$wactivist_scale_16 <- as.numeric(levels(data$wactivist_scale_16))[data$wactivist_scale_16]

data$wactivist_s_comfort <- factor(data$wactivist_scale_1,
                                   levels = c(1,2,3,4,5,6,7),
                                   labels = c("Strongly Disagree",
                                              "Disagree",
                                              "Somewhat Disagree",
                                              "Neither Agree nor Disagree", 
                                              "Somewhat Agree",
                                              "Agree",
                                              "Strongly Agree"))

data$wactivist_s_increasea <- factor(data$wactivist_scale_2,
                                     levels = c(1,2,3,4,5,6,7),
                                     labels = c("Strongly Disagree",
                                                "Disagree",
                                                "Somewhat Disagree",
                                                "Neither Agree nor Disagree", 
                                                "Somewhat Agree",
                                                "Agree",
                                                "Strongly Agree"))

data$wactivist_s_learnsj <- factor(data$wactivist_scale_3,
                                   levels = c(1,2,3,4,5,6,7),
                                   labels = c("Strongly Disagree",
                                              "Disagree",
                                              "Somewhat Disagree",
                                              "Neither Agree nor Disagree", 
                                              "Somewhat Agree",
                                              "Agree",
                                              "Strongly Agree"))

data$wactivist_s_newfriends <- factor(data$wactivist_scale_4,
                                      levels = c(1,2,3,4,5,6,7),
                                      labels = c("Strongly Disagree",
                                                 "Disagree",
                                                 "Somewhat Disagree",
                                                 "Neither Agree nor Disagree", 
                                                 "Somewhat Agree",
                                                 "Agree",
                                                 "Strongly Agree"))

data$wactivist_s_protestnew <- factor(data$wactivist_scale_15,
                                      levels = c(1,2,3,4,5,6,7), 
                                      labels = c("Strongly Disagree",
                                                 "Disagree",
                                                 "Somewhat Disagree",
                                                 "Neither Agree nor Disagree", 
                                                 "Somewhat Agree",
                                                 "Agree",
                                                 "Strongly Agree"))

data$wactivist_s_madediff <- factor(data$wactivist_scale_16,
                                    levels = c(1,2,3,4,5,6,7),
                                    labels = c("Strongly Disagree",
                                               "Disagree",
                                               "Somewhat Disagree",
                                               "Neither Agree nor Disagree", 
                                               "Somewhat Agree",
                                               "Agree",
                                               "Strongly Agree"))

data$howgetinvolved_1 <- characterclean_ones(data$howgetinvolved_1, "Spread Word")
data$howgetinvolved_2 <- characterclean_ones(data$howgetinvolved_2, "Keep Up Online")
data$howgetinvolved_3 <- characterclean_ones(data$howgetinvolved_3, "Participate Campaigns")
data$howgetinvolved_4 <- characterclean_ones(data$howgetinvolved_4, "Buy HPA Merch")
data$howgetinvolved_5 <- characterclean_ones(data$howgetinvolved_5, "Contribute HPA Fundraisers")
data$howgetinvolved_6 <- characterclean_ones(data$howgetinvolved_6, "HPA Volunteer Staff")
data$howgetinvolved_7 <- characterclean_ones(data$howgetinvolved_7, "HPA Chapter Member")
data$howgetinvolved_8 <- characterclean_ones(data$howgetinvolved_8, "HPA Chapter CO")
data$howgetinvolved_9 <- characterclean_ones(data$howgetinvolved_9, "GLA Attend")
data$howgetinvolved_10 <- characterclean_ones(data$howgetinvolved_10, "Other")

data$areas_of_activism_1 <- characterclean_ones(data$areas_of_activism_1, "Gender Equality")
data$areas_of_activism_2 <- characterclean_ones(data$areas_of_activism_2, "Racial Equality")
data$areas_of_activism_3 <- characterclean_ones(data$areas_of_activism_3, "Entho-Religious Equality")
data$areas_of_activism_4 <- characterclean_ones(data$areas_of_activism_4, "Environmental Issues")
data$areas_of_activism_5 <- characterclean_ones(data$areas_of_activism_5, "Clean Energy Issues")
data$areas_of_activism_6 <- characterclean_ones(data$areas_of_activism_6, "Net Neutrality")
data$areas_of_activism_7 <- characterclean_ones(data$areas_of_activism_7, "Media Reform")
data$areas_of_activism_8 <- characterclean_ones(data$areas_of_activism_8, "Free Trade")
data$areas_of_activism_9 <- characterclean_ones(data$areas_of_activism_9, "Education")
data$areas_of_activism_10 <- characterclean_ones(data$areas_of_activism_10, "Library Advocacy")
data$areas_of_activism_11 <- characterclean_ones(data$areas_of_activism_11, "LGBTQIA Rights")
data$areas_of_activism_12 <- characterclean_ones(data$areas_of_activism_12, "Missed 1")
data$areas_of_activism_13 <- characterclean_ones(data$areas_of_activism_13, "Missed 2")
data$areas_of_activism_14 <- characterclean_ones(data$areas_of_activism_14, "Missed 3")

data$howlearn_hpa <- as.numeric(levels(data$howlearn_hpa))[data$howlearn_hpa]
data$howlearn_hpa <-  factor(data$howlearn_hpa,
                             levels = c(1,2,3,4,5,6,7),
                             labels = c("Social Media",
                                        "Chapter",
                                        "Nerdfighteria",
                                        "Con or Event", 
                                        "Teacher",
                                        "Friend",
                                        "Other"))

data$campaign_acciobooks <- factorclean_campaign(data$campaigns_1_Group)
data$campaign_bodybindh <- factorclean_campaign(data$campaigns_2_Group)
data$campaign_breakmugg <- factorclean_campaign(data$campaigns_3_Group)
data$campaign_bullyingh <- factorclean_campaign(data$campaigns_4_Group)
data$campaign_climateh <- factorclean_campaign(data$campaigns_5_Group)
data$campaign_darfur <- factorclean_campaign(data$campaigns_6_Group)
data$campaign_darkwaldemort <- factorclean_campaign(data$campaigns_7_Group)
data$campaign_dementorh <- factorclean_campaign(data$campaigns_8_Group)
data$campaign_estherday <- factorclean_campaign(data$campaigns_9_Group)
data$campaign_fanworksfair <- factorclean_campaign(data$campaigns_10_Group)
data$campaign_helphaiti <- factorclean_campaign(data$campaigns_11_Group)
data$campaign_hungernogame <- factorclean_campaign(data$campaigns_12_Group)
data$campaign_magicalacts <- factorclean_campaign(data$campaigns_13_Group)
data$campaign_nevillefights <- factorclean_campaign(data$campaigns_14_Group)
data$campaign_notinharrysname <- factorclean_campaign(data$campaigns_15_Group)
data$campaign_oddsinfavor <- factorclean_campaign(data$campaigns_16_Group)
data$campaign_positivefandom <- factorclean_campaign(data$campaigns_17_Group)
data$campaign_protego <- factorclean_campaign(data$campaigns_18_Group)
data$campaign_rockingvolde <- factorclean_campaign(data$campaigns_19_Group)
data$campaign_superman <- factorclean_campaign(data$campaigns_20_Group)
data$campaign_videocreators <- factorclean_campaign(data$campaigns_21_Group)
data$campaign_vote2012 <- factorclean_campaign(data$campaigns_22_Group)
data$campaign_wwdumbledoredo <- factorclean_campaign(data$campaigns_23_Group)
data$campaign_wizrockvote <- factorclean_campaign(data$campaigns_24_Group)
data$campaign_wrock2009 <- factorclean_campaign(data$campaigns_25_Group)
data$campaign_wrock2011 <- factorclean_campaign(data$campaigns_26_Group)
data$campaign_chapterevent1 <- factorclean_campaign(data$campaigns_27_Group)
data$campaign_chapterevent2 <- factorclean_campaign(data$campaigns_28_Group)

data$fav_campaign <- as.numeric(levels(data$fav_campaign))[data$fav_campaign]
data$fav_campaign <-  factor(data$fav_campaign,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                        20,21, 22,23,24,25,26,27,28),
                             labels = c("Accio Books", "Bodybind Horcrux",
                                        "Breaking the Muggle Mindset", "Bullying Horcrux",
                                        "Climate Horcrux",   "Darfur Group",
                                        "Dark Lord Waldemort", "Dementor Horcrux",
                                        "Esther's Day", "Fan Works are Fair Use",
                                        "Helping Haiti Heal",  "Hunger is Not a Game",
                                        "Magical Acts of Kindness", "Neville Fights Back",
                                        "Not in Harry's Name", "Odds In Our Favor", 
                                        "Positive Fandom", "Protego",
                                        "Rock Voldemedia", "Superman was an Immigrant",
                                        "Vid Creators for Net Neutrality", "Vote 2012", 
                                        "What Would Dumbledore Do",  "Wizard Wrock the Vote", 
                                        "Wrock for Equality 2009", "Wrock for Equality 2011", 
                                        "Chapter Event 1", "Chapter Event 2" ))

data$leastfav_campaign <-  factor(data$leastfav_campaign,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                        20,21, 22,23,24,25,26,27,28),
                             labels = c("Accio Books", "Bodybind Horcrux",
                                        "Breaking the Muggle Mindset", "Bullying Horcrux",
                                        "Climate Horcrux",   "Darfur Group",
                                        "Dark Lord Waldemort", "Dementor Horcrux",
                                        "Esther's Day", "Fan Works are Fair Use",
                                        "Helping Haiti Heal",  "Hunger is Not a Game",
                                        "Magical Acts of Kindness", "Neville Fights Back",
                                        "Not in Harry's Name", "Odds In Our Favor", 
                                        "Positive Fandom", "Protego",
                                        "Rock Voldemedia", "Superman was an Immigrant",
                                        "Vid Creators for Net Neutrality", "Vote 2012", 
                                        "What Would Dumbledore Do",  "Wizard Wrock the Vote", 
                                        "Wrock for Equality 2009", "Wrock for Equality 2011", 
                                        "Chapter Event 1", "Chapter Event 2" ))


data$hpa_rating_1 <- as.numeric(levels(data$hpa_rating_1))[data$hpa_rating_1]

data$howkeepup_1 <- characterclean_ones(data$howkeepup_1, "Email")
data$howkeepup_2 <- characterclean_ones(data$howkeepup_2, "Twitter")
data$howkeepup_3 <- characterclean_ones(data$howkeepup_3, "Tumblr")
data$howkeepup_4 <- characterclean_ones(data$howkeepup_4, "Facebook")
data$howkeepup_5 <- characterclean_ones(data$howkeepup_5, "Instagram")
data$howkeepup_6 <- characterclean_ones(data$howkeepup_6, "Youtube")
data$howkeepup_7 <- characterclean_ones(data$howkeepup_7, "Snapchat")
data$howkeepup_8 <- characterclean_ones(data$howkeepup_8, "Pinterest")
data$howkeepup_9 <- characterclean_ones(data$howkeepup_9, "Medium")
data$howkeepup_10 <- characterclean_ones(data$howkeepup_10, "Other")

data$fav_follow <- as.numeric(levels(data$fav_follow))[data$fav_follow]
data$fav_follow <-  factor(data$fav_follow,
                             levels = c(1,2,3,4,5,6,7,8,9,10),
                             labels = c("Email", "Twitter", "Tumblr", "Facebook",
                                        "Instagram", "Youtube", "Snapchat", "Pinterest", "Medium", "Other"))


data$hpamerch_bought_1 <- characterclean_ones(data$hpamerch_bought_1, "Yes, through the online store.")
data$hpamerch_bought_2 <- characterclean_ones(data$hpamerch_bought_2, "Yes, at a con.")
data$hpamerch_bought_3 <- characterclean_ones(data$hpamerch_bought_3, "No.")
data$hpamerch_bought_4 <- characterclean_ones(data$hpamerch_bought_4, "Did not know could buy.")

data$ever_donated_1 <- characterclean_ones(data$ever_donated_1, "Yes, through a fundraiser.")
data$ever_donated_2 <- characterclean_ones(data$ever_donated_2, "Yes, through site 1 time donation.")
data$ever_donated_3 <- characterclean_ones(data$ever_donated_3, "Yes, through site recurring donation.")
data$ever_donated_4 <- characterclean_ones(data$ever_donated_4, "No but want to.")
data$ever_donated_5 <- characterclean_ones(data$ever_donated_5, "No, and I don't want to.")
data$ever_donated_6 <- characterclean_ones(data$ever_donated_6, "You take donations?")

data$hasdonated <- 0
data$hasdonated[data$ever_donated_1=="Yes, through a fundraiser."] <- 1
data$hasdonated[data$ever_donated_1=="Yes, through site 1 time donation."] <- 1
data$hasdonated[data$ever_donated_1=="Yes, through site recurring donation."] <- 1

data$encourages_donation_1 <- characterclean_ones(data$encourages_donation_1, "Believing in the cause")
data$encourages_donation_2 <- characterclean_ones(data$encourages_donation_2, "Creating a measureable impact")
data$encourages_donation_3 <- characterclean_ones(data$encourages_donation_3, "I'm a long-time supporter of the HPA and want the organization to do well")
data$encourages_donation_4 <- characterclean_ones(data$encourages_donation_4, "I only donate when I'm buying merch or receiving a perk")
data$encourages_donation_5 <- characterclean_ones(data$encourages_donation_5, "The stuff I receive for donating.")
data$encourages_donation_6 <- characterclean_ones(data$encourages_donation_6, "Tax deducibility of donations.")
data$encourages_donation_7 <- characterclean_ones(data$encourages_donation_7, "Supporting young people's leadership and unconventional advocacy.")
data$encourages_donation_8 <- characterclean_ones(data$encourages_donation_8, "Other")

data$what_stoppeddonate_1 <- characterclean_ones(data$what_stoppeddonate_1, "Didnt know you accepted")
data$what_stoppeddonate_2 <- characterclean_ones(data$what_stoppeddonate_2, "I don't have enough disposible income")
data$what_stoppeddonate_3 <- characterclean_ones(data$what_stoppeddonate_3, "I don't have clear understanding")
data$what_stoppeddonate_4 <- characterclean_ones(data$what_stoppeddonate_4, "No benefits from fundraising appeased")
data$what_stoppeddonate_5 <- characterclean_ones(data$what_stoppeddonate_5, "Not invested enough in cause to donate")
data$what_stoppeddonate_6 <- characterclean_ones(data$what_stoppeddonate_6, "Thought donation better used elsewhere")
data$what_stoppeddonate_7 <- characterclean_ones(data$what_stoppeddonate_7, "Other")