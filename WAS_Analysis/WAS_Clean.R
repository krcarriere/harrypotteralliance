#HPA WAS Survey Analysis 2017-2018

#Welcome! My name is Kevin and I'll be leading you through how to analyze our data.

#First, read our data. 
#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("psych")

setwd("~/Desktop/HPA/WAS/")
#ggplot2 never likes being called. It always wants to be installed. 
library(ggplot2)
library(plyr)

data <- read.csv("data/HPA_Wizard_Activist_Survey__Ready_to_Go_V20.csv")

#The way Qualtrics works, we need to delete the first line. But, the first line will be useful.
#So, we'll make a copy just for reference.
dwline <- data #data-with-line = dwline
data <- data[-1,] #Take data, and within that, remove the first row (see the negative 1), keep all variables.

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

factorclean_fandom <- function(input){
  input <- as.numeric(levels(input)[input])
  input <- factor(input, 
                  levels=c(0,1,2,3),
                  labels = c("I love this 5ever",
                             "I enjoy this normal amount",
                             "I don't enjoy this",
                             "I have no feelings about this"))
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

characterclean_wrock_ones <- function(input, rename){
  input <- as.numeric(levels(input))[input]
  input <- as.character(input)
  input <- plyr::revalue(input, c("0"=rename))
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

#Next we're just going to remove some useless variables.
data <- data[ , !names(data) %in% c("Q112_1", "Q112_2", "Q112_3", "Q112_4",
                                    "Q110_1", "Q110_2", "Q110_3", "Q110_4",
                                    "Q111_1", "Q111_2", "Q111_3", "Q111_4",
                                    "Q113_1", "Q113_2", "Q113_3", "Q113_4",
                            "Q115_1", "Q115_2", "Q115_3", "Q115_4",
                            "Q119_1", "Q119_2", "Q119_3", "Q119_4",
                            "Q120_1", "Q120_2", "Q120_3", "Q120_4",
                            "Q121_1", "Q121_2", "Q121_3", "Q121_4",
                            "Q122_1", "Q122_2", "Q122_3", "Q122_4",
                            "Q123_1", "Q123_2", "Q123_3", "Q123_4",
                            "Q124_1", "Q124_2", "Q124_3", "Q124_4",
                            "Q127_1", "Q127_2", "Q127_3", "Q127_4",
                            "Q128_1", "Q128_2", "Q128_3", "Q128_4",
                            "Q129_1", "Q129_2", "Q129_3", "Q129_4",
                            "Q130_1", "Q130_2", "Q130_3", "Q130_4",
                            "Q131_1", "Q131_2", "Q131_3", "Q131_4",
                            "Q134_1", "Q134_2", "Q134_3", "Q134_4",
                            "Q136_1", "Q136_2", "Q136_3", "Q136_4",
                            "Q137_1", "Q137_2", "Q137_3", "Q137_4",
                            "Q138_1", "Q138_2", "Q138_3", "Q138_4",
                            "V2", "V3", "V4", "V5", "V7", "V10", "Q59",
                            "Q109_1_TEXT", "Q109_2_TEXT", "Q109_3_TEXT",
                            "Q109_4_TEXT", "Q109_5_TEXT", "Q109_6_TEXT",
                            "Q109_7_TEXT", "X", "LocationAccuracy",
                            "favwrockers_7_Rank", "favwrockers_8_Rank",
                            "favwrockers_9_Rank", "favwrockers_10_Rank",
                            "favwrockers_11_Rank", "favwrockers_12_Rank",
                            "favwrockers_13_Rank", "favwrockers_14_Rank",
                            "favwrockers_15_Rank", "favwrockers_16_Rank",
                            "favwrockers_17_Rank", "favwrockers_18_Rank",
                            "favwrockers_19_Rank", "favwrockers_21_Rank",
                            "favwrockers_22_Rank", "favwrockers_23_Rank",
                            "favwrockers_24_Rank", "favwrockers_25_Rank",
                            "favwrockers_26_Rank", "favwrockers_27_Rank",
                            "favwrockers_28_Rank", "favwrockers_29_Rank",
                            "favwrockers_30_Rank", "favwrockers_31_Rank",
                            "favwrockers_32_Rank", "favwrockers_33_Rank",
                            "favwrockers_36_Rank", "favwrockers_42_Rank",
                            "favwrockers_56_Rank", "favwrockers_58_Rank",
                            "favwrockers_59_Rank", "favwrockers_60_Rank",
                            "favwrockers_63_Rank", "favwrockers_68_Rank",
                            "favwrockers_70_Rank", "favwrockers_72_Rank",
                            "favwrockers_74_Rank", "favwrockers_75_Rank",
                            "campaigns_1_Rank", "campaigns_2_Rank", 
                            "campaigns_3_Rank", "campaigns_4_Rank", 
                            "campaigns_5_Rank", "campaigns_6_Rank", 
                            "campaigns_7_Rank", "campaigns_8_Rank", 
                            "campaigns_9_Rank", "campaigns_10_Rank", 
                            "campaigns_11_Rank", "campaigns_12_Rank", 
                            "campaigns_13_Rank", "campaigns_14_Rank", 
                            "campaigns_15_Rank", "campaigns_16_Rank", 
                            "campaigns_17_Rank", "campaigns_18_Rank", 
                            "campaigns_19_Rank", "campaigns_20_Rank", 
                            "campaigns_21_Rank", "campaigns_22_Rank", 
                            "campaigns_23_Rank", "campaigns_24_Rank", 
                            "campaigns_25_Rank", "campaigns_26_Rank", 
                            "campaigns_27_Rank", "campaigns_28_Rank", 
                            "campaigns_29_Rank",
                            "fandom_blocks_1_Rank",  "fandom_blocks_2_Rank", 
                            "fandom_blocks_3_Rank",  "fandom_blocks_4_Rank", 
                            "fandom_blocks_5_Rank",  "fandom_blocks_6_Rank", 
                            "fandom_blocks_7_Rank",  "fandom_blocks_8_Rank", 
                            "fandom_blocks_9_Rank",  "fandom_blocks_10_Rank", 
                            "fandom_blocks_11_Rank",  "fandom_blocks_12_Rank", 
                            "fandom_blocks_13_Rank",  "fandom_blocks_14_Rank", 
                            "fandom_blocks_15_Rank",  "fandom_blocks_16_Rank", 
                            "fandom_blocks_17_Rank",  "fandom_blocks_18_Rank", 
                            "fandom_blocks_19_Rank",  "fandom_blocks_20_Rank", 
                            "fandom_blocks_21_Rank",  "fandom_blocks_22_Rank", 
                            "fandom_blocks_23_Rank",  "fandom_blocks_24_Rank", 
                            "fandom_blocks_25_Rank",  "fandom_blocks_26_Rank", 
                            "fandom_blocks_27_Rank",  "fandom_blocks_28_Rank", 
                            "fandom_blocks_29_Rank",  "fandom_blocks_30_Rank", 
                            "fandom_blocks_31_Rank",  "fandom_blocks_32_Rank", 
                            "fandom_blocks_33_Rank",  "fandom_blocks_34_Rank", 
                            "fandom_blocks_35_Rank",  "fandom_blocks_36_Rank", 
                            "fandom_blocks_37_Rank",  "fandom_blocks_38_Rank"
                            )]

#Okay. So, data is our data. $ means variable. There are a lot of packages in R - functions to do things.
#When I want to call a specific function, I always include the package so R knows which one to draw from.
#(see plyr::revalue) while others might just write (revalue). If you need a package, it's:
#install.packages("plyr") and then if you don't want to have to constantly call it, it's "library(plyr)".

data$all_houses_1 <- characterclean_ones(data$all_houses_1, "Gryffindor")
data$all_houses_2 <- characterclean_ones(data$all_houses_2, "Hufflepuff")
data$all_houses_3 <- characterclean_ones(data$all_houses_3, "Ravenclaw")
data$all_houses_4 <- characterclean_ones(data$all_houses_4, "Slytherin")
data$all_houses_5 <- characterclean_ones(data$all_houses_5, "I don't know")

data$pick_house <- factorclean(data$pick_house)
data$pick_house <- plyr::revalue(data$pick_house, c("1"="Gryffindor",
                                                    "2"="Hufflepuff",
                                                    "3"="Ravenclaw",
                                                    "4"="Slytherin"))

data$country <- plyr::revalue(data$country, c("1"="Afghanistan", "2"="Albania", "3"="Algeria",
                                              "4"="Andorra", "5"="Angola", "6"="Anguilla",
                                              "7"="Antigua & Barbuda", "8"="Argentina", "9"="Armenia", 
                                              "10"="Australia", "11"="Austria", "12"="Azerbaijan",
                                              "13"="Bahamas", "14"="Bahrain", "15"="Bangladesh",
                                              "16"="Barbados", "17"="Belarus", "18"="Belgium",
                                              "19"="Belize", "20"="Benin", "21"="Bermuda",
                                              "22"="Bhutan", "23"="Bolivia", "24"="Bosnia & Herzegovina",
                                              "25"="Botswana", "26"="Brazil", "27"="Brunei Darussalam",
                                              "28"="Bulgaria", "29"="Burkina Faso", "30"= "Mynamar/Burma",
                                              "31"="Burundi", "32"="Cambodia", "33"="Cameroon",
                                              "34"="Canada", "35"="Cape Verde", "36"="Cayman Islands",
                                              "37"="Central African Republic", "38"="Chad", "39"="Chile",
                                              "40"="China", "41"="Columbia", "42"="Comoros",
                                              "43"="Congo", "44"="Costa Rica", "45"="Croatia",
                                              "46"="Cuba", "47"="Cyprus", "48"="Czech Republic",
                                              "49"="Democratic Republic of the Congo", "50"="Denmark", "51"="Djibouti",
                                              "52"="Dominican Republic", "53"="Domninica" , "54"="Ecuador",
                                              "55"="Egypt", "56"="El Salvador", "57"="Equatorial Guinea", 
                                              "58"="Eritrea", "59"="Estonia", "60"="Ethiopia",
                                              "61"="Fiji", "62"="Finland", "63"="France",
                                              "64"="French Guiana", "65"="Gabon", "66"="Gambia",
                                              "67"= "Georgia", "68"="Germany", "69"="Ghana",
                                              "70"= "Great Britain", "71"="Greece", "72"="Grenada", 
                                              "73"= "Guadeloupe","74"="Guatemala", "75"="Guinea",
                                              "76"="Guinea-Bissau", "77"="Guyana", "78"="Haiti",
                                              "79"= "Honduras", "80"="Hungary", "81"="Iceland",
                                              "82"= "India", "83"="Indonesia", "84"="Iran",
                                              "85"= "Iraq", "86"="Israel and the Occupied Territories", "87"="Italy",
                                              "88"= "Ivory Coast", "89"="Jamaica", "90"="Japan",
                                              "91"= "Jordan", "92"="Kazakhstan", "93"="Kenya", 
                                              "94"= "Kosovo", "95"="Kuwait", "96"="Kyrgyz Republic",
                                              "97"= "Laos", "98"="Latvia", "99"="Lebanon", 
                                              "100"= "Lesotho", "101"="Liberia", "102"="Libya",
                                              "103"= "Liechtenstein", "104"="Lithuania", "105"="Luxemboourg",
                                              "106"="Republic of Macedonia", "107"="Madagascar", "108"="Malawi",
                                              "109"= "Malaysia", "110"="Maldives", "111"="Mali",
                                              "112"= "Malta", "113"="Martinique", "114"="Maurtania", 
                                              "115"= "Mauritius", "116"="Mayotte", "117"="Mexico", 
                                              "118"="Moldova, Republic of", "119"="Monaco", "120"="Mongolia", 
                                              "121"="Montenegro", "122"="Montserrat", "123"="Morocco",
                                              "124"="Mozambique", "125"="Namibia", "126"="Nepal",
                                              "127"= "Netherlands", "128"="New Zealand", "129"="Nicaragua",
                                              "130"= "Niger", "131"="Nigeria", "132"="Korea, Democractic Republic of North Korea",
                                              "133"= "Norway", "134"="Oman", "135"="Pacific Islands", 
                                              "136"= "Pakistan", "137"="Panama", "138"="Papua New Guinea",
                                              "139"="Paraguay", "140"= "Peru", "141"="Philippines", 
                                              "142"="Poland", "143"= "Portugal", "144"="Puerto Rico", 
                                              "145"="Qatar", "146"="Reunion", "147"="Romania", 
                                              "148"="Russian Federation",  "149"= "Rwanda", "150"="Saint Kitts and Nevis",
                                              "151"="Saint Lucia", "152"= "Saint Vicents and Grenadines", "153"="Samoa",
                                              "154"="Sao Tome and Principe","155"= "Saudi Arabia", "156"="Senegal",
                                              "157"="Serbia", "158"= "Seychelles", "159"="Sierra Leone",
                                              "160"="Singapore", "161"="Slovak Republic", "162"="Slovenia",
                                              "163"="Soloman Islands", "164"="Somalia", "165"="South Africa",
                                              "166"="South Korea", "167"="South Sudan", "168"="Spain", 
                                              "169"="Sri Lanka", "170"="Sudan", "171"="Suriname",
                                              "172"="Swaziland", "173"="Sweden", "174"="Switzerland",
                                              "175"="Syria", "176"="Tajikistan", "177"="Tanzania", 
                                              "178"="Thailand", "179"="Timor Leste", "180"="Togo", 
                                              "181"="Trinidad & Tobago", "182"="Tunisia", "183"="Turkey",
                                              "184"="Turkmenistan", "185"="Turks and Caicos Islands", "186"="Uganda", 
                                              "187"="Ukraine", "188"="United Arab Emirates",  "189"="USA",
                                              "190"="Uruguay", "191"="Uzbekistan",  "192"="Venezeula",
                                              "193"="Vietnam", "194"="Virgin Islands UK",  "195"="Virgin Islands US",
                                              "196"="Yemen", "197"="Zambia", "198"="Zimbabwe"
))

data$country <- factor(data$country)
data$country[data$country==""] <- NA

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

data$international_wizard[is.na(data$state)] <- 1
data$international_wizard[is.na(data$international_wizard)] <- 0

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

data$race_1 <- characterclean_ones(data$race_1, "Arab")
data$race_2 <- characterclean_ones(data$race_2, "Asian")
data$race_3 <- characterclean_ones(data$race_3, "Black")
data$race_4 <- characterclean_ones(data$race_4, "Hispanic")
data$race_5 <- characterclean_ones(data$race_5, "Latino")
data$race_6 <- characterclean_ones(data$race_6, "Multi-Racial")
data$race_7 <- characterclean_ones(data$race_7, "Pacific Islander")
data$race_8 <- characterclean_ones(data$race_8, "Other")
data$race_9 <- characterclean_ones(data$race_9, "White")

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

data$employed <- characterclean_two(data$employed, "Yes", "No")

data$newest_sweatshirt <- characterclean_three(data$newest_sweatshirt, "My own money", "Parents no Expectations", "Parents w Expectations")
data$order_food <-  characterclean_three(data$order_food, "My own money","Parents no Expectation", "Parents w Expectation")

data$selfsufficient <- 0
data$selfsufficient[data$order_food=="My own money" & data$newest_sweatshirt=="My own money"] <- 1

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

data$fandom_the100 <- factorclean_fandom(data$fandom_blocks_1_Group)
data$fandom_boardgames <- factorclean_fandom(data$fandom_blocks_2_Group)
data$fandom_books <- factorclean_fandom(data$fandom_blocks_3_Group)
data$fandom_broadway <- factorclean_fandom(data$fandom_blocks_4_Group)
data$fandom_buffy <- factorclean_fandom(data$fandom_blocks_5_Group)
data$fandom_dcuniverse <- factorclean_fandom(data$fandom_blocks_6_Group)
data$fandom_disney <- factorclean_fandom(data$fandom_blocks_7_Group)
data$fandom_doctorwho <- factorclean_fandom(data$fandom_blocks_8_Group)
data$fandom_gameofthrones <- factorclean_fandom(data$fandom_blocks_9_Group)
data$fandom_glee <- factorclean_fandom(data$fandom_blocks_10_Group)
data$fandom_harrypotter <- factorclean_fandom(data$fandom_blocks_11_Group)
data$fandom_hungergames <- factorclean_fandom(data$fandom_blocks_12_Group)
data$fandom_lotr <- factorclean_fandom(data$fandom_blocks_13_Group)
data$fandom_themagicians <- factorclean_fandom(data$fandom_blocks_14_Group)
data$fandom_marvel <- factorclean_fandom(data$fandom_blocks_15_Group)
data$fandom_music <- factorclean_fandom(data$fandom_blocks_16_Group)
data$fandom_nerdfighteria <- factorclean_fandom(data$fandom_blocks_17_Group)
data$fandom_theoffice <- factorclean_fandom(data$fandom_blocks_18_Group)
data$fandom_onceuponatime <- factorclean_fandom(data$fandom_blocks_19_Group)
data$fandom_parksandrec <- factorclean_fandom(data$fandom_blocks_20_Group)
data$fandom_percyjackson <- factorclean_fandom(data$fandom_blocks_21_Group)
data$fandom_politics <- factorclean_fandom(data$fandom_blocks_22_Group)
data$fandom_sherlock <- factorclean_fandom(data$fandom_blocks_23_Group)
data$fandom_snitchwiches <- factorclean_fandom(data$fandom_blocks_24_Group)
data$fandom_starkid <- factorclean_fandom(data$fandom_blocks_25_Group)
data$fandom_startrek <- factorclean_fandom(data$fandom_blocks_26_Group)
data$fandom_starwars <- factorclean_fandom(data$fandom_blocks_27_Group)
data$fandom_stevenuni <- factorclean_fandom(data$fandom_blocks_28_Group)
data$fandom_supernatural <- factorclean_fandom(data$fandom_blocks_29_Group)
data$fandom_veronicamars <- factorclean_fandom(data$fandom_blocks_30_Group)
data$fandom_videogames <- factorclean_fandom(data$fandom_blocks_31_Group)
data$fandom_vlogbrothers <- factorclean_fandom(data$fandom_blocks_32_Group)
data$fandom_walkingdead <- factorclean_fandom(data$fandom_blocks_33_Group)
data$fandom_welcomingtonightvale <- factorclean_fandom(data$fandom_blocks_34_Group)
data$fandom_thewestwing <- factorclean_fandom(data$fandom_blocks_35_Group)
data$fandom_whendonverse <- factorclean_fandom(data$fandom_blocks_36_Group)
data$fandom_yabooks <- factorclean_fandom(data$fandom_blocks_37_Group)
data$fandom_youtube <- factorclean_fandom(data$fandom_blocks_38_Group)

data$listen_wrock <- characterclean_six(data$listen_wrock, "Yes I do", "Sometimes!", "Not yet but want to", "I used to, but not anymore", "No I do not", "Casually")

data$favwrockers_7_Group <- characterclean_wrock_ones(data$favwrockers_7_Group, "The Blibbering Humdingers")
data$favwrockers_8_Group <- characterclean_wrock_ones(data$favwrockers_8_Group, "Gred and Forge")
data$favwrockers_9_Group <- characterclean_wrock_ones(data$favwrockers_9_Group, "The Butterbeer Experience/Lena Gabrielle")
data$favwrockers_10_Group <- characterclean_wrock_ones(data$favwrockers_10_Group, "Catchlove")
data$favwrockers_11_Group <- characterclean_wrock_ones(data$favwrockers_11_Group, "Oliver Boyd and the Remembralls")
data$favwrockers_12_Group <- characterclean_wrock_ones(data$favwrockers_12_Group, "The Chocolate Frogs")
data$favwrockers_13_Group <- characterclean_wrock_ones(data$favwrockers_13_Group, "Creevey Crisis")
data$favwrockers_14_Group <- characterclean_wrock_ones(data$favwrockers_14_Group, "Neville's Diary")
data$favwrockers_15_Group <- characterclean_wrock_ones(data$favwrockers_15_Group, "Diagon Alley")
data$favwrockers_16_Group <- characterclean_wrock_ones(data$favwrockers_16_Group, "Ginny and the Heartbreakers")
data$favwrockers_17_Group <- characterclean_wrock_ones(data$favwrockers_17_Group, "Hawthorn and Holly")
data$favwrockers_18_Group <- characterclean_wrock_ones(data$favwrockers_18_Group, "Draco and the Malfoys")
data$favwrockers_19_Group <- characterclean_wrock_ones(data$favwrockers_19_Group, "How Airplanes Fly")
data$favwrockers_21_Group <- characterclean_wrock_ones(data$favwrockers_21_Group, "The Mudbloods")
data$favwrockers_22_Group <- characterclean_wrock_ones(data$favwrockers_22_Group, "Quaffle Kids")
data$favwrockers_23_Group <- characterclean_wrock_ones(data$favwrockers_23_Group, "The Parselmouths")
data$favwrockers_24_Group <- characterclean_wrock_ones(data$favwrockers_24_Group, "Gryffindor Common Room Rejects")
data$favwrockers_25_Group <- characterclean_wrock_ones(data$favwrockers_25_Group, "Harry and the Potters")
data$favwrockers_26_Group <- characterclean_wrock_ones(data$favwrockers_26_Group, "Philosopherock")
data$favwrockers_27_Group <- characterclean_wrock_ones(data$favwrockers_27_Group, "The Hermione Crookshanks Experience")
data$favwrockers_28_Group <- characterclean_wrock_ones(data$favwrockers_28_Group, "Romilda Vane and the Chocolate Cauldrons")
data$favwrockers_29_Group <- characterclean_wrock_ones(data$favwrockers_29_Group, "Seen and Unforeseen")
data$favwrockers_30_Group <- characterclean_wrock_ones(data$favwrockers_30_Group, "Witherwings")
data$favwrockers_31_Group <- characterclean_wrock_ones(data$favwrockers_31_Group, "The House of Black")
data$favwrockers_32_Group <- characterclean_wrock_ones(data$favwrockers_32_Group, "The Hungarian Horntails")
data$favwrockers_33_Group <- characterclean_wrock_ones(data$favwrockers_33_Group, "Justin Finch-Fletchley and the Sugar Quills")
data$favwrockers_36_Group <- characterclean_wrock_ones(data$favwrockers_36_Group, "Lauren Fairweather")
data$favwrockers_42_Group <- characterclean_wrock_ones(data$favwrockers_42_Group, "The Moaning Mrytles")
data$favwrockers_56_Group <- characterclean_wrock_ones(data$favwrockers_56_Group, "Remus and the Lupins")
data$favwrockers_58_Group <- characterclean_wrock_ones(data$favwrockers_58_Group, "RiddleTM")
data$favwrockers_59_Group <- characterclean_wrock_ones(data$favwrockers_59_Group, "Tianna and the Cliffhangers")
data$favwrockers_60_Group <- characterclean_wrock_ones(data$favwrockers_60_Group, "Roonil Wazlib")
data$favwrockers_63_Group <- characterclean_wrock_ones(data$favwrockers_63_Group, "Siriusly Hazza P")
data$favwrockers_68_Group <- characterclean_wrock_ones(data$favwrockers_68_Group, "Swish and Flick")
data$favwrockers_70_Group <- characterclean_wrock_ones(data$favwrockers_70_Group, "Tonks and the Aurors")
data$favwrockers_72_Group <- characterclean_wrock_ones(data$favwrockers_72_Group, "The Whomping Willows")
data$favwrockers_74_Group <- characterclean_wrock_ones(data$favwrockers_74_Group, "Totally Forgot X")
data$favwrockers_75_Group <- characterclean_wrock_ones(data$favwrockers_75_Group, "Totally Forgot X2")

data$length_active_hpa <- as.numeric(levels(data$active_hpa))[data$active_hpa]

data$fandom_events_1 <- characterclean_ones(data$fandom_events_1, "Granger Leadership Academy")
data$fandom_events_2 <- characterclean_ones(data$fandom_events_2, "LeakyCon")
data$fandom_events_3 <- characterclean_ones(data$fandom_events_3, "GeekyCon")
data$fandom_events_4 <- characterclean_ones(data$fandom_events_4, "HPEF")
data$fandom_events_5 <- characterclean_ones(data$fandom_events_5, "Terminus")
data$fandom_events_6 <- characterclean_ones(data$fandom_events_6, "BroadwayCon")
data$fandom_events_7 <- characterclean_ones(data$fandom_events_7, "VidCon")
data$fandom_events_8 <- characterclean_ones(data$fandom_events_8, "NerdCon: Stories")
data$fandom_events_9 <- characterclean_ones(data$fandom_events_9, "NerdCon: Nerdfighteria")
data$fandom_events_10 <- characterclean_ones(data$fandom_events_10, "The Yule Ball")
data$fandom_events_11 <- characterclean_ones(data$fandom_events_11, "The Quidditch World Cup")
data$fandom_events_12 <- characterclean_ones(data$fandom_events_12, "MISTI Con")
data$fandom_events_13 <- characterclean_ones(data$fandom_events_13, "NYCC")
data$fandom_events_14 <- characterclean_ones(data$fandom_events_14, "SDCC")
data$fandom_events_15 <- characterclean_ones(data$fandom_events_15, "Geek Girl Con")
data$fandom_events_16 <- characterclean_ones(data$fandom_events_16, "Fill in 1")
data$fandom_events_17 <- characterclean_ones(data$fandom_events_17, "Fill in 2")
data$fandom_events_18 <- characterclean_ones(data$fandom_events_18, "Fill in 3")
data$fandom_events_19 <- characterclean_ones(data$fandom_events_19, "Fill in 4")
data$fandom_events_20 <- characterclean_ones(data$fandom_events_20, "Fill in 5")
data$fandom_events_21 <- characterclean_ones(data$fandom_events_21, "Never Attended")

#This counts the number of events selected.
#Step One: Create new Data Frame of Events
data_fandom_events<- data[ , names(data) %in% c("fandom_events_1",  "fandom_events_2", 
                                               "fandom_events_3",  "fandom_events_4", 
                                               "fandom_events_5",  "fandom_events_6", 
                                               "fandom_events_7",  "fandom_events_8", 
                                               "fandom_events_9",  "fandom_events_10", 
                                               "fandom_events_11",  "fandom_events_12", 
                                               "fandom_events_13",  "fandom_events_14", 
                                               "fandom_events_15",  "fandom_events_16", 
                                               "fandom_events_17",  "fandom_events_18", 
                                               "fandom_events_19",  "fandom_events_20", 
                                               "fandom_events_21"
                                               )]

#Step Two: Count all Non-Missing Entries
data_fandom_events$test <- apply(data_fandom_events, 1, function(x) sum(!is.na(x)))
#Add as variable
data$num_fanevents <- data_fandom_events$test
#Remove test dataset
rm(data_fandom_events)

data$event_times.x1. <- as.numeric(levels(data$event_times.x1.))[data$event_times.x1.]
data$event_times.x2. <- as.numeric(levels(data$event_times.x2.))[data$event_times.x2.]
data$event_times.x3. <- as.numeric(levels(data$event_times.x3.))[data$event_times.x3.]
data$event_times.x4. <- as.numeric(levels(data$event_times.x4.))[data$event_times.x4.]
data$event_times.x5. <- as.numeric(levels(data$event_times.x5.))[data$event_times.x5.]
data$event_times.x6. <- as.numeric(levels(data$event_times.x6.))[data$event_times.x6.]
data$event_times.x7. <- as.numeric(levels(data$event_times.x7.))[data$event_times.x7.]
data$event_times.x8. <- as.numeric(levels(data$event_times.x8.))[data$event_times.x8.]
data$event_times.x9. <- as.numeric(levels(data$event_times.x9.))[data$event_times.x9.]
data$event_times.x10. <- as.numeric(levels(data$event_times.x10.))[data$event_times.x10.]
data$event_times.x11. <- as.numeric(levels(data$event_times.x11.))[data$event_times.x11.]
data$event_times.x12. <- as.numeric(levels(data$event_times.x12.))[data$event_times.x12.]
data$event_times.x13. <- as.numeric(levels(data$event_times.x13.))[data$event_times.x13.]
data$event_times.x14. <- as.numeric(levels(data$event_times.x14.))[data$event_times.x14.]
data$event_times.x15. <- as.numeric(levels(data$event_times.x15.))[data$event_times.x15.]
data$event_times.x16. <- as.numeric(levels(data$event_times.x16.))[data$event_times.x16.]
data$event_times.x17. <- as.numeric(levels(data$event_times.x17.))[data$event_times.x17.]
data$event_times.x18. <- as.numeric(levels(data$event_times.x18.))[data$event_times.x18.]
data$event_times.x19. <- as.numeric(levels(data$event_times.x19.))[data$event_times.x19.]
data$event_times.x20. <- as.numeric(levels(data$event_times.x20.))[data$event_times.x20.]
data$event_times.x21. <- as.numeric(levels(data$event_times.x21.))[data$event_times.x21.]

datatest<- data[ , names(data) %in% c("event_times.x1.",  "event_times.x2.", 
                                      "event_times.x3.",  "event_times.x4.", 
                                      "event_times.x5.",  "event_times.x6.", 
                                      "event_times.x7.",  "event_times.x8.", 
                                      "event_times.x9.",  "event_times.x10.", 
                                      "event_times.x11.",  "event_times.x12.", 
                                      "event_times.x13.",  "event_times.x14.", 
                                      "event_times.x15.",  "event_times.x16.", 
                                      "event_times.x17.",  "event_times.x18.", 
                                      "event_times.x19.",  "event_times.x20.", 
                                      "event_times.x21."
                                      )]

datatest$test <- rowSums(datatest, na.rm=TRUE)
data$timesattendedfav <- datatest$test
rm(datatest)

data$event_firstyear.x1. <- as.numeric(levels(data$event_firstyear.x1.))[data$event_firstyear.x1.]
data$event_firstyear.x2. <- as.numeric(levels(data$event_firstyear.x2.))[data$event_firstyear.x2.]
data$event_firstyear.x3. <- as.numeric(levels(data$event_firstyear.x3.))[data$event_firstyear.x3.]
data$event_firstyear.x4. <- as.numeric(levels(data$event_firstyear.x4.))[data$event_firstyear.x4.]
data$event_firstyear.x5. <- as.numeric(levels(data$event_firstyear.x5.))[data$event_firstyear.x5.]
data$event_firstyear.x6. <- as.numeric(levels(data$event_firstyear.x6.))[data$event_firstyear.x6.]
data$event_firstyear.x7. <- as.numeric(levels(data$event_firstyear.x7.))[data$event_firstyear.x7.]
data$event_firstyear.x8. <- as.numeric(levels(data$event_firstyear.x8.))[data$event_firstyear.x8.]
data$event_firstyear.x9. <- as.numeric(levels(data$event_firstyear.x9.))[data$event_firstyear.x9.]
data$event_firstyear.x10. <- as.numeric(levels(data$event_firstyear.x10.))[data$event_firstyear.x10.]
data$event_firstyear.x11. <- as.numeric(levels(data$event_firstyear.x11.))[data$event_firstyear.x11.]
data$event_firstyear.x12. <- as.numeric(levels(data$event_firstyear.x12.))[data$event_firstyear.x12.]
data$event_firstyear.x13. <- as.numeric(levels(data$event_firstyear.x13.))[data$event_firstyear.x13.]
data$event_firstyear.x14. <- as.numeric(levels(data$event_firstyear.x14.))[data$event_firstyear.x14.]
data$event_firstyear.x15. <- as.numeric(levels(data$event_firstyear.x15.))[data$event_firstyear.x15.]
data$event_firstyear.x16. <- as.numeric(levels(data$event_firstyear.x16.))[data$event_firstyear.x16.]
data$event_firstyear.x17. <- as.numeric(levels(data$event_firstyear.x17.))[data$event_firstyear.x17.]
data$event_firstyear.x18. <- as.numeric(levels(data$event_firstyear.x18.))[data$event_firstyear.x18.]
data$event_firstyear.x19. <- as.numeric(levels(data$event_firstyear.x19.))[data$event_firstyear.x19.]
data$event_firstyear.x20. <- as.numeric(levels(data$event_firstyear.x20.))[data$event_firstyear.x20.]
data$event_firstyear.x21. <- as.numeric(levels(data$event_firstyear.x21.))[data$event_firstyear.x21.]

datatest<- data[ , names(data) %in% c("event_firstyear.x1.",  "event_firstyear.x2.", 
                                      "event_firstyear.x3.",  "event_firstyear.x4.", 
                                      "event_firstyear.x5.",  "event_firstyear.x6.", 
                                      "event_firstyear.x7.",  "event_firstyear.x8.", 
                                      "event_firstyear.x9.",  "event_firstyear.x10.", 
                                      "event_firstyear.x11.",  "event_firstyear.x12.", 
                                      "event_firstyear.x13.",  "event_firstyear.x14.", 
                                      "event_firstyear.x15.",  "event_firstyear.x16.", 
                                      "event_firstyear.x17.",  "event_firstyear.x18.", 
                                      "event_firstyear.x19.",  "event_firstyear.x20.", 
                                      "event_firstyear.x21."
)]

datatest$test <- rowSums(datatest, na.rm=TRUE)
data$firstyear <- datatest$test
rm(datatest)

data$event_lastyear.x1. <- as.numeric(levels(data$event_lastyear.x1.))[data$event_lastyear.x1.]
data$event_lastyear.x2. <- as.numeric(levels(data$event_lastyear.x2.))[data$event_lastyear.x2.]
data$event_lastyear.x3. <- as.numeric(levels(data$event_lastyear.x3.))[data$event_lastyear.x3.]
data$event_lastyear.x4. <- as.numeric(levels(data$event_lastyear.x4.))[data$event_lastyear.x4.]
data$event_lastyear.x5. <- as.numeric(levels(data$event_lastyear.x5.))[data$event_lastyear.x5.]
data$event_lastyear.x6. <- as.numeric(levels(data$event_lastyear.x6.))[data$event_lastyear.x6.]
data$event_lastyear.x7. <- as.numeric(levels(data$event_lastyear.x7.))[data$event_lastyear.x7.]
data$event_lastyear.x8. <- as.numeric(levels(data$event_lastyear.x8.))[data$event_lastyear.x8.]
data$event_lastyear.x9. <- as.numeric(levels(data$event_lastyear.x9.))[data$event_lastyear.x9.]
data$event_lastyear.x10. <- as.numeric(levels(data$event_lastyear.x10.))[data$event_lastyear.x10.]
data$event_lastyear.x11. <- as.numeric(levels(data$event_lastyear.x11.))[data$event_lastyear.x11.]
data$event_lastyear.x12. <- as.numeric(levels(data$event_lastyear.x12.))[data$event_lastyear.x12.]
data$event_lastyear.x13. <- as.numeric(levels(data$event_lastyear.x13.))[data$event_lastyear.x13.]
data$event_lastyear.x14. <- as.numeric(levels(data$event_lastyear.x14.))[data$event_lastyear.x14.]
data$event_lastyear.x15. <- as.numeric(levels(data$event_lastyear.x15.))[data$event_lastyear.x15.]
data$event_lastyear.x16. <- as.numeric(levels(data$event_lastyear.x16.))[data$event_lastyear.x16.]
data$event_lastyear.x17. <- as.numeric(levels(data$event_lastyear.x17.))[data$event_lastyear.x17.]
data$event_lastyear.x18. <- as.numeric(levels(data$event_lastyear.x18.))[data$event_lastyear.x18.]
data$event_lastyear.x19. <- as.numeric(levels(data$event_lastyear.x19.))[data$event_lastyear.x19.]
data$event_lastyear.x20. <- as.numeric(levels(data$event_lastyear.x20.))[data$event_lastyear.x20.]
data$event_lastyear.x21. <- as.numeric(levels(data$event_lastyear.x21.))[data$event_lastyear.x21.]

datatest<- data[ , names(data) %in% c("event_lastyear.x1.",  "event_lastyear.x2.", 
                                      "event_lastyear.x3.",  "event_lastyear.x4.", 
                                      "event_lastyear.x5.",  "event_lastyear.x6.", 
                                      "event_lastyear.x7.",  "event_lastyear.x8.", 
                                      "event_lastyear.x9.",  "event_lastyear.x10.", 
                                      "event_lastyear.x11.",  "event_lastyear.x12.", 
                                      "event_lastyear.x13.",  "event_lastyear.x14.", 
                                      "event_lastyear.x15.",  "event_lastyear.x16.", 
                                      "event_lastyear.x17.",  "event_lastyear.x18.", 
                                      "event_lastyear.x19.",  "event_lastyear.x20.", 
                                      "event_lastyear.x21."
)]

datatest$test <- rowSums(datatest, na.rm=TRUE)
data$lastyear <- datatest$test
rm(datatest)


data <- data[ , !names(data) %in% c("fandom_blocks_1_Group",  "fandom_blocks_2_Group", 
                                    "fandom_blocks_3_Group",  "fandom_blocks_4_Group", 
                                    "fandom_blocks_5_Group",  "fandom_blocks_6_Group", 
                                    "fandom_blocks_7_Group",  "fandom_blocks_8_Group", 
                                    "fandom_blocks_9_Group",  "fandom_blocks_10_Group", 
                                    "fandom_blocks_11_Group",  "fandom_blocks_12_Group", 
                                    "fandom_blocks_13_Group",  "fandom_blocks_14_Group", 
                                    "fandom_blocks_15_Group",  "fandom_blocks_16_Group", 
                                    "fandom_blocks_17_Group",  "fandom_blocks_18_Group", 
                                    "fandom_blocks_19_Group",  "fandom_blocks_20_Group", 
                                    "fandom_blocks_21_Group",  "fandom_blocks_22_Group", 
                                    "fandom_blocks_23_Group",  "fandom_blocks_24_Group", 
                                    "fandom_blocks_25_Group",  "fandom_blocks_26_Group", 
                                    "fandom_blocks_27_Group",  "fandom_blocks_28_Group", 
                                    "fandom_blocks_29_Group",  "fandom_blocks_30_Group", 
                                    "fandom_blocks_31_Group",  "fandom_blocks_32_Group", 
                                    "fandom_blocks_33_Group",  "fandom_blocks_34_Group", 
                                    "fandom_blocks_35_Group",  "fandom_blocks_36_Group", 
                                    "fandom_blocks_37_Group",  "fandom_blocks_38_Group",
                                    "event_times.x1.",  "event_times.x2.", 
                                    "event_times.x3.",  "event_times.x4.", 
                                    "event_times.x5.",  "event_times.x6.", 
                                    "event_times.x7.",  "event_times.x8.", 
                                    "event_times.x9.",  "event_times.x10.", 
                                    "event_times.x11.",  "event_times.x12.", 
                                    "event_times.x13.",  "event_times.x14.", 
                                    "event_times.x15.",  "event_times.x16.", 
                                    "event_times.x17.",  "event_times.x18.", 
                                    "event_times.x19.",  "event_times.x20.", 
                                    "event_times.x21.", 
                                    "event_lastyear.x1.",  "event_lastyear.x2.", 
                                    "event_lastyear.x3.",  "event_lastyear.x4.", 
                                    "event_lastyear.x5.",  "event_lastyear.x6.", 
                                    "event_lastyear.x7.",  "event_lastyear.x8.", 
                                    "event_lastyear.x9.",  "event_lastyear.x10.", 
                                    "event_lastyear.x11.",  "event_lastyear.x12.", 
                                    "event_lastyear.x13.",  "event_lastyear.x14.", 
                                    "event_lastyear.x15.",  "event_lastyear.x16.", 
                                    "event_lastyear.x17.",  "event_lastyear.x18.", 
                                    "event_lastyear.x19.",  "event_lastyear.x20.", 
                                    "event_lastyear.x21.",
                                    "event_firstyear.x1.",  "event_firstyear.x2.", 
                                    "event_firstyear.x3.",  "event_firstyear.x4.", 
                                    "event_firstyear.x5.",  "event_firstyear.x6.", 
                                    "event_firstyear.x7.",  "event_firstyear.x8.", 
                                    "event_firstyear.x9.",  "event_firstyear.x10.", 
                                    "event_firstyear.x11.",  "event_firstyear.x12.", 
                                    "event_firstyear.x13.",  "event_firstyear.x14.", 
                                    "event_firstyear.x15.",  "event_firstyear.x16.", 
                                    "event_firstyear.x17.",  "event_firstyear.x18.", 
                                    "event_firstyear.x19.",  "event_firstyear.x20.", 
                                    "event_firstyear.x21."
)]


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

data$chapters_1 <- characterclean_ones(data$chapters_1,"Yes, as member")
data$chapters_2 <- characterclean_ones(data$chapters_2,"Yes, as CO")
data$chapters_3 <- characterclean_ones(data$chapters_3,"No, but I used to be")
data$chapters_4 <- characterclean_ones(data$chapters_4,"No")
data$chapters_5 <- characterclean_ones(data$chapters_5,"What's a chapter?")

data$chapters <- 0
data$chapters[data$chapters_1=="Yes, as member"] <- 1
data$chapters[data$chapters_2=="Yes, as CO"] <- 1
data$chapters <- as.factor(data$chapters)
data$chapters <- factor(data$chapters, levels = c(0,1), labels = c("No", "Yes"))

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
#Boycotted
data$wactivist_scale_17 <- as.numeric(levels(data$wactivist_scale_17))[data$wactivist_scale_17]

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

data$wactivist_s_boycottnew <- factor(data$wactivist_scale_17,
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

data$getinvolved_1 <- characterclean_ones(data$getinvolved_1, "Spread Word")
data$getinvolved_2 <- characterclean_ones(data$getinvolved_2, "Keep Up Online")
data$getinvolved_3 <- characterclean_ones(data$getinvolved_3, "Participate Campaigns")
data$getinvolved_4 <- characterclean_ones(data$getinvolved_4, "Buy HPA Merch")
data$getinvolved_5 <- characterclean_ones(data$getinvolved_5, "Contribute HPA Fundraisers")
data$getinvolved_6 <- characterclean_ones(data$getinvolved_6, "HPA Volunteer Staff")
data$getinvolved_7 <- characterclean_ones(data$getinvolved_7, "HPA Chapter Member")
data$getinvolved_8 <- characterclean_ones(data$getinvolved_8, "HPA Chapter CO")
data$getinvolved_9 <- characterclean_ones(data$getinvolved_9, "GLA Attend")
data$getinvolved_10 <- characterclean_ones(data$getinvolved_10, "Other")

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
data$campaign_worldwithouthermione <- factorclean_campaign(data$campaigns_29_Group)

data <- data[ , !names(data) %in% c("campaigns_1_Group",  "campaigns_2_Group", 
                                    "campaigns_3_Group",  "campaigns_4_Group", 
                                    "campaigns_5_Group",  "campaigns_6_Group", 
                                    "campaigns_7_Group",  "campaigns_8_Group", 
                                    "campaigns_9_Group",  "campaigns_10_Group", 
                                    "campaigns_11_Group",  "campaigns_12_Group", 
                                    "campaigns_13_Group",  "campaigns_14_Group", 
                                    "campaigns_15_Group",  "campaigns_16_Group", 
                                    "campaigns_17_Group",  "campaigns_18_Group", 
                                    "campaigns_19_Group",  "campaigns_20_Group", 
                                    "campaigns_21_Group",  "campaigns_22_Group", 
                                    "campaigns_23_Group",  "campaigns_24_Group", 
                                    "campaigns_25_Group",  "campaigns_26_Group", 
                                    "campaigns_27_Group",  "campaigns_28_Group", 
                                    "campaigns_29_Group")]

data$fav_campaign <- as.numeric(levels(data$fav_campaign))[data$fav_campaign]
data$fav_campaign <-  factor(data$fav_campaign,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                        20,21, 22,23,24,25,26,27,28,29),
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
                                        "Chapter Event 1", "Chapter Event 2", "World Without Hermione" ))

data$leastfav_campaign <-  factor(data$leastfav_campaign,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,
                                        20,21, 22,23,24,25,26,27,28,29),
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
                                        "Chapter Event 1", "Chapter Event 2", "World Without Hermione" ))

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

names(data)[names(data) == 'activism_rank_x1'] <- 'GenderEqualityRank'
names(data)[names(data) == 'activism_rank_x2'] <- 'RacialEqualityRank'
names(data)[names(data) == 'activism_rank_x3'] <- 'ReligiousEqualityRank'
names(data)[names(data) == 'activism_rank_x4'] <- 'EnvironmentalRank'
names(data)[names(data) == 'activism_rank_x5'] <- 'CleanEnergyRank'
names(data)[names(data) == 'activism_rank_x6'] <- 'NetNeutralityRank'
names(data)[names(data) == 'activism_rank_x7'] <- 'MediaReformRank'
names(data)[names(data) == 'activism_rank_x8'] <- 'FairTradeRank'
names(data)[names(data) == 'activism_rank_x9'] <- 'EducationRank'
names(data)[names(data) == 'activism_rank_x10'] <- 'LibraryAdvocacyRank'
names(data)[names(data) == 'activism_rank_x11'] <- 'LGBTQIARank'
names(data)[names(data) == 'activism_rank_x12'] <- 'FillInRank'
names(data)[names(data) == 'activism_rank_x13'] <- 'FillIn2Rank'
names(data)[names(data) == 'activism_rank_x14'] <- 'FillIn3Rank'

data$topissue[data$GenderEqualityRank==1] <- "Gender Equality"
data$topissue[data$RacialEqualityRank==1] <- "Racial Equality"
data$topissue[data$ReligiousEqualityRank==1] <- "Religious Equality"
data$topissue[data$EnvironmentalRank==1] <- "Environmental"
data$topissue[data$CleanEnergyRank==1] <- "Clean Energy"
data$topissue[data$NetNeutralityRank==1] <- "Net Neutrality"
data$topissue[data$MediaReformRank==1] <- "Media Reform"
data$topissue[data$FairTradeRank==1] <- "Fair Trade"
data$topissue[data$EducationRank==1] <- "Education"
data$topissue[data$LibraryAdvocacyRank==1] <- "Library Advocacy"
data$topissue[data$LGBTQIARank==1] <- "LGBTQIA"
data$topissue[data$FillInRank==1] <- "Fill In 1"
data$topissue[data$Fill2InRank==1] <- "Fill in 2"
data$topissue[data$Fill3InRank==1] <- "Fill in 3"

data$secondissue[data$GenderEqualityRank==2] <- "Gender Equality"
data$secondissue[data$RacialEqualityRank==2] <- "Racial Equality"
data$secondissue[data$ReligiousEqualityRank==2] <- "Religious Equality"
data$secondissue[data$EnvironmentalRank==2] <- "Environmental"
data$secondissue[data$CleanEnergyRank==2] <- "Clean Energy"
data$secondissue[data$NetNeutralityRank==2] <- "Net Neutrality"
data$secondissue[data$MediaReformRank==2] <- "Media Reform"
data$secondissue[data$FairTradeRank==2] <- "Fair Trade"
data$secondissue[data$EducationRank==2] <- "Education"
data$secondissue[data$LibraryAdvocacyRank==2] <- "Library Advocacy"
data$secondissue[data$LGBTQIARank==2] <- "LGBTQIA"
data$secondissue[data$FillInRank==2] <- "Fill In 1"
data$secondissue[data$Fill2InRank==2] <- "Fill in 2"
data$secondissue[data$Fill3InRank==2] <- "Fill in 3"

data$thirdissue[data$GenderEqualityRank==3] <- "Gender Equality"
data$thirdissue[data$RacialEqualityRank==3] <- "Racial Equality"
data$thirdissue[data$ReligiousEqualityRank==3] <- "Religious Equality"
data$thirdissue[data$EnvironmentalRank==3] <- "Environmental"
data$thirdissue[data$CleanEnergyRank==3] <- "Clean Energy"
data$thirdissue[data$NetNeutralityRank==3] <- "Net Neutrality"
data$thirdissue[data$MediaReformRank==3] <- "Media Reform"
data$thirdissue[data$FairTradeRank==3] <- "Fair Trade"
data$thirdissue[data$EducationRank==3] <- "Education"
data$thirdissue[data$LibraryAdvocacyRank==3] <- "Library Advocacy"
data$thirdissue[data$LGBTQIARank==3] <- "LGBTQIA"
data$thirdissue[data$FillInRank==3] <- "Fill In 1"
data$thirdissue[data$Fill2InRank==3] <- "Fill in 2"
data$thirdissue[data$Fill3InRank==3] <- "Fill in 3"

data$fourthissue[data$GenderEqualityRank==4] <- "Gender Equality"
data$fourthissue[data$RacialEqualityRank==4] <- "Racial Equality"
data$fourthissue[data$ReligiousEqualityRank==4] <- "Religious Equality"
data$fourthissue[data$EnvironmentalRank==4] <- "Environmental"
data$fourthissue[data$CleanEnergyRank==4] <- "Clean Energy"
data$fourthissue[data$NetNeutralityRank==4] <- "Net Neutrality"
data$fourthissue[data$MediaReformRank==4] <- "Media Reform"
data$fourthissue[data$FairTradeRank==4] <- "Fair Trade"
data$fourthissue[data$EducationRank==4] <- "Education"
data$fourthissue[data$LibraryAdvocacyRank==4] <- "Library Advocacy"
data$fourthissue[data$LGBTQIARank==4] <- "LGBTQIA"
data$fourthissue[data$FillInRank==4] <- "Fill In 1"
data$fourthissue[data$Fill2InRank==4] <- "Fill in 2"
data$fourthissue[data$Fill3InRank==4] <- "Fill in 3"

data$fifthissue[data$GenderEqualityRank==5] <- "Gender Equality"
data$fifthissue[data$RacialEqualityRank==5] <- "Racial Equality"
data$fifthissue[data$ReligiousEqualityRank==5] <- "Religious Equality"
data$fifthissue[data$EnvironmentalRank==5] <- "Environmental"
data$fifthissue[data$CleanEnergyRank==5] <- "Clean Energy"
data$fifthissue[data$NetNeutralityRank==5] <- "Net Neutrality"
data$fifthissue[data$MediaReformRank==5] <- "Media Reform"
data$fifthissue[data$FairTradeRank==5] <- "Fair Trade"
data$fifthissue[data$EducationRank==5] <- "Education"
data$fifthissue[data$LibraryAdvocacyRank==5] <- "Library Advocacy"
data$fifthissue[data$LGBTQIARank==5] <- "LGBTQIA"
data$fifthissue[data$FillInRank==5] <- "Fill In 1"
data$fifthissue[data$Fill2InRank==5] <- "Fill in 2"
data$fifthissue[data$Fill3InRank==5] <- "Fill in 3"

write.csv(data, "data/CleanedWAS2017.csv")
