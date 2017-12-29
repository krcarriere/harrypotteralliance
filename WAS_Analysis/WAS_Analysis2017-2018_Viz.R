setwd("~/Desktop/S17/HPA/WAS/")
source("scripts/WAS_Clean.R")

#Note: We don't read in the data, like below, because then the scales have to be
#re-ordered in the appropriate factors. Running the whole script over again
#ensures that the data is ordered properly.
#datatwo <- read.csv("data/CleanedWAS2017.csv")
library(ggplot2)

#Let's get some colors going!
gryfcol <- c("#740001", "#ae0001", "#eeba30", "#d3a625", "#000000") #http://www.color-hex.com/color-palette/813
ravecol <- c("#111c40", "#1d2a54", "#986f30", "#b8873d", "#000000") #http://www.color-hex.com/color-palette/3654
huffcol <- c("#eec540", "#d2ad33", "#927a2c", "#58460e", "#000000") #http://www.color-hex.com/color-palette/31845
slytcol <- c("#20472e", "#2d5e3e", "#565555", "#a1a1a1", "#000000") #http://www.color-hex.com/color-palette/3676
hogwcol <- c("#970702", "#e6a915", "#0c1e58", "#083d0f", "#000000") #http://www.color-hex.com/color-palette/20654 with edits

#There might come times where we need more than 5 categories,
#so below we tell R, hey, make up colors that match our palette. What an artist.

extended_gryfcol = colorRampPalette(gryfcol)
extended_ravecol = colorRampPalette(ravecol)
extended_huffcol = colorRampPalette(huffcol)
extended_slytcol = colorRampPalette(slytcol)
extended_hogwcol = colorRampPalette(hogwcol)

histo_age <- ggplot2::ggplot(data=data, aes(x=age)) + 
  geom_histogram(col="black", aes(fill=..count..)) + 
  xlab("Age in Years") + ylab("Frequency") + labs(title="Age of Respondents") +
  scale_fill_gradient("Count", low = gryfcol[3], high = gryfcol[1])

histo_edyr <- ggplot2::ggplot(data=data, aes(x=years_ed)) + 
  geom_histogram(col="black", aes(fill=..count..)) + 
  xlab("Years of Education") + ylab("Frequency") + labs(title="Years of Education") +
  scale_fill_gradient("Count", low = ravecol[4], high = ravecol[1])

histo_scale1 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_comfort)), aes(x=wactivist_s_comfort)) + 
  geom_bar(col="black", stat="count", fill=extended_ravecol(7)) + 
  xlab("") + ylab("Frequency") + labs(title=" I Am More Comfortable Being an Activist Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale2 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_increasea)), aes(x=wactivist_s_increasea)) + 
  geom_bar(col="black", stat="count", fill=extended_huffcol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Increased My Activism Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale3 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_learnsj)), aes(x=wactivist_s_learnsj)) + 
  geom_bar(col="black", stat="count", fill=extended_slytcol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Learned About Social Justice Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale4 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_newfriends)), aes(x=wactivist_s_newfriends)) + 
  geom_bar(col="black", stat="count", fill=extended_hogwcol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Protested New Things Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale5 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_madediff)), aes(x=wactivist_s_madediff)) + 
  geom_bar(col="black", stat="count", fill=extended_gryfcol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Made a Difference in the World Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale6 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_boycottnew)), aes(x=wactivist_s_boycottnew)) + 
  geom_bar(col="black", stat="count", fill=extended_ravecol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Boycotted Something Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

histo_scale7 <- ggplot2::ggplot(data=subset(data, !is.na(wactivist_s_protestnew)), aes(x=wactivist_s_protestnew)) + 
  geom_bar(col="black", stat="count", fill=extended_huffcol(7)) + 
  xlab("") + ylab("Frequency") + labs(title="I Have Protested Something Due to the HPA") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data$wactivist_s_madediff <- as.numeric(data$wactivist_s_madediff)
data$wactivist_s_increasea <- as.numeric(data$wactivist_s_increasea)
data$wactivist_s_learnsj <- as.numeric(data$wactivist_s_learnsj)
data$wactivist_s_comfort <- as.numeric(data$wactivist_s_comfort)
data$wactivist_s_newfriends <- as.numeric(data$wactivist_s_newfriends)
data$wactivist_s_boycottnew <- as.numeric(data$wactivist_s_boycottnew)
data$wactivist_s_protestnew <- as.numeric(data$wactivist_s_protestnew)

aggTots <- ddply(data, c("consider_wactivist", "female"), summarise,
                 N_madediff    = sum(!is.na(wactivist_s_madediff)),
                 mean_madediff = mean(wactivist_s_madediff, na.rm=TRUE),
                 sd_madediff   = sd(wactivist_s_madediff, na.rm=TRUE),
                 se_madediff   = sd_madediff / sqrt(N_madediff),
                 
                 N_increasea    = sum(!is.na(wactivist_s_increasea)),
                 mean_increasea = mean(wactivist_s_increasea, na.rm=TRUE),
                 sd_increasea   = sd(wactivist_s_increasea, na.rm=TRUE),
                 se_increasea   = sd_increasea / sqrt(N_increasea),
                 
                 N_comfort    = sum(!is.na(wactivist_s_comfort)),
                 mean_comfort = mean(wactivist_s_comfort, na.rm=TRUE),
                 sd_comfort   = sd(wactivist_s_comfort, na.rm=TRUE),
                 se_comfort   = sd_comfort / sqrt(N_comfort),
                 
                 N_learnsj    = sum(!is.na(wactivist_s_learnsj)),
                 mean_learnsj = mean(wactivist_s_learnsj, na.rm=TRUE),
                 sd_learnsj   = sd(wactivist_s_learnsj, na.rm=TRUE),
                 se_learnsj   = sd_learnsj / sqrt(N_learnsj),
                 
                 N_newfriends    = sum(!is.na(wactivist_s_newfriends)),
                 mean_newfriends = mean(wactivist_s_newfriends, na.rm=TRUE),
                 sd_newfriends   = sd(wactivist_s_newfriends, na.rm=TRUE),
                 se_newfriends   = sd_newfriends / sqrt(N_newfriends),
                 
                 N_boycottnew   = sum(!is.na(wactivist_s_boycottnew)),
                 mean_boycottnew = mean(wactivist_s_boycottnew, na.rm=TRUE),
                 sd_boycottnew   = sd(wactivist_s_boycottnew, na.rm=TRUE),
                 se_boycottnew   = sd_boycottnew / sqrt(N_boycottnew),
                 
                 N_protestnew    = sum(!is.na(wactivist_s_protestnew)),
                 mean_protestnew = mean(wactivist_s_protestnew, na.rm=TRUE),
                 sd_protestnew   = sd(wactivist_s_protestnew, na.rm=TRUE),
                 se_protestnew   = sd_protestnew / sqrt(N_protestnew)
)

diffworld <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_madediff, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I make a difference in the world b/c of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#740001", "#eeba30"))+
  geom_errorbar(aes(ymin=mean_madediff-se_madediff, ymax=mean_madediff+se_madediff),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))

comfort <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_comfort, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I am more comfortable being an\n activist because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#111c40", "#986f30"))+
  geom_errorbar(aes(ymin=mean_comfort-se_comfort, ymax=mean_comfort+se_comfort),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))

increasea <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_increasea, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I have increased in activism\n because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#eec540", "#927a2c"))+
  geom_errorbar(aes(ymin=mean_increasea-se_increasea, ymax=mean_increasea+se_increasea),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))

learnsj <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_learnsj, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I have learned about social justice\n because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#20472e", "#565555"))+
  geom_errorbar(aes(ymin=mean_learnsj-se_learnsj, ymax=mean_learnsj+se_learnsj),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))

newfriends <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_newfriends, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I have gained new friends\n because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#ae0001", "#d3a625"))+
  geom_errorbar(aes(ymin=mean_newfriends-se_newfriends, ymax=mean_newfriends+se_newfriends),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))


boycottnew <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_boycottnew, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I have boycotted something I wouldn't have\n because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#1d2a54", "#b8873d"))+
  geom_errorbar(aes(ymin=mean_boycottnew-se_boycottnew, ymax=mean_boycottnew+se_boycottnew),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))

protestnew <- ggplot(aggTots, aes(x=consider_wactivist, y=mean_protestnew, group=female))+
  geom_bar(stat="identity", position="dodge", aes(fill=female))+
  labs(y="I have protested something I wouldn't have\n because of The HPA!", x="Do you see yourself as a Wizard Activist?", fill="")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values=c("#d2ad33", "#a1a1a1"))+
  geom_errorbar(aes(ymin=mean_protestnew-se_protestnew, ymax=mean_protestnew+se_protestnew),
                width=.3,                    # Width of the error bars
                position=position_dodge(.9))


histo_scale1
histo_scale2
histo_scale3
histo_scale4
histo_scale5
histo_scale6
histo_scale7

diffworld
comfort
increasea
learnsj
newfriends
boycottnew
protestnew