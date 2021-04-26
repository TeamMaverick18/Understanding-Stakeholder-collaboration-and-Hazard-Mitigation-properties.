library(igraph)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(fitdistrplus)
library(ggpubr)
library(purrr)
library(tidyr)
#library(tidyverse)

# Load mission assignment and disaster information
MA<-read.csv("G:\\Shared drives\\Yitong Li, GMU, 2018\\Research\\PH.D Thesis\\Code and Data\\Datasets\\MissionAssignments.csv")

#############################
##### Clean the dataset #####
# Append 0 to MA that does not end with 0 (for aggragating funding)
MA$lastCharacter <- sapply(strsplit(as.character(MA$maId), ""), tail, 1)
MA$maId<-as.character(MA$maId)

for (i in 1:nrow(MA)){
  str<-MA[i,1]
  if (MA[i,24]!=0) {
    MA[i,1]<-paste0(str,"0")
  } else {
    MA[i,1]<-str
  }
}

MA$obligationAmount<-as.character(MA$obligationAmount)

# Remove SOWs that contian no information
MA<-MA[!(MA$statementOfWork==""), ]
MA<-MA[!(MA$statementOfWork==0), ]
MA<-MA[!(MA$statementOfWork==31250), ]

# Filter out the obligated amount that has NA or 0
MA_RM<-MA %>% # removed
  filter(!is.na(obligationAmount))%>%
  filter(obligationAmount !=0)

# Remove the 896 duplicated budget (either deobligated twice or obligated twice)
MA_RD<-MA_RM[!duplicated(MA_RM[ ,c("maId","disasterNumber","agency","dateRequested","dateObligated","obligationAmount")]),]

# Filter the obligated amount that has NA or 0
MA_NA<-MA %>%
  filter(is.na(obligationAmount)|obligationAmount==0)

# Combine the MA dataset (processed cost information)
MA_Cost<-rbind(MA_NA,MA_RD)
MA_Cost$obligationAmount<-as.numeric(MA_Cost$obligationAmount)

MA_cleaned<-MA_Cost %>%
  group_by(maId) %>%
  mutate(cost=sum(obligationAmount))%>%
  distinct(maId, .keep_all=TRUE)

MA_cleaned$AD<-paste(MA_cleaned$agency, MA_cleaned$disasterDescription) 


#write.csv(MA_cleaned, "G:\\My Drive\\GMU Material\\GMU Course\\Spring 2021\\DAEN 690\\Code and Data\\Cleaned MA.csv")

##############################
##### Types of disasters #####
Disaster_Type<-MA_cleaned %>%
  group_by(disasterDescription) %>%
  summarize(Count=n())
Disaster_Type<-Disaster_Type[order(-Disaster_Type$Count),]
#Disaster_Type<-Disaster_Type[-13,]

ggplot(Disaster_Type, aes(x=reorder(disasterDescription, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  labs(x="Disaster Type", y="Frequency", title="MA Frequency for All Disasters")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

##############################
##### Samples for CNN Training #####
Disaster_AD<-MA_cleaned %>%
  group_by(AD) %>%
  summarize(Count=n())
Disaster_AD<-Disaster_AD[order(-Disaster_AD$Count),]

ggplot(Disaster_AD[1:20,], aes(x=reorder(AD, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  labs(x="Agency and Disaster", y="Frequency", title="Selected MA Frequency Based on Agency and Disasters (20/436)")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

# Groupby and randomly select half of samples from each category using SpaCy
set.seed(498723)
Sample<-MA_cleaned %>% 
  group_by(AD) %>% 
  sample_frac(0.5)
#write.csv(Sample, "G:\\My Drive\\GMU Material\\GMU Course\\Spring 2021\\DAEN 690\\Code and Data\\sample.csv")

#################################################
##### Hurricane, Severe Storms, Flood, Fire #####
Hurricane<-filter(MA_cleaned, disasterDescription=="Hurricane")
SevereStorm<-filter(MA_cleaned, disasterDescription=="Severe Storm(s)")
Flood<-filter(MA_cleaned, disasterDescription=="Flood")
Fire<-filter(MA_cleaned, disasterDescription=="Fire")

# Hurricane (DN disaster number)
Hurricane_DN<-Hurricane %>%
  group_by(disasterNumber) %>%
  summarize(Count=n())# 168 DN
Hurricane_DN<-Hurricane_DN[order(-Hurricane_DN$Count),]

Hurricane_plt<-ggplot(Hurricane_DN[1:10,], aes(x=reorder(disasterNumber, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  ylim(0,400)+
  labs(x="Disaster Number", y="Frequency", title="Top 10 Hurricanes Ranked Based on MA Frequency")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

# SevereStorm
SevereStorm_DN<-SevereStorm %>%
  group_by(disasterNumber) %>%
  summarize(Count=n())
SevereStorm_DN<-SevereStorm_DN[order(-SevereStorm_DN$Count),]

SevereStorm_plt<-ggplot(SevereStorm_DN[1:10,], aes(x=reorder(disasterNumber, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  ylim(0,400)+
  labs(x="Disaster Number", y="Frequency", title="Top 10 SevereStorms Ranked Based on MA Frequency")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

# Flood
Flood_DN<-Flood %>%
  group_by(disasterNumber) %>%
  summarize(Count=n())
Flood_DN<-Flood_DN[order(-Flood_DN$Count),]

Flood_plt<-ggplot(Flood_DN[1:10,], aes(x=reorder(disasterNumber, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  ylim(0,400)+
  labs(x="Disaster Number", y="Frequency", title="Top 10 Floods Ranked Based on MA Frequency")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

# Fire
Fire_DN<-Fire %>%
  group_by(disasterNumber) %>%
  summarize(Count=n())
Fire_DN<-Fire_DN[order(-Fire_DN$Count),]

Fire_plt<-ggplot(Fire_DN[1:10,], aes(x=reorder(disasterNumber, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  ylim(0,400)+
  labs(x="Disaster Number", y="Frequency", title="Top 10 Fires Ranked Based on MA Frequency")+
  theme_minimal()+
  theme(axis.text.x=element_text(color = "black", size=10, angle=70, vjust=.8, hjust=0.8))

ggarrange(Hurricane_plt, SevereStorm_plt, Flood_plt, Fire_plt,
          ncol = 2, nrow = 2)

#########################################################################################
# ##### Puerto Maria, North Dakota Severe Storms, Louisiana Flooding, California Fire #####
# PM<-filter(MA_cleaned, disasterNumber=="4339")
# NS<-filter(MA_cleaned, disasterNumber=="1829")
# LF<-filter(MA_cleaned, disasterNumber=="4277")
# CF<-filter(MA_cleaned, disasterNumber=="1731")

###################################
#### Count of agency occurance ####
Hurricane<-filter(MA_cleaned, disasterDescription=="Hurricane")
SevereStorm<-filter(MA_cleaned, disasterDescription=="Severe Storm(s)")
Flood<-filter(MA_cleaned, disasterDescription=="Flood")
Fire<-filter(MA_cleaned, disasterDescription=="Fire")

Assigned_agency_Hurricane<-Hurricane %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_SevereStorm<-SevereStorm %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Flood<-Flood %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Fire<-Fire %>%
  group_by(agency) %>%
  summarize(Count=n())

PM_agency<-ggplot(Assigned_agency_Hurricane, aes(x=reorder(agency, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(x="Agency", y="Frequency", title="Hurricane")+
  theme(axis.text.x=element_text(color = "black", size=9, angle=90, vjust=.8, hjust=0.8))

NS_agency<-ggplot(Assigned_agency_SevereStorm, aes(x=reorder(agency, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(x="Agency", y="Frequency", title="Severe Storms")+
  theme(axis.text.x=element_text(color = "black", size=9, angle=90, vjust=.8, hjust=0.8))

LF_agency<-ggplot(Assigned_agency_Flood, aes(x=reorder(agency, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(x="Agency", y="Frequency", title="Flooding")+
  theme(axis.text.x=element_text(color = "black", size=9, angle=90, vjust=.8, hjust=0.8))

CF_agency<-ggplot(Assigned_agency_Fire, aes(x=reorder(agency, -Count), y=Count)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(x="Agency", y="Frequency", title="Wildfires")+
  theme(axis.text.x=element_text(color = "black", size=9, angle=90, vjust=.8, hjust=0.8))

agency<-ggarrange(PM_agency, NS_agency, LF_agency, CF_agency, ncol = 2, nrow = 2)
annotate_figure(agency, top = text_grob("Total Number of MA Assigned to Each Agency", face = "bold", size = 14))

# Pick 80% agency
Assigned_agency_Hurricane<-Hurricane %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_SevereStorm<-SevereStorm %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Flood<-Flood %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Fire<-Fire %>%
  group_by(agency) %>%
  summarize(Count=n())


cumPer <- function(df) {
  df<-df[order(-df$Count),]
  df$cumPer<-100*cumsum(df$Count)/sum(df$Count)
  return(df)
  }


Assigned_agency_SevereStorm<-SevereStorm %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Flood<-Flood %>%
  group_by(agency) %>%
  summarize(Count=n())
Assigned_agency_Fire<-Fire %>%
  group_by(agency) %>%
  summarize(Count=n())

Assigned_agency_Hurricane<-cumPer(Assigned_agency_Hurricane) #20
Assigned_agency_SevereStorm<-cumPer(Assigned_agency_SevereStorm) # 14
Assigned_agency_Flood<-cumPer(Assigned_agency_Flood) # 19
Assigned_agency_Fire<-cumPer(Assigned_agency_Fire) # 15

h<-Assigned_agency_Hurricane[1:20,]
ss<-Assigned_agency_SevereStorm[1:14,]
fl<-Assigned_agency_Flood[1:19,]
fi<-Assigned_agency_Fire[1:15,]

# Comparision of agencies involved in different disasters #
relcomp <- function(a, b) {
  comp <- vector()
  for (i in a) {
    if (i %in% a && !(i %in% b)) {
      comp <- append(comp, i)
    }
  }
  return(comp)
}

# List of agencies involved in all four types of disasters
a<-Reduce(intersect, list(h$agency, ss$agency, fl$agency, fi$agency))
# Unique to PM
print(relcomp(h$agency, a))
print(relcomp(ss$agency, a))
print(relcomp(fl$agency, a))
print(relcomp(fi$agency, a))

