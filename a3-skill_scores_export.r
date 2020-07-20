#==============
#This code is a3-skill_scores_export.r
#For a given skill score, this code exports the median of said skill score per timestep, for all sims, for all thresholds, USES ALL GUAGES (see line 28)
#Files created: POD.txt, FAR.txt, CSI.txt etc.
# *********** ATTENTION**********
# THIS CODE DOESN'T WORK FOR ONLY ONE SIMULATION
# Line 101 requires to calculate the mean of several columns (columns = simulations)

#install.packages("ggplot2")
library("ggplot2")
library(reshape)
rm(list=ls())

#Location of folder with skill scores per time step for all simulations
#location_files <- ("E:/E_Newcastle/WRF_version4/Skill_Scores/")
#location_files <- ("E:/E_Newcastle/delayed_ts/Skill_Scores/")
location_files <- ("F:/F_Newcastle/SKSC/Sim21-30/")
#location_files <- ("F:/F_Birmingham/SKSC/")

#a_simulations <- c(1:5, 13:19)
#a_simulations <- c("1delayed6","1delayed24","1delayed48","4delayed6","4delayed24","4delayed48","5delayed6","5delayed24","5delayed48")
#a_simulations <- c(21:30)
#a_simulations <- LETTERS[1:10]
a_simulations <- c(21,22,24,"25b","26b","27b","28b","29b","30b") #NEW URBAN
#a_simulations <- c("21delayed6","21delayed24")   # DELAYED
#a_simulations <- c("A","B","D","E","Fb","Gb","Hb","I","Jb")

schwelle<- 0.1                           # c(0,0.1,0.3,0.5,0.7,1) thresholds
prozent <- 0.01 #Minimum percentage of total gauges that will be used to calculates skill scores

# Filename ROOT for reference (WITHOUT threshold, threshold is assigned later in the code)
# To define num gauges used per tstep 
# This can be any simulation because all simulations for a given threshold have the same number of gauges used per time step
# (and this is because the criteria for removal is applied to the observed data)
#E.g. WRF sims a, b and c for thresh=0 have the same number of gauges used per time step.
#reference_file <- "Skill_scores_WRFsim13_thresh" 
#reference_file <- "Skill_scores_WRFsim21delayed24_thresh"   # DELAYED
reference_file <- "Skill_scores_WRFsim21_thresh"     #Newcastle new
#reference_file <- "Skill_scores_WRFsimA_thresh"     #Birmingham


#========CODE
setwd(location_files)
a_skill_score<-c("POD","FAR","CSI","RMSE","MBE","SD")

for (ii in 1:length(a_skill_score)) {
  
  #Plot number of gauges usedd to calculate SKill scores
  gu<-list()
  for (k in 1:length(schwelle)) {
    aa<-read.table(paste0(location_files,reference_file,schwelle[k],".txt"), sep=" ", header=TRUE)
    gu[[k]] <-aa$gauges_used 
  }
  
  oo<-do.call(cbind,gu)
  hh<-as.data.frame(cbind(c(1:nrow(oo)),oo)) # Number of gauges considered valid for each time step
  colnames(hh) <- c("SimTime",schwelle)
  tt<-melt(hh,id.vars = "SimTime", variable.name = "Gauges_used")
  #melt puts all data in a single column to be plotted
  colnames(tt)<- c("SimTime","Threshold","Gauges_used") 
  max_gauges<- max(hh[,2]) # Max number of gauges is for threshold 0 (column 2 of hh)
  
  dateiname <- sprintf("Gauges_used_to_calc_SkillScores.png")
  subcapzion <-  sprintf("Out of %s to calculate skill scores and accuracy metrics", max_gauges)
  
  #Max and actual gauges used per threshold
  #Requires at least two thresholds to calculate maximum. If there's only one, that will me the "maximum"
  if (length(schwelle) >=2) {
    num_gauges <- apply(hh[,2:ncol(hh)],2,max,na.rm=TRUE)
  } else {
    num_gauges <- max(hh)
  }
  
  #each column is a threshold
  maxx_gauges <- data.frame(Threshold=schwelle,Max_num_gauges=num_gauges, Gauges_used_calc=num_gauges*prozent)
  
  #Export number of gauges left after threshold applied
  write.table(hh,paste0(location_files,"Gauges used per time step.txt"),sep="\t",eol = "\n",row.names = FALSE)
  
  #============================= 
  #Exports the POD calculated with the adjusted number of gauges to a text file
  #Note that there are two lines that need to be adjusted manually
  
  mm<- list()
  
  for (j in 1:length(schwelle)) {
    uu<-list()
    for (i in 1:length(a_simulations)) {
      aa<-read.table(paste0(location_files,"Skill_scores_WRFsim",a_simulations[i],"_thresh",schwelle[j],".txt"), sep=" ", header=TRUE)
      aaa<-as.matrix(aa)
      uu[[i]] <-aaa[,ii]
    }
    
    oo<-do.call(cbind,uu)
    hh2<-data.frame(1:nrow(oo),oo,hh[,j+1]) #No. timestep, skill score for all sims for threshold schwelle[j], 
    #num gauges used to calculate skill score per timestep
    colnames(hh2) <- c("SimTime",a_simulations,"Gauges-per-TStp")
    
    #Eliminate rows with less gauges than the minimum "prozent"
    hh3<-hh2[(which(hh2$`Gauges-per-TStp`>=maxx_gauges$Gauges_used_calc[j])),]
    #Obtain median per column starting in column 2 
    #each column of hh3 is a simulation
    #each row of mm is a simulation, each column of mm is a threshold
    # If there is only one simulation, simply obtain the mean of column 2 of hh3
    if (length(a_simulations) == 1) {
      mm[[j]] <- mean(hh3[,2],na.rm=TRUE)
    } else {
      mm[[j]]<-apply(hh3[,2:(ncol(hh2)-1)],2,mean,na.rm=TRUE)  
    }
  }
  
  nn<-do.call(cbind,mm)
  colnames(nn)<-schwelle
  
  #write.table(hh2,paste0(location_files,a_skill_score[ii],"_pertimestep.txt"),sep="\t",eol = "\n",row.names=FALSE) 
  write.table(nn,paste0(location_files,a_skill_score[ii],".txt"),sep="\t",eol = "\n",row.names=FALSE) 
  
} # Each threshold

ggplot(tt, aes(SimTime, Gauges_used)) + geom_line(aes(colour = Threshold),size=1) +  
  labs(x = "Simulation time [h]", y="", title="Gauges used", subtitle=subcapzion, 
       caption="capzion", colour="Threshold", fill="Envelopes") +
  theme(plot.title = element_text(size=20),
        plot.subtitle = element_text(size=11),
        axis.title.x = element_text(size=14),
        axis.text.x  = element_text(size=13, vjust=1, hjust=0),
        axis.title.y = element_text(size=14),    # title
        axis.text.y  = element_text(size=13),     # numbers
        legend.title = element_text(size=13),
        legend.text = element_text(size=16),
        legend.position="bottom") +
  ggsave(dateiname, width = 8, height = 6) 
