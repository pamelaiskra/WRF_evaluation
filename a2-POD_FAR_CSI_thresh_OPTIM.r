#=======Input variables========
#This code is a2-POD_FAR_CSI_optim.r
#This code writes a file with skill scores per timestep for a given simulation for a given threshold
#e.g. Skill_scores_WRFsimG_thresh1.txt
#THIS IS ***HOURLY***, DOES NOT TRANSFORM LOW WRF VALUES TO ZERO (see line 159), DISREGARDS OBS 0.00 VALUES
rm(list=ls())

# Define location of file that has number of rows = number of gauges in domain; 
# Cols 1-3: Easting, northing and name of gauge 
# Col 4: WRF grid cell ID for that gauge
# Separated by tabs
location_file <- ("F:/F_Newcastle/RainGa_WRF2019.txt") # Newcastle OLD and NEW
#location_file <- ("E:/E_Newcastle/delayed_ts/RainGa_WRFdelayed.txt") # Newcastle delayed
#location_file <- ("F:/F_Birmingham/RainGa_WRFBir.txt")

# Define location of the rain gauge files timeseries
#rain_folder <- ("E:/DynaTOPMODEL/Docs_from_EnvObs/Hourly_Rainfall_EA_MIDAS")     
rain_folder <- ("F:/DECIPHeR/Docs_from_EnvObss/Hourly_Rainfall_EA_MIDAS")   

# Define folder location of WRF timeseries for each cell.
# This path must end where the WRF simulation number/names start for the "for" loop to work.
# E.g. for files named "WRF_sim1", "WRF_sim2", "WRF_sim3", wrf_path should end in "WRF_sim"
#wrf_path <- ("E:/DynaTOPMODEL/DECIPHeR/Hourly_timeseries/Hourly_rainfall_WRF/WRF_2km_sim")  # Newcastle
#wrf_path <- ("E:/E_Newcastle/delayed_ts/WRF") # Newcastle delayed 
wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/NewcastleWRF")
#wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/BirminghamWRF_")

# Define prefix of simulated rainfall files per cell
wrf_prefix <- ("rainfall_WRF_gridID")

# Define full path of where the skill score files will be located
fss_path <- ("E:/E_Newcastle/WRF_version4/Skill_Scores") # Newcastle original
fss_path <- ("E:/E_Newcastle/delayed_ts/Skill_Scores")
fss_path <- ("F:/F_Newcastle/Skill_Scores") #Newcastle new
fss_path <- ("F:/F_Newcastle/Skill_Scores_v2") #Newcastle new version 2 (transforming WRF data to zeros)
#fss_path <- ("F:/F_Birmingham/SKSC") #Birmingham

anfang  <- c(2012,6,28,1)                                             # Start of validation period w/o spin-up
#anfang <- c(2007,7,20,1)    # Birmingham
tage <- 2                                                             # Days in the simulation
stunde <-0                                                            # Hours (in addition to days) in the simulation
schwelle_vector <- 0.1                                                  #  c(0,0.1,0.3,0.5,0.7,1)
#wrf_to_analyse <- c(1:5,13:19)                                       # Names of WRF simulations to analyse.
#wrf_to_analyse <- c("1delayed6","1delayed24","1delayed48","4delayed6","4delayed24","4delayed48","5delayed6","5delayed24","5delayed48")   
#wrf_to_analyse <- c(21:30)
#wrf_to_analyse <- LETTERS[1:10]  #Birmingham
#wrf_to_analyse <- c(21,22,24,25, "26b","27b",28,"29b","30b") #NEW URBAN
#wrf_to_analyse <- c("A","B","D","E","Fb","Gb","Hb","I","Jb")

#---Measuring the time...
#system.time({

#=========Function=============
# To get simulated rainfall for a given time step, from the WRF files

get_WRF_rainfall <- function(WRFfile, tstep) {
  table <- read.delim(WRFfile, header=FALSE,sep=" ",dec=".")                              #read file
  colnames(table) <- c("Jahr","Monat","Tag","Stunde","WRF_rainfall")                      #renames columns
  erste_zeil <- which(table$Jahr==anfang[1] & table$Monat==anfang[2] & table$Tag==anfang[3] & table$Stunde==anfang[4])    #First row of file
  SIMRAIN<-table$WRF_rainfall[erste_zeil+tstep-1]                                         #Rainfall for timestep "tstep"
  return(SIMRAIN)
}

#===========Code===============

base_df <- read.table(location_file, sep="\t", header=TRUE) 
colnames(base_df) <- c("EASTING", "NORTHING", "GAUGE_NAME", "WRFgrid")
base_df$WRFgrid <- round(base_df$WRFgrid)
no_timesteps <- tage*24+stunde
#rm(stunde,tage)
base_matrix<-as.matrix(base_df)

timestep_skillscores<- list()
#contingency_table<- list()

# Modify base_matrix so that column 3 (gauge file) have the absolute path of the files needed
for (gg in 1:nrow(base_matrix)) {                                                       
  base_matrix[gg,3]<- paste0(rain_folder, "/",levels(droplevels(base_df[gg,3])))
}

# To avoid going through the rain files every time a timestep is analysed, a list will be created with a level per gauge,
# and each level will contain the observed rainfall for the duration of the simulation
# For reference, 24 timesteps for 52 files takes 73,6 s.    

all_obs_rainfall<-list()
skipped_gauge_name <- list()
skipped_gauge_index <- list()
rr <- 0

for (i in 1:nrow(base_matrix)) {
  table <- read.delim(base_matrix[i,3], header=FALSE,sep=" ",dec=".", skip=2)             #read file
  table <- table[,1:7]     
  #Keeps only cols with date and rainfall
  colnames(table) <- c("GAUGE","Provider","Jahr","Monat","Tag","Stunde","Rainfall")       #renames columns
  
  if (table$Jahr[1]>anfang[1] ) {
    rr <- rr + 1
    skipped_gauge_index[[rr]] <- i
    skipped_gauge_name[[rr]] <- base_df$GAUGE_NAME[i]
    cat(sprintf("Skipping gauge %s (data doesn't cover selected dates)\n",base_df$GAUGE_NAME[i]))
  } else {
    table$Rainfall <- table$Rainfall*0.1                                                    #RF from EnvObs is in tenths of mm
    erste_zeil <- which(table$Jahr==anfang[1] & table$Monat==anfang[2] & table$Tag==anfang[3] & table$Stunde==anfang[4])    #First row of file
    letzte_zeil <- erste_zeil+no_timesteps-1                                                #Last row of file to analyse
    all_obs_rainfall[[i]]<-table$Rainfall[erste_zeil:letzte_zeil]  
    cat(sprintf("Reading observed rainfall file %i of %i\n",i,nrow(base_matrix)))
  }
  
}

skipped_gauge_index <- as.vector(do.call(rbind,skipped_gauge_index))

# For each threshold...
for (lala in 1:length(schwelle_vector))  {
  
  # For each WRF file to analyse...
  for(j in 1:length(wrf_to_analyse)) {
    
    # Modify base_matrix so that column 4 (WRF file) have the absolute path of the files needed
    for (gg in 1:nrow(base_matrix)) {                                                       
      base_matrix[gg,4]<- paste0(wrf_path,wrf_to_analyse[j],"/rainfall_WRF_gridID",base_df$WRFgrid[gg],".txt")
    }
    
    #For every timestep "zeitschritt", create empty lists...
    
    for (zeitschritt in 1:no_timesteps) {
      obs_rain <- list()
      sim_rain <- list()
      
      #... to store observed and simulated rainfall for location "ort" from the matrix base_matrix
      for (ort in 1:nrow(base_matrix)) {
        obs_rain[[ort]] <- all_obs_rainfall[[ort]][zeitschritt]
        sim_rain[[ort]] <- get_WRF_rainfall(base_matrix[ort,4], zeitschritt)
        #cat(sprintf("Timestep: %i of %i. Location %i of %i. Simulation %i (%i of %i)\n", zeitschritt, no_timesteps, ort, nrow(base_matrix), wrf_to_analyse[j],j,length(wrf_to_analyse)))
      }
      
      # The matrix with observed and simulated rainfall for a given timestep for all locations is yy.
      # Removes data (stations where dates don't cover selected period), if necessary
      # Still  keeps the stations with -99.9 values
      if (length(skipped_gauge_index)>0) {
        yy<-data.frame(OBS=do.call(rbind,obs_rain),SIM=do.call(rbind,sim_rain)[-c(skipped_gauge_index)]) #----- 1)
      } else {
        yy<-data.frame(OBS=do.call(rbind,obs_rain),SIM=do.call(rbind,sim_rain))
      }
      
      # Eliminates rows without data (-99.9)
      yy<- yy[-which(yy$OBS<0),]                    #-------------- 2)
      
      # Applies threshold. If zero, matrix stays the same
      schwelle <- schwelle_vector[lala]
      if (schwelle==0) {
        yy <- yy
        #print(nrow(yy))
      } else {
        yy <- yy[-which(yy$OBS<=schwelle),]              #--------------- 3)
        #print(nrow(yy[-which(yy$OBS<=schwelle),]))
      }
      
      # Applies theshold (version 2). All sim values (0  0,04] mm are considered 0
      # This is because values of 0,05 would round up to 0,1 which is the precision of the observed rainfall.
      #yy$SIM[which(yy$SIM > 0 & yy$SIM < 0.04)] = 0
      
      
      # If, given the threshold, no values from any station remained for analysis, then NaN values are introduced
      # for that timestep.
      if (nrow(yy)==0) {
        skillscores <- data.frame(POD="NaN",FAR="NaN",CSI="NaN",RMSE="NaN",MBE="NaN",SD="NaN",gauges_used=nrow(yy),stringsAsFactors=FALSE)
        # Otherwise, skill scores are calculated.
      } else {                                                                                         
        row.names(yy) <-c(1:nrow(yy))                                                    #Corrects row numbers
        # Calculates hits, misses and false alarms
        dummy_matrix<-matrix(0,nrow(yy),3)
        colnames(dummy_matrix) <- c("hits","misses","false_alarms")
        
        yy<- cbind(yy,dummy_matrix)
        
        yy$hits[which(yy$OBS >0 & yy$SIM >0)] =1
        yy$misses[which(yy$OBS >0 & yy$SIM ==0)] =1
        yy$false_alarms[which(yy$OBS ==0 & yy$SIM >0)] =1
        
        #contingency_table[[zeitschritt]] <-yy
        
        HH<- sum(yy$hits)
        MM<- sum(yy$misses)
        FA<- sum(yy$false_alarms)
        RMSE1<- sqrt((sum((yy$SIM-yy$OBS)^2))*(1/nrow(yy)))
        MBE1<- (sum(yy$SIM-yy$OBS))*(1/nrow(yy))
        SD1<- sqrt(sum((yy$SIM-yy$OBS-MBE1)^2)*(1/(nrow(yy)-1)))
        
        #---Calculates skills scores and stores them in a data frame,
        #   which will be stores in a list for every iteration (every time step of the simulation: zeitschritt)
        POD1<- HH/(HH+MM)
        FAR1<- FA/(FA+HH)
        CSI1<- HH/(HH+FA+MM)
        
        skillscores <- data.frame(POD=POD1,FAR=FAR1,CSI=CSI1,RMSE=RMSE1,MBE=MBE1,SD=SD1,gauges_used=nrow(yy),stringsAsFactors=FALSE)
        print(do.call(rbind,skillscores))
      }
      
      timestep_skillscores[[zeitschritt]] <- skillscores
      
    } #Each timestep
    
    MATRIX_SKILL_SCORES<-do.call(rbind,timestep_skillscores)
    datei_fullpath <- paste0(fss_path,"/",sprintf("Skill_scores_WRFsim%s_thresh%s.txt",wrf_to_analyse[j],schwelle))
    write.table(MATRIX_SKILL_SCORES,datei_fullpath,quote=FALSE,row.names = FALSE)
    
  } # Each simulation
  
  
} # Each threshold  

#}) #... Finishes measuring the time

#============================PLAYGROUND==================================
#a_algo<-list()
#for (i in 1:52) {
#  a_table <- read.delim(base_matrix[i,3], header=FALSE,sep=" ",dec=".", skip=2)             #read file
#  a_table <- a_table[,1:7]                                                                    #Keeps only cols with date and rainfall
#  colnames(a_table) <- c("GAUGE","Provider","Jahr","Monat","Tag","Stunde","Rainfall")       #renames columns
#  a_table$Rainfall <- a_table$Rainfall*0.1                                                    #RF from EnvObs is in tenths of mm
#  a_erste_zeil <- which(a_table$Jahr==anfang[1] & a_table$Monat==anfang[2] & a_table$Tag==anfang[3] & a_table$Stunde==anfang[4])    #First row of file
#  a_letzte_zeil <- a_erste_zeil+no_timesteps-1                                                #Last row of file to analyse
#  a_algo[[i]]<-a_table$Rainfall[a_erste_zeil:a_letzte_zeil]  
#}

#a_obs_rain<-do.call(rbind,a_algo)