# This code creates a meteogram for all the gauges in the domain
# only for gauges WITHOUT NaN values and that cover selected dates.
# It also calculates the meteogram for 2, 3 and 4-hour period 
# This code is a6-Meteogram_OPTIM+acc.r
#install.packages("ggplot2")
#library("ggplot2")

#=======Input variables========
rm(list=ls()) 

# Define location of file that has number of rows = number of gauges in domain; 
# Cols 1-3: Easting, northing and name of gauge 
# Col 4: WRF grid cell ID for that gauge
# Separated by tabs
location_file <- ("F:/F_Newcastle/RainGa_WRF2019.txt")
#location_file <- ("F:/F_Birmingham/RainGa_WRFBir.txt")  #Birmingham

# Define location of the rain gauge files timeseries  
rain_folder <- ("F:/DECIPHeR/Docs_from_EnvObss/Hourly_Rainfall_EA_MIDAS")   

# Define location of WRF files (the timeseries for each cell).
# This path must end where the WRF simulation number/names start.
# E.g. for files named "WRF_sim1", "WRF_sim2", "WRF_sim3", wrf_path should end in "WRF_sim"
# See line 15 of code a2-POD_FAR_CSI.r
#wrf_path <- ("E:/DynaTOPMODEL/DECIPHeR/Hourly_timeseries/Hourly_rainfall_WRF/WRF_2km_sim")  # Newcastle
#wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/BirminghamWRF_")
wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/NewcastleWRF")

# Define prefix of simulated rainfall files per cell
wrf_prefix <- ("rainfall_WRF_gridID")

# Define full path of where the skill score files will be located
#fss_path <- ("F:/F_Newcastle/Skill_Scores/Meteogram")
#fss_path <- ("F:/F_Birmingham/Skill_Scores/Meteogram")
fss_path <- ("F:/F_Newcastle/Meteograms_UCM") #Filder has to be created manually

anfang <- c(2012,6,28,1)   #Newcastle
#anfang <- c(2007,7,20,1) #Birmingham
tage <- 2                                                                          # Days in the simulation
stunde <-0                                                                         # Hours (in addition to days) in the simulation
wrf_to_analyse <- c(21,22,24)
#wrf_to_analyse <- LETTERS[1:10]   #Birmingham

#Graphs stats. Max value of meteogram, x coord, y coord of upper left corner of label of meteogram 
graph_stats <- c(50,36,47) #Newcastle
#graph_stats <- c(50,36,35) #Birmingham

#Select for which gauge the will be meteograms
#(in case you just want to plot some stations)
gauge_meteogram<-c("EA_06_alston_s_wks_auto_qc.txt", "EA_06_chirdon_logger_qc.txt","EA_06_crewe_fell_f.h._tel_qc.txt","EA_06_greenhills_farm_auto_qc.txt","EA_06_jesmond_dene_logger_qc.txt","EA_06_long_meadows_qc.txt","EA_06_marine_first_school_qc.txt","EA_06_noonstones_hill_qc.txt","EA_06_howdon_s_wks_no_2_auto_qc.txt","EA_06_tunstall_resr_auto_qc.txt","EA_06_washington_s_wks_qc.txt","EA_06_wileysike_gland_tel_qc.txt")

#---Measuring the time...
#system.time({

#=========Function=============
# To get simulated rainfall for a given grid cell, for all tsteps

get_WRF_rainfall <- function(WRFfile) {
  table <- read.delim(WRFfile, header=FALSE,sep=" ",dec=".")                              #read file
  colnames(table) <- c("Jahr","Monat","Tag","Stunde","WRF_rainfall")                      #renames columns
  erste_zeil <- which(table$Jahr==anfang[1] & table$Monat==anfang[2] & table$Tag==anfang[3] & table$Stunde==anfang[4])    #First row of file
  letzte_zeil <- erste_zeil+no_timesteps-1
  SIMRAIN<-table$WRF_rainfall[erste_zeil:letzte_zeil]                                         #Rainfall for timestep "tstep"
  return(SIMRAIN)
}

#===========Code===============

#--------Observed rainfall. 

base_df <- read.table(location_file, sep="\t", header=TRUE) 
colnames(base_df) <- c("EASTING", "NORTHING", "GAUGE_NAME", "WRFgrid")
base_df$WRFgrid <- round(base_df$WRFgrid)
no_timesteps <- tage*24+stunde
base_matrix<-as.matrix(base_df)

# Modify base_matrix so that column 3 (gauge file) have the absolute path of the files needed
for (gg in 1:nrow(base_matrix)) {                                                       
  base_matrix[gg,3]<- paste0(rain_folder, "/",levels(droplevels(base_df[gg,3])))
}

if (exists("gauge_meteogram") == TRUE) { # If there's just a couple of stations to analyse
  gaugess <- gauge_meteogram
} else {
  gaugess <- as.vector(base_df$GAUGE_NAME) # Gauges in the domain
}

performance <- list() 
gauges_clean <- list()
max_values <- list()
contador <- 0

for (indexx in 1:length(gaugess)) {
  
  cat(sprintf("Processing gauge %i of %i\n",indexx,length(gaugess)))
  gauge_meteogram <- gaugess[indexx]
  
  # Read information for selected rain gauge
  gauge_index <- which(base_df$GAUGE_NAME == gauge_meteogram)
  table <- read.delim(base_matrix[gauge_index,3], header=FALSE,sep=" ",dec=".", skip=2)             #read file
  table <- table[,1:7]                                                                    #Keeps only cols with date and rainfall
  colnames(table) <- c("GAUGE","Provider","Jahr","Monat","Tag","Stunde","Rainfall")       #renames columns
  table$Rainfall <- table$Rainfall*0.1                                                    #RF from EnvObs is in tenths of mm
  
  # 1) check that gauge has data for selected dates
  if (table$Jahr[1]>anfang[1] | table$Jahr[nrow(table)]<anfang[1]) {
    cat(sprintf("Gauge %s's data don't cover selected dates\n\n",gauge_meteogram))
  } else {
    cat(sprintf("Gauge %s's data cover selected dates\n",gauge_meteogram))
    erste_zeil <- which(table$Jahr==anfang[1] & table$Monat==anfang[2] & table$Tag==anfang[3] & table$Stunde==anfang[4])    #First row of file
    letzte_zeil <- erste_zeil+no_timesteps-1                                                #Last row of file to analyse
    OBSRAINvalues_formeteogram<-table$Rainfall[erste_zeil:letzte_zeil] 
    
    # 2) Check for -99.99 values
    if ( length(which(OBSRAINvalues_formeteogram < 0 )) != 0 ) {
      cat(sprintf("... but gauge has NaN values for %i out of %i timesteps. Skipping gauge\n\n",length(which(OBSRAINvalues_formeteogram < 0 )),length(OBSRAINvalues_formeteogram)))
    } else {
      cat(sprintf("Gauge does not contain Nan values. Continuing analysis...\n"))
      
      #------------Simulated rainfall
      
      WRF_cell <- base_df$WRFgrid[which(base_df$GAUGE_NAME == gauge_meteogram)]
      WRF_rain <- list()
      for (tt in 1: length(wrf_to_analyse)) {
        WRF_file <- paste0(wrf_path,wrf_to_analyse[tt],"/rainfall_WRF_gridID",WRF_cell,".txt")
        WRF_rain[[tt]] <- get_WRF_rainfall(WRF_file)
      }
      
      # Store WRf outputs in a matrix of simulated rainfall with nrows = WRF sims, ncols = no. timesteps
      WRFvalues_formeteogram <- do.call(rbind,WRF_rain)
      
      # 3) Check that WRF rain does NOT contain all 0's. If it does, the cell is probably in the margins of the domain. Skip.
      
      if (length(which(WRFvalues_formeteogram == 0)) == (nrow(WRFvalues_formeteogram) * ncol(WRFvalues_formeteogram)) ) {
        cat(sprintf("WRF cell %i for gauge %s contains zeros only (might be in the edge of domain). Skipping gauge\n\n",WRF_cell,gauge_meteogram))
      } else {
        
        METEOGRAM_values <- rbind(OBSRAINvalues_formeteogram, WRFvalues_formeteogram)
        rownames(METEOGRAM_values) <- c("OBS",wrf_to_analyse)
        #write.table(METEOGRAM_values, paste0(fss_path,"/Meteogram_",gauge_meteogram,".txt"),sep="\t",eol="\n",col.names = FALSE)
        
        #dateiname <- paste0(fss_path,"/Meteogram_",gauge_meteogram,".png",sep="")
        #png(file=dateiname, width = 800, height = 600,pointsize = 18)
        #boxplot(WRFvalues_formeteogram,outline=TRUE,col="grey",xlab="Time steps", ylab="Rainfall [mm/h]", #KEEPS OUTLIERS
        #        #main=paste0("Meteogram for ",gauge_meteogram), ylim=c(0,max(WRFvalues_formeteogram,OBSRAINvalues_formeteogram)*1.05)) 
        #        main=paste0("Meteogram for ",gauge_meteogram), ylim=c(0,graph_stats[1])) 
        #points(1:ncol(WRFvalues_formeteogram),OBSRAINvalues_formeteogram,col="blue",pch=19)
        ##legend(1,max(METEOGRAM_values)*0.75,legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
        #legend(graph_stats[2],graph_stats[3],legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
        #dev.off()
        
        #Rectangle
        #dateiname <- paste0(fss_path,"/Meteogram_",gauge_meteogram,".png",sep="") # Version 4
        #png(file=dateiname, width = 7, height = 4, units="in", res=1200)
        #par(cex.axis = 1.1, cex.lab= 1)
        #boxplot(WRFvalues_formeteogram,outline=TRUE,col="grey",xlab="Simulation time", ylab="Rainfall [mm/h]", #KEEPS OUTLIERS
        #        main=paste0("Meteogram for ",gauge_meteogram), ylim=c(0,graph_stats[1]), xaxt = "n",
        #        medlwd = 1.5, boxlty = 0, whisklty = 3, staplelwd = 1, outpch = 8, outcex = .7) 
        #axis(side=1, at=seq(from=8,to=48,by=8), labels=c("08:00","16:00", "00:00", "08:00", "16:00", "00:00"))
        #points(1:ncol(WRFvalues_formeteogram),OBSRAINvalues_formeteogram,col="blue",pch=19, cex=.7)
        #legend(graph_stats[2],graph_stats[3],legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
        #dev.off()
        
        #Square
        dateiname <- paste0(fss_path,"/Meteogram_",gauge_meteogram,".png",sep="") # Version 4
        png(file=dateiname, width = 5.5, height = 4, units="in", res=1200)
        par(cex.axis = 1.15, cex.lab= 1.15)
        boxplot(WRFvalues_formeteogram,outline=TRUE,col="grey",xlab="Simulation time", ylab="Rainfall [mm/h]", #KEEPS OUTLIERS
                main=gauge_meteogram, ylim=c(0,graph_stats[1]), xaxt = "n",
                medlwd = 1.5, boxlty = 0, whisklty = 3, staplelwd = 1, outpch = 8, outcex = .7) 
        axis(side=1, at=seq(from=8,to=48,by=8), labels=c("08:00","16:00", "00:00", "08:00", "16:00", "00:00"))
        points(1:ncol(WRFvalues_formeteogram),OBSRAINvalues_formeteogram,col="blue",pch=19, cex=.5)
        dev.off()
        
        boxplot(WRFvalues_formeteogram,outline=TRUE,col="grey",xlab="Time steps", ylab="Rainfall [mm/h]", 
                main=paste0("Meteogram for ",gauge_meteogram)) 
        points(1:ncol(WRFvalues_formeteogram),OBSRAINvalues_formeteogram,col="blue",pch=19)
        #legend(1,max(METEOGRAM_values)*0.75,legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
        legend(36,floor(max(METEOGRAM_values)),legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
        
        cat(sprintf("Meteogram created for gauge %s\n\n",gauge_meteogram))
        
        #Average difference between median of WRF simulations (for alltsteps, 2, 3 and 4 acc hours) and observed simulation
        diff <- rep(NA,4)
        diff[1] <- mean(OBSRAINvalues_formeteogram - (apply(WRFvalues_formeteogram,2,mean)))
        
        for (acc_hrs in 2:4) {
          
          num_boxes <- ncol(METEOGRAM_values)/acc_hrs
          METEOGRAM_values_acc <- matrix(NA,ncol=num_boxes,nrow = nrow(METEOGRAM_values))
          row.names(METEOGRAM_values_acc) <- rownames(METEOGRAM_values)
          
          dateiname_acc <- paste0(fss_path,"/Meteogram_acc_",acc_hrs,"hrs_",gauge_meteogram,".png",sep="")
          
          for (m in 1:num_boxes){
            col_indexes <- c((m*acc_hrs-acc_hrs+1):(m*acc_hrs))
            METEOGRAM_values_acc[,m] <- as.vector(apply(METEOGRAM_values[,col_indexes],1,sum)) 
          }
          
          #png(file=dateiname_acc, width = 800, height = 600,pointsize = 18)
          #cielo<-max(METEOGRAM_values_acc)*1.1
          #boxplot(METEOGRAM_values_acc[2:(length(wrf_to_analyse)+1),],outline=TRUE,col="grey",xlab="Time steps", ylab="Rainfall [mm/h]", 
          #        main=paste0("Meteogram for ",gauge_meteogram),ylim=c(0,graph_stats[1])) 
          #points(1:ncol(METEOGRAM_values_acc),METEOGRAM_values_acc[1,],col="blue",pch=19)
          #legend(length(METEOGRAM_values_acc[1,])*0.8,graph_stats[3],legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
          #dev.off()
          
          tick_interval <- 6
          tick_labels <- c("08:00","16:00", "00:00", "08:00", "16:00", "00:00")
          if (acc_hrs == 3) {
            tick_interval <- 4
            tick_labels <- c("12:00","00:00", "12:00", "00:00")
          }
          
          png(file=dateiname_acc, width = 7, height = 4, units="in", res=800)
          cielo<-max(METEOGRAM_values_acc)*1.1
          boxplot(METEOGRAM_values_acc[2:(length(wrf_to_analyse)+1),],outline=TRUE,col="grey",xlab="Simulation time", ylab="Rainfall [mm/h]", 
                  main=paste0("Meteogram for ",gauge_meteogram),ylim=c(0,graph_stats[1]), xaxt="n",
                  medlwd = 1.5, boxlty = 0, whisklty = 3, staplelwd = 1, outpch = 8, outcex = .7) 
          axis(side=1, at=seq(from=(num_boxes/tick_interval),to=num_boxes,by=(num_boxes/tick_interval)), labels=tick_labels)
          points(1:ncol(METEOGRAM_values_acc),METEOGRAM_values_acc[1,],col="blue",pch=19, cex=.7)
          legend(length(METEOGRAM_values_acc[1,])*0.8,graph_stats[3],legend=c("OBS rain","WRF sim") ,col=c("blue","grey"),lty=c(0,1),pch=c(19,NA))
          dev.off()
          
          #write.table(METEOGRAM_values_acc, paste0(fss_path,"/Meteogram_acc_",acc_hrs,"hrs_",gauge_meteogram,".txt",sep=""),sep="\t",eol="\n",col.names = FALSE)
          diff[acc_hrs] <- mean(METEOGRAM_values_acc[1,] - (apply(METEOGRAM_values_acc[2:nrow(METEOGRAM_values),],2,mean)))
          
        } # 2 to 4 accumulated hours
        
        contador <- contador + 1
        performance[[contador]] <- diff
        gauges_clean[[contador]] <- gauge_meteogram
        max_values[[contador]] <- max(OBSRAINvalues_formeteogram)
        
      } # If WRF cell is not zeros.
      
    } # If gauge doesn't contain Nans 
    
  } # If gauge covers selected dates
  
} # For all gauges

perform <- do.call(rbind,performance)
gauges_cleaned <- as.vector(do.call(rbind,gauges_clean))
maxrfvalues <- as.vector(do.call(rbind,max_values))

COORDS <- matrix(NA,nrow = length(gauges_cleaned), ncol = 2)
for (ii in 1:length(gauges_cleaned)) {
  indexxx <- which(gauges_cleaned[ii] == as.vector(base_df$GAUGE_NAME))
  COORDS[ii,1] <- base_df$EASTING[indexxx]
  COORDS[ii,2] <- base_df$NORTHING[indexxx]
}

perform <- cbind(perform, COORDS, maxrfvalues)
row.names(perform) <- gauges_cleaned
colnames(perform) <- c("All_tsteps","Acc2hrs","Acc3hrs","Acc4hrs","EASTING","NORTHING","MaxOBSrain")
write.table(perform,paste0(fss_path,"/Meteogram_stats.txt"),col.names = TRUE)
