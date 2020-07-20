#This is Hourly-rain_obs-vs-sim
#This code creates a meteogram for all the gauges in the domain
# only for gauges WITHOUT NaNs and that cover slected dates.
# It also calculates the meteogram for 2, 3 and 4-hour period 
# This code is Hourly-rain_obs-vs-sim.r
#install.packages("ggplot2")
#library("ggplot2")
#library(matrixStats)
#library(reshape)

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
wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/NewcastleWRF")
#wrf_path <- ("F:/DECIPHeR/Hourly_Rainfall/BirminghamWRF_")


# Define prefix of simulated rainfall files per cell
wrf_prefix <- ("rainfall_WRF_gridID")

# Define full path of where the skill score files will be located
#fss_path <- ("F:/F_Newcastle/Meteograms_UCM_VF/Rain_21-25b-28b_THOM") #Filder has to be created manually
#fss_path <- ("F:/F_Birmingham/Meteograms_UCM_VF/Rain_A-B-D_SLUCM")
#Rain_A-B-D_SLUCM
#Rain_Eb-Fb-Gb_BEP
#Rain_Hb-Ib-Jb_BEM

anfang <- c(2012,6,28,1)   #Newcastle
#anfang <- c(2007,7,20,1) #Birmingham
tage <- 2                                                                          # Days in the simulation
stunde <-0                                                                         # Hours (in addition to days) in the simulation
wrf_to_analyse <- c(21,"25b","28b")

#Thompson: 21, 25, 28. A, E, Hb
#WRSM6: 22, 26b, 29b. B, Fb, I
#Morrison: 24, 27b, 30b. D, Gb, Jb
#NOTE: If plotting sth other than grouping sims by UCM, make sure to modify headers of the dataframe to plot (line 146)

#Select for which gauge the will be meteograms
#(in case you just want to plot LESS THAN TEN stations)
gauge_meteogram<-c("EA_06_alston_s_wks_auto_qc.txt", "EA_06_chirdon_logger_qc.txt","EA_06_crewe_fell_f.h._tel_qc.txt",
                   "EA_06_greenhills_farm_auto_qc.txt","EA_06_jesmond_dene_logger_qc.txt","EA_06_long_meadows_qc.txt",
                   "EA_06_marine_first_school_qc.txt","EA_06_noonstones_hill_qc.txt","EA_06_howdon_s_wks_no_2_auto_qc.txt",
                   "EA_06_tunstall_resr_auto_qc.txt","EA_06_washington_s_wks_qc.txt","EA_06_wileysike_gland_tel_qc.txt")

gauge_meteogram <- c("EA_06_byfield_stw_auto_qc.txt","EA_06_draycott_rain_qc.txt","EA_06_hollies_qc.txt",
                     "EA_06_littlethorpe_rain_qc.txt","EA_06_overseal_qc.txt","EA_06_rfh_tbr_qc.txt",
                     "EA_06_saltley_qc.txt","EA_06_sheriffs_lench_qc.txt","EA_06_staythorpe_qc.txt",
                     "EA_06_the_bratch_qc.txt","EA_06_waseley_hills_qc.txt")

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
        
        METEOGRAM_df <- data.frame(tstep=1:48,OBS=METEOGRAM_values[1,],THOM=METEOGRAM_values[2,],WSM6=METEOGRAM_values[3,],MORR=METEOGRAM_values[4,])
        METEOGRAM_df2<- melt(METEOGRAM_df,id.vars="tstep")
        farbe2 <- c("blue","#39FF33","#FF5B33","#B61BFF")
        #farbe2 <- c("blue","#B67915") #For Sim23
        dateiname <- paste0(fss_path, "/Rain_", gauge_meteogram,".png")
        
        
        ggplot(METEOGRAM_df2,aes()) +
          geom_line(aes(x=tstep, y=value, colour=variable, linetype=variable), size = 0.8) +
          geom_point(aes(x=tstep, y=value,color=variable), size=2.5) +
          scale_linetype_manual(values=c("dashed", "solid","solid", "solid")) + #linetypes
          scale_color_manual(values=farbe2) +                        # line  colours
          labs(x = "\nSimulation time [h]", y="Accumulated rainfall [mm]\n", title=gauge_meteogram) +
          scale_x_continuous(breaks=c(8,16,24,32,40,48),labels=c("08:00", "16:00", "00:00", "08:00", "16:00", "00:00")) +
          ylim(0,50) +
          theme_bw() +
          theme(plot.title = element_text(size=25),
                axis.title.x = element_text(size=20),
                axis.text.x  = element_text(size=20),
                axis.title.y = element_text(size=20), 
                axis.text.y  = element_text(size=20, angle=90, hjust=0.5, vjust=4),
                axis.ticks.length = unit(0.35, "cm"),
                legend.title = element_blank(),
                legend.text = element_text(size=25),
                legend.position = c(0.14,0.82),
                panel.border = element_rect(colour="black", fill=NA, size=1.25),
                panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          ggsave(dateiname, width = 9, height = 7, dpi=600)
        
        
        cat(sprintf("Rain plot created for gauge %s\n\n",gauge_meteogram))
        
        #Average difference between median of WRF simulations (for alltsteps, 2, 3 and 4 acc hours) and observed simulation
        diff <- rep(NA,4)
        diff[1] <- mean(OBSRAINvalues_formeteogram - (apply(WRFvalues_formeteogram,2,mean)))
        
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

####-miniplot
#max_rain <- which(METEOGRAM_df$OBS == max(METEOGRAM_df$OBS)) #max value
#range_miniplot <- c((max_rain-3):(max_rain+3)) #create window around max value
#horas <- c(0,4,8,12,16,20) #time stamps will be placed in this hours
#breaks_miniplot <- which(range_miniplot %in% horas) #from the range, position of the timestamps
#timestamps <- rep(NA, length(breaks_miniplot)) #blank vector of timestamps
#for (ii in 1:length(breaks_miniplot)) {
#  timestamps[ii] <- paste0(range_miniplot[breaks_miniplot[ii]],":00") #Timestamps
#} 

#METEO_df_miniplot <- METEOGRAM_df[range_miniplot,]

#ggplot(METEO_df_miniplot,aes()) +
#  geom_line(aes(x=1:7, y=OBS, colour="Obs rain"), size = 0.8) +
#  geom_line(aes(x=1:7, y=SLUCM, colour="SLUCM"), size = 0.8) +
#  geom_line(aes(x=1:7, y=BEP, colour="BEP"), size = 0.8) +
#  geom_line(aes(x=1:7, y=BEM, colour="BEM"), size = 0.8, linetype="dashed") +
#  scale_x_continuous(breaks=breaks_miniplot,labels=timestamps) +
#  ylim(0,50) +
#  theme_bw() +
#  scale_color_manual(values=c("blue", "green", "#FF5B33","#B61BFF")) +  
#  labs(y="Accumulated rainfall [mm]\n") +
#  theme(plot.title = element_text(size=25),axis.title.x = element_blank(),axis.text.x  = element_text(size=20),axis.title.y = element_text(size=20), axis.text.y  = element_text(size=20, angle=90, hjust=0.5, vjust=4),axis.ticks.length = unit(0.35, "cm"),legend.position = "none",panel.border = element_rect(colour="black", fill=NA, size=1.25),panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank())
