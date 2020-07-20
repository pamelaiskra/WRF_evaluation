install.packages("ncdf4")
library(ncdf4)

rm(list=ls())

#-------------CHANGE THESE VARIABLES------------
  
  #ncdf_filename_input <- ("E:/E_Newcastle/WRF_version4/WRF18/wrfout_d04_2012-06-27_12%3A00%3A00")
  #ncdf_filename_input <- ("E:/E_Newcastle/SIMS_DOMINIO_COREGIDO/1_DC/wrfout_d04_2012-06-27_12%3A00%3A00")
  #ncdf_filename_input <- ("E:/E_Newcastle/delayed1/1delayed48/wrfout_d04_2012-06-26_00%3A00%3A00")
  #ncdf_filename_input <- ("F:/F_Newcastle/ALL_DELAYED/1delayed6_d3d4/wrfout_d04_2012-06-27_18%3A00%3A00") #delayed
  #ncdf_filename_input <- ("F:/F_Newcastle/NewcWRF21/wrfout_d04_2012-06-27_12%3A00%3A00")
  ncdf_filename_input <- ("F:/F_Birmingham/BirWRF_G/wrfout_d04_2007-07-19_12%3A00%3A00")
  
  #arbeitverzeichnis <- ("E:/DynaTOPMODEL/DECIPHeR/Hourly_timeseries/Hourly_rainfall_WRF/WRF_2km_sim18") # Folder of the raingrid timeseries
  #arbeitverzeichnis <- ("E:/E_Newcastle/delayed_ts/WRF1delayed48")
  #arbeitverzeichnis <- ("F:/DECIPHeR/Hourly_Rainfall/NewcastleWRF21")
  arbeitverzeichnis <- ("F:/DECIPHeR/Hourly_Rainfall/BirminghamWRF_G")
  
  num_tsteps_per_hour <- 1
  #num_tsteps_per_hour <- 2 #delayed
  
  #Write initial and end dates. KEEP THE FORMAT
  #Initial time should be when accumulated velues should start
  #e.g. WRF started running at 12:00, so time in "anfang" should be 13:00
  anfang <- as.POSIXct( "2012-06-27 19:00:00", tz = "UTC")    # Newc delayed 6 hr
  endung <- as.POSIXct( "2012-06-29 00:00:00", tz = "UTC")  
  #anfang <- as.POSIXct( "2012-06-26 01:00:00", tz = "UTC")   # newc delayed 24 hr
  #endung <- as.POSIXct( "2012-06-29 00:00:00", tz = "UTC")   
  #anfang <- as.POSIXct( "2012-06-27 13:00:00", tz = "UTC")   # Newcastle
  #endung <- as.POSIXct( "2012-06-30 00:00:00" , tz = "UTC")
  anfang <- as.POSIXct( "2007-07-19 13:00:00", tz = "UTC")   # Birmingham
  endung <- as.POSIXct( "2007-07-22 00:00:00" , tz = "UTC")
  
  #To sum RAINC + RAINNC write 0, to use only RAINNC write 1
  antwort <-1

#--------------------------- 
  
  nc <- nc_open(ncdf_filename_input, write=FALSE)
  cat("The file has",  nc$nvars, "variables", nc$ndims, "dimensions and",  nc$natts, "attributes.")
  wrf_variablen <- attributes(nc$var)$names
  rainnc_wrf <- ncvar_get(nc, wrf_variablen[which(wrf_variablen=="RAINNC")])
  rainc_wrf <- ncvar_get(nc, wrf_variablen[which(wrf_variablen=="RAINC")])
  
  if(antwort==0) rain_wrf <- rainnc_wrf + rainc_wrf
  if(antwort==1) rain_wrf <- rainnc_wrf
  
  zeilen<-(dim(rain_wrf)[2]) #rows
  spalten<-(dim(rain_wrf)[1]) #columns
  hours_in_sim <- ((dim(rain_wrf)[3]-1)/num_tsteps_per_hour) #36
  
#Note that timestep=0, or vector[1] from WRF output corresponds to a void layer
#    the "additional" hour in the for loop is actually the last hour that has been "added" because of this void layer
#    the for loop starts in 2 because the first hour starts at timestep 2.  
#    vector_final starts at [i-1] to store the values of the for loop in the hour that corresponds
#    e.g. i=3 corresponds to second hour so it's stored in vector_final[i-1] i.e. in vector_final[2]
    
create_hourly_vector<- function(myvector){
    n_timesteps <- length(myvector)
    vector_final<-numeric(hours_in_sim)
    #vector_final[1] <- myvector[1]
    
    for (i in 2:hours_in_sim+1){
        indice_final <- (i-1)*num_tsteps_per_hour + 1     # timestep at the end of the hour#
        indice_inicial <- indice_final-num_tsteps_per_hour   # timestep at the end of the previous hour
        vector_final[i-1] <- myvector[indice_final] - myvector[indice_inicial]
      }
    return(vector_final)
  }
 
create_hourly_matrix <- function(threedim_matrix){
  zeilen<-(dim(threedim_matrix)[2])
  spalten<-(dim(threedim_matrix)[1])
  final_matrix <- matrix(list(),zeilen,spalten)

  for (j in 1:zeilen){ 
    for (k in 1:spalten){ 
      vvector <- threedim_matrix[k,j,] #dim(my_matrix) = cols | rows | z
      final_matrix[j,k][[1]] <- create_hourly_vector(vvector) 
    }
  }
  return(final_matrix)
}

  wrf_hourly_matrix<- create_hourly_matrix(rain_wrf)


#write reference date file #1990 1 1 10 NaN
alles_datum <- seq(from = anfang, to = endung, by = "hour")    # by hour
datum_list <- strsplit(as.character(alles_datum), '[- :]+')    # splits all characters inside the [ ]+
lista3<-list()                                                 # converts all characters to numeric, one by one  
for (i in 1:length(datum_list)){
  lista3[[i]] <- as.numeric(datum_list[i][[1]][])
}
datum_cells <- do.call(rbind, lista3)                          # puts all nor numeric values in a list
Datum <- datum_cells[,c(1:4)]                                  # leaves (Y, m, d, h)

#Small test
#zorrito <- wrf_hourly_matrix[5,2][[1]] 
#final <- cbind(Datum, zorrito)

setwd(arbeitverzeichnis)

temporall <- list()
for (m in 1:zeilen){
  for (n in 1:spalten){
    aa <- (m-1)*spalten+(n-1)
    if (aa==0){
      print('Skipping cell(0,0)')
    } else {
      cat(sprintf('Writing file %i of %i\n', aa, (zeilen*spalten-1)))
      titulo <- sprintf("rainfall_WRF_gridID%i.txt", aa)
      tabla <- cbind(Datum,wrf_hourly_matrix[m,n][[1]]  )
      write.table(tabla,titulo, row.names = FALSE, col.names = FALSE)
    }
  }
}

rm(i, m, n, temporall, zeilen)


#schreiben <- do.call(rbind, temporall)

#Vector to text file
#arbeitverzeichnis <-"E:/DynaTOPMODEL/WRF" 
#setwd(arbeitverzeichnis)
#zorrito <- wrf_hourly_matrix[1,2][[1]] 
#matriz_prueba <- cbind(c(1:36), c(37:72), zorrito)
#write.table(matriz_prueba, file="mi archivo.TXT", row.names = FALSE, col.names = FALSE)

#sipues <- ("2000-01-01 02:00:00")
#nopues <- strsplit(as.character(sipues), '[- :]+')

#thedate2 = ISOdate(2005,9,28,18,tz="UTC")
#format(thedate2,'%Y %m %d %H')
#lista <- strsplit(as.character(seq(anfang, endung, by="days")), '-') #strplit(as character(lista de fechas), 'caracter')
#lista3 <- list()
#for (i in 1:length(lista)){
#  lista3[[i]] <- as.numeric(lista[i][[1]][])
#}
#Datum <- do.call(rbind, lista3)



#------Don't use this.
#to_hourly<- function(myvector){
#  n_timesteps <- length(myvector)
#  num_tsteps_per_hour <- 4
#  hours_in_sim <- ((n_timesteps-1)/num_tsteps_per_hour)+1
  
#  valores<-numeric(hours_in_sim)
#  valores2<-numeric(hours_in_sim)
#  valores2[1] <- myvector[1]
  
#  for (i in 2:hours_in_sim){
#    eins <- (num_tsteps_per_hour*(i-1)) - 2
#    zwei <- (num_tsteps_per_hour*(i-1)) - 1
#    drei <- (num_tsteps_per_hour*(i-1))
#    vier <- (num_tsteps_per_hour*(i-1)) + 1
#    valores[i]<- myvector[eins]+myvector[zwei]+myvector[drei]+myvector[vier]
#    valores2[i]<- valores[i]-valores[i-1]
#  }
#  return(valores2)
#}

#------------Some stuff

#der_test <- rainnc_wrf[1,1,]
#bbb <- to_hourly(der_test)

#to_hourly(test)
#uno<- capture.output(to_hourly(test))
  
#------------Parameters used to run Newcastle with corrected domain
#ncdf_filename_input <- ("E:/E_Newcastle/SIMS_DOMINIO_COREGIDO/5_DC/wrfout_d04_2012-06-27_12%3A00%3A00")
#arbeitverzeichnis <- ("E:/DynaTOPMODEL/WRF_rainfall_vf/WRF_2km_sim5") # Folder of the raingrid timeseries
#nc <- nc_open(ncdf_filename_input, write=FALSE)
#cat("The file has",  nc$nvars, "variables", nc$ndims, "dimensions and",  nc$natts, "attributes.")
#wrf_variablen <- attributes(nc$var)$names

#rainnc_wrf <- ncvar_get(nc, wrf_variablen[112])
#rainc_wrf <- ncvar_get(nc, wrf_variablen[])
#rain_wrf <- rainnc_wrf + rainc_wrf

#zeilen<-(dim(rain_wrf)[2])
#spalten<-(dim(rain_wrf)[1])

#num_tsteps_per_hour <- 4
#hours_in_sim <- ((dim(rain_wrf)[3]-1)/num_tsteps_per_hour) #36

#--------------Ord function
#create_hourly_vector<- function(myvector){
#  n_timesteps <- length(myvector)
#  vector_final<-numeric(hours_in_sim)
#  vector_final[1] <- myvector[1]
  
#  for (i in 2:hours_in_sim){
#    indice_inicial <- (i-1)*num_tsteps_per_hour - 2   # timestep at the beginning of the hour
#    indice_final <- (i-1)*num_tsteps_per_hour + 1     # timestep at the end of the hour
#    vector_final[i] <- myvector[indice_final] - myvector[indice_inicial]
#  }
#  return(vector_final)
#}