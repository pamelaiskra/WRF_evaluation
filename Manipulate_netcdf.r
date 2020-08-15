install.packages("ncdf4")
library(ncdf4)
install.packages("VIM")
library(VIM)

rm(list=ls())
setwd()

# ================================================
#                 P A R T   1
# ================================================

mync <- nc_open("wrf2")
mync <- nc_open("data_part_1.nc")

# Variables of the NetCDF file 
mync_var  <- attributes(mync$var)$names
mync_dim  <- attributes(mync$dim)$names

# 1.1a) Period of time
sink("test.txt", append=TRUE)
print(mync)
sink()

# 1.1b) spatial resolution
#REsolution
lon <- ncvar_get(mync, mync_dim[3]) # 285
lat <- ncvar_get(mync, mync_dim[4]) # 165
lay <- ncvar_get(mync, mync_dim[1])

# 1.2) Map of temperature for third day
uu <- ncvar_get(mync, mync_var[3])
uu2 <- uu[,,3]
matrixplot(t(uu2))

# 1.3) Temperature for upper and lower half, for all period
upper <- uu[1:84,,]
lower <- uu[85:165,,]

upper_t <- list()
for (i in 1:6) {
  upper_t[[i]] <- mean(upper[,,i])
}
upper_t <-do.call(rbind,upper_t)

lower_t <- list()
for (i in 1:6) {
  lower_t[[i]] <- mean(lower[,,i])
}
lower_t <-do.call(rbind,lower_t)

plot(upper_t,col="red",main="Temperature",type="l",ylim=c(-5,2))
lines(lower_t,col="blue")

# ================================================
#                 P A R T   2
# ================================================

rm(list=ls())
setwd("C:/Users/Andrés Mejía/Downloads/R_Pam/")
mync <- read.table("data_part_2.txt")
myt <- data.frame(mync)

#Select pertinent columns
myt <- myt[,c(2,6)]

#Convert values <= 1 mm to NA
myt[(which(myt[,2] <= 1)),2] <- NA 

#Delete top row and rename rows
myt <- myt [2:nrow(myt),] 
#myt <- myt[11130:11150,]
row.names(myt) <- c(1:nrow(myt))

# ============ VARIABLES =============

# Define the format of the names of the months
months_name <- c(1:12) 
# Or, for example
# months_nom <- month.abb

# Define the vector with the months
# Name of month will be repeated as many times as the temporal resolution
# For daily time series for several years, months_loc will be 
# "Jan" x 31, "Feb" x 28... OR "1" x 31, "2" x 28, etc
months_loc <- as.numeric(myt[,1])

# !!! IMPORTANT !!!
# months_name and months_loc must have the same type of variable

# Define the vector with the indexes of the days with NA
vindex <- which(is.na(myt[,2]))

# Define length of the events [in days] to analyse
durations <- c(3,5,9)

# ============ CODE =============

# ------ OPTION 1: FUNCTIONS

# Function that calculates the INDEX of n-day events
# Index: day when the n-day event started
index_events <- function(myvec, nday) {
  indexx <- list()
  contador <- 1
  for (i in 2:length(myvec)) {
#diff is the length of the interval between the index of NA values
#i.e. the difference between one index and the one that follows
#is the duration of the event
    diff <- myvec[i]-myvec[i-1]
    if (diff == (nday + 1)) {
      indexx[[contador]] <- myvec[i-1] +1
      contador <- contador + 1
    }
  }
  result <- do.call(rbind,indexx)
  return(result)
}

# Function that calculates the NUMBER of n-day events per season
num_events_season <- function(myvec) {
  indexes <- index_events(myvec, nday)
  # Indexes of the days when the n-day events started
  # months_loc is the vector with all the months in the timeseries
  seasonal <- months_loc[indexes]
  # How many times the months appear
  # According to the format given by months_name
  DJF <- sum(seasonal == months_name[12]) +
    sum(seasonal == months_name[1]) + sum(seasonal == months_name[2])
  MAM <- sum(seasonal == months_name[3]) + 
    sum(seasonal == months_name[4]) + sum(seasonal == months_name[5])
  JJA <- sum(seasonal == months_name[6]) + 
    sum(seasonal == months_name[7]) + sum(seasonal == months_name[8])
  SON <- sum(seasonal == months_name[9]) + 
    sum(seasonal == months_name[10]) + sum(seasonal == months_name[11])
  
  seasonal <- c(DJF,MAM,JJA,SON)
  return(seasonal)
}

# ------ OPTION 2: NO FUNCTIONS

#Obtain indexes of NA values
vindex <- which(is.na(myt[,2]))

#Check that there is a difference of 6 between indexes (5-day storm)
event5 <- list()
contador <- 1
for (i in 2:length(vindex)) {
  diff <- vindex[i]-vindex[i-1]
  if (diff == nday + 1) {
    event5[[contador]] <- vindex[i-1] +1
    contador <- contador + 1
  }
}

no_events5 <- length(do.call(rbind,event5))
index5 <- do.call(rbind,event5)
month5 <- as.numeric(sort(myt[index5,1]))
DJF5 <- sum(month5 == 12) + sum(month5 == 1) + sum(month5 == 2)
MAM5 <- sum(month5 == 3) + sum(month5 == 4) + sum(month5 == 5)
JJA5 <- sum(month5 == 6) + sum(month5 == 7) + sum(month5 == 8)
SON5 <- sum(month5 == 9) + sum(month5 == 10) + sum(month5 == 11)

event_per_month <- c(DJF5,MAM5,JJA5,SON5)

# ------ DISPLAY TOTALS

cat("For",nday,"-day duration events, there are",event_per_month[1],
    "events in DJF,",event_per_month[2],"in MAM,", event_per_month[3],
    "in JJA and",event_per_month[4], "in SON")

# ============ TASKS =============

matrix_results <- matrix(NA,nrow=length(durations),ncol=5)
for (j in 1:length(durations)) {
  matrix_results[j,1] <- durations[j]
  nday <- durations[j]
  matrix_results[j,2:5] <- num_events_season(vindex)
}

colnames(matrix_results) <- c("Duration of event","DJF","MAM","JJA","SON")

# ============ PLOT =============

labelss <- c("DJF","MAM","JJA","SON")
colourss <- c("blue","red","black","green")

# Create legend
legendd <- c(1:length(durations))
for (m in 1:length(durations)) {
  legendd[m] <- paste0(durations[m],"-day events")
}

# Plot
plot(matrix_results[1,2:5], xaxt ="n", xlab="Season",
     ylab="# of n-day events", ylim=c(0,max(matrix_results)),
     pch=16,col=colourss[1])
axis(1,at=seq_along(labelss),tick = TRUE,labels = labelss)
for (k in 2:nrow(matrix_results)) {
  points(matrix_results[k,2:5],pch=16,col=colourss[k])
}
legend("center", legendd,fill=colourss[1:length(durations)])

