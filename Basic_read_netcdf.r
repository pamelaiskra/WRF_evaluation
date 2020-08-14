install.packages("ncdf4")
library(ncdf4)

# ======= VARIABLES

rm(list=ls())
mync <- nc_open("my_file.nc")
#mync <- nc_open("fuse_catch/input/us_09066300_input.nc")
#mync <- nc_open("wrf2")

# * Run this part to check the variables of the NetCDF file *
mync_var  <- attributes(mync$var)$names
mync_dim  <- attributes(mync$dim)$names

# Select a variable from mync_var (e.g. "RAINNC")
var_name <- "RAINNC"

# Z variable (Time or other that's not lat or lon)
z_var <- mync_dim[1]

# ====== CODE

cat("The file has",  mync$nvars, "variables which are '", mync_var,
    "'\nIt has", mync$ndims, "dimensions which are '", mync_dim, "'")

# ------ Checking the dimensions of the NetCDF file

dummy <- ncvar_get(mync, mync_var[2])

# If it contains multiple rows and columns

mync_rows <- dim(dummy)[2]    # latitude  (rows)
mync_cols <- dim(dummy)[1]    # longitude (cols)
mync_layr <- dim(dummy)[3]    # 3rd dimension band

# If it only contains one row and one column (i.e. info is in a vector
# where each element is a time step)
if (is.na(ncol(dummy)*nrow(dummy))==TRUE) {
  mync_rows <- 1    # longitude
  mync_cols <- 1    # latitude
  mync_layr <- length(dummy)    # 3rd dimension band
}

#Time units (the number must correspond to "Time" or Z in mync_dim)
z_units <- ncatt_get(mync,z_var,"units")$value
if (is.null(z_units) == TRUE) z_units <- "Time steps"

# Search the name of the variable
var_index <- which(mync_var==var_name)
# Select all rows, all columns and all tsteps for that variable
var <- ncvar_get(mync, mync_var[var_index])
var_units <- ncatt_get(mync,mync_var[var_index],"units")$value

# var_name can be changed for the long name, if it exists
#var_name <- ncatt_get(mync,mync_var[var_index],"long_name")$value

# ------ Plotting

xxlab <- paste0(z_var," [",z_units,"]")
yylab <- paste0(mync_var[var_index]," [",var_units,"]")

# A) If NetCDF file has 1 row and 1 column

plot(var, main=var_name, type="l",col="blue",
     xlab=xxlab,ylab=yylab)

# B) If NetCDF file has MORE than 1 row and 1 column

# Select which cells (rows and columns)
# Note: number of rows and columns are given by mync_rows and mync_cols
spatial_rows <- c(20:22)
spatial_cols <- c(30:32)

# Extract information for those cells

subset <- var[spatial_rows,spatial_cols,]

plot(subset[1,1,], main=var_name,type="l",col="red",
     xlab=z_units,ylab=var_units)
for (j in 1:length(spatial_rows)){ 
  for (k in 1:length(spatial_cols)){ 
    lines(subset[j,k,], col="red")
  }
}

  
# C) If NetCDF has only one value (1 row, 1 col, 1 layer)

#Select variables
indexes <- c(96:101)

# Here are the values
extract_values <- function(indexx, myncc=mync, mync_varr=mync_var) {
  varr <- ncvar_get(myncc, mync_varr[indexx])
  return(varr)
}
myvalues <- do.call(rbind,lapply(indexes, extract_values))

# Here are the short names of the variables
short_names <- mync_var[indexes]

# Here are the long names of the variables
long_names <- c(1:length(indexes))
for (i in 1:length(indexes)) long_names[i] <- ncatt_get(mync,mync_var[i],
                                                        "long_name")$value
# HEre is the dataframe
valuesvf <- data.frame(NameVar=short_names,ValVar=myvalues)

plot(valuesvf$ValVar, main="Title", xaxt ="n",
     xlab="X values",ylab="Y values")
axis(1, labels = FALSE)
# Plot x labs at default x position
text(x =  seq_along(short_names),
     y = par("usr")[3] - (par("usr")[4] - par("usr")[3])/30,
     srt = 45, adj = 1, labels = short_names, xpd = TRUE)

# OR
ggplot(valuesvf, aes(x=NameVar,y=ValVar)) +
  geom_point() +
  labs(x = "Units X", y=expression(paste("algo"^"otroalgo"," otro"^"lala")), 
       title="Plot", subtitle="Subtitle") +
  theme( axis.text.x  = element_text(angle=345))
  
