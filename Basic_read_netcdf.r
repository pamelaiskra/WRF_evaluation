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

