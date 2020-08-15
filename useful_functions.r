#USeful functions

list.files()
file.create("file_chars.txt")
sink("file_chars.txt", append=TRUE)
mync
sink()

#Attributes
z <- ncatt_get(mync,"pr")    # 3rd dimension ban
bands <- ncatt_get(mync,"time","units")$value
#zeit comes from mync_dim o mync_var

lala <- function (x, y=mync_layr) {
  yo=y-x
  return(yo)
}

lala <- function (x, y=mync_layr) {
  y-x
}

ggplot(data=aamelt, aes(x=tstep, y=value, group=variable)) + 
  geom_line() +
  geom_line(aes(x=c(1:(ncol(aa)-1)),y=aamean,colour="aaMean"),size=0.8) +
  scale_color_manual(values=c("#B46D6D", "#6DC7CB", "#626262"))
