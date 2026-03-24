
# Read LGD arrival stats for each site each year
arriv <- read.csv("granite_arrival_dates.csv", header=T, as.is=TRUE)

# Read site info to get distance to LGD; add info for coloring points
sites <- read.csv("sites.csv", header=T, as.is=TRUE)
colors <- c("#9a86d6", "#60a0dc", "#e97233", "#43bbad", "skyblue", "black")
sites$color.ix <- c(5,5,2,5,2,4,2,1,3,2,2,3,3,2,1,4)


# Put arrival stats in year-by-sites matrices
med.arriv.year.by.site <- matrix(arriv[,4], ncol=16, byrow=T)
q10.arriv.year.by.site <- matrix(arriv[,3], ncol=16, byrow=T)
q90.arriv.year.by.site <- matrix(arriv[,5], ncol=16, byrow=T)
dimnames(med.arriv.year.by.site) <- 
dimnames(q10.arriv.year.by.site) <- 
dimnames(q90.arriv.year.by.site) <- list(1993:2023, arriv[1:16,1])

id <- arriv[1:16,1]    # for distance lookup below

# Calculate stats needed for plotting and put them in a data.frame
arriv.by.site.dat <- data.frame(
  site_id = id,                                                            # site_id labels points in plot
  distance = sites$Dist[match(id, sites$Site_ID)],                         # lookup distance in sites info
  color.ix = sites$color.ix[match(id, sites$Site_ID)],                     # lookup color in sites info
  n.years = apply(!is.na(med.arriv.year.by.site),2,sum),                   # number of years for each site
  med.of.med = apply(med.arriv.year.by.site,2,median,na.rm=T),             # median of n.years median arrival
  med.of.q10 = apply(q10.arriv.year.by.site,2,median,na.rm=T),             # median of n.years 10%ile arrival
  med.of.q90 = apply(q90.arriv.year.by.site,2,median,na.rm=T),             # median of n.years 90%ile arrival
  q10.of.med = apply(med.arriv.year.by.site,2,quantile,probs=0.1,na.rm=T), # 10%ile of n.years median arrival
  q90.of.med = apply(med.arriv.year.by.site,2,quantile,probs=0.9,na.rm=T)) # 90%ile of n.years median arrival


# Draw frame for plot elements 
plot(x=arriv.by.site.dat$med.of.med, y=arriv.by.site.dat$distance, xlim=c(103,148), ylim=c(400,770),
         type="n", xaxt="n", yaxt="n", yaxs="i",
         xlab="Arrival Date at LGR",ylab="Distance Release to Lower Granite Dam (km)")

# Add axis labels
xticks <- c(95,100,105,110,115,120,125,130,135,140,145,150,156,161)
axis(1, at=xticks,
        labels=c("", "Apr 10", "", "Apr 20", "", "Apr 30", "", "May 10", "", "May 20", "", "May 30", "", "Jun 10"))  
axis(2, at=seq(400,750,50), labels=seq(400,750,50))

# Dashed vertical grid lines
abline(v=xticks, lty=2, col="gray75")
# Dashed horizontal grid lines
abline(h=seq(450,750,50), lty=2, col="gray75")

# Add black arrows between 10%ile and 90%ile of median arrival (before points so that points don't get overwritten)
arrows(x0=arriv.by.site.dat$q10.of.med, y0=arriv.by.site.dat$distance, 
       x1=arriv.by.site.dat$q90.of.med, y1=arriv.by.site.dat$distance, code=3, angle=90, length=0.03)

# Add points for site medians
points(arriv.by.site.dat$med.of.med, arriv.by.site.dat$distance, pch=19, cex=2.5, col=colors[arriv.by.site.dat$color.ix])
points(arriv.by.site.dat$med.of.med, arriv.by.site.dat$distance, pch=1, cex=2.5)
text(x=arriv.by.site.dat$med.of.med, y=arriv.by.site.dat$distance, labels=arriv.by.site.dat$site_id, col="white", cex=.5, font=2)

# Add labels for number of years for each site
text(labels=arriv.by.site.dat$n.years, x=arriv.by.site.dat$med.of.med-0.3, y=arriv.by.site.dat$distance-14, cex=0.7)


# Add red points and lines to extend to the median of the 10%ile arrival
points(arriv.by.site.dat$med.of.q10, y=arriv.by.site.dat$distance, pch= 15, col="red", cex=0.9)
arrows(x0=arriv.by.site.dat$med.of.q10, y0=arriv.by.site.dat$distance, 
       x1=arriv.by.site.dat$q10.of.med, y1=arriv.by.site.dat$distance, code=3, angle=90, length=0, col="red", lty=3)

# Add blue points and lines to extend to the median of the 90%ile arrival
points(arriv.by.site.dat$med.of.q90, y=arriv.by.site.dat$distance, pch= 17, col="blue", cex=0.9)
arrows(x0=arriv.by.site.dat$q90.of.med, y0=arriv.by.site.dat$distance, 
       x1=arriv.by.site.dat$med.of.q90, y1=arriv.by.site.dat$distance, code=3, angle=90, length=0, col="blue", lty=3)


legend("topleft", legend = c("Upper Salmon", "Upper Middle", "Lower Middle", "South Fork", "Chamberlain", "med. of median", "med. of 10%", "med. of 90%"), 
          pch = c(rep(19,5),1,15,17), col = c(colors[c(1,2,5,3,4)],"black","red","blue"), bg="white", cex=0.75)




  
  