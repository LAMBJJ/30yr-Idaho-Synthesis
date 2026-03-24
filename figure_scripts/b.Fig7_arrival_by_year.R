
# Read LGD arrival stats for each site each year
arriv <- read.csv("granite_arrival_dates.csv", header=T, as.is=TRUE)

# Put arrival stats in year-by-sites matrices
med.arriv.year.by.site <- matrix(arriv[,4], ncol=16, byrow=T)
q10.arriv.year.by.site <- matrix(arriv[,3], ncol=16, byrow=T)
q90.arriv.year.by.site <- matrix(arriv[,5], ncol=16, byrow=T)
dimnames(med.arriv.year.by.site) <- 
dimnames(q10.arriv.year.by.site) <- 
dimnames(q90.arriv.year.by.site) <- list(1993:2023, arriv[1:16,1])

# Calculate stats needed for plotting and put them in a data.frame
arriv.by.year.dat <- data.frame(
  migr.yr    = 1993:2023,                                                     # migration year
  ypos       = 31:1,                                                          # y value to "stack" years with 1993 at top, no 2021
  n.sites    = apply(!is.na(med.arriv.year.by.site),1,sum),                   # number of sites
  med.of.med = apply(med.arriv.year.by.site,1,median,na.rm=T),                # median of n.sites median arrival
  med.of.q10 = apply(q10.arriv.year.by.site,1,median,na.rm=T),                # median of n.sites 10%ile arrival
  med.of.q90 = apply(q90.arriv.year.by.site,1,median,na.rm=T),                # median of n.sites 90%ile arrival
  q10.of.med = apply(med.arriv.year.by.site,1,quantile,probs=0.1,na.rm=T),    # 10%ile of n.sites median arrival
  q90.of.med = apply(med.arriv.year.by.site,1,quantile,probs=0.9,na.rm=T))    # 90%ile of n.sites median arrival

# Draw frame for plot elements 
plot(x=arriv.by.year.dat$med.of.med, y=arriv.by.year.dat$ypos, xlim=c(97,160), ylim=c(0,32),
         type="n", xaxt="n", yaxt="n", yaxs="i",
         xlab="Arrival Date at LGR",ylab="Migration Year")

# Add axis labels
xticks <- c(95,100,105,110,115,120,125,130,135,140,145,150,156,161)
axis(1, at=xticks,
        labels=c("", "Apr 10", "", "Apr 20", "", "Apr 30", "", "May 10", "", "May 20", "", "May 30", "", "Jun 10"))  
axis(2, at=seq(29,4,-5), labels=seq(1995,2020,5), las=1, cex.axis=0.75)

# Dashed vertical grid lines
abline(v=xticks, lty=2, col="gray75")

# Point size (cex) for median of estimates for each year is scaled according to number of sites
max.cex <- 1.8; min.cex <- 0.6; rng.cex <- max.cex-min.cex
min.n.sites <- min(arriv.by.year.dat$n.sites); max.n.sites <- max(arriv.by.year.dat$n.sites); rng.n.sites <- max.n.sites-min.n.sites
plot.cex <- min.cex + rng.cex*(arriv.by.year.dat$n.sites - min.n.sites)/rng.n.sites

# Add points for annual medians
points(arriv.by.year.dat$med.of.med, y=arriv.by.year.dat$ypos, pch= 19, cex=plot.cex)
   
# Add black arrows between 10%ile and 90%ile of median arrival
arrows(x0=arriv.by.year.dat$q10.of.med, y0=arriv.by.year.dat$ypos,  
       x1=arriv.by.year.dat$q90.of.med, y1=arriv.by.year.dat$ypos, code=3, angle=90, length=0.03) 

# Add blue points and lines to extend to the median of the 10%ile arrival
points(arriv.by.year.dat$med.of.q10, y=arriv.by.year.dat$ypos, pch= 15, col="red", cex=0.5*plot.cex) 
arrows(x0=arriv.by.year.dat$med.of.q10, y0=arriv.by.year.dat$ypos,  
       x1=arriv.by.year.dat$q10.of.med, y1=arriv.by.year.dat$ypos, code=3, angle=90, length=0, col="red", lty=3)

# Add red points and lines to extend to the median of the 90%ile arrival
points(arriv.by.year.dat$med.of.q90, y=arriv.by.year.dat$ypos, pch= 17, col="blue", cex=0.5*plot.cex) 
arrows(x0=arriv.by.year.dat$q90.of.med, y0=arriv.by.year.dat$ypos,  
       x1=arriv.by.year.dat$med.of.q90, y1=arriv.by.year.dat$ypos, code=3, angle=90, length=0, col="blue", lty=3)

# Add legend
legend("bottomright", pch=c(19,19,15,17), pt.cex=c(max.cex,min.cex,.8,.8), col=c("black","black","red","blue"), bg="white",
       legend=c("med. of med. 16 sites", "med. of med. 3 sites", "med. of 10%", "med. of 90%"), cex=0.75)



  
  