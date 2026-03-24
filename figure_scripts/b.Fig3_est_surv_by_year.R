
# Read survival estimates for each site each year
surv <- read.csv("est_survival.csv", header=T, as.is=TRUE)

# Put survival estimates in a year-by-sites matrix
est.survival.year.by.site <- matrix(surv[,3], ncol=16, byrow=T)
dimnames(est.survival.year.by.site) <- list(1993:2023, surv[1:16,1])

# Calculate stats needed for plotting and put them in a data.frame
surv.by.year.dat <- data.frame(
  migr.yr = 1993:2023,                                                  # migration year
  n.sites = apply(!is.na(est.survival.year.by.site),1,sum),             # number of sites
  median = apply(est.survival.year.by.site,1,median,na.rm=T),           # median of n.sites survival estimates
  q10 = apply(est.survival.year.by.site,1,quantile,probs=0.1,na.rm=T),  # 10%ile of survival estimates
  q90 = apply(est.survival.year.by.site,1,quantile,probs=0.9,na.rm=T))  # 90%ile of survival estimates


# Draw frame for plot elements 
plot(surv.by.year.dat$migr.yr, surv.by.year.dat$median, ylim=c(0,.41),
         type="n", xaxt="n", yaxt="n", yaxs="i",
         xlab="Migration Year", ylab="Survival to Lower Granite Dam")

# Add axis labels
axis(1, at=seq(1995,2020,5), labels=seq(1995,2020,5))
axis(2, at=c(0,.1,.2,.3,.4), labels=c("0","0.1","0.2","0.3","0.4"), las=1)

# Dashed horizontal grid lines
abline(h=seq(.05,.4,.05), lty=2, col="gray75")

# Add points for all the estimates for individual site x year
for (i in 1:nrow(est.survival.year.by.site))
{
  my        <- surv.by.year.dat$migr.yr[i]
  s.by.site <- est.survival.year.by.site[i,]
  s.by.site <- s.by.site[!is.na(s.by.site)]

# one point for each site in the year, offset from the year a bit to avoid overwriting by arrows
  points(rep(my-.1, surv.by.year.dat$n.sites[i]), s.by.site, pch=16, cex=0.6, col="gray70")
}


# Point size (cex) for median of estimates for each year is scaled according to number of sites
max.cex <- 1.8; min.cex <- 0.6; rng.cex <- max.cex-min.cex
min.n.sites <- min(surv.by.year.dat$n.sites); max.n.sites <- max(surv.by.year.dat$n.sites); rng.n.sites <- max.n.sites-min.n.sites
plot.cex <- min.cex + rng.cex*(surv.by.year.dat$n.sites - min.n.sites)/rng.n.sites

# Add points for annual medians
points(surv.by.year.dat$migr.yr, surv.by.year.dat$median, pch=19, cex=plot.cex)

# Add "arrows" from 10th to 90th percentiles
arrows(x0=surv.by.year.dat$migr.yr, y0=surv.by.year.dat$q10, 
       x1=surv.by.year.dat$migr.yr, y1=surv.by.year.dat$q90,
       code=3, angle=90, length=0.03)

# Add legend
legend("topright",pch=c(19,19), pt.cex=c(max.cex,min.cex), legend=c("median 16 sites", "median 3 sites"), bg="white")



  
  