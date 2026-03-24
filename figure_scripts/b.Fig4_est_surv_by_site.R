
# Read survival estimates for each site each year
surv <- read.csv("est_survival.csv", header=T, as.is=TRUE)

# Read site info to get distance to LGD; add info for coloring points
sites <- read.csv("sites.csv", header=T, as.is=TRUE)
colors <- c("#9a86d6", "#60a0dc", "#e97233", "#43bbad", "skyblue", "black")
sites$color.ix <- c(5,5,2,5,2,4,2,1,3,2,2,3,3,2,1,4)

# Put survival estimates in a year-by-sites matrix
est.survival.year.by.site <- matrix(surv[,3], ncol=16, byrow=T)
dimnames(est.survival.year.by.site) <- list(1993:2023, surv[1:16,1])

id <- surv[1:16,1]    # for distance lookup below

# Calculate stats needed for plotting and put them in a data.frame
surv.by.site.dat <- data.frame(
  site_id = id,                                                         # site_id labels points in plot
  distance = sites$Dist[match(id, sites$Site_ID)],                      # lookup distance in sites info
  color.ix = sites$color.ix[match(id, sites$Site_ID)],                  # lookup color in sites info
  n.years = apply(!is.na(est.survival.year.by.site),2,sum),             # number of years
  median = apply(est.survival.year.by.site,2,median,na.rm=T),           # median of n.years survival estimates
  q10 = apply(est.survival.year.by.site,2,quantile,probs=0.1,na.rm=T),  # 10%ile of survival estimates
  q90 = apply(est.survival.year.by.site,2,quantile,probs=0.9,na.rm=T))  # 90%ile of survival estimates


plot(surv.by.site.dat$distance, surv.by.site.dat$median, ylim=c(0,.41), 
         type="n", xaxt="n", yaxt="n", yaxs="i", pch=19, 
         xlab="Distance Release to Lower Granite Dam (km)", ylab="Survival to Lower Granite Dam")



# Add axis labels
axis(1, at=seq(450,750,50), labels=seq(450,750,50))
axis(2, at=c(0,.1,.2,.3,.4), labels=c("0","0.1","0.2","0.3","0.4"), las=1)

# Dashed horizontal grid lines
abline(h=seq(.05,.4,.05), lty=2, col="gray75")

# Add points for all the estimates for individual site x year
for (j in 1:ncol(est.survival.year.by.site))
{
  dist      <- surv.by.site.dat$distance[j]
  s.by.year <- est.survival.year.by.site[,j]
  s.by.year <- s.by.year[!is.na(s.by.year)]

# one point for each year for the site, offset from the distance a bit to avoid overwriting by arrows
  points(rep(dist-1, surv.by.site.dat$n.years[j]), s.by.year, pch=16, cex=0.6, col="gray70")
}
# Add labels for number of years for each site
text(labels=surv.by.site.dat$n.years, x=surv.by.site.dat$distance, y=surv.by.site.dat$q10 - .008, cex=0.7)


# Add "arrows" from 10th to 90th percentiles
# Do this before adding colored points, so that points end up in front
arrows(x0=surv.by.site.dat$distance, y0=surv.by.site.dat$q10, 
       x1=surv.by.site.dat$distance, y1=surv.by.site.dat$q90,
       code=3, angle=90, length=0.03)



# Add points for site medians
points(surv.by.site.dat$distance, surv.by.site.dat$median, pch=19, cex=2.5, col=colors[surv.by.site.dat$color.ix])
points(surv.by.site.dat$distance, surv.by.site.dat$median, pch= 1, cex=2.5)

# Get site ids to label each median point
# 
lbls <- surv.by.site.dat$site_id
# BV CH and EL are on top of each other without special treatment
# apply labels to all but BV CH EL
lbls[lbls %in% c("BV","CH","EL")] <- ""
text(x=surv.by.site.dat$distance, y=surv.by.site.dat$median, labels=lbls, col="white", cex=.5, font=2)

# special treatment for BV CH EL
text(x=623.5, y=.126, labels="BV", cex=.5, font=2)
text(x=638.5, y=.126, labels="EL", cex=.5, font=2)
text(x=641, y=.165, labels="CH", cex=.5, font=2)
arrows(x0=638.5, y0=.16, x1=630, y1=.142, code=2, angle=30, length=0.02)

# Add legend
legend("topright", legend = c("Upper Salmon", "Upper Middle", "Lower Middle", "South Fork", "Chamberlain"), 
                        pch = 19, col = colors[c(1,2,5,3,4)], cex=0.75, bg="white")


  
  