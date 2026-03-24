
# Read Stats for Growth between release and recapture at LGD for each site each year
growth <- read.csv("growth_rates.csv", header=T, as.is=TRUE)

# Put growth stats in year-by-sites matrices
med.growth.year.by.site <- matrix(growth[,4], ncol=16, byrow=T)
q10.growth.year.by.site <- matrix(growth[,3], ncol=16, byrow=T)
q90.growth.year.by.site <- matrix(growth[,5], ncol=16, byrow=T)
dimnames(med.growth.year.by.site) <- 
dimnames(q10.growth.year.by.site) <- 
dimnames(q90.growth.year.by.site) <- list(2001:2023, growth[1:16,1])

# Calculate stats needed for plotting and put them in a data.frame
growth.by.year.dat <- data.frame(
  migr.yr    = 2001:2023,                                                  # migration year
  n.sites    = apply(!is.na(med.growth.year.by.site),1,sum),                   # number of sites
  med.of.med = apply(med.growth.year.by.site,1,median,na.rm=T),                # median of n.sites median growth
  med.of.q10 = apply(q10.growth.year.by.site,1,median,na.rm=T),                # median of n.sites 10%ile growth
  med.of.q90 = apply(q90.growth.year.by.site,1,median,na.rm=T))                # median of n.sites 90%ile growth


# Draw frame for plot elements 
plot(x=growth.by.year.dat$migr.yr, y=growth.by.year.dat$med.of.med, ylim=c(15,60),
         type="n", xaxt="n", yaxt="n", yaxs="i",
         xlab="Migration Year",ylab="Change in Length (mm) to Lower Granite Dam")

# Add axis labels
axis(1, at=seq(1995,2020,5), labels=seq(1995,2020,5))
axis(2, at=seq(0,80,10), labels=seq(0,80,10), las=1)

# Dashed horizontal grid lines
abline(h=seq(0,55,5), lty=2, col="gray75")


# Add points for all the estimates for individual site x year
for (i in 1:nrow(med.growth.year.by.site))
{
  my        <- growth.by.year.dat$migr.yr[i]
  g.by.site <- med.growth.year.by.site[i,]
  g.by.site <- g.by.site[!is.na(g.by.site)]

# one point for each site in the year, offset from the year a bit to avoid overwriting by arrows
  points(rep(my-.1, growth.by.year.dat$n.sites[i]), g.by.site, pch=16, cex=0.6, col="gray70")
}

# Point size (cex) for median of estimates for each site is scaled according to number of years
max.cex <- 1.8; min.cex <- 0.6; rng.cex <- max.cex-min.cex
min.n.sites <- min(growth.by.year.dat$n.sites); max.n.sites <- max(growth.by.year.dat$n.sites); rng.n.sites <- max.n.sites-min.n.sites
plot.cex <- min.cex + rng.cex*(growth.by.year.dat$n.sites - min.n.sites)/rng.n.sites

# Add points for annual medians
points(growth.by.year.dat$migr.yr, growth.by.year.dat$med.of.med, pch=19, cex=plot.cex)


# Add "arrows" from 10th to 90th percentiles
arrows(x0=growth.by.year.dat$migr.yr, y0=growth.by.year.dat$med.of.q10, 
       x1=growth.by.year.dat$migr.yr, y1=growth.by.year.dat$med.of.q90,
       code=3, angle=90, length=0.03)

# Add legend
legend("topright",pch=c(19,19), pt.cex=c(max.cex,min.cex), legend=c("median 16 sites", "median 3 sites"), bg="white")



  