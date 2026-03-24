
# Read Stats for Growth between release and recapture at LGD for each site each year
growth <- read.csv("growth_rates.csv", header=T, as.is=TRUE)

# Read site info to get distance to LGD; add info for coloring points
sites <- read.csv("sites.csv", header=T, as.is=TRUE)
colors <- c("#9a86d6", "#60a0dc", "#e97233", "#43bbad", "skyblue", "black")
sites$color.ix <- c(5,5,2,5,2,4,2,1,3,2,2,3,3,2,1,4)

# Put growth stats in year-by-sites matrices
med.growth.year.by.site <- matrix(growth[,4], ncol=16, byrow=T)
q10.growth.year.by.site <- matrix(growth[,3], ncol=16, byrow=T)
q90.growth.year.by.site <- matrix(growth[,5], ncol=16, byrow=T)
dimnames(med.growth.year.by.site) <- 
dimnames(q10.growth.year.by.site) <- 
dimnames(q90.growth.year.by.site) <- list(2001:2023, growth[1:16,1])

id <- growth[1:16,1]    # for distance lookup below

# Calculate stats needed for plotting and put them in a data.frame
growth.by.site.dat <- data.frame(
  site_id = id,                                                         # site_id labels points in plot
  distance = sites$Dist[match(id, sites$Site_ID)],                      # lookup distance in sites info
  color.ix = sites$color.ix[match(id, sites$Site_ID)],                  # lookup color in sites info
  n.years = apply(!is.na(med.growth.year.by.site),2,sum),               # number of years
  med.of.med = apply(med.growth.year.by.site,2,median,na.rm=T),         # median of n.years median growth
  med.of.q10 = apply(q10.growth.year.by.site,2,median,na.rm=T),         # median of n.years 10%ile growth
  med.of.q90 = apply(q90.growth.year.by.site,2,median,na.rm=T))         # median of n.years 90%ile growth



# Draw frame for plot elements 
plot(growth.by.site.dat$distance, growth.by.site.dat$med.of.med, ylim=c(15,60),
         type="n", xaxt="n", yaxt="n", yaxs="i",
         xlab="Distance Release to Lower Granite Dam (km)", ylab="Change in Length (mm) to Lower Granite Dam")

# Add axis labels
axis(1, at=seq(450,750,50), labels=seq(450,750,50))
axis(2, at=seq(0,80,10), labels=seq(0,80,10), las=1)

# Dashed horizontal grid lines
abline(h=seq(0,55,5), lty=2, col="gray75")

# Add points for all the estimates for individual site x year
for (j in 1:ncol(med.growth.year.by.site))
{
  dist      <- growth.by.site.dat$distance[j]
  g.by.year <- med.growth.year.by.site[,j]
  g.by.year <- g.by.year[!is.na(g.by.year)]

# one point for each site in the year, offset from the year a bit to avoid overwriting by arrows
  points(rep(dist-1, growth.by.site.dat$n.years[j]), g.by.year, pch=16, cex=0.6, col="gray70")
}
# Add labels for number of years for each site

text(labels=growth.by.site.dat$n.years, x=growth.by.site.dat$distance, y=growth.by.site.dat$med.of.q10 - 1, cex=0.7)

# Add "arrows" from 10th to 90th percentiles
# Do this before adding colored points, so that points end up in front
arrows(x0=growth.by.site.dat$distance, y0=growth.by.site.dat$med.of.q10, 
       x1=growth.by.site.dat$distance, y1=growth.by.site.dat$med.of.q90,
       code=3, angle=90, length=0.03)

# Add points for site medians
points(growth.by.site.dat$distance, growth.by.site.dat$med.of.med, pch=19, cex=2.5, col=colors[growth.by.site.dat$color.ix])
points(growth.by.site.dat$distance, growth.by.site.dat$med.of.med, pch= 1, cex=2.5)

# Nothing gets overwritten, so no special treatment needed for labeling points
text(x=growth.by.site.dat$distance, y=growth.by.site.dat$med.of.med, 
     labels=growth.by.site.dat$site_id, col="white", cex=.5, font=2)

# Add legend
legend("topright", legend = c("Upper Salmon", "Upper Middle", "Lower Middle", "South Fork", "Chamberlain"), 
                        pch = 19, col = colors[c(1,2,5,3,4)], cex=0.75, bg="white")



  
  