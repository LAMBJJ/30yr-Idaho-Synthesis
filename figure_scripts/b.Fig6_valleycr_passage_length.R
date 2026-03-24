# Figure 6: Instream monitors----

instream_detections <- read.csv("data/instream_detections.csv")
instream_detections$DateDetected <- as.Date(instream_detections$DateDetected, origin = "1970-01-01", format = "%m/%d/%y")

instream_lengths <- read.csv("data/taglength_instreamdet.csv")
instream_lengths$DateDetected <- as.Date(instream_lengths$DateDetected, origin = "1970-01-01", format = "%m/%d/%y")


#counts detected
instream_detections <- instream_detections[instream_detections$Count > 0,]
dat1 <- instream_detections[instream_detections$InstreamMonitor == "Valley",]
dat1 <- dat1[order(dat1$DateDetected),]
dat1 <- dat1[6:nrow(dat1),] # first few days for this location reflect high movement after tagging and instream monitor is very nearby
x1 <- dat1$DateDetected
y1 <- dat1$Count

# lengths of fish detected
dat3 <- instream_lengths[instream_lengths$InstreamMonitor == "Valley",]
dat3 <- dat3[order(dat3$DateDetected),]
min(dat1$DateDetected)
dat3 <- dat3[dat3$DateDetected >= min(dat1$DateDetected),]
x3 <- dat3$DateDetected
y3 <- dat3$Length

png("plots4ms/instream_monitors2.png", width = 7, height = 6, units = "in", res = 600)
par(mar = c(3,5,2,1), oma = c(rep(0,4)), mfrow = c(2,1), cex.main = 0.9)
plot(x1, y1, type = "n", las = 1, xlim = c(as.Date("2016-07-30"), as.Date("2017-05-24")), axes = F,
     ylab = "Observed passage numbers", main = "Valley Creek monitor, 2016-17", xlab = "", cex = 0.9, xaxt = 'n')
x_ticks <- pretty(x1, 8)
axis.Date(side = 1, x1, at = x_ticks); axis(side = 2, las = 1)
rect(x_ticks[5], 0, x_ticks[9], (max(y1) + 5), col = "gray90", border = NA)
arrows(x1, 0, x1, y1, code = 3, length = 0, angle = 90, col = "gray50")
text(x_ticks[3], 28, "Fall")
text(x_ticks[3], 25, "PSS = 22.8%", cex = 0.7)
text(x_ticks[3], 23, "(13.4-33.5%)", cex = 0.7)
text(x_ticks[7], 28, "Winter")
text(x_ticks[7], 25, "PSS = 26.3%", cex = 0.7)
text(x_ticks[7], 23, "(10.4-43.3%)", cex = 0.7)
text(as.Date("2017-04-15"), 28, "Spring")
text(as.Date("2017-04-15"), 25, "PSS = 53.6%", cex = 0.7)
text(as.Date("2017-04-15"), 23, "(11.9-109.2%)", cex = 0.7)
box()

plot(x3, y3, type = "n", las = 1, xlim = c(as.Date("2016-07-30"), as.Date("2017-05-24")), axes = F,
     ylim = c(50, 90), ylab = "Length at tagging (mm)", xlab = "", cex = 0.9, xaxt = 'n')
x_ticks <- pretty(x3, 8)
axis.Date(side = 1, x3, at = pretty(x3, 8)); axis(side = 2, las = 1)
rect(x_ticks[5], 0, x_ticks[9], (max(y3) + 5), col = "gray90", border = NA)
points(x3, y3, col = "gray30", pch = 19, cex = 0.5)
box()

dev.off()

write.csv(instream_detections, file = "plots4ms/instream_detections.csv")
write.csv(instream_lengths, file = "plots4ms/instream_lengths.csv")
