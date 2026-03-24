# Figure 5: Density versus Survival ----


density <- read.csv("data/density.csv")


png("plots4ms/density_vs_survival.png", width = 6.58, height = 5, units = "in", res = 600)
x <- density[,"TaggingYear"]
y1 <- density[, "ParrDensity"]
y2 <- density[, "Parr2SmoltSurvival"]
par(mar = c(5,5.5,3,5))
plot(x, y1, las = 1, ylab = "", type = 'b', xlab = "Tagging year", pch = 19, col = "dodgerblue",
     ylim = as.numeric(c(min(y1, na.rm = T), max(y2, na.rm = T))), axes = F)
axis(side = 1, at = pretty(range(x, na.rm = T)), las = 1)
axis(side = 2, at = pretty(range(y1, na.rm = T)), las = 1)
mtext(expression("Density (parr / 100 "~ m^2 * ")"), side = 2, line = 3, col = "dodgerblue")
par(new = T)
plot(x, y2, type = 'b', pch = 17, col = "darkorange", axes = F, xlab = "", ylab = "",
     ylim = as.numeric(c(min(y1, na.rm = T), max(y2, na.rm = T))))
axis(side = 4, at = pretty(range(y2, na.rm = T)), las = 1)
mtext("Parr-to-smolt Survival", side = 4, line = 3, col = "darkorange")
box()
dev.off()
write.csv(survival_sites, file = "plots4ms/density_vs_survival.csv")

