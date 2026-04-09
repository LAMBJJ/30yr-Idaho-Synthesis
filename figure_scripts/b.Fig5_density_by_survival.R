# Figure 5: Density versus Survival ----
options(scipen = 99)


density <- read.csv("data/density_vs_survival.csv")

cor.test(dat$Parr_density, dat$Parr_to_smolt_survival)
p <- round(cortest$p.value, 5); if(p < 0.001) p <- "< 0.001"
r <- round(cortest$estimate, 2)
plot(dat$Parr_density, dat$Parr_to_smolt_survival, las = 1, ylab = "parr-to-smolt survival", xlab = "parr density in natal habitats", pch = 19)
legend("topright", legend = paste0("r = ", r, " (p = ", p, ")"), bty = 'n')

png("plots/density_vs_survival.png", width = 6.58, height = 5, units = "in", res = 600)
x <- density[,"Year"]
y1 <- density[, "Parr_density"]
y2 <- density[, "Parr_to_smolt_survival"]
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
legend("topright", legend = paste0("r = ", r, " (p = ", p, ")"), bty = 'n')
dev.off()
