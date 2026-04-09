
library(ggplot2)
library(rlang)

surv <- read.csv("data/est_survival.csv")
arriv <- read.csv("data/granite_arrival_dates.csv")
growth <- read.csv("data/growth_rates.csv")

# Basic stats
# group populations as in Crozier and Zabel 2006's cluster analysis, then compare survival among clusters

site_map <- c(
  "LA" = "C1", "SE" = "C1", "SF" = "C1", "VA" = "C1",
  "CA" = "C2", "HE" = "C2", "EL" = "C2", "MA" = "C2", "BV" = "C2", "CH" = "C2", 
  "BU" = "C3", "SU" = "C3",
  "BL" = "C4", "LO" = "C4",
  "CL" = "C5", "WC" = "C5" # adding Chamberlain (not in paper)
)


fncClusterEval <- function(dat, var) {
  var_expr <- enquo(var)
  var_name <- rlang::as_label(var_expr)
  
  dat$Cluster <- as.factor(site_map[dat$Site_ID])
  
  p <- ggplot(dat, aes(x = Cluster, y = {{ var }}, fill = Cluster)) +
    geom_boxplot(outlier.colour = "gray", outlier.shape = 16) +
    labs(
      title = paste("Distribution of", var_name),
      x = "Cluster Group",
      y = var_name
    ) +
    theme_minimal()
  
  print(p)
  
  cat("\n--- ANOVA Summary for:", var_name, "---\n")
  
  fit <- aov(dat[[var_name]] ~ Cluster, data = dat)
  print(summary(fit))
}

# Survival
fncClusterEval(dat = surv, var = Est_survival)

# Arrival timing
fncClusterEval(dat = arriv, var = Median)

# Growth
fncClusterEval(dat = growth, var = Median)


