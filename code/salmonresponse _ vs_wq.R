# Quick look at metrics summarizing thermal exposure and water depth versus salmon responses by populations, years, and life history pathways
# AH Fullerton 4/3/26

library(ggplot2)
library(dplyr)

# Load fish data
survival <- read.csv("data/est_survival.csv")
growth <- read.csv("data/growth_rates.csv")
arrival <- read.csv("data/granite_arrival_dates.csv")

# Load water quality data
depth.mets <- read.csv("data_wq/depth_metrics_empirical.csv")
depth.mets <- subset(depth.mets, Life.stage == "rearing")
temp.mets <- read.csv("data_wq/thermal_metrics_empirical.csv")
temp.mets <- subset(temp.mets, Life.stage == "rearing")

fncComparisons <- function(wq.dat, fs.dat, wq.var, fs.var, wnm, fnm){
  #align names
  wq.dat$Site_ID <- wq.dat$SiteCode
  site_map <- c(
    "BIL" = "BL", "BIU" = "BU", "BVC" = "BV", "CAM" = "CA",
    "CHA" = "CL", "CHC" = "CH", "HER" = "HE", "LAK" = "LA",
    "LOO" = "LO", "MAR" = "MA", "SEC" = "SE", "SFS" = "SF",
    "SUL" = "SU", "VAL" = "VA", "WFC" = "WC"
  )
  wq.dat$Site_ID <- site_map[wq.dat$Site_ID]
  wq.dat <- wq.dat[!is.na(wq.dat$Site_ID),]
  
  # No survival data for Sawtooth Hatchery (STL); no WQ data for Elk (EL)
  colnames(wq.dat)[which(colnames(wq.dat) %in% 'year')] <- "Year"
  
  # join with fishy data
  dat <- dplyr::left_join(fs.dat, wq.dat, by = c("Site_ID", "Year"))
  
  wq_name <- rlang::as_label(enquo(wq.var))

  # plot
  ggplot(dat, aes(x = {{wq.var}}, y = {{fs.var}}, color = factor(LifeHistory))) +
    geom_point(alpha = 0.7, size = 2) +           
    geom_smooth(method = "lm", se = F) +     
    facet_wrap(~ Site_ID) +                         
    labs(
      title = "",
      x = wq_name,
      y = fnm,
      color = "Life history pathway"
    ) +
    theme_minimal()
  
  # save
  ggsave(paste0("plots/", wnm, "_vs_", fnm, ".png"), width = 10, height = 8, units = "in", dpi = 300)
  
}

# temperature
fncComparisons(wq.dat = temp.mets, fs.dat = survival, wq.var = cum.exp, fs.var = Est_survival, wnm = "cumexp", fnm = "survival")
fncComparisons(wq.dat = temp.mets, fs.dat = growth, wq.var = cum.exp, fs.var = Median, wnm = "cumexp", fnm = "growth")
fncComparisons(wq.dat = temp.mets, fs.dat = arrival, wq.var = cum.exp, fs.var = Median, wnm = "cumexp", fnm = "arrival")

# depth
fncComparisons(wq.dat = depth.mets, fs.dat = survival, wq.var = AWA, fs.var = Est_survival, wnm = "depth", fnm = "survival")
fncComparisons(wq.dat = depth.mets, fs.dat = growth, wq.var = AWA, fs.var = Median, wnm = "depth", fnm = "growth")
fncComparisons(wq.dat = depth.mets, fs.dat = arrival, wq.var = AWA, fs.var = Median, wnm = "depth", fnm = "arrival")

