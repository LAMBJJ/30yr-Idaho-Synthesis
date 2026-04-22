# Basic stats

library(ggplot2)
library(rlang)
library(cluster)  
library(tidyverse) 
library(factoextra)

surv <- read.csv("data/est_survival.csv")
arriv <- read.csv("data/granite_arrival_dates.csv")
growth <- read.csv("data/growth_rates.csv")

# Hierarchical agglomerative 'hclust'
fncCluster <- function(dat, var){
  
  # Format into year-by-sites matrix
  df <- matrix(dat[,var], ncol = length(unique(dat$Site_ID)), byrow = T)
  dimnames(df) <- list(unique(dat$Year), unique(dat$Site_ID))
  
  # Remove empty rows
  df <- subset(df, rowSums(df, na.rm = T) > 0)
  df <- t(df)
  
  # Scale the data if needed
  #df <- scale(df)
  
  # Compute dissimilarity matrix (Euclidean distance)
  dist_mat <- dist(df, method = "euclidean")
  
  # Hierarchical clustering (Ward's method)
  hc_res <- hclust(dist_mat, method = "ward.D2")
  
  # Plot the Dendrogram
  plot(hc_res, cex = 0.6, hang = -1, main = "Data Dendrogram")
  rect.hclust(hc_res, k = 5, border = 2:5) # Highlight clusters
  
  # Group populations based on the result
  site_map <- cutree(hc_res, k = 5)
  
  return(site_map)
}

# K-means
fncCluster2 <- function(dat, var){
  # Format into year-by-sites matrix
  df <- matrix(dat[,var], ncol = length(unique(dat$Site_ID)), byrow = T)
  dimnames(df) <- list(unique(dat$Year), unique(dat$Site_ID))
  
  # Remove empty rows
  df <- subset(df, rowSums(df, na.rm = T) > 0)
  df <- t(df)
  
  # Scale the data if needed
  #df <- scale(df)
  
  # Impute to fill NAs (necessary for next step)
  df_imputed <- as_tibble(df) %>%
    mutate(across(everything(), ~replace_na(., median(., na.rm = TRUE))))
  row.names(df_imputed) <- row.names(df)
  
  # Determine optimal clusters using the 'Elbow method'
  fviz_nbclust(df_imputed, kmeans, method = "wss")
  
  # Run K-means
  set.seed(123) # For reproducibility
  km_res <- kmeans(df_imputed, centers = 5, nstart = 25)
  
  # Visualize
  fviz_cluster(km_res, data = df_imputed, geom = "point",
               ellipse.type = "convex", main = "K-means Clustering")
  
  km_assignments <- km_res$cluster
  site_mapping_km <- as_tibble(df) %>%
    mutate(Cluster = km_assignments) %>%
    # If your sites have names in the row names, let's make them a column
    rownames_to_column(var = "Site_Name")
  print(sort(km_assignments))
  
  return(km_assignments)
}
  
# group populations as in Crozier and Zabel 2006's cluster analysis, then compare survival among clusters
site_map2 <- c(
  "LA" = 1, "SE" = 1, "SF" = 1, "VA" = 1,
  "CA" = 2, "HE" = 2, "EL" = 2, "MA" = 2, "BV" = 2, "CH" = 2, 
  "BU" = 3, "SU" = 3,
  "BL" = 4, "LO" = 4,
  "CL" = 5, "WC" = 5 # adding Chamberlain & WF Chamberlain (not in paper)
)

fncClusterEval <- function(dat, var, sm) {
  var_expr <- enquo(var)
  var_name <- rlang::as_label(var_expr)
  
  dat$Cluster <- as.factor(sm[dat$Site_ID])
  
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
site_map1 <- fncCluster(dat = surv, var = "Est_survival")
site_map1 <- fncCluster2(dat = surv, var = "Est_survival") #get equivalent results
sort(site_map1)
fncClusterEval(dat = surv, var = Est_survival, sm = site_map1)

# Arrival timing
site_map1 <- fncCluster(dat = arriv, var = "Median")
sort(site_map1)
fncClusterEval(dat = arriv, var = Median, sm = site_map1)

# Growth
site_map1 <- fncCluster(dat = growth, var = "Median")
sort(site_map1)
fncClusterEval(dat = growth, var = Median, sm = site_map1)


