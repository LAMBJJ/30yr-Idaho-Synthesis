library(sf)
library(mapsf)

# Load data -----
crb <- sf::read_sf("data/shapefiles/CRB_boundary_prj.shp")
pnw <- sf::read_sf("data/shapefiles/PNW_prj.shp")
salmon_bdry <- sf::read_sf("data/shapefiles/Salmon_boundary_prj.shp")
rivers <- sf::read_sf("data/shapefiles/Rivers_SOgt5_prj.shp")
streams <- sf::read_sf("data/shapefiles/SalmonSnake_streams_h12.shp")
streams5 <- subset(streams, StreamOrde > 5)
rivers7 <- subset(rivers, StreamOrde > 7)

points <- sf::read_sf("data/shapefiles/Idaho_sites_prj.shp")
points <- points[order(points$Name),]
points$Lbl[points$Stream == "Bear Valley Creek"] <- "BV"
points$Stream[points$Stream == "Big Creek (lower)/Rush Creek"] <- "Big Creek (lower)"
points$Lbl[points$Stream == "Big Creek (lower)"] <- "BL"
points$Lbl[points$Stream == "Big Creek (upper)"] <- "BU"
points$Lbl[points$Stream == "Camas Creek"] <- "CA"
points$Lbl[points$Stream == "Cape Horn Creek"] <- "CH"
points$Lbl[points$Stream == "Chamberlain Creek"] <- "CL"
points$Lbl[points$Stream == "East Fork Salmon River"] <- "EF"
points$Lbl[points$Stream == "Elk Creek"] <- "EL"
points$Lbl[points$Stream == "Herd Creek"] <- "HE"
points$Lbl[points$Stream == "Lake Creek"] <- "LA"
points$Lbl[points$Stream == "Loon Creek"] <- "LO"
points$Lbl[points$Stream == "Marsh Creek"] <- "MA"
points$Lbl[points$Stream == "Rush Creek"] <- "RU"
points$Lbl[points$Stream == "South Fork Salmon River"] <- "SF"
points$Lbl[points$Stream == "Secesh River"] <- "SE"
points$Lbl[points$Stream == "Sulphur Creek"] <- "SU"
points$Lbl[points$Stream == "Valley Creek"] <- "VA"
points$Lbl[points$Stream == "West Fork Chamberlain Creek"] <- "WC"

lbls <- points$Lbl

iptds_rst <- read.csv("data/IPTDS_RST_sites.csv")
target_crs <- sf::st_crs(points)
iptds_rst <- sf::st_as_sf(iptds_rst, coords = c("Lon", "Lat"), crs = 4326)
iptds_rst <- sf::st_transform(iptds_rst, crs = target_crs)
#print(st_crs(points) == st_crs(iptds_sp))

iptds <- subset(iptds_rst, Type == "IPTDS")
rst <- subset(iptds_rst, Type == "RST")


# Map -----
#mf_export(mf,"main_map.png", width = 12, height = 12, units = "in", res = 300)
mf <- mf_init(salmon_bdry, expandBB = rep(0.1, 4))
mf <- mf_theme(bg = "white", tab = TRUE, mar = c(0,0,0,0), pos = "left")
mf <- mf_map(streams, col = "#689ba7", lwd = 1, add = T)
mf <- mf_map(salmon_bdry, col = "#E8E8E8", border = NA, add = T) #"#d4dfe2" #d8d7d2
mf <- mf_map(streams5, col = "#20839b", lwd = round(rivers$StreamOrde/4), add = T)
mf <- mf_map(streams, col = "#689ba7", lwd = 0.6, add = T)

# Add sites
mf_label(points, "Lbl", col = "#ac330d", halo = T, bg = "#fccd04", overlap = T, q = 3, lines = F, cex = 0.5)
mf_label(rst, "Code", col = "white", halo = T, bg = "black", overlap = T, q = 3, lines = F, cex = 0.5)
mf_label(iptds, "Code", col = "black", halo = T, bg = "white", overlap = T, q = 3, lines = F, cex = 0.5)

# Add cartographic details
mf <- mf_graticule(x = salmon_bdry, col = "grey70", lty = 3, cex = 1.75)
mf <- mf_scale(pos = "bottom", cex = 1.2)
mf <- mf_arrow("bottom")
#dev.off()


# Inset map 
mf_export(mf,"inset_map.png", width = 7, height = 6, units = "in", res = 300)
mf <- mf_init(crb, expandBB = rep(0.2, 4))
mf <- mf_map(crb, col = "gray90", border = 0, add = T) 
mf <- mf_map(pnw, col = NA, border = "white", add = T)
mf <- mf_map(salmon_bdry, col = "gray60", border = NA, add = T)
mf <- mf_label(pnw, "Lbl", col = "black", overlap = T, q = 3)
mf <- mf_map(st_zm(rivers7), col = "gray40", lwd = 2, add = T)
mf <- mf_graticule(x = crb, col = "grey70", lty = 3, cex = 1.5)
dev.off()
