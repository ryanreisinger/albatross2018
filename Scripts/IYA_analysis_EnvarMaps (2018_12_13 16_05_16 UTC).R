## Indian Yellow-Nosed Albatros

library(raster)
library(marmap)
library(rgdal)
library(plyr)


#------------------------------

setwd("D:/PEI_Toppredators/IYA/Working")
dat <- read.csv("dataAllCombined_02.csv", stringsAsFactors = F)
dat$date <- as.POSIXct(dat$date, format ="%Y-%m-%d %H:%M:%S", "GMT")
dat <- dat[order(dat$id, dat$date), ]
dat <- dat[!(duplicated(dat)), ]

ids <- unique(dat$id)

#Split into years
dat.2008 <- dat[dat$date < "2010-01-01 00:00:00", ]
dat.2011 <- dat[dat$date > "2010-01-01 00:00:00", ]


fits <- readRDS("fit.sub.locs.rds")
ids2 <- unique(fits$id)

#------------------------------
## 03. Descriptive stats

#add year
fits$year <- rep(NA, nrow(fits))
for(i in 1:nrow(fits)){
  if(fits$date[i] < "2010-01-01 00:00:00"){
    fits$year[i] <- "2008"
  } else {
    fits$year[i] <- "2011"
    }
}

#add julian day
library(lubridate)
fits$day <- yday(fits$date)
plot(fits$day, col = as.factor(fits$year))

#add deployment duration and distance
library(geosphere)
dt <- list()
for(i in 1:length(ids2)){
  d <- fits[fits$id == ids2[i], ]
  d.start <- min(d$date)
  d.end <- max(d$date)
  dur <- d.end - d.start
  dur <- as.character(dur)
  d$start <- rep(d.start, nrow(d))
  d$end <- rep(d.end, nrow(d))
  d$dur <- rep(dur, nrow(d))
  slon <- rep(d$lon[1], nrow(d))
  slat <- rep(d$lat[1], nrow(d))
  s <- as.matrix(cbind(slon, slat))
  e <- as.matrix(cbind(d$lon, d$lat))
  d$dis <- distGeo(s, e)/1000
  d$dmax <- max(d$dis)
  plot(d$dis, main = d$id[1])
  abline(h = 10, col = "red")
  dt[i] <- list(d)
}

fits.up <- do.call(rbind, dt)




#-----------------------------------------------------------
#Get environmental data
#1. SST
sst08 <- raster("EnvarMaps/SST_08.grd")
sst08.p <- rasterToPoints(sst08)
sst08.p <- data.frame(sst08.p)
colnames(sst08.p) <- c("lon", "lat", "val")
sst08.p$year <- rep("2008", nrow(sst08.p))

sst11 <- raster("EnvarMaps/SST_11.grd")
sst11.p <- rasterToPoints(sst11)
sst11.p <- data.frame(sst11.p)
colnames(sst11.p) <- c("lon", "lat", "val")
sst11.p$year <- rep("2011", nrow(sst11.p))

sst <- rbind(sst08.p, sst11.p)

#2. SSH
ssh08 <- raster("EnvarMaps/SSH_08.grd")
ssh08.p <- rasterToPoints(ssh08)
ssh08.p <- data.frame(ssh08.p)
colnames(ssh08.p) <- c("lon", "lat", "val")
ssh08.p$year <- rep("2008", nrow(ssh08.p))

ssh11 <- raster("EnvarMaps/SSH_11.grd")
ssh11.p <- rasterToPoints(ssh11)
ssh11.p <- data.frame(ssh11.p)
colnames(ssh11.p) <- c("lon", "lat", "val")
ssh11.p$year <- rep("2011", nrow(ssh11.p))

ssh <- rbind(ssh08.p, ssh11.p)

#3. CHLA
chla08 <- readRDS("EnvarMaps/CHL_08.RDS")
chla08$year <- rep("2008", nrow(chla08))
chla11 <- readRDS("EnvarMaps/CHL_11.RDS")
chla11$year <- rep("2011", nrow(chla11))
chla <- rbind(chla08, chla11)
names(chla) <- c("lon", "lat", "val", "year")

#4. Depth
dep11 <- raster("b4.grd")
dep11 <- crop(dep11, extent(ssh11))

dep11.p <- rasterToPoints(dep11)
dep11.p <- data.frame(dep11.p)
colnames(dep11.p) <- c("lon", "lat", "val")
dep11.p$year <- rep("2011", nrow(dep11.p))

dep08.p <- dep11.p
dep08.p$year <- "2008"

dep <- rbind(dep08.p, dep11.p)

#-----------------------------------------------------------
#Plot

library(ggplot2)
library(ggmap)

fits.copy <- fits
fits.copy[fits.copy$b >= 1.75, "state"] <- "R"
fits.copy[fits.copy$b <= 1.25, "state"] <- "T"
fits.copy[fits.copy$b > 1.25 & fits.copy$b < 1.75, "state"] <- "U"

fits.copy$state <- as.factor(fits.copy$state)

fits.copy <- fits.copy[fits.copy$lon < 45, ]

library(viridis)
minx <- 11
maxx <- 50
miny <- -50
maxy <- -33

world <- borders("world", colour="gray20", fill = "gray20", xlim = c(min(fits.copy$lon), max(fits.copy$lon)), ylim = c(min(fits.copy$lat), max(fits.copy$lat)))


# 1. SST

p1 <- ggplot(data = sst, aes(x=lon, y=lat))
p1 <- p1 + geom_tile(aes(fill=val)) + facet_grid(. ~ year)
p1 <- p1 + scale_x_continuous(expand = c(0,0), limits = c(minx, maxx))
p1 <- p1 + scale_y_continuous(expand = c(0,0), limits = c(miny, maxy))
p1 <- p1 + scale_fill_viridis(name = "SST (C)")
#p1 <- p1 + scale_fill_viridis(guide = FALSE)
#p1 <- p1 + geom_point(data = fits.copy, aes(x = fits.copy$lon, y = fits.copy$lat), colour = "black", alpha = 0.5, size = 0.5) + facet_grid(. ~ year)
p1 <- p1 + coord_quickmap()
p1 <- p1 + theme_bw(base_size = 8) + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_text(colour = "black", size = 8),
                              strip.text = element_text(colour = "black", size = 8),
                              legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8),
                              strip.background = element_blank())
p1 <- p1 + world + annotate("point", x = 37.94, y = -46.64, pch = 16, size = 2, colour = "red")

p1

# 2. SSH

p2 <- ggplot(data = ssh, aes(x=lon, y=lat))
p2 <- p2 + geom_tile(aes(fill=val)) + facet_grid(. ~ year)
p2 <- p2 + scale_x_continuous(expand = c(0,0), limits = c(minx, maxx))
p2 <- p2 + scale_y_continuous(expand = c(0,0), limits = c(miny, maxy))
p2 <- p2 + scale_fill_viridis(name = "SSHA (m)")
#p2 <- p2 + scale_fill_viridis(guide = FALSE)
#p2 <- p2 + geom_point(data = fits.copy, aes(x = fits.copy$lon, y = fits.copy$lat), colour = "black", alpha = 0.5, size = 0.5) + facet_grid(. ~ year)
p2 <- p2 + coord_quickmap()
p2 <- p2 + theme_bw(base_size = 8) + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_text(colour = "black", size = 8),
                              strip.text = element_text(colour = "black", size = 8),
                              legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8),
                              strip.background = element_blank())
p2 <- p2 + world + annotate("point", x = 37.94, y = -46.64, pch = 16, size = 2, colour = "red")

p2

# 3. CHLA

p3 <- ggplot(data = chla, aes(x=lon, y=lat))
p3 <- p3 + geom_tile(aes(fill=val)) + facet_grid(. ~ year)
p3 <- p3 + scale_x_continuous(expand = c(0,0), limits = c(minx, maxx))
p3 <- p3 + scale_y_continuous(expand = c(0,0), limits = c(miny, maxy))
p3 <- p3 + scale_fill_viridis(name = "CHL (mg/mg2)", trans = "log")
#p3 <- p3 + scale_fill_viridis(guide = FALSE, trans = "log")
#p3 <- p3 + geom_point(data = fits.copy, aes(x = fits.copy$lon, y = fits.copy$lat), colour = "gray", alpha = 0.5, size = 0.5) + facet_grid(. ~ year)
p3 <- p3 + coord_quickmap()
p3 <- p3 + theme_bw(base_size = 8) + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_text(colour = "black", size = 8),
                              strip.text = element_text(colour = "black", size = 8),
                              legend.title = element_text(size = 8),
                              legend.text = element_text(size = 8),
                              strip.background = element_blank())
p3 <- p3 + world + annotate("point", x = 37.94, y = -46.64, pch = 16, size = 2, colour = "red")

p3


# 4. Depth

p4 <- ggplot(data = dep, aes(x=lon, y=lat))
p4 <- p4 + geom_tile(aes(fill=val)) + facet_grid(. ~ year)
p4 <- p4 + scale_x_continuous(expand = c(0,0), limits = c(minx, maxx))
p4 <- p4 + scale_y_continuous(expand = c(0,0), limits = c(miny, maxy))
#p4 <- p4 + scale_fill_gradient(name = "DEPTH (m)", low = "grey60", high = "white")
p4 <- p4 + scale_fill_gradient(guide = FALSE, low = "grey60", high = "white")
p4 <- p4 + geom_point(data = fits.copy, aes(x = fits.copy$lon, y = fits.copy$lat, colour = fits.copy$b), size = 0.8) + facet_grid(. ~ year)
#p4 <- p4 + scale_colour_viridis(guide = FALSE)
p4 <- p4 + scale_colour_viridis(name = "Behavioural state")
p4 <- p4 + coord_quickmap()
p4 <- p4 + theme_bw(base_size = 8) + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank(),
                              axis.title = element_blank(),
                              axis.text = element_text(colour = "black", size = 8),
                              strip.text = element_text(colour = "black", size = 8),
                              legend.text = element_text(size = 8),
                              strip.background = element_blank())
p4 <- p4 + world + annotate("point", x = 37.94, y = -46.64, pch = 16, size = 2, colour = "red")

p4

library(gridExtra)
tiff(file = "envMapScales2.tiff", width = 4.7, height = 8, units = "in", res = 800,
     antialias = "cleartype", compression = "none")
grid.arrange(p4, p1, p3, p2, ncol = 1)
dev.off()
