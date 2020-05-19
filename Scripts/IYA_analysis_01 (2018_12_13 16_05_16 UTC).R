## Indian Yellow-Nosed Albatros

#------------------------------
## 01. Data Wrangling

setwd("C:/Users/Ryan Reisinger/Documents/Academic/PEI_Toppredators/IYA/Data_Original")
list.files()

dat1 <- read.csv("all_data_RobCrawford_newiMakhado.csv", stringsAsFactors = F)
dat1 <- dat1[dat1$common_name == "Indian Yellow-nosed Albatross", ]
dat1 <- dat1[ , c("original_track_id", "date_gmt", "time_gmt", "latitude", "longitude", "argos_quality")]
names(dat1) <- c("id", "date", "time", "lat", "lon", "lc")
dat1$date <- paste(dat1$date, dat1$time, sep = " ")
dat1$time <- NULL
dat1$date <- as.POSIXct(dat1$date, format = "%Y/%m/%d %H:%M:%S", "GMT")

dat2 <- read.csv("datasets_tow_RobCrawf.csv", stringsAsFactors = F)
dat2 <- dat2[dat2$Common == "Indian Yellow-nosed Albatross", ]
dat2 <- dat2[ , c("tr_id", "DateGMT", "TimeGMT", "Latitude", "Longitude", "Quality")]
names(dat2) <- c("id", "date", "time", "lat", "lon", "lc")
dat2$date <- paste(dat2$date, dat2$time, sep = " ")
dat2$time <- NULL
dat2$date <- as.POSIXct(dat2$date, format = "%d/%m/%Y %H:%M:%S", "GMT")

dat3 <- read.csv("ind yel nos.csv", stringsAsFactors = F)
dat3 <- dat3[ , c("TrackId", "DateGMT", "TimeGMT", "Latitude", "Longitude", "ArgosQuality")]
names(dat3) <- c("id", "date", "time", "lat", "lon", "lc")
dat3$date <- paste(dat3$date, dat3$time, sep = " ")
dat3$time <- NULL
dat3$date <- as.POSIXct(dat3$date, format = "%d/%m/%Y %H:%M:%S", "GMT")

dat4 <- read.csv("indian yellow nosed 2011.csv", stringsAsFactors = F)
dat4 <- dat4[ , c("PTT", "DATE", "TIME", "LAT1", "LONG1", "LC")]
names(dat4) <- c("id", "date", "time", "lat", "lon", "lc")
dat4$date <- paste(dat4$date, dat4$time, sep = " ")
dat4$time <- NULL
dat4$date <- as.POSIXct(dat4$date, format = "%Y/%m/%d %H:%M:%S", "GMT")


dat <- rbind.data.frame(dat1, dat2, dat3, dat4)
dat <- dat[!duplicated(dat), ]#remove duplicate lines
write.csv(dat, "C:/Users/Ryan Reisinger/Documents/Academic/PEI_Toppredators/IYA/Working/dataAllCombined_02.csv", row.names = F)
rm(list = ls())

#------------------------------
## 02. 

setwd("C:/Users/Ryan Reisinger/Documents/Academic/PEI_Toppredators/IYA/Working")
dat <- read.csv("dataAllCombined_02.csv", stringsAsFactors = F)
dat$date <- as.POSIXct(dat$date, format ="%Y-%m-%d %H:%M:%S", "GMT")
dat <- dat[order(dat$id, dat$date), ]
dat <- dat[!(duplicated(dat)), ]

ids <- unique(dat$id)

#time intervals
# ints <- list()
# for(i in 1:length(ids)){
#   d <- dat[dat$id == ids[i], ]
#   t <- diff(d$date)
#   ints[i] <- list(t)
# }
# ints <- unlist(ints)
# ints <- ints/60/60

#Split into years
dat.2008 <- dat[dat$date < "2010-01-01 00:00:00", ]
dat.2011 <- dat[dat$date > "2010-01-01 00:00:00", ]


#bsam
library(rjags)
library(PBSmapping)
library(devtools)
# devtools::install_github("ianjonsen/bsam", force = TRUE)
library(bsam)

#bsam test
tst <- dat[dat$id == ids[1], ]
fit.tst_01 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_01)
map_ssm(fit.tst_01)

tst <- dat[dat$id == ids[2], ]
fit.tst_2 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_2)
map_ssm(fit.tst_2)

tst <- dat[dat$id == ids[3], ]
fit.tst_03 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_03)
map_ssm(fit.tst_03)

tst <- dat[dat$id == ids[4], ]
fit.tst_04 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_04)
map_ssm(fit.tst_04)

tst <- dat[dat$id == ids[5], ]
fit.tst_05 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_05)
map_ssm(fit.tst_05)

tst <- dat[dat$id == ids[6], ]
fit.tst_6 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_6)
map_ssm(fit.tst_6)

tst <- dat[dat$id == ids[7], ]
fit.tst_07 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_07)
map_ssm(fit.tst_07)

tst <- dat[dat$id == ids[8], ]
fit.tst_08 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_08)
map_ssm(fit.tst_08)

tst <- dat[dat$id == ids[9], ]
fit.tst_09 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_09)
map_ssm(fit.tst_09)

# tst <- dat[dat$id == ids[10], ] #too few data (n = 23)
# fit.tst_10 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
# plot_fit(fit.tst_10)
# map_ssm(fit.tst_10)
 
tst <- dat[dat$id == ids[11], ]
fit.tst_11 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_11)
map_ssm(fit.tst_11)

# tst <- dat[dat$id == ids[12], ]
# fit.tst_12 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
# plot_fit(fit.tst_12)
# map_ssm(fit.tst_12)

tst <- dat[dat$id == ids[13], ]
fit.tst_13 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_13)
map_ssm(fit.tst_13)

tst <- dat[dat$id == ids[14], ]
fit.tst_14 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_14)
map_ssm(fit.tst_14)

tst <- dat[dat$id == ids[15], ]
fit.tst_15 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_15)
map_ssm(fit.tst_15)

tst <- dat[dat$id == ids[16], ]
fit.tst_16 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_16)
map_ssm(fit.tst_16)

tst <- dat[dat$id == ids[17], ]
fit.tst_17 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_17)
map_ssm(fit.tst_17)

tst <- dat[dat$id == ids[18], ]
fit.tst_18 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_18)
map_ssm(fit.tst_18)

tst <- dat[dat$id == ids[19], ]
fit.tst_19 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_19)
map_ssm(fit.tst_19)

tst <- dat[dat$id == ids[20], ]
fit.tst_20 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_20)
map_ssm(fit.tst_20)

tst <- dat[dat$id == ids[21], ]
fit.tst_21 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_21)
map_ssm(fit.tst_21)

tst <- dat[dat$id == ids[22], ]
fit.tst_22 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_22)
map_ssm(fit.tst_22)

tst <- dat[dat$id == ids[23], ]
fit.tst_23 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_23)
map_ssm(fit.tst_23)

tst <- dat[dat$id == ids[24], ]
fit.tst_24 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_24)
map_ssm(fit.tst_24)

tst <- dat[dat$id == ids[25], ]
fit.tst_25 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_25)
map_ssm(fit.tst_25)

tst <- dat[dat$id == ids[26], ]
fit.tst_26 <- fit_ssm(data = tst, model = "DCRWS", tstep = 3/24)
plot_fit(fit.tst_26)
map_ssm(fit.tst_26)

#-----------------------------------------------
## Fit SSSM to all

#ids[10], ids[12]

dat.sub <- subset(dat, dat$id != ids[10])
dat.sub <- subset(dat.sub, dat.sub != ids[12])

fit.sub <- fit_ssm(data = dat.sub, model = "DCRWS", tstep = 3/24, adapt = 40000, samples = 20000, thin = 10)
saveRDS(fit.sub, file = "SSSM.fit.sub2.rds")
#fit.sub <- readRDS("SSSM.fit.sub2.rds")

fits <- list()
for(i in 1:length(fit.sub)){
  f <- fit.sub[[i]]$summary
  fits[i] <- list(f)
}
fits <- do.call(rbind.data.frame, fits)
fits$state <- rep("U", nrow(fits))
fits[fits$b > 1.75, "state"] <- "R"
fits[fits$b < 1.25, "state"] <- "T"


saveRDS(fits, "fit.sub.locs.rds")
#fits <- readRDS("fit.sub.locs.rds")
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

#summary table

fits.sum <- fits.up[!duplicated(fits.up$id), c("year", "id", "start", "end", "dur", "dmax")]
write.csv(fits.sum, file = "summary.csv", row.names = F)

#------------------------------
## 04. Extract environmental variables

# 1. Depth

#Define extent
#Get larger area for mapping
minx <- 10
maxx <- 50
miny <- -55
maxy <- -30

# minx <- min(fits$lon)
# maxx <- max(fits$lon)
# miny <- min(fits$lat)
# maxy <- max(fits$lat)

library(raster)
library(marmap)
# b <- getNOAA.bathy(minx,maxx, miny, maxy, resolution = 1)
# b2 <- as.raster(b)
# writeRaster(b2, filename = "b.grd", format = "raster")
b2 <- raster("b2.grd")

#smaller version for large plot
b3 <- getNOAA.bathy(minx,maxx, miny, maxy, resolution = 4)
b4 <- as.raster(b3)
b4[b4>0] <- NA #set land
writeRaster(b4, filename = "b4.grd", format = "raster")
#b4 <- raster("b4.grd")

b4.p <- rasterToPoints(b4)
b4.p <- data.frame(b4.p)
colnames(b4.p) <- c("lon", "lat", "val")

library(ggplot2)
library(ggmap)

fits.copy <- fits
fits.copy[fits.copy$b > 1.75, "state"] <- "R"
fits.copy[fits.copy$b < 1.25, "state"] <- "T"
fits.copy[fits.copy$b > 1.25 & fits.copy$b < 1.75, "state"] <- "U"

p1 <- ggplot(data = b4.p, aes(x=lon, y=lat))
p1 <- p1 + geom_tile(aes(fill=val))
p1 <- p1 + scale_x_continuous(expand = c(0,0))
p1 <- p1 + scale_y_continuous(expand = c(0,0))
p1 <- p1 + scale_fill_gradient2(low="dodgerblue4", mid="gainsboro", high="burlywood4", midpoint = 0)
p1 <- p1 + geom_point(data = fits.copy, aes(x = fits$lon, y = fits$lat, colour = as.factor(state))) + facet_grid(year ~ .) +
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#999999"))
p1 <- p1 + coord_quickmap()
p1 <- p1 + theme_bw() + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank())
p1

## alternative with grey land, remember to set bath > 0 to 0 if using this:
## or set "fill" of "world" to NA
p2 <- ggplot(data = b4.p, aes(x=lon, y=lat))
p2 <- p2 + geom_tile(aes(fill=val))
p2 <- p2 + scale_fill_gradient(low="gray60", high="gray99", name = "Depth (m)") #if land is NA
p2 <- p2 + geom_point(data = fits.copy, aes(x = fits.copy$lon, y = fits.copy$lat, colour = as.factor(state))) + facet_grid(year ~ .) +
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "#999999"), name = "Behavioural state")
p2 <- p2 + coord_quickmap()
p2 <- p2 + theme_bw() + theme(panel.grid.minor=element_blank(),
                              panel.grid.major=element_blank(),
                              strip.background=element_blank(),
                              panel.border = element_rect(colour = "black"),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank())
world <- borders("world", colour="black", fill = "white", xlim = c(min(fits.copy$lon), max(fits.copy$lon)), ylim = c(min(fits.copy$lat), max(fits.copy$lat)))
p2 <- p2 + world + xlim(minx, maxx) + ylim(miny, maxy) + annotate("point", x = 37.94, y = -46.64, pch = 16, size = 3)
p2

#-----------------------------------

#1. DEPTH
fits.up$DEPTH <- extract(x = b2, y = fits[ , c("lon", "lat")])

#2. DIST.200
#reclassify
land<-c(0, 10000, NA)
shelf<-c(-199, 0, 1)
deepsea<-c(-10000, -200, NA)
cls<- rbind(land, shelf, deepsea)
d<-reclassify(x=b2, rcl=cls)

#distance to shelf
d2<-distance(d)
# writeRaster(d2, filename = "dist200.grd", format = "raster")
# d2 <- raster("dist200.grd")

fits.up$DIST.200 <- extract(x = d2, y = fits[ , c("lon", "lat")])
fits.up$DIST.200 <- fits.up$DIST.200/1000


#Continue Environmental variable extraction with RAADtools on R Server...
saveRDS(fits.up, "fits_up.rds")
#fits.up <- readRDS("fits_up.rds")

#------------------------------
## 05. Modelling

# Read data with predictors back in
fits.env <- readRDS("fits_env.rds")

# Write to CSV for GIS
write.csv(fits.env[fits.env$DEPTH < 0, ], "Fitted_tracks_noland.csv", row.names = F)

#Remove on-land locations
fits.env.sub <- fits.env[fits.env$DEPTH < 0, ]

# Remove uncertain states and reset Transit to 0 and Restricted to 1
fits.env.sub <- fits.env.sub[!is.na(fits.env.sub$state), ]
fits.env.sub[fits.env.sub$state == 1, "state"] <- 0
fits.env.sub[fits.env.sub$state == 2, "state"] <- 1
fits.env.sub$id <- as.factor(fits.env.sub$id)



#gam
library(mgcv)
gam1 <- gam(state ~ s(DEPTH) + s(DIST.200) + s(SST) + s(SSTgrad) +
              s(SSHA) + s(SSHgrad) + s(EKE) + s(PROD) + s(CHL) + s(WINDU) +
              s(WINDV) + s(lat, lon) + s(id, bs="re"), data = fits.env.sub,
            family = binomial, method = "REML", select = TRUE, na.action = "na.omit")
summary(gam1)
plot(gam1, pages = 1)

gam2 <- gam(state ~ s(DEPTH) + s(SST) +
              s(SSHA) + s(PROD) + s(WINDU) +
              s(WINDV) + s(lat, lon) + s(id, bs="re"), data = fits.env.sub,
            family = binomial, method = "REML", select = TRUE, na.action = "na.omit")
summary(gam2)
plot(gam2, pages = 1, shade = T, shade.col = "grey")

library(visreg)
visreg(gam1, rug=2)


#random forest
library(randomForest)

rf1 <- randomForest(formula = as.factor(state) ~ DEPTH + DIST.200 + SST + SSTgrad +
                      SSHA + SSHgrad + EKE + PROD + WINDU + WINDV + CHL,
                    data = fits.env.sub, na.action = "na.omit",
                    keep.inbag = T, keep.forest = T, ntree = 1000, importance = F)
rf1
varImpPlot(rf1, main = NULL, pch = 16)
partialPlot(rf1, pred.data = fits.env.sub, x.var = DEPTH, which.class = 1)

library(forestFloor)
X <- fits.env.sub[ , c("DEPTH", "DIST.200", "SST", "PROD", "CHL", "SSHA", "WINDV", "WINDU", "SSTgrad", "EKE", "SSHgrad")]
X <- X[complete.cases(X), ]
ff = forestFloor(rf1, X ,binary_reg = T,calc_np=T)
Col = fcol(ff,cols=1,outlier.lim = 2.5)
plot(ff,col="grey", plot_GOF = T, orderByImportance=TRUE, GOF_args = list(col="black"))

#------------------------------
#re-try analysis with a more aggresive 1.5 dileneation for behaviour?

fits.env.sub2 <- fits.env
fits.env.sub2[fits.env.sub2$b < 1.5, "state"] <- 0
fits.env.sub2[fits.env.sub2$b > 1.5, "state"] <- 1
fits.env.sub2$id <- as.factor(fits.env.sub2$id)

gam3 <- gam(state ~ s(DEPTH) + s(DIST.200) + s(SST) + s(SSTgrad) +
              s(SSHA) + s(SSHgrad) + s(EKE) + s(PROD) + s(CHL) + s(WINDU) +
              s(WINDV) + s(lat, lon) + s(id, bs="re"), data = fits.env.sub2,
            family = binomial, method = "REML", select = TRUE, na.action = "na.omit")
summary(gam3)
plot(gam3)

#------------------------------
#plots of environmental variables

library(beanplot)
fits.env.sub$state2 <- rep("NA", nrow(fits.env.sub))
fits.env.sub[fits.env.sub$state == 1, "state2"] <- "R"
fits.env.sub[fits.env.sub$state == 0, "state2"] <- "T"


par(mfrow=c(4, 3), mar = c(2, 4, 2, 2))

beanplot(DEPTH ~ state2, data = fits.env.sub,
         main = NULL, ylab = "DEPTH", side = "both",
         ylim = c(min(fits.env.sub$DEPTH, na.rm = T), max(fits.env.sub$DEPTH, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(DIST.200 ~ state2, data = fits.env.sub,
         main = NULL, ylab = "DIST.200", side = "both",
         ylim = c(min(fits.env.sub$DIST.200, na.rm = T), max(fits.env.sub$DIST.200, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(SST ~ state2, data = fits.env.sub,
         main = NULL, ylab = "SST", side = "both",
         ylim = c(min(fits.env.sub$SST, na.rm = T), max(fits.env.sub$SST, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(PROD ~ state2, data = fits.env.sub,
         main = NULL, ylab = "PROD", side = "both",
         ylim = c(min(fits.env.sub$PROD, na.rm = T), max(fits.env.sub$PROD, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(CHL ~ state2, data = fits.env.sub,
         main = NULL, ylab = "CHL", side = "both",
         ylim = c(min(fits.env.sub$CHL, na.rm = T), max(fits.env.sub$CHL, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(SSHA ~ state2, data = fits.env.sub,
         main = NULL, ylab = "SSHA", side = "both",
         ylim = c(min(fits.env.sub$SSHA, na.rm = T), max(fits.env.sub$SSHA, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(WINDV ~ state2, data = fits.env.sub,
         main = NULL, ylab = "WINDV", side = "both",
         ylim = c(min(fits.env.sub$WINDV, na.rm = T), max(fits.env.sub$WINDV, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(WINDU ~ state2, data = fits.env.sub,
         main = NULL, ylab = "WINDU", side = "both",
         ylim = c(min(fits.env.sub$WINDU, na.rm = T), max(fits.env.sub$WINDU, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(SSTgrad ~ state2, data = fits.env.sub,
         main = NULL, ylab = "SSTgrad", side = "both",
         ylim = c(min(fits.env.sub$SSTgrad, na.rm = T), max(fits.env.sub$SSTgrad, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(EKE ~ state2, data = fits.env.sub,
         main = NULL, ylab = "EKE", side = "both",
         ylim = c(min(fits.env.sub$EKE, na.rm = T), max(fits.env.sub$EKE, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))

beanplot(SSHgrad ~ state2, data = fits.env.sub,
         main = NULL, ylab = "SSHgrad", side = "both",
         ylim = c(min(fits.env.sub$SSHgrad, na.rm = T), max(fits.env.sub$SSHgrad, na.rm = T)),
         bw="nrd",
         overallline = "median",
         what=c(1,1,1,0), frame.plot = F,
         maxwidth = 0.4,
         log = "",
         border = NA, col = list("#e41a1c", "#377eb8"))


#------------------------------
## 06. TRIP Analysis

library(trip)

d <- fits.env[ , c("id", "date", "lon", "lat")]
coordinates(d) <- ~lon+lat
proj4string(d) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
tr <- trip(d, c("date", "id")) #create trip object
grd <- makeGridTopology(tr, cellsize = c(.25, .25)) #create grid topology
trg <- tripGrid(tr, grid = grd) #create time spent sp data.frame

tsr <- raster(trg) #create a raster
tsr <- tsr/(60*60) #convert to hours
writeRaster(tsr, "tsr.grd", format = "raster", overwrite = TRUE) #write to file

test <- rasterToPoints(tsr)

tsdf <- as.data.frame(trg) #create a data frame
names(tsdf) <- c("t", "lon", "lat")
tsdf$t <- tsdf$t/(60*60)

#distances
dsts <- fits[ , 1:2]
dsts$d <- trackDistance(tr, prev = F, longlat = T)

dis.id <- unique(dsts$id)
dis.l <- list()

for(i in 1:length(dis.id)){
  a <- dsts[dsts$id == dis.id[i], ]
  b <- sum(a$d)
  dis.l[i] <- list(b)
}

dis <- cbind(dis.id, unlist(dis.l))
dis <- as.data.frame(dis)
names(dis) <- c("id", "d")
dis$distance <- round(dis$d , digits = 0)
write.csv(dis, "cumulative_distances.csv", row.names = F)
#------------------------------
## 07. Extract environmental variables for time-spent raster

#----------
# Static:

#1. DEPTH
tsdf$DEPTH <- extract(x = b2, y = tsdf[ , c("lon", "lat")])

#2. DIST.200
tsdf$DIST.200 <- extract(x = d2, y = tsdf[ , c("lon", "lat")])
tsdf$DIST.200 <- tsdf$DIST.200/1000

#----------
# Dynamic:

#Will use the mean of values from individual locations falling in a cell
tracks.sp <- fits.env
coordinates(tracks.sp) <- ~lon+lat
proj4string(d) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")



#Rasterize each variable and extract

#SST
r.SST<- rasterize(tracks.sp, tsr, field = "SST", fun = mean, na.rm = F)
tsdf$SST <- extract(x = r.SST, y = tsdf[ , c("lon", "lat")])
rm(r.SST)


#SSTgrad
r.SSTgrad<- rasterize(tracks.sp, tsr, field = "SSTgrad", fun = mean, na.rm = T)
tsdf$SSTgrad <- extract(x = r.SSTgrad, y = tsdf[ , c("lon", "lat")])
rm(r.SSTgrad)

#SSHA
r.SSHA<- rasterize(tracks.sp, tsr, field = "SSHA", fun = mean, na.rm = T)
tsdf$SSHA <- extract(x = r.SSHA, y = tsdf[ , c("lon", "lat")])
rm(r.SSHA)

#SSHgrad
r.SSHgrad<- rasterize(tracks.sp, tsr, field = "SSHgrad", fun = mean, na.rm = T)
tsdf$SSHgrad <- extract(x = r.SSHgrad, y = tsdf[ , c("lon", "lat")])
rm(r.SSHgrad)

#EKE
r.EKE<- rasterize(tracks.sp, tsr, field = "EKE", fun = mean, na.rm = T)
tsdf$EKE <- extract(x = r.EKE, y = tsdf[ , c("lon", "lat")])
rm(r.EKE)

#MLD
# r.MLD<- rasterize(tracks.sp, tsr, field = "MLD", fun = mean, na.rm = T)
# tsdf$MLD <- extract(x = r.MLD, y = tsdf[ , c("lon", "lat")])
# rm(r.MLD)

#DEPTH
r.DEPTH<- rasterize(tracks.sp, tsr, field = "DEPTH", fun = mean, na.rm = T)
tsdf$DEPTH <- extract(x = r.DEPTH, y = tsdf[ , c("lon", "lat")])
rm(r.DEPTH)

#PROD
r.PROD <- rasterize(tracks.sp, tsr, field = "PROD", fun = mean, na.rm = T)
tsdf$PROD <- extract(x = r.PROD, y = tsdf[ , c("lon", "lat")])
rm(r.PROD)

#WINDU
r.WINDU <- rasterize(tracks.sp, tsr, field = "WINDU", fun = mean, na.rm = T)
tsdf$WINDU <- extract(x = r.WINDU, y = tsdf[ , c("lon", "lat")])
rm(r.WINDU)

#WINDv
r.WINDV <- rasterize(tracks.sp, tsr, field = "WINDV", fun = mean, na.rm = T)
tsdf$WINDV <- extract(x = r.WINDV, y = tsdf[ , c("lon", "lat")])
rm(r.WINDV)

saveRDS(tsdf, "tsdf_env.rds")

#------------------------------
## 08. Model time spent

## Results no good!

tsdf.env <- tsdf[tsdf$t > 0, ]
tsdf.complete <- tsdf.env[complete.cases(tsdf.env), ]
#remove Marion Island cell (t > 400)
tsdf.complete <- tsdf.complete[tsdf.complete$t < 300, ]

#GAM
gam.t <- gam(t ~ s(DEPTH) + s(DIST.200) + s(SST) + s(SSTgrad) +
              s(SSHA) + s(SSHgrad) + s(EKE) + s(PROD) + s(CHL) + s(WINDU) +
              s(WINDV) + s(lat, lon), data = tsdf.complete,
            family = Gamma, method = "REML", select = TRUE, na.action = "na.omit")
summary(gam.t)
plot(gam.t, pages = 1)

#RF
rf.t <- randomForest(formula = t ~ DEPTH + DIST.200 + SST + SSTgrad +
                      SSHA + SSHgrad + EKE + PROD + WINDU + WINDV + CHL,
                    data = tsdf.complete, na.action = "na.omit",
                    keep.inbag = T, keep.forest = T)
rf.t
varImpPlot(rf.t)
