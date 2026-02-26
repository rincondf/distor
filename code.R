require(ncdf4)

data_2023 <- nc_open("tmin_2023.nc")
data_2023_ID <- ncvar_get(data_2023, "station_name")
data_2023_long <- ncvar_get(data_2023, "stn_lon")
data_2023_lat <- ncvar_get(data_2023, "stn_lat")
data_2023_obs <- ncvar_get(data_2023, "obs")
data_2023_pred <- ncvar_get(data_2023, "pred")







data2023Mins <- data.frame(ID = data_2023_ID, lat = data_2023_lat, 
                           long = data_2023_long)

data2023Mins <- data2023Mins[which(data2023Mins$lat < 49 & data2023Mins$lat > 46), ]

data2023Mins <- data2023Mins[which(data2023Mins$long > -125 & data2023Mins$long < -117), ]


min_2023_obs <- data_2023_obs[, as.numeric(row.names(data2023Mins))]
min_2023_pred <- data_2023_pred[, as.numeric(row.names(data2023Mins))]

min_2023_res <- min_2023_obs - min_2023_pred
min_2023_res <- min_2023_res^2

mse_2023_res <- colMeans(min_2023_res, na.rm = TRUE)


min_2023_obs <- c(min_2023_obs)
min_2023_pred <- c(min_2023_pred)


data2023Mins$mse <- mse_2023_res



write.csv(data2023Mins, file = "locs2023.csv")




plot(min_2023_obs, min_2023_pred)
abline(0, 1, col = "blue", lwd =  2)

mod_mins <- lm(min_2023_pred ~ min_2023_obs)
summary(mod_mins)
abline(mod_mins, col = "red", lwd = 2)




meta2023Mins <- meta_tmin2023[which(meta_tmin2023$stn_lat < 49 & meta_tmin2023$stn_lat > 46), ]
meta2023Mins <- meta2023Mins[which(meta2023Mins$stn_lon > -125 & meta2023Mins$stn_lon < -117), ]










data_2023 <- nc_open("tmax_2023.nc")
data_2023_ID <- ncvar_get(data_2023, "station_name")
data_2023_long <- ncvar_get(data_2023, "stn_lon")
data_2023_lat <- ncvar_get(data_2023, "stn_lat")
data_2023_obs <- ncvar_get(data_2023, "obs")
data_2023_pred <- ncvar_get(data_2023, "pred")



data2023Max <- data.frame(ID = data_2023_ID, lat = data_2023_lat, 
                           long = data_2023_long)

data2023Max <- data2023Max[which(data2023Max$lat < 49 & data2023Max$lat > 46), ]

data2023Max <- data2023Max[which(data2023Max$long > -125 & data2023Max$long < -117), ]


max_2023_obs <- data_2023_obs[, as.numeric(row.names(data2023Max))]
max_2023_pred <- data_2023_pred[, as.numeric(row.names(data2023Max))]

max_2023_res <- max_2023_obs - max_2023_pred
max_2023_res <- max_2023_res^2

mse_2023_resM <- colMeans(max_2023_res, na.rm = TRUE)


max_2023_obs <- c(max_2023_obs)
max_2023_pred <- c(max_2023_pred)


data2023Max$mse <- mse_2023_resM



write.csv(data2023Max, file = "locs2023max.csv")



plot(max_2023_obs, max_2023_pred)
abline(0, 1, col = "blue", lwd =  2)

mod_max <- lm(max_2023_pred ~ max_2023_obs)
summary(mod_max)
abline(mod_max, col = "red", lwd = 2)



# Checking if cross-validation includes the same stations used to produce predicted values


meta2023Max <- meta_tmax3023[which(meta_tmax3023$stn_lat < 49 & meta_tmax3023$stn_lat > 46), ]
meta2023Max <- meta2023Max[which(meta2023Max$stn_lon > -125 & meta2023Max$stn_lon < -117), ]













test <- calc_dd_vec(tmax = rep(25, 30), tmin = seq(-9, 5, length.out = 30), lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal")

plot(seq(-9, 5, length.out = 30), test)

plot(seq(0, 360), sin(seq(0, 360)*pi/180))
points(seq(0, 360), 2*sin(seq(0, 360)*pi/180))







tmax = 30
tmin = 10


A = (tmax - tmin) / 2 
D = (tmax + tmin) / 2
B = 2 * pi / 24
C = 6


plot(seq(0, 48), A*sin(B*(seq(0, 48) - C)) + D, type = "l")


lines(seq(0, 48), A*sin(B*(seq(0, 48) - C)) + D)

abline(h = 12)
abline(h = 25)


# error in mins

min_bel <- abs(calc_dd_vec(tmax = 30, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal") - 
                 calc_dd_vec(tmax = 30, tmin = 9.9, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal"))


min_abo <- abs(calc_dd_vec(tmax = 30, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal") - 
                 calc_dd_vec(tmax = 30, tmin = 10.1, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal"))

min_bel + min_abo


# error in maxs

max_bel <- abs(calc_dd_vec(tmax = 30, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal") - 
                 calc_dd_vec(tmax = 29.9, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal"))

max_abo <- abs(calc_dd_vec(tmax = 30.1, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal") - 
                 calc_dd_vec(tmax = 30, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal"))


max_bel + max_abo

#####

min_bel + max_bel

min_abo + max_abo

####

devs <- c(5, 2, 1, 3, 4, 0.5, 0.1)

mins_err <- c(4.00, 1.73, 0.90, 2.52, 3.27, 0.46, 0.09)


mod_dev <- lm(mins_err ~ devs)
summary(mod_dev)

plot(devs, mins_err)
abline(mod_dev)


###########


single_sin <- function(x, tu, tl) {
  tmax = 15
  tmin = -5
  
  
  
  A = (tmax - tmin) / 2 
  D = (tmax + tmin) / 2
  B = 2 * pi / 1
  C = 1/4
  
  tem <- rep(NA, length(x))
  
  for(i in 1: length(x)) {
    tem[i] <- (A*sin(B*(x[i] - C)) + D)
    if(tem[i] >= tu) tem[i] <- tu
    tem[i] <- tem[i] - tl
    if(tem[i] < 0) tem[i] <- 0
  }
  
 
  tem
  
}

plot(seq(0, 1, 0.01), single_sin(seq(0, 1, 0.01), tu = 25, tl = 6), type = "l")

integrate(single_sin, lower = 0, upper = 1, tu = 25, tl = 6)




single_sinA <- function(x) {
  tmax = 35
  tmin = 10
  
  
  
  A = (tmax - tmin) / 2 
  D = (tmax + tmin) / 2
  B = 2 * pi / 1
  C = 1/4
  
  (A*sin(B*(x - C)) + D)
  
}


plot(seq(0, 1, 0.01), single_sinA(seq(0, 1, 0.01)), type = "l", ylim = c(0, 35.5), lwd = 2)
abline(h = 10, lty = 2)
abline(h = 29, lty = 2)

lines(seq(0, 1, 0.01), single_sinA(seq(0, 1, 0.01)), type = "l", col = "red", lwd = 2)






#############################


colnames(awn_2023_summary) <- c("station_id", "date", "temp_max", "temp_min")

awn_2023_summary$date <- as.Date(awn_2023_summary$date, format = "%Y/%m/%d")
awn_2023_summary$julian <- as.numeric(format(awn_2023_summary$date, "%j"))


awn_2023_summary$long <- rep(NA, 121210)
awn_2023_summary$lat <- rep(NA, 121210)



for(i in 1: length(awn_stations$station_id)) {
  awn_2023_summary$long[which(awn_2023_summary$station_id == awn_stations$station_id[i])] <-
    awn_stations$lng[which(awn_stations$station_id == awn_stations$station_id[i])]
}


for(i in 1: length(awn_stations$station_id)) {
  awn_2023_summary$lat[which(awn_2023_summary$station_id == awn_stations$station_id[i])] <-
    awn_stations$lat[which(awn_stations$station_id == awn_stations$station_id[i])]
}



library(daymetr)

temp_recs <- list()

for(i in 1: length(awn_stations$station_id)) {
  temp_recs[[i]] <- download_daymet(
    site = "Daymet",
    lat = awn_stations$lat[i],
    lon = awn_stations$lng[i],
    start = 2023,
    end = 2023,
    path = tempdir(),
    internal = TRUE,
    silent = FALSE,
    force = FALSE,
    simplify = FALSE
  )
}


tmax <- list()

for(i in 1: length(awn_stations$station_id)) {
  tmax[[i]] <- as.numeric(temp_recs[[i]]$data[,7])
}

tmin <- list()

for(i in 1: length(awn_stations$station_id)) {
  tmin[[i]] <- as.numeric(temp_recs[[i]]$data[,8])
}




awn_2023_summary <- awn_2023_summary[-which(awn_2023_summary$date > "2023-12-31"), ]

a <- tapply(awn_2023_summary$temp_max, awn_2023_summary$station_id, length)
a <- a[which(a == 365)]
a <- as.numeric(names(a))


awn_2023_summary <- awn_2023_summary[which(awn_2023_summary$station_id %in% a), ]

tapply(awn_2023_summary$temp_max, awn_2023_summary$station_id, length)

awn_2023_summary$t_maxDM <- rep(NA, 119355)
awn_2023_summary$t_minDM <- rep(NA, 119355)




for(i in 1: length(awn_stations$station_id)) {
  awn_2023_summary$t_maxDM[which(awn_2023_summary$station_id == awn_stations$station_id[i])] <- tmax[[i]]
}

for(i in 1: length(awn_stations$station_id)) {
  awn_2023_summary$t_minDM[which(awn_2023_summary$station_id == awn_stations$station_id[i])] <- tmin[[i]]
}


awn_2023_summary$temp_max <- (awn_2023_summary$temp_max - 32) * (5/9)
awn_2023_summary$temp_min <- (awn_2023_summary$temp_min - 32) * (5/9)

awn_2023_summary <- awn_2023_summary[-which(awn_2023_summary$temp_max > 50), ]
awn_2023_summary <- awn_2023_summary[-which(awn_2023_summary$temp_min == -17.77777777777777777777777), ]

plot(awn_2023_summary$temp_max, awn_2023_summary$t_maxDM)
abline(0, 1, col = "blue", lwd =  2)

mod_max_awn <- lm(awn_2023_summary$t_maxDM ~ awn_2023_summary$temp_max)
summary(mod_max_awn)
abline(mod_max_awn, col = "red", lwd = 2)


###

plot(awn_2023_summary$temp_min, awn_2023_summary$t_minDM)
abline(0, 1, col = "blue", lwd =  2)

mod_min_awn <- lm(awn_2023_summary$t_minDM ~ awn_2023_summary$temp_min)
summary(mod_min_awn)
abline(mod_min_awn, col = "red", lwd = 2)


##########


awn_2023_summary$se_max <- (awn_2023_summary$temp_max - awn_2023_summary$t_maxDM)^2
awn_2023_summary$se_min <- (awn_2023_summary$temp_min - awn_2023_summary$t_minDM)^2





max_mse_awn <- data.frame(mse = as.numeric(tapply(awn_2023_summary$se_max, awn_2023_summary$station_id, mean, na.rm = TRUE)),
                          station_id = as.numeric(dimnames(tapply(awn_2023_summary$se_max, awn_2023_summary$station_id, mean, na.rm = TRUE))[[1]]))




max_mse_awn$long <- rep(NA, 327)
max_mse_awn$lat <- rep(NA, 327)



for(i in 1: length(awn_stations$station_id)) {
  max_mse_awn$long[which(max_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lng[which(awn_stations$station_id == awn_stations$station_id[i])]
}


for(i in 1: length(awn_stations$station_id)) {
  max_mse_awn$lat[which(max_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lat[which(awn_stations$station_id == awn_stations$station_id[i])]
}





max_mse_awn <- data.frame(mse = as.numeric(tapply(awn_2023_summary$se_max, awn_2023_summary$station_id, mean, na.rm = TRUE)),
                          station_id = as.numeric(dimnames(tapply(awn_2023_summary$se_max, awn_2023_summary$station_id, mean, na.rm = TRUE))[[1]]))




max_mse_awn$long <- rep(NA, 327)
max_mse_awn$lat <- rep(NA, 327)



for(i in 1: length(awn_stations$station_id)) {
  max_mse_awn$long[which(max_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lng[which(awn_stations$station_id == awn_stations$station_id[i])]
}


for(i in 1: length(awn_stations$station_id)) {
  max_mse_awn$lat[which(max_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lat[which(awn_stations$station_id == awn_stations$station_id[i])]
}



#############






min_mse_awn <- data.frame(mse = as.numeric(tapply(awn_2023_summary$se_min, awn_2023_summary$station_id, mean, na.rm = TRUE)),
                          station_id = as.numeric(dimnames(tapply(awn_2023_summary$se_min, awn_2023_summary$station_id, mean, na.rm = TRUE))[[1]]))



min_mse_awn$long <- rep(NA, 327)
min_mse_awn$lat <- rep(NA, 327)



for(i in 1: length(awn_stations$station_id)) {
  min_mse_awn$long[which(min_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lng[which(awn_stations$station_id == awn_stations$station_id[i])]
}


for(i in 1: length(awn_stations$station_id)) {
  min_mse_awn$lat[which(min_mse_awn$station_id == awn_stations$station_id[i])] <-
    awn_stations$lat[which(awn_stations$station_id == awn_stations$station_id[i])]
}


write.csv(max_mse_awn, file = "max_mse_awn.csv")
write.csv(min_mse_awn, file = "min_mse_awn.csv")
