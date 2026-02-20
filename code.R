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
abline(mod_mins, col = "red", lwd = 2)









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
abline(mod_max, col = "red", lwd = 2)













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



calc_dd_vec(tmax = 30, tmin = 10, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal") - 
  calc_dd_vec(tmax = 30, tmin = 5, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal")





calc_dd_vec(tmax = 35, tmin = 5, lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal")
calc_dd_vec(tmax = 10, tmin = -5, lower_threshold = 6, upper_threshold = 25, cutoff = "horizontal")



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
  tmax = 22
  tmin = -2
  
  
  
  A = (tmax - tmin) / 2 
  D = (tmax + tmin) / 2
  B = 2 * pi / 1
  C = 1/4
  
  (A*sin(B*(x - C)) + D)
  
}


plot(seq(0, 1, 0.01), single_sinA(seq(0, 1, 0.01)), type = "l", ylim = c(-5, 25.5), lwd = 2)
abline(h = 25, lty = 2)
abline(h = 5, lty = 2)

lines(seq(0, 1, 0.01), single_sinA(seq(0, 1, 0.01)), type = "l", col = "red", lwd = 2)
