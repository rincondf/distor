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
write.csv(data2023Mins, file = "locs2023.csv")

row.names(data2023Mins)


data_2023_obs <- data_2023_obs[, as.numeric(row.names(data2023Mins))]
data_2023_pred <- data_2023_pred[, as.numeric(row.names(data2023Mins))]


plot(data_2023_obs, data_2023_pred)
abline(0, 1)







test <- calc_dd_vec(tmax = rep(25, 30), tmin = seq(-9, 5, length.out = 30), lower_threshold = 10, upper_threshold = 29, cutoff = "horizontal")

plot(seq(-9, 5, length.out = 30), test)








