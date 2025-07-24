#install.packages("pangaear")
library("pangaear")
library("stringr")
library("sp")
library("sf")
library("terra")
library("raster")
library("dplyr")
library("plyr")
#install.packages("clock")
library("clock")
#install.packages("lubridate")
library("lubridate")
library("ncdf4")
library("zoo")
library("sdmpredictors")
library("leaflet")
library("dsm")
library("dplyr")
library("graph4lg")
library("geosphere")
library("CAST")
#library("doParallel")
#library("GSIF")
library("SpatialML")
library("sf")
library("dplyr")
library("ncdf4")
library("fields")
library("raster")
require(rgdal)
library("zoo")
library("caret")
library("randomForest")
require("purrr")
library("pdp")
library("readxl")
library("rgeos")
library("alphahull")
library("gstat")
library("caret")
#install.packages("raster")
library("raster")
library("terra")
library("abind")


options(timeout = max(10000, getOption("timeout")))
files_dir <- "~/UCD/Legacy_data_paper/R_objects"

shapefiles_dir <- "~/UCD/shapefiles/"
#Creating boundaries/parameters for the output rasters.
world <- read_sf("~/QU/GIS shapefiles/World_Continents/World_Continents.shp")
GB_and_I <- read_sf(paste(shapefiles_dir, "GB_I_Isles/GB_I_Isles.shp", sep = ""))
inference_shp <- read_sf(paste(shapefiles_dir, "inference_shp/inference_shp.shp", sep = ""))
dublin_bay <- read_sf(paste(shapefiles_dir, "Dublin_bay2/Dublin_bay2.shp", sep = ""))
irish_sea <- read_sf(paste(shapefiles_dir, "Irish_Sea/Model_Area.shp", sep = ""))
nwe_shelf <- read_sf(paste(shapefiles_dir, "NW_Europe_Shelf/NW_Europe_Shelf.shp", sep = ""))
area_of_interest <- nwe_shelf


#Vars inside CMEMES_MOD_GLO...nc files: {"so": salinity}, {"thetao": temperature}, {"uo": eastward sea water velocity},
#{"vo": northward sea water velocity}.

#Depth levels inside CMEMES_MOD_GLO...nc files: 1 - 35

cell_size <- c(0.083, 0.083)

aoi_rast <- rast(area_of_interest)
res(aoi_rast) <- cell_size

#create a grid
cmems_raster <- aoi_rast
res(cmems_raster) <- cell_size
cmems_raster <- crop(cmems_raster, area_of_interest)
#adding data into raster to avoid 'no data' error
cmems_raster[] <- 1:ncell(cmems_raster)
cmems_raster



new_pts <- as.data.frame(cmems_raster, xy = T)
new_pts <- new_pts[, 1:2]
pts <- vect(new_pts, geom = c("x", "y"), crs = crs(cmems_raster))
pts


if(FALSE){		#Comment out large section of code here	

var_name <- c("thetao", "so", "uo", "vo")
files <- list.files("~/UCD/datasets/CMEMS", full.names = T)

var_rasts <- list()
for(n in 3:length(var_name)){
	file_rasts <- list()
	for(m in 1:length(files)){
		out_list <- list()
		output_rast <- list()
		for(j in 1:24){
			out_mat <- matrix(0, nrow(new_pts), 35)
			for(i in 1:35){
				nc_rast <- raster(files[m], varname = var_name[n], level = i, band = j)		#level = depth, band = month
				nc_rast <- crop(nc_rast, nwe_shelf)
				nc_rast <- rast(nc_rast)
				ext_pts <- terra::extract(nc_rast, pts)
				out_mat[, i] <- ext_pts[, 2]
			}
	
			out_list[[j]] <- out_mat
			bot_var <- c()
			for(i in 1:nrow(out_list[[j]])){
				var_index <- ifelse(is.na(sum(out_list[[j]][i, 1])) == TRUE, 0, length(na.omit(out_list[[j]][i, ])))
				bot_var[i] <- ifelse(var_index < 1, NA, out_list[[j]][i, var_index])
			}
	
			mon_df <- data.frame(new_pts, bot_var)
			rasterMeanPoints<- rasterize(x=vect(mon_df, geom = c("x", "y")), y=cmems_raster, field='bot_var', fun=mean)
			output_rast[[j]] <- focal(rasterMeanPoints, w=9, fun=mean, na.policy="only")
			plot(output_rast[[j]], main = paste(var_name[n], j))
		}
	file_rasts[[m]] <- output_rast
	print(m)
	}
	var_rasts[[n]] <- file_rasts
	print(var_name[n])
}
}

cell_multiplier <- 2.5	#This is to try different resolutions. The original resolution is the same as EMODNet
 
cell_size <- c(0.00117, 0.002075) * cell_multiplier
 
aoi_rast <- rast(area_of_interest)
res(aoi_rast) <- cell_size

#create a grid
blankRaster <- aoi_rast
res(blankRaster) <- cell_size * 10
blankRaster <- crop(blankRaster, area_of_interest)
#adding data into raster to avoid 'no data' error
blankRaster[] <- 1:ncell(blankRaster)
blankRaster


if(FALSE){
out_rast <- list()
for(j in 3:length(var_name)){
	mos <- list()
	for(i in 1:length(var_rasts[[j]])){
		rlist <- var_rasts[[j]][[i]]
		rlist$fun <- mean
		mos[[i]] <- do.call(mosaic, rlist)
	}

	mos$fun <- mean
	mean_rast <- do.call(mosaic, mos)		#Mean monthly Salinity from 2000 to 2020
	mean_rast <- resample(mean_rast, blankRaster)
	mean_rast <- mask(mean_rast, vect(area_of_interest))
	mean_rast <- mask(mean_rast, vect(GB_and_I), inverse = T)
	out_rast[[j]] <- crop(mean_rast, area_of_interest)
}

names(out_rast) <- c("temperature", "salinity", "uo", "vo")

par(mfrow = c(2, 2))
plot(crop(out_rast$temperature, irish_sea), main = names(out_rast)[1])
plot(crop(out_rast$salinity, irish_sea), main = names(out_rast)[2])
plot(crop(out_rast$uo, irish_sea), main = names(out_rast)[3])
plot(crop(out_rast$vo, irish_sea), main = names(out_rast)[4])
par(mfrow = c(1, 1))


setwd(paste(files_dir, "/bottom_salinity", sep = ""))
writeRaster(out_rast$salinity, "original.tif", overwrite = T)

setwd(paste(files_dir, "/bottom_temp", sep = ""))
writeRaster(out_rast$temperature, "original.tif", overwrite = T)


setwd(paste(files_dir, "/bottom_current_velocity", sep = ""))
install.packages("remotes")
remotes::install_github("edwardlavender/fvcom.tbx")
library("fvcom.tbx")

u_vals <- as.data.frame(out_rast$uo, xy=T)
v_vals <- as.data.frame(out_rast$vo, xy=T)
components <- list(u_vals[, 3], v_vals[, 3])
current <- calc_speed(components)
current_df <- data.frame(u_vals[, 1:2], current)
names(current_df) <- c("lon", "lat", "current")

current_ras <- rasterize(vect(current_df), blankRaster, field='current', fun=mean)
writeRaster(current_ras, "mean_original.tif", overwrite = T)



var_name <- c("uo", "vo")
files <- list.files("~/UCD/datasets/CMEMS", full.names = T)

var_rasts <- list()
for(n in 1:length(var_name)){
	file_rasts <- list()
	for(m in 1:length(files)){
		out_list <- list()
		output_rast <- list()
		for(j in 1:24){
			out_mat <- matrix(0, nrow(new_pts), 35)
			for(i in 1:35){
				nc_rast <- raster(files[m], varname = var_name[n], level = i, band = j)		#level = depth, band = month
				nc_rast <- crop(nc_rast, nwe_shelf)
				nc_rast <- rast(nc_rast)
				ext_pts <- terra::extract(nc_rast, pts)
				out_mat[, i] <- ext_pts[, 2]
			}
	
			out_list[[j]] <- out_mat
			bot_var <- c()
			for(i in 1:nrow(out_list[[j]])){
				var_index <- ifelse(is.na(sum(out_list[[j]][i, 1])) == TRUE, 0, length(na.omit(out_list[[j]][i, ])))
				bot_var[i] <- ifelse(var_index < 1, NA, out_list[[j]][i, var_index])
			}
	
			mon_df <- data.frame(new_pts, bot_var)
			rasterMeanPoints<- rasterize(x=vect(mon_df, geom = c("x", "y")), y=cmems_raster, field='bot_var', fun=max)
			output_rast[[j]] <- focal(rasterMeanPoints, w=9, fun=max, na.policy="only")
			plot(output_rast[[j]], main = paste(var_name[n], j))
		}
	file_rasts[[m]] <- output_rast
	print(m)
	}
	var_rasts[[n]] <- file_rasts
	print(var_name[n])
}



out_rast <- list()
for(j in 1:length(var_name)){
	mos <- list()
	for(i in 1:length(var_rasts[[j]])){
		rlist <- var_rasts[[j]][[i]]
		rlist$fun <- max
		mos[[i]] <- do.call(mosaic, rlist)
	}

	mos$fun <- max
	mean_rast <- do.call(mosaic, mos)		#Mean monthly Salinity from 2000 to 2020
	mean_rast <- resample(mean_rast, blankRaster)
	mean_rast <- mask(mean_rast, vect(area_of_interest))
	mean_rast <- mask(mean_rast, vect(GB_and_I), inverse = T)
	out_rast[[j]] <- crop(mean_rast, area_of_interest)
}

names(out_rast) <- c("uo", "vo")

par(mfrow = c(1, 2))
plot(crop(out_rast$uo, irish_sea), main = names(out_rast)[1])
plot(crop(out_rast$vo, irish_sea), main = names(out_rast)[2])
par(mfrow = c(1, 1))

setwd(paste(files_dir, "/bottom_current_velocity", sep = ""))
u_vals <- as.data.frame(out_rast$uo, xy=T)
v_vals <- as.data.frame(out_rast$vo, xy=T)
components <- list(u_vals[, 3], v_vals[, 3])
current <- calc_speed(components)
current_df <- data.frame(u_vals[, 1:2], current)
names(current_df) <- c("lon", "lat", "current")

current_ras <- rasterize(vect(current_df), blankRaster, field='current', fun=max)
writeRaster(current_ras, "max_original.tif", overwrite = T)
}




out_rast <- list()
out_rast$salinity <- rast("~/UCD/Legacy_data_paper/R_objects/bottom_salinity/original.tif")
out_rast$temperature <- rast("~/UCD/Legacy_data_paper/R_objects/bottom_temp/original.tif")

q_step <- 0.01

library('qmap')

##########################################################################################

				#	CHLOROPHYLL - A	#

##########################################################################################

setwd(files_dir)
out_df <- readRDS("surface_chl_pangaea.RData")
str(out_df)

setwd(paste(files_dir, "/surface_chl", sep = ""))

start_date <- as.POSIXlt("1999-12-01")
end_date <- as.POSIXlt("2020-01-01")
out_df <- out_df[out_df$year >= start_date & out_df$year < end_date, ]


#Crop data to area of interest and average data points within a certain cell size of each other
masked_pts <- terra::mask(vect(out_df), vect(area_of_interest))
cropped_pts <- terra::crop(masked_pts, area_of_interest)

st_pts <- st_as_sf(cropped_pts)
response_data <- data.frame(st_pts$chl, st_pts$date_time)
coordinate_data <- data.frame(st_coordinates(st_pts$geometry))

plot_pts <- data.frame(coordinate_data, response_data)
#plot_pts <- out_df[, c(1, 2, 5, 4)]
names(plot_pts) <- c("lon", "lat", "chl", "date_time")
pts <- plot_pts[, 1:2]
names(pts) <- c("lon", "lat")
plot_pts <- plot_pts[plot_pts$chl>0, ]
str(plot_pts)


#Inspect salinity data to see if there's any weird values
hist(plot_pts$chl)
median(plot_pts$chl)


interp_rast <- list()
files <- list.files("/Users/mark/UCD/Legacy_data_paper/R_objects/surface_chl/interp_rasts", full.names = TRUE)
for(i in 1:length(files)){
	interp_rast[[i]] <- rast(files[[i]])
}


obs_rast <- mosaic(interp_rast[[1]], interp_rast[[2]], interp_rast[[3]],
		interp_rast[[4]], interp_rast[[5]], interp_rast[[6]],
		interp_rast[[7]], interp_rast[[8]], interp_rast[[9]],
		interp_rast[[10]], interp_rast[[11]], interp_rast[[12]],
		fun = "mean")

par(mfrow = c(1, 1))
plot(obs_rast)


##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################

#Is there systematic bias in the model (bio-oracle) data compared to observed (inerpolated) data for the original sampling points?
pts <- plot_pts
pts <- vect(pts)
obs_rast <- resample(obs_rast, blankRaster)
obs_dat <- extract(obs_rast, pts)

#Extract model data
nc_rasts <- list()
nc_file <- "~/UCD/datasets/copernicus/chl/cmems_obs-oc_glo_bgc-plankton_my_l4-multi-4km_P1M_1709032972734.nc"
for(i in 1:240){
	nc_raster <- raster(nc_file, band = i)
	nc_rast_crop <- crop(nc_raster, area_of_interest)
	nc_rasts[[i]] <- rast(nc_rast_crop)
}


rlist <- nc_rasts
rlist$fun <- mean
mean_rast <- do.call(mosaic, rlist)	
mean_rast <- resample(mean_rast, blankRaster)
mean_rast <- mask(mean_rast, vect(area_of_interest))
mean_rast <- mask(mean_rast, vect(GB_and_I), inverse = T)
resample_mod_rast <- crop(mean_rast, area_of_interest)
mod_dat <- extract(resample_mod_rast, pts)
plot(resample_mod_rast)


dat <- data.frame(obs_dat[, 2], mod_dat[, 2])
names(dat) <- c("obs", "mod")
dat <- na.omit(dat)
str(dat)


#Just look at spread of data
setwd(paste(files_dir, "/surface_chl", sep = ""))
tiff("Data spread.tiff", units="cm", width=15, height=15, res=200)
ggplot(dat, aes(mod, obs)) + geom_point() +	
	xlab("Model chlorophyll-a (µg l)") + ylab("Observation chlorophyll-a (µg l)")
dev.off()

#QQ correction of whole model raster using whole interpolated observation raster
#Extract values from model and observation rasters
obs_vals <- values(obs_rast)
mod_vals <- values(resample_mod_rast)
pts <- as.data.frame(blankRaster, xy = T)
dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
names(dat_vals) <- c("lon", "lat", "obs", "mod")
dat_vals <- na.omit(dat_vals)
str(dat_vals)

#Fit and apply QQ correction model to bio-oracle model data
library('qmap')
qmap_fit <- fitQmap(dat_vals$obs, dat_vals$mod, method = "QUANT", qstep = q_step, wet.day = FALSE)
quant_correct <- doQmap(dat_vals$mod, qmap_fit)
dat_correct <- data.frame(dat_vals, quant_correct)
str(dat_correct)


#Plot Cumulative Distribution Function of observed and modelled data
chlorophyll_cdf <- ggplot() + 
			stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_correct, geom = "point", size = 1.05, show.legend=T) +
			stat_ecdf(aes(x = quant_correct, colour = "Adjusted"), data = dat_correct, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
			stat_ecdf(aes(x = obs, colour = "Observations"), data = dat, geom = "point", size = 1.05, show.legend=T) +				
			xlab(expression("Chlorophyll-a (µg" *~l^-1*")")) + ylab("Cumulative distribution") +
			theme_bw(base_size=14) +
  			theme(legend.position = "bottom") +
  			theme(legend.title = element_blank()) +
		  	theme(legend.position="none") + 
  			guides(colour = guide_legend(override.aes = list(size = 3.5)))

tiff("Original_CDF.tiff", units="cm", width=15, height=15, res=200)
plot(chlorophyll_cdf)
dev.off()



##########################################################################################

		#	PLOT ADJUSTED MODEL DATA AND COMPARE TO ORIGINAL		#

##########################################################################################


corr_df <- dat_correct[, c(1, 2, 5)]
corr_rast <- rasterize(vect(corr_df), blankRaster, field = "quant_correct", fun = "mean")
plot(corr_rast)



#	Compare to BIO-ORACLE
pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))

r4 <- clamp(obs_rast, 0, 4) 
r2 <- clamp(resample_mod_rast, 0, 4) 
r1 <- clamp(corr_rast, 0, 4) 
r3 <- clamp(corr_rast - resample_mod_rast, 0, 5) 



tiff("Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 4))
plot(r4, col = pal(500), main = "Averaged monthly observations")
plot(vect(plot_pts), add = T, pch = 20, col = "red", cex = 0.1)
plot(r2, col = pal(500), main = "Original model")
plot(r1, col = pal(500), main = "Bias adjusted")
plot(r3, col = pal(100), main = "Difference")
dev.off()


tiff("Irish_Sea_Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()


writeRaster(resample_mod_rast, "original.tif", overwrite = T)
writeRaster(corr_rast, "bias_corrected_downscaled.tif", overwrite = T)
writeRaster(corr_rast, "interpolated_observations.tif", overwrite = T)

##########################################################################################

		#	VALIDATION		#

##########################################################################################


#Cross validation of QQ corrected raster vs original
#Create k folds of corrected values
#5 fold random corss validation as per Maraun et al. (2014) and Xavier et al. (2020)
#Xavier et al. (2020). Evaluation of Quantile Delta Mapping as a bias-correction method...
#Maraun et al. (2014). VALUE: A framework to validate downscaling approaches...


valid <- dat

k <- 5
rand <- sample(nrow(valid))
fold1 <- rand[rand %% k + 1 == 1]
fold2 <- rand[rand %% k + 1 == 2]
fold3 <- rand[rand %% k + 1 == 3]
fold4 <- rand[rand %% k + 1 == 4]
fold5 <- rand[rand %% k + 1 == 5]

folds <- list(fold1, fold2, fold3, fold4, fold5)
str(folds)

rmse_out <- c()
for(i in 1:k){
	fits <- fitQmap(valid$obs[-folds[[i]]], valid$mod[-folds[[i]]], method = "QUANT", qstep = q_step)
	corrections <- doQmap(valid$mod[-folds[[i]]], fits)
	rmse_out[i] <- sqrt(mean((valid$obs[-folds[[i]]] - corrections)^2, na.rm = T))
}

#RMSE for k fold CV for bias adjusted
corrected_rmse <- mean(round(rmse_out, 2))

#RMSE for original bio oracle model
original_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 2)


rmse_df <- data.frame(corrected_rmse, original_rmse)
rmse_df

write.csv(rmse_df, file = "RMSEs.CSV", row.names = F)


##########################################################################################

		#	TOTAL SUSPENDED SOLIDS 		#

##########################################################################################

setwd(files_dir)
out_df <- readRDS("surface_tss.RData")

setwd(paste(files_dir, "/surface_tss", sep = ""))

#Outlier removal. Measurements more than 5 SD's from the mean were considered outliers (Cheng and Zhu 2016).
outlier_filters <- c(median(out_df$tss) - (5 * mad(out_df$tss)), median(out_df$tss) + (5 * mad(out_df$tss)))
out_df <- out_df[between(out_df$tss, outlier_filters[1], outlier_filters[2]), ]
str(out_df)


#Crop data to area of interest and average data points within a certain cell size of each other
masked_pts <- terra::mask(vect(out_df), vect(area_of_interest))
cropped_pts <- terra::crop(masked_pts, area_of_interest)


#plot_pts <- data.frame(coordinate_data, response_data)
plot_coords <- geom(cropped_pts, df=TRUE)
plot_dat <- as.data.frame(cropped_pts)
plot_pts <- data.frame(plot_coords$x, plot_coords$y, plot_dat$tss, plot_dat$date_time)
names(plot_pts) <- c("lon", "lat", "tss", "date_time")
pts <- plot_pts[, 1:2]
names(pts) <- c("lon", "lat")
str(plot_pts)


interp_rast <- list()
files <- list.files("/Users/mark/UCD/Legacy_data_paper/R_objects/surface_tss/interp_rasts", full.names = TRUE)
for(i in 1:length(files)){
	interp_rast[[i]] <- rast(files[[i]])
}


obs_rast <- list()

obs_rast[[1]] <- mosaic(interp_rast[[12]], interp_rast[[1]], interp_rast[[2]], interp_rast[[3]], fun = "mean")
obs_rast[[2]] <- mosaic(interp_rast[[6]], interp_rast[[7]], interp_rast[[8]], interp_rast[[9]], fun = "mean")

names(obs_rast) <- c("Winter", "Summer")


#par(mfrow = c(1, 2))
#plot(obs_rast[[1]], main = paste(names(obs_rast)[1], "SPM"))
#plot(obs_rast[[2]], main = paste(names(obs_rast)[2], "SPM"))

##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################

setwd(paste(files_dir, "/surface_tss", sep = ""))
pts <- plot_pts[, 1:2]
vect_pts <- vect(pts)

#Extract model data
seasons <- c("winter", "summer")
season_spm <- list()
spm_mat <- matrix(0, nrow = nrow(pts), ncol = length(seasons))
for(j in 1:length(seasons)){
	nc_files <- list.files(paste("~/UCD/datasets/SPM_", seasons[j], sep = ""), full.names=T)
	nc_rast <- list()
	for(i in 1:length(nc_files)){
		nc_raster <- raster(nc_files[i])
		nc_rast_crop <- crop(nc_raster, area_of_interest)
		nc_rast_crop <- rast(nc_rast_crop)
		nc_rast[[i]] <- resample(nc_rast_crop, blankRaster)
		print(i)
		flush.console()
	}
	s <- sds(nc_rast)
	s_rast <- app(s, mean, na.rm = T)
#	s_rast <- resample(s_rast, blankRaster)	#
	s_rast <- mask(s_rast, area_of_interest)
	s_rast <- 61.875 * s_rast		 							#Particulate backscatter coeficient (bbp), conversion to TSS (g/m^2) @ 443
	season_spm[[j]] <- s_rast
#	season_spm[[j]] <- resample(s_rast, blankRaster)			#nm wavelength. From Jiang et al. (2023). 'Estimating the concentration of total
	spm <- extract(season_spm[[j]], vect_pts)					#suspended solids in inland and coastal waters from Sentinel-2 MSI: A semi-analytical
	spm_mat[, j] <- spm[, 2]									#approach'.
}
names(season_spm) <- seasons


#For loop for both seasons
par(mfrow = c(1, 2))
rmse_df <- data.frame(1:2, 1:2, 1:2)
names(rmse_df) <- c("adjusted", "original", "season")
spm_cdf <- list()
corr_rast <- list()
for(m in 1:length(seasons)){
	pts <- plot_pts[, 1:2]
	obs_dat <- extract(obs_rast[[m]], pts)
	obs_dat <- obs_dat[, 2]

	dat <- data.frame(obs_dat, spm_mat[, m])
	names(dat) <- c("obs", "mod")
	dat <- na.omit(dat)
	str(dat)
	
	valid <- dat

	k <- 5
	rand <- sample(nrow(valid))
	fold1 <- rand[rand %% k + 1 == 1]
	fold2 <- rand[rand %% k + 1 == 2]
	fold3 <- rand[rand %% k + 1 == 3]
	fold4 <- rand[rand %% k + 1 == 4]
	fold5 <- rand[rand %% k + 1 == 5]

	folds <- list(fold1, fold2, fold3, fold4, fold5)
	str(folds)

	rmse_out <- c()
	for(i in 1:k){
		fits <- fitQmap(valid$obs[-folds[[i]]], valid$mod[-folds[[i]]], method = "QUANT", qstep = q_step, wet.day = FALSE)
		corrections <- doQmap(valid$mod[-folds[[i]]], qmap_fit)
		rmse_out[i] <- sqrt(mean((valid$obs[-folds[[i]]] - corrections)^2, na.rm = T))
	}

	#RMSE for k fold CV for bias adjusted
	corr_rmse <- mean(round(rmse_out, 2))

	#RMSE for original bio oracle model
	orig_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 2)

	rmse_df[m, ] <- data.frame(corr_rmse, orig_rmse, seasons[m])


	#QQ correction of whole model raster using whole interpolated observation raster
	#Extract values from model and observation rasters
	obs_rast_resample <- resample(obs_rast[[m]], season_spm[[m]])
	obs_vals <- values(obs_rast_resample)
	mod_vals <- values(season_spm[[m]])
	pts <- as.data.frame(blankRaster, xy = T)
	dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
	names(dat_vals) <- c("lon", "lat", "obs", "mod")
	dat_vals <- na.omit(dat_vals)

	#Fit and apply QQ correction model to bio-oracle model data
	library('qmap')
	qmap_fit <- fitQmap(dat_vals$obs, dat_vals$mod, method = "QUANT", qstep = q_step, wet.day = FALSE)
	quant_correct <- doQmap(dat_vals$mod, qmap_fit)
	dat_correct <- data.frame(dat_vals, quant_correct)
#	str(dat_correct)
	
	corr_df <- dat_correct[, c(1, 2, 5)]
	corr_rast[[m]] <- rasterize(vect(corr_df), blankRaster, field = "quant_correct", fun = "mean")
	
	#Plot Cumulative Distribution Function of observed and modelled data
	spm_cdf[[m]] <- ggplot() + 
				stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_correct, geom = "point", size = 1.05, show.legend=T) +
				stat_ecdf(aes(x = quant_correct, colour = "Adjusted"), data = dat_correct, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
				stat_ecdf(aes(x = obs, colour = "Observations"), data = dat, geom = "point", size = 1.05, show.legend=T) +				
				xlab(expression("Suspended particulate matter (mg" *~l^-1*")")) + ylab("Cumulative distribution") +
				theme_bw(base_size=14) +
  				theme(legend.position = "bottom") +
  				theme(legend.title = element_blank()) +
		  		theme(legend.position="none") + 
  				guides(colour = guide_legend(override.aes = list(size = 3.5)))

	tiff(paste(seasons[m], "Original_CDF.tiff", sep = "_"), units="cm", width=15, height=15, res=200)
	plot(spm_cdf[[m]])
	dev.off()
	
	flush.console()
	print(m)

}

par(mfrow = c(1, 1))

rmse_df

write.csv(rmse_df, file = "RMSEs.csv", row.names = F)

##########################################################################################

		#	PLOT ADJUSTED MODEL DATA AND COMPARE TO ORIGINAL		#

##########################################################################################


#	Compare to BIO-ORACLE
pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))


for(m in 1:length(seasons)){
	r4 <- clamp(obs_rast[[m]], 0, 20) 
	r2 <- clamp(season_spm[[m]], 0, 20) 
	r1 <- clamp(corr_rast[[m]], 0, 20) 
	r3 <- clamp(corr_rast[[m]] - season_spm[[1]], 0, 20) 

	tiff(paste(seasons[m], "Comparisons.tiff", sep = "_"), units="cm", width=35, height=12, res=200)
	par(mfrow = c(1, 4))
	plot(r4, col = pal(1000), main = "Averaged monthly observations")
	plot(vect(plot_pts), add = T, pch = 20, col = "red", cex = 0.1)
	plot(r2, col = pal(1000), main = "Original model")
	plot(r1, col = pal(1000), main = "Bias adjusted")
	plot(r3, col = pal(1000), main = "Difference")
	dev.off()
	
	tiff(paste(seasons[m], "Irish_Sea_Comparisons.tiff", sep = "_"), units="cm", width=35, height=12, res=200)
	par(mfrow = c(1, 3))
	plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
	plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
	plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
	dev.off()

	writeRaster(season_spm[[m]], paste(seasons[m], "_original.tif", sep = ""), overwrite = T)
	writeRaster(corr_rast[[m]], paste(seasons[m], "_bias_corrected_downscaled.tif", sep = ""), overwrite = T)
}




##########################################################################################

		#	SALINITY		#

##########################################################################################

setwd(files_dir)
out_df <- readRDS("bottom_salinity_pangaea_and_MI.RData")
str(out_df)


setwd(paste(files_dir, "/bottom_salinity", sep = ""))


#Crop data to area of interest and average data points within a certain cell size of each other
masked_pts <- terra::mask(vect(out_df), vect(area_of_interest))
cropped_pts <- terra::crop(masked_pts, area_of_interest)

st_pts <- st_as_sf(cropped_pts)
response_data <- data.frame(st_pts$salinity, st_pts$date_time)
coordinate_data <- data.frame(st_coordinates(st_pts$geometry))

plot_pts <- data.frame(coordinate_data, response_data)
#plot_pts <- data.frame(out_df[, 1:2], out_df$salinity, out_df$date_time)
names(plot_pts) <- c("lon", "lat", "salinity", "date_time")
pts <- plot_pts[, 1:2]
names(pts) <- c("lon", "lat")
str(plot_pts)

par(mfrow = c(1, 1))

interp_rast <- list()
files <- list.files("/Users/mark/UCD/Legacy_data_paper/R_objects/bottom_salinity/interp_rasts", full.names = TRUE)
for(i in 1:length(files)){
	interp_rast[[i]] <- rast(files[[i]])
}

obs_rast <- mosaic(interp_rast[[1]], interp_rast[[2]], interp_rast[[3]],
		interp_rast[[4]], interp_rast[[5]], interp_rast[[6]],
		interp_rast[[7]], interp_rast[[8]], interp_rast[[9]],
		interp_rast[[10]], interp_rast[[11]], interp_rast[[12]],
		fun = "mean")

par(mfrow = c(1, 1))
plot(obs_rast)


##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################


#Is there systematic bias in the model (bio-oracle) data compared to observed (inerpolated) data for the original sampling points?
pts <- plot_pts
pts <- vect(pts)
obs_dat <- extract(obs_rast, pts)


#Extract model data
mod_rast <- out_rast$salinity
mod_rast <- mask(mod_rast, area_of_interest)
mod_rast <- crop(mod_rast, area_of_interest)
resample_mod_rast <- mod_rast
resample_mod_rast <- resample(mod_rast, blankRaster)
mod_dat <- extract(mod_rast, pts)

dat <- data.frame(obs_dat[, 2], mod_dat[, 2])
names(dat) <- c("obs", "mod")
dat <- na.omit(dat)
str(dat)



#QQ correction of whole model raster using whole interpolated observation raster
#Extract values from model and observation rasters

obs_resample <- resample(obs_rast, blankRaster)
obs_vals <- values(obs_resample)
mod_vals <- values(resample_mod_rast)
pts <- as.data.frame(blankRaster, xy = T)
dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
names(dat_vals) <- c("lon", "lat", "obs", "mod")
dat_vals <- na.omit(dat_vals)
str(dat_vals)

#Fit and apply QQ correction model to bio-oracle model data
library('qmap')
qmap_fit <- fitQmap(dat_vals$obs, dat_vals$mod, method = "QUANT", qstep = q_step, wet.day = FALSE)
quant_correct <- doQmap(dat_vals$mod, qmap_fit)
dat_correct <- data.frame(dat_vals, quant_correct)
str(dat_correct)


#Plot Cumulative Distribution Function of observed and modelled data
salinity_cdf <- ggplot() + 
			stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_correct, geom = "point", size = 1.05, show.legend=T) +
			stat_ecdf(aes(x = quant_correct, colour = "Adjusted"), data = dat_correct, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
			stat_ecdf(aes(x = obs, colour = "Observations"), data = dat, geom = "point", size = 1.05, show.legend=T) +				
			xlab("Salinity") + ylab("Cumulative distribution") +
			theme_bw(base_size=14) +
  			theme(legend.position = "bottom") +
  			theme(legend.title = element_blank()) +
		  	theme(legend.position="none") + 
  			guides(colour = guide_legend(override.aes = list(size = 3.5)))


tiff("Original_CDF.tiff", units="cm", width=15, height=15, res=200)
plot(salinity_cdf)
dev.off()



##########################################################################################

		#	PLOT ADJUSTED MODEL DATA AND COMPARE TO ORIGINAL		#

##########################################################################################


corr_df <- dat_correct[, c(1, 2, 5)]
corr_rast <- rasterize(vect(corr_df), blankRaster, field = "quant_correct", fun = "mean")
plot(corr_rast)



#	Compare to BIO-ORACLE

pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r4 <- clamp(obs_rast, 32.5, 35.25) 
r2 <- clamp(mod_rast, 32.5, 35.25) 
r1 <- clamp(corr_rast, 32.5, 35.25) 
r3 <- clamp(corr_rast - resample_mod_rast, -1, 1) 

tiff("Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 4))

plot(r4, col = pal(500), main = "Averaged monthly observations")
plot(vect(plot_pts), add = T, pch = 20, col = "red", cex = 0.1)
plot(r2, col = pal(500), main = "Original model")
plot(r1, col = pal(500), main = "Bias adjusted")
plot(r3, col = pal(100), main = "Difference")
dev.off()


tiff("Irish_Sea_Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()


writeRaster(mod_rast, "original.tif", overwrite = T)
writeRaster(corr_rast, "bias_corrected_downscaled.tif", overwrite = T)
writeRaster(corr_rast, "interpolated_observations.tif", overwrite = T)

##########################################################################################

		#	VALIDATION		#

##########################################################################################


#Cross validation of QQ corrected raster vs original
#Create k folds of corrected values
#5 fold random corss validation as per Maraun et al. (2014) and Xavier et al. (2020)
#Xavier et al. (2020). Evaluation of Quantile Delta Mapping as a bias-correction method...
#Maraun et al. (2014). VALUE: A framework to validate downscaling approaches...

valid <- dat

k <- 5
rand <- sample(nrow(valid))
fold1 <- rand[rand %% k + 1 == 1]
fold2 <- rand[rand %% k + 1 == 2]
fold3 <- rand[rand %% k + 1 == 3]
fold4 <- rand[rand %% k + 1 == 4]
fold5 <- rand[rand %% k + 1 == 5]

folds <- list(fold1, fold2, fold3, fold4, fold5)
str(folds)

rmse_out <- c()
for(i in 1:k){
	fits <- fitQmap(valid$obs[-folds[[i]]], valid$mod[-folds[[i]]], method = "QUANT", qstep = q_step)
	corrections <- doQmap(valid$mod[-folds[[i]]], fits)
	rmse_out[i] <- sqrt(mean((valid$obs[-folds[[i]]] - corrections)^2, na.rm = T))
}

#RMSE for k fold CV for bias adjusted
corrected_rmse <- mean(round(rmse_out, 2))

#RMSE for original bio oracle model
original_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 2)


rmse_df <- data.frame(corrected_rmse, original_rmse)
rmse_df

write.csv(rmse_df, file = "RMSEs.CSV", row.names = F)


##########################################################################################

		#	BOTTOM TEMPERATURE		#

##########################################################################################


setwd(files_dir)
out_df <- readRDS("bottom_temperature_pangaea_and_MI.RData")
str(out_df)

setwd(paste(files_dir, "/bottom_temp", sep = ""))

#Crop data to area of interest and average data points within a certain cell size of each other
masked_pts <- terra::mask(vect(out_df), vect(area_of_interest))
cropped_pts <- terra::crop(masked_pts, area_of_interest)

st_pts <- st_as_sf(cropped_pts)
response_data <- data.frame(st_pts$temperature, st_pts$date_time)
coordinate_data <- data.frame(st_coordinates(st_pts$geometry))

plot_pts <- data.frame(coordinate_data, response_data)
#plot_pts <- data.frame(out_df[, 1:2], out_df$temperature, out_df$date_time)
names(plot_pts) <- c("lon", "lat", "temperature", "date_time")
pts <- plot_pts[, 1:2]
names(pts) <- c("lon", "lat")
str(plot_pts)



interp_rast <- list()
files <- list.files("/Users/mark/UCD/Legacy_data_paper/R_objects/bottom_temp/interp_rasts", full.names = TRUE)
for(i in 1:length(files)){
	interp_rast[[i]] <- rast(files[[i]])
}


obs_rast <- mosaic(interp_rast[[1]], interp_rast[[2]], interp_rast[[3]],
		interp_rast[[4]], interp_rast[[5]], interp_rast[[6]],
		interp_rast[[7]], interp_rast[[8]], interp_rast[[9]],
		interp_rast[[10]], interp_rast[[11]], interp_rast[[12]],
		fun = "mean")

#par(mfrow = c(1, 1))
#plot(obs_rast)


##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################


#Is there systematic bias in the model (bio-oracle) data compared to observed (inerpolated) data for the original sampling points?
pts <- plot_pts
pts <- vect(pts)
obs_rast <- resample(obs_rast, blankRaster)
obs_dat <- extract(obs_rast, pts)


#Extract model data
mod_rast <- out_rast$temperature
mod_rast <- resample(mod_rast, blankRaster)
mod_dat <- extract(mod_rast, pts)

dat <- data.frame(obs_dat[, 2], mod_dat[, 2])
names(dat) <- c("obs", "mod")
dat <- na.omit(dat)
str(dat)


#QQ correction of whole model raster using whole interpolated observation raster
#Extract values from model and observation rasters
obs_vals <- values(obs_rast)
mod_vals <- values(mod_rast)
pts <- as.data.frame(blankRaster, xy = T)
dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
names(dat_vals) <- c("lon", "lat", "obs", "mod")
dat_vals <- na.omit(dat_vals)
str(dat_vals)

#Fit and apply QQ correction model to bio-oracle model data
library('qmap')
qmap_fit <- fitQmap(dat_vals$obs, dat_vals$mod, method = "QUANT", qstep = q_step, wet.day = FALSE)
quant_correct <- doQmap(dat_vals$mod, qmap_fit)
dat_correct <- data.frame(dat_vals, quant_correct)
str(dat_correct)


#Plot Cumulative Distribution Function of observed and modelled data
temp_cdf <- ggplot() + 
			stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_correct, geom = "point", size = 1.05, show.legend=T) +
			stat_ecdf(aes(x = quant_correct, colour = "Adjusted"), data = dat_correct, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
			stat_ecdf(aes(x = obs, colour = "Observations"), data = dat, geom = "point", size = 1.05, show.legend=T) +				
			xlab("Temperature (°C)") + ylab("Cumulative distribution") +
			theme_bw(base_size=14) +
  			theme(legend.position = "bottom") +
  			theme(legend.title = element_blank()) +
		  	theme(legend.position="none") + 
  			guides(colour = guide_legend(override.aes = list(size = 3.5)))

tiff("Original_CDF.tiff", units="cm", width=15, height=15, res=200)
plot(temp_cdf)
dev.off()


##########################################################################################

		#	PLOT ADJUSTED MODEL DATA AND COMPARE TO ORIGINAL		#

##########################################################################################

corr_df <- dat_correct[, c(1, 2, 5)]
corr_rast <- rasterize(vect(corr_df), blankRaster, field = "quant_correct", fun = "mean")
plot(corr_rast)



#	Compare to BIO-ORACLE

pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r4 <- clamp(obs_rast, 8.5, 12) 
r2 <- clamp(mod_rast, 8.5, 12)
r1 <- clamp(corr_rast, 8.5, 12) 
r3 <- clamp(corr_rast - mod_rast, -1, 1) 


tiff("Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 4))
plot(r4, col = pal(500), main = "Averaged monthly observations")
plot(vect(plot_pts), add = T, pch = 20, col = "red", cex = 0.1)
plot(r2, col = pal(500), main = "Original model")
plot(r1, col = pal(500), main = "Bias adjusted")
plot(r3, col = pal(100), main = "Difference")
dev.off()

tiff("Irish_Sea_Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()


writeRaster(mod_rast, "original.tif", overwrite = T)
writeRaster(corr_rast, "bias_corrected_downscaled.tif", overwrite = T)
writeRaster(corr_rast, "interpolated_observations.tif", overwrite = T)


##########################################################################################

		#	VALIDATION		#

##########################################################################################


#Cross validation of QQ corrected raster vs original
#Create k folds of corrected values
#5 fold random corss validation as per Maraun et al. (2014) and Xavier et al. (2020)
#Xavier et al. (2020). Evaluation of Quantile Delta Mapping as a bias-correction method...
#Maraun et al. (2014). VALUE: A framework to validate downscaling approaches...


valid <- dat

k <- 5
rand <- sample(nrow(valid))
fold1 <- rand[rand %% k + 1 == 1]
fold2 <- rand[rand %% k + 1 == 2]
fold3 <- rand[rand %% k + 1 == 3]
fold4 <- rand[rand %% k + 1 == 4]
fold5 <- rand[rand %% k + 1 == 5]

folds <- list(fold1, fold2, fold3, fold4, fold5)
str(folds)

rmse_out <- c()
for(i in 1:k){
	fits <- fitQmap(valid$obs[-folds[[i]]], valid$mod[-folds[[i]]], method = "QUANT", qstep = q_step)
	corrections <- doQmap(valid$mod[-folds[[i]]], fits)
	rmse_out[i] <- sqrt(mean((valid$obs[-folds[[i]]] - corrections)^2, na.rm = T))
}

#RMSE for k fold CV for bias adjusted
corrected_rmse <- mean(round(rmse_out, 2))

#RMSE for original bio oracle model
original_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 2)


rmse_df <- data.frame(corrected_rmse, original_rmse)
rmse_df

write.csv(rmse_df, file = "RMSEs.CSV", row.names = F)



##########################################################################################

		#	DRY BULK DENSITY	#

##########################################################################################

setwd(files_dir)
setwd(paste(files_dir, "/dry_bulk_density", sep = ""))

#create a grid
#blankRaster <- aoi_rast
#res(blankRaster) <- cell_size * 10
#blankRaster <- crop(blankRaster, area_of_interest)
#adding data into raster to avoid 'no data' error
#blankRaster[] <- 1:ncell(blankRaster)
#blankRaster

#Interpolate measurements
plot_pts <- readRDS('~/UCD/Legacy_data_paper/R_objects/DBD_data_search_raw_TEST.RData')
plot_pts$dbd <- as.numeric(plot_pts$dbd)
plot_pts <- plot_pts[plot_pts$depth <= 0.2, ]
plot_pts$dbd <- ifelse(plot_pts$dbd < 5, plot_pts$dbd * 1000, plot_pts$dbd)
plot_pts <- plot_pts[plot_pts$dbd>0, ]
plot_pts <- plot_pts[, 1:3]
str(plot_pts)
hist(plot_pts$dbd)
max(plot_pts$dbd)
min(plot_pts$dbd)



quest_dbd <- read.csv("~/UCD/datasets/CE2023016_DBD.csv", header = T, sep = ",")
quest_dbd <- na.omit(quest_dbd)
quest_dbd$dbd.g.cc <- quest_dbd$dbd.g.cc * 1000
names(quest_dbd) <- c(names(quest_dbd)[1:3], "dbd")
quest_dbd <- quest_dbd[, 2:4]
str(quest_dbd)

mosaic_dbd <- read.csv("~/UCD/datasets/DBD_MOSAIC.csv", header = T, sep=";")
mosaic_dbd$dry_bulk_density_g_cm3 <- mosaic_dbd$dry_bulk_density_g_cm3 * 1000
mos_dbd <- data.frame(mosaic_dbd$latitude, mosaic_dbd$longitude, mosaic_dbd$dry_bulk_density_g_cm3)
names(mos_dbd) <- c("lat", "lon", "dbd")
str(mos_dbd)

plot_pts <- data.frame(rbind(plot_pts, quest_dbd, mos_dbd))
str(plot_pts)


#Crop data to area of interest and average data points within a certain cell size of each other
masked_pts <- terra::mask(vect(plot_pts), vect(GB_and_I), inverse = T)
cropped_pts <- terra::crop(masked_pts, area_of_interest)

masked_pts <- terra::mask(cropped_pts, vect(area_of_interest))
cropped_pts <- terra::crop(masked_pts, area_of_interest)


st_pts <- st_as_sf(cropped_pts)
response_data <- data.frame(st_pts$dbd)
coordinate_data <- data.frame(st_coordinates(st_pts$geometry))

plot_pts <- data.frame(coordinate_data, response_data)
#plot_pts <- data.frame(out_df[, 1:2], out_df$temperature, out_df$date_time)
#names(plot_pts) <- c("lon", "lat", "temperature", "date_time")
pts <- plot_pts[, 1:2]
names(pts) <- c("lon", "lat")
names(plot_pts) <- c("lon", "lat", "dbd")
str(plot_pts)
plot_pts <- plot_pts[plot_pts$lon > 0 | plot_pts$lon < -2, ]


#Remove outliers
#plot_pts <- plot_pts[plot_pts$dbd>300, ]
min_filter <- mean(plot_pts$dbd) - (1*sd(plot_pts$dbd))
max_filter <- mean(plot_pts$dbd) + (5*sd(plot_pts$dbd))
plot_pts <- plot_pts[plot_pts$dbd>min_filter & plot_pts$dbd<max_filter, ]

hist(plot_pts$dbd)
min(plot_pts$dbd)



mon_df <- plot_pts[, 1:3]
obs_rast <- rasterize(x = vect(mon_df), y = blankRaster, field='dbd', fun=mean)
#obs_rast <- focal(rasterMeanPoints, w=9, fun=median, na.policy="only", na.rm=T)
out_df <- as.data.frame(obs_rast, xy = T)
names(out_df) <- c("lon", "lat", "dbd")
	

#Variation of IDW interpolation: K nearest neighbours interpolation
grid <- blankRaster
xy <- xyFromCell(grid, 1:ncell(grid))
coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"))

loc <- st_as_sf(x = out_df, coords = c("lon", "lat"))
gs <- gstat(formula = dbd ~ 1, locations = loc, nmax = 100, set = list(idp = 1))
nearest_pred <- predict(gs, coop)

#Rasterize interpolation output data frame object
nearest_pred$x <- st_coordinates(nearest_pred)[, 1]
nearest_pred$y <- st_coordinates(nearest_pred)[, 2]
nearest_pred$pred <- nearest_pred$var1.pred
interp_rast <- rasterize(nearest_pred, blankRaster, field = "pred", fun = "mean")
interp_rast <- mask(interp_rast, vect(area_of_interest))
interp_rast <- mask(interp_rast, vect(GB_and_I), inverse = T)
interp_rast <- crop(interp_rast, area_of_interest)
interp_rast <- resample(interp_rast, blankRaster)
writeRaster(interp_rast, "interp_rast.tif", overwrite = TRUE)
#plot(interp_rast)


obs_rast <- rast("/Users/mark/UCD/Legacy_data_paper/R_objects/dry_bulk_density/interp_rast.tif")
plot(obs_rast)


##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################


#Is there systematic bias in the model (bio-oracle) data compared to observed (inerpolated) data for the original sampling points?
pts <- plot_pts[, 1:2]
pts <- vect(pts)
obs_dat <- extract(obs_rast, pts)


#Extract model data
mod_rast <- rast("~/UCD/Legacy_data_paper/R_objects/porosity/bias_corrected_downscaled.tif")		#Prevoiusly bias adjusted porosity
mod_rast <- (1 - mod_rast) *2650		#Model DBD from porosity as per Diesing et al. 2017
mod_rast <- mask(mod_rast, area_of_interest)
mod_rast <- crop(mod_rast, area_of_interest)
mod_rast <- resample(mod_rast, blankRaster)
mod_dat <- extract(mod_rast, pts)

dat <- data.frame(obs_dat[, 2], mod_dat[, 2])
names(dat) <- c("obs", "mod")
dat <- na.omit(dat)
str(dat)


#Just look at spread of data
tiff("Data spread.tiff", units="cm", width=15, height=15, res=200)
ggplot(dat, aes(mod, obs)) + geom_point() +	
	xlab("Model dry bulk density (g cm-3)") + ylab("Observation dry bulk density (g cm-3)")
dev.off()


#QQ correction of whole model raster using whole interpolated observation raster
#Extract values from model and observation rasters
obs_vals <- values(obs_rast)
mod_vals <- values(mod_rast)
pts <- as.data.frame(blankRaster, xy = T)
dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
names(dat_vals) <- c("lon", "lat", "obs", "mod")
dat_vals <- na.omit(dat_vals)
str(dat_vals)

#Fit and apply QQ correction model to bio-oracle model data
library('qmap')
qmap_fit <- fitQmap(dat_vals$obs, dat_vals$mod, method = "QUANT", qstep = q_step, wet.day = FALSE)
quant_correct <- doQmap(dat_vals$mod, qmap_fit)
dat_correct <- data.frame(dat_vals, quant_correct)
str(dat_correct)


original_pdf <- ggplot() + 
			geom_density(aes(obs, fill = "Observations"), alpha = 0.2, data = dat) + 
			geom_density(aes(mod, fill = "Model"), alpha = 0.2, data = dat_correct) + 
			geom_density(aes(quant_correct, fill = "Adjusted"), alpha = 0.2, data = dat_correct) + 
			scale_fill_manual(name = "", values = c(Observations = "red", Model = "blue", Adjusted = "green")) +
			xlab(expression("Dry bulk density (g" *~cm^-3*")")) + ylab("Density") + theme(legend.position = "bottom")

tiff("Original_PDF.tiff", units="cm", width=15, height=15, res=200)
plot(original_pdf)
dev.off()

#Plot Cumulative Distribution Function of observed and modelled data
dbd_cdf <- ggplot() + 
			stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_correct, geom = "point", size = 1.05, show.legend=T) +
			stat_ecdf(aes(x = quant_correct, colour = "Adjusted"), data = dat_correct, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
			stat_ecdf(aes(x = obs, colour = "Observations"), data = dat, geom = "point", size = 1.05, show.legend=T) +				
			xlab(expression("Dry bulk density (g" *~cm^-3*")")) + ylab("Cumulative distribution") +
			theme_bw(base_size=14) +
  			theme(legend.position = "bottom") +
  			theme(legend.title = element_blank()) +
		  	theme(legend.position="none") + 
  			guides(colour = guide_legend(override.aes = list(size = 3.5)))

tiff("Original_CDF.tiff", units="cm", width=15, height=15, res=200)
plot(dbd_cdf)
dev.off()


##########################################################################################

		#	PLOT ADJUSTED MODEL DATA AND COMPARE TO ORIGINAL		#

##########################################################################################

corr_df <- dat_correct[, c(1, 2, 5)]
corr_rast <- rasterize(vect(corr_df), blankRaster, field = "quant_correct", fun = "mean")
plot(corr_rast)



#	Compare to BIO-ORACLE

pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r4 <- clamp(obs_rast, 700, 1500) 
r2 <- clamp(mod_rast, 700, 1500)
r1 <- clamp(corr_rast, 700, 1500) 
r3 <- clamp(abs(corr_rast - mod_rast), 0, 700) 


tiff("Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 4))
plot(r4, col = pal(500), main = "Interpolated observations")
plot(cropped_pts, add = T, pch = 20, col = "red", cex = 0.1)
plot(r2, col = pal(500), main = "Original model")
plot(r1, col = pal(500), main = "Bias adjusted")
plot(r3, col = pal(100), main = "Difference")
dev.off()

tiff("Irish_Sea_Comparisons.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()


writeRaster(mod_rast, "original.tif", overwrite = T)
writeRaster(corr_rast, "bias_corrected_downscaled.tif", overwrite = T)
writeRaster(corr_rast, "interpolated_observations.tif", overwrite = T)

##########################################################################################

		#	VALIDATION		#

##########################################################################################


#Cross validation of QQ corrected raster vs original
#Create k folds of corrected values
#5 fold random corss validation as per Maraun et al. (2014) and Xavier et al. (2020)
#Xavier et al. (2020). Evaluation of Quantile Delta Mapping as a bias-correction method...
#Maraun et al. (2014). VALUE: A framework to validate downscaling approaches...


valid <- dat

k <- 5
rand <- sample(nrow(valid))
fold1 <- rand[rand %% k + 1 == 1]
fold2 <- rand[rand %% k + 1 == 2]
fold3 <- rand[rand %% k + 1 == 3]
fold4 <- rand[rand %% k + 1 == 4]
fold5 <- rand[rand %% k + 1 == 5]

folds <- list(fold1, fold2, fold3, fold4, fold5)
str(folds)

rmse_out <- c()
for(i in 1:k){
	fits <- fitQmap(valid$obs[-folds[[i]]], valid$mod[-folds[[i]]], method = "QUANT", qstep = q_step)
	corrections <- doQmap(valid$mod[-folds[[i]]], fits)
	rmse_out[i] <- sqrt(mean((valid$obs[-folds[[i]]] - corrections)^2, na.rm = T))
}

#RMSE for k fold CV for bias adjusted
corrected_rmse <- mean(round(rmse_out, 2))

#RMSE for original bio oracle model
original_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 2)


rmse_df <- data.frame(corrected_rmse, original_rmse)
rmse_df

write.csv(rmse_df, file = "RMSEs.CSV", row.names = F)


##########################################################################################

		#	SEDIMENT PROPERTIES		#

##########################################################################################

#Load in point data
grabs <- readOGR(dsn = "~/UCD/datasets/SedimentSamples", layer = "Grab_Samples")
str(grabs)
grab_mud <- as.numeric(substr(grabs@data$F__MUD_, 1, str_length(grabs@data$F__MUD_)-1))
grab_sand <- as.numeric(substr(grabs@data$F__SAND_, 1, str_length(grabs@data$F__SAND_)-1))
grab_gravel <- as.numeric(substr(grabs@data$F__GRAVEL_, 1, str_length(grabs@data$F__GRAVEL_)-1))
grab_df <- data.frame(grabs@data$LONG, grabs@data$LAT, grab_mud/100, grab_sand/100, grab_gravel/100)
names(grab_df) <- c("lon", "lat", "mud", "sand", "gravel")
grab_df_sum <- grab_df$mud + grab_df$sand + grab_df$gravel
grab_df <- grab_df[grab_df_sum == 1, ]
str(grab_df)

mitch_dat <- read.csv("~/Downloads/Input data 2/GT_Samples_MEAN_ALR.csv", header = T, sep = ",")
mitch_dat <- data.frame(mitch_dat$Longitude, mitch_dat$Latitude, mitch_dat$Mud, mitch_dat$Sand, mitch_dat$Gravel)
names(mitch_dat) <- names(grab_df)
wils_dat <- read.csv("~/UCD/datasets/Wilson_2018/data_files_csv/sediment_properties.csv", header = T, sep = ",")
steph_dat <- read.csv("~/Downloads/train_and_test_data.csv", header = T, sep = ",")


plot(crop(vect(grab_df), area_of_interest), col = "red", cex = 0.01)
plot(irish_sea, add = T)
plot(crop(vect(grab_df), area_of_interest), add = T, col = "red", cex = 0.25)
plot(vect(mitch_dat, geom = c("lon", "lat")), cex = 0.25, add = T)
#plot(vect(wils_dat, geom = c("Longitude", "Latitude")), cex = 0.25, add = T)
#plot(vect(steph_dat, geom = c("Longitude", "Latitude")), cex = 0.25, add = T)

#plot_pts <- data.frame(coordinate_data, response_data)
plot_pts <- data.frame(rbind(grab_df, mitch_dat))
plot_pts <- na.omit(plot_pts)
names(plot_pts) <- c("lon", "lat", "mud", "sand", "gravel")
str(plot_pts)

df <- plot_pts
names(df) <- c("lon", "lat", "mud", "sand", "gravel")
str(df)

#Load in previous model data
setwd(files_dir)
setwd(paste(files_dir, "/sediment_properties", sep = ""))

#Mud, sand and gravel %
mitch_mud <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Mud_Fraction.tif")
mitch_mud <- resample(mitch_mud, blankRaster)
mitch_sand <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Sand_Fraction.tif")
mitch_sand <- resample(mitch_sand, blankRaster)

original_mud <- mitch_mud
original_sand <- mitch_sand 
original_gravel <- 1 - (original_mud + original_sand)



##################################################################################################

		# NORMALIZE FRACTIONS TO SUM EQUALS 1 #

##################################################################################################

#Mud, sand and gravel %
mitch_mud <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Mud_Fraction.tif")
mitch_mud <- resample(mitch_mud, blankRaster)
mitch_sand <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Sand_Fraction.tif")
mitch_sand <- resample(mitch_sand, blankRaster)
mitch_gravel <- 1 - (mitch_mud + mitch_sand)
mitchel <- list(mitch_mud, mitch_sand, mitch_gravel)


ras_list <- list()
file_list1 <- list("~/UCD/datasets/Stevens_2015/North_Sea_and_UK_shelf_substrate_composition_predictions_mud.tif",
				"~/UCD/datasets/Wilson_2018/data_files_asc/sediment_properties/MudPercent.asc")

file_list2 <- list("~/UCD/datasets/Stevens_2015/North_Sea_and_UK_shelf_substrate_composition_predictions_sand.tif",
				"~/UCD/datasets/Wilson_2018/data_files_asc/sediment_properties/SandPercent.asc")

file_list3 <- list("~/UCD/datasets/Stevens_2015/North_Sea_and_UK_shelf_substrate_composition_predictions_gravel.tif",
				"~/UCD/datasets/Wilson_2018/data_files_asc/sediment_properties/GravelPercent.asc")

outer_list <- list()
list_lists <- list(file_list1, file_list2, file_list3)
for(j in 1:length(list_lists)){
	for(i in 1:length(list_lists[[j]])){
		ras <- rast(list_lists[[j]][[i]])
		ras <- terra::project(ras, blankRaster)
		ras_list[[i]] <- resample(ras, blankRaster)
		print(i)
		flush.console()
	}
	names(ras_list) <- c("Stevens", "Wilson")
	var_list <- list(mitchel[[j]], ras_list[[1]], ras_list[[2]]/100)
	names(var_list) <- c("Mitchel", names(ras_list))
	outer_list[[j]] <- var_list
}
names(outer_list) <- c("mud", "sand", "gravel")



averaged_mud <- mosaic(outer_list$mud$Mitchel, outer_list$mud$Stevens, outer_list$mud$Wilson, fun="mean")
averaged_sand <- mosaic(outer_list$sand$Mitchel, outer_list$sand$Stevens, outer_list$sand$Wilson, fun="mean")
averaged_gravel <- mosaic(outer_list$gravel$Mitchel, outer_list$gravel$Stevens, outer_list$gravel$Wilson, fun="mean")

#Normalize averaged compositional data
mud <- averaged_mud/(averaged_mud + averaged_sand + averaged_gravel)
sand <- averaged_sand/(averaged_mud + averaged_sand + averaged_gravel)
gravel <- averaged_gravel/(averaged_mud + averaged_sand + averaged_gravel)

#	Compare to BIO-ORACLE

pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r2 <- clamp(crop(original_mud, irish_sea), 0, 1)
r1 <- clamp(crop(mud, irish_sea), 0, 1) 
r3 <- clamp(abs(crop(mud, irish_sea) - crop(original_mud, irish_sea)), 0, 1) 


tiff("Irish_Sea_Comparisons_mud.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()




pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r2 <- clamp(crop(original_sand, irish_sea), 0, 1)
r1 <- clamp(crop(sand, irish_sea), 0, 1) 
r3 <- clamp(abs(crop(sand, irish_sea) - crop(original_sand, irish_sea)), 0, 1) 


tiff("Irish_Sea_Comparisons_sand.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()




pal <- colorRampPalette(c("#440154FF","#443A83FF", "#2C728EFF", "#20A486FF", "#75D054FF","#FDE725FF"))
r2 <- clamp(crop(original_gravel, irish_sea), 0, 1)
r1 <- clamp(crop(gravel, irish_sea), 0, 1) 
r3 <- clamp(abs(crop(gravel, irish_sea) - crop(original_gravel, irish_sea)), 0, 1) 


tiff("Irish_Sea_Comparisons_gravel.tiff", units="cm", width=35, height=12, res=200)
par(mfrow = c(1, 3))
plot(crop(r2, irish_sea), col = pal(500), main = "Original model")
plot(crop(r1, irish_sea), col = pal(500), main = "Adjusted model")
plot(crop(r3, irish_sea), col = pal(500), main = "Difference")
dev.off()


##########################################################################################

		#	BIAS CORRECT AND DOWNSCALE MODEL		#

##########################################################################################

#Is there systematic bias in the model (bio-oracle) data compared to observed (inerpolated) data for the original sampling points

original_rasts <- list(original_mud, original_sand, original_gravel)
adjusted_rasts <- list(mud, sand, gravel)

rmse_df <- data.frame(1:3, 1:3, 1:3)
names(rmse_df) <- c("sediment", "adjusted", "original")
sed_vars <- grab_df[, 3:5]
sed_names <- c("Mud", "Sand", "Gravel")
extract_pts <- grab_df[, 1:2]
#pts <- vect(pts)

setwd("/Users/mark/UCD/Legacy_data_paper/R_objects/sediment_properties")
sediment_prop_cdf <- list()
for(i in 1:length(sed_names)){
	
	obs_dat <- sed_vars[i]
	obs_df <- data.frame(grab_df[, 1:2], obs_dat)

	#Extract model data
	mod_rast <- original_rasts[[i]]
	mod_rast <- resample(mod_rast, blankRaster)
	mod_dat <- extract(mod_rast, extract_pts)
		
	obs_rast <- adjusted_rasts[[i]]
	obs_rast <- resample(obs_rast, blankRaster)
	obs_dat <- extract(obs_rast, extract_pts)

	obs_df <- data.frame(obs_df, mod_dat[, 2], obs_dat[, 2])
	obs_df <- na.omit(obs_df)
	obs_df <- obs_df[obs_df$obs <= 1 & obs_df$obs >= 0, ]
	names(obs_df) <- c("lon", "lat", "obs", "mod", "adj")	


	#Extract values from model and observation rasters
	obs_vals <- values(obs_rast)
	mod_vals <- values(mod_rast)
	pts <- as.data.frame(blankRaster, xy = T)
	dat_vals <- data.frame(pts[, 1:2], obs_vals, mod_vals)
	names(dat_vals) <- c("lon", "lat", "adj", "mod")
	dat_vals <- dat_vals[dat_vals$mod <=1 & dat_vals$mod >= 0, ]
	dat_vals <- na.omit(dat_vals)
	str(dat_vals)

	
	#Plot Cumulative Distribution Function of observed and modelled data
	sediment_prop_cdf[[i]] <- ggplot() + 
					stat_ecdf(aes(x = mod, colour = "Modelled"), data = dat_vals, geom = "point", size = 1.05, show.legend=T) +
					stat_ecdf(aes(x = adj, colour = "Adjusted"), data = dat_vals, geom = "point", size = 1.05, alpha = 0.1, show.legend=T) +
					stat_ecdf(aes(x = obs, colour = "Observations"), data = obs_df, geom = "point", size = 1.05, show.legend=T) +				
					xlab(paste(sed_names[i], " content", sep="")) + ylab("Cumulative distribution") +
					coord_cartesian(xlim=c(0, 1.00)) + 
					theme_bw(base_size=14) +
  					theme(legend.position = "bottom") +
		  			theme(legend.title = element_blank()) +
		  			theme(legend.position="none") + 
  					guides(colour = guide_legend(override.aes = list(size = 3.5)))

	tiff(paste(names(adjusted_rasts)[[i]], "_CDF.tiff", sep=""), units="cm", width=15, height=15, res=200)
	print(sediment_prop_cdf[[i]])
	dev.off()


	##########################################################################################

					#	VALIDATION		#

	##########################################################################################

	valid <- obs_df

	k <- 5
	rand <- sample(nrow(valid))
	fold1 <- rand[rand %% k + 1 == 1]
	fold2 <- rand[rand %% k + 1 == 2]
	fold3 <- rand[rand %% k + 1 == 3]
	fold4 <- rand[rand %% k + 1 == 4]
	fold5 <- rand[rand %% k + 1 == 5]

	folds <- list(fold1, fold2, fold3, fold4, fold5)


	rmse_out <- c()
	for(j in 1:k){
		rmse_out[j] <- sqrt(mean((valid$obs[-folds[[j]]] - valid$adj[-folds[[j]]])^2, na.rm = T))
	}

	#RMSE for k fold CV for bias adjusted
	corrected_rmse <- mean(round(rmse_out, 4))

	#RMSE for original bio oracle model
	original_rmse <- round(sqrt(mean((valid$obs - valid$mod)^2)), 4)


	rmse_df[i, ] <- data.frame(sed_names[i], corrected_rmse, original_rmse)
	
	print(i)
	flush.console()
}

write.csv(rmse_df, file = "RMSEs.CSV", row.names = F)


##########################################################################################


library("ggpubr")

setwd("/Users/mark/UCD/Legacy_data_paper/R_objects")
tiff("CDF_plots.tiff", width=36, height=24, unit="cm", res=200)
ggarrange(chlorophyll_cdf, spm_cdf[[1]], spm_cdf[[2]],
		salinity_cdf, temp_cdf, dbd_cdf, sediment_prop_cdf[[1]], 
		sediment_prop_cdf[[2]], sediment_prop_cdf[[3]],
		ncol=3, nrow=3,
		labels=c("a.", "b.", "c.", "d.", "e.", "f.", "g.", "h.", "i."),
		common.legend=TRUE, legend="bottom")
dev.off()



##########################################################################################

		#	END!!!		#

##########################################################################################

