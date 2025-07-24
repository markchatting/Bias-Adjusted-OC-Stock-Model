set.seed(20)
library(future)
library(future.apply)


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

plot_res <- 300

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

cell_multiplier <- 1	#This is to try different resolutions. The original resolution is the same as EMODNet
 
cell_size <- c(0.00585, 0.010375) * cell_multiplier
 
aoi_rast <- rast(area_of_interest)
res(aoi_rast) <- cell_size

#create a grid
blankRaster <- aoi_rast
res(blankRaster) <- cell_size
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

#Pangaea data points
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

#QUEST data points
quest_dbd <- read.csv("~/UCD/datasets/CE2023016_DBD.csv", header = T, sep = ",")
quest_dbd <- na.omit(quest_dbd)
quest_dbd$dbd.g.cc <- quest_dbd$dbd.g.cc * 1000
names(quest_dbd) <- c(names(quest_dbd)[1:3], "dbd")
quest_dbd <- quest_dbd[, 2:4]
str(quest_dbd)

plot(vect(GB_and_I))
plot(vect(quest_dbd, geom = c("lon", "lat"), crs = "EPSG:4326"), add = T, col = "red")

#MOSAIC data points
#mosaic_dbd <- read.csv("~/UCD/datasets/DBD_MOSAIC.csv", header = T, sep=";")
#mosaic_dbd$dry_bulk_density_g_cm3 <- mosaic_dbd$dry_bulk_density_g_cm3 * 1000
#mos_dbd <- data.frame(mosaic_dbd$latitude, mosaic_dbd$longitude, mosaic_dbd$dry_bulk_density_g_cm3)
#names(mos_dbd) <- c("lat", "lon", "dbd")
#str(mos_dbd)

#MOSAIC data points
mosaic_dbd <- read.csv("~/UCD/datasets/DBD_MOSAIC.csv", header = T, sep=";")
mosaic_dbd$dry_bulk_density_g_cm3 <- mosaic_dbd$dry_bulk_density_g_cm3 * 1000
mos_dbd <- data.frame(mosaic_dbd$latitude, mosaic_dbd$longitude, mosaic_dbd$dry_bulk_density_g_cm3)
names(mos_dbd) <- c("lat", "lon", "dbd")
str(mos_dbd)

#mos_dbd <- read.csv("~/UCD/datasets/DBD_MOSAIC_CUT_POINTS.csv", header = T, sep=",")
#names(mos_dbd) <- c("lon", "lat", "dbd")
#str(mos_dbd)

plot(vect(GB_and_I))
plot(vect(mos_dbd, geom = c("lon", "lat"), crs = "EPSG:4326"), col = "red", add = T)


#Billy data points
billy <- read.csv("~/UCD/datasets/billy4/NI_Sed_OC_Mapping_Zenodo.csv", header = T, sep=",")
billy$Metier <- as.factor(billy$Metier)
billy_df <- billy[billy$Metier == "Corer", ]
billy_dbd <- data.frame(billy_df$Latitude, billy_df$Longitude, billy_df$Dry_Bulk_Density)
names(billy_dbd) <- c("lat", "lon", "dbd")
str(billy_dbd)

plot_pts <- data.frame(rbind(plot_pts, quest_dbd, mos_dbd, billy_dbd))
plot_pts <- na.omit(plot_pts)
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
#plot_pts <- plot_pts[plot_pts$lon > 0 | plot_pts$lon < -2, ]


#Remove outliers
#plot_pts <- plot_pts[plot_pts$dbd>300, ]
min_filter <- mean(plot_pts$dbd) - (1*sd(plot_pts$dbd))
#min_filter <- 110
max_filter <- mean(plot_pts$dbd) + (5*sd(plot_pts$dbd))
test <- plot_pts[plot_pts$dbd < min_filter, ]
plot_pts <- plot_pts[plot_pts$dbd>min_filter & plot_pts$dbd<max_filter, ]

plot(vect(GB_and_I))
plot(vect(plot_pts, geom = c("lon", "lat")), add = T, col = "blue")
plot(vect(test, geom = c("lon", "lat")), add = T, col = "red")

hist(plot_pts$dbd)
range(plot_pts$dbd)


mon_df <- plot_pts[, 1:3]
obs_rast <- rasterize(x = vect(mon_df), y = blankRaster, field='dbd', fun=mean)
#obs_rast <- focal(obs_rast, w=9, fun=median, na.policy="only", na.rm=T)
out_df <- as.data.frame(obs_rast, xy = T)
names(out_df) <- c("lon", "lat", "dbd")
plot(obs_rast)

##########################################################################################



#Mud, sand and gravel %
if (FALSE){
mitch_mud <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Mud_Fraction.tif")
mitch_mud <- resample(mitch_mud, blankRaster)
mitch_sand <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Sand_Fraction.tif")
mitch_sand <- resample(mitch_sand, blankRaster)
mitchel <- list(mitch_mud, mitch_sand)


mitch_mud <- mitch_mud
mitch_sand <- mitch_sand
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
}


#Commented out these sediment property layers
#Mud, sand and gravel %
#if (FALSE) {
mud <- rast("~/UCD/Legacy_data_paper/R_objects/sediment_properties/bias_adjusted_mud.tif")
mud <- resample(mud, blankRaster)
mud <- mask(mud, area_of_interest)
mud <- crop(mud, area_of_interest)

sand <- rast("~/UCD/Legacy_data_paper/R_objects/sediment_properties/bias_adjusted_sand.tif")
sand <- resample(sand, blankRaster)
sand <- mask(sand, area_of_interest)
sand <- crop(sand, area_of_interest)

gravel <- rast("~/UCD/Legacy_data_paper/R_objects/sediment_properties/bias_adjusted_gravel.tif")
gravel <- resample(gravel, blankRaster)
gravel <- mask(gravel, area_of_interest)
gravel <- crop(gravel, area_of_interest)
#}




#Not bespoke bot temp, salinity, mean current, max current, chlorophyll a and suspended particulate matter
raster_list <- list("~/UCD/Legacy_data_paper/R_objects/bottom_temp/original.tif",
			"~/UCD/Legacy_data_paper/R_objects/bottom_salinity/original.tif",
			"~/UCD/Legacy_data_paper/R_objects/surface_chl/original.tif",
			"~/UCD/Legacy_data_paper/R_objects/surface_tss/summer_original.tif",
			"~/UCD/Legacy_data_paper/R_objects/surface_tss/winter_original.tif",
			"~/UCD/Legacy_data_paper/R_objects/bottom_current_velocity/mean_original.tif",
			"~/UCD/Legacy_data_paper/R_objects/bottom_current_velocity/max_original.tif")
			
original_ras <- list()
for(i in 1:length(raster_list)){
	raster_file <- raster_list[[i]]
	ras <- rast(raster_file)
	ras <- resample(ras, blankRaster)
	ras <- mask(ras, vect(area_of_interest))
	original_ras[[i]] <- crop(ras, area_of_interest)
	print(i)
	flush.console()
}
names(original_ras) <- c("temp", "salinity", "chl", "summer_spm", "winter_spm", "mean_current", "max_current")



#Orbital wave velocity at the seabed
out_ras <- list()
file_list <- list("~/UCD/datasets/Wilson_2018/data_files_asc/sediment_properties/OrbitalVelMean.asc",
				"~/UCD/datasets/Wilson_2018/data_files_asc/sediment_properties/OrbitalVelMax.asc")
for(i in 1:length(file_list)){
	raster_file <- file_list[[i]]
	ras <- rast(raster_file)
	ras <- resample(ras, blankRaster)
	ras <- mask(ras, vect(area_of_interest))
	out_ras[[i]] <- crop(ras, area_of_interest)
#	plot(out_ras[[i]])
	print(i)
	flush.console()
}
names(out_ras) <- c("OrbitalVelMean", "OrbitalVelMax")
out_ras

##########################################################################################

pred_ras <- c(obs_rast, original_ras$salinity, original_ras$temp, original_ras$max_current, original_ras$mean_current, original_ras$chl, original_ras$winter_spm, original_ras$summer_spm, mud, sand, gravel, out_ras$OrbitalVelMax, out_ras$OrbitalVelMean)
names(pred_ras) <- c("response", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "OrbitalVelMax", "OrbitalVelMean")
pred_vars <- extract(pred_ras, vect(plot_pts))
#pred_vars <- na.omit(pred_vars[, 2:5])
str(pred_vars)

infer_ras <- c(original_ras$salinity, original_ras$temp, original_ras$max_current, original_ras$mean_current, original_ras$chl, original_ras$winter_spm, original_ras$summer_spm, mud, sand, gravel, out_ras$OrbitalVelMax, out_ras$OrbitalVelMean)
names(infer_ras) <- c("salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "OrbitalVelMax", "OrbitalVelMean")

#Training parameters
trees <- 500
folds <- 10
n_preds <- 2

df <- data.frame(plot_pts[, 1:2], pred_vars)
response <- df$response
train_df <- na.omit(df)
str(train_df)

#Convert train_df dataframe to sf object
my_sf <- st_as_sf(train_df, coords=c("lon", "lat"))
st_crs(my_sf) <- "+proj=longlat +datum=WGS84"


#Nearest Neighbour Distance Matching 
aoi_crs <- 3040
nndm_AOI <- area_of_interest
nndm_AOI <- st_transform(area_of_interest, crs = st_crs(aoi_crs))
trans_sf <- st_transform(my_sf, crs = st_crs(aoi_crs))

knndm_folds <- CAST::knndm(trans_sf, nndm_AOI, samplesize=round(nrow(trans_sf) * 0.8, 0))

tiff("kNNDM_distance_functions.tiff", units="cm", width=15, height=15, res=plot_res)    
plot(knndm_folds, type="simple")
dev.off()

#Vizualise spatial folds
C <- train_df
coordinates(C) <- c("lon", "lat")
B <- SpatialPoints(spat_group)
D <- SpatialPoints(C)


tiff("Spatial_folds.tiff", units="cm", width=35, height=35, res=plot_res)
par(mfrow = c(2, 5))
par(mar = c(0, 1, 0, 1))
for(i in 1:length(knndm_folds)){
#	plot(PP)
#	plot(B, cex = 1, pch = 0)
	plot(C[knndm_folds$indx_train[[i]], ], pch=20, col="black")
	plot(C[knndm_folds$indx_test[[i]], ], pch=20, col="red", add=T)
	plot(GB_and_I, add = T)
	print(i)
}
dev.off()

train_pts <- train_df[, 1:2]
train_df <- train_df[, -c(1, 2, 3)]
str(train_df)



########################################################################################

			#	DBD MODEL TRAINING	#

########################################################################################

#Forward feature selection and spatial cross validation without Euclidean Distances
train_df_i <- data.frame(train_df[, 2:13])
response <- train_df[, 1]
mtry <- 2
dbd_mod <- ffs(train_df_i, response, metric="RMSE",
				method="rf", tuneGrid=data.frame("mtry"=mtry),
				importance=TRUE, ntree=trees,
				trControl=trainControl(method="cv", index=knndm_folds$indx_train, indexOut=knndm_folds$indx_test))



# Define selected predictor names
if (FALSE) {
selected_vars <- c("spm_winter", "spm_summer", "OrbitalVelMax", 
                   "OrbitalVelMean", "gravel_percent", "mud_percent")

# Subset training predictors to only the selected ones
train_df_selected <- train_df_i[, selected_vars]

# Train the Random Forest model
dbd_mod <- train(
  x = train_df_selected, 
  y = response,
  method = "rf",
  metric = "RMSE",
  tuneGrid = data.frame(mtry = mtry),
  importance = TRUE,
  ntree = trees,
  trControl = trainControl(
    method = "cv",
    index = knndm_folds$indx_train,
    indexOut = knndm_folds$indx_test
  )
)
}

dbd_mod


tiff("Variable_importance_nndm.tiff", units="cm", width=12, height=12, res=plot_res)
print(plot(varImp(dbd_mod, scale = F), xlab = "% Increase in MSE"))
dev.off()

tiff("FFS_variable_runs_nndm.tiff", units="cm", width=18, height=12, res=plot_res)
print(plot(dbd_mod))
dev.off()

varImp(dbd_mod, scale = F)
var_imp <- importance(dbd_mod$finalModel)
var_imp[order(-var_imp[, "%IncMSE"]), ]





########################################################################################

			#	PARALLEL PROCESSING SET SEARCH	#

########################################################################################

if (FALSE){

library(doParallel)
library(foreach)
library(CAST)
library(caret)
library(digest)

# Set up parallel backend using all but one core
n_cores <- parallel::detectCores() - 1
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Target predictor fingerprint
target_vars <- c("spm_winter", "spm_summer", "OrbitalVelMax", 
                 "OrbitalVelMean", "gravel_percent", "mud_percent")
target_hash <- digest(paste(sort(target_vars), collapse = ","), algo = "sha256")

# Seeds to test
seeds <- 1:1000

# Loop over seeds in parallel
results <- foreach(seed = seeds, .packages = c("caret", "CAST", "digest")) %dopar% {
  set.seed(seed)

  train_df_i <- data.frame(train_df[, 2:13])
  response <- train_df[, 1]
  mtry <- 2

  model <- tryCatch({
    train(
      x = train_df_i,
      y = response,
      method = "rf",
      metric = "RMSE",
      tuneGrid = data.frame(mtry = mtry),
      importance = TRUE,
      ntree = trees,
      trControl = trainControl(
        method = "cv",
        index = knndm_folds$indx_train,
        indexOut = knndm_folds$indx_test
      )
    )
  }, error = function(e) NULL)

  if (!is.null(model)) {
    pred_names <- model$finalModel$xNames
    pred_hash <- digest(paste(sort(pred_names), collapse = ","), algo = "sha256")
    if (identical(pred_hash, target_hash)) {
      return(list(seed = seed, model = model))
    }
  }

  return(NULL)
}

# Stop parallel cluster
stopCluster(cl)

# Filter successful matches
matching_result <- Filter(Negate(is.null), results)

# Save and report
if (length(matching_result) > 0) {
  selected_model <- matching_result[[1]]$model
  matched_seed <- matching_result[[1]]$seed
  message("🎯 Final match: Seed = ", matched_seed)
  saveRDS(selected_model, file = paste0("selected_model_seed_", matched_seed, ".rds"))
} else {
  message("❌ No match found in the tested seed range.")
}

}


########################################################################################

		#		PARTIAL PLOTS		#
		
########################################################################################

library("gridExtra")

selected_model <- dbd_mod
selected_model
predictors <- selected_model$finalModel$xNames
predictor_labels <- c(
					expression("S"[bot]),
					expression("T"[bot]* "("*degree*"C)"),
					expression(italic(U)[plain("bot,max")] * " (m s"^{-1} * ")"),
					expression(italic(U)[plain("bot,mean")] * " (m s"^{-1} * ")"),
					expression("Surface chlorophyll-a" * " (" * mu * "g l"^{-1} * ")"),
					expression("SPM"[winter]* " (mg l"^{-1} * ")"),
					expression("SPM"[summer]* " (mg l"^{-1} * ")"),
					expression("Mud"[cont]* " (%)"),
					expression("Sand"[cont]* " (%)"),
					expression("Gravel"[cont]* " (%)"),
					expression(italic(u)[plain("orb,max")] * " (m s"^{-1} * ")"),
					expression(italic(u)[plain("orb,mean")] * " (m s"^{-1} * ")")
					)

# Define two string vectors
vector1 <- names(pred_vars[, 3:length(names(pred_vars))])
vector2 <- predictor_labels[1:length(vector1)]

# Create a named vector (dictionary) where vector1 elements are keys, and vector2 elements are values
dictionary <- setNames(vector2, vector1)

# Define a function that takes an element from vector1 and prints the corresponding element from the dictionary
print_equivalent <- function(item) {
  # Check if the item exists in the dictionary
  if (item %in% names(dictionary)) {
    # Print the corresponding value from the dictionary
    return(dictionary[[item]])
  } else {
    return("Item not found in dictionary")
  }
}



# Usage example
print_equivalent(predictors[i])   # Output: "red"

					
predicted_response <- selected_model$finalModel$predicted
#nRow <- floor(sqrt(length(predictors)))
nRow <- 1


library(gridExtra)
library(grid)
library(ggplot2)
library(pdp)

# Step 1: Reorder predictors by variable importance
ordered_vars <- rownames(var_imp[order(-var_imp[, "%IncMSE"]), ])
predictors_ordered <- intersect(ordered_vars, predictors)  # in case of mismatch

# Step 2: Re-label plots alphabetically
pdplot <- list()
letters_labels <- letters[1:length(predictors_ordered)]

# Step 3: Generate partial dependence plots in new order
for (i in seq_along(predictors_ordered)) {
  pd <- partial(selected_model, pred.var = predictors_ordered[i], progress = "none")

  p <- ggplot(pd, aes_string(x = predictors_ordered[i], y = "yhat")) +
    geom_line(color = "black", linewidth = 0.6) +
    labs(
      x = print_equivalent(predictors_ordered[i]),
      y = expression("DBD (kg " * m^{-3} * ")")
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 5),
      axis.title.y = element_text(size = 5),
      axis.text.x = element_text(size = 3.5),
      axis.text.y = element_text(size = 3.5),
      plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm")
    )

  labeled_plot <- arrangeGrob(
    p,
    top = textGrob(
      paste0("(", letters_labels[i], ")"),
      x = unit(0.01, "npc"),
      y = unit(0.99, "npc"),
      just = c("left", "top"),
      gp = gpar(fontface = "bold", fontsize = 8)
    )
  )

  pdplot[[i]] <- labeled_plot
}

# Step 4: Save the plots (1 row)
pd_width <- 3 + (length(predictors_ordered) * 2)
tiff("DBD_partial_plots_nndm_ONE_ROW.tiff", units = "cm", width = (pd_width+8), height = 6, res = 300)
grid.arrange(grobs = pdplot, nrow = 1)
dev.off()


# Step 4: Save the plots (1 row)
pd_width <- 3 + (length(predictors_ordered) * 2)
tiff("DBD_partial_plots_nndm.tiff", units = "cm", width = 11, height = 8, res = 300)
grid.arrange(grobs = pdplot, nrow = 2)
dev.off()





##########################################################################################

pred_ras <- terra::predict(infer_ras, selected_model, se.fit = TRUE, na.rm = T)
masked_rast <- terra::mask(pred_ras, vect(area_of_interest))
cropped_rast <- terra::crop(masked_rast, area_of_interest)
plot(cropped_rast)

test <- mask(cropped_rast, irish_sea)
test <- crop(test, irish_sea)
plot(test)

writeRaster(cropped_rast, "DBD_RF_prediction.tif", overwrite=TRUE)


########################################################################################

		#		AREA OF APPLICABILITY AOA		#
		
########################################################################################

#Load previously created objects
infer_df <- as.data.frame(infer_ras)
#infer_pts <- readRDS("infer_pts.RData")

train_DI <- trainDI(model = selected_model, train = train_df, variables = selected_model$finalModel$xNames)
AOA <- aoa(infer_ras, model = selected_model, trainDI = train_DI, variables = selected_model$finalModel$xNames, CVtest = indices$index)

tiff("AOA.tiff", units="cm", width=14, height=14, res=200)
plot(AOA)
dev.off()

outsideAOA <- AOA$DI > AOA$parameters$threshold
plot(outsideAOA)

writeRaster(outsideAOA, "dbd_aoa_ras.tif", overwrite=T)

tiff("AOA.tiff", units="cm", width=14, height=14, res=200)
plot(outsideAOA)
dev.off()


# Ensure CRS match and erase coastal areas
coast <- read_sf("~/UCD/shapefiles/World_Internal_Waters_v4_20231025/eez_internal_waters_v4.shp")
coast <- crop(vect(coast), nwe_shelf)
coast <- st_as_sf(coast)


irish_sea2 <- rmapshaper::ms_erase(irish_sea, coast)
area_of_interest2 <- st_transform(irish_sea2, crs(outsideAOA))

# Crop and mask the raster to the area of interest
raster_in_aoi <- mask(crop(outsideAOA, vect(area_of_interest2)), vect(area_of_interest2))
plot(raster_in_aoi)

# Step 1: Convert raster to data frame
r_points <- as.data.frame(raster_in_aoi, xy = TRUE)
colnames(r_points)[3] <- "outside_AOA"

# Step 2: Convert TRUE/FALSE to factor 0/1
r_points$AOA <- ifelse(r_points$outside_AOA, 1, 0)

# Step 3: Plot using ggplot
tiff("AOA_threshold.tiff", units = "cm", width = 14, height = 14, res = 200)

print(
  ggplot() +
    geom_tile(data = r_points, aes(x = x, y = y, fill = as.factor(AOA))) +
#    geom_point(data = train_pts, aes(x = lon, y = lat), size = 0.5) +
    geom_sf(data = GB_and_I, fill = "#dadada", inherit.aes = FALSE) +
    scale_fill_manual(
      values = c("0" = "#66c2a5", "1" = "#fc8d62"),
      labels = c("Inside AOA", "Outside AOA"),
      name = ""
    ) +
    coord_sf(xlim = range(r_points$x), ylim = range(r_points$y)) +
    theme_bw() +
    theme(
      legend.position = "top",
      axis.title = element_blank(),
      axis.line = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.margin = unit(c(0.25, -0.15, 0, -0.15), "cm")
    )
)

dev.off()



# Use terra::freq without 'useNA' argument
freq_table <- freq(raster_in_aoi)

# Extract FALSE values (FALSE = 0 in logical rasters)
false_count <- freq_table[freq_table$value == 0, "count"]
total_count <- sum(freq_table$count, na.rm = TRUE)

# Calculate percentage
percent_false <- (false_count / total_count) * 100

# Output
cat("Percentage of FALSE values within area_of_interest:", percent_false, "%\n")


########################################################################################


predictors <- selected_model$finalModel$xNames
n_predictors <- length(predictors)
uncert_dat <- train_df[, predictors[1:n_predictors]]
uncert_response <- train_df$response
uncert_dat <- data.frame(uncert_response, uncert_dat)
str(uncert_dat)

#Monte Carlo uncertainty estimation as per Diesing et al. (2021)
uncert_vals <- list()
for(i in 1:25){
	dt <- sort(sample(nrow(uncert_dat), nrow(uncert_dat)*0.7))
	train <- uncert_dat[dt, 1:ncol(uncert_dat)-1]
	test <- uncert_dat[-dt, 1:ncol(uncert_dat)-1]
	uncert_mod <- train(uncert_response ~ ., data = train,
				method="rf", tuneGrid=data.frame("mtry"=mtry),
				importance=TRUE, ntree=trees,
				trControl=trainControl(method="cv"))
	uncert_ras <- terra::predict(infer_ras, uncert_mod, na.rm = T)
	uncert_ras <- uncert_ras
	uncert_sf <- as.points(uncert_ras)
	uncert_sf <- st_as_sf(uncert_sf)
	uncert_vals[[i]] <- uncert_sf$lyr1
	print(i)
	flush.console()
}

uncert_mat <- data.frame(do.call('cbind', uncert_vals))
uncert_pts <- st_coordinates(uncert_sf$geometry)
uncert_df <- data.frame(uncert_pts, uncert_mat)
uncert <- apply(uncert_df[, 3:27], 1, sd)
uncert_df <- data.frame(uncert_pts, uncert)
names(uncert_df) <- c("lon", "lat", "uncert")
uncertainty_ras <- rasterize(x=vect(uncert_df), y=blankRaster, field="uncert", fun=mean)

writeRaster(uncert_ras, "dbd_RF_uncertainty_ras.tif", overwrite=T)




##########################################################################################

#Sediment proportions in the study area
mud_crop <- mud %>%
			resample(blankRaster) %>%
			mask(area_of_interest2) %>%
			crop(area_of_interest2)
			

sand_crop <- sand %>%
			resample(blankRaster) %>%
			mask(area_of_interest2) %>%
			crop(area_of_interest2)
			
gravel_crop <- gravel %>%
			resample(blankRaster) %>%
			mask(area_of_interest2) %>%
			crop(area_of_interest2)


mud_crop
sand_crop
gravel_crop




# -------------------------------------------
# 💡 Define custom hex colours for each component. Colour paletter from here: https://colorhunt.co/palette/6de1d2ffd63affa955f75a5a
mud_hex    <- "#6DE1D2"  # Turquoise
sand_hex   <- "#FFD63A"  # Yellow
gravel_hex <- "#F75A5A"  # Red
# You can change these to any hex you want!
# -------------------------------------------

# Convert hex to RGB (scaled 0–255)
mud_rgb    <- col2rgb(mud_hex)
sand_rgb   <- col2rgb(sand_hex)
gravel_rgb <- col2rgb(gravel_hex)

# Stack and normalize
sed_stack <- c(mud_crop, sand_crop, gravel_crop)
names(sed_stack) <- c("mud", "sand", "gravel")
sed_stack <- sed_stack / sum(sed_stack)

# Convert to data frame
sed_df <- as.data.frame(sed_stack, xy = TRUE, na.rm = TRUE)

# Calculate RGB blend from hex colours
sed_df$r <- with(sed_df, mud * mud_rgb[1,] + sand * sand_rgb[1,] + gravel * gravel_rgb[1,])
sed_df$g <- with(sed_df, mud * mud_rgb[2,] + sand * sand_rgb[2,] + gravel * gravel_rgb[2,])
sed_df$b <- with(sed_df, mud * mud_rgb[3,] + sand * sand_rgb[3,] + gravel * gravel_rgb[3,])

# Convert to hex RGB string
sed_df$rgb <- rgb(sed_df$r, sed_df$g, sed_df$b, maxColorValue = 255)

# Plot
sed_comp <- ggplot(sed_df) +
  	geom_tile(aes(x = x, y = y, fill = rgb)) +
  	scale_fill_identity() +
	  coord_equal() +
	  labs(title = "Sediment Composition (Mud/Sand/Gravel)") +
 	 geom_sf(data = GB_and_I, fill = "#dadada", inherit.aes = FALSE) +
  	coord_sf(xlim = range(sed_df$x), ylim = range(sed_df$y)) +
  	theme_bw(base_size = 12) +
  	theme(
	    legend.position = "bottom",
    	legend.key.width = unit(0.15, "cm"),
	    axis.title = element_blank(),
    	axis.line = element_blank(),
 	    panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
	    panel.border = element_blank(),
    	panel.background = element_blank(),
	    plot.margin = unit(c(0.25, -0.15, -0.05, -0.15), "cm")
  )

tiff("sediment_composition.tiff", units = "cm", width = 14, height = 14, res = 200)
sed_comp
dev.off()



# Step 1: Stack and normalize
sed_stack <- c(mud_crop, sand_crop, gravel_crop)
names(sed_stack) <- c("mud", "sand", "gravel")
sed_stack <- sed_stack / sum(sed_stack)  # ensures proportions sum to ~1

# Step 2: Convert to data frame (mask NAs)
sed_df <- as.data.frame(sed_stack, xy = FALSE, na.rm = TRUE)

# Step 3: Find dominant component
dominant <- apply(sed_df, 1, function(x) c("mud", "sand", "gravel")[which.max(x)])

# Step 4: Tabulate proportions
total_cells <- length(dominant)
dominant_table <- table(dominant)
dominant_proportions <- round(100 * dominant_table / total_cells, 1)

# Step 5: Print result
print(dominant_proportions)


library(terra)
library(ggplot2)
library(ggtern)
library(patchwork)
library(sf)

# -------------------------------------------
# 💡 Custom hex colours (Color palette: https://colorhunt.co/palette/6de1d2ffd63affa955f75a5a)
mud_hex    <- "#6DE1D2"  # Turquoise
sand_hex   <- "#FFD63A"  # Yellow
gravel_hex <- "#F75A5A"  # Red
# -------------------------------------------

# Convert hex to RGB
mud_rgb    <- col2rgb(mud_hex)
sand_rgb   <- col2rgb(sand_hex)
gravel_rgb <- col2rgb(gravel_hex)

# Stack and normalize sediment rasters
sed_stack <- c(mud_crop, sand_crop, gravel_crop)
names(sed_stack) <- c("mud", "sand", "gravel")
sed_stack <- sed_stack / sum(sed_stack)  # ensures mud + sand + gravel ≈ 1

# Convert to data frame
sed_df <- as.data.frame(sed_stack, xy = TRUE, na.rm = TRUE)

# Create RGB blend based on custom colours
sed_df$r <- with(sed_df, mud * mud_rgb[1,] + sand * sand_rgb[1,] + gravel * gravel_rgb[1,])
sed_df$g <- with(sed_df, mud * mud_rgb[2,] + sand * sand_rgb[2,] + gravel * gravel_rgb[2,])
sed_df$b <- with(sed_df, mud * mud_rgb[3,] + sand * sand_rgb[3,] + gravel * gravel_rgb[3,])
sed_df$rgb <- rgb(sed_df$r, sed_df$g, sed_df$b, maxColorValue = 255)

# -------------------------------
# 1. Spatial RGB map
spatial_map <- ggplot() +
  geom_tile(data = sed_df, aes(x = x, y = y, fill = rgb)) +
  scale_fill_identity() +
  geom_sf(data = GB_and_I, fill = "#dadada", inherit.aes = FALSE) +
  coord_sf(xlim = range(sed_df$x), ylim = range(sed_df$y)) +
  labs(title = "Sediment Composition") +
  theme_bw(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(0.15, "cm"),
    axis.title = element_blank(),
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = unit(c(0.25, -0.15, -0.05, -0.15), "cm")
  )

# -------------------------------
# 2. Ternary colour key using same RGB blending

# Create ternary grid of valid compositions
tern_grid <- expand.grid(
  mud = seq(0, 1, by = 0.05),
  sand = seq(0, 1, by = 0.05)
)
tern_grid$gravel <- 1 - tern_grid$mud - tern_grid$sand
tern_grid <- tern_grid[tern_grid$gravel >= 0, ]

# Normalise to ensure mud + sand + gravel = 1
row_sums <- with(tern_grid, mud + sand + gravel)
tern_grid$mud    <- tern_grid$mud / row_sums
tern_grid$sand   <- tern_grid$sand / row_sums
tern_grid$gravel <- tern_grid$gravel / row_sums

# Blend RGB colours using same logic
tern_grid$r <- with(tern_grid, mud * mud_rgb[1,] + sand * sand_rgb[1,] + gravel * gravel_rgb[1,])
tern_grid$g <- with(tern_grid, mud * mud_rgb[2,] + sand * sand_rgb[2,] + gravel * gravel_rgb[2,])
tern_grid$b <- with(tern_grid, mud * mud_rgb[3,] + sand * sand_rgb[3,] + gravel * gravel_rgb[3,])
tern_grid$rgb <- rgb(tern_grid$r, tern_grid$g, tern_grid$b, maxColorValue = 255)

# Create ternary plot
tern_key <- ggtern(data = tern_grid, aes(x = sand, y = gravel, z = mud)) +
  geom_point(aes(color = rgb), size = 2) +
  scale_color_identity() +
  labs(title = "", x = "", y = "", z = "") +
  annotate("text", x = 1, y = 0, z = 0, label = "Sand", size = 3, hjust = 0.85, vjust = 1.5) +
  annotate("text", x = 0, y = 1, z = 0, label = "Gravel", size = 3, hjust = 0.55, vjust = -0.95) +
  annotate("text", x = 0, y = 0, z = 1, label = "Mud", size = 3, vjust = 1.8) +
  theme_minimal(base_size = 5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    tern.axis.title.T = element_blank(),
    tern.axis.title.L = element_blank(),
    tern.axis.title.R = element_blank(),
    tern.axis.arrow.show = FALSE
  )


# -------------------------------
# 3. Combine both plots using patchwork
final_plot <- spatial_map + tern_key + plot_layout(ncol = 2, widths = c(2, 1))
print(final_plot)

tiff("sediment_composition.tiff", units = "cm", width = 14, height = 14, res = 200)
final_plot
dev.off()


##########################################################################################

		#	END		#

########################################################################################
