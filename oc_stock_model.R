set.seed(20)	#20, 25 with mean

options(timeout = max(10000, getOption("timeout")))
#install.packages("sf")
#install.packages("devtools")
#install.packages('gridExtra')
#install.packages("alphahull")
#install.packages(c("randomForest", "gstat", "readxl"))
#install.packages("SpatialML")
#devtools::install_github("imbs-hl/ranger")
#install.packages("ncdf4")
#install.packages("fields")
#install.packages("rgdal")
#install.packages("pdp")
#install.packages("terra")
#library("devtools")
#devtools::install_github("cran/GSIF")
#install.packages("doParallel")
#install.packages("CAST")
#install.packages("geosphere")
#install.packages("graph4lg")
#install.packages("dsm")
#xinstall.packages("sdmpredictors")
#install.packages("leaflet")
#install.packages("rmapshaper")
#install.packages("officer")

library("patchwork")
library("officer")
library("ggpubr")
library("rmapshaper")
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

library("sf")
library("CAST")
library("caret")
library("knitr")


#install.packages(
#  "ggOceanMapsData", 
#  repos = c("https://mikkovihtakari.github.io/drat", 
#            "https://cloud.r-project.org")
#)

library("ggOceanMaps")


#define plot resolution
plot_res <- 300

########################################################################################

		#	IMPORT SHAPEFILES		#

########################################################################################

GB_and_I <- read_sf("~/UCD/shapefiles/GB_I_Isles/GB_I_Isles.shp")
inference_shp <- read_sf("~/UCD/shapefiles/inference_shp/inference_shp.shp")
dublin_bay <- read_sf("~/UCD/shapefiles/Dublin_bay2/Dublin_bay2.shp")
irish_sea <- read_sf("~/UCD/shapefiles/Irish_Sea/Model_Area.shp")
nwe_shelf <- read_sf("~/UCD/shapefiles/NW_Europe_Shelf/NW_Europe_Shelf.shp")
coast2 <- read_sf("~/UCD/shapefiles/World_Internal_Waters_v4_20231025/eez_internal_waters_v4.shp")
coast <- crop(vect(coast2), nwe_shelf)
coast <- st_as_sf(coast)


area_of_interest <- rmapshaper::ms_erase(irish_sea, coast)

seagrass <- read_sf("~/UCD/shapefiles/seagrass_wgs/seagrass_wgs.shp")
seagrass <- crop(vect(seagrass), area_of_interest)

saltmarsh <- read_sf("~/UCD/shapefiles/Saltmarsh/01_Data/WCMC027_Saltmarshes_Py_v6_1.shp")
saltmarsh <- crop(vect(saltmarsh), area_of_interest)

estuary <- read_sf("~/UCD/shapefiles/Estauries/01_Data/14_001_UBC003_SAU_Estuaries2003_v2.shp")
estuary <- crop(vect(estuary), area_of_interest)


########################################################################################

#	Response Variable

########################################################################################

#Extract response data from various sources

#Mean LOI conversion factor of 2.96 reported in Sutherland 1998: "Loss-on-ignition estimates of organic matter and relationships to organic carbon in fluvial bed sediments". This paper compiled a bunch of studies and found 2.96 as the mean (2.13 as the median).

LOI_conv <- 2.96

#Natura
setwd("~/UCD/datasets")				
natura_psa <- vect("~/UCD/datasets/PSA/Natura_PSA.shp")
natura_psa <- as.data.frame(natura_psa, xy=TRUE)
str(natura_psa)


#Mason et et al. (2017) aka Diesing et al. (2017)
mason <- vect("~/UCD/datasets/Mason_2017/Recordset_9483Point.shp")
mason <- as.data.frame(mason, xy=TRUE)

mason$RelativeDe <- as.factor(mason$RelativeDe)
masondf <- mason[mason$RelativeDe == "surface", ]
str(masondf)

#Marine Institute
data_dirs <- list.files("~/UCD/datasets/MI_Irish_sea_PSA", full.names=T) #These 2 directories are the same!!!
out_list <- list()
for(j in 1:length(data_dirs)){
	data_files <- list.files(data_dirs[[j]], full.names=T)
	my_list <- list()
	for(i in 1:length(data_files)){
		my_df <- read_excel(data_files[i])
		my_list[[i]] <- data.frame(my_df$Longitude, my_df$Latitude, my_df$loi_percentage_450degrees)
	}
	out_list[[j]] <- my_list
	print(data_dirs[[j]])
}
str(out_list)
new_df <- data.frame(rbind(out_list[[1]][[1]], out_list[[1]][[2]], out_list[[1]][[3]], out_list[[1]][[4]], 
			out_list[[1]][[5]], out_list[[1]][[6]], out_list[[1]][[7]], out_list[[1]][[8]], 
			out_list[[1]][[9]]))
names(new_df) <- c("lon", "lat", "loi")

#INFOMAR
infomar <- read.csv("infomar.csv", header = T)

#Scottish Blue Carbon Forum aka Smeaton et al. (2021)
setwd("~/UCD/datasets/ScottishBlueCarbonForum")
sbf_1 <- read.csv("Sedimentary_OC_Quality_Reactivity.csv", header=T, fileEncoding="latin1")
sbf_2 <- read.csv("SBF_INFOMAR.csv", header=T, fileEncoding="latin1")
sbf_3 <- read.csv("SBF_primary.csv", header=T, fileEncoding="latin1")

#HABMAP Natural Resources Wales aka McBreen et al. (2008)
setwd("~/UCD/datasets/HABMAP")
habmap <- read.csv("HABMAP.csv", header=T, sep=",")

#MERC consultants
setwd("~/UCD/datasets/MERC_consultants")
merc <- read.csv("MERC_consultants.csv", header = T, sep = ",")
#Convert ITM coordinates to lat and lon
utm_convert<-SpatialPoints(cbind(merc$Easting..ITM, merc$Northing..ITM),
		proj4string=CRS("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=0.99982 +x_0=600000 +y_0=750000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
merc_pts<-spTransform(utm_convert,CRS("+proj=longlat +datum=WGS84"))

#ICES dataset
setwd("~/UCD/datasets/ICES")
ices <- read.csv("DomeSediment_Data_0426381955.csv", header = T, sep = ",")

#Anthony data
setwd("~/UCD/datasets")
AG <- read.csv("QUEST02-0823.csv", header = T, sep = ",")
str(AG)
glm_mod <- glm(AG$TOC ~ AG$LOI, family = poisson(link = "log"))
b <- exp(coef(glm_mod)[1])
a <- 1/exp(coef(glm_mod)[2])

glm_plot <- ggplot(AG, aes(LOI, TOC)) +
			geom_smooth(method = glm, formula = y ~ x, color = "black", method.args = list(family = poisson)) +
			geom_point() + xlab("LOI (%)") + ylab("TOC (%)") + ggtitle("GLM") +
			coord_cartesian(ylim = c(0, 4)) +
			theme_classic()
#glm_plot


#Billy's datasets
data_dirs <- list.files("~/UCD/datasets/billy/", full.names=T) #These 2 directories are the same!!!
billy_list <- list()
for(i in 1:length(data_dirs)){
	billy_file <- read.csv(data_dirs[[i]], header = T, sep = ",")
	billy_list[[i]] <- aggregate(billy_file$Value, by = list(billy_file$Longitude, billy_file$Latitude), FUN = "mean")
	print(i)
}
billy_df <- do.call("rbind", billy_list)
names(billy_df) <- c("lon", "lat", "orgC")
str(billy_df)


#Billy's second dataset
billy2 <- read.csv("~/UCD/datasets/billy2/Sed_OC_Mapping_Cdata.csv", header = T, sep = ",")
str(billy2)


#Billy's third dataset
billy3 <- read.csv("~/UCD/datasets/billy3/Sed_OC_Mapping_MSPACE.csv", header = T, sep = ",")
str(billy3)

#Billy data points
billy4 <- read.csv("~/UCD/datasets/billy4/NI_Sed_OC_Mapping_Zenodo.csv", header = T, sep=",")

#Legacy Pangaea data
pang <- readRDS("~/UCD/Legacy_data_paper/R_objects/loi_data_search_raw.RData")
pang$loi <- pang$loi/100
str(pang)


#Combining all data into one dataframe
df <- data.frame(
			rbind(
				cbind(infomar$Longitude, infomar$Latitude, infomar$TOC, 	rep("infomar", times=nrow(infomar))),
				cbind(natura_psa$Longitude, natura_psa$Latitude, (natura_psa$LOI_pc*a+b),	rep("natura_psa", times=nrow(natura_psa))),
				cbind(masondf$Longitude, masondf$Latitude, masondf$'2mm_OC__',	rep("mason", times=nrow(masondf))),
				cbind(new_df$lon, new_df$lat, (new_df$loi*a+b),	rep("mi_psa", times=nrow(new_df))),
				cbind(sbf_1$Longitude, sbf_1$Latitude, sbf_1$OC_.,	rep("SBF_2021", times=nrow(sbf_1))),
#				cbind(sbf_2$Dec..Long, sbf_2$Dec..Lat, sbf_2$OC...., rep("SBF_INFOMAR", times=nrow(sbf_2))), # Removed as this is duplicate of some of the ICES data
				cbind(sbf_3$Dec..Long, sbf_3$Dec..Lat, sbf_3$OC....,	rep("SBF_primary", times=nrow(sbf_3))),
				cbind(habmap$lon, habmap$lat, habmap$OrgCarbon.,	rep("HABMAP", times=nrow(habmap))),
				cbind(merc_pts@coords, (merc$OM.LOI*a+b),	rep("MERC", times=nrow(merc_pts@coords))),
				cbind(ices$Longitude, ices$Latitude, ices$X.OrgC,	rep("ICES", times=nrow(ices))),
				cbind(AG$Longitude, AG$Latitude, AG$TOC,	rep("AG", times=nrow(AG))),
				cbind(billy_df$lon, billy_df$lat, billy_df$orgC, rep("AFBI", times=nrow(billy_df))),
				cbind(billy2$Long, billy2$Lat, billy2$orgC,	rep("AFBI", times=nrow(billy2))))
#				cbind(billy4$Longitude, billy4$Latitude, billy4$orgC,	rep("AFBI", times=nrow(billy4)))
#				cbind(billy3$Long, billy3$Lat, billy3$orgC, rep("AFBI", times=nrow(billy3)))		#removed this dataset as Billy said some of the positioning was estimated
#				cbind(pang$lon, pang$lat, pang$loi*a+b, pang$source)
#				)
			)

names(df) <- c("lon", "lat", "carbon", "source")	#, "source"


df$lon <- as.numeric(df$lon)
df$lat <- as.numeric(df$lat)
df$carbon <- as.numeric(df$carbon)
#df$carbon <- asin(sqrt(df$carbon/100))
df$carbon <- df$carbon
df$source <- as.factor(df$source)
#df <- na.omit(df)
train_df <- df[, 1:4]
str(train_df)

save_dir <- "~/UCD/Legacy_data_paper/model_training"
setwd(save_dir)
write.csv(train_df, "all_legacy_df.csv", row.names = F)



########################################################################################

#	Predictor Variables for Area of Interest

########################################################################################

#create a grid
cell_size <- c(0.00117, 0.002075) * 1		#APPROXIMATELY 130 X 230 METRES OR 7.5 ARC SECONDS. SAME AS MITCHELL ET AL. 2019. 
#cell_size <- c(0.001041667, 0.001041667)
blankRaster <- rast(ext(area_of_interest))	#MILTIPLY BY 5 TO BE SAME RESOLUTION AS BIAS ADJUSTED RASTERS FROM PANGAEA DATA
res(blankRaster) <- cell_size
blankRaster[] <- 1:ncell(blankRaster)
blankRaster <- mask(blankRaster, vect(area_of_interest))
blankRaster <- crop(blankRaster, area_of_interest)
blankRaster

#Create coordinates dataframe to extract from netcdfs/geotiffs/whatever other formats
pts <- train_df

#Crop train_df to spatial area of area of interest
masked_pts <- mask(vect(pts), vect(area_of_interest))
#masked_pts <- mask(masked_pts, vect(coast), inverse=TRUE)
cropped_pts <- terra::crop(masked_pts, area_of_interest)


#Distance to land with terra
v <- vect(GB_and_I)
crs(v) <- "local"
r <- blankRaster
crs(r) <- "local"
r <- rasterize(v, r)
d <- distance(r)
d <- resample(d, blankRaster)
crs(d) <- "+proj=longlat +datum=WGS84"
d <- terra::project(d, blankRaster)


#plot(d)
#plot(cropped_pts, add = T)
#plot(new_pts, add = T, col = "red")



#TOGGLE THIS LINE ON/OFF TO SUBSET INCLUDE OR NOT COAST_DIST THRESHOLD.
new_pts <- cropped_pts

#calc mean (or other function) of points per cell 
carbon_rast <- terra::rasterize(x=new_pts, y=blankRaster, field='carbon', fun=mean)
names(carbon_rast) <- "response"
train_df <- as.data.frame(carbon_rast, xy = T)
names(train_df) <- c("lon", "lat", "carbon")
my_sf <- st_as_sf(train_df, coords=c("lon", "lat"), crs=crs(blankRaster))

pts <- train_df[, 1:2]
names(pts) <- c("lon", "lat")
str(pts)


write.csv(train_df, "train_df.csv", row.names = F)

mudbelt <- read_sf("~/UCD/shapefiles/mudbelt_smalls/mudbelt_smalls.shp")
mid <- mean(train_df$carbon, na.rm=T)
continents <- read_sf("~/UCD/shapefiles/continent-poly/Continents.shp")

library(cowplot)


#Visualize geographic locations of training points
sampling_plot <- ggplot(my_sf) + 
    geom_sf(aes(color = carbon), size = 0.1) +
    xlab("") + 
    ylab("") + 
    labs(color = "OC content (%)") + 
    scale_color_gradient(low = "blue", high = "yellow", limits = c(0, 2), breaks = c(0, 1, 2)) +
    theme_bw() +
    theme(
        legend.key.size = unit(0.3, "cm"),          # Reduce size of legend keys
        legend.text = element_text(size = 8),      # Reduce size of legend text
        legend.title = element_text(size = 9),     # Reduce size of legend title
        legend.spacing = unit(0.1, "cm"),          # Reduce spacing between legend items
        legend.spacing.x = unit(0.1, "cm"),        # Reduce horizontal spacing
        legend.spacing.y = unit(0.1, "cm"),        # Reduce vertical spacing
        legend.position = "bottom",                # Position legend at the bottom
        axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.margin = unit(c(0, 0, 0, -0.5), "cm") # Adjust bottom margin of the plot
    ) +
    geom_sf(data = mudbelt, fill = NA, linewidth = 0.75, color = "black", inherit.aes = TRUE) +
    geom_sf(data = coast, fill = "#E883B2", color = NA, alpha = 0.25, inherit.aes = TRUE) +
    geom_sf(data = irish_sea, fill = NA, color = "black", linewidth = 0.15) +
    geom_sf(data = continents, fill = "#dadada", inherit.aes = TRUE) +
    coord_sf(xlim = c(-9, 2), ylim = c(50, 56))



inset_plot <- ggplot() +
				geom_sf(data=continents, fill="#dadada", inherit.aes=TRUE) +
				coord_sf(
				xlim=c(-31, 35),
				ylim=c(34, 73),
				expand=FALSE) +
				geom_rect(
					aes(xmin=-8.5, xmax=0, ymin=49.75, ymax=56),
					fill=NA,
					color="red",
					linewidth=0.5) +
			theme_classic() +
			theme(legend.position="none",
					axis.text.x=element_blank(),
					axis.ticks.x=element_blank(),
					axis.text.y=element_blank(),
					axis.ticks.y=element_blank(),
					panel.border=element_rect(color="black", fill=NA, linewidth=1),
					plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "cm") # Remove margins around the inset plot
					)

tiff("Sampling_data_locations.tiff", units="cm", width=9, height=9, res=plot_res)
ggdraw(sampling_plot) +
	draw_plot(
		{ 
			inset_plot
		},
		x=0.69,
		y=0.69,
		width=0.3,
		height=0.3)
dev.off()

################################################################################

		#		Creating Raster stack of adjusted Predictors		#

################################################################################


#Distance to coast
#plot(d)

#Bathymetry
raster_files <- list.files("~/UCD/datasets/bathymetry/ASCIIs", full.names = T)
ras_output <- list()
for(i in 1:length(raster_files)){
	raster_file <- raster_files[[i]]
	ras <- rast(raster_file)
	ras_output[[i]] <- resample(ras, blankRaster)
	print(i)
	flush.console()
}
emod_bath <- mosaic(ras_output[[1]], ras_output[[2]], ras_output[[3]], ras_output[[4]], ras_output[[5]], 
		ras_output[[6]], ras_output[[7]], ras_output[[8]], ras_output[[9]], fun = "mean")
#plot(emod_bath)


#Distance to coast
#plot(d)

# Set working directory
setwd("~/UCD/shapefiles")				

# Read the shapefile using sf
smi_sf <- st_read("SMI/SMI.shp")

# Convert to terra vector
smi_vect <- vect(smi_sf)

# Check that the relevant field exists
names(smi_vect)  # to confirm 'smi_val' or similar exists

# Create a blank raster with extent of your area of interest
smi_blank_ras <- rast(ext(area_of_interest))

# Rasterize using terra
smi_ras <- rasterize(smi_vect, smi_blank_ras, field = "SMI_value", fun = mean)

# Align and mask
smi_ras <- resample(smi_ras, blankRaster) %>%
				mask(area_of_interest) %>%
				crop(area_of_interest)


# Read the SDI shapefile with sf
sdi_sf <- st_read("SDI/SDI.shp")

# Check geometry and CRS
st_crs(sdi_sf) <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +rf=299.3249646 +units=m +no_defs +type=crs"

# Convert sf object to terra vector
sdi_vect <- vect(sdi_sf)

# Reproject to match raster CRS (assuming blankRaster is in the target CRS)
sdi_reproj <- project(sdi_vect, crs(blankRaster))

# Rasterize using terra
sdi_ras <- rasterize(sdi_reproj, smi_blank_ras, field = "SDI_Value", fun = mean)

# Align and mask
sdi_ras <- resample(sdi_ras, blankRaster) %>%
				mask(area_of_interest) %>%
				crop(area_of_interest)
				


setwd(save_dir)
tiff("Sediment_disturbance_indices.tiff", units="cm", width=20, height=12, res=plot_res)
par(mfrow = c(1, 2))
plot(smi_ras, main="Sediment mobility index (SMI)")
plot(sdi_ras, main="Sediment disturbance index (SDI)")
dev.off()
par(mfrow = c(1, 1))



# Define file paths and names in one step
raster_paths <- c(
  temp         = "~/UCD/Legacy_data_paper/R_objects/bottom_temp/original.tif",
  salinity     = "~/UCD/Legacy_data_paper/R_objects/bottom_salinity/bias_corrected_downscaled.tif",
  mean_current = "~/UCD/Legacy_data_paper/R_objects/bottom_current_velocity/mean_current_velocity.tif",
  max_current  = "~/UCD/Legacy_data_paper/R_objects/bottom_current_velocity/max_current_velocity.tif",
  chl          = "~/UCD/Legacy_data_paper/R_objects/surface_chl/bias_corrected_downscaled.tif",
  summer_spm   = "~/UCD/Legacy_data_paper/R_objects/surface_tss/summer_original.tif",
  winter_spm   = "~/UCD/Legacy_data_paper/R_objects/surface_tss/winter_bias_corrected_downscaled.tif"
)

# Process all rasters using lapply for cleaner code
bespoke_ras <- lapply(raster_paths, function(path) {
  ras <- rast(path)
  ras <- resample(ras, blankRaster)
  ras <- mask(ras, vect(area_of_interest))
  crop(ras, area_of_interest)
})

# Optionally plot them
# par(mfrow = c(2, 4))
# lapply(bespoke_ras, plot)

# Check names
names(bespoke_ras)


#Mud, sand and gravel %
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



orb_val <- c("max", "mean")
main_strings <- c("Maximum", "Mean")
orb_ras_output <- list()


for (i in seq_along(orb_val)) {
  # 1. Read shapefile as SpatVector
  orb_vect <- vect(paste0("~/UCD/datasets/Wave_orbital_velocity_seafloor_Irish Sea/Uw_", orb_val[i], "/Uw_", orb_val[i], ".shp"))
  
  # 2. Rename fields if necessary (assumes columns: easting, northing, val)
  names(orb_vect) <- c("easting", "northing", "val")
  
  # 3. Reproject to match blank raster CRS
  orb_reproj <- terra::project(orb_vect, crs(blankRaster))
  
  # 4. Rasterize without interpolation — 1:1 raster cell coverage of points
  orb_ras <- terra::rasterize(orb_reproj, smi_blank_ras, field = "val", fun = "mean", background = NA)
  
  # 5. Align resolution with blankRaster
  orb_ras <- resample(orb_ras, blankRaster)  # "near" ensures no interpolation

  # 6. Apply mask and crop to area of interest
  orb_ras <- mask(orb_ras, vect(area_of_interest))
  orb_ras_output[[i]] <- crop(orb_ras, area_of_interest)
  
  # Optional plot
  # plot(orb_ras_output[[i]], main = paste(main_strings[i], "orbital wave velocity at the seafloor"))
}

names(orb_ras_output) <- orb_val

# Quick plot
#par(mfrow = c(1, 2))
#plot(orb_ras_output$mean, main = "IDW Mean Orbital Velocity")
#plot(orb_ras_output$max,  main = "IDW Max Orbital Velocity")
#par(mfrow = c(1, 1))



#THIS IS THE 'CORRECTED' PREDICTORS ONLY LIST
infer_list <- list(bespoke_ras$salinity, bespoke_ras$temp, bespoke_ras$max_current, bespoke_ras$mean_current, bespoke_ras$chl, bespoke_ras$winter_spm, bespoke_ras$summer_spm, mud, sand, gravel, orb_ras_output$max, orb_ras_output$mean)
infer_names <- c("salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")


par(mfrow = c(3, 4))
infer_ras <- list()
for(i in 1:length(infer_list)){
	out_ras <- mask(infer_list[[i]], area_of_interest)
	infer_ras[[i]] <- crop(out_ras, area_of_interest)
	#plot(infer_ras[[i]], main = infer_names[i])
	#plot(vect(pts), cex = 0.25, col = "red", add = T)
}
par(mfrow = c(1, 1))


adjusted_infer_vars <- c(infer_ras[[1]], infer_ras[[2]], infer_ras[[3]], infer_ras[[4]], infer_ras[[5]], infer_ras[[6]], infer_ras[[7]], infer_ras[[8]], infer_ras[[9]], infer_ras[[10]], infer_ras[[11]], infer_ras[[12]])
names(adjusted_infer_vars) <- infer_names



########################################################################################

		#		ALL PREDICTORS (ADJUSTED + OTHERS)		#

########################################################################################


#All predictors, even those with no correction/not corrected.
infer_list <- list(d, emod_bath, bespoke_ras$salinity, bespoke_ras$temp, bespoke_ras$max_current, bespoke_ras$mean_current, bespoke_ras$chl, bespoke_ras$winter_spm, bespoke_ras$summer_spm, mud, sand, gravel, orb_ras_output$max, orb_ras_output$mean)
infer_names <- c("coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")


setwd(paste(save_dir, "/all_adjusted", sep = ""))
tiff("Predictors.tiff", units="cm", width=36, height=16, res=plot_res)
par(mfrow = c(2, 7))
infer_ras <- list()
for(i in 1:length(infer_list)){
	out_ras <- mask(infer_list[[i]], area_of_interest)
	infer_ras[[i]] <- crop(out_ras, area_of_interest)
	plot(infer_ras[[i]], main = infer_names[i])
#	plot(vect(pts), cex = 0.25, col = "red", add = T)
}
dev.off()

par(mfrow = c(1, 1))


all_infer_vars <- c(infer_ras[[1]], infer_ras[[2]], infer_ras[[3]], infer_ras[[4]], infer_ras[[5]], infer_ras[[6]], infer_ras[[7]], infer_ras[[8]], infer_ras[[9]], infer_ras[[10]], infer_ras[[11]], infer_ras[[12]], infer_ras[[13]], infer_ras[[14]])
names(all_infer_vars) <- infer_names



########################################################################################

		#		NOT ADJUSTED PREDICTORS		#

########################################################################################



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




#Mud, sand and gravel %
original_mud <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Mud_Fraction.tif")
original_mud <- resample(original_mud, blankRaster)
original_mud <- mask(original_mud, area_of_interest)
original_mud <- crop(original_mud, area_of_interest)

original_sand <- rast("~/UCD/datasets/Diesing_legacy_paper/Sediment predictions 1/Predicted_Sand_Fraction.tif")
original_sand <- resample(original_sand, blankRaster)
original_sand <- mask(original_sand, area_of_interest)
original_sand <- crop(original_sand, area_of_interest)

original_gravel <- 1 - (original_mud + original_sand)


#Combine all predictor variables. BUT THIS IS THE 'NOT CORRECTED' LIST
infer_list <- list(d, emod_bath, original_ras$salinity, original_ras$temp, original_ras$max_current, original_ras$mean_current, original_ras$chl, original_ras$winter_spm, original_ras$summer_spm, original_mud, original_sand, original_gravel, out_ras$OrbitalVelMax, out_ras$OrbitalVelMean)
infer_names <- c("coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "OrbitalVelMax", "OrbitalVelMean")


setwd(paste(save_dir, "/not_adjusted", sep = ""))
tiff("Predictors.tiff", units="cm", width=20, height=20, res=plot_res)
par(mfrow = c(3, 5))
infer_ras <- list()
for(i in 1:length(infer_list)){
	out_ras <- mask(infer_list[[i]], area_of_interest)
	infer_ras[[i]] <- crop(out_ras, area_of_interest)
	plot(infer_ras[[i]], main = infer_names[i])
}
dev.off()

par(mfrow = c(1, 1))


original_infer_vars <- c(infer_ras[[1]], infer_ras[[2]], infer_ras[[3]], infer_ras[[4]], infer_ras[[5]], infer_ras[[6]], infer_ras[[7]], infer_ras[[8]], infer_ras[[9]], infer_ras[[10]], infer_ras[[11]], infer_ras[[12]], infer_ras[[13]], infer_ras[[14]])
names(original_infer_vars) <- infer_names


########################################################################################


pred_ras <- c(carbon_rast, adjusted_infer_vars, original_infer_vars, all_infer_vars)
pred_vars <- extract(pred_ras, vect(pts))
pred_vars <- data.frame(pts, pred_vars)
pred_vars <- na.omit(pred_vars)
str(pred_vars)

adjusted_vars <- data.frame(pred_vars[, c(1:2, 4:14)])
str(adjusted_vars)

original_vars <- data.frame(pred_vars[, c(1:2, 4, 17:30)])
names(original_vars) <- c("lon", "lat", "response", "coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")
str(original_vars)

all_vars <- data.frame(pred_vars[, c(1:2, 4, 31:ncol(pred_vars))])
names(all_vars) <- c("lon", "lat", "response", "coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")
str(all_vars)


pred_vars <- list(adjusted_vars, original_vars, all_vars)
names(pred_vars) <- c("adjusted", "original", "all")
str(pred_vars)


inference <- list(original_infer_vars, all_infer_vars)
#names(inference[[1]]) <- names(adjusted_infer_vars)
names(inference[[1]]) <- c("coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")
names(inference[[2]]) <- c("coast_dist", "bathymetry", "salinity", "temperature", "max_current", "mean_current","productivity", "spm_winter", "spm_summer", "mud_percent", "sand_percent", "gravel_percent", "max_wave_orbital", "mean_wave_orbital")


setwd(save_dir)
saveRDS(pred_vars, "pred_vars.RData")


########################################################################################

	#	FOR LOOP TO TRAIN BOTH MODELS	#
	
########################################################################################

folders <- c("/not_adjusted", "/all_adjusted")

metrics_mat <- matrix(0, nrow = length(folders), ncol = 10)
metrics <- data.frame(metrics_mat)
names(metrics) <- c("Input data", "Mean OC content (%)", "OC content (SD)", "Total stock estimate (Mt)", "Total stock uncertainty (Mt)", "Mean BD", "BD SD", "Rsquared", "RMSE", "MAE")
metrics


#BD_adjust <- rast("~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/bias_corrected_downscaled.tif")
BD_adjust <- rast("~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/DBD_RF_prediction.tif")
BD_adjust <- resample(BD_adjust, blankRaster)
BD_adjust <- mask(BD_adjust, vect(area_of_interest))
BD_adjust <- crop(BD_adjust, area_of_interest)


porosity <- 0.3805 * original_mud + 0.42071
BD_not_adjust <- (1 - porosity) * 2650
BD_not_adjust <- mask(BD_not_adjust, vect(area_of_interest))
BD_not_adjust <- crop(BD_not_adjust, vect(area_of_interest))

#porosity <- 0.3805 * mud + 0.42071
#BD_adjust <- (1 - porosity) * 2650
#BD_adjust <- mask(BD_adjust, vect(area_of_interest))
#BD_adjust <- crop(BD_adjust, vect(area_of_interest))

BD <- list(BD_not_adjust, BD_adjust)

predicted_response <- list()
predicted_raster_vals <- list()
poc_plot <- list()
poc_uncertainty <- list()
mpoc_plot <- list()
mpoc_uncertainty <- list()
bulk_density <- list()
sed_density <- list()
var_imp <- list()
oc_content_ras <- list()
oc_content_uncert_ras <- list()
mpoc_ras <- list()
mpoc_uncert_ras <- list()



setwd(save_dir)
all_pred_vars <- readRDS("pred_vars.RData")
all_pred_vars <- list(all_pred_vars[[2]], all_pred_vars[[3]])
names(all_pred_vars) <- c( "original", "all_adjusted")


for(p in 1:length(folders)){

pred_vars <- all_pred_vars[[p]]
data_points <- nrow(pred_vars)

########################################################################################

#Pie chart of the sediment propoerties of the data
sed_density[[p]] <- ggplot() +
				geom_density(aes(mud_percent*100, fill="Mud"), alpha=0.2, data=pred_vars) +
				geom_density(aes(sand_percent*100, fill="Sand"), alpha=0.2, data=pred_vars) +
				geom_density(aes(gravel_percent*100, fill="Gravel"), alpha=0.2, data=pred_vars) +
				theme(legend.position = "bottom") +
				theme(legend.title=element_blank()) +
				xlab("Content (%)") + ylab("Density")


setwd(paste(save_dir, folders[p], sep = ""))
tiff("Sediment_densities.tiff", units="cm", width=15, height=15, res=plot_res)
print(sed_density[[p]])
dev.off()
				
########################################################################################
	
setwd(paste(save_dir, folders[p], sep = ""))

library("automap")
library("blockCV")
spat_train <- pred_vars
coordinates(spat_train) <- ~ lon+lat
proj4string(spat_train) <- CRS("+proj=longlat +datum=WGS84")

k <- 10 # Number of folds

########################################################################################

	#	TRAINING DATA FRAME CREATION	#
	
########################################################################################

#Training parameters
trees <- 500
folds <- 10
n_preds <- 2

#Training DF creation
#pred_vars <- readRDS("pred_vars.RData")

df <- pred_vars
response <- df$response
train_df <- df

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


tiff("Spatial_folds.tiff", units="cm", width=13.5, height=6.5, res=plot_res)
par(mfrow = c(2, 5))
par(mar = c(0, 1, 0, 1))
for(i in 1:length(knndm_folds)){
#	plot(PP)
#	plot(B, cex = 1, pch = 0)
	plot(C[knndm_folds$indx_train[[i]], ], pch=16, col="#1c1c1c", cex=0.25) 
	plot(C[knndm_folds$indx_test[[i]], ], pch=16, col="#ff2c2c", add=T, cex=0.25)
	plot(GB_and_I, col="#dadada", add = T, lwd=0.3)
	arrows(x0 = -7.15, y0 = 54, x1 = -7.15, y1 = 55, length = 0.025, col = "black", lwd = 1)
	text(x = -7.15, y = 55.25, labels = "N", cex = 0.75, col = "black")
	print(i)
}
dev.off()

train_pts <- train_df[, 1:2]
train_df <- train_df[, -c(1, 2)]
str(train_df)

saveRDS(train_df, "train_df.RData")
saveRDS(train_pts, "train_pts.RData")
saveRDS(knndm_folds, "nndm_folds.RData")

########################################################################################

			#	CARBON DENSITY MODEL TRAINING	#

########################################################################################

#Load previously created train_df
train_df <- readRDS("train_df.RData")
train_pts <- readRDS("train_pts.RData")
knndm_folds <- readRDS("nndm_folds.RData")

#Forward feature selection and spatial cross validation without Euclidean Distances
train_df_i <- data.frame(train_df[, -1])
response <- train_df[, 1]
mtry <- 2
rf_mod_ffs_nndm <- ffs(train_df_i, response, metric="RMSE",
				method="rf", tuneGrid=data.frame("mtry"=mtry),
				importance=TRUE, ntree=trees,
				trControl=trainControl(method="cv", index=knndm_folds$indx_train, indexOut=knndm_folds$indx_test))

rf_mod_ffs_nndm	

tiff("Variable_importance_nndm.tiff", units="cm", width=12, height=12, res=plot_res)
print(plot(varImp(rf_mod_ffs_nndm, scale = F), xlab = "% Increase in MSE"))
dev.off()

tiff("FFS_variable_runs_nndm.tiff", units="cm", width=18, height=12, res=plot_res)
print(plot(rf_mod_ffs_nndm))
dev.off()			

saveRDS(rf_mod_ffs_nndm, "rf_mod_ffs_nndm.RData")

observed_response <- train_df$response

########################################################################################

		#		PARTIAL PLOTS		#
		
########################################################################################

library("gridExtra")

varImp(rf_mod_ffs_nndm, scale = F)
var_imp[[p]] <- importance(rf_mod_ffs_nndm$finalModel)


selected_model <- rf_mod_ffs_nndm
selected_model
predictors <- selected_model$finalModel$xNames
predictor_labels <- c("Distance to coast (km)",
					"Bathymetry (m)",
					expression("S"[bot]),
					expression("T"[bot]* "("*degree*"C)"),
					expression(italic(U)[bot max] * " (m s"^{-1} * ")"),
					expression(italic(U)[bot mean] * " (m s"^{-1} * ")"),
					expression("Surface chlorophyll-a" * " (" * mu * "g l"^{-1} * ")"),
					expression("SPM"[winter]* " (mg l"^{-1} * ")"),
					expression("SPM"[summer]* " (mg l"^{-1} * ")"),
					expression("Mud"[cont]* " (%)"),
					expression("Sand"[cont]* " (%)"),
					expression("Gravel"[cont]* " (%)"),
					expression(italic(u)[orb max] * " (m s"^{-1} * ")"),
					expression(italic(u)[orb mean] * " (m s"^{-1} * ")")
					)

# Define two string vectors
vector1 <- names(pred_vars[, 4:length(names(pred_vars))])
vector2 <- predictor_labels[1:length(vector1)]

# Create a named vector (dictionary) where vector1 elements are keys, and vector2 elements are values
dictionary <- setNames(vector2, vector1)

# Define a function that takes an element from vector1 and prints the corresponding element from the dictionary
print_equivalent <- function(item) {
  # Check if the item exists in the dictionary
  if (item %in% names(dictionary)) {
    # Print the corresponding value from the dictionary
    print(dictionary[[item]])
  } else {
    print("Item not found in dictionary")
  }
}

					
predicted_response[[p]] <- selected_model$finalModel$predicted
#nRow <- floor(sqrt(length(predictors)))
nRow <- 1



if (FALSE) {
pdplot <- list()
for(i in 1:length(predictors)){
	pd <- partial(selected_model, pred.var=predictors[i])
	pdplot[[i]] <- plotPartial(pd, ylim=c(range(pd$yhat)[1]-0.05, range(pd$yhat)[2]+0.05),
	ylab = 'Carbon content (%)',
	xlab = print_equivalent(predictors[i]))
	print(i)
}

pd_width <- 20 + (length(predictors)*4)
jpeg("carbon_density_partial_plots_nndm.jpeg", units = "cm", width = pd_width, height = 12, res = plot_res)
print(do.call("grid.arrange", c(pdplot, nrow=nRow)))
dev.off()
}



library(gridExtra)
library(grid)
library(ggplot2)
library(pdp)

# Step 1: Reorder predictors by variable importance
ordered_vars <- rownames(var_imp[[p]][order(-var_imp[[p]][, "%IncMSE"]), ])
predictors_ordered <- intersect(ordered_vars, predictors)  # in case of mismatch

# Step 2: Re-label plots alphabetically
pdplot <- list()
letters_labels <- letters[1:length(predictors_ordered)]

# Step 3: Generate partial dependence plots in new order
for (i in seq_along(predictors_ordered)) {
  pd <- partial(selected_model, pred.var = predictors_ordered[i], progress = "none")

  pp <- ggplot(pd, aes_string(x = predictors_ordered[i], y = "yhat")) +
    geom_line(color = "black", linewidth = 0.6) +
    labs(
      x = print_equivalent(predictors_ordered[i]),
      y = 'Carbon content (%)'
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
    pp,
    top = textGrob(
      paste0(letters_labels[i], ")"),
      x = unit(0.01, "npc"),
      y = unit(0.99, "npc"),
      just = c("left", "top"),
      gp = gpar(fontface = "bold", fontsize = 10)
    )
  )

  pdplot[[i]] <- labeled_plot
}

# Step 4: Save the plots (1 row)
pd_width <- 20 + (length(predictors)*4)
tiff("carbon_density_partial_plots_nndm.tiff", units = "cm", width = pd_width, height = 12, res = 300)
grid.arrange(grobs = pdplot, nrow = nRow)
dev.off()


########################################################################################

		#		INFERENCE OVER AREA ADJUSTED VS NOT ADJUSTED PREDICTORS		#
		
########################################################################################

pred_ras <- terra::predict(inference[[p]], selected_model, se.fit = TRUE, na.rm = T)
masked_rast <- terra::mask(pred_ras, vect(area_of_interest))
cropped_rast <- terra::crop(masked_rast, area_of_interest)
#cropped_rast <- ((sin(cropped_rast))^2)*100
cropped_rast <- cropped_rast
par(mfrow = c(1, 1))
#plot(cropped_rast)
#plot(GB_and_I, add = T)

oc_content_ras[[p]] <- cropped_rast

writeRaster(cropped_rast, "pred_ras.tif", overwrite=T)


plot_legend_width <- 0.15

#Display raster using ggplot
r_points <- as.data.frame(cropped_rast, xy = T)
r_df <- data.frame(r_points)
names(r_df) <- c("x", "y", "layer")
predicted_raster_vals[[p]] <- r_df

poc_plot[[p]] <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + 
	scale_fill_viridis_c(option="cividis", limits=c(0, 2), breaks=c(0, 1, 2), na.value="white") +
	xlab("") + ylab("") + labs(fill="OC content (%)") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) + 
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) +
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))


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
	uncert_ras <- terra::predict(inference[[p]], uncert_mod, na.rm = T)
#	uncert_ras <- ((sin(uncert_ras))^2)*100	
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
#plot(uncert_ras)

oc_content_uncert_ras[[p]] <- uncert_ras

#Display raster using ggplot
r_points <- as.data.frame(uncert_ras, xy = T)
r_df <- data.frame(r_points)
names(r_df) <- c("x", "y", "layer")

poc_uncertainty[[p]] <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + 
	scale_fill_viridis_c(option="inferno", limits=c(0, 2), breaks=c(0, 1, 2), na.value="white") +
	xlab("") + ylab("") + labs(fill="OC content\nuncertainty (%)") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) + 
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) +
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))



tiff("POC.tiff", units="cm", width=12, height=14, res=plot_res)
print(poc_plot[[p]])
dev.off()


tiff("POC_uncertainty.tiff", units="cm", width=12, height=14, res=plot_res)
print(poc_uncertainty[[p]])
dev.off()


tiff("POC_plots.tiff", units="cm", width=18, height=12, res=plot_res)
print(ggarrange(poc_plot[[p]], poc_uncertainty[[p]], labels=c("a.", "b.")))
dev.off()

########################################################################################

#Carbon stock calculations
poc <- cropped_rast
crs(poc) <- "+proj=longlat +datum=WGS84"


depth <- 0.1
mpoc <- (poc/100) * BD[[p]] * depth

total_area  <- expanse(vect(area_of_interest), unit = "m")
mean_mpoc <- mean(values(mpoc), na.rm = T)
mean_poc <- mean(values(poc), na.rm = T)
mean_bd <- mean(values(BD[[p]]), na.rm = T)

mpoc_1 <- total_area * mean_mpoc / 1000000000
mpoc_1 			#TOTAL CARBON STOCK SCALED UP FOR STUDY AREA (OFF THE SHELF ESTIMATE)

#cell_area <- (res(mpoc)[1] * 111320) * (res(mpoc)[2] * 111320)
cell_area <- cellSize(mpoc)
mpoc_2 <- sum(values(mpoc * cell_area), na.rm=T) / 1000000000 
mpoc_2

mpoc_ras[[p]] <- mpoc


#Display raster using ggplot
r_df <- as.data.frame(mpoc, xy = T)
names(r_df) <- c("x", "y", "layer")


mpoc_plot[[p]] <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) +
	scale_fill_viridis_c(option="viridis", direction=1, limits=c(0, 2), breaks=c(0, 1, 2), na.value="white") +
	xlab("") + ylab("") + labs(fill="OC stock (kg" ~m^-2~ ")") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))




#Display raster using ggplot
r_points <- as.data.frame(BD[[p]], xy = T)
r_df <- data.frame(r_points)
names(r_df) <- c("x", "y", "layer")
upr_bd <- ceiling(max(r_df$layer, na.rm=T) / 100) * 100
lwr_bd <- floor(min(r_df$layer, na.rm=T) / 100) * 100
mid_bd <- (abs(upr_bd) + abs(lwr_bd)) / 2

bulk_density[[p]] <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + 
	scale_fill_viridis_c(option = "viridis", limits=c(lwr_bd, upr_bd), breaks=c(lwr_bd, mid_bd, upr_bd), na.value="white") +
	xlab("") + ylab("") + labs(fill="DBD (kg" ~m^-3~ ")") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.5,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
        coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))



#Carbon stock uncertainty calculations
poc_uncert <- uncert_ras
crs(poc_uncert) <- "+proj=longlat +datum=WGS84"

dbd_uncert <- rast("~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/dbd_RF_uncertainty_ras.tif")
dbd_uncert <- resample(dbd_uncert, blankRaster)
dbd_uncert <- mask(dbd_uncert, area_of_interest)
dbd_uncert <- crop(dbd_uncert, area_of_interest)

depth <- 0.1
#mpoc_uncert <- (poc_uncert/100) * BD[[p]] * depth
mpoc_uncert <- (poc_uncert/100) * dbd_uncert * depth

total_area_uncert  <- expanse(vect(area_of_interest), unit = "m")
mean_mpoc_uncert <- mean(values(mpoc_uncert), na.rm = T)
mean_poc_uncert <- mean(values(poc_uncert), na.rm = T)
mean_bd_uncert <- mean(values(BD[[p]]), na.rm = T)

mpoc_1_uncert <- total_area_uncert * mean_mpoc_uncert / 1000000000
mpoc_1_uncert #	46.9Mt TOTAL CARBON STOCK SCALED UP FOR STUDY AREA (OFF THE SHELF ESTIMATE)


mpoc_2_uncert <- sum(values(mpoc_uncert), na.rm=T) * cell_area / 1000000000 
mpoc_2_uncert

mpoc_uncert_ras[[p]] <- mpoc_uncert

#Display raster using ggplot
r_df <- as.data.frame(mpoc_uncert, xy = T)
names(r_df) <- c("x", "y", "layer")


mpoc_uncertainty[[p]] <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + 
	scale_fill_viridis_c(option="plasma", limits=c(0, 2.5), breaks=c(0, 1.25, 2.5), na.value="white") +
	xlab("") + ylab("") + labs(fill="OC stock\nuncertainty (kg" ~m^-2~ ")") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) + 
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))


tiff("MPOC.tiff", units="cm", width=12, height=14, res=plot_res)
print(mpoc_plot[[p]])
dev.off()


tiff("BD.tiff", units="cm", width=12, height=14, res=plot_res)
print(bulk_density[[p]])
dev.off()


tiff("MPOC_uncertainty.tiff", units="cm", width=12, height=14, res=plot_res)
print(mpoc_uncertainty[[p]])
dev.off()


tiff("MPOC_plots.tiff", units="cm", width=18, height=12, res=plot_res)
print(ggarrange(mpoc_plot[[p]], mpoc_uncertainty[[p]], labels=c("a.", "b."), common.legend=TRUE))
dev.off()



########################################################################################

		#		AREA OF APPLICABILITY AOA		#
		
########################################################################################

train_DI <- trainDI(model = selected_model, train = train_df, variables = selected_model$finalModel$xNames)
AOA <- aoa(inference[[p]], model = selected_model, trainDI = train_DI, variables = selected_model$finalModel$xNames, CVtest = indices$index)


# Step 4: Binary mask of AOA
outsideAOA <- AOA$DI > AOA$parameters$threshold

# Step 5: Save raw AOA raster
tiff("AOA_raw.tiff", units = "cm", width = 14, height = 14, res = 200)
plot(AOA)
dev.off()

# Step 6: Save binary AOA mask raster
tiff("AOA_binary.tiff", units = "cm", width = 14, height = 14, res = 200)
plot(outsideAOA)
dev.off()

# Step 7: Convert binary mask to data.frame for ggplot
r_points <- as.data.frame(outsideAOA, xy = TRUE, na.rm = TRUE)
names(r_points)[3] <- "AOA"
r_points$AOA <- as.integer(r_points$AOA)  # 1 = outside AOA, 0 = inside AOA

# Step 8: Plot ggplot version
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

# Step 9: Calculate % of FALSE cells within AOI
freq_table <- freq(outsideAOA)
false_count <- freq_table[freq_table$value == 0, "count"]
total_count <- sum(freq_table$count, na.rm = TRUE)
percent_false <- (false_count / total_count) * 100
cat("Percentage of FALSE values within area_of_interest:", percent_false, "%\n")

########################################################################################

		#		MODEL METRICS		#
		
########################################################################################

trans_predicted <- selected_model$finalModel$predicted
trans_response <- response

rmse_met <- sqrt(mean((trans_predicted - trans_response)^2))
mae <- mean(abs(trans_predicted - trans_response))

metrics[p, ] <- data.frame(folders[p],
				round(mean(values(cropped_rast, na.rm = T)), 2),
				round(mean(values(uncert_ras), na.rm=T), 2),
				round(mpoc_2, 2), 
				round(mpoc_2_uncert, 2),
#				round(AOA_total_area, 2),
#				AOA_prop,
				round(mean(values(BD[[p]]), na.rm=T), 2),
				sd(values(BD[[p]]), na.rm=T),				 
				round(mean(selected_model$finalModel$rsq, na.rm=T), 2),
				round(rmse_met, 4),
				round(mae, 4)
				)

print(p)
flush.console()

}

setwd(save_dir)
write.csv(metrics, "model_metrics_nndm.csv", row.names=F)
metrics

var_imp


p11 <- (poc_plot[[1]] | poc_uncertainty[[1]]) +
  plot_annotation(title = "Not adjusted", tag_levels = list(c("a", "b"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))

p12 <- (poc_plot[[2]] | poc_uncertainty[[2]]) +
  plot_annotation(title = "Adjusted", tag_levels = list(c("c", "d"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))
  
p13 <- wrap_elements(p11) + wrap_elements(p12) + plot_layout(guides = "collect")


tiff("All_POC_plots.tiff", units="cm", width=28, height=16, res=plot_res)
print(p13)
dev.off()




p11 <- (mpoc_plot[[1]] | mpoc_uncertainty[[1]]) +
  plot_annotation(title = "Not adjusted", tag_levels = list(c("a", "b"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))

p12 <- (mpoc_plot[[2]] | mpoc_uncertainty[[2]]) +
  plot_annotation(title = "Adjusted", tag_levels = list(c("c", "d"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))
  
p13 <- wrap_elements(p11) + wrap_elements(p12) + plot_layout(guides = "collect")


tiff("All_MPOC_plots.tiff", units="cm", width=28, height=16, res=plot_res)
print(p13)
dev.off()

#Display raster using ggplot
dbd_uncert <- rast("~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/dbd_RF_uncertainty_ras.tif")
dbd_uncert <- resample(dbd_uncert, blankRaster)
dbd_uncert <- mask(dbd_uncert, area_of_interest)
dbd_uncert <- crop(dbd_uncert, area_of_interest)
plot(dbd_uncert)

r_points <- as.data.frame(dbd_uncert, xy = T)
r_df <- data.frame(r_points)
names(r_df) <- c("x", "y", "layer")
upr_bd <- ceiling(max(r_df$layer, na.rm=T) / 100) * 100
lwr_bd <- floor(min(r_df$layer, na.rm=T) / 100) * 100
mid_bd <- (abs(upr_bd) + abs(lwr_bd)) / 2

dbd_uncert_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) + 
	scale_fill_distiller(palette = "PRGn", limits=c(lwr_bd, upr_bd), breaks=c(lwr_bd, mid_bd, upr_bd), na.value="white") +
	xlab("") + ylab("") + labs(fill="DBD\nuncertainty(kg" ~m^-3~ ")") + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.5,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
        coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))

p14a <- (bulk_density[[1]]) +
  plot_annotation(title = "Not adjusted", tag_levels = list(c("a", "b"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))

p14b <- (bulk_density[[2]]) +
  plot_annotation(title = "Adjusted", tag_levels = list(c("a", "b"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))


p14 <- wrap_elements(p14a) | wrap_elements(p14b) + plot_layout(guides = "collect") & theme(legend.position = "bottom")


tiff("All_BD_plots.tiff", units="cm", width=16, height=16, res=plot_res)
print(p14)
dev.off()





p11 <- (poc_plot[[1]] | poc_uncertainty[[1]]) +
  plot_annotation(title = "Not adjusted", tag_levels = list(c("a", "b"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))

p12 <- (poc_plot[[2]] | poc_uncertainty[[2]]) +
  plot_annotation(title = "Adjusted", tag_levels = list(c("c", "d"))) & 
  theme(plot.title = element_text(size=18, face="bold", hjust = 0.5), plot.tag = element_text(size = 14, face = "bold"))
  
p13 <- wrap_elements(p11) | wrap_elements(p12) + plot_layout(guides = "collect")



p11a <- (mpoc_plot[[1]] | mpoc_uncertainty[[1]]) +
  plot_annotation(tag_levels = list(c("e", "f"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

p12a <- (mpoc_plot[[2]] | mpoc_uncertainty[[2]]) +
  plot_annotation(tag_levels = list(c("g", "h"))) & 
  theme(plot.tag = element_text(size = 14, face = "bold"))

p13a <- wrap_elements(p11a) | wrap_elements(p12a) + plot_layout(guides = "collect")  

p15 <- (p13) / (p13a)


tiff("All_plots.tiff", units="cm", width=34, height=34, res=plot_res)
print(p15)
dev.off()



#Observed vs predicted by adjusted vs not-adjusted scatter
obs_pred_df1 <- data.frame(observed_response, predicted_response[[1]], rep("Not adjusted", times=length(predicted_response[[1]])))
#obs_pred_df1 <- data.frame(observed_response, predicted_response[[1]], rep("Not adjusted", times=length(predicted_response[[1]])))
names(obs_pred_df1) <- c("obs", "pred", "factor")
obs_pred_df2 <- data.frame(observed_response, predicted_response[[2]], rep("Adjusted", times=length(predicted_response[[2]])))
#obs_pred_df2 <- data.frame(observed_response, predicted_response[[2]], rep("Adjusted", times=length(predicted_response[[2]])))
names(obs_pred_df2) <- c("obs", "pred", "factor")
obs_pred_df <- data.frame(rbind(obs_pred_df1, obs_pred_df2))
obs_pred_df$factor <- as.factor(obs_pred_df$factor)

jpeg("predicted_vs_observed.jpeg", units = "cm", width = 18, height = 18, res = plot_res)
print(ggplot(obs_pred_df, aes(x=obs, y=pred, col=factor)) + 
	geom_point(alpha=0.3) +
	geom_smooth(method="lm") +	
	xlab("Observed OC content (%)") + ylab("Predicted OC content (%)") + 
	theme_bw() + 
	theme(legend.position="bottom") +
	theme(legend.title=element_blank())
	)
dev.off()


raster_vals <- list()
raster_vals[[1]] <- data.frame(rep("Not_adjusted", times=length(predicted_raster_vals[[1]]$layer)), predicted_raster_vals[[1]]$layer)
names(raster_vals[[1]]) <- c("factor", "values")
raster_vals[[2]] <- data.frame(rep("Adjusted", times=length(predicted_raster_vals[[2]]$layer)), predicted_raster_vals[[2]]$layer)
names(raster_vals[[2]]) <- c("factor", "values")

jpeg("OC_prediction_PDF.jpeg", units = "cm", width = 18, height = 18, res = plot_res)
print(ggplot() + 
	geom_density(aes(values, fill="Not adjusted"), alpha=0.2, data=raster_vals[[1]]) + 
	geom_density(aes(values, fill="Adjusted"), alpha=0.2, data=raster_vals[[2]]) + 	
	scale_fill_manual(name="", values=c("Not adjusted"="#00BFC4", "Adjusted"="#F8766D")) + 
	xlab("Predicted OC content (%)") + ylab("Density") + 
	theme_bw() + 
	theme(legend.position="bottom"))
dev.off()



########################################################################################


#Display raster using ggplot
poc_delta <- oc_content_ras[[2]] - oc_content_ras[[1]]
r_df <- as.data.frame(poc_delta, xy = T)
names(r_df) <- c("x", "y", "layer")


poc_delta_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) +
	scale_fill_distiller(palette="Spectral", direction=1, limits=c(-1,1), breaks=c(-1, 0, 1), na.value="white") +
	xlab("") + ylab("") + labs(fill=expression(Delta*"OC content (%)")) + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))

tiff("POC_delta.tiff", units="cm", width=14, height=14, res=plot_res)
print(poc_delta_plot)
dev.off()

#Display raster using ggplot
mpoc_delta <- mpoc_ras[[2]] - mpoc_ras[[1]] 
r_df <- as.data.frame(mpoc_delta, xy = T)
names(r_df) <- c("x", "y", "layer")


mpoc_delta_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) +
	scale_fill_distiller(palette="RdYlBu", direction=1, limits=c(-2,1), breaks=c(-2, -0.5, 1), na.value="white") +
	xlab("") + ylab("") + labs(fill=expression(Delta*"OC stock (kg" ~m^-2~ ")")) + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))

tiff("MPOC_delta.tiff", units="cm", width=14, height=14, res=plot_res)
print(mpoc_delta_plot)
dev.off()



#Display raster using ggplot
bd_delta <- BD[[2]] - BD[[1]]
r_df <- as.data.frame(bd_delta, xy = T)
names(r_df) <- c("x", "y", "layer")

upr_bd <- ceiling(max(r_df$layer, na.rm=T) / 100) * 100
lwr_bd <- floor(min(r_df$layer, na.rm=T) / 100) * 100
mid_bd <- (abs(upr_bd) - abs(lwr_bd)) / 2


bd_delta_plot <- ggplot() + geom_tile(data=r_df, aes(x=x, y=y, fill=layer)) + 
	theme(axis.title = element_blank()) +
	scale_fill_distiller(palette="RdYlBu", direction=1, limits=c(lwr_bd, upr_bd), breaks=c(lwr_bd, mid_bd, upr_bd), na.value="white") +
	xlab("") + ylab("") + labs(fill=expression(Delta*"DBD (kg" ~m^-3~ ")")) + 
	theme_bw(base_size=5) +
	theme(legend.key.width=unit(plot_legend_width, "cm")) +
	theme(legend.position = "bottom",
		axis.line = element_blank(),
    	panel.grid.major = element_blank(),
    	panel.grid.minor = element_blank(),
    	panel.border = element_blank(),
    	panel.background = element_blank(),
    	plot.margin=unit(c(0.25,-0.15,-0.05,-0.15), "cm")
        ) + geom_sf(data=GB_and_I, fill="#dadada", inherit.aes=FALSE) + 
		coord_sf(xlim = c(range(r_df$x)), ylim = c(range(r_df$y)))

#tiff("BD_delta.tiff", units="cm", width=14, height=14, res=plot_res)
#print(bd_delta_plot)
#dev.off()




p11 <- (poc_plot[[2]] | poc_uncertainty[[2]] | poc_delta_plot) +
  plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)"))) & 
  theme(plot.title = element_text(size=6.5, face="bold", hjust = 0.5), plot.tag = element_text(size = 6.5, face = "bold"))

tiff("POC_plots_w_DELTA.tiff", units="cm", width=11, height=8, res=plot_res)
print(p11)
dev.off()


p11 <- (mpoc_plot[[2]] | mpoc_uncertainty[[2]] | mpoc_delta_plot) +
  plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)"))) & 
  theme(plot.title = element_text(size=6.5, face="bold", hjust = 0.5), plot.tag = element_text(size = 6.5, face = "bold"))

tiff("MPOC_plots_w_DELTA.tiff", units="cm", width=11, height=8, res=plot_res)
print(p11)
dev.off()


p11 <- (bulk_density[[2]] | dbd_uncert_plot | bd_delta_plot) +
  plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)"))) & 
  theme(plot.title = element_text(size=6.5, face="bold", hjust = 0.5), plot.tag = element_text(size = 6.5, face = "bold"))

tiff("BD_plots_w_DELTA.tiff", units="cm", width=11, height=8, res=plot_res)
print(p11)
dev.off()





########################################################################################

nba_oc <- rast("~/UCD/Legacy_data_paper/model_training/not_adjusted/pred_ras.tif")
ba_oc <- rast("~/UCD/Legacy_data_paper/model_training/all_adjusted/pred_ras.tif")


none <- sum(values((nba_oc/100) * BD[[1]] * depth * cell_area), na.rm=T) / 1000000000	
bd_adj <- sum(values((nba_oc/100) * BD[[2]] * depth * cell_area), na.rm=T) / 1000000000	
poc_adj <- sum(values((ba_oc/100) * BD[[1]] * depth * cell_area), na.rm=T) / 1000000000	
all <- sum(values((ba_oc/100) * BD[[2]] * depth * cell_area), na.rm=T) / 1000000000	

none	#Nothing adjusted
bd_adj	#BD adjusted
poc_adj	#OC content adjusted
all		#All adjusted


metrics

round((none - bd_adj) / ((none - bd_adj) + (none - poc_adj)) * 100, 1)	#Percent of total difference due to BD adjustment
round((none - poc_adj) / ((none - bd_adj) + (none - poc_adj)) * 100, 1)	#Percent of total difference due to POC adjustment
round((none - all) / none * 100, 2) #Percent increase in total stock estimate using unadjusted predictors and DBD

var_imp



mean(values(inference[[1]]$mud_percent), na.rm=T) - mean(values(inference[[2]]$mud_percent), na.rm=T)

mean(values(inference[[1]]$sand_percent), na.rm=T) - mean(values(inference[[2]]$sand_percent), na.rm=T)

mean(values(inference[[1]]$gravel_percent), na.rm=T) - mean(values(inference[[2]]$gravel_percent), na.rm=T)


mean(values(BD[[1]]), na.rm=T) - mean(values(BD[[2]]), na.rm=T)

sd(values(BD[[1]]), na.rm=T)
sd(values(BD[[2]]), na.rm=T)

range(values(BD[[1]]), na.rm=T)
range(values(BD[[2]]), na.rm=T)


par(mfrow = c(1, 2))
mudbelt_dbd_unadj <- mask(BD[[1]], mudbelt)
mudbelt_dbd_unadj <- crop(mudbelt_dbd_unadj, mudbelt)
plot(mudbelt_dbd_unadj)

mudbelt_dbd_adj <- mask(BD[[2]], mudbelt)
mudbelt_dbd_adj <- crop(mudbelt_dbd_adj, mudbelt)
plot(mudbelt_dbd_adj)
par(mfrow = c(1, 1))

mean(values(mudbelt_dbd_unadj), na.rm=T) - mean(values(mudbelt_dbd_adj), na.rm=T)

########################################################################################

		#		MUD to POROSITY USGS RELATIONSHIP		#
		
########################################################################################


# Load required packages
library(sf)
library(dplyr)

# Define the base directory
base_dir <- "~/UCD/datasets"

# List of subdirectories
subdirs <- c("gmx_prs", "gmx_fac", "gmx_ext", "gmx_cmp", "gmx_clc")

# Initialize an empty list to collect cleaned data
mud_porosity_list <- list()

# Loop through each subdirectory
for (subdir in subdirs) {
  shapefile_path <- file.path(base_dir, subdir, paste0(subdir, ".shp"))
  
  # Read shapefile
  shp_data <- st_read(shapefile_path, quiet = TRUE)
  
  # Check for required columns
  if (all(c("MUD", "POROSITY") %in% colnames(shp_data))) { 
    # Extract and clean the data
    cleaned_data <- shp_data %>%
      st_drop_geometry() %>%
      dplyr::select(MUD, POROSITY) %>%
      filter(MUD >= 0, POROSITY >= 0)
    
    # Store the cleaned data
    mud_porosity_list[[subdir]] <- cleaned_data
    
    cat("Extracted and cleaned MUD and POROSITY from:", subdir, "\n")
  } else {
    cat("Skipping", subdir, "- missing MUD and/or POROSITY columns\n")
  }
}

# Combine all into one cleaned data frame
mud_porosity_df <- bind_rows(mud_porosity_list)

# Scale MUD and POROSITY to range from 0 to 1
mud_porosity_df <- mud_porosity_df %>%
  mutate(
    MUD = MUD / 100,
    POROSITY = POROSITY / 100
  )

# Print the result
print(head(mud_porosity_df))


# Load required libraries
library(terra)
library(ggplot2)
library(dplyr)

# --- STEP 1: Fit the model and plot scatter with confidence interval ---
lm_fit <- lm(POROSITY ~ MUD, data = mud_porosity_df)

# R-squared label for plot
r_squared <- summary(lm_fit)$r.squared
r_squared_label <- paste0("R² = ", round(r_squared, 3))

# Scatter plot of POROSITY vs MUD
ggplot(mud_porosity_df, aes(x = MUD, y = POROSITY)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  annotate("text", x = Inf, y = -Inf, label = r_squared_label,
           hjust = 1.1, vjust = -1.1, size = 5, color = "black") +
  labs(
    title = "Relationship between MUD and POROSITY",
    x = "MUD",
    y = "POROSITY"
  ) +
  theme_minimal()

# --- STEP 2: 1D prediction with 95% prediction intervals ---
mud_seq <- seq(min(mud_porosity_df$MUD), max(mud_porosity_df$MUD), length.out = 100)
porosity_preds <- predict(lm_fit, newdata = data.frame(MUD = mud_seq), interval = "prediction", level = 0.99)

porosity_bounds_df <- data.frame(
  MUD = mud_seq,
  porosity_fit = porosity_preds[, "fit"],
  porosity_lower = porosity_preds[, "lwr"],
  porosity_upper = porosity_preds[, "upr"]
) %>%
  mutate(
    dbd_lower  = (1 - porosity_upper) * 2580,
    dbd_upper  = (1 - porosity_lower) * 3100,
    dbd_median = (1 - porosity_fit) * mean(c(2580, 3100))
  )

# Plot DBD bounds
ggplot(porosity_bounds_df, aes(x = MUD, y = dbd_median)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = dbd_lower, ymax = dbd_upper), alpha = 0.3, fill = "blue") +
  labs(
    title = "Dry Bulk Density with Propagated Uncertainty",
    x = "MUD (0–1)",
    y = "Dry Bulk Density (kg/m³)"
  ) +
  theme_minimal()

# --- STEP 3: Raster-based predictions with total uncertainty ---

# Extract coefficients
intercept <- coef(lm_fit)[1]
slope     <- coef(lm_fit)[2]

# Extract SE of fit using model matrix and vcov
mud_vals <- values(mud)
mud_df   <- data.frame(Predicted_Mud_Fraction = mud_vals)
X        <- model.matrix(~ Predicted_Mud_Fraction, data = mud_df)
se_fit   <- sqrt(rowSums((X %*% vcov(lm_fit)) * X))

# Create SE raster (for mean prediction)
se_raster <- mud
values(se_raster) <- se_fit

# Get residual standard deviation (σ)
sigma <- summary(lm_fit)$sigma

# Compute total standard error (prediction uncertainty)
total_se_raster <- sqrt(se_raster^2 + sigma^2)

# Predict porosity (median and 95% prediction interval)
porosity_median <- intercept + slope * mud
porosity_lower  <- porosity_median - 1.96 * total_se_raster
porosity_upper  <- porosity_median + 1.96 * total_se_raster

# Clamp to physical limits
porosity_median <- clamp(porosity_median, lower = 0, upper = 1)
porosity_lower  <- clamp(porosity_lower,  lower = 0, upper = 1)
porosity_upper  <- clamp(porosity_upper,  lower = 0, upper = 1)


# --- STEP 4: Convert porosity to dry bulk density with propagated uncertainty ---

grain_density_min  <- 2410  # kg/m³
grain_density_max  <- 2700  # kg/m³
grain_density_mean <- mean(c(grain_density_min, grain_density_max))

dbd_median <- (1 - porosity_median) * grain_density_mean
dbd_lower  <- (1 - porosity_upper)  * grain_density_min  # lower = max porosity
dbd_upper  <- (1 - porosity_lower)  * grain_density_max  # upper = min porosity

# Name layers
names(dbd_median) <- "dbd_median"
names(dbd_lower)  <- "dbd_lower"
names(dbd_upper)  <- "dbd_upper"

# Stack the output
dbd_stack <- c(dbd_median, dbd_lower, dbd_upper)



# Plot or save
# plot(dbd_stack)
# writeRaster(dbd_stack, "dbd_stack.tif", overwrite = TRUE)







selected_model$resample


# View per-fold results
head(selected_model$resample)

# Calculate standard deviation
sd_r2  <- sd(selected_model$resample$Rsquared)
sd_rmse <- sd(selected_model$resample$RMSE)
sd_mae  <- sd(selected_model$resample$MAE)

# Print stability metrics
cat("SD of R²:", round(sd_r2, 4), "\n")
cat("SD of RMSE:", round(sd_rmse, 4), "\n")
cat("SD of MAE:", round(sd_mae, 4), "\n")




########################################################################################

		#		END!!!!!		#
		
########################################################################################



# 1. Select only the predictors used in the selected_model
selected_vars <- c("bathymetry", "mud_percent", "productivity", "coast_dist", "salinity")
X <- train_df_i[, selected_vars]

# 2. Define response variable
y <- response

# 3. Run opt_prediction to evaluate prediction stability over a range of tree counts
set.seed(123)  # for reproducibility
opt_pred_result <- opt_prediction(
  y = y,
  X = X,
  number_repetitions = 10,                         # How many times to rerun RF for stability
  num.trees_values = c(250, 500, 750, 1000, 2000),  # You can adjust this range
  alpha = 0.15,                                     # Top 15% predictions to assess
  select_for = "high",                             # You can also try "low" or "zero"
  visualisation = "prediction",
  recommendation = "prediction",
  round_recommendation = "hundred",
  verbose = TRUE
)

# Plot prediction stability curve and highlight recommended number of trees
plot_stability(
  opt_pred_result,
  measure = "prediction",
  add_recommendation = TRUE
)


# Estimate how many trees are needed for a specific stability threshold (e.g., 95%)
estimate_numtrees(opt_pred_result, measure = "prediction", for_stability = 0.95)



# 3. Run opt_prediction to evaluate prediction stability over a range of tree counts
set.seed(123)  # for reproducibility
opt_pred_result <- opt_prediction(
  y = y,
  X = X,
  number_repetitions = 10,                         # How many times to rerun RF for stability
  num.trees_values = c(250, 500, 750, 1000, 2000),  # You can adjust this range
  alpha = 0.15,                                     # Top 15% predictions to assess
  select_for = "low",                             # You can also try "low" or "zero"
  visualisation = "prediction",
  recommendation = "prediction",
  round_recommendation = "hundred",
  verbose = TRUE
)

# Check stability for low-predicted individuals
estimate_numtrees(opt_pred_result, measure = "prediction", for_stability = 0.95)





#install.packages(c("magick", "tiff"))
library(magick)  # For image manipulation and composition
library(tiff)    # Optional: to inspect or process TIFFs as arrays

# ---- STEP 1: Load TIFF images ----
img_paths <- c("~/UCD/Legacy_data_paper/model_training/all_adjusted/AOA_raw.tiff",
				"~/UCD/Legacy_data_paper/model_training/all_adjusted/AOA_threshold.tiff",
				"~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/AOA.tiff",
				"~/UCD/Legacy_data_paper/R_objects/dry_bulk_density/AOA_threshold.tiff")
imgs <- lapply(img_paths, image_read)

# ---- STEP 2: Resize to uniform size (optional but recommended) ----
imgs <- lapply(imgs, function(x) image_resize(x, "1000x1000!"))

# ---- STEP 3: Add letter labels (e.g., (a), (b), ...) ----
labels <- c("(a)", "(b)", "(c)", "(d)")
imgs_labeled <- mapply(function(img, label) {
  image_annotate(img, label,
                 size = 60,
                 gravity = "northwest",
                 location = "+30+30",
                 color = "black",
                 font = "Helvetica-Bold")
}, imgs, labels, SIMPLIFY = FALSE)

# ---- STEP 4: Combine into panel (2x2) ----
top_row <- image_append(c(imgs_labeled[[1]], imgs_labeled[[2]]))
bottom_row <- image_append(c(imgs_labeled[[3]], imgs_labeled[[4]]))
panel <- image_append(c(top_row, bottom_row), stack = TRUE)

# ---- STEP 5: Save output TIFF at 300 DPI ----
image_write(panel, path = "panel_output.tiff", format = "tiff", density = "300x300")



