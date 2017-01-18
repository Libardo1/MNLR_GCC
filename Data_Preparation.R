# Reading in the Landsat7 Data (this file includes the response variable too)

# Subsetting to the ground truthed observations

# Read in Landsat7.csv
                                        
LS.data <- read.csv(file = '~/unwgbdos/Data/ABS_Data/Landsat7.csv') # occupies ~2.5Gb RAM

# These data were accompanied by a file of explanatory notes: `Explanatory_Notex.docx' from which the following contextual information is drawn

# These data contain columns:

# crop:       Crop type that was observed in the shp.month month and the shp.year year. (this is the response)

# farm.id:    Identifier for fields.

# shp.year:   Year that the ground-truth observation was made.

# shp.month:  Month that the ground-truth observation was made.

# img.year:   Year that the satellite image was collected.

# img.month:  Month that the satellite image was collected.

# img.day:    Day that the satellite image was collected.

# `An overall unique field identifier can be created by concatenating farm.id, crop, shp.year and shp.month'
# creating the unique filed identifier `field.id'

library('dplyr')

nrow(LS.data) # 10092471

colnames(LS.data)

# create new column `field.id'

LS.data <- mutate(.data = LS.data, field.id = paste('farm',farm.id, crop, shp.year, shp.month, sep = '.'))

length(unique(LS.data$field.id)) # 961 unique grond truth observations with multiple Landsat pixels per observation?

# the rows in the full data sets LS.data correspond to the time series all landsat pixels from when a particular crop was sown to when it was harvested
# this will be great for functional data analysis based approaches where time series of vegetation indices are smoothed and the curve is used as a predictor of the crop type
# what we're going to do here though is simpler
# i'm going to attempt a pixel level classification via multinomial regression and as such will only use the ground truthed observations because this method matches a response observations with single covariates observations (rather than a time series of covariate observations) and as such we need to know the crop was present above the soil (ie. not just sown) i.e. spectral info matches actual crop type in order for this approach to have some chance of success


# subset to rows that contained the ground truthed observations
# ie rows where image year = shape year & image month = shape month - 

LS.data.ies <- filter(.data = LS.data, img.year == shp.year & img.month == shp.month) # ies = image = shape

nrow(LS.data.ies) # 247210

nrow(LS.data)/nrow(LS.data.ies)

length(unique(LS.data.ies$field.id)) # 740, so we have multiple Landsat pixels per ground truthed observation? - i guess this makes sense as crop paddocks could easily contain multiple 50m x 50m pixels

# Distribution of Reponse

library('ggplot2')

LS.data.ies.gbc <- group_by(.data = LS.data.ies, crop)

crop.ies.n <- summarise(.data = LS.data.ies.gbc, count = n())

crop.ies.n

crop.ies.n.df <- data.frame(crop.ies.n)

crop.ies.n.df.a <- arrange(.data = crop.ies.n.df, count)

crop.ies.n.df.a$row.num <- nrow(crop.ies.n.df.a):1

crop.p <- ggplot(aes(x = row.num, y = count), data = crop.ies.n.df.a)

crop.p <- crop.p + geom_segment(aes(xend = row.num), yend = 0, size = 5) + scale_x_continuous(breaks = crop.ies.n.df.a$row.num, labels = crop.ies.n.df.a$crop) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), panel.background = element_rect(fill = 'white'), panel.grid.major = element_line(colour = 'grey'), axis.ticks = element_line(colour = 'grey')) + labs(x = 'crop', title = paste('Ground Truthed Pixels (img = shp), n = ', sum(crop.ies.n.df.a$count))) + scale_y_continuous(breaks = seq(from = 0, to = max(crop.ies.n.df.a$count), by = 1e4))

crop.p

# we may want to combine/exclude some of those rare categories?

#

library(xtable)
                                                                                                                     xtable(crop.ies.n.df.a[,c(1,2)], caption = paste('Ground Truthed Pixels (img = shp), n = ', sum(crop.ies.n.df.a$count)))

crop.ies.n.df.a

# Calculate a selection of Vegetation Indices:

# ok let's just do some by hand and exclude any that have constants I don't know how to / dont have data to calculate:

# DVI <- s * nir - red

# EVI <- G * ((nir - red)/(nir + C1 * red - C2 * blue + L_evi))

# EVI2 <- G * (nir - red)/(nir + 2.4 * red + 1)

#NDVIC <- (nir - red)/(nir + red) * (1 - ((swir2 - swir2ccc)/(swir2coc - swir2ccc)))

# WDVI <- nir - s * red

#  SATVI = (swir1 - red)/(swir1 + red + L) * (1 + L) - (swir2/2),

#  SAVI = (nir - red) * (1 + L)/(nir + red + L)

#   NBRI = (nir - swir2)/(nir + swir2)

#   SLAVI = nir/(red + swir2)


# double check i've transcribed these formulae correctly

# I'm taking values for G, C1 and C2 and L_evi from two recent papers that calculate EVI from Landsat 7 data
# same with L = 0.5 for SAVI

# She, X., Zhang, L., Cen, Y., Wu, T., Huang, C., & Baig, M. H. A. (2015). Comparison of the continuity of vegetation indices derived from Landsat 8 OLI and Landsat 7 ETM+ data among different vegetation types. Remote Sensing, 7(10), 13485–13506. http://doi.org/10.3390/rs71013485

# Ahmadian, N., Ghasemi, S., Wigneron, J.-P., & Zölitz, R. (2016). Comprehensive study of the biophysical parameters of agricultural crops based on assessing Landsat 8 OLI and Landsat 7 ETM+ vegetation indices. GIScience & Remote Sensing, 1603(April), 1–23. http://doi.org/10.1080/15481603.2016.1155789

# EVI <- G * ((nir - red)/(nir + C1 * red - C2 * blue + L_evi))

# EVI <- 2.5 * ((nir - red)/(nir + 6 * red - 7.5 * blue + 1))

LS.data.ies.naom <- na.omit(LS.data.ies)

LS.VI <- mutate(.data = LS.data.ies.naom,
  blue = band1,
  green = band2,
  red = band3,
  nir = band4,
  swir1 = band5,
  tir1 = band6, 
  CTVI = (nir - red)/(nir + red) + 0.5,
  EVI = 2.5 * ((nir - red)/(nir + 6 * red - 7.5 * blue + 1)),
  GEMI = (((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5))/(nir + red + 0.5)) * (1 - ((((nir^2 - red^2) * 2 + (nir * 1.5) + (red * 0.5))/(nir + red + 0.5)) * 0.25)) - ((red - 0.125)/(1 - red)),
  LSWI = (nir - swir1)/(nir + swir1),
  MNDWI =  (green - swir1)/(green + swir1),
  MSAVI = nir + 0.5 - (0.5 * sqrt((2 * nir + 1)^2 - 8 * (nir - (2 * red)))),
  MSAVI2 = (2 * (nir + 1) - sqrt((2 * nir + 1)^2 - 8 * (nir - red)))/2,
  NDVI =  (nir - red)/(nir + red),
  NDWI = (green - nir)/(green + nir),
  NRVI = (red/nir - 1)/(red/nir + 1),
  RVI = red/nir,
  SR = nir/red,
  SAVI = (nir - red) * (1 + 0.5)/(nir + red + 0.5),
  TVI = sqrt((nir - red)/(nir + red) + 0.5),
  TTVI = sqrt(abs((nir - red)/(nir + red) + 0.5)))

# how about some tasseled cap can't with out band7 (swir2) which landsat 7 does record ABS just haven't supplied that info

colnames(LS.VI)

dim(LS.VI)

##

Data.VI <- select(.data = LS.VI, crop, band1, band2, band3, band4, band5, band6, CTVI, EVI, GEMI, LSWI, MNDWI, MSAVI, MSAVI2, NDVI, NDWI, NRVI, RVI, SR, SAVI, TVI, TTVI)

# Inf in EVI from divide by zero...

tail(sort((Data.VI$EVI)))

Data.VI[Data.VI$EVI== Inf,]

  blue = 1448
  red = 1401
  nir = 2453

(nir + 6 * red - 7.5 * blue + 1)

2.5 * ((nir - red)/(nir + 6 * red - 7.5 * blue + 1))

# ok let's drop EVI for now

Data.VI <- select(.data = LS.VI, crop, band1, band2, band3, band4, band5, band6, CTVI, GEMI, LSWI, MNDWI, MSAVI, MSAVI2, NDVI, NDWI, NRVI, RVI, SR, SAVI, TVI, TTVI)

# Create the design matrix:

VI <- select(.data = LS.VI, band1, band2, band3, band4, band5, band6, CTVI, GEMI, LSWI, MNDWI, MSAVI, MSAVI2, NDVI, NDWI, NRVI, RVI, SR, SAVI, TVI, TTVI)

# examine the collinearity among the columns of the design matrix:

library(tidyr)

VI.Cor <- cor(VI)

VI.Cor.df <- data.frame(C1 = rownames(VI.Cor), cor(VI))

VI.Cor.df %>%
  gather(data = ., key = C2, value = Cor, -C1) -> VI.Cor.df.G

Cor.Mat.p <- ggplot(aes(x = C1, y = C2, fill = Cor), data = VI.Cor.df.G) + geom_raster() + scale_fill_gradientn(colours = c('#af8dc3', '#f7f7f7', '#7fbf7b')) + coord_equal() + theme(axis.text.x = element_text(angle = 90)) + labs(x = '', y = '')

Cor.Mat.p

# drop a selection of columns to reduce the overall collinearity of the design matrix

library(caret)

findCorrelation(x = cor(VI), cutoff = 0.9, exact = TRUE, names = TRUE)

drop.these.colnums <- findCorrelation(x = cor(VI), cutoff = 0.9, exact = TRUE)

# filter design matrix accordingly:

VI.F <- select(.data = VI, -drop.these.colnums)


# check the resulting correlation matrix:

VI.F.Cor <- cor(VI.F)

VI.F.Cor.df <- data.frame(C1 = rownames(VI.F.Cor), VI.F.Cor)

VI.F.Cor.df %>%
  gather(data = ., key = C2, value = Cor, -C1) -> VI.F.Cor.df.G

F.Cor.Mat.p <- ggplot(aes(x = C1, y = C2, fill = Cor), data = VI.F.Cor.df.G) + geom_raster() + scale_fill_gradientn(colours = c('#af8dc3', '#f7f7f7', '#7fbf7b')) + coord_equal() + theme(axis.text.x = element_text(angle = 90)) + labs(x = '', y = '')

F.Cor.Mat.p

Test <- ggplot(aes(x = C1, y = C2, fill = abs(Cor) > 0.9), data = VI.F.Cor.df.G) + geom_raster() + coord_equal() + theme(axis.text.x = element_text(angle = 90)) + labs(x = '', y = '')

Test

Filtered.VIs <- VI.F

Data.F <- select(.data = LS.VI, one_of(c('crop',colnames(Filtered.VIs))))

summary(Data.F)

# look at categories with over 1000 observation per category... 
# we need to use cross validation to tune models so categories with 54 observations and other with 72 000 observations won't work well together (difficult to ensure training sets have adequate representation of rare categories)

Data.F %>%
  group_by(crop) %>%
    summarise(n()) %>%
      arrange(`n()`)

Data.F %>%
  group_by(crop) %>%
    summarise(n() > 1e3) %>%
      filter(`n() > 1000` == TRUE) %>%
        select(crop) -> common.crops

common.crops$crop

Data <- filter(.data = Data.F, crop %in% common.crops$crop)

Data %>%
  group_by(crop) %>%
    summarise(n()) %>%
      arrange(`n()`)


dim(Data) #  rows = 244753, cols = 9

colnames(Data)

#  "crop"  "band2" "band4" "band5" "band6" "LSWI"  "MNDWI" "NDWI"  "SR"   

summary(Data$crop)

# remove empty levels

Data$crop.c <- factor(as.character(Data$crop))

summary(as.character(Data$crop.c) == as.character(Data$crop))

Data <- select(.data = Data, -crop)

summary(Data$crop.c)

# RAM post 686Mb

summary(Data)

summary(Data$crop)

levels(Data$crop)

library(glmnet)

# clear the workspace of everything but Data
rm(list = ls()[!(ls() == 'Data')])
gc()
ls()

nrow(Data) # 244753

dim(Data)

save(list = 'Data', file = '~/MNLR_GCC/Data/Filtered_Data.RData')
