library(raster)
library(rgdal)
library(ggplot2)
library(GGally)
library(sp)
setwd("~/owncloud/7-aplicaciones")
#camino <- "imagenes/indices/20171116T141930-indices.tif"

# Defino que imagenes abrir de los indices
#files <- list.files(path = camino, pattern = '.tif$', full.names = TRUE)
#indices <- stack(files)
# Abro la imagen cosmo
indices <- stack("campoCONAE/imagenes/indices/20171116T141930-indices_reprojected.tif")
names(indices) <- c("arvi", "dvi", "gemi", "gndvi", "ipvi", "ireci", "mcari", "msavi2", "msavi", "mtci", "ndi45", "pssra", "pvi", "reip", "rvi", "s2rep", "savi", "tndvi", "tsavi", "wdvi", "NDVI")


#plot(values(indices$arvi), values(indices$gemi))
#cor(values(indices$arvi), values(indices$gemi))


# Apilo todo

# Cargo el vector para hacer el muestreo
poligonoextraccion<- readOGR(dsn="campoCONAE", layer="puntos-buffer-maiz")
poligonoextraccionmaiz <- extract(indices, poligonoextraccion, fun=mean, df=TRUE)

maiz <- readOGR(dsn="campoCONAE", layer="maiz2016")

extraccionpol <- over(poligonoextraccion, maiz, df=TRUE)
names(extraccionpol) <- c("var1", "var2", "var3")
extraccionpol$ID <- seq.int(nrow(extraccionpol))
pepe <- merge(extraccionpol, poligonoextraccionmaiz)

write.csv(extraccionpol, "campoCONAE/extraccionpol.csv")
write.csv(poligonoextraccionmaiz, "campoCONAE/poligonoextraccionmaiz.csv")


# Extraigo los datos, el promedio y el desvio
datos <- extract(indices, extraccionpol, df=TRUE)
datos <- datos[-1]
datos <- datos[-4]
names(datos) <- c("lai", "mndwi", "ndwi2", "ndwi",  "rvi")

mean.df <- extract(apilado, poligonoextraccion, df=TRUE, fun=mean)
sd.df <- extract(apilado, poligonoextraccion, df=TRUE, fun=sd)

dataframemediasd <- as.data.frame(cbind(mean.df, sd.df))
dataframemediasd <- dataframemediasd[-5]
dataframemediasd <- dataframemediasd[-1]
dataframemediasd <- dataframemediasd[-6]
dataframemediasd <- dataframemediasd[-9]
names(dataframemediasd) <- c("meanlai", "meanmndwi", "meanndwi2", "meanndwi",  "meanrvi", "sdlai", "sdmndwi", "sdndwi2", "sdndwi",  "sdrvi" )

# Calculo la correlacion
#cor <- rcorr(as.matrix(mean.df), type="spearman")
write.csv(dataframemediasd, "medias-sdlotes.csv")
#subset x LAI
cor(datos[datos$X20160323.lai < 0.7,])
cor(mean.df)

datoslaialto <- (mean.df[mean.df$X20160323.lai > 0.5,])
datoslaibajo<- (mean.df[mean.df$X20160323.lai < 0.5,])



#RVI medias total
ndwi<-dataframemediasd$meanndwi
rvi<-dataframemediasd$meanrvi
sd<-dataframemediasd$sdrvi
qplot(ndwi,rvi)#+geom_errorbar(aes(ndwi=ndwi, ymin=rvi-sd, ymax=rvi+sd), width=0.05)
qplot(ndwi,rvi)#+geom_errorbar(aes(ndwi=ndwi, ymin=rvi-sd, ymax=rvi+sd), width=0.05)
#plotlai <- ggplot(datostotales, aes(x = datos$rvi, y = datos$lai)) + geom_point() + ggtitle("NDVI vs RVI") +labs(x = "NDVI")+labs(y = "RVI")+ 





