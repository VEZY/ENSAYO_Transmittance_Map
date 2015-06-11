#######################################################################################################################
####---------------------------------------------Paramétrage fichier point.dat-------------------------------------####
#######################################################################################################################

# Sert ? simuler la transmittance sur le plot sur chauque point donn?.


# Import --------------------------------------------------------------------------------------

setwd("F:/These/Simulations_Maespa/3-Simulations_ENSAYO/Arbres_only_lumiere/MAESPA_V7")
system.time(library(Maeswrap))
source("F:/These/Simulations_Maespa/00-SCRIPTS/Fonctions/Maeswrap_cor_VR.R")
Coord_All_Coffees= read.table("F:/These/Ensayo_CATIE/Data/Coord_All_coffes.csv", h=T, sep=";",
                              numerals = "no.loss")
str(Coord_All_Coffees)



# Creation du fichier de points ---------------------------------------------------------------

Z_position= rep(2, nrow(Coord_All_Coffees))
minY= 1094942.737 #Y minimum du plot entr? dans MAESPA. Cr?e un nouveau rep?re spatial.
minX= 207104.504

Table_points= as.data.frame(cbind(X_position=round((Coord_All_Coffees$X-minX),3), 
                    Y_position=round((Coord_All_Coffees$Y-minY),3), 
                    Z_position))
# replacePAR(datfile = "points.dat", namelist = "CONTROL", parname = "NOPOINTS", newval = nrow(Coord_All_Coffees))
# replacePAR(datfile = "points.dat", namelist = "XYZ", parname = "COORDS", newval = Table_points)




NPoints= nrow(Coord_All_Coffees)
replacePAR_VR(datfile="confile.dat", parname = "startdate", namelist = "dates", "01/01/15")
replacePAR_VR(datfile="confile.dat", parname = "enddate", namelist = "dates", "01/01/15")
replacePAR_VR(datfile = "points.dat", namelist = "CONTROL", parname = "NOPOINTS", newval = NPoints)
replacePAR_VR(datfile = "points.dat", namelist = "XYZ", parname = "COORDS", newval = Table_points) 
system('MAESPA_Remko_VR', show.output.on.console=T)
Results= readtestflx("testflx.dat")

# Problem: At the begining, MAESPA couldn't simulate more test points than there is trees in the plot.
# The easiest and fastest way to adress it is to divide the test points into subsets.
# Each subset will be at least the length of the tree numbers.
# Nsubset= ceiling(NPoints/NTrees)                        # round the number of subsets to upper integer
# Fullsubsets= rep(1:(Nsubset-1), each=1266)              # Attribute a subset number to the full subsets
# LastSubset= rep(Nsubset,(NPoints-length(Fullsubsets)))  # Attributes the last subset number to the last points
# Subset_ID= c(Fullsubsets, LastSubset)  
# Resultats= c()
# system.time(
# for (i in 1:Nsubset){
#     Subset_COORDS= Table_points[Subset_ID==i,]
#     replacePAR_VR(datfile = "points.dat", namelist = "CONTROL", parname = "NOPOINTS", newval = nrow(Subset_COORDS))
#     replacePAR_VR(datfile = "points.dat", namelist = "XYZ", parname = "COORDS", newval = Subset_COORDS) 
#     system('MAESPA_Remko_VR', show.output.on.console=T)
#     testflux=readtestflx("testflx.dat")
#     if (i == 1){ Resultats <- testflux
#     }else {Resultats <- rbind(Resultats,testflux)   
#     }
#     print(c("Subset N?",i))
# }
# )
# Filepath= "F:/These/Simulations_Maespa/3-Simulations_ENSAYO/Arbres_only_lumiere/MAESPA_V7/TESTFLUX_semaine.csv"
# # write.table(Resultats, Filepath, sep=";", row.names=F)
# Results= read.table(Filepath, sep=";", h=T)
#ATTENTION: La colonne PT est fausse dans la boucle.


# Prepares the results ---------------------------------------------------------------------

library(raster)
#Rajouter les Y et X min de la parcelle en coordonn?es WGS 84 (cf Parametrage_Trees_ENSAYO_OR2 l.281)
Results$X= Results$X + minX
Results$Y= Results$Y + minY
# Defines the system projection:
crsplot="+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
#ref: http://spatialreference.org/ref/epsg/wgs-84-utm-zone-17n/proj4/
# Create an empty raster:
Raster_plot= raster(nrows=350, ncols=518, xmn=minX, xmx=(518+minX), ymn=minY, ymx=(350+minY), 
                   crs=crsplot)



# Make a mean table by point (mean of the days and the hours) ---------------------------------

Results_jourmoy= aggregate(Results[,-c(1:3,6)], list(Results[,3]), mean)
str(Results_jourmoy)


# Create a raster layer and plot it -----------------------------------------------------------

# nrows: on a 349.431 m en axe des X. Pour avoir une carte ? la r?solution des 10 cm, on prends un 
# pixel = 10 cm, donc il nous faudra 3494 pixels pour couvrir l'axe des Y ? cette r?solution.
# ncol: idem
temp_Results_APAR= cbind(x=Results_jourmoy$X, y=Results_jourmoy$Y, z=Results_jourmoy$APAR)
APAR_plot=Raster_plot
cell1 <- cellFromXY(APAR_plot, temp_Results_APAR[,1:2])
# finds the corresponding coordinates of temp_Results in APAR_plot
APAR_plot[cell1] <- temp_Results_APAR[,3]
# writes the PAR values in the raster
plot(APAR_plot)
RasterPath="F:/These/Simulations_Maespa/3-Simulations_ENSAYO/Arbres_only_lumiere/Resultats_Transmittance"
# writeRaster(APAR_plot, paste(RasterPath, "/APAR_plot.asc", sep=""), format="ascii", overwrite=T)
APAR_plot=raster(paste(RasterPath, "/APAR_plot.asc", sep=""))

library(rgdal)
TREES <- readOGR(dsn="F:/These/Ensayo_CATIE/Mappage", layer="Trees_B3_MS")
par(mfrow=c(1,1))
plot(TREES, xlim=c(78,79), ylim=c(8,22), axes=TRUE)
plot(APAR_plot, col=rainbow(100, start=0, end=1))
# , main=expression(Incident~PAR~(?mol.m[-2].s[-1]),add=T)










temp_Results_TTOT= cbind(x=Results_jourmoy$X, y=Results_jourmoy$Y, z=Results_jourmoy$TTOT)
Total_rad_plot=Raster_plot
cell2 <- cellFromXY(Total_rad_plot, temp_Results_TTOT[,1:2])
# finds the corresponding coordinates of temp_Results in PAR_plot
Total_rad_plot[cell2] <- temp_Results_TTOT[,3]
# writes the PAR values in the raster
plot(Total_rad_plot, col=rainbow(100, start=0, end=1))
# writeRaster(Total_rad_plot, paste(RasterPath, "/Total_rad_plot.asc", sep=""), format="ascii", overwrite=T)
# Total_rad_plot=raster(paste(RasterPath, "/Total_rad_plot.asc", sep=""))
head(Results)

temp_Results_APAR= cbind(x=Results_jourmoy$X, y=Results_jourmoy$Y, z=Results_jourmoy$APAR)
Incident_PAR_plot=Raster_plot
cell2 <- cellFromXY(Incident_PAR_plot, temp_Results_APAR[,1:2])
# finds the corresponding coordinates of temp_Results in PAR_plot
Incident_PAR_plot[cell2] <- temp_Results_APAR[,3]
# writes the PAR values in the raster
plot(Incident_PAR_plot, col=rainbow(100, start=0, end=1))
# writeRaster(Incident_PAR_plot, paste(RasterPath, "/Incident_PAR_plot.asc", sep=""), format="ascii", overwrite=T)
# Incident_PAR_plot=raster(paste(RasterPath, "/Incident_PAR_plot.asc", sep=""))



x11()
length(breakpoints)
breakpoints <- seq(0, 450, 30)
# colors <- rainbow(20, start=0, end=1)
colors= terrain.colors(20)
plot(Total_rad_plot,breaks=breakpoints,col=colors, main='TestFlux TTOT (umol m-2 s-1)')
plot(APAR_plot,breaks=breakpoints,col=colors, main='TestFlux APAR (umol m-2 s-1)')
