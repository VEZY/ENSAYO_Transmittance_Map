####################################################################################################
####------------------------------Simulation Transmittance points-------------------------------####
####################################################################################################

# Simpulation de la transmittance au dessus de chaque de café suivi, pour la journée à laquelle la 
# photo hémisphérique à été prise.


library(Maeswrap)
source("F:/These/Projects/R-Functions/Maeswrap_cor_VR.R")
Path_sensible_data= "F:/These/Projects_classified/ENSAYO_Transmittance_Map"
Table_measures_temp= read.table(paste(Path_sensible_data,"/DATA/Coresp.csv", sep=""),
                                h=T, sep=";", numerals = "no.loss")
Table_measures_temp$Parcela_PER= as.integer(Table_measures_temp$Parcela_PER)
Table_measures_temp$Manejo_PER= as.integer(Table_measures_temp$Manejo_PER)
Table_meas_temp= aggregate(Table_measures_temp, list(Table_measures_temp$ID_cafe_PER), mean)
# aggregate returns NA's (and warnings) for non numeric variables if not integers
Date= tapply(Table_measures_temp$TIMESTAMP, list(Table_measures_temp$ID_cafe_PER), unique)
Table_meas= Table_meas_temp[which(!is.na(Table_meas_temp$X_PER)),]
Table_Comparison= data.frame(NumPhoto=as.integer(Table_meas$Num_photo_PER) ,x=Table_meas$X_PER, 
                             y=Table_meas$Y_PER, TD_measure=Table_meas$DiffTransmittance,
                             Parcelle= Table_meas$Parcela_PER, Bloque= Table_meas$Bloque_PER,
                             Treatment= Table_meas$Manejo_PER)


# Run Maespa for each Day ---------------------------------------------------------------------


# Import des métadonnées des photos de tranmittance extraites avec ImageJ pour en extraire les dates 
# Voir macro F:/Logiciels/ImageJ/macros/Extract_EXIF.ijm ; 
# source: http://rsb.info.nih.gov/ij/macros/ExifDateAndTime.txt

confile= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/confile.dat", sep='')
points= paste(Path_sensible_data, "/MAESPA_routine/MAESPA_Remko_VR/points.dat", sep='')
PhotoNumber= na.omit(unique(Table_measures_temp$Num_photo_PER))
DatePhoto= rep(NA, length(PhotoNumber))
minY= 1094942.737 #Y minimum Y of the ENSAYO plot. Creates a new spatial reference.
minX= 207104.504

j=1
for (i in PhotoNumber){
    EXIF= readLines(paste(Path_sensible_data, "/DATA/Photos_Transmittance/DSCN0", i, ".txt", sep=""))
    DateLine= grep("Date/Time Original:", EXIF)
    DatePhoto[j]= strsplit(EXIF[DateLine], split="\t")[[1]][2]
    j= j+1
}
DatePhoto= format(as.Date(DatePhoto, format="%Y:%m:%d %H:%M:%S"), "%d/%m/%Y")
Table_Date_Photo= data.frame(DatePhoto, PhotoNumber)
DATES= unique(DatePhoto)
setwd(paste(Path_sensible_data,'/MAESPA_routine/MAESPA_Remko_VR', sep=''))
# system command works only on the working directory

for (i in 1:length(DATES)){
    
    replacePAR_VR(datfile=confile, parname = "startdate", namelist = "dates", DATES[i])
    replacePAR_VR(datfile=confile, parname = "enddate", namelist = "dates",  DATES[i])
    NumPhotoDATE_i= Table_Date_Photo[Table_Date_Photo$DatePhoto==DATES[i],2]
    X_Ind= round((Table_Comparison$x[match(NumPhotoDATE_i, Table_Comparison$NumPhoto)])- minX, 2)
    Y_Ind= round((Table_Comparison$y[match(NumPhotoDATE_i, Table_Comparison$NumPhoto)])- minY, 2)
    Z_Ind= rep(2, length(Y_Ind))
    Table_points= data.frame(X_Ind, Y_Ind, Z_Ind, NumPhotoDATE_i)
    
    if(!all(is.na(Table_points$X_Ind|Table_points$Y_Ind))){
    if(anyNA(Table_points)){ warning("one or more photograph(s) skipped due to NA's")}
        Table_points= na.omit(Table_points)
        replacePAR_VR(datfile = points, namelist = "CONTROL", parname = "NOPOINTS", 
                  newval = nrow(Table_points))
    replacePAR_VR(datfile = points, namelist = "XYZ", parname = "COORDS", newval = Table_points[,1:3]) 
    system(paste(Path_sensible_data,'/MAESPA_routine/MAESPA_Remko_VR/', 'MAESPA_Remko_VR', sep=''),
           show.output.on.console=T)
    if (i==1){
    ResultsTestFlux= readtestflx(paste(Path_sensible_data,
                                       "/MAESPA_routine/MAESPA_Remko_VR/testflx.dat", sep=""))
    ResultsTestFlux$DAY= rep(DATES[i], nrow(ResultsTestFlux))
    ResultsTestFlux$NumPhoto= rep(Table_points[,4], length(unique(ResultsTestFlux$HR)))
    }else{
        ResultsTestFlux_temp= readtestflx(paste(Path_sensible_data,
                                           "/MAESPA_routine/MAESPA_Remko_VR/testflx.dat", sep=""))
        ResultsTestFlux_temp$DAY= rep(DATES[i], nrow(ResultsTestFlux_temp))
        ResultsTestFlux_temp$NumPhoto= rep(Table_points[,4], length(unique(ResultsTestFlux_temp$HR)))
        ResultsTestFlux= rbind(ResultsTestFlux, ResultsTestFlux_temp)
        
    }
    } # If one column of Tablepoints is full of NA, skip these photos 
      # (either unused or not filled at this time)
}

View(ResultsTestFlux)

Simulated_TD= aggregate(data.frame(ResultsTestFlux$X, ResultsTestFlux$Y, ResultsTestFlux$TD),
                        list(ResultsTestFlux$NumPhoto), unique)
# Returns a Table with the Diffuse Transmittance values for each couple of X and Y (i.e each point)
colnames(Simulated_TD)= c('NumPhoto','x', 'y', 'TD')
Simulated_TD$x= Simulated_TD$x + minX
Simulated_TD$y= Simulated_TD$y + minY


# Mesures comparison --------------------------------------------------------------------------
# Measures where made with hemispheric images.

Comparison_sim_meas= merge(Table_Comparison, Simulated_TD, by.x= 'NumPhoto', by.y= 'NumPhoto')

library(ggplot2)
qplot(Comparison_sim_meas$TD_measure, Comparison_sim_meas$TD, color=Comparison_sim_meas$Parcelle, cex=2)
LMTot= lm(Comparison_sim_meas$TD~Comparison_sim_meas$TD_measure)


Parcelle= Comparison_sim_meas$Parcelle
Simulated_TD= Comparison_sim_meas$TD
Measured_TD= Comparison_sim_meas$TD_measure
x11()
par(bg="azure3" )
couleur= rainbow(max(Parcelle))
couleur[4]='black'
plot(Simulated_TD,Simulated_TD, type= 'n', ylim=c(0,1), xlim=c(0,1))
points(Measured_TD, Simulated_TD, bg=couleur[Parcelle], col='white', pch=21 , cex=1.5)
cor.test(Measured_TD,Simulated_TD)

abline(0,1, lty=2)
box()
for (i in 1: max(Parcelle)){
   lm_i= lm(Simulated_TD[Parcelle==i]~Measured_TD[Parcelle==i])
   abline(lm_i, col=couleur[i])
}
#Relancer Table_measures_temp, puis:

legend("bottomright", legend= paste("Parcelle", levels(Table_measures_temp$Parcela_PER)), 
       col=couleur, pch=16, bg='white', cex=0.8) 


# Outliers problem ----------------------------------------------------------------------------
Trees_Table=read.table(paste(Path_sensible_data, "/DATA/Trees_GPS.csv", sep=''), h=T, sep=";")
raster_TD= raster(paste(Path_sensible_data,"/Results/TIFF/Diffuse Transmittance high FBEAM/Diffuse_Tranmittance_29_03_2015.asc", sep=""))
plot(raster_TD)
View(ResultsTestFlux[which(ResultsTestFlux$TD<0.05),-c(8,9)])
Outliers_Table= ResultsTestFlux[which(ResultsTestFlux$TD<0.05),-c(8,9)]
Outliers_Table$Y= Outliers_Table$Y +minY
Outliers_Table$X= Outliers_Table$X +minX
points(Outliers_Table$Y~Outliers_Table$X, col='red')
write.table(Outliers_Table, file= paste(Path_sensible_data, "/Outliers_TD.txt", sep=''))
points(Trees_Table$YCOORD.N.21.6~Trees_Table$XCOORD.N.21.6)
# As we thought, the outliers are coffees that have nearly the same coordinates than a tree. 
# We have to remove them.
Comparison_sim_meas$TD[Comparison_sim_meas$TD<0.05]=NA
# Redo from l.109 until l.129



# Plot by parcelle ---------------------------------------------------------------------------

TD_Parcelle= aggregate(Comparison_sim_meas, 
    list(Comparison_sim_meas$Parcelle, Comparison_sim_meas$Bloque, Comparison_sim_meas$Treatment), 
    mean, na.rm=T)
RMSE= sqrt(mean((TD_Parcelle$TD-TD_Parcelle$TD_measure)^2,na.rm=TRUE))
x11()
par(bg='azure3')
plot(TD_Parcelle$TD_measure, TD_Parcelle$TD, xlim=c(0,1), ylim=c(0,1),
     col=couleur[TD_Parcelle$Parcelle], pch=19, cex=1.3, xlab='Measured diffuse transmittance (0-1)',
     ylab='Simulated diffuse transmittance (0-1)', 
     main= 'Diffuse transmittance: MAESPA simulation and Hemispheric photographs' )
legend("bottomright", legend= paste("Parcel", levels(as.factor(Table_measures_temp$Parcela_PER))), 
       col=couleur, pch=19, bg='lightsteelblue', cex=1.5,  text.col='white') 
abline(0, 1, lty=2)
Correlation= cor.test(TD_Parcelle$TD_measure, TD_Parcelle$TD)
legend('topleft', legend=paste("RMSE:", round(RMSE,3)), bg= 'lightsteelblue',
       text.col='white', cex= 1.3)


grey.colors(n, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL)
couleurgrey= grey.colors(max(Parcelle), start = 0, end = 0.7, gamma=1)
Forme= c(15:18, 20, 10, 8)
par(bg='grey98')
plot(TD_Parcelle$TD_measure, TD_Parcelle$TD, xlim=c(0.2,1), ylim=c(0.2,1),
     col=couleurgrey[TD_Parcelle$Parcelle], pch= Forme[TD_Parcelle$Parcelle],
     cex=1.5, xlab='Measured diffuse transmittance (0-1)', 
     ylab='Simulated diffuse transmittance (0-1)', 
     main= 'Diffuse transmittance: MAESPA simulation and Hemispheric photographs')
legend("bottomright", legend= paste("Parcel", levels(as.factor(Table_measures_temp$Parcela_PER))), 
       col=couleurgrey, pch= Forme, bg='grey95', cex=1, pt.bg = 'white'
       )#ncol= max(Table_measures_temp$Parcela_PER))
abline(0, 1, lty=2)
Correlation= cor.test(TD_Parcelle$TD_measure, TD_Parcelle$TD)
legend('topleft', legend=paste("RMSE:", round(RMSE,3)), bg= 'grey95', cex= 1)
