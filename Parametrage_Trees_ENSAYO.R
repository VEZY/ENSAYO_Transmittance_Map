#######################################################################################################################
####---------------------------------------------Paramétrage Trees Ensayo------------------------------------------
#To import csv file "Trees_GPS.csv" into DF "Trees_Table"
#To slim it by merging 2 lines per tree into one line per tree and create "Trees_Table_True"
#To export to excel, introduce the trunks and reimport "Trees_MAESPA.csv" and frame it into "Trees_Table" (1266 obs, including trunks)
#To plot
#To export data to the .dat files run by MAESPA
#To estimate Tree Leaf area
#To compute LA per tree
##Author Remi Vezy

#######################################################################################################################





#  If you does not have to understand the variables construction, you can jump to the part:
# "Exporting to MAESPA .dat files"






# Tree= Shade Tree
# Plant= whole coffee plant
# Resprout= coffee resprout

Path_sensible_data= "F:/These/Projects_classified/ENSAYO_Transmittance_Map"
library(Maeswrap)


################### Import du tableau de donn?es des arbres -----------------------------------------------------

Trees_Table=read.table(paste(Path_sensible_data, "/DATA/Trees_GPS.csv", sep=''), h=T, sep=";")
head(Trees_Table) #to see the first lines
str(Trees_Table)
#View(Trees_Table)
#data.frame':   1290 obs. of  16 variables:
 # NumTree      : int  1 1 2 2 3 3 4 4 5 5 ...
 # Date         : Factor w/ 30 levels "01/09/2014","01/10/2014",..: 1 1 1 1 1 1 1 1 1 1 ...
 # Bloc         : int  3 3 3 3 3 3 3 3 3 3 ...
 # Fert         : Factor w/ 8 levels "AC","BO","BORDE",..: 3 3 3 3 3 3 3 3 3 3 ...
 # Shade        : Factor w/ 6 levels "AB","ABE","ABT",..: 4 4 4 4 4 4 4 4 4 4 ...
 # Azim         : Factor w/ 4 levels "E","N","S","W": 2 4 4 2 4 2 2 4 2 4 ...
 # NumPhot      : Factor w/ 1290 levels "1","10","100",..: 417 419 424 422 427 821 429 431 433 829 ...
 # Circ130cm    : num  117.2 NA NA 38.5 NA ...
 # Circ0cm      : num  129 NA NA 116 NA ...
 # TreeHeight   : num  2.82 2.5 3.14 3 2.44 3.13 2.75 2.78 2.38 3.47 ...
 # CrownHeight  : num  1.23 1.44 1.72 1.58 1.26 1.15 1.25 1.26 0.64 2.21 ...
 # CrownWidth   : num  1.79 2.65 1.99 2.12 1.61 1.59 1.31 1.54 2.02 2.93 ...
 # CrownArea    : num  1.84 2.66 2.15 2.58 1.53 1.32 1.38 1.28 1.05 3.89 ...
 # Species      : Factor w/ 3 levels "CACHA","PORO",..: 2 2 2 2 2 2 2 2 2 2 ...
 # XCOORD.N.21.6: num  207129 207129 207141 207141 207153 ...
 # YCOORD.N.21.6: num  1095073 1095073 1095074 1095074 1095074 ...


#############To slim it by merging 2 lines per tree into one line per tree and create Trees_Table_True
# # A savoir: l'affichage des grands chiffres (comme valeurs dans YCOORD.N.21.6) sont arrondies ? 
# # l'affichage, mais pas en m?moire. On travaille bien en double pr?cision.
# # Tableau avec les donn?es sur tous les arbres de l'essai.
# # Each tree has two lines corresponding to the measure in the Azim column direction (W/N). It has to 
#be aggregated below in one average line only per tree
# 
Tree_number= unique(Trees_Table$NumTree)#returns a vector, data frame or array like ?x? but with
# duplicate elements/rows removed
Tree_diam_temp= tapply(Trees_Table$Circ130cm, Trees_Table$NumTree, mean, na.rm=T)
Tree_diam= Tree_diam_temp/100
Tree_height= tapply(Trees_Table$TreeHeight, Trees_Table$NumTree, mean, na.rm=T)
Tree_Crown_height= tapply(Trees_Table$CrownHeight, Trees_Table$NumTree, mean, na.rm=T)
Tree_Trunk_height= Tree_height - Tree_Crown_height
Crown_RAD_X= (Trees_Table$CrownWidth[Trees_Table$Azim=="W"|Trees_Table$Azim=="E"])/2
#takes crown width only E and W
#symbol "|" for "or"; the original came in diameter, not in radius
Crown_RAD_Y= (Trees_Table$CrownWidth[Trees_Table$Azim=="N"|Trees_Table$Azim=="S"])/2
Xcoord= tapply(Trees_Table$XCOORD.N.21.6, Trees_Table$NumTree, unique, na.rm=T)
#unique can be used in Tapply too
#XCOORD.N.21.6 is just the original name of the variable
Ycoord= tapply(Trees_Table$YCOORD.N.21.6, Trees_Table$NumTree, unique, na.rm=T)
Species= tapply(Trees_Table$Species, Trees_Table$NumTree, unique, na.rm=T)

# # Par ordre alphab?tique: Cacha= 1 ; Poro= 2 ; Terminalia= 3
# Problem, Date coming from tapply comes into factor
Trees_Table$Date=as.Date(Trees_Table$Date, format="%d/%m/%Y")# to transform into a date
position= 1:length(Trees_Table$Date)
position%%2==1 #divide by 2 and give the rest #
Date= Trees_Table$Date[(position%%2==1)==T] 
#Je r?cup?re les valeurs impaires uniquement (une valeur sur 2)

Shade=Trees_Table$Shade[(position%%2==1)==T]
Bloc= tapply(Trees_Table$Bloc, Trees_Table$NumTree, unique, na.rm=T)
Fert= tapply(Trees_Table$Fert, Trees_Table$NumTree, unique, na.rm=T)
 
# # Il y avait un probl?me de r?p?tition de certaines lignes (fautes de frappe):
# test=rep(1, 1290)
# test2= tapply(test, Trees_Table$NumTree, sum, na.rm=T)
# test2[test2>2]
# # Les test2 sup?rieurs ? 2 ont 4 lignes, correction directement dans le csv. Tout est bon maintenant.
# 
plot(Trees_Table$YCOORD.N.21.6~Trees_Table$XCOORD.N.21.6)
points(Ycoord~Xcoord, col='red') #Test: is all coordinates presents? If all black becomes red: OK.

# # Certains arbres pr?sentent des NA, il s'agit de Poro morts sur pied. Je les retire du jeu de donn?es,
# # puisqu'ils sont insignifiants (r?duits ? un tronc sec de 50cm de haut)
# 
#DataFrame
Trees_Table2= data.frame(Tree_number, Species, Xcoord, Ycoord, Tree_diam, Tree_Crown_height, 
                         Tree_Trunk_height, Crown_RAD_X, Crown_RAD_Y, Date, Bloc, Fert, Shade)
# 
# # L'arbre N?110 (un Poro) pr?sente une valeur en X mais pas en Y. Il s'agit d'un Poro mort, mais elles
# # ont tent? de le mesurer tout de m?me au d?part, donnant des valeurs tr?s faibles. On peut le retirer.
# 
Trees_Table_True_temp= na.omit(Trees_Table2)
head(Trees_Table_True_temp)
str(Trees_Table_True_temp) #633 obs. of  13 variables:
# 
Trees_Table_True= data.frame(Tree_number=Trees_Table_True_temp[,1], 
                         Tree_num_MAESPA= c(1:length(Trees_Table_True_temp$Tree_number)),
                        Trees_Table_True_temp[,2:ncol(Trees_Table_True_temp)])
 head(Trees_Table_True)

for (i in 1:ncol(Trees_Table_True)){
print(class(Trees_Table_True[,i]))
}

str(Trees_Table_True) #633 obs. of  14 variables:

####################To export to excel, introduce the trunks and reimport "Trees_MAESPA.csv"
#

# Trunk ---------------------------------------------------------------------------------------
#We have to simulate the tree trunks manually, by making it an independant species.

Table_trunk= Trees_Table_True 
#Take the previous table (some variables have to repeat exactly, i.e.: X and Y coord)

Table_trunk$Species= rep(4, nrow(Trees_Table_True))                    
#Trunk= Species N?4
Table_trunk$Tree_number= rep(NA, nrow(Trees_Table_True))
Table_trunk$Tree_num_MAESPA= (nrow(Trees_Table_True)+1):(nrow(Trees_Table_True)*2)
# Tree_num_MAESPA for the trunks (start at the end of the previous numbers)
Table_trunk$Tree_diam= rep(0.001, nrow(Trees_Table_True))              
# No trunk for the trunks: their respiration is already accounted
Table_trunk$Crown_RAD_X= (Trees_Table_True$Tree_diam)/2           
# The diamater of the trunk is simulated through a MAESPA crown
Table_trunk$Crown_RAD_Y= (Trees_Table_True$Tree_diam)/2            
# Only one angle of measure, the trunk will be perfectly round
Table_trunk$Tree_Crown_height= Trees_Table_True$Tree_Trunk_height  
#The height of the crown is the actual heigth of the trunk

Trees_Table_Tree_Trunk= rbind(Trees_Table_True, Table_trunk)


###########To estimate Tree Leaf area
# indivlarea ----------------------------------------------------------------------------------
# En attendant les donn?es, on a calcul? la leaf area density (LAD) des Erythrines mesur?es par Fabien.
# Il s'agit du LA divis? par le volume de couronne. Nous nous sommes servis de la valeur moyenne donn?e
# dans son article (Table 2): LA moyen= 914 & Vcouronne moyen= 2130. 
# Ainsi, LAD= 0.429 m?leaf.m-3 couronne
# Nous consid?rons pour l'instant que nos 3 esp?ces ont cette LAD. Il n'y a plus qu'a multiplier ce LAD
# par le volume de couronne de chaque individu pour obtenir le leaf area maximum (= LA 100%).
# Ensuite, on se sert des lois de distribution du leaf area sur l'ann?e a priori pour chaque esp?ce
# pour intrapoler une surface foliaire (leaf area) ? nos dates: cf fonction.

DOY= c(1, 62, 89, 102, 118, 131, 146, 159, 173, 187, 210, 222, 236, 250, 264, 278, 292, 306, 320,
       334, 348, 362)#22 dates qui correspondent
#aux dates de mesures du LAI dans manipe Tuzet par defaut

YEAR= c(rep(2014,21))
# DOY=1:365

# On a une dynamique ? dire d'Expert des leaf area par esp?ce d'arbres:

# Poro: poro sheds leaves naturally before DOY 50, then recovers, then gets pruned severely on DOY215
DOYLA_Poro= c(1, 26, 62, 92, 123, 130, 140, 151, 160, 185, 200, 210, 212, 215, 219, 221, 222, 225, 
              226, 230, 236, 250, 273, 304, 324, 330, 340, 345, 350, 354, 360) #31 dates
LA_Percentage_Poro= c(1, 0.50, 0.20, 0.50, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.1, 0.03, 0.035, 0.04,
            0.07, 0.125, 0.25,0.5, 1, 1, 1, 1, 1, 1, 1, 1)#fraction of max LAD, 31 dates, from expert
splinelarea_Poro= smooth.spline(LA_Percentage_Poro~DOYLA_Poro, spar=0.05)
predictlarea_Poro= predict(splinelarea_Poro, DOY)  
predictlarea_Poro$y[predictlarea_Poro$y>1]=1 #execute str(splinelarea_Poro), y is the predicted
Percent_LA_Poro= predictlarea_Poro$y
str(Percent_LA_Poro)#22 obs, will be reused further in the matrix, see down
plot(LA_Percentage_Poro~DOYLA_Poro, ylim=c(0,1.5), xlab="Day of year", ylab="Leaf cover (%)",
     main="Poro leaf coverage evolution during Year", type='l', lwd=2, col="lightgrey")
points(Percent_LA_Poro~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))
str(DOY)    #22 dates
str(Percent_LA_Poro) #22 dates
str(predictlarea_Poro) #22 dates, x,y,w

# Cacha:casha sheds leaves naturally and slowly before DOY110, then recovers
DOYLA_Cacha= c(1, 25, 35, 55, 59, 62, 74, 115, 151, 166, 190, 200, 236, 250, 273, 304, 324, 354)
LA_Percentage_Cacha= c(1, 1, 1, 1, 1, 0.5, 0.25, 0.03, 0.25, 0.5, 1, 1, 1, 1, 1, 1, 1, 1)
splinelarea_Cacha= smooth.spline(LA_Percentage_Cacha~DOYLA_Cacha, spar=0.2)
predictlarea_Cacha= predict(splinelarea_Cacha, DOY)  
predictlarea_Cacha$y[predictlarea_Cacha$y>1]=1
Percent_LA_Cacha= predictlarea_Cacha$y
str(Percent_LA_Cacha)#22obs
plot(LA_Percentage_Cacha~DOYLA_Cacha, ylim=c(0,1.5), xlab="Day of year", ylab="Leaf cover (%)",
     main="Cacha leaf coverage evolution during Year", type='l', lwd=2, col="lightgrey")
points(Percent_LA_Cacha~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))

# Terminalia:terminalia sheds leaves rapidly before DOY110, then recovers
DOYLA_Terminalia= c(1, 25, 35, 50, 75, 85, 90, 93, 100, 120, 130, 140, 147, 150, 155, 160, 170, 185,
                    250, 273, 304, 324, 354)
LA_Percentage_Terminalia= c(1, 1, 1, 1, 1, 1, 1 ,0.5, 0.25, 0.03, 0.125,0.25, 0.5, 1, 1, 1, 1, 1, 1,
                            1, 1, 1, 1)

splinelarea_Terminalia= smooth.spline(LA_Percentage_Terminalia~DOYLA_Terminalia, spar=0.2)
predictlarea_Terminalia= predict(splinelarea_Terminalia, DOY)  
predictlarea_Terminalia$y[predictlarea_Terminalia$y>1]=1
Percent_LA_Terminalia= predictlarea_Terminalia$y

plot(LA_Percentage_Terminalia~DOYLA_Terminalia, ylim=c(0,1.5), xlab="Day of year", 
     ylab="Leaf cover (%)", main="Terminalia leaf coverage evolution during Year", type='l',
     lwd=2, col="lightgrey")
points(Percent_LA_Terminalia~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))


#########To compute LA per tree
LAD= 0.429 #m?leaf.m-3 couronne
CrownVolume= Trees_Table_Tree_Trunk$Crown_RAD_X * Trees_Table_Tree_Trunk$Crown_RAD_Y * Trees_Table_Tree_Trunk$Tree_Crown_height/2 * pi * 4/3 
str(CrownVolume) #1266 obs
LeafAreaMax= CrownVolume * LAD     #Maximum leaf area, vector with 1266 obs , to imagine vertical
ndates= length(DOY) #Number of dates to simulate = 22 dates, to imagine horizonthal
str(ndates) #22 dates

#Prepare a matrix, filled with NA
LEAF_AREA= matrix(NA, nrow= length(Trees_Table_Tree_Trunk$Tree_num_MAESPA), ncol= ndates)
#declare the matrix, filled with NA
str(LEAF_AREA) 
# logi [1:1266, 1:22] with 22 columns of dates, the same length as for Percent_LA_Cacha, see above

for (i in 1:length(Trees_Table_Tree_Trunk$Tree_num_MAESPA))  {     
    # Pour chaque arbre (du premier au dernier), 1266 obs,
    # i est le num?ro de la boucle, qui commence ? 1 et finit ? 1266, donc ici en plier est ?gal au 
    # num?ro de ligne, mais pas forc?ment, on pourrait ?crire 
    # for (i in 10:length(Trees_Table_Tree_Trunk$Tree_num_MAESPA)) la boucle 1 commencerait 
    # dans ce cas ? la ligne 10
    
    if (Trees_Table_Tree_Trunk$Species[i]==1){
    # dans la boucle numero i, on prend Trees_Table_Tree_Trunk$Species, et on ne le garde que s'il vaut 1
        LEAF_AREA[i,]= Percent_LA_Cacha*LeafAreaMax[i]
    # filling the first line [i,] of the matrix, for every 22 column, using 22
    # values of Percent_LA_Cacha, used now as a Horizontal vector.  LEAF_AREA[i,] means number of loops 
    # i in rows; LeafAreaMax[i] is a column vector of 1266 obs, i refers to nuber of loops, thus of row 
        
    }else{
        if (Trees_Table_Tree_Trunk$Species[i]==2){
            LEAF_AREA[i,]= Percent_LA_Poro*LeafAreaMax[i]
        }else{
            if(Trees_Table_Tree_Trunk$Species[i]==3) {LEAF_AREA[i,]= Percent_LA_Terminalia*LeafAreaMax[i]
            }else{
                LEAF_AREA[i,]= rep(600, ndates)
                #for the Species trunk, the option here is to fill the ndates with 600 = the defaut
                #LA taken by Fabien for trunks
            }
        }
    }
}
colnames(LEAF_AREA)= c(1:ndates) #define the name of the 22 horizontal dates
head(LEAF_AREA) # matrix 22 dates in columns, 1266 obs
tail(LEAF_AREA)
str(LEAF_AREA)

plot(LEAF_AREA[80,]~DOY) # plot only the row number = 80, for 22 dates



# MAESPA X and Y coordinates ------------------------------------------------------------------


plot(Trees_Table_Tree_Trunk$Ycoord~Trees_Table_Tree_Trunk$Xcoord)

minY= 1094942.737
minX= 207104.504
XYMAESPA= cbind(round((Trees_Table_Tree_Trunk$Xcoord-minX),3), round((Trees_Table_Tree_Trunk$Ycoord-minY),3))
#Set the X and Y coordinates from a 0 reference taken as the required plot min X and Y
# Minimal points of the plot: X= 207104.504 ; Y= 1094942.737
# Ce point sera le point de r?f?rence de la parcelle, c?d le point 0.
plot(XYMAESPA)  #Absolutely nothing must change except for the axes values and lab

# Ultimate Table ------------------------------------------------------------------------------

Table_final= cbind(Trees_Table_Tree_Trunk[,1:5],  X_MAESPA=XYMAESPA[,1], Y_MAESPA=XYMAESPA[,2],
                   Trees_Table_Tree_Trunk[,6:(ncol(Trees_Table_Tree_Trunk))], round(LEAF_AREA,2))

# write.table(Table_final, paste(Path_sensible_data, "/DATA/Trees_MAESPA_final.csv"), sep=";", row.names=F)

# Exporting to MAESPA .dat files --------------------------------------------------------------
Table_final=read.table(paste(Path_sensible_data, "/DATA/Trees_MAESPA_final.csv"), h=T, sep=";")

Treesdat= paste(Path_sensible_data, "/DATA/Fichiers_dat/Nouveaux_ENSAYO/Trees.dat", sep='')
#replacePAR : a maeswrap function to replace names of parameters
replacePAR(Treesdat, "x0", "plot", "test")
replacePAR(Treesdat, "y0", "plot", round((1094942.737-minY), 3))
replacePAR(Treesdat, "xmax", "plot", round((207621.842-minX),3))
replacePAR(Treesdat, "ymax", "plot", round((1095292.168-minY),3))
replacePAR(Treesdat, "intx0", "plot", round((207104.504-minX),3))
replacePAR(Treesdat, "inty0", "plot", round((1094942.737-minY),3))
replacePAR(Treesdat, "intxmax", "plot", round((207621.842-minX),3))
replacePAR(Treesdat, "intymax", "plot", round((1095292.168-minY),3))
# A remplir!!!
# replacePAR(Treesdat, "xslope", "plot", 0)
# replacePAR(Treesdat, "yslope", "plot", 0)
# replacePAR(Treesdat, "bearing", "plot", 0)
replacePAR(Treesdat, "notrees", "plot",newval=max(Table_final$Tree_num_MAESPA))


# aerodyn -------------------------------------------------------------------------------------
# IDEM plot Aquiares. Le calcul est diff?rent aujourd'hui, aerodyn ne sert plus pas besoin de changer.


# xy with new centered coordinates for use in MAESPA specifically---------------------------------
replacePAR(Treesdat, "xycoords", "xy", newval=XYMAESPA)

# speclist ------------------------------------------------------------------------------------
replacePAR(Treesdat, "ispecies", "speclist",newval=Table_final$Species)

# indivradx -----------------------------------------------------------------------------------
replacePAR(Treesdat, "values", "indivradx",newval=Table_final$Crown_RAD_X)
replacePAR(Treesdat, "nodates", "indivradx",newval=1)
replacePAR(Treesdat, "dates", "indivradx",newval= "01/09/2014")

# indivrady -----------------------------------------------------------------------------------
replacePAR(Treesdat, "values", "indivrady",newval=Table_final$Crown_RAD_Y)
replacePAR(Treesdat, "nodates", "indivrady",newval=1)
replacePAR(Treesdat, "dates", "indivrady",newval= "01/09/2014")

# indivhtcrown --------------------------------------------------------------------------------
replacePAR(Treesdat, "values", "indivhtcrown",newval=Table_final$Tree_Crown_height)
replacePAR(Treesdat, "nodates", "indivhtcrown",newval=1)
replacePAR(Treesdat, "dates", "indivhtcrown",newval= "01/09/2014")

# indivdiam -----------------------------------------------------------------------------------
replacePAR(Treesdat, "values", "indivdiam",newval=Table_final$Tree_diam)
replacePAR(Treesdat, "nodates", "indivdiam",newval=1)

# indivhttrunk --------------------------------------------------------------------------------
replacePAR(Treesdat, "values", "indivhttrunk",newval=Table_final$Tree_Trunk_height)
replacePAR(Treesdat, "nodates", "indivhttrunk",newval=1)
replacePAR(Treesdat, "dates", "indivhttrunk",newval= "01/09/2014")


#####################################################################################################

replacePAR(Treesdat, "values", "indivlarea",newval=round(LEAF_AREA,2))
replacePAR(Treesdat, "nodates", "indivlarea",newval=length(DOY))
NEWDATES= c("01/01/2014", "04/03/2014", "31/03/2014", "13/04/2014", "29/04/2014", "12/05/2014", 
            "27/05/2014", "09/06/2014", "23/06/2014", "07/07/2014", "30/07/2014", "11/08/2014",
            "25/08/2014", "08/09/2014", "22/09/2014", "06/10/2014", "20/10/2014", "03/11/2014",
            "17/11/2014", "01/12/2014", "15/12/2014", "29/12/2014")
replacePAR(Treesdat, "dates", "indivlarea",newval= NEWDATES)


