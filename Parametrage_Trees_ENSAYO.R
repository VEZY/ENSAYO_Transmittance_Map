#######################################################################################################################
####---------------------------------------------Paramétrage Trees Ensayo------------------------------------------####
#######################################################################################################################

# Tree= Shade Tree
# Plant= whole coffee plant
# Resprout= coffee resprout
setwd("F:/These/Ensayo_CATIE/Data/Fichiers_dat/Nouveaux_ENSAYO")
library(Maeswrap)

# Import du tableau de données des arbres -----------------------------------------------------

# Trees_Table=read.table("F:/These/Ensayo_CATIE/Data/Trees_GPS.csv", h=T, sep=";", numerals = "no.loss")
# head(Trees_Table)
# # A savoir: l'affichage des grands chiffres (comme valeurs dans YCOORD.N.21.6) sont arrondies à 
# # l'affichage, mais pas en mémoire. On travaille bien en double précision.
# # Tableau avec les données sur tous les arbres de l'essai.
# # Each tree has two lines corrsponding to the measure in the Azim column direction (W/N).
# 
# Tree_number= unique(Trees_Table$NumTree)
# Tree_diam_temp= tapply(Trees_Table$Circ130cm, Trees_Table$NumTree, mean, na.rm=T)
# Tree_diam= Tree_diam_temp/100
# Tree_height= tapply(Trees_Table$TreeHeight, Trees_Table$NumTree, mean, na.rm=T)
# Tree_Crown_height= tapply(Trees_Table$CrownHeight, Trees_Table$NumTree, mean, na.rm=T)
# Tree_Trunk_height= Tree_height - Tree_Crown_height
# Crown_RAD_X= (Trees_Table$CrownWidth[Trees_Table$Azim=="W"|Trees_Table$Azim=="E"])/2
# Crown_RAD_Y= (Trees_Table$CrownWidth[Trees_Table$Azim=="N"|Trees_Table$Azim=="S"])/2
# Xcoord= tapply(Trees_Table$XCOORD.N.21.6, Trees_Table$NumTree, unique, na.rm=T)
# Ycoord= tapply(Trees_Table$YCOORD.N.21.6, Trees_Table$NumTree, unique, na.rm=T)
# Species= tapply(Trees_Table$Species, Trees_Table$NumTree, unique, na.rm=T)
# # Par ordre alphabétique: Cacha= 1 ; Poro= 2 ; Terminalia= 3
# 
# Trees_Table$Date=as.Date(Trees_Table$Date, format="%d/%m/%Y")
# position= 1:length(Trees_Table$Date)
# position%%2==1
# Date= Trees_Table$Date[(position%%2==1)==T] #Je récupère les valeurs impaires uniquement (une valeur sur 2)
# Shade=Trees_Table$Shade[(position%%2==1)==T]
# Bloc= tapply(Trees_Table$Bloc, Trees_Table$NumTree, unique, na.rm=T)
# Fert= tapply(Trees_Table$Fert, Trees_Table$NumTree, unique, na.rm=T)
# 
# # Il y avait un problème de répétition de certaines lignes (fautes de frappe):
# # test=rep(1, 1290)
# # test2= tapply(test, Trees_Table$NumTree, sum, na.rm=T)
# # test2[test2>2]
# # Les test2 supérieurs à 2 ont 4 lignes, correction directement dans le csv. Tout est bon maintenant.
# 
# # plot(Trees_Table$YCOORD.N.21.6~Trees_Table$XCOORD.N.21.6)
# # points(Ycoord~Xcoord, col='red') #Test: is all coordinates presents? If all black becomes red: OK.
# 
# # Certains arbres présentent des NA, il s'agit de Poro morts sur pied. Je les retire du jeu de données,
# # puisqu'ils sont insignifiants (réduits à un tronc sec de 50cm de haut)
# 
# Trees_Table2= data.frame(Tree_number, Species, Xcoord, Ycoord, Tree_diam, Tree_Crown_height, Tree_Trunk_height, 
#                     Crown_RAD_X, Crown_RAD_Y, Date, Bloc, Fert, Shade)
# 
# # L'arbre N°110 (un Poro) présente une valeur en X mais pas en Y. Il s'agit d'un Poro mort, mais elles
# # ont tenté de le mesurer tout de même au départ, donnant des valeurs très faibles. On peut le retirer.
# 
#Trees_Table_True_temp= na.omit(Trees_Table2)
# head(Trees_Table_True_temp)
# 
# Trees_Table_True= data.frame(Tree_number=Trees_Table_True_temp[,1], 
#                         Tree_num_MAESPA= c(1:length(Trees_Table_True_temp$Tree_number)),
#                         Trees_Table_True_temp[,2:ncol(Trees_Table_True_temp)])
# head(Trees_Table_True)
# # for (i in 1:ncol(Trees_Table_True)){
# # print(class(Trees_Table_True[,i]))
# # }
# 
# write.table(Trees_Table_True, "F:/These/Ensayo_CATIE/Data/Trees_MAESPA.csv", sep=";", row.names=F)

Trees_Table=read.table("F:/These/Ensayo_CATIE/Data/Trees_MAESPA.csv", h=T, sep=";", numerals = "no.loss")
#Attention, ce CVS à été modifié directement sans Excel (ajout de l'espèce tronc). Il ne s'agit donc
# plus du même fichier que celui du write précédent
head(Trees_Table)

# plot ----------------------------------------------------------------------------------------

# Point minimal de la pareclle: X= 207104.504 ; Y= 1094942.737
# Ce point sera le point de référence de la parcelle, càd le point 0.

plot(Trees_Table$Ycoord~Trees_Table$Xcoord)
minY= min(1094942.737)
minX= min(207104.504)

replacePAR("Trees.dat", "x0", "plot", round((207104.504-minX), 3))
replacePAR("Trees.dat", "y0", "plot", round((1094942.737-minY), 3))
replacePAR("Trees.dat", "xmax", "plot", round((207621.842-minX),3))
replacePAR("Trees.dat", "ymax", "plot", round((1095292.168-minY),3))
replacePAR("Trees.dat", "intx0", "plot", round((207104.504-minX),3))
replacePAR("Trees.dat", "inty0", "plot", round((1094942.737-minY),3))
replacePAR("Trees.dat", "intxmax", "plot", round((207621.842-minX),3))
replacePAR("Trees.dat", "intymax", "plot", round((1095292.168-minY),3))
# A remplir!!!
# replacePAR("Trees.dat", "xslope", "plot", 0)
# replacePAR("Trees.dat", "yslope", "plot", 0)
# replacePAR("Trees.dat", "bearing", "plot", 0)
replacePAR("Trees.dat", "notrees", "plot",newval=max(Trees_Table$Tree_num_MAESPA))


# aerodyn -------------------------------------------------------------------------------------
# IDEM plot Aquiares. Le calcul est différent aujourd'hui, pas besoin de changer.


# xy ------------------------------------------------------------------------------------------
XYMAESPA= cbind(round((Trees_Table$Xcoord-minX),3), round((Trees_Table$Ycoord-minY),3))
replacePAR("Trees.dat", "xycoords", "xy", newval=XYMAESPA)

# speclist ------------------------------------------------------------------------------------
replacePAR("Trees.dat", "ispecies", "speclist",newval=Trees_Table$Species)

# indivradx -----------------------------------------------------------------------------------
replacePAR("Trees.dat", "values", "indivradx",newval=Trees_Table$Crown_RAD_X)
replacePAR("Trees.dat", "nodates", "indivradx",newval=1)
replacePAR("Trees.dat", "dates", "indivradx",newval= "01/09/2014")

# indivrady -----------------------------------------------------------------------------------
replacePAR("Trees.dat", "values", "indivrady",newval=Trees_Table$Crown_RAD_Y)
replacePAR("Trees.dat", "nodates", "indivrady",newval=1)
replacePAR("Trees.dat", "dates", "indivrady",newval= "01/09/2014")

# indivhtcrown --------------------------------------------------------------------------------
replacePAR("Trees.dat", "values", "indivhtcrown",newval=Trees_Table$Tree_Crown_height)
replacePAR("Trees.dat", "nodates", "indivhtcrown",newval=1)
replacePAR("Trees.dat", "dates", "indivhtcrown",newval= "01/09/2014")

# indivdiam -----------------------------------------------------------------------------------
replacePAR("Trees.dat", "values", "indivdiam",newval=Trees_Table$Tree_diam)
replacePAR("Trees.dat", "nodates", "indivdiam",newval=1)

# indivhttrunk --------------------------------------------------------------------------------
replacePAR("Trees.dat", "values", "indivhttrunk",newval=Trees_Table$Tree_Trunk_height)
replacePAR("Trees.dat", "nodates", "indivhttrunk",newval=1)
replacePAR("Trees.dat", "dates", "indivhttrunk",newval= "01/09/2014")


# indivlarea ----------------------------------------------------------------------------------
# En attendant les données, on a calculé la leaf area density (LAD) des Erythrines mesurées par Fabien.
# Il s'agit du LA divisé par le volume de couronne. Nous nous sommes servis de la valeur moyenne donnée
# dans son article (Table 2): LA moyen= 914 & Vcouronne moyen= 2130. 
# Ainsi, LAD= 0.429 m²leaf.m-3 couronne
# Nous considérons pour l'instant que nos 3 espèces ont cette LAD. Il n'y a plus qu'a multiplier ce LAD
# par le volume de couronne de chaque individu pour obtenir le leaf area maximum (= LA 100%).
# Ensuite, on se sert des lois de distribution du leaf area sur l'année a priori pour chaque espèce
# pour intrapoler une surface foliaire (leaf area) à nos dates: cf fonction.

DOY= c(1, 62, 89, 102, 118, 131, 146, 159, 173, 187, 210, 222, 236, 250, 264, 278, 292, 306, 320, 334, 348, 362)

YEAR= c(rep(2014,21))
# DOY=1:365

# On a une dynamique à priori des leaf area par espèce d'arbres:

# Poro:
DOYLA_Poro= c(1, 26, 62, 92, 123, 130, 140, 151, 160, 185, 200, 210, 212, 215, 219, 221, 222, 225, 
              226, 230, 236, 250, 273, 304, 324, 330, 340, 345, 350, 354, 360)
LA_Percentage_Poro= c(1, 0.50, 0.20, 0.50, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.1, 0.03, 0.035, 0.04,
                      0.07, 0.125, 0.25,0.5, 1, 1, 1, 1, 1, 1, 1, 1)

splinelarea_Poro= smooth.spline(LA_Percentage_Poro~DOYLA_Poro, spar=0.05)
predictlarea_Poro= predict(splinelarea_Poro, DOY)  
predictlarea_Poro$y[predictlarea_Poro$y>1]=1
Percent_LA_Poro= predictlarea_Poro$y
plot(LA_Percentage_Poro~DOYLA_Poro, ylim=c(0,1.5), xlab="Day of year", ylab="Leaf cover (%)",
     main="Poro leaf coverage evolution during Year", type='l', lwd=2, col="lightgrey")
points(Percent_LA_Poro~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))

# Cacha:
DOYLA_Cacha= c(1, 25, 35, 55, 59, 62, 74, 115, 151, 166, 190, 200, 236, 250, 273, 304, 324, 354)
LA_Percentage_Cacha= c(1, 1, 1, 1, 1, 0.5, 0.25, 0.03, 0.25, 0.5, 1, 1, 1, 1, 1, 1, 1, 1)
splinelarea_Cacha= smooth.spline(LA_Percentage_Cacha~DOYLA_Cacha, spar=0.2)
predictlarea_Cacha= predict(splinelarea_Cacha, DOY)  
predictlarea_Cacha$y[predictlarea_Cacha$y>1]=1
Percent_LA_Cacha= predictlarea_Cacha$y

plot(LA_Percentage_Cacha~DOYLA_Cacha, ylim=c(0,1.5), xlab="Day of year", ylab="Leaf cover (%)",
     main="Cacha leaf coverage evolution during Year", type='l', lwd=2, col="lightgrey")
points(Percent_LA_Cacha~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))

# Terminalia:
DOYLA_Terminalia= c(1, 25, 35, 50, 75, 85, 90, 93, 100, 120, 130, 140, 147, 150, 155, 160, 170, 185,
                    250, 273, 304, 324, 354)
LA_Percentage_Terminalia= c(1, 1, 1, 1, 1, 1, 1 ,0.5, 0.25, 0.03, 0.125,0.25, 0.5, 1, 1, 1, 1, 1, 1,
                            1, 1, 1, 1)

splinelarea_Terminalia= smooth.spline(LA_Percentage_Terminalia~DOYLA_Terminalia, spar=0.2)
predictlarea_Terminalia= predict(splinelarea_Terminalia, DOY)  
predictlarea_Terminalia$y[predictlarea_Terminalia$y>1]=1
Percent_LA_Terminalia= predictlarea_Terminalia$y

plot(LA_Percentage_Terminalia~DOYLA_Terminalia, ylim=c(0,1.5), xlab="Day of year", ylab="Leaf cover (%)",
     main="Terminalia leaf coverage evolution during Year", type='l', lwd=2, col="lightgrey")
points(Percent_LA_Terminalia~DOY, col='red', pch=16)
legend("topright", legend=c("A priori from Expert", "Required days (spline fitted)"), pch=c(NA, 16),
       lwd=c(2, NA), col=c("lightgrey", "red"))



###################################Calcul du LA par arbre##########################################
LAD= 0.429
CrownVolume= Trees_Table$Crown_RAD_X/2 * Trees_Table$Crown_RAD_Y/2 * Trees_Table$Tree_Crown_height/2 * pi * 4/3 
LeafAreaMax= CrownVolume * LAD     #Maximum leaf area
ndates= length(DOY) #Number of dates to simulate

LEAF_AREA= matrix(NA, nrow= length(Trees_Table$Tree_num_MAESPA), ncol= ndates)

for (i in 1:length(Trees_Table$Tree_num_MAESPA))  {     #Pour chaque arbre (du premier au dernier)

if (Trees_Table$Species[i]==1){
    LEAF_AREA[i,]= Percent_LA_Cacha*LeafAreaMax[i]
}else{
    if (Trees_Table$Species[i]==2){
        LEAF_AREA[i,]= Percent_LA_Poro*LeafAreaMax[i]
    }else{
        if(Trees_Table$Species[i]==3) {LEAF_AREA[i,]= Percent_LA_Terminalia*LeafAreaMax[i]
        }else{
            LEAF_AREA[i,]= rep(600, ndates)
        }
        }
}
}
colnames(LEAF_AREA)= c(1:ndates)
head(LEAF_AREA)
tail(LEAF_AREA)
# plot(LEAF_AREA[80,]~DOY)

#####################################################################################################

replacePAR("Trees.dat", "values", "indivlarea",newval=round(LEAF_AREA,2))
replacePAR("Trees.dat", "nodates", "indivlarea",newval=length(DOY))
NEWDATES= c("01/01/2014", "04/03/2014", "31/03/2014", "13/04/2014", "29/04/2014", "12/05/2014", 
            "27/05/2014", "09/06/2014", "23/06/2014", "07/07/2014", "30/07/2014", "11/08/2014",
            "25/08/2014", "08/09/2014", "22/09/2014", "06/10/2014", "20/10/2014", "03/11/2014",
            "17/11/2014", "01/12/2014", "15/12/2014", "29/12/2014")
replacePAR("Trees.dat", "dates", "indivlarea",newval= NEWDATES)
head(Trees_Table)
Table_final= cbind(Trees_Table[,1:5],  X_MAESPA=XYMAESPA[,1], Y_MAESPA=XYMAESPA[,2],
                   Trees_Table[,6:(ncol(Trees_Table))], round(LEAF_AREA,2))
head(Table_final)
# write.table(Trees_Table, "F:/These/Ensayo_CATIE/Data/Trees_MAESPA_final.csv", sep=";", row.names=F)

