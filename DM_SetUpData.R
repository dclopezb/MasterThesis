#___________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________________
#_________________________________________Prediccion del riesgo default en acuerdos de ingreso compartido:__________________________________________
#_____________________Evidencia para la financiacion de educacion superior en Colombia usando metodos de machine learning___________________________
#__________________________________________________MSc.(c) Diana Carolina Lopez Becerra_____________________________________________________________
#___________________________________________________________Ph.D.Giovanni Munoz_____________________________________________________________________
#__________________________________________Universidad Nacional de Colombia, sede Bogota D.C._______________________________________________________
#_________________________________________________________________2019______________________________________________________________________________

#manejar tildes#####################################################################################################################################
Sys.setlocale('LC_ALL','es_CO.iso88591')

#instalar paquetes##################################################################################################################################
install.packages("vcd")
install.packages("hms")
install.packages("plyr")
install.packages("woeBinning")

#cargar paquetes####################################################################################################################################
library(vcd)
library(plyr)
library(hms)
library(woeBinning)

#cargar la base de datos############################################################################################################################
base<-read.csv(paste('DM_DB_tobeDebugged.csv',sep=""),header = TRUE, sep=";")

#declarar clase de las variables####################################################################################################################
base$CuotMora<-as.factor(base$CuotMora)
base$RP_Aplic<-as.numeric(base$RP_Aplic)

#ordenar valores de las variables###################################################################################################################
base$EstCiv<-factor(base$EstCiv,levels =c("Soltero", "Union libre","Casado", "Divorciado"))
base$LugHerm<-factor(base$LugHerm,levels =c("Hijo unico", "Menor","Intermedio", "Mayor"))

#crear variable default a partir de la variable CuotMora############################################################################################
base$Default<-as.character(base$Default)
for(i in 1:nrow(base)){
  if(base$CuotMora[i]=='0'||base$CuotMora[i]=="1"||base$CuotMora[i]=="2"||base$CuotMora[i]=="3"
     ||base$CuotMora[i]=="4"||base$CuotMora[i]=="5"){
    base$Default[i]<-as.character("No")}else{
      base$Default[i]<-as.character("Si")
    }
}
base$Default<-factor(base$Default)

#crear variable riesgo a partir de la variable CuotMora############################################################################################
base$Riesgo<-as.character(base$Riesgo)
for(i in 1:nrow(base)){
  if(base$CuotMora[i]=='0'||base$CuotMora[i]=="1"||base$CuotMora[i]=="2"){
    base$Riesgo[i]<-as.character("Aceptable")
  }else if(base$CuotMora[i]=="3"||base$CuotMora[i]=="4"){
    base$Riesgo[i]<-as.character("Apreciable")
  }else if(base$CuotMora[i]=="5"||base$CuotMora[i]=="6"){
    base$Riesgo[i]<-as.character("Significativo")
  }
}
base$Riesgo<-factor(base$Riesgo)

#crear la variable Región de nacimiento a partir de la variable Dpto de nacimiento##################################################################
base$RegNac<-as.character("a")
for(i in 1:nrow(base)){
  if(base$DptoNac[i]=='ATLANTICO'||base$DptoNac[i]=="BOLIVAR"||base$DptoNac[i]=="CORDOBA"
     ||base$DptoNac[i]=="MAGDALENA"||base$DptoNac[i]=="SUCRE"||base$DptoNac[i]=='CASANARE'
     ||base$DptoNac[i]=="META"||base$DptoNac[i]=="BOYACA"||base$DptoNac[i]=="SANTANDER"||base$DptoNac[i]=="NORTE DE SANTANDER"){
    base$RegNac[i]<-as.character("N.E.")#Noreste
  }else if(base$DptoNac[i]=='CAQUETA'||base$DptoNac[i]=="CAUCA"||base$DptoNac[i]=="VALLE DEL CAUCA"||base$DptoNac[i]=="NARINO"){
    base$RegNac[i]<-as.character("S.O.")#Suroeste
  } else if(base$DptoNac[i]=='ANTIOQUIA'||base$DptoNac[i]=="CALDAS"||base$DptoNac[i]=="CUNDINAMARCA"
            ||base$DptoNac[i]=="HUILA"||base$DptoNac[i]=="TOLIMA"||base$DptoNac[i]=="QUINDIO"){
    base$RegNac[i]<-as.character("C.")#Centro
  }else{
    base$RegNac[i]<-as.character(base$DptoNac[i])#Bogota D.C.
  }
}
base$RegNac<-factor(base$RegNac)

#editar la categoria "Entre 5 y 100" de la variable "FamDirViv" por "Mas de 5"######################################################################
levels(base$FamDirViv)[2] <-'5 o mas'
base$FamDirViv<-factor(base$FamDirViv,levels =c("Ninguno", "Entre 1 y 4","5 o mas"))

#editar las categorias de la variable "Herm" #######################################################################################################
levels(base$Herm)[3] <-'Menor o igual a uno'
levels(base$Herm)[2] <-'Mas de tres'
base$Herm<-factor(base$Herm,levels =c("Menor o igual a uno", "Dos", "Tres", "Mas de tres"))

#crear la variable Rango edad promedio de los hijos: a partir del analisis realizado con la muestra y la variable "PromEdadH"#######################
levels(base$PromEdadH)[18] <-0
base$PromEdadH<-as.numeric(paste(base$PromEdadH))
base$R_PromEdadH<-cut(x = base$PromEdadH, breaks = c(-1,0,6.5,100), labels = c("Sin hijos","Recien nacidos - 6.5","> 6.5"))

#crear la variable Rango estrato a partir del analisis realizado con la muestra y la variable "Estrato"#############################################
base$R_Estrato<-as.character("a")
for(i in 1:nrow(base)){
  if(base$Estrato[i]=='Uno'||base$Estrato[i]=="Dos"){
    base$R_Estrato[i]<-as.character("Uno y Dos")
  }else if(base$Estrato[i]=="Cinco"||base$Estrato[i]=="Seis"){
    base$R_Estrato[i]<-as.character("Cinco y Seis")
  }else{
    base$R_Estrato[i]<-as.character(base$Estrato[i])
  }
}
base$R_Estrato<-factor(base$R_Estrato)
base$R_Estrato<-factor(base$R_Estrato, levels =c("Uno y Dos","Tres","Cuatro","Cinco y Seis"))

#editar la categoria "Usted" de la variable "RespEcnm" por "El postulante"##########################################################################
levels(base$RespEcnm)[3] <-'El postulante'

#editar la variable IES: tabla que organiza de menor a mayor la frecuencia de la IES del postulante#################################################
#IES<-count(base$IES)
#IES[order(IES$freq),]

#editar la variable Nivel de formación##############################################################################################################
levels(base$NivFrmc)[1] <-"Especializacion"
levels(base$NivFrmc)[2] <-"Maestria"
levels(base$NivFrmc)[3] <-"Universitario"
base$NivFrmc<-factor(base$NivFrmc,levels =c("Universitario", "Especializacion", "Maestria"))

#editar la variable Programa académico: tabla que organiza de menor a mayor la frecuencia del PA del postulante#####################################
#PA<-count(base$PA)
#PA[order(PA$freq),]
#editar la variable Metodología#####################################################################################################################
levels(base$Metdlg)[1] <-"Distancia"
base$Metdlg<-factor(base$Metdlg,levels =c("Presencial", "Virtual","Distancia"))

#editar la variable Areas del conocimiento##########################################################################################################
base$AreaCncm<-as.factor(base$AreaCncm)
levels(base$AreaCncm)[1] <-"B.A."
levels(base$AreaCncm)[2] <-"C.Ed."
levels(base$AreaCncm)[3] <-"C.S."
levels(base$AreaCncm)[4] <-"C.So."
levels(base$AreaCncm)[5] <-"C.Ec."
levels(base$AreaCncm)[6] <-"I.A."
levels(base$AreaCncm)[7] <-"M.Cn."

#construir base para "train" y "test"###############################################################################################################
muestra<-base[base$TipBase=="Muestra",]

#construir base estudiantes: para experiemento "en vivo"############################################################################################
experimento<-base[base$TipBase=="Experimiento",]

#crear objeto colores para las graficas#############################################################################################################
coloresCM<-c("lightcyan4","cadetblue4","cadetblue", "cadetblue3","lightblue","lightblue2","lightcyan2")

#construccion de rangos para las variables numéricas##################################################################################################
#crear la variable Rango edad postulación: a partir del analisis realizado con la muestra y la variable "edad postulación"##########################
R_EdadPost<-woe.binning(muestra, "Default", "EdadPost") 
muestra<-woe.binning.deploy(muestra, R_EdadPost, add.woe.or.dum.var = "woe")

#crear la variable Rango periodos programa académico: a partir del analisis realizado con la muestra y la variable "PrdPA"##########################
R_PrdPA<-woe.binning(muestra, "Default", "PrdPA") 
muestra<-woe.binning.deploy(muestra, R_PrdPA, add.woe.or.dum.var = "woe")

#crear la variable Rango Valor matrícula semestral: a partir del analisis realizado con la muestra y la variable "Mtrc"#############################
muestra$Mtrc<-as.numeric(muestra$Mtrc)
R_Mtrc<-woe.binning(muestra, "Default", "Mtrc") 
muestra<-woe.binning.deploy(muestra, R_Mtrc, add.woe.or.dum.var = "woe")

#crear la variable Rango CP: a partir del analisis realizado con la muestra y la variable "CultPag"#################################################
muestra$CultPag<-as.numeric(muestra$CultPag)
R_CP<-woe.binning(muestra, "Default", "CultPag") 
muestra<-woe.binning.deploy(muestra, R_CP, add.woe.or.dum.var = "woe")

#crear la variable Rango PC: a partir del analisis realizado con la muestra y la variable "PrcntCuota"####################################################
muestra$PrcntCuota<-as.numeric(muestra$PrcntCuota)
R_PC<-woe.binning(muestra, "Default", "PrcntCuota") 
muestra<-woe.binning.deploy(muestra, R_PC, add.woe.or.dum.var = "woe")

#crear la variable Rango RP: a partir del analisis realizado con la muestra y la variable "CuotPact"######################################################
R_RP<-woe.binning(muestra, "Default", "CuotPact") 
muestra<-woe.binning.deploy(muestra, R_RP, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Logros-verbal: a partir del analisis realizado con la muestra###############################################
R_LV<-woe.binning(muestra, "Default", "L_verb") 
muestra<-woe.binning.deploy(muestra, R_LV, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Logros-espacial: a partir del analisis realizado con la muestra#############################################
R_LE<-woe.binning(muestra, "Default", "L_esp") 
muestra<-woe.binning.deploy(muestra, R_LE, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Logros-numerico: a partir del analisis realizado con la muestra#############################################
R_LN<-woe.binning(muestra, "Default", "L_num") 
muestra<-woe.binning.deploy(muestra, R_LN, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Logros numerico y espacial: a partir del analisis realizado con la muestra ################################
R_LSin<-woe.binning(muestra, "Default", "L_sinV") 
muestra<-woe.binning.deploy(muestra, R_LSin, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Logros: a partir del analisis realizado con la muestra#####################################################
R_L<-woe.binning(muestra, "Default", "Logros") 
muestra<-woe.binning.deploy(muestra, R_L, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad-agradabilidad: a partir del analisis realizado con la muestra###############################################
R_P_agrd<-woe.binning(muestra, "Default", "P_agrd") 
muestra<-woe.binning.deploy(muestra, R_P_agrd, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad-apertura a la experiencia: a partir del analisis realizado con la muestra###############################################
R_P_apertExp<-woe.binning(muestra, "Default", "P_apertExp") 
muestra<-woe.binning.deploy(muestra, R_P_apertExp, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad-conciencia: a partir del analisis realizado con la muestra###############################################
R_P_conc<-woe.binning(muestra, "Default", "P_conc") 
muestra<-woe.binning.deploy(muestra, R_P_conc, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad-estabilidad emocional:a partir del analisis realizado con la muestra###############################################
R_P_estEmc<-woe.binning(muestra, "Default", "P_estEmc") 
muestra<-woe.binning.deploy(muestra, R_P_estEmc, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad-extraversion:a partir del analisis realizado con la muestra###############################################
R_P_extrv<-woe.binning(muestra, "Default", "P_extrv") 
muestra<-woe.binning.deploy(muestra, R_P_extrv, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba personalidad: a partir del analisis realizado con la muestra###############################################
R_P<-woe.binning(muestra, "Default", "Pers") 
muestra<-woe.binning.deploy(muestra, R_P, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Ciencias Economicas:a partir del analisis realizado con la muestra###############################################
R_I_cEcnm<-woe.binning(muestra, "Default", "I_cEcnm") 
muestra<-woe.binning.deploy(muestra, R_I_cEcnm, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Ingenierias y Arquitectura:a partir del analisis realizado con la muestra###############################################
R_I_ingArq<-woe.binning(muestra, "Default", "I_ingArq") 
muestra<-woe.binning.deploy(muestra, R_I_ingArq, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Bellas Artes:a partir del analisis realizado con la muestra###############################################
R_I_bellArt<-woe.binning(muestra, "Default", "I_bellArt") 
muestra<-woe.binning.deploy(muestra, R_I_bellArt, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Ciencias de la salud:a partir del analisis realizado con la muestra###############################################
R_I_cSalud<-woe.binning(muestra, "Default", "I_cSalud") 
muestra<-woe.binning.deploy(muestra, R_I_cSalud, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Ciencias sociales:a partir del analisis realizado con la muestra###############################################
R_I_cSoc<-woe.binning(muestra, "Default", "I_cSoc") 
muestra<-woe.binning.deploy(muestra, R_I_cSoc, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Técnicos y tecnologicos:a partir del analisis realizado con la muestra###############################################
R_I_tyt<-woe.binning(muestra, "Default", "I_tyt") 
muestra<-woe.binning.deploy(muestra, R_I_tyt, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba Intereses-Matematicas y Ciencias Naturales:a partir del analisis realizado con la muestra###############################################
R_I_matCNat<-woe.binning(muestra, "Default", "I_matCNat") 
muestra<-woe.binning.deploy(muestra, R_I_matCNat, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba intereses: a partir del analisis realizado con la muestra###################################################
R_I<-woe.binning(muestra, "Default", "Intereses") 
muestra<-woe.binning.deploy(muestra, R_I, add.woe.or.dum.var = "woe")

#crear la variable Rango Puntaje prueba de selección a partir del analisis realizado con la muestra##############################################################
R_PrbSel<-woe.binning(muestra, "Default", "PrbSel") 
muestra<-woe.binning.deploy(muestra, R_PrbSel, add.woe.or.dum.var = "woe")

#crear la variable Rango Productos bancarios s a partir del analisis realizado con la muestra###########################################################
muestra$Pts_Banc<-as.numeric(muestra$Pts_Banc)
R_Pts_Banc<-woe.binning(muestra, "Default", "Pts_Banc") 
muestra<-woe.binning.deploy(muestra, R_Pts_Banc, add.woe.or.dum.var = "woe")

#crear la variable Rango Bienes ponderados a partir del analisis realizado con la muestra###########################################################
muestra$B_pond<-as.numeric(muestra$B_pond)
R_B_pond<-woe.binning(muestra, "Default", "B_pond") 
muestra<-woe.binning.deploy(muestra, R_B_pond, add.woe.or.dum.var = "woe")

#crear la variable Rango Scoring #################################################################################################################
R_Scoring<-woe.binning(muestra, "Default", "Scoring") 
muestra<-woe.binning.deploy(muestra, R_Scoring, add.woe.or.dum.var = "woe")

#primera depuracion de la muestra#######################################################################################################################
names(muestra)
muestra<-muestra[,-c(83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,127,129,131,133,135,137,139)]
names(muestra)

####################################################################################################################################################
####################################construir tablas de contingencia: variable de interes vs posibles predictores ##################################
####################################################################################################################################################

#1Género############################################################################################################################################
pdf("t1Genero.pdf")
t1Género<-table(muestra$Genero,muestra$CuotMora)
plot(CuotMora~Genero,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Género vs Máximas cuotas en mora alcanzadas",xlab="Género")
dev.off()

pdf("t1_R_Gen.pdf")
t1_R_Gen<-table(muestra$Genero,muestra$Riesgo)
plot(Riesgo~Genero,data = muestra,col=coloresCM,ylab="Riesgo",main="Género vs Riesgo",xlab="Género")
dev.off()

pdf("t1_D_Gen.pdf")
t1_D_Gen<-table(muestra$Genero,muestra$Default)
plot(Default~Genero,data = muestra,col=coloresCM,ylab="Default",main="Género vs Default",xlab="Género")
dev.off()

#2Estado civil#######################################################################################################################################
pdf("t2EstCiv.pdf")
t2EstCiv<-table(muestra$EstCiv,muestra$CuotMora)
plot(CuotMora~EstCiv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Estado civil vs Máximas cuotas en mora alcanzadas",xlab="Estado civil")
dev.off()

pdf("t2_R_EstCiv.pdf")
t2_R_EstCiv<-table(muestra$EstCiv,muestra$Riesgo)
plot(Riesgo~EstCiv,data = muestra,col=coloresCM,ylab="Riesgo",main="Estado civil vs Riesgo",xlab="Estado civil")
dev.off()

pdf("t2_D_EstCiv.pdf")
t2_D_EstCiv<-table(muestra$EstCiv,muestra$Default)
plot(Default~EstCiv,data = muestra,col=coloresCM,ylab="Default",main="Estado civil vs Default",xlab="Estado civil")
dev.off()

#3Dpto y/o Región de nacimiento######################################################################################################################
pdf("t3RegNac.pdf")
t3RegNac<-table(muestra$RegNac,muestra$CuotMora)
plot(CuotMora~RegNac,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Región de nacimiento vs Máximas cuotas en mora alcanzadas",xlab="Región de nacimiento")
dev.off()

pdf("t3_R_RegNac.pdf")
t3_R_RegNac<-table(muestra$RegNac,muestra$Riesgo)
plot(Riesgo~RegNac,data = muestra,col=coloresCM,ylab="Riesgo",main="Región de nacimiento vs Riesgo",xlab="Región de nacimiento")
dev.off()

pdf("t3_D_RegNac.pdf")
t3_D_RegNac<-table(muestra$RegNac,muestra$Default)
plot(Default~RegNac,data = muestra,col=coloresCM,ylab="Default",main="Región de nacimiento vs Riesgo",xlab="Región de nacimiento")
dev.off()

#4Nacido en capital#################################################################################################################################
pdf("t4NacCap.pdf")
t4NacCap<-table(muestra$NacCap,muestra$CuotMora)
plot(CuotMora~NacCap,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="El Municipio de nacimiento es capital de departamento?",
     main="El Municipio de nacimiento es capital de departamento? vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t4_R_NacCap.pdf")
t4_R_NacCap<-table(muestra$NacCap,muestra$Riesgo)
plot(Riesgo~NacCap,data = muestra,col=coloresCM,ylab="Riesgo",xlab="El Municipio de nacimiento es capital de departamento?",
     main="El Municipio de nacimiento es capital de departamento? vs Riesgo")
dev.off()

pdf("t4_D_NacCap.pdf")
t4_D_NacCap<-table(muestra$NacCap,muestra$Default)
plot(Default~NacCap,data = muestra,col=coloresCM,ylab="Default",main="El Municipio de nacimiento es capital de departamento? vs Default")
dev.off()

#5Estudia en lugar de residencia?##################################################################################################################
table(muestra$EdadPost)
summary(muestra$EdadPost)
sd(muestra$EdadPost)
#la distribucion de la edad de postulacion se encuentra entre 18 y 45 anos, con una media y mediana de 25 anos y una desviacion estandar en 4.4
hEdadPost<-hist(muestra$EdadPost)
pdf("h6EdadPost.pdf")
plot(hEdadPost,main = "Histograma: Edad de postulación",ylab = "Frecuencia",xlab = "Edad de postulación",
     xlim= c(10,45), ylim= c(0,130))
dev.off()

###
pdf("t5ResEst.pdf")
t5ResEst<-table(muestra$ResEst,muestra$CuotMora)
plot(CuotMora~ResEst,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Estudia en el lugar de residencia?",
     main="Estudia en el lugar de residencia? vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t5_R_ResEst.pdf")
t5_R_ResEst<-table(muestra$ResEst,muestra$Riesgo)
plot(Riesgo~ResEst,data = muestra,col=coloresCM,ylab="Riesgo",main="Estudia en el lugar de residencia? vs Riesgo",xlab="Estudia en el lugar de residencia?")
dev.off()

pdf("t5_D_ResEst.pdf")
t5_D_ResEst<-table(muestra$ResEst,muestra$Default)
plot(Default~ResEst,data = muestra,col=coloresCM,ylab="Default",main="Estudia en el lugar de residencia? vs Default",xlab="Estudia en el lugar de residencia?")
dev.off()

pdf("t6EdadPost.pdf")
t6EdadPost<-table(muestra$EdadPost,muestra$CuotMora)
plot(CuotMora~EdadPost,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Edad postulación vs Máximas cuotas en mora alcanzadas",xlab="Edad postulación")
dev.off()

pdf("t6_R_EdadPost.pdf")
t6_R_EdadPost<-table(muestra$EdadPost,muestra$Riesgo)
plot(Riesgo~EdadPost,data = muestra,col=coloresCM,ylab="Riesgo",main="Edad postulación vs Riesgo",xlab="Edad postulación")
dev.off()

pdf("t6_D_EdadPost.pdf")
t6_D_EdadPost<-table(muestra$EdadPost,muestra$Default)
plot(Default~EdadPost,data = muestra,col=coloresCM,ylab="Default",main="Edad postulación vs Default",xlab="Edad postulación")
dev.off()

#rangos

pdf("t6EdadPost_R.pdf")
t6EdadPostR<-table(muestra$EdadPost.binned,muestra$CuotMora)
plot(CuotMora~EdadPost.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Edad postulación",
     main="Edad postulación vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t6_R_EdadPostR.pdf")
t6_R_EdadPostR<-table(muestra$EdadPost.binned,muestra$Riesgo)
plot(Riesgo~EdadPost.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Edad postulación vs Riesgo",xlab="Edad postulación")
dev.off()

pdf("t6_D_EdadPostR.pdf")
t6_D_EdadPostR<-table(muestra$EdadPost.binned,muestra$Default)
plot(Default~EdadPost.binned,data = muestra,col=coloresCM,ylab="Default",main="Edad postulación vs Default",xlab="Edad postulación")
dev.off()

#7Vive con familiares directos####################################################################################################################
pdf("t7FamDirViv.pdf")
t7FamDirViv<-table(muestra$FamDirViv,muestra$CuotMora)
plot(CuotMora~FamDirViv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Cantidad de familiares directos que viven con el postulante",
     main="Cantidad de familiares directos que viven con el postulante vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t7_R_FamDirViv.pdf")
t7_R_FamDirViv<-table(muestra$FamDirViv,muestra$Riesgo)
plot(Riesgo~FamDirViv,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Cantidad de familiares directos que viven con el postulante",
     main="Cantidad de familiares directos que viven con el postulante vs Riesgo")
dev.off()

pdf("t7_D_FamDirViv.pdf")
t7_D_FamDirViv<-table(muestra$FamDirViv,muestra$Default)
plot(Default~FamDirViv,data = muestra,col=coloresCM,ylab="Default",xlab="Cantidad de familiares directos que viven con el postulante",
     main="Cantidad de familiares directos que viven con el postulante vs Default")
dev.off()

#8La madre aun vive###############################################################################################################################
pdf("t8MadViv.pdf")
t8MadViv<-table(muestra$MadViv,muestra$CuotMora)
plot(CuotMora~MadViv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="La madre vive",main="La madre vive vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t8_R_MadViv.pdf")
t8_R_MadViv<-table(muestra$MadViv,muestra$Riesgo)
plot(Riesgo~MadViv,data = muestra,col=coloresCM,ylab="Riesgo",main="La madre vive vs Riesgo",xlab="La madre vive")
dev.off()

pdf("t8_D_MadViv.pdf")
t8_D_MadViv<-table(muestra$MadViv,muestra$Default)
plot(Default~MadViv,data = muestra,col=coloresCM,ylab="Default",main="La madre vive vs Default",xlab="La madre vive")
dev.off()

#9El padre aun vive###############################################################################################################################
pdf("t9PadViv.pdf")
t9PadViv<-table(muestra$PadViv,muestra$CuotMora)
plot(CuotMora~PadViv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="El padre vive vs Máximas cuotas en mora alcanzadas",xlab="El padre vive")
dev.off()

pdf("t9_R_PadViv.pdf")
t9_R_PadViv<-table(muestra$PadViv,muestra$Riesgo)
plot(Riesgo~PadViv,data = muestra,col=coloresCM,ylab="Riesgo",main="El padre vive vs Riesgo",xlab="El padre vive")
dev.off()

pdf("t9_D_PadViv.pdf")
t9_D_PadViv<-table(muestra$PadViv,muestra$Default)
plot(Default~PadViv,data = muestra,col=coloresCM,ylab="Default",main="El padre vive vs Default",xlab="El padre vive")
dev.off()

#10El postulante vive en casa de los padres########################################################################################################
pdf("t10VivPad.pdf")
t10VivPad<-table(muestra$VivPad,muestra$CuotMora)
plot(CuotMora~VivPad,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="El postulante vive en casa de los padres vs Máximas cuotas en mora alcanzadas",xlab="El postulante vive en casa de los padres")
dev.off()

pdf("t10_R_VivPad.pdf")
t10_R_VivPad<-table(muestra$VivPad,muestra$Riesgo)
plot(Riesgo~VivPad,data = muestra,col=coloresCM,ylab="Riesgo",main="El postulante vive en casa de los padres vs Riesgo",xlab="El postulante vive en casa de los padres")
dev.off()

pdf("t10_D_VivPad.pdf")
t10_D_VivPad<-table(muestra$VivPad,muestra$Default)
plot(Default~VivPad,data = muestra,col=coloresCM,ylab="Default",main="El postulante vive en casa de los padres vs Default",xlab="El postulante vive en casa de los padres")
dev.off()

#11Cantidad de hermanos############################################################################################################################
pdf("t11Herm.pdf")
t11Herm<-table(muestra$Herm,muestra$CuotMora)
plot(CuotMora~Herm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",main="Cantidad de hermanos vs Máximas cuotas en mora alcanzadas",xlab="Cantidad de hermanos")
dev.off()

pdf("t11_R_Herm.pdf")
t11_R_Herm<-table(muestra$Herm,muestra$Riesgo)
plot(Riesgo~Herm,data = muestra,col=coloresCM,ylab="Riesgo",main="Cantidad de hermanos vs Riesgo",xlab="Cantidad de hermanos")
dev.off()

pdf("t11_D_Herm.pdf")
t11_D_Herm<-table(muestra$Herm,muestra$Default)
plot(Default~Herm,data = muestra,col=coloresCM,ylab="Default",main="Cantidad de hermanos vs Default",xlab="Cantidad de hermanos")
dev.off()

#12Lugar entre los hermanos########################################################################################################################
pdf("t12LugHerm.pdf")
t12LugHerm<-table(muestra$LugHerm,muestra$CuotMora)
plot(CuotMora~LugHerm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Lugar entre los hermanos vs Máximas cuotas en mora alcanzadas",xlab="Lugar entre los hermanos")
dev.off()

pdf("t12_R_LugHerm.pdf")
t12_R_LugHerm<-table(muestra$LugHerm,muestra$Riesgo)
plot(Riesgo~LugHerm,data = muestra,col=coloresCM,ylab="Riesgo",main="Lugar entre los hermanos vs Riesgo",xlab="Lugar entre los hermanos")
dev.off()

pdf("t12_D_LugHerm.pdf")
t12_D_LugHerm<-table(muestra$LugHerm,muestra$Default)
plot(Default~LugHerm,data = muestra,col=coloresCM,ylab="Default",main="Lugar entre los hermanos vs Default",xlab="Lugar entre los hermanos")
dev.off()

#13Cantidad de hijos###############################################################################################################################
pdf("t13Hijos.pdf")
t13Hijos<-table(muestra$Hijos,muestra$CuotMora)
plot(CuotMora~Hijos,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cantidad de hijos vs Máximas cuotas en mora alcanzadas",xlab="Cantidad de hijos")
dev.off()

pdf("t13_R_Hijos.pdf")
t13_R_Hijos<-table(muestra$Hijos,muestra$Riesgo)
plot(Riesgo~Hijos,data = muestra,col=coloresCM,ylab="Riesgo",main="Cantidad de hijos vs Riesgo",xlab="Cantidad de hijos")
dev.off()

pdf("t13_D_Hijos.pdf")
t13_D_Hijos<-table(muestra$Hijos,muestra$Default)
plot(Default~Hijos,data = muestra,col=coloresCM,ylab="Default",main="Cantidad de hijos vs Default",xlab="Cantidad de hijos")
dev.off()

#14Promedio de edad de los hijos###################################################################################################################
table(muestra$PromEdadH)
summary(muestra$PromEdadH)
sd(muestra$PromEdadH)
pdf("h15PromEdadH.pdf")
hEdadH<-hist(muestra$PromEdadH, main = "Histograma: Edad promedio de los hijos", ylab = "Frecuencia", 
             xlab = "Edad promedio de los hijos", xlim = c(0,14),ylim = c(0,250))
dev.off()

###
pdf("t14PromEdadH.pdf")
t14PromEdadH<-table(muestra$R_PromEdadH,muestra$CuotMora)
plot(CuotMora~R_PromEdadH,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Promedio de edad de los hijos del postulante vs Máximas cuotas en mora alcanzadas",xlab="Promedio de edad de los hijos")
dev.off()

pdf("t14_R_PromEdadH.pdf")
t14_R_PromEdadH<-table(muestra$R_PromEdadH,muestra$Riesgo)
plot(Riesgo~R_PromEdadH,data = muestra,col=coloresCM,ylab="Riesgo",
     main="Promedio de edad de los hijos del postulante vs Riesgo",xlab="Promedio de edad de los hijos")
dev.off()

pdf("t14_D_PromEdadH.pdf")
t14_D_PromEdadH<-table(muestra$R_PromEdadH,muestra$Default)
plot(Default~R_PromEdadH,data = muestra,col=coloresCM,ylab="Default",
     main="Promedio de edad de los hijos del postulante vs Default",xlab="Promedio de edad de los hijos")
dev.off()

#15Estrato###########################################################################################################################################
pdf("t15Estrato.pdf")
t15Estrato<-table(muestra$R_Estrato,muestra$CuotMora)
plot(CuotMora~R_Estrato,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Estrato vs Máximas cuotas en mora alcanzadas",xlab="Estrato")
dev.off()

pdf("t15_R_Estrato.pdf")
t15_R_Estrato<-table(muestra$R_Estrato,muestra$Riesgo)
plot(Riesgo~R_Estrato,data = muestra,col=coloresCM,ylab="Riesgo",main="Estrato vs Riesgo",xlab="Estrato")
dev.off()

pdf("t15_D_Estrato.pdf")
t15_D_Estrato<-table(muestra$R_Estrato,muestra$Default)
plot(Default~R_Estrato,data = muestra,col=coloresCM,ylab="Default",main="Estrato vs Default",xlab="Estrato")
dev.off()

#16Tipo de vivienda##################################################################################################################################
pdf("t16TipViv.pdf")
t16TipViv<-table(muestra$TipViv,muestra$CuotMora)
plot(CuotMora~TipViv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Tipo de Vivienda vs Máximas cuotas en mora alcanzadas",xlab="Tipo de Vivienda")
dev.off()

pdf("t16_R_TipViv.pdf")
t16_R_TipViv<-table(muestra$TipViv,muestra$Riesgo)
plot(Riesgo~TipViv,data = muestra,col=coloresCM,ylab="Riesgo",main="Tipo de Vivienda vs Riesgo",xlab="Tipo de Vivienda")
dev.off()

pdf("t16_D_TipViv.pdf")
t16_D_TipViv<-table(muestra$TipViv,muestra$Default)
plot(Default~TipViv,data = muestra,col=coloresCM,ylab="Default",main="Tipo de Vivienda vs Default",xlab="Tipo de Vivienda")
dev.off()

#17Quien responde economicamente por el postulante?################################################################################################
pdf("t17RespEcnm.pdf")
t17RespEcnm<-table(muestra$RespEcnm,muestra$CuotMora)
plot(CuotMora~RespEcnm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Quien responde economicamente por el postulante? vs Máximas cuotas en mora alcanzadas",xlab="Quien responde economicamente por el postulante?")
dev.off()

pdf("t17_R_RespEcnm.pdf")
t17_R_RespEcnm<-table(muestra$RespEcnm,muestra$Riesgo)
plot(Riesgo~RespEcnm,data = muestra,col=coloresCM,ylab="Riesgo",main="Quien responde economicamente por el postulante? vs Riesgo",
     xlab="Quien responde economicamente por el postulante?")
dev.off()

pdf("t17_D_RespEcnm.pdf")
t17_D_RespEcnm<-table(muestra$RespEcnm,muestra$Default)
plot(Default~RespEcnm,data = muestra,col=coloresCM,ylab="Default",xlab="Quien responde economicamente por el postulante?",
     main="Quien responde economicamente por el postulante? vs Default")
dev.off()

#18Cantidad de personas dependientes del postulante################################################################################################
pdf("t18PersDep.pdf")
t18PersDep<-table(muestra$PersDep,muestra$CuotMora)
plot(CuotMora~PersDep,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cantidad de personas dependientes del postulante vs Máximas cuotas en mora alcanzadas",xlab="Cantidad de personas dependientes del postulante")
dev.off()

pdf("t18_R_PersDep.pdf")
t18_R_PersDep<-table(muestra$PersDep,muestra$Riesgo)
plot(Riesgo~PersDep,data = muestra,col=coloresCM,ylab="Riesgo",main="Cantidad de personas dependientes del postulante vs Riesgo",
     xlab="Cantidad de personas dependientes del postulante")
dev.off()

pdf("t18_D_PersDep.pdf")
t18_D_PersDep<-table(muestra$PersDep,muestra$Default)
plot(Default~PersDep,data = muestra,col=coloresCM,ylab="Default",xlab="Cantidad de personas dependientes del postulante"
     ,main="Cantidad de personas dependientes del postulante vs Default")
dev.off()

#19Estado laboral del postulante ##################################################################################################################
pdf("t19EstLabPost.pdf")
t19EstLabPost<-table(muestra$EstLabPost,muestra$CuotMora)
plot(CuotMora~EstLabPost,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Estado laboral del postulante vs Máximas cuotas en mora alcanzadas",xlab="Estado laboral del postulante")
dev.off()

pdf("t19_R_EstLabPost.pdf")
t19_R_EstLabPost<-table(muestra$EstLabPost,muestra$Riesgo)
plot(Riesgo~EstLabPost,data = muestra,col=coloresCM,ylab="Riesgo",main="Estado laboral del postulante vs Riesgo",xlab="Estado laboral del postulante")
dev.off()

pdf("t19_D_EstLabPost.pdf")
t19_D_EstLabPost<-table(muestra$EstLabPost,muestra$Default)
plot(Default~EstLabPost,data = muestra,col=coloresCM,ylab="Default",main="Estado laboral del postulante vs Default",xlab="Estado laboral del postulante")
dev.off()

#20Alta calidad #####################################################################################################################
pdf("t20AltaCalidad.pdf")
t20AltaCalidad<-table(muestra$AltaCalidad,muestra$CuotMora)
plot(CuotMora~AltaCalidad,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Alta Calidad vs Máximas cuotas en mora alcanzadas",xlab="Alta Calidad IES")
dev.off()

pdf("t20_R_AltaCalidad.pdf")
t20_R_AltaCalidad<-table(muestra$AltaCalidad,muestra$Riesgo)
plot(Riesgo~AltaCalidad,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Alta Calidad IES"
     ,main="Alta Calidad vs Riesgo")
dev.off()

pdf("t20_D_AltaCalidad.pdf")
t20_D_AltaCalidad<-table(muestra$AltaCalidad,muestra$Default)
plot(Default~AltaCalidad,data = muestra,col=coloresCM,ylab="Default",xlab="Alta Calidad IES",main="Alta Calidad vs Default")
dev.off()

#21QSapiens IES######################################################################################################################################
pdf("t21QSapiens.pdf")
t21QSapiens<-table(muestra$Qsapiens,muestra$CuotMora)
plot(CuotMora~Qsapiens,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Ranking universitario Q Sapiens",
     main="Q Sapiens vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t21_R_QSapiens.pdf")
t21_R_QSapiens<-table(muestra$Qsapiens,muestra$Riesgo)
plot(Riesgo~Qsapiens,data = muestra,col=coloresCM,ylab="Riesgo",main="Q Sapiens vs Riesgo",xlab="Ranking universitario Q Sapiens")
dev.off()

pdf("t21_D_QSapiens.pdf")
t21_D_QSapiens<-table(muestra$Qsapiens,muestra$Default)
plot(Default~Qsapiens,data = muestra,col=coloresCM,ylab="Default",main="Q Sapiens vs Default",xlab="Ranking universitario Q Sapiens")
dev.off()

#22Nivel académico#################################################################################################################################
pdf("t22NivAcdm.pdf")
t22NivAcdm<-table(muestra$NivAcdm, muestra$CuotMora)
plot(CuotMora~NivAcdm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Nivel académico del programa a financiar vs Máximas cuotas en mora alcanzadas",xlab="Nivel académico del programa a financiar")
dev.off()

pdf("t22_R_NivAcdm.pdf")
t22_R_NivAcdm<-table(muestra$NivAcdm, muestra$Riesgo)
plot(Riesgo~NivAcdm,data = muestra,col=coloresCM,ylab="Riesgo",main="Nivel académico del programa a financiar vs Riesgo",xlab="Nivel académico del programa a financiar")
dev.off()

pdf("t22_D_NivAcdm.pdf")
t22_D_NivAcdm<-table(muestra$NivAcdm, muestra$Default)
plot(Default~NivAcdm, data = muestra,col = coloresCM, ylab ="Default",main = "Nivel académico del programa a financiar vs Default",xlab="Nivel académico del programa a financiar")
dev.off()

#22Nivel formación#################################################################################################################################
pdf("t22NivFrmc.pdf")
t22NivFrmc<-table(muestra$NivFrmc,muestra$CuotMora)
plot(CuotMora~NivFrmc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Nivel formación del programa a financiar vs Máximas cuotas en mora alcanzadas",xlab="Nivel formación del programa a financiar")
dev.off()

pdf("t22_R_NivFrmc.pdf")
t22_R_NivFrmc<-table(muestra$NivFrmc,muestra$Riesgo)
plot(Riesgo~NivFrmc,data = muestra,col=coloresCM,ylab="Riesgo",main="Nivel formación vs Riesgo",xlab="Nivel formación del programa a financiar")
dev.off()

pdf("t22_D_NivFrmc.pdf")
t22_D_NivFrmc<-table(muestra$NivFrmc,muestra$Default)
plot(Default~NivFrmc,data = muestra,col=coloresCM,ylab="Default",main="Nivel formación vs Default",xlab="Nivel formación del programa a financiar")
dev.off()

#23Metodología Programa académico##################################################################################################################
pdf("t23Metdlg.pdf")
t23Metdlg<-table(muestra$Metdlg,muestra$CuotMora)
plot(CuotMora~Metdlg,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Metodología del programa académico vs Máximas cuotas en mora alcanzadas",xlab="Metodología del programa académico")
dev.off()

pdf("t23_R_Metdlg.pdf")
t23_R_Metdlg<-table(muestra$Metdlg,muestra$Riesgo)
plot(Riesgo~Metdlg,data = muestra,col=coloresCM,ylab="Riesgo",main="Metodología del programa académico vs Riesgo",xlab="Metodología del programa académico")
dev.off()

pdf("t23_D_Metdlg.pdf")
t23_D_Metdlg<-table(muestra$Metdlg,muestra$Default)
plot(Default~Metdlg,data = muestra,col=coloresCM,ylab="Default",main="Metodología del programa académico vs Default",xlab="Metodología del programa académico")
dev.off()

#24Area conocimiento###############################################################################################################################
pdf("t24AreaCncm.pdf")
t24AreaCncm<-table(muestra$AreaCncm,muestra$CuotMora)
plot(CuotMora~AreaCncm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Área de conocimiento del programa académico",
     main="Area de conocimiento vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t24_R_AreaCncm.pdf")
t24_R_AreaCncm<-table(muestra$AreaCncm,muestra$Riesgo)
plot(Riesgo~AreaCncm,data = muestra,col=coloresCM,ylab="Riesgo",main="Area de conocimiento del programa académicovs Riesgo",xlab="Área de conocimiento del programa académico")
dev.off()

pdf("t24_D_AreaCncm.pdf")
t24_D_AreaCncm<-table(muestra$AreaCncm,muestra$Default)
plot(Default~AreaCncm,data = muestra,col=coloresCM,ylab="Default",main="Area de conocimiento del programa académicovs Default",xlab="Área de conocimiento del programa académico")
dev.off()

#25Periodos académicos#############################################################################################################################
table(muestra$PrdPA)
summary(muestra$PrdPA)
sd(muestra$PrdPA)
pdf("h25PrdPA.pdf")
hPrdPA<-hist(muestra$PrdPA, main = "Histograma: Periodos programa académico",xlab = "Periodos programa académico",
             ylab = "Frecuencia", ylim = c(0, 120))
dev.off()

pdf("t25PrdPA.pdf")
t25PrdPA<-table(muestra$PrdPA,muestra$CuotMora)
plot(CuotMora~PrdPA,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Cantidad de periodos del programa académico",
     main="Cantidad de periodos del programa académico vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t25_R_PrdPA.pdf")
t25_R_PrdPA<-table(muestra$PrdPA,muestra$Riesgo)
plot(Riesgo~PrdPA,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Cantidad de periodos del programa académico",main="Cantidad de periodos del programa académico vs Riesgo")
dev.off()

pdf("t25_D_PrdPA.pdf")
t25_D_PrdPA<-table(muestra$PrdPA,muestra$Default)
plot(Default~PrdPA,data = muestra,col=coloresCM,ylab="Default",xlab="Cantidad de periodos del programa académico",main="Cantidad de periodos del programa académico vs Default")
dev.off()

#rangos

pdf("t25PrdPA_R.pdf")
t25PrdPA_R<-table(muestra$PrdPA.binned,muestra$CuotMora)
plot(CuotMora~PrdPA.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Cantidad de periodos del programa académico",
     main="Cantidad de periodos del programa académico vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t25_R_PrdPA_R.pdf")
t25_R_PrdPA_R<-table(muestra$PrdPA.binned,muestra$Riesgo)
plot(Riesgo~PrdPA.binned,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Cantidad de periodos del programa académico",main="Cantidad de periodos del programa académico vs Riesgo")
dev.off()

pdf("t25_D_PrdPA_R.pdf")
t25_D_PrdPA_r<-table(muestra$PrdPA.binned,muestra$Default)
plot(Default~PrdPA.binned,data = muestra,col=coloresCM,ylab="Default",xlab="Cantidad de periodos del programa académico",main="Cantidad de periodos del programa académico vs Default")
dev.off()

#26Valor matrícula semestral#######################################################################################################################
table(muestra$Mtrc)
#hay dos valores atipicos: 26.000.000 y 22.400.000 COP, se sabe porque esas universidades no son tan caras; revision en SF: sea justo a 2.600.0000
#la otra postulacion parece estar correcta porque el desembolso fue por 17.000.000 COP, la persona aun es estudiante
summary(muestra$Mtrc)
sd(muestra$Mtrc)
#la distribucion del valor de la matricula se encuentra entre 100.000 COP (100k) y 14.600.000, con una media de 5.023.091,
#mediana de 3.798.000 y una desviacion  estandar de 2979964
pdf("h26Mtrc.pdf")
hMtrc<-hist(muestra$Mtrc, main = "Histograma: Valor matrícula semestral",xlab = "Valor matrícula semestral (COP)", 
            ylab = "Frecuencia", xlim = c(0,20000000), ylim = c(0,140))
dev.off()

##
pdf("t26Mtrc.pdf")
t26Mtrc<-table(muestra$Mtrc,muestra$CuotMora)
plot(CuotMora~Mtrc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Valor matrícula semestral (COP)",
     main="Valor matrícula semestral vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t26_R_Mtrc.pdf")
t26_R_Mtrc<-table(muestra$Mtrc,muestra$Riesgo)
plot(Riesgo~Mtrc,data = muestra,col=coloresCM,ylab="Riesgo",main="Valor matrícula semestral vs Riesgo",
     xlab="Valor matrícula semestral (COP)")
dev.off()

pdf("t26_D_Mtrc.pdf")
t26_D_Mtrc<-table(muestra$Mtrc,muestra$Default)
plot(Default~Mtrc,data = muestra,col=coloresCM,ylab="Default",main="Valor matrícula semestral vs Default",
     xlab="Valor matrícula semestral (COP)")
dev.off()

#rangos

pdf("t26MtrcR.pdf")
t26MtrcR<-table(muestra$Mtrc.binned,muestra$CuotMora)
plot(CuotMora~Mtrc.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Valor matrícula semestral (COP)",
     main="Valor matrícula semestral vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t26_R_MtrcR.pdf")
t26_R_MtrcR<-table(muestra$Mtrc.binned,muestra$Riesgo)
plot(Riesgo~Mtrc.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Valor matrícula semestral vs Riesgo ",
     xlab="Valor matrícula semestral (COP)")
dev.off()

pdf("t26_D_MtrcR.pdf")
t26_D_MtrcR<-table(muestra$Mtrc.binned,muestra$Default)
plot(Default~Mtrc.binned,data = muestra,col=coloresCM,ylab="Default",main="Valor matrícula semestral vs Default",
     xlab="Valor matrícula semestral (COP)")
dev.off()

#27Cultura de pago#################################################################################################################################
table(muestra$CultPag)
summary(muestra$CultPag)
sd(muestra$CultPag)
#la persona que no paga CP, paga cuotas desde la epoca dae estudio
#la distribucion de las CP se encuentra entre 0 COP y 300.000 COP, con una media de 30.47 COP, 
#mediana de 22.400 COP y una desviacion estandar de 31 .772 COP
pdf("h27CP.pdf")
hCP<-hist(muestra$CultPag, breaks=12, main = "Histograma: Cultura de pago",xlab ="Cultura de pago (COP)",ylab="Frecuencia" )
dev.off()

pdf("t27CP.pdf")
t27CP<-table(muestra$CultPag,muestra$CuotMora)
plot(CuotMora~CultPag,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cultura de pago vs Máximas cuotas en mora alcanzadas",xlab="Cultura de pago (COP)")
dev.off()

pdf("t27_R_CP.pdf")
t27_R_CP<-table(muestra$CultPag,muestra$Riesgo)
plot(Riesgo~CultPag,data = muestra,col=coloresCM,ylab="Riesgo",main="Cultura de pago vs Riesgo",xlab="Cultura de pago (COP)")
dev.off()

pdf("t27_D_CP.pdf")
t27_D_CP<-table(muestra$CultPag,muestra$Default)
plot(Default~CultPag,data = muestra,col=coloresCM,ylab="Default",main="Cultura de pago vs Default",xlab="Cultura de pago (COP)")
dev.off()

#rangos
pdf("t27CP_R.pdf")
t27CP_R<-table(muestra$CultPag.binned,muestra$CuotMora)
plot(CuotMora~CultPag.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cultura de pago vs Máximas cuotas en mora alcanzadas",xlab="Cultura de pago (COP)")
dev.off()

pdf("t27_R_CP_R.pdf")
t27_R_CP_R<-table(muestra$CultPag.binned,muestra$Riesgo)
plot(Riesgo~CultPag.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Cultura de pago vs Riesgo",xlab="Cultura de pago (COP)")
dev.off()

pdf("t27_D_CP_R.pdf")
t27_D_CP_R<-table(muestra$CultPag.binned,muestra$Default)
plot(Default~CultPag.binned,data = muestra,col=coloresCM,ylab="Default",main="Cultura de pago vs Default",xlab="Cultura de pago (COP)")
dev.off()

#28Porcentaje salario comprometido#################################################################################################################
table(muestra$PrcntCuota)
summary(muestra$PrcntCuota)
sd(muestra$PrcntCuota)
#la distribucion del % de salario compromentido se encuentra entre 4.42% y 20%, con una media de 16.56%, mediana de 14.97%
#y una desviacion estandar de 3.08%.
pdf("h28PrcntCuota.pdf")
hPC<-hist(muestra$PrcntCuota, main = "Histograma: Porcentaje del salario comprometido",
          xlab = "Porcentaje del salario comprometido",ylab = "Frecuencia", xlim = c(0,20))
dev.off()

pdf("t28PrcntCuota.pdf")
t28PrcntCuota<-table(muestra$PrcntCuota,muestra$CuotMora)
plot(CuotMora~PrcntCuota,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Porcentaje salario comprometido vs Máximas cuotas en mora alcanzadas",xlab="Porcentaje salario comprometido")
dev.off()

pdf("t28_R_PrcntCuota.pdf")
t28_R_PrcntCuota<-table(muestra$PrcntCuota,muestra$Riesgo)
plot(Riesgo~PrcntCuota,data = muestra,col=coloresCM,ylab="Riesgo",main="Porcentaje salario comprometido vs Riesgo",xlab="Porcentaje salario comprometido")
dev.off()

pdf("t28_D_PrcntCuota.pdf")
t28_D_PrcntCuota<-table(muestra$PrcntCuota,muestra$Default)
plot(Default~PrcntCuota,data = muestra,col=coloresCM,ylab="Default",main="Porcentaje salario comprometido vs Default",xlab="Porcentaje salario comprometido")
dev.off()

#rangos

pdf("t28PrcntCuotaR.pdf")
t28PrcntCuotaR<-table(muestra$PrcntCuota.binned,muestra$CuotMora)
plot(CuotMora~PrcntCuota.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Porcentaje salario comprometido",
     main="Porcentaje salario comprometido vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t28_R_PrcntCuotaR.pdf")
t28_R_PrcntCuotaR<-table(muestra$PrcntCuota.binned,muestra$Riesgo)
plot(Riesgo~PrcntCuota.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Porcentaje salario comprometido vs Riesgo",xlab="Porcentaje salario comprometido")
dev.off()

pdf("t28_D_PrcntCuotaR.pdf")
t28_D_PrcntCuotaR<-table(muestra$PrcntCuota.binned,muestra$Default)
plot(Default~PrcntCuota.binned,data = muestra,col=coloresCM,ylab="Default",main="Porcentaje salario comprometido vs Default",xlab="Porcentaje salario comprometido")
dev.off()

#29Cuotas pactadas#################################################################################################################################
table(muestra$CuotPact)
summary(muestra$CuotPact)
sd(muestra$CuotPact)
#la distribucion de las cuotas pactadas se encuentra entre 16 y 99, con una media de 57.6, mediana de 57
#y una desviacion estandar de 17.77
pdf("h29CuotPact.pdf")
hRP<-hist(muestra$CuotPact, breaks=6, main = "Histograma: Cuotas pactadas",
          xlab =  "Cuotas pactadas", ylab = "Frecuencia", xlim = c(0,100), ylim = c(0,60))
dev.off()


pdf("t29CuotPact.pdf")
t29CuotPact<-table(muestra$CuotPact,muestra$CuotMora)
plot(CuotMora~CuotPact,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cuotas pactadas vs Máximas cuotas en mora alcanzadas",xlab="Cuotas pactadas")
dev.off()

pdf("t29_R_CuotPact.pdf")
t29_R_CuotPact<-table(muestra$CuotPact,muestra$Riesgo)
plot(Riesgo~CuotPact,data = muestra,col=coloresCM,ylab="Riesgo",main="Cuotas pactadas vs Riesgo",xlab="Cuotas pactadas")
dev.off()

pdf("t29_D_CuotPact.pdf")
t29_D_CuotPact<-table(muestra$CuotPact,muestra$Default)
plot(Default~CuotPact,data = muestra,col=coloresCM,ylab="Default",main="Cuotas pactadas vs Default",xlab="Cuotas pactadas")
dev.off()

#rangos
pdf("t29CuotPact.pdf")
t29CuotPact<-table(muestra$CuotPact.binned,muestra$CuotMora)
plot(CuotMora~CuotPact.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Cuotas pactadas vs Máximas cuotas en mora alcanzadas",xlab="Cuotas pactadas")
dev.off()

pdf("t29_R_CuotPact.pdf")
t29_R_CuotPact<-table(muestra$CuotPact.binned,muestra$Riesgo)
plot(Riesgo~CuotPact.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Cuotas pactadas vs Riesgo",xlab="Cuotas pactadas")
dev.off()

pdf("t29_D_CuotPact.pdf")
t29_D_CuotPact<-table(muestra$CuotPact.binned,muestra$Default)
plot(Default~CuotPact.binned,data = muestra,col=coloresCM,ylab="Default",main="Cuotas pactadas vs Default",xlab="Cuotas pactadas")
dev.off()

#30Puntaje prueba Logros-verbal###################################################################################################################
pdf("h30L_verb.pdf")
hRP<-hist(muestra$L_verb, main = "Histograma: Puntaje prueba Logros-verbal",xlab ="Puntaje prueba Logros-verbal", 
          ylab = "Frecuencia", ylim = c(0,60))
dev.off()

pdf("t30L_verb.pdf")
t30L_verb<-table(muestra$L_verb,muestra$CuotMora)
plot(CuotMora~L_verb,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Logros-verbal vs Máximas cuotas en mora alcanzadas",xlab="Cuotas pactadas")
dev.off()

pdf("t30_R_L_verb.pdf")
t30_R_L_verb<-table(muestra$L_verb,muestra$Riesgo)
plot(Riesgo~L_verb,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-verbal vs Riesgo",xlab="Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_D_L_verb.pdf")
t30_D_L_verb<-table(muestra$L_verb,muestra$Default)
plot(Default~L_verb,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-verbal vs Default",xlab="Puntaje prueba Logros-verbal")
dev.off()

#rangos

pdf("t30L_verbR.pdf")
t30L_verbR<-table(muestra$L_verb.binned,muestra$CuotMora)
plot(CuotMora~L_verb.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Logros-verbal vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_R_L_verbR.pdf")
t30_R_L_verbR<-table(muestra$L_verb.binned,muestra$Riesgo)
plot(Riesgo~L_verb.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-verbal vs Riesgo",xlab="Puntaje prueba Logros-verbal")
dev.off()

pdf("t30_D_L_verbR.pdf")
t30_D_L_verbR<-table(muestra$L_verb.binned,muestra$Default)
plot(Default~L_verb.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-verbal vs Default",xlab="Puntaje prueba Logros-verbal")
dev.off()

#31Puntaje prueba Logros-espacial#################################################################################################################
pdf("h31L_esp.pdf")
hRP<-hist(muestra$L_esp, main = "Histograma: Puntaje prueba Logros-espacial",xlab ="Puntaje prueba Logros-espacial", ylab = "Frecuencia")
dev.off()

pdf("t31L_esp.pdf")
t31L_esp<-table(muestra$L_esp,muestra$CuotMora)
plot(CuotMora~L_esp,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Logros-espacial vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_R_L_esp.pdf")
t31_R_L_esp<-table(muestra$L_esp,muestra$Riesgo)
plot(Riesgo~L_esp,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-espacial vs Riesgo",xlab="Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_D_L_esp.pdf")
t31_D_L_esp<-table(muestra$L_esp,muestra$Default)
plot(Default~L_esp,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-espacial vs Default",xlab="Puntaje prueba Logros-espacial")
dev.off()

#rangos

pdf("t31L_espR.pdf")
t31L_espR<-table(muestra$L_esp.binned,muestra$CuotMora)
plot(CuotMora~L_esp.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Logros-espacial vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_R_L_espR.pdf")
t31_R_L_espR<-table(muestra$L_esp.binned,muestra$Riesgo)
plot(Riesgo~L_esp.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-espacial vs Riesgo",xlab="Puntaje prueba Logros-espacial")
dev.off()

pdf("t31_D_L_espR.pdf")
t31_D_L_espR<-table(muestra$L_esp.binned,muestra$Default)
plot(Default~L_esp.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-espacial vs Default",xlab="Puntaje prueba Logros-espacial")
dev.off()

#32Puntaje prueba Logros-numerico#################################################################################################################
table(muestra$L_num)
summary(muestra$L_num)
sd(muestra$L_num)
#la distribucion del Puntaje prueba Logros: numerico se encuentra entre 2 y 11 , con una media de 7.7, mediana de 8 y una desviacion estandar de 2.1. 
pdf("h32L_num.pdf")
hRP<-hist(muestra$L_num, main = "Histograma: Puntaje prueba Logros-numérico",xlab ="Puntaje prueba Logros-numérico", ylab = "Frecuencia", xlim = c(2,12))
dev.off()

pdf("t32L_num.pdf")
t32L_num<-table(muestra$L_num,muestra$CuotMora)
plot(CuotMora~L_num,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-numerico",
     main="Puntaje prueba Logros-numerico vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t32_R_L_num.pdf")
t32_R_L_num<-table(muestra$L_num,muestra$Riesgo)
plot(Riesgo~L_num,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-numerico vs Riesgo",xlab="Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_D_L_num.pdf")
t32_D_L_num<-table(muestra$L_num,muestra$Default)
plot(Default~L_num,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-numerico vs Default",xlab="Puntaje prueba Logros-numerico")
dev.off()

#rangos

pdf("t32L_numR.pdf")
t32L_numR<-table(muestra$L_num.binned,muestra$CuotMora)
plot(CuotMora~L_num.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-numerico",
     main="Puntaje prueba Logros-numerico vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t32_R_L_numR.pdf")
t32_R_L_numR<-table(muestra$L_num.binned,muestra$Riesgo)
plot(Riesgo~L_num.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros-numerico vs Riesgo",xlab="Puntaje prueba Logros-numerico")
dev.off()

pdf("t32_D_L_numR.pdf")
t32_D_L_numR<-table(muestra$L_num.binned,muestra$Default)
plot(Default~L_num.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros-numerico vs Default",xlab="Puntaje prueba Logros-numerico")
dev.off()

#33Puntaje prueba Logros sin verbal#############################################################################################################
table(muestra$L_sinV)
summary(muestra$L_sinV)
sd(muestra$L_sinV)
#la distribucion de la prueba de Logros numerico y espacial se encuentra entre 3 y 18, con una media de 11.5, mediana de 12 y una desviacion estandar de 2.9
pdf("h33L_sinV.pdf")
hL_V<-hist(muestra$L_sinV, main = "Histograma: Puntaje prueba Logros-numérico y espacial",
           xlab = "Puntaje prueba Logros-numérico y espacial",ylab = "Frecuencia", xlim = c(0,20))
dev.off()

pdf("t33L_sinV.pdf")
t33L_sinV<-table(muestra$L_sinV,muestra$CuotMora)
plot(CuotMora~L_sinV,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t33_R_L_sinV.pdf")
t33_R_L_sinV<-table(muestra$L_sinV,muestra$Riesgo)
plot(Riesgo~L_sinV,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Riesgo")
dev.off()

pdf("t33_D_L_sinV.pdf")
t33_D_L_sinV<-table(muestra$L_sinV,muestra$Default)
plot(Default~L_sinV,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Default")
dev.off()

#rangos

pdf("t33L_sinV_R.pdf")
t33L_sinV_R<-table(muestra$L_sinV.binned,muestra$CuotMora)
plot(CuotMora~L_sinV.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t33_R_L_sinV_R.pdf")
t33_R_L_sinV_R<-table(muestra$L_sinV.binned,muestra$Riesgo)
plot(Riesgo~L_sinV.binned,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Riesgo")
dev.off()

pdf("t33_D_L_sinV_R.pdf")
t33_D_L_sinV_R<-table(muestra$L_sinV.binned,muestra$Default)
plot(Default~L_sinV.binned,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba Logros-numerico y espacial",
     main="Puntaje prueba Logros-numerico y espacial vs Default")
dev.off()

#34Puntaje prueba Logros###########################################################################################################################
table(muestra$Logros)
summary(muestra$Logros)
sd(muestra$Logros)
#la distribucion del Puntaje prueba Logros se encuentra entre 8 y 28 , con una media de 18.9, mediana de 19 y una desviacion estandar de 3.7
pdf("h34Logros.pdf")
hL<-hist(muestra$Logros, main = "Histograma: Puntaje prueba-Logros",xlab = "Puntaje prueba-Logros",ylab = "Frecuencia",
         xlim = c(5,30), ylim = c(0,50))
dev.off()

pdf("t34Logros.pdf")
t34Logros<-table(muestra$Logros,muestra$CuotMora)
plot(CuotMora~Logros,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros",
     main="Puntaje prueba Logros vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t34_R_Logros.pdf")
t34_R_Logros<-table(muestra$Logros,muestra$Riesgo)
plot(Riesgo~Logros,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros vs Riesgo",xlab="Puntaje prueba Logros")
dev.off()

pdf("t34_D_Logros.pdf")
t34_D_Logros<-table(muestra$Logros,muestra$Default)
plot(Default~Logros,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros vs Default",xlab="Puntaje prueba Logros")
dev.off()

#rangos

pdf("t34LogrosR.pdf")
t34LogrosR<-table(muestra$Logros.binned,muestra$CuotMora)
plot(CuotMora~Logros.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Logros",
     main="Puntaje prueba Logros vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t34_R_LogrosR.pdf")
t34_R_LogrosR<-table(muestra$Logros.binned,muestra$Riesgo)
plot(Riesgo~Logros.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Logros vs Riesgo",xlab="Puntaje prueba Logros")
dev.off()

pdf("t34_D_LogrosR.pdf")
t34_D_LogrosR<-table(muestra$Logros.binned,muestra$Default)
plot(Default~Logros.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Logros vs Default",xlab="Puntaje prueba Logros")
dev.off()

#35Puntaje prueba Personalidad-agradabilidad#######################################################################################################
table(muestra$P_agrd)
pdf("h35P_agrd.pdf")
hist(muestra$P_agrd, main = "Histograma: Puntaje prueba personalidad-agradabilidad", 
     xlab = "Puntaje prueba personalidad-agradabilidad", ylab = "Frecuencia",xlim = c(2,7) )
dev.off()

pdf("t35P_agrd.pdf")
t35P_agrd<-table(muestra$P_agrd,muestra$CuotMora)
plot(CuotMora~P_agrd,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-agradabilidad",
     main="Puntaje prueba personalidad-agradabilidad vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t35_R_P_agrd.pdf")
t35_R_P_agrd<-table(muestra$P_agrd,muestra$Riesgo)
plot(Riesgo~P_agrd,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-agradabilidad vs Riesgo",
     xlab="Puntaje prueba personalidad-agradabilidad")
dev.off()

pdf("t35_D_P_agrd.pdf")
t35_D_P_agrd<-table(muestra$P_agrd,muestra$Default)
plot(Default~P_agrd,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-agradabilidad vs Default",
     xlab="Puntaje prueba personalidad-agradabilidad")
dev.off()

#rangos

pdf("t35P_agrdR.pdf")
t35P_agrdR<-table(muestra$P_agrd.binned,muestra$CuotMora)
plot(CuotMora~P_agrd.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-agradabilidad",
     main="Puntaje prueba personalidad-agradabilidad vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t35_R_P_agrdR.pdf")
t35_R_P_agrdR<-table(muestra$P_agrd.binned,muestra$Riesgo)
plot(Riesgo~P_agrd.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-agradabilidad vs Riesgo",
     xlab="Puntaje prueba personalidad-agradabilidad")
dev.off()

pdf("t35_D_P_agrdR.pdf")
t35_D_P_agrdR<-table(muestra$P_agrd.binned,muestra$Default)
plot(Default~P_agrd.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-agradabilidad vs Default",
     xlab="Puntaje prueba personalidad-agradabilidad")
dev.off()

#36Puntaje prueba Personalidad-apertura experiencia################################################################################################
table(muestra$P_apertExp)
pdf("h36P_apertExp.pdf")
hist(muestra$P_apertExp, main = "Histograma: Puntaje prueba personalidad-apertura a la experiencia",
     xlab = "Puntaje prueba personalidad-apertura a la experiencia",ylab = "Frecuencia")
dev.off()

pdf("t36P_apertExp.pdf")
t36P_apertExp<-table(muestra$P_apertExp,muestra$CuotMora)
plot(CuotMora~P_apertExp,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-apertura a la experiencia",
     main="Puntaje prueba personalidad-apertura a la experiencia vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t36_R_P_apertExp.pdf")
t36_R_P_apertExp<-table(muestra$P_apertExp,muestra$Riesgo)
plot(Riesgo~P_apertExp,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-apertura a la experiencia vs Riesgo",
     xlab="Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

pdf("t36_D_P_apertExp.pdf")
t36_D_P_apertExp<-table(muestra$P_apertExp,muestra$Default)
plot(Default~P_apertExp,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-apertura a la experiencia vs Default",
     xlab="Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

#rangos

pdf("t36P_apertExpR.pdf")
t36P_apertExpR<-table(muestra$P_apertExp.binned,muestra$CuotMora)
plot(CuotMora~P_apertExp.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-apertura a la experiencia",
     main="Puntaje prueba personalidad-apertura a la experiencia vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t36_R_P_apertExpR.pdf")
t36_R_P_apertExpR<-table(muestra$P_apertExp.binned,muestra$Riesgo)
plot(Riesgo~P_apertExp.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-apertura a la experiencia vs Riesgo",
     xlab="Puntaje prueba personalidad-apertura a la experiencia")
dev.off()

pdf("t36_D_P_apertExpR.pdf")
t36_D_P_apertExpR<-table(muestra$P_apertExp.binned,muestra$Default)
plot(Default~P_apertExp.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-apertura a la experiencia vs Default",
     xlab="Puntaje prueba personalidad-apertura a la experiencia")
dev.off()


#37Puntaje prueba Personalidad-conciencia##########################################################################################################
table(muestra$P_conc)
pdf("h37P_conc.pdf")
hist(muestra$P_conc, main = "Histograma: Puntaje prueba personalidad-conciencia",
     xlab = "Puntaje prueba personalidad-conciencia",ylab = "Frecuencia")
dev.off()

pdf("t37P_conc.pdf")
t37P_conc<-table(muestra$P_conc,muestra$CuotMora)
plot(CuotMora~P_conc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t37_R_P_conc.pdf")
t37_R_P_conc<-table(muestra$P_conc,muestra$Riesgo)
plot(Riesgo~P_conc,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Riesgo")
dev.off()

pdf("t37_D_P_conc.pdf")
t37_D_P_conc<-table(muestra$P_conc,muestra$Default)
plot(Default~P_conc,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Default")
dev.off()

#rangos
pdf("t37P_concR.pdf")
t37P_concR<-table(muestra$P_conc.binned,muestra$CuotMora)
plot(CuotMora~P_conc.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t37_R_P_concR.pdf")
t37_R_P_concR<-table(muestra$P_conc.binned,muestra$Riesgo)
plot(Riesgo~P_conc.binned,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Riesgo")
dev.off()

pdf("t37_D_P_concR.pdf")
t37_D_P_concR<-table(muestra$P_conc.binned,muestra$Default)
plot(Default~P_conc.binned,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba personalidad-conciencia",
     main="Puntaje prueba personalidad-conciencia vs Default")
dev.off()

#38Puntaje prueba Personalidad-estabilidad emocional###############################################################################################
table(muestra$P_estEmc)
pdf("h38P_estEmc.pdf")
hist(muestra$P_estEmc, main = "Histograma: Puntaje prueba personalidad-estabilidad emocional",
     xlab = "Puntaje prueba personalidad-estabilidad emocional",ylab = "Frecuencia", ylim = c(0,100))
dev.off()

pdf("t38P_estEmc.pdf")
t38P_estEmc<-table(muestra$P_estEmc,muestra$CuotMora)
plot(CuotMora~P_estEmc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t38_R_P_estEmc.pdf")
t38_R_P_estEmc<-table(muestra$P_estEmc,muestra$Riesgo)
plot(Riesgo~P_estEmc,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Riesgo")
dev.off()

pdf("t38_D_P_estEmc.pdf")
t38_D_P_estEmc<-table(muestra$P_estEmc,muestra$Default)
plot(Default~P_estEmc,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Default")
dev.off()

#rangos

pdf("t38P_estEmcR.pdf")
t38P_estEmcR<-table(muestra$P_estEmc.binned,muestra$CuotMora)
plot(CuotMora~P_estEmc.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t38_R_P_estEmcR.pdf")
t38_R_P_estEmcR<-table(muestra$P_estEmc.binned,muestra$Riesgo)
plot(Riesgo~P_estEmc.binned,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Riesgo")
dev.off()

pdf("t38_D_P_estEmcR.pdf")
t38_D_P_estEmcR<-table(muestra$P_estEmc.binned,muestra$Default)
plot(Default~P_estEmc.binned,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba personalidad-estabilidad emocional",
     main="Puntaje prueba personalidad-estabilidad emocional vs Default")
dev.off()

#39Puntaje prueba Personalidad-extraversion########################################################################################################
table(muestra$P_extrv)
pdf("h39P_extrv.pdf")
hist(muestra$P_extrv, main = "Histograma: Puntaje prueba personalidad-extraversion",
     ylab = "Frecuencia", xlab = "Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39P_extrv.pdf")
t39P_extrv<-table(muestra$P_extrv,muestra$CuotMora)
plot(CuotMora~P_extrv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-extraversion",
     main="Puntaje prueba personalidad-extraversion vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t39_R_P_extrv.pdf")
t39_R_P_extrv<-table(muestra$P_extrv,muestra$Riesgo)
plot(Riesgo~P_extrv,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-extraversion vs Riesgo",xlab="Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39_D_P_extrv.pdf")
t39_D_P_extrv<-table(muestra$P_extrv,muestra$Default)
plot(Default~P_extrv,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-extraversion vs Default",xlab="Puntaje prueba personalidad-extraversion")
dev.off()

#rangos
pdf("t39P_extrvR.pdf")
t39P_extrvR<-table(muestra$P_extrv.binned,muestra$CuotMora)
plot(CuotMora~P_extrv.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba personalidad-extraversion",
     main="Puntaje prueba personalidad-extraversion vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t39_R_P_extrvR.pdf")
t39_R_P_extrvR<-table(muestra$P_extrv.binned,muestra$Riesgo)
plot(Riesgo~P_extrv.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba personalidad-extraversion vs Riesgo",xlab="Puntaje prueba personalidad-extraversion")
dev.off()

pdf("t39_D_P_extrvR.pdf")
t39_D_P_extrvR<-table(muestra$P_extrv.binned,muestra$Default)
plot(Default~P_extrv.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba personalidad-extraversion vs Default",xlab="Puntaje prueba personalidad-extraversion")
dev.off()

#40Puntaje prueba Personalidad####################################################################################################################
pdf("h40Pers.pdf")
hist(muestra$Pers, main = "Histograma: Puntaje prueba Personalidad",
     ylab = "Frecuencia", xlab = "Puntaje prueba personalidad", xlim = c(20,35), ylim = c(0,40))
dev.off()

pdf("t40pers.pdf")
t40pers<-table(muestra$Pers,muestra$CuotMora)
plot(CuotMora~Pers,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Personalidad vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Personalidad")
dev.off()

pdf("t40_R_pers.pdf")
t40_R_pers<-table(muestra$Pers,muestra$Riesgo)
plot(Riesgo~Pers,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Personalidad vs Riesgo",xlab="Puntaje prueba Personalidad")
dev.off()

pdf("t40_D_pers.pdf")
t40_D_pers<-table(muestra$Pers,muestra$Default)
plot(Default~Pers,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Personalidad vs Default",xlab="Puntaje prueba Personalidad")
dev.off()

#RANGOS

pdf("t40persR.pdf")
t40persR<-table(muestra$Pers.binned,muestra$CuotMora)
plot(CuotMora~Pers.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba Personalidad vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Personalidad")
dev.off()

pdf("t40_R_persR.pdf")
t40_R_persR<-table(muestra$Pers.binned,muestra$Riesgo)
plot(Riesgo~Pers.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba Personalidad vs Riesgo",xlab="Puntaje prueba Personalidad")
dev.off()

pdf("t40_D_persR.pdf")
t40_D_persR<-table(muestra$Pers.binned,muestra$Default)
plot(Default~Pers.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba Personalidad vs Default",xlab="Puntaje prueba Personalidad")
dev.off()

#41Puntaje prueba intereses-ciencias economicas####################################################################################################
pdf("h41I_cEcnm.pdf")
hist(muestra$I_cEcnm, main = "Histograma: Puntaje prueba intereses-ciencias economicas",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-ciencias economicas")
dev.off()

pdf("t41I_cEcnm.pdf")
t41I_cEcnm<-table(muestra$I_cEcnm,muestra$CuotMora)
plot(CuotMora~I_cEcnm,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-ciencias economicas vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-ciencias economicas")
dev.off()

pdf("t41_R_I_cEcnm.pdf")
t41_R_I_cEcnm<-table(muestra$I_cEcnm,muestra$Riesgo)
plot(Riesgo~I_cEcnm,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias economicas vs Riesgo",
     xlab="Puntaje prueba Intereses-ciencias economicas")
dev.off()

pdf("t41_D_I_cEcnm.pdf")
t41_D_I_cEcnm<-table(muestra$I_cEcnm,muestra$Default)
plot(Default~I_cEcnm,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias economicas vs Default",
     xlab="Puntaje prueba Intereses-ciencias economicas")
dev.off()

#RANGOS
pdf("t41I_cEcnmR.pdf")
t41I_cEcnmR<-table(muestra$I_cEcnm.binned,muestra$CuotMora)
plot(CuotMora~I_cEcnm.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-ciencias economicas vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-ciencias economicas")
dev.off()

pdf("t41_R_I_cEcnmR.pdf")
t41_R_I_cEcnmR<-table(muestra$I_cEcnm.binned,muestra$Riesgo)
plot(Riesgo~I_cEcnm.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias economicas vs Riesgo",
     xlab="Puntaje prueba Intereses-ciencias economicas")
dev.off()

pdf("t41_D_I_cEcnmR.pdf")
t41_D_I_cEcnmR<-table(muestra$I_cEcnm.binned,muestra$Default)
plot(Default~I_cEcnm.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias economicas vs Default")
dev.off()

#42Puntaje prueba intereses-ingenierias y arquitectura####################################################################################################
pdf("h42I_ingArq.pdf")
hist(muestra$I_ingArq, main = "Histograma: Puntaje prueba intereses-ingenierias y arquitectura",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-ingenierias y arquitectura", ylim = c(0, 120))
dev.off()

pdf("t42I_ingArq.pdf")
t42I_ingArq<-table(muestra$I_ingArq,muestra$CuotMora)
plot(CuotMora~I_ingArq,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-ingenierias y arquitectura vs Máximas cuotas en mora alcanzadas",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura")
dev.off()

pdf("t42_R_I_ingArq.pdf")
t42_R_I_ingArq<-table(muestra$I_ingArq,muestra$Riesgo)
plot(Riesgo~I_ingArq,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ingenierias y arquitectura vs Riesgo",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura")
dev.off()

pdf("t42_D_I_ingArq.pdf")
t42_D_I_ingArq<-table(muestra$I_ingArq,muestra$Default)
plot(Default~I_ingArq,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ingenierias y arquitectura vs Default",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura")
dev.off()

#RANGOS
pdf("t42I_ingArqR.pdf")
t42I_ingArqR<-table(muestra$I_ingArq.binned,muestra$CuotMora)
plot(CuotMora~I_ingArq.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura",
     main="Puntaje prueba intereses-ingenierias y arquitectura vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t42_R_I_ingArqR.pdf")
t42_R_I_ingArqR<-table(muestra$I_ingArq.binned,muestra$Riesgo)
plot(Riesgo~I_ingArq.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ingenierias y arquitectura vs Riesgo",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura")
dev.off()

pdf("t42_D_I_ingArqR.pdf")
t42_D_I_ingArqR<-table(muestra$I_ingArq.binned,muestra$Default)
plot(Default~I_ingArq.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ingenierias y arquitectura vs Default",
     xlab="Puntaje prueba Intereses-ingenierias y arquitectura")
dev.off()

#43Puntaje prueba intereses-bellas artes###########################################################################################################
pdf("h43I_bellArt.pdf")
hist(muestra$I_bellArt, main = "Histograma: Puntaje prueba intereses-bellas artes",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-bellas artes")
dev.off()

pdf("t43I_bellArt.pdf")
t43I_bellArt<-table(muestra$I_bellArt,muestra$CuotMora)
plot(CuotMora~I_bellArt,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-bellas artes vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

pdf("t43_R_I_bellArt.pdf")
t43_R_I_bellArt<-table(muestra$I_bellArt,muestra$Riesgo)
plot(Riesgo~I_bellArt,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-bellas artes vs Riesgo",
     xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

pdf("t43_D_I_bellArt.pdf")
t43_D_I_bellArt<-table(muestra$I_bellArt,muestra$Default)
plot(Default~I_bellArt,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-bellas artes vs Default",
     xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

#RANGOS
pdf("t43I_bellArtR.pdf")
t43I_bellArtR<-table(muestra$I_bellArt.binned,muestra$CuotMora)
plot(CuotMora~I_bellArt.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-bellas artes vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

pdf("t43_R_I_bellArtR.pdf")
t43_R_I_bellArtR<-table(muestra$I_bellArt.binned,muestra$Riesgo)
plot(Riesgo~I_bellArt.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-bellas artes vs Riesgo",
     xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

pdf("t43_D_I_bellArtR.pdf")
t43_D_I_bellArtR<-table(muestra$I_bellArt.binned,muestra$Default)
plot(Default~I_bellArt.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-bellas artes vs Default",
     xlab="Puntaje prueba Intereses-bellas artes")
dev.off()

#44Puntaje prueba intereses:ciencias de la salud###################################################################################################
pdf("h44I_cSalud.pdf")
hist(muestra$I_cSalud, main = "Histograma: Puntaje prueba intereses-ciencias de la salud",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-ciencias de la salud")
dev.off()

pdf("t44I_cSalud.pdf")
t44I_cSalud<-table(muestra$I_cSalud,muestra$CuotMora)
plot(CuotMora~I_cSalud,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas", xlab="Puntaje prueba Intereses-ciencias de la salud",
     main="Puntaje prueba intereses-ciencias de la salud vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t44_R_I_cSalud.pdf")
t44_R_I_cSalud<-table(muestra$I_cSalud,muestra$Riesgo)
plot(Riesgo~I_cSalud,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias de la salud vs Riesgo", 
     xlab="Puntaje prueba Intereses-ciencias de la salud")
dev.off()

pdf("t44_D_I_cSalud.pdf")
t44_D_I_cSalud.pdf<-table(muestra$I_cSalud,muestra$Default)
plot(Default~I_cSalud,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias de la salud vs Default", 
     xlab="Puntaje prueba Intereses-ciencias de la salud")
dev.off()

#rangos 

pdf("t44I_cSaludR.pdf")
t44I_cSaludR<-table(muestra$I_cSalud.binned,muestra$CuotMora)
plot(CuotMora~I_cSalud.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-ciencias de la salud vs Máximas cuotas en mora alcanzadas", xlab="Puntaje prueba Intereses-ciencias de la salud")
dev.off()

pdf("t44_R_I_cSaludR.pdf")
t44_R_I_cSaludR<-table(muestra$I_cSalud.binned,muestra$Riesgo)
plot(Riesgo~I_cSalud.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias de la salud vs Riesgo",
      xlab="Puntaje prueba Intereses-ciencias de la salud")
dev.off()

pdf("t44_D_I_cSaludR.pdf")
t44_D_I_cSalud.pdfR<-table(muestra$I_cSalud.binned,muestra$Default)
plot(Default~I_cSalud.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias de la salud vs Default",
     xlab="Puntaje prueba Intereses-ciencias de la salud")
dev.off()

#45Puntaje prueba intereses-ciencias sociales######################################################################################################
pdf("h45I_cSoc.pdf")
hist(muestra$I_cSoc, main = "Histograma: Puntaje prueba intereses-ciencias sociales",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-ciencias sociales")
dev.off()

pdf("t45I_cSoc.pdf")
t45I_cSoc.pdf<-table(muestra$I_cSoc,muestra$CuotMora)
plot(CuotMora~I_cSoc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas", xlab="Puntaje prueba Intereses-ciencias sociales",
     main="Puntaje prueba intereses-ciencias sociales vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t45_R_I_cSoc.pdf")
t45_R_I_cSoc<-table(muestra$I_cSoc,muestra$Riesgo)
plot(Riesgo~I_cSoc,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias sociales vs Riesgo",
     xlab="Puntaje prueba Intereses-ciencias sociales")
dev.off()

pdf("t45_D_I_cSoc.pdf")
t45_D_I_cSoc<-table(muestra$I_cSoc,muestra$Default)
plot(Default~I_cSoc,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias sociales vs Default",
     xlab="Puntaje prueba Intereses-ciencias sociales")
dev.off()

#RANGOS

pdf("t45I_cSocR.pdf")
t45I_cSocR<-table(muestra$I_cSoc.binned,muestra$CuotMora)
plot(CuotMora~I_cSoc.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",
     main="Puntaje prueba intereses-ciencias sociales vs Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-ciencias sociales")
dev.off()

pdf("t45_R_I_cSocR.pdf")
t45_R_I_cSocR<-table(muestra$I_cSoc.binned,muestra$Riesgo)
plot(Riesgo~I_cSoc.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-ciencias sociales vs Riesgo",
     xlab="Puntaje prueba Intereses-ciencias sociales")
dev.off()

pdf("t45_D_I_cSocR.pdf")
t45_D_I_cSocR<-table(muestra$I_cSoc.binned,muestra$Default)
plot(Default~I_cSoc.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-ciencias sociales vs Default",
     xlab="Puntaje prueba Intereses-ciencias sociales")
dev.off()

#46Puntaje prueba intereses:tecnicas y tecnologicas################################################################################################
pdf("h46I_tyt.pdf")
hist(muestra$I_tyt, main = "Histograma: Puntaje prueba intereses-técnicas y tecnológicas",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-técnicas y tecnológicas")
dev.off()

pdf("t46I_tyt.pdf")
t46I_tyt.pdf<-table(muestra$I_tyt,muestra$CuotMora)
plot(CuotMora~I_tyt,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-tecnicas y tecnologicas",
     main="Puntaje prueba intereses-tecnicas y tecnologicas vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t46_R_I_tyt.pdf.pdf")
t46_R_I_tyt<-table(muestra$I_tyt,muestra$Riesgo)
plot(Riesgo~I_tyt,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-tecnicas y tecnologicas vs Riesgo",
     xlab="Puntaje prueba Intereses-tecnicas y tecnologicas")
dev.off()

pdf("t46_D_I_tyt.pdf")
t46_D_I_tyt<-table(muestra$I_tyt,muestra$Default)
plot(Default~I_tyt,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-tecnicas y tecnologicas vs Default",
     xlab="Puntaje prueba Intereses-tecnicas y tecnologicas")
dev.off()

#RANGOS

pdf("t46I_tytR.pdf")
t46I_tytR<-table(muestra$I_tyt.binned,muestra$CuotMora)
plot(CuotMora~I_tyt.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-tecnicas y tecnologicas",
     main="Puntaje prueba intereses-tecnicas y tecnologicas vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t46_R_I_tytR.pdf")
t46_R_I_tytR<-table(muestra$I_tyt.binned,muestra$Riesgo)
plot(Riesgo~I_tyt.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses-tecnicas y tecnologicas vs Riesgo",
     xlab="Puntaje prueba Intereses-tecnicas y tecnologicas")
dev.off()

pdf("t46_D_I_tytR.pdf")
t46_D_I_tytR<-table(muestra$I_tyt.binned,muestra$Default)
plot(Default~I_tyt.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses-tecnicas y tecnologicas vs Default",
     xlab="Puntaje prueba Intereses-tecnicas y tecnologicas")
dev.off()

#47Puntaje prueba intereses:ciencias naturales####################################################################################################
pdf("h47I_matCNat.pdf")
hist(muestra$I_matCNat, main = "Histograma: Puntaje prueba intereses-ciencias naturales",
     ylab = "Frecuencia", xlab = "Puntaje prueba intereses-ciencias naturales")
dev.off()

pdf("t47I_matCNat.pdf")
t47I_matCNat<-table(muestra$I_matCNat,muestra$CuotMora)
plot(CuotMora~I_matCNat,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t47_R_I_matCNat.pdf")
t47_R_I_matCNat<-table(muestra$I_matCNat,muestra$Riesgo)
plot(Riesgo~I_matCNat,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Riesgo")
dev.off()

pdf("t47_D_I_matCNat.pdf")
t47_D_I_matCNat<-table(muestra$I_matCNat,muestra$Default)
plot(Default~I_matCNat,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Default")
dev.off()

#RANGOS
pdf("t47I_matCNatR.pdf")
t47I_matCNatR<-table(muestra$I_matCNat.binned,muestra$CuotMora)
plot(CuotMora~I_matCNat.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t47_R_I_matCNatR.pdf")
t47_R_I_matCNatR<-table(muestra$I_matCNat.binned,muestra$Riesgo)
plot(Riesgo~I_matCNat.binned,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Riesgo")
dev.off()

pdf("t47_D_I_matCNatR.pdf")
t47_D_I_matCNatR<-table(muestra$I_matCNat.binned,muestra$Default)
plot(Default~I_matCNat.binned,data = muestra,col=coloresCM,ylab="Default",xlab="Puntaje prueba Intereses-ciencias naturales",
     main="Puntaje prueba intereses-ciencias naturales vs Default")
dev.off()

#48Puntaje prueba intereses########################################################################################################################
table(muestra$Intereses)
pdf("h48Intereses.pdf")
hist(muestra$Intereses, main = "Histograma: Puntaje prueba intereses", xlab = "Puntaje prueba intereses", ylab = "Frecuencia",
     xlim = c(15,35), ylim = c(0,60))
dev.off()

pdf("t48Intereses.pdf")
t48Intereses<-table(muestra$Intereses,muestra$CuotMora)
plot(CuotMora~Intereses,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses",
     main="Puntaje prueba intereses vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t48_R_Intereses.pdf")
t48_R_Intereses<-table(muestra$Intereses,muestra$Riesgo)
plot(Riesgo~Intereses,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses vs Riesgo",xlab="Puntaje prueba Intereses")
dev.off()

pdf("t48_D_Intereses.pdf")
t48_D_Intereses<-table(muestra$Intereses,muestra$Default)
plot(Default~Intereses,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses vs Default",xlab="Puntaje prueba Intereses")
dev.off()

#RANGOS

pdf("t48InteresesR.pdf")
t48Intereses_R<-table(muestra$Intereses.binned,muestra$CuotMora)
plot(CuotMora~Intereses.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba Intereses",
     main="Puntaje prueba intereses vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t48_R_InteresesR.pdf")
t48_R_Intereses_R<-table(muestra$Intereses.binned,muestra$Riesgo)
plot(Riesgo~Intereses.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba intereses vs Riesgo",xlab="Puntaje prueba Intereses")
dev.off()

pdf("t48_D_InteresesR.pdf")
t48_D_Intereses_R<-table(muestra$Intereses.binned,muestra$Default)
plot(Default~Intereses.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba intereses vs Default",xlab="Puntaje prueba Intereses")
dev.off()

#49Puntaje prueba selección#########################################################################################################################
table(muestra$PrbSel)
pdf("h49prueba.pdf")
hist(muestra$PrbSel, main = "Histograma: Puntaje prueba de selección", 
     xlab = "Puntaje prueba de selección", ylab = "Frecuencia", xlim = c(50,100))
dev.off()

pdf("t49PrbSel.pdf")
t49PrbSel<-table(muestra$PrbSel,muestra$CuotMora)
plot(CuotMora~PrbSel,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba de selección",
     main="Puntaje prueba de selección vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t49_R_PrbSel.pdf")
t49_R_PrbSel<-table(muestra$PrbSel,muestra$Riesgo)
plot(Riesgo~PrbSel,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba de selección vs Riesgo",xlab="Puntaje prueba de selección")
dev.off()

pdf("t49_D_PrbSel.pdf")
t49_D_PrbSel<-table(muestra$PrbSel,muestra$Default)
plot(Default~PrbSel,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba de selección vs Default",xlab="Puntaje prueba de selección")
dev.off()

#RANGOS

pdf("t49PrbSelR.pdf")
t49PrbSelR<-table(muestra$PrbSel.binned,muestra$CuotMora)
plot(CuotMora~PrbSel.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Puntaje prueba de selección",
     main="Puntaje prueba de selección vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t49_R_PrbSelR.pdf")
t49_R_PrbSelR<-table(muestra$PrbSel.binned,muestra$Riesgo)
plot(Riesgo~PrbSel.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Puntaje prueba de selección vs Riesgo",xlab="Puntaje prueba de selección")
dev.off()

pdf("t49_D_PrbSelR.pdf")
t49_D_PrbSelR<-table(muestra$PrbSel.binned,muestra$Default)
plot(Default~PrbSel.binned,data = muestra,col=coloresCM,ylab="Default",main="Puntaje prueba de selección vs Default",xlab="Puntaje prueba de selección")
dev.off()

#50Ingresos/Egresos################################################################################################################################
pdf("t50Ing_Egr.pdf")
t50Ing_Egr<-table(muestra$Ing_Egr,muestra$CuotMora)
plot(CuotMora~Ing_Egr,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Ingresos/Egresos",
     main="Ingresos/Egresos vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t50_R_Ing_Egr.pdf")
t50_R_Ing_Egr<-table(muestra$Ing_Egr,muestra$Riesgo)
plot(Riesgo~Ing_Egr,data = muestra,col=coloresCM,ylab="Riesgo",xlab="Ingresos/Egresos",main="Ingresos/Egresos vs Riesgo")
dev.off()

pdf("t50_D_Ing_Egr.pdf")
t50_D_Ing_Egr<-table(muestra$Ing_Egr,muestra$Default)
plot(Default~Ing_Egr,data = muestra,col=coloresCM,ylab="Default",xlab="Ingresos/Egresos",main="Ingresos/Egresos vs Default")
dev.off()

#51Cuenta ahorros##################################################################################################################################
pdf("t51CtaAh.pdf")
t51CtaAh<-table(muestra$CtaAh,muestra$CuotMora)
plot(CuotMora~CtaAh,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Tener cuenta de ahorros",
     main="Tener cuenta de ahorros vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t51_R_CtaAh.pdf")
t51_R_CtaAh<-table(muestra$CtaAh,muestra$Riesgo)
plot(Riesgo~CtaAh,data = muestra,col=coloresCM,ylab="Riesgo",main="Tener cuenta de ahorros vs Riesgo",xlab="Tener cuenta de ahorros")
dev.off()

pdf("t51_D_CtaAh.pdf")
t51_D_CtaAh<-table(muestra$CtaAh,muestra$Default)
plot(Default~CtaAh,data = muestra,col=coloresCM,ylab="Default",main="Tener cuenta de ahorros vs Default",xlab="Tener cuenta de ahorros")
dev.off()

#52Cuenta corriente################################################################################################################################
pdf("t52CtaCte.pdf")
t52CtaCte<-table(muestra$CtaCte,muestra$CuotMora)
plot(CuotMora~CtaCte,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Tener cuenta corriente",
     main="Tener cuenta corriente vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t52_R_CtaCte.pdf")
t52_R_CtaCte<-table(muestra$CtaCte,muestra$Riesgo)
plot(Riesgo~CtaCte,data = muestra,col=coloresCM,ylab="Riesgo",main="Tener cuenta corriente vs Riesgo",xlab="Tener cuenta corriente")
dev.off()

pdf("t52_D_CtaCte.pdf")
t52_D_CtaCte<-table(muestra$CtaCte,muestra$Default)
plot(Default~CtaCte,data = muestra,col=coloresCM,ylab="Default",main="Tener cuenta corriente vs Default",xlab="Tener cuenta corriente")
dev.off()

#53Tarjeta de credito##############################################################################################################################
pdf("t53TarjCred.pdf")
t53TarjCred<-table(muestra$TarjCred,muestra$CuotMora)
plot(CuotMora~TarjCred,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Tener tarjeta de crédito",
     main="Tener tarjeta de crédito vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t53_R_TarjCred.pdf")
t53_R_TarjCred<-table(muestra$TarjCred,muestra$Riesgo)
plot(Riesgo~TarjCred,data = muestra,col=coloresCM,ylab="Riesgo",main="Tener tarjeta de crédito vs Riesgo",xlab="Tener tarjeta de crédito")
dev.off()

pdf("t53_D_TarjCred.pdf")
t53_D_TarjCred<-table(muestra$TarjCred,muestra$Default)
plot(Default~TarjCred,data = muestra,col=coloresCM,ylab="Default",main="Tener tarjeta de crédito vs Default",xlab="Tener tarjeta de crédito")
dev.off()

#54creditos#######################################################################################################################################
pdf("t54Cred.pdf")
t54Cred<-table(muestra$Creditos,muestra$CuotMora)
plot(CuotMora~Creditos,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Tener créditos",
     main="Tener créditos vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t54_R_Cred.pdf")
t54_R_Cred<-table(muestra$Creditos,muestra$Riesgo)
plot(Riesgo~Creditos,data = muestra,col=coloresCM,ylab="Riesgo",main="Tener créditos vs Riesgo",xlab="Tener créditos")
dev.off()

pdf("t54_D_Cred.pdf")
t54_D_Cred<-table(muestra$Creditos,muestra$Default)
plot(Default~Creditos,data = muestra,col=coloresCM,ylab="Default",main="Tener créditos vs Default",xlab="Tener créditos")
dev.off()

#55Productos Bancarios##############################################################################################################################
summary(muestra$Pts_Banc)
sd(muestra$Pts_Banc)
#la distribucion de la ponderacion de bienes se encuentra entre 0 y 5, con una media de 2.4, mediana de 2.36
#y una desviacion estandar de 1.28. Razon por la cual se grafica un histograma acumulado y se divide en 5 rangos la muestra
pdf("h55Pts_Banc.pdf")
hB<-hist(muestra$Pts_Banc,  main = "Histograma: Productos bancarios", xlab = "Productos bancarios", ylab = "Frecuencia", ylim = c(0,100))
dev.off()

pdf("t55PtsBanc.pdf")
t55PtsBanc<-table(muestra$Pts_Banc,muestra$CuotMora)
plot(CuotMora~Pts_Banc,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Productos financieros",
     main="Productos financieros vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t55_R_PtsBanc.pdf")
t55_R_PtsBanc<-table(muestra$Pts_Banc,muestra$Riesgo)
plot(Riesgo~Pts_Banc,data = muestra,col=coloresCM,ylab="Riesgo",main="Productos financieros vs Riesgo",xlab="Productos financieros")
dev.off()

pdf("t55_D_PtsBanc.pdf")
t55_D_PtsBanc<-table(muestra$Pts_Banc,muestra$Default)
plot(Default~Pts_Banc,data = muestra,col=coloresCM,ylab="Default",main="Productos financieros vs Default",xlab="Productos financieros")
dev.off()

#RANGOS

pdf("t55PtsBancR.pdf")
t55PtsBancR<-table(muestra$Pts_Banc.binned,muestra$CuotMora)
plot(CuotMora~Pts_Banc.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Productos financieros",
     main="Productos financieros vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t55_R_PtsBancR.pdf")
t55_R_PtsBancR<-table(muestra$Pts_Banc.binned,muestra$Riesgo)
plot(Riesgo~Pts_Banc.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Productos financieros vs Riesgo",xlab="Productos financieros")
dev.off()

pdf("t55_D_PtsBancR.pdf")
t55_D_PtsBancR<-table(muestra$Pts_Banc.binned,muestra$Default)
plot(Default~Pts_Banc.binned,data = muestra,col=coloresCM,ylab="Default",main="Productos financieros vs Default",xlab="Productos financieros")
dev.off()

#56Vivienda########################################################################################################################################
pdf("t56B_viv.pdf")
t56B_viv<-table(muestra$B_viv,muestra$CuotMora)
plot(CuotMora~B_viv,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Poseer bienes: vivienda",
     main="Poseer bienes: vivienda vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t56_R_B_viv.pdf")
t56_R_B_viv<-table(muestra$B_viv,muestra$Riesgo)
plot(Riesgo~B_viv,data = muestra,col=coloresCM,ylab="Riesgo",main="Poseer bienes: vivienda vs Riesgo",xlab="Poseer bienes: vivienda")
dev.off()

pdf("t56_D_B_viv.pdf")
t56_D_B_viv<-table(muestra$B_viv,muestra$Default)
plot(Default~B_viv,data = muestra,col=coloresCM,ylab="Default",main="Poseer bienes: vivienda vs Default",xlab="Poseer bienes: vivienda")
dev.off()

#57Computador#######################################################################################################################################
pdf("t57B_comp.pdf")
t57B_comp<-table(muestra$B_comp,muestra$CuotMora)
plot(CuotMora~B_comp,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Poseer bienes: computador",
     main="Poseer bienes: computador vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t57_R_B_comp.pdf")
t57_R_B_comp<-table(muestra$B_comp,muestra$Riesgo)
plot(Riesgo~B_comp,data = muestra,col=coloresCM,ylab="Riesgo",main="Poseer bienes: computador vs Riesgo",xlab="Poseer bienes: computador")
dev.off()

pdf("t57_D_B_comp.pdf")
t57_D_B_comp<-table(muestra$B_comp,muestra$Default)
plot(Default~B_comp,data = muestra,col=coloresCM,ylab="Default",main="Poseer bienes: computador vs Default",xlab="Poseer bienes: computador")
dev.off()

#58Celular##########################################################################################################################################
pdf("t58B_cel.pdf")
t58B_cel<-table(muestra$B_cel,muestra$CuotMora)
plot(CuotMora~B_cel,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Poseer bienes: celular",
     main="Poseer bienes: celular vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t58_R_B_cel.pdf")
t58_R_B_cel<-table(muestra$B_cel,muestra$Riesgo)
plot(Riesgo~B_cel,data = muestra,col=coloresCM,ylab="Riesgo",main="Poseer bienes: celular vs Riesgo",xlab="Poseer bienes: celular")
dev.off()

pdf("t58_D_B_cel.pdf")
t58_D_B_cel<-table(muestra$B_cel,muestra$Default)
plot(Default~B_cel,data = muestra,col=coloresCM,ylab="Default",main="Poseer bienes: celular vs Default",xlab="Poseer bienes: celular")
dev.off()

#59Moto#############################################################################################################################################
pdf("t59B_moto.pdf")
t59B_moto<-table(muestra$B_moto,muestra$CuotMora)
plot(CuotMora~B_moto,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Poseer bienes: moto",
     main="Poseer bienes: moto vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t59_R_B_moto.pdf")
t59_R_B_moto<-table(muestra$B_moto,muestra$Riesgo)
plot(Riesgo~B_moto,data = muestra,col=coloresCM,ylab="Riesgo",main="Poseer bienes: moto vs Riesgo",xlab="Poseer bienes: moto")
dev.off()

pdf("t59_D_B_moto.pdf")
t59_D_B_moto<-table(muestra$B_moto,muestra$Default)
plot(Default~B_moto,data = muestra,col=coloresCM,ylab="Default",main="Poseer bienes: moto vs Default",xlab="Poseer bienes: moto")
dev.off()

#60Carro############################################################################################################################################
pdf("t60B_carro.pdf")
t60B_carro<-table(muestra$B_carro,muestra$CuotMora)
plot(CuotMora~B_carro,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Poseer bienes: carro",
     main="Poseer bienes: carro vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t60_R_B_carro.pdf")
t60_R_B_carro<-table(muestra$B_carro,muestra$Riesgo)
plot(Riesgo~B_carro,data = muestra,col=coloresCM,ylab="Riesgo",main="Poseer bienes: carro vs Riesgo",xlab="Poseer bienes: carro")
dev.off()

pdf("t60_D_B_carro.pdf")
t60_D_B_carro<-table(muestra$B_carro,muestra$Default)
plot(Default~B_carro,data = muestra,col=coloresCM,ylab="Default",main="Poseer bienes: carro vs Default",xlab="Poseer bienes: carro")
dev.off()

#61Bienes Ponderados################################################################################################################################
table(muestra$B_pond)
summary(muestra$B_pond)
sd(muestra$B_pond)
#la distribucion de la ponderacion de bienes se encuentra entre 0 y 5, con una media de 2.4, mediana de 3 y una desviacion estandar de 1.2
pdf("h61B_pond.pdf")
hB<-hist(muestra$B_pond, breaks=7, main = "Histograma: Bienes ponderados", xlab = "Bienes ponderados", ylab = "Frecuencia", ylim = c(0,100))
dev.off()

pdf("t61B_pond.pdf")
t61B_pond<-table(muestra$B_pond,muestra$CuotMora)
plot(CuotMora~B_pond,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Bienes ponderados",
     main="Bienes ponderados vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t61_R_B_pond.pdf")
t61_R_B_pond<-table(muestra$B_pond,muestra$Riesgo)
plot(Riesgo~B_pond,data = muestra,col=coloresCM,ylab="Riesgo",main="Bienes ponderados vs Riesgo",xlab="Bienes ponderados")
dev.off()

pdf("t61_D_B_pond.pdf")
t61_D_B_pond<-table(muestra$B_pond,muestra$Default)
plot(Default~B_pond,data = muestra,col=coloresCM,ylab="Default",main="Bienes ponderados vsDefault",xlab="Bienes ponderados")
dev.off()

#RANGOS

pdf("t61B_pondR.pdf")
t61B_pondR<-table(muestra$B_pond.binned,muestra$CuotMora)
plot(CuotMora~B_pond.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Bienes ponderados",
     main="Bienes ponderados vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t61_R_B_pondR.pdf")
t61_R_B_pondR<-table(muestra$B_pond.binned,muestra$Riesgo)
plot(Riesgo~B_pond.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Bienes ponderados vs Riesgo",xlab="Bienes ponderados")
dev.off()

pdf("t61_D_B_pondR.pdf")
t61_D_B_pondR<-table(muestra$B_pond.binned,muestra$Default)
plot(Default~B_pond.binned,data = muestra,col=coloresCM,ylab="Default",main="Bienes ponderados vs Default",xlab="Bienes ponderados")
dev.off()

#62Pagare############################################################################################################################################
pdf("t62Pagare.pdf")
t62Pagare<-table(muestra$Pagare,muestra$CuotMora)
plot(CuotMora~Pagare,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Pagaré",
     main="Pagaré vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t62_R_Pagare.pdf")
t62_R_Pagare<-table(muestra$Pagare,muestra$Riesgo)
plot(Riesgo~Pagare,data = muestra,col=coloresCM,ylab="Riesgo",main="Pagaré vs Riesgo",xlab="Pagaré")
dev.off()

pdf("t62_D_Pagare.pdf")
t62_D_Pagare<-table(muestra$Pagare,muestra$Default)
plot(Default~Pagare,data = muestra,col=coloresCM,ylab="Default",main="Pagaré vs Default",xlab="Pagaré")
dev.off()

#63Scoring################################################################################################################################
pdf("t63Scoring.pdf")
t63Scoring<-table(muestra$Scoring,muestra$CuotMora)
plot(CuotMora~Scoring,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Scoring",
     main="Scoring vs Máximas cuotas en mora alcanzadas")
dev.off()

pdf("t63_R_Scoring.pdf")
t63_R_Scoring<-table(muestra$Scoring,muestra$Riesgo)
plot(Riesgo~Scoring,data = muestra,col=coloresCM,ylab="Riesgo",main="Scoring vs Riesgo",xlab="Scoring")
dev.off()

pdf("t63_D_Scoring.pdf")
t63_D_Scoring<-table(muestra$Scoring,muestra$Default)
plot(Default~Scoring,data = muestra,col=coloresCM,ylab="Default",main="Scoring vsDefault",xlab="Scoring")
dev.off()

#RANGOS

pdf("t63ScoringR.pdf")
t63ScoringR<-table(muestra$Scoring.binned,muestra$CuotMora)
plot(CuotMora~Scoring.binned,data = muestra,col=coloresCM,ylab="Máximas cuotas en mora alcanzadas",xlab="Scoring",
     main="Scoring vs Máximas cuotas en mora alcanzadas")
dev.off()
 
pdf("t63_R_ScoringR.pdf")
t63_R_ScoringR<-table(muestra$Scoring.binned,muestra$Riesgo)
plot(Riesgo~Scoring.binned,data = muestra,col=coloresCM,ylab="Riesgo",main="Scoring vs Riesgo",xlab="Scoring")
dev.off()

pdf("t63_D_ScoringR.pdf")
t63_D_ScoringR<-table(muestra$Scoring.binned,muestra$Default)
plot(Default~Scoring.binned,data = muestra,col=coloresCM,ylab="Default",main="Scoring vs Default",xlab="Scoring")
dev.off()


#depurar base final para la construccion del modelo###################################################################################################
names(muestra)
DM_DB_Debugged<-muestra[,-c(12,14:16,26:27,32,35,37)]
names(DM_DB_Debugged)
DM_DB_Debugged<-DM_DB_Debugged[,c(1:11, 70, 12:21,71:72,22:69,73:101)]
names(DM_DB_Debugged)
write.csv(DM_DB_Debugged,paste('DM_DB_Debugged.csv',sep=""))

#matriz correlacion########################################################################################################################
vars<-names(DM_DB_Debugged[,10:72])
dat<-DM_DB_Debugged
catcorrm <- function(vars, dat) sapply(vars, function(y) sapply(vars, function(x) assocstats(table(dat[,x], dat[,y]))$cramer))
matCor<-catcorrm(vars,dat)
View(matCor)
write.csv(matCor,paste('matCor.csv',sep=""))

#variables correlacionadas >=0.6############################################################################################################
altCor6<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.6,]
  a$xVariable<-rownames(a)
  altCor6<-rbind(altCor6,a)
}
freCor6<-count(altCor6$variable)
freCor6<-freCor6[order(freCor6$freq,decreasing = TRUE),]
freVar6<-count(freCor6$freq)
freVar6<-freVar6[order(freVar6$x,decreasing = TRUE),]
View(altCor6)
View(freCor6)
View(freVar6)

#variables correlacionadas >=0.7############################################################################################################
altCor7<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.7,]
  a$xVariable<-rownames(a)
  altCor7<-rbind(altCor7,a)
}
freCor7<-count(altCor7$variable)
freCor7<-freCor7[order(freCor7$freq,decreasing = TRUE),]
freVar7<-count(freCor7$freq)
freVar7<-freVar7[order(freVar7$x,decreasing = TRUE),]
View(altCor7)
View(freCor7)
View(freVar7)

#variables correlacionadas >=0.8############################################################################################################
altCor8<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.8,]
  a$xVariable<-rownames(a)
  altCor8<-rbind(altCor8,a)
}
freCor8<-count(altCor8$variable)
freCor8<-freCor8[order(freCor8$freq,decreasing = TRUE),]
freVar8<-count(freCor8$freq)
freVar8<-freVar8[order(freVar8$x,decreasing = TRUE),]
View(altCor8)
View(freCor8)
View(freVar8)

#variables correlacionadas >=0.9############################################################################################################
altCor9<-data.frame()
for (i in 1:nrow(matCor)) {
  a<-data.frame("variable"=colnames(matCor)[i],"cor"=matCor[i,])
  a<-a[a$cor>0.9,]
  a$xVariable<-rownames(a)
  altCor9<-rbind(altCor9,a)
}
freCor9<-count(altCor9$variable)
freCor9<-freCor9[order(freCor9$freq,decreasing = TRUE),]
freVar9<-count(freCor9$freq)
freVar9<-freVar9[order(freVar9$x,decreasing = TRUE),]
freCor9<-freCor9[freCor9$freq>=3,]
altCor9<-altCor9[altCor9$variable%in%freCor9$x,]
View(altCor9)
View(freCor9)
View(freVar9)
write.csv(freCor9,paste('cor0.9.csv',sep=""))


#construir y guardar base depurada para "train" y "test"####################################################################################
sample<-DM_DB_Debugged[DM_DB_Debugged$TipBase=="Muestra",]
#sample$CuotMora<-as.numeric(sample$CuotMora)
View(sample)
names(sample)
nrow(sample)
ncol(sample)
write.csv(sample,paste('sample.csv',sep=""))

#base con variables numericas originales
sampleO<-sample[,1:72]
names(sampleO)
#sampleO$CuotMora<-as.numeric(sampleO$CuotMora)
write.csv(sampleO,paste('sampleO.csv',sep=""))

#base con variables numericas en rangos
sampleR<-sample[,-c(15,34:58,64,70,72)];
names(sampleR)
sampleR<-sampleR[,c(1:14,44,15:32,45:69,33:37,70,38:42,71,43,72)];
names(sampleR)
#sampleR$CuotMora<-as.numeric(sampleR$CuotMora)
write.csv(sampleR,paste('sampleR.csv',sep=""))