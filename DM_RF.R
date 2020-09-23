7#___________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________________
#_________________________________________Prediccion del riesgo Default en Acuerdos de Ingreso Compartido:__________________________________________
#__________________________________________________MSc.(c) Diana Carolina Lopez Becerra_____________________________________________________________
#___________________________________________________________Ph.D.Hernando Diaz_____________________________________________________________________
#__________________________________________Universidad Nacional de Colombia, sede Bogota D.C._______________________________________________________
#_________________________________________________________________2020______________________________________________________________________________

#manejar tildes#####################################################################################################################################
Sys.setlocale('LC_ALL','es_CO.iso88591')

#instalar paquetes##################################################################################################################################
install.packages("randomForest")
install.packages("rpart")
install.packages("ROCR")
install.packages("ggplot2", dependencies = TRUE)
install.packages("caret")
install.packages("caTools")
install.packages("xgboost")
install.packages("vip") 

#cargar paquetes####################################################################################################################################
library(randomForest)
library(rpart)
library(ROCR)
library(ggplot2)
library(lattice)
library(caret)#no carga en amazon
library(caTools)#xgboost
library(xgboost)
library(vip) 

#cargar la base de datos de muestra#################################################################################################################
sample<-read.csv(paste('sample.csv',sep=""),header = TRUE, sep="," )
sampleO<-read.csv(paste('sampleO.csv',sep=""),header = TRUE, sep="," )
sampleR<-read.csv(paste('sampleR.csv',sep=""),header = TRUE, sep="," )

#cargar la base de datos de estudiantes para "experimiento en vivo"##################################################################################
experiment<-read.csv(paste('experiment.csv',sep=""),header = TRUE, sep="," )
experimentO<-read.csv(paste('experimentO.csv',sep=""),header = TRUE, sep="," )
experimentR<-read.csv(paste('experimentR.csv',sep=""),header = TRUE, sep="," )

#depurar bases######################################################################################################################################
sampleO$CuotMora<-as.factor(sampleO$CuotMora)
sampleR$CuotMora<-as.factor(sampleR$CuotMora)
sampleO$CuotMora<-relevel(sampleO$CuotMora,ref="0")
sampleR$CuotMora<-relevel(sampleR$CuotMora,ref="0")

RFsampleMoraO<-sampleO[,8:73]
RFsampleMoraO<-RFsampleMoraO[,-c(2,3)]
write.csv(RFsampleMoraO,paste('RFsampleMoraO.csv',sep=""))

RFsampleMoraR<-sampleR[,8:73]
RFsampleMoraR<-RFsampleMoraR[,-c(2,3)]
write.csv(RFsampleMoraR,paste('RFsampleMoraR.csv',sep=""))

RFsampleDefaultO<-sampleO[,9:73]
RFsampleDefaultO<-RFsampleDefaultO[,-2]

RFsampleDefaultR<-sampleR[,9:73]
RFsampleDefaultR<-RFsampleDefaultR[,-2]

RFsampleRiskO<-sampleO[,10:73]
RFsampleRiskR<-sampleR[,10:73]

#reparticion de los datos en entrenamiento y prueba#################################################################################################
seed<-123
set.seed(seed)
RFid_entrenamientoC<-createDataPartition(RFsampleMoraO$CuotMora, p=0.69, list=FALSE)

RFsampleMoraO_tr<-RFsampleMoraO[RFid_entrenamientoC,]
RFsampleMoraO_ts<-RFsampleMoraO[-RFid_entrenamientoC,]
RFsampleMoraR_tr<-RFsampleMoraR[RFid_entrenamientoC,]
RFsampleMoraR_ts<-RFsampleMoraR[-RFid_entrenamientoC,]

set.seed(seed)
RFid_entrenamientoD<-createDataPartition(RFsampleDefaultO$Default, p=0.7, list=FALSE)

RFsampleDefaultO_tr<-RFsampleDefaultO[RFid_entrenamientoD,]
RFsampleDefaultO_ts<-RFsampleDefaultO[-RFid_entrenamientoD,]
RFsampleDefaultR_tr<-RFsampleDefaultR[RFid_entrenamientoD,]
RFsampleDefaultR_ts<-RFsampleDefaultR[-RFid_entrenamientoD,]

set.seed(seed)
RFid_entrenamientoR<-createDataPartition(RFsampleRiskO$Riesgo, p=0.7, list=FALSE)

RFsampleRiskO_tr<-RFsampleRiskO[RFid_entrenamientoR,]
RFsampleRiskO_ts<-RFsampleRiskO[-RFid_entrenamientoR,]
RFsampleRiskR_tr<-RFsampleRiskR[RFid_entrenamientoR,]
RFsampleRiskR_ts<-RFsampleRiskR[-RFid_entrenamientoR,]

###DECISION TREES MODELS###########################################################################################################################
#funcion rpart()##################################################################################################################################
#CART para cuotas en mora VARIABLES ORIGINALES##########################################################################################
fitC<- rpart(CuotMora ~ ., data = RFsampleMoraO_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_C_O_rpart.pdf")
plot(fitC, margin = 0.1)#visualizar las divisiones 
text(fitC, cex = 0.75)
dev.off()

CART_C_O<- predict(fitC, type = "class")
confusionMatrix(CART_C_O,RFsampleMoraO_tr$CuotMora)

CART_c_O_ts<- predict(fitC, RFsampleMoraO_ts,type = "class")
confusionMatrix(CART_c_O_ts,RFsampleMoraO_ts$CuotMora)

#CART para cuotas en mora VARIABLES ORIGINALES ~Scoring##################################################################
fitC1<- rpart(CuotMora ~ Scoring, data = RFsampleMoraO_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_C1_O_rpart.pdf")
plot(fitC1, margin = 0.1)#visualizar las divisiones 
text(fitC1, cex = 0.75)
dev.off()

CART_C1_O<- predict(fitC1, type = "class")
confusionMatrix(CART_C1_O,RFsampleMoraO_tr$CuotMora)

CART_c1_O_ts<- predict(fitC1, RFsampleMoraO_ts,type = "class")
confusionMatrix(CART_c1_O_ts,RFsampleMoraO_ts$CuotMora)

#CART para cuotas en mora VARIABLES CONSTRUIDAS (RANGOS)#########################################################################################
fitC_R<- rpart(CuotMora ~ ., data = RFsampleMoraR_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_C_R_rpart.pdf")
plot(fitC_R, margin = 0.1)#visualizar las divisiones 
text(fitC_R, cex = 0.75)
dev.off()

CART_C_R<- predict(fitC_R, type = "class")
confusionMatrix(CART_C_R,RFsampleMoraR_tr$CuotMora)

CART_C_R_ts<- predict(fitC_R, RFsampleMoraR_ts,type = "class")
confusionMatrix(CART_C_R_ts,RFsampleMoraR_ts$CuotMora)

#CART para cuotas en mora VARIABLES CONSTRUIDAS (RANGOS)~Scoring##############################################################################
fitC_R1<- rpart(CuotMora ~ Scoring.binned, data = RFsampleMoraR_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_C_R1_rpart.pdf")
plot(fitC_R1, margin = 0.1)#visualizar las divisiones 
text(fitC_R1, cex = 0.75)
dev.off()

CART_C_R1<- predict(fitC_R1, type = "class")
confusionMatrix(CART_C_R1,RFsampleMoraR_tr$CuotMora)

CART_C_R1_ts<- predict(fitC_R1, RFsampleMoraR_ts,type = "class")
confusionMatrix(CART_C_R1_ts,RFsampleMoraR_ts$CuotMora)

#CART para default VARIABLES ORIGINALES###############################################################################################
fitD<- rpart(Default ~ ., data = RFsampleDefaultO_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_D_O_rpart.pdf")
plot(fitD, margin = 0.1)#visualizar las divisiones 
text(fitD, cex = 0.75)
dev.off()

pdf("CART_D_O_VIP.pdf")
vip(fitD,num_features=40) 
dev.off()

CART_D_O<- predict(fitD, type = "class")
confusionMatrix(CART_D_O,RFsampleDefaultO_tr$Default)

CART_D_O_ts<- predict(fitD, RFsampleDefaultO_ts,type = "class")
confusionMatrix(CART_D_O_ts,RFsampleDefaultO_ts$Default)

#CART para default VARIABLES ORIGINALES~Scoring###############################################################################################
fitD1<- rpart(Default ~ Scoring, data = RFsampleDefaultO_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_D1_O_rpart.pdf")
plot(fitD1, margin = 0.1)#visualizar las divisiones 
text(fitD1, cex = 0.75)
dev.off()

CART_D1_O<- predict(fitD1, type = "class")
confusionMatrix(CART_D1_O,RFsampleDefaultO_tr$Default)

CART_D1_O_ts<- predict(fitD1, RFsampleDefaultO_ts,type = "class")
confusionMatrix(CART_D1_O_ts,RFsampleDefaultO_ts$Default)

#train CART para default VARIABLES ORIGINALES###############################################################################################
train_fitD<- train(Default ~ .,method = "rpart",tuneGrid = data.frame(cp = seq(0.0, 1, len = 25)),data = RFsampleDefaultO_tr)
pdf("CART_D_O_train.pdf")
plot(train_fitD)
dev.off()

trainCART_D_O<- predict(train_fitD, type = "raw")
confusionMatrix(trainCART_D_O,RFsampleDefaultO_tr$Default)

trainCART_D_O_ts<- predict(train_fitD, RFsampleDefaultO_ts,type = "raw")
confusionMatrix(trainCART_D_O_ts,RFsampleDefaultO_ts$Default)

#CART para default VARIABLES ORIGINALES hipotesis###############################################################################################
fitD2<- rpart(Default ~ ., data = RFsampleDefaultO_tr[,c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)], 
             control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_D2_O_rpart.pdf")
plot(fitD2, margin = 0.1)#visualizar las divisiones 
text(fitD2, cex = 0.75)
dev.off()

pdf("CART_D2_O_VIP.pdf")
vip(fitD2,num_features=40) 
dev.off()

CART_D2_O<- predict(fitD2, type = "class")
confusionMatrix(CART_D2_O,RFsampleDefaultO_tr$Default)

CART_D2_O_ts<- predict(fitD2, RFsampleDefaultO_ts,type = "class")
confusionMatrix(CART_D2_O_ts,RFsampleDefaultO_ts$Default)

#CART para default VARIABLES ORIGINALES sin faltantes###############################################################################################
fitD3<- rpart(Default ~ ., data = RFsampleDefaultO_tr[,c(1:16, 18:21, 23:64)],control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_D3_O_rpart.pdf")
plot(fitD3, margin = 0.1)#visualizar las divisiones 
text(fitD3, cex = 0.75)
dev.off()

pdf("CART_D3_O_VIP.pdf")
vip(fitD3,num_features=40) 
dev.off()

CART_D3_O<- predict(fitD3, type = "class")
confusionMatrix(CART_D3_O,RFsampleDefaultO_tr$Default)

CART_D3_O_ts<- predict(fitD3, RFsampleDefaultO_ts,type = "class")
confusionMatrix(CART_D3_O_ts,RFsampleDefaultO_ts$Default)

#CART para default VARIABLES CONSTRUIDAS (RANGOS)###############################################################################################
fitD_R<- rpart(Default ~ ., data = RFsampleDefaultR_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_D_R_rpart.pdf")
plot(fitD_R, margin = 0.1)#visualizar las divisiones 
text(fitD_R, cex = 0.75)
dev.off()

CART_D_R<- predict(fitD_R, type = "class")
confusionMatrix(CART_D_R,RFsampleDefaultR_tr$Default)

CART_D_R_ts<- predict(fitD_R, RFsampleDefaultR_ts,type = "class")
confusionMatrix(CART_D_R_ts,RFsampleDefaultR_ts$Default)

#CART para riesgo VARIABLES ORIGINALES###############################################################################################
fitR<- rpart(Riesgo ~ ., data = RFsampleRiskO_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_R_O_rpart.pdf")
plot(fitR, margin = 0.1)#visualizar las divisiones 
text(fitR, cex = 0.75)
dev.off()

CART_R_O<- predict(fitR, type = "class")
confusionMatrix(CART_R_O,RFsampleRiskO_tr$Riesgo)

CART_R_O_ts<- predict(fitR, RFsampleRiskO_ts,type = "class")
confusionMatrix(CART_R_O_ts,RFsampleRiskO_ts$Riesgo)

#CART para riesgo VARIABLES CONSTRUIDAS (RANGOS)###############################################################################################
fitR_R<- rpart(Riesgo ~ ., data = RFsampleRiskR_tr, control = rpart.control(cp =  0.0, minsplit = 2))
pdf("CART_R_R_rpart.pdf")
plot(fitR_R, margin = 0.1)#visualizar las divisiones 
text(fitR_R, cex = 0.75)
dev.off()

CART_R_R<- predict(fitR_R, type = "class")
confusionMatrix(CART_R_R,RFsampleRiskR_tr$Riesgo)

CART_R_R_ts<- predict(fitR_R, RFsampleRiskR_ts,type = "class")
confusionMatrix(CART_R_R_ts,RFsampleRiskR_ts$Riesgo)



###RANDOM FOREST MODELS############################################################################################################################
#mtry: Number of variables randomly sampled as candidates at each split.
#ntree: Number of trees to grow.
#method="rf",'cforest',"ranger","xgbTree";

###trainControl()
#index = group_folds --> INSTEAD "number="
#p= the training percentage for the LGOCV #verboseIter: A logical for printing a training log.

##class weigth
#sampsize=c(6,6) 

controlBT<- trainControl(method="boot", number=100)
controlCV10<- trainControl(method="cv", number=10) 
controlRCV<- trainControl(method="repeatedcv", number=10, repeats=3)
controlRCV_S<- trainControl(method='repeatedcv',number=10,repeats=3, search='random')
controlRCV_G<- trainControl(method='repeatedcv',number=10,repeats=3, search='grid')
controlLOOCV<- trainControl(method="LOOCV") 
controlCV5<- trainControl(method = "cv",number = 5,savePredictions = TRUE,classProbs = TRUE,allowParallel = TRUE, returnResamp = "all")

gridSE<- expand.grid(mtry = seq(4, (ncol(RFsampleMoraO_tr[,-c(63:64)])-1), 4))
tunegrid<-expand.grid(mtry = (1:8))
parametersGrid<-  expand.grid(eta = 0.1, colsample_bytree=c(0.5,0.9),max_depth=c(4, 8),
                               nrounds=c(200,1000),gamma=c(1,3),min_child_weight=c(1, 3),subsample = c(0.5,1))

##RF para Default VARIABLES ORIGINALES##################################################################################################
#0.rf Scoring,controlBT,tuneGrid###########################################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  modRF_D0<- train(Default ~ Scoring, data=RFsampleDefaultO_tr, method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(i,i))
  modRF_D0$finalModel
  RF_d0_O<- predict(modRF_D0, type = "raw")
  print(confusionMatrix(RF_d0_O,RFsampleDefaultO_tr$Default))
  RF_d0_O_ts<- predict(modRF_D0, RFsampleDefaultO_ts,type = "raw")
  print(confusionMatrix(RF_d0_O_ts,RFsampleDefaultO_ts$Default))
}
set.seed(123)
modRF_D0<- train(Default ~ Scoring, data=RFsampleDefaultO_tr, method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(5,5))
modRF_D0$finalModel
RF_d0_O<- predict(modRF_D0, type = "raw")
confusionMatrix(RF_d0_O,RFsampleDefaultO_tr$Default)
RF_d0_O_ts<- predict(modRF_D0, RFsampleDefaultO_ts,type = "raw")
confusionMatrix(RF_d0_O_ts,RFsampleDefaultO_ts$Default)

pdf("mtry_D0_O.pdf")
plot(modRF_D0)
dev.off()

#1.rf todas las variables,controlBT,tuneGrid###########################################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  modRF_D <- train(Default ~ ., data=RFsampleDefaultO_tr, method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(i,i))
  modRF_D$finalModel
  RF_d_O<- predict(modRF_D, type = "raw")
  print(confusionMatrix(RF_d_O,RFsampleDefaultO_tr$Default))
  RF_d_O_ts<- predict(modRF_D, RFsampleDefaultO_ts,type = "raw")
  print(confusionMatrix(RF_d_O_ts,RFsampleDefaultO_ts$Default))
}  
set.seed(123)  
modRF_D <- train(Default ~ ., data=RFsampleDefaultO_tr, method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(10,10))
modRF_D$finalModel
RF_d_O<- predict(modRF_D, type = "raw")
confusionMatrix(RF_d_O,RFsampleDefaultO_tr$Default)
RF_d_O_ts<- predict(modRF_D, RFsampleDefaultO_ts,type = "raw")
confusionMatrix(RF_d_O_ts,RFsampleDefaultO_ts$Default)
pdf("modRF_D.pdf")
plot(modRF_D)
dev.off()
pdf("varImp_D_O.pdf")
vip(modRF_D,num_features=100) 
dev.off()

#2.rf variables hipotesis,controlBT,tuneGrid#######################################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  modRF_D2<- train(Default~., data=RFsampleDefaultO_tr[,c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)], 
                 method="rf",trControl=controlBT,tuneGrid=tunegrid,sampsize=c(i,i), importance = TRUE)
  modRF_D2$finalModel
  RF_d_O2<- predict(modRF_D2, type = "raw")
  print(confusionMatrix(RF_d_O2,RFsampleDefaultO_tr$Default))
  RF_d_O2_ts<- predict(modRF_D2, RFsampleDefaultO_ts,type = "raw")
  print(confusionMatrix(RF_d_O2_ts,RFsampleDefaultO_ts$Default))
}
set.seed(123)
modRF_D2<- train(Default~., data=RFsampleDefaultO_tr[,c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)], 
                 method="rf",trControl=controlBT,tuneGrid=tunegrid,sampsize=c(22,22))
modRF_D2$finalModel
RF_d_O2<- predict(modRF_D2, type = "raw")
confusionMatrix(RF_d_O2,RFsampleDefaultO_tr$Default)
RF_d_O2_ts<- predict(modRF_D2, RFsampleDefaultO_ts,type = "raw")
confusionMatrix(RF_d_O2_ts,RFsampleDefaultO_ts$Default)
pdf("mtry_D_O2.pdf")
plot(modRF_D2)
dev.off()
pdf("varImp_D_O2.pdf")
vip(modRF_D2,num_features=40) 
dev.off()

#3.rf variables mas importante del modelo 2,controlBT############################################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  modRF_D3<- train(Default ~ ., data=RFsampleDefaultO_tr[,c(1,7,27,29,30,32,34,35,41,50,64)],
                   method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(i,i),importance=TRUE)
  modRF_D3$finalModel
  RF_d_O3<- predict(modRF_D3, type = "raw")
  print(confusionMatrix(RF_d_O3,RFsampleDefaultO_tr$Default))
  RF_d_O3_ts<- predict(modRF_D3, RFsampleDefaultO_ts,type = "raw")
  print(confusionMatrix(RF_d_O3_ts,RFsampleDefaultO_ts$Default))
}
set.seed(123)
modRF_D3<- train(Default ~ ., data=RFsampleDefaultO_tr[,c(1,7,27,29,30,32,34,35,41,50,64)],
                 method="rf", trControl=controlBT,tuneGrid=tunegrid,sampsize=c(18,18))
modRF_D3$finalModel
RF_d_O3<- predict(modRF_D3, type = "raw")
confusionMatrix(RF_d_O3,RFsampleDefaultO_tr$Default)
RF_d_O3_ts<- predict(modRF_D3, RFsampleDefaultO_ts,type = "raw")
confusionMatrix(RF_d_O3_ts,RFsampleDefaultO_ts$Default)
pdf("mtry_D_O3.pdf")
plot(modRF_D3)
dev.off()
pdf("varImp_D_O3.pdf")
vip(modRF_D3,num_features=40) 
dev.off()

#4.rf variables hipotesis###############################################################################################################
for (i in 4:6) {
  print(i)
  set.seed(123)
  rfD_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                 method = "rf", metric='Accuracy',sampsize=c(i,i))
  pt_RF_D_O4<-predict(rfD_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D_O4,RFsampleDefaultO_tr$Default)) 
  p_RF_D_O4<-predict(rfD_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D_O4,RFsampleDefaultO_ts$Default))
}
set.seed(123)
rfD_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
               method = "rf", metric='Accuracy',sampsize=c(17,17))
pt_RF_D_O4<-predict(rfD_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D_O4,RFsampleDefaultO_tr$Default) 
p_RF_D_O4<-predict(rfD_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D_O4,RFsampleDefaultO_ts$Default)
pdf("rfD_O4.pdf")
plot(rfD_O4)
dev.off()
pdf("varImp_RF_D_O4.pdf")
vip(rfD_O4,num_features=40) 
dev.off()

#4.1rf variables hipotesis, controlCV5###############################################################################################################
for (i in 1:12) {
  print(i)
  set.seed(123)
  rfD1_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                    method = "rf",trControl = controlCV5, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D1_O<-predict(rfD1_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D1_O4,RFsampleDefaultO_tr$Default))
  p_RF_D1_O4<-predict(rfD1_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D1_O4,RFsampleDefaultO_ts$Default))
}

set.seed(123)
rfD1_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlCV5, metric='Accuracy')#,sampsize=c(10,10)

pt_RF_D1_O<-predict(rfD1_O4,RFsampleDefaultO_tr)
print(confusionMatrix(pt_RF_D1_O4,RFsampleDefaultO_tr$Default))

p_RF_D1_O4<-predict(rfD1_O4,RFsampleDefaultO_ts)
print(confusionMatrix(p_RF_D1_O4,RFsampleDefaultO_ts$Default))

pdf("rfD1_O4.pdf")
plot(rfD1_O4)
dev.off()

pdf("varImp_RF_D1_O4.pdf")
vip(rfD1_O4,num_features=50) 
dev.off()

#4.2*rf variables hipotesis, controlCV5,tunegrid###############################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  rfD2_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlCV5,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D2_O4<-predict(rfD2_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D2_O4,RFsampleDefaultO_tr$Default))
  p_RF_D2_O4<-predict(rfD2_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D2_O4,RFsampleDefaultO_ts$Default))
}
set.seed(123)
rfD2_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlCV5,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(3,3))
pt_RF_D2_O4<-predict(rfD2_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D2_O4,RFsampleDefaultO_tr$Default) 
p_RF_D2_O4<-predict(rfD2_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D2_O4,RFsampleDefaultO_ts$Default)
pdf("rfD2_O4.pdf")
plot(rfD2_O4)
dev.off()
pdf("varImp_RF_D2_O4.pdf")
vip(rfD2_O4,num_features=40) 
dev.off()

#4.3rf variables hipotesis,controlCV10,tunegrid###############################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  rfD3_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D3_O4<-predict(rfD3_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D3_O4,RFsampleDefaultO_tr$Default) )
  p_RF_D3_O4<-predict(rfD3_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D3_O4,RFsampleDefaultO_ts$Default))
}
  
set.seed(123)
rfD3_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(2,2))
pt_RF_D3_O4<-predict(rfD3_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D3_O4,RFsampleDefaultO_tr$Default) 
p_RF_D3_O4<-predict(rfD3_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D3_O4,RFsampleDefaultO_ts$Default)

pdf("rfD3_O4.pdf")
plot(rfD3_O4)
dev.off()

pdf("varImp_RF_D3_O4.pdf")
vip(rfD3_O4,num_features=40) 
dev.off()

#4.4rf variables hipotesis, controlRCV,tunegrid###############################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  rfD4_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlRCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D4_O4<-predict(rfD4_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D4_O4,RFsampleDefaultO_tr$Default)) 
  p_RF_D4_O4<-predict(rfD4_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D4_O4,RFsampleDefaultO_ts$Default))
}
set.seed(123)
rfD4_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlRCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(3,3))
pt_RF_D4_O4<-predict(rfD4_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D4_O4,RFsampleDefaultO_tr$Default) 
p_RF_D4_O4<-predict(rfD4_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D4_O4,RFsampleDefaultO_ts$Default)
pdf("rfD4_O4.pdf")

plot(rfD4_O4)
dev.off()
pdf("varImp_RF_D4_O4.pdf")
vip(rfD4_O4,num_features=40) 
dev.off()

#4.5rf variables hipotesis, controlRCV_S,tunegrid###############################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  rfD5_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlRCV_S,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D5_O4<-predict(rfD5_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D5_O4,RFsampleDefaultO_tr$Default) )
  p_RF_D5_O4<-predict(rfD5_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D5_O4,RFsampleDefaultO_ts$Default))
}
set.seed(123)
rfD5_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlRCV_S,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(3,3))
pt_RF_D5_O4<-predict(rfD5_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D5_O4,RFsampleDefaultO_tr$Default) 
p_RF_D5_O4<-predict(rfD5_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D5_O4,RFsampleDefaultO_ts$Default)
pdf("rfD5_O4.pdf")
plot(rfD5_O4)
dev.off()
pdf("varImp_RF_D5_O4.pdf")
vip(rfD5_O4,num_features=40) 
dev.off()

#4.6rf variables hipotesis, controlRCV_G,tunegrid###############################################################################################################
for (i in 1:12) {
  print(i)
set.seed(123)
rfD6_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlRCV_G,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
pt_RF_D6_O4<-predict(rfD6_O4,RFsampleDefaultO_tr)
print(confusionMatrix(pt_RF_D6_O4,RFsampleDefaultO_tr$Default) )
p_RF_D6_O4<-predict(rfD6_O4,RFsampleDefaultO_ts)
print(confusionMatrix(p_RF_D6_O4,RFsampleDefaultO_ts$Default))
}

set.seed(123)
rfD6_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlRCV_G,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(3,3))
pt_RF_D6_O4<-predict(rfD6_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D6_O4,RFsampleDefaultO_tr$Default) 
p_RF_D6_O4<-predict(rfD6_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D6_O4,RFsampleDefaultO_ts$Default)

pdf("rfD6_O4.pdf")
plot(rfD6_O4)
dev.off()
pdf("varImp_RF_D6_O4.pdf")
vip(rfD6_O4,num_features=40) 
dev.off()

#4.7rf variables hipotesis, controlLOOCV,tunegrid###############################################################################################################
for (i in 13:24) {
  print(i)
  set.seed(123)
  rfD7_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                  method = "rf",trControl = controlLOOCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(i,i))
  pt_RF_D7_O4<-predict(rfD7_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_RF_D7_O4,RFsampleDefaultO_tr$Default) )
  p_RF_D7_O4<-predict(rfD7_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_RF_D7_O4,RFsampleDefaultO_ts$Default))
}

set.seed(123)
rfD7_O4<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3,5:7,12,14:16,19,21,23,25:35,40,41,45,49,50,62:64)],
                method = "rf",trControl = controlLOOCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(3,3))
pt_RF_D7_O4<-predict(rfD7_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D7_O4,RFsampleDefaultO_tr$Default) 
p_RF_D7_O4<-predict(rfD7_O4,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D7_O4,RFsampleDefaultO_ts$Default)
pdf("rfD7_O4.pdf")
plot(rfD7_O4)
dev.off()
pdf("varImp_RF_D7_O4.pdf")
vip(rfD7_O4,num_features=40) 
dev.off()

#4.8 rf controlCV5###############################################################################################################
for (i in 25:30) {
  print(i)
  set.seed(1234)
  rfD8_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,63:64)],
                  method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5,ntree=200)
  pt_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_rfD8_O4,RFsampleDefaultO_tr$Default))
  p_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD8_O4,RFsampleDefaultO_ts$Default))
}
for (i in c(seq(110,190,10))){
  print(i)
  set.seed(1234)
  rfD8_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,63:64)],
                method = "rf", metric='Accuracy',sampsize=c(24,24),trControl = controlCV5,ntree=i)
  pt_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_rfD8_O4,RFsampleDefaultO_tr$Default))
  p_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD8_O4,RFsampleDefaultO_ts$Default))
}
set.seed(1234)
rfD8_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,63:64)],
                method = "rf", metric='Accuracy',sampsize=c(24,24),trControl = controlCV5,ntree=150)
pt_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_rfD8_O4,RFsampleDefaultO_tr$Default)
p_rfD8_O4<-predict(rfD8_O4,RFsampleDefaultO_ts)
confusionMatrix(p_rfD8_O4,RFsampleDefaultO_ts$Default)
pdf("rfD8_O4.pdf")
plot(rfD8_O4)
dev.off()
pdf("varImp_rfD8_O4.pdf")
vip(rfD8_O4,num_features=40) 
dev.off()

#4.9 rf controlCV5###############################################################################################################
for (i in 21:24) {
  print(i)
  set.seed(1234)
  rfD9_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,64)],
                  method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5)
  #pt_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_tr)
  #print(confusionMatrix(pt_rfD9_O4,RFsampleDefaultO_tr$Default))
  p_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD9_O4,RFsampleDefaultO_ts$Default))
}
for (i in c(seq(200,600,50))){
  print(i)
  set.seed(1234)
  rfD9_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,64)],
                  method = "rf", metric='Accuracy',sampsize=c(11,11),trControl = controlCV5,ntree=i)
  #pt_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_tr)
  #print(confusionMatrix(pt_rfD9_O4,RFsampleDefaultO_tr$Default))
  p_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD9_O4,RFsampleDefaultO_ts$Default))
}
set.seed(1234)
rfD9_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,32,34,64)],
                method = "rf", metric='Accuracy',sampsize=c(11,11),trControl = controlCV5,ntree=350)
pt_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_rfD9_O4,RFsampleDefaultO_tr$Default)
p_rfD9_O4<-predict(rfD9_O4,RFsampleDefaultO_ts)
confusionMatrix(p_rfD9_O4,RFsampleDefaultO_ts$Default)
pdf("rfD9_O4.pdf")
plot(rfD9_O4)
dev.off()
pdf("varImp_rfD9_O4.pdf")
vip(rfD9_O4,num_features=40) 
dev.off()

#4.10 rf controlCV5###############################################################################################################
for (i in 8:9) {
  print(i)
  set.seed(1234)
  rfD10_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,30,32,34,63,64)],
                   method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5)
  #pt_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_tr)
  #print(confusionMatrix(pt_rfD10_O4,RFsampleDefaultO_tr$Default))
  p_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD10_O4,RFsampleDefaultO_ts$Default))
}
for (i in c(seq(490,500,10))){
  print(i)
  set.seed(1234)
  rfD10_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,30,32,34,63,64)],
                   method = "rf", metric='Accuracy',sampsize=c(9,9),trControl = controlCV5,ntree=i)
  #pt_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_tr)
  #print(confusionMatrix(pt_rfD10_O4,RFsampleDefaultO_tr$Default))
  p_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD10_O4,RFsampleDefaultO_ts$Default))
}
set.seed(1234)
rfD10_O4<- train(Default~.,data = RFsampleDefaultO_tr[,c(1,30,32,34,63,64)],
                 method = "rf", metric='Accuracy',sampsize=c(9,9),trControl = controlCV5,ntree=500)
pt_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_rfD10_O4,RFsampleDefaultO_tr$Default)
p_rfD10_O4<-predict(rfD10_O4,RFsampleDefaultO_ts)
confusionMatrix(p_rfD10_O4,RFsampleDefaultO_ts$Default)
pdf("rfD10_O4.pdf")
plot(rfD10_O4)
dev.off()
pdf("varImp_rfD10_O4.pdf")
vip(rfD10_O4,num_features=40) 
dev.off()

#4.11 rf controlCV5###############################################################################################################
for (i in 1:2) {
  print(i)
  set.seed(1234)
  rfD11_O4<- train(Default~.,data = RFsampleDefaultO_tr[,-c(3:4,12,15:18,19,22:25,55,59)],
                   method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5)
  pt_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_rfD11_O4,RFsampleDefaultO_tr$Default))
  p_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD11_O4,RFsampleDefaultO_ts$Default))
}
for (i in c(seq(100,500,100))){
  print(i)
  set.seed(1234)
  rfD11_O4<- train(Default~.,data = RFsampleDefaultO_tr[,-c(3:4,12,15:18,19,22:25,55,59)],
                   method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=i)
  pt_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_tr)
  print(confusionMatrix(pt_rfD11_O4,RFsampleDefaultO_tr$Default))
  p_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_ts)
  print(confusionMatrix(p_rfD11_O4,RFsampleDefaultO_ts$Default))
}
set.seed(1234)
rfD11_O4<- train(Default~.,data = RFsampleDefaultO_tr[,-c(3:4,12,15:18,19,22:25,55,59)],
                 method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=500)
pt_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_tr)
confusionMatrix(pt_rfD11_O4,RFsampleDefaultO_tr$Default)
p_rfD11_O4<-predict(rfD11_O4,RFsampleDefaultO_ts)
confusionMatrix(p_rfD11_O4,RFsampleDefaultO_ts$Default)
pdf("rfD11_O4.pdf")
plot(rfD11_O4)
dev.off()
pdf("varImp_rfD11_O4.pdf")
vip(rfD11_O4,num_features=110) 
dev.off()


#4.11 rf controlCV5###############################################################################################################


#1.xgboost todas las variables###############################################################################################################
set.seed(123)
xgboostD_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "xgbTree",scale_pos_weight=5)
xgboostD_O1$finalModel

pdf("xgboostD_O1.pdf")
plot(xgboostD_O1)
dev.off()

pdf("varImp_XG_D_O1.pdf")
vip(xgboostD_O1,num_features=40) 
dev.off()

pt_XG_D_O1<-predict(xgboostD_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D_O1,RFsampleDefaultO_tr$Default)

p_XG_D_O1<-predict(xgboostD_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D_O1,RFsampleDefaultO_ts$Default)

#1.1xgboost controlCV5#############################################################################################################################
set.seed(123)
xgboostD1_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "xgbTree",trControl = controlCV5,scale_pos_weight=5)

pdf("xgboostD1_O1.pdf")
plot(xgboostD1_O1)
dev.off()

pdf("varImp_XG_D1_O1.pdf")
vip(xgboostD1_O1,num_features=40) 
dev.off()

pt_XG_D1_O1<-predict(xgboostD1_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D1_O1,RFsampleDefaultO_tr$Default)

p_XG_D1_O1<-predict(xgboostD1_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D1_O1,RFsampleDefaultO_ts$Default)

#1.2xgboost controlCV5,parametersGrid###############################################################################################################
set.seed(123)
xgboostD2_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "xgbTree",trControl = controlCV5,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD2_O1.pdf")
plot(xgboostD2_O1)
dev.off()

pdf("varImp_XG_D2_O1.pdf")
vip(xgboostD2_O1,num_features=40) 
dev.off()

pt_XG_D2_O1<-predict(xgboostD2_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D2_O1,RFsampleDefaultO_tr$Default)

p_XG_D2_O1<-predict(xgboostD2_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D2_O1,RFsampleDefaultO_ts$Default)

#1.3xgboost controlCV10,parametersGrid##################################################################################################################
set.seed(123)
xgboostD3_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlCV10,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD3_O1.pdf")
plot(xgboostD3_O1)
dev.off()

pdf("varImp_XG_D3_O1.pdf")
vip(xgboostD3_O1,num_features=40) 
dev.off()

pt_XG_D3_O1<-predict(xgboostD3_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D3_O1,RFsampleDefaultO_tr$Default)

p_XG_D3_O1<-predict(xgboostD3_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D3_O1,RFsampleDefaultO_ts$Default)


#1.4xgboost controlRCV,parametersGrid##################################################################################################################
set.seed(123)
xgboostD4_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "xgbTree",
                     trControl = controlRCV,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD4_O1.pdf")
plot(xgboostD4_O1)
dev.off()

pdf("varImp_XG_D4_O1.pdf")
vip(xgboostD4_O1,num_features=40) 
dev.off()

pt_XG_D4_O1<-predict(xgboostD4_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D4_O1,RFsampleDefaultO_tr$Default)

p_XG_D4_O1<-predict(xgboostD4_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D4_O1,RFsampleDefaultO_ts$Default)

#1.5xgboost controlRCV_S,parametersGrid##################################################################################################################
set.seed(123)
xgboostD5_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlRCV_S,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD5_O1.pdf")
plot(xgboostD5_O1)
dev.off()

pdf("varImp_XG_D5_O1.pdf")
vip(xgboostD5_O1,num_features=40) 
dev.off()

pt_XG_D5_O1<-predict(xgboostD5_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D5_O1,RFsampleDefaultO_tr$Default)

p_XG_D5_O1<-predict(xgboostD5_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D5_O1,RFsampleDefaultO_ts$Default)

#1.6xgboost controlRCV_G,parametersGrid##################################################################################################################
set.seed(123)
xgboostD6_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlRCV_G,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD6_O1.pdf")
plot(xgboostD6_O1)
dev.off()

pdf("varImp_XG_D6_O1.pdf")
vip(xgboostD6_O1,num_features=40) 
dev.off()

pt_XG_D6_O1<-predict(xgboostD6_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D6_O1,RFsampleDefaultO_tr$Default)

p_XG_D6_O1<-predict(xgboostD6_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D6_O1,RFsampleDefaultO_ts$Default)

#1.7xgboost controlLOOCV,parametersGrid##################################################################################################################
set.seed(123)
xgboostD7_O1<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "xgbTree",
                     trControl = controlLOOCV,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD7_O1.pdf")
plot(xgboostD7_O1)
dev.off()

pdf("varImp_XG_D7_O1.pdf")
vip(xgboostD7_O1,num_features=40) 
dev.off()

pt_XG_D7_O1<-predict(xgboostD7_O1,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D7_O1,RFsampleDefaultO_tr$Default)

p_XG_D7_O1<-predict(xgboostD7_O1,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D7_O1,RFsampleDefaultO_ts$Default)


#2.xgboost variables hipotesis###############################################################################################################
set.seed(123)
xgboostD_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "xgbTree",scale_pos_weight=5)

pdf("xgboostD_O2.pdf")
plot(xgboostD_O2)
dev.off()

pdf("varImp_XG_D_O2.pdf")
vip(xgboostD_O2,num_features=40) 
dev.off()

pt_XG_D_O2<-predict(xgboostD_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D_O2,RFsampleDefaultO_tr$Default) 

p_XG_D_O2<-predict(xgboostD_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D_O2,RFsampleDefaultO_ts$Default)

#2.1xgboost variables hipotesis,controlCV5###############################################################################################################
set.seed(123)
xgboostD1_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV5,scale_pos_weight=5 )

pdf("xgboostD1_O2.pdf")
plot(xgboostD1_O2)
dev.off()

pdf("varImp_XG_D_O2.pdf")
vip(xgboostD1_O2,num_features=40) 
dev.off()

pt_XG_D1_O2<-predict(xgboostD1_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D1_O2,RFsampleDefaultO_tr$Default) 

p_XG_D1_O2<-predict(xgboostD1_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D1_O2,RFsampleDefaultO_ts$Default)

#2.2xgboost variables hipotesis,controlCV5,parametersGrid###############################################################################################################
set.seed(123)
xgboostD2_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV5,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD2_O2.pdf")
plot(xgboostD2_O2)
dev.off()

pdf("varImp_XG_D2_O2.pdf")
vip(xgboostD2_O2,num_features=40) 
dev.off()

pt_XG_D2_O2<-predict(xgboostD2_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D2_O2,RFsampleDefaultO_tr$Default) 

p_XG_D2_O2<-predict(xgboostD2_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D2_O2,RFsampleDefaultO_ts$Default)


#2.3xgboost variables hipotesis,controlCV10,parametersGrid###############################################################################################################
set.seed(123)
xgboostD3_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV10,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD3_O2.pdf")
plot(xgboostD3_O2)
dev.off()

pdf("varImp_XG_D3_O2.pdf")
vip(xgboostD3_O2,num_features=40) 
dev.off()

pt_XG_D3_O2<-predict(xgboostD3_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D3_O2,RFsampleDefaultO_tr$Default) 

p_XG_D3_O2<-predict(xgboostD3_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D3_O2,RFsampleDefaultO_ts$Default)

#2.4xgboost variables hipotesis,controlRCV###############################################################################################################
set.seed(123)
xgboostD4_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy',scale_pos_weight=5)

pdf("xgboostD4_O2.pdf")
plot(xgboostD4_O2)
dev.off()

pdf("varImp_XG_D4_O2.pdf")
vip(xgboostD4_O2,num_features=40) 
dev.off()

pt_XG_D4_O2<-predict(xgboostD4_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D4_O2,RFsampleDefaultO_tr$Default) 

p_XG_D4_O2<-predict(xgboostD4_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D4_O2,RFsampleDefaultO_ts$Default)


#2.5xgboost variables hipotesis,controlRCV_S###############################################################################################################
set.seed(123)
xgboostD5_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy',scale_pos_weight=5)

pdf("xgboostD5_O2.pdf")
plot(xgboostD5_O2)
dev.off()

pdf("varImp_XG_D5_O2.pdf")
vip(xgboostD5_O2,num_features=40) 
dev.off()

pt_XG_D5_O2<-predict(xgboostD5_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D5_O2,RFsampleDefaultO_tr$Default) 

p_XG_D5_O2<-predict(xgboostD5_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D5_O2,RFsampleDefaultO_ts$Default)

#2.6xgboost variables hipotesis,controlRCV_G###############################################################################################################
set.seed(123)
xgboostD6_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV_G,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD6_O2.pdf")
plot(xgboostD6_O2)
dev.off()

pdf("varImp_XG_D6_O2.pdf")
vip(xgboostD6_O2,num_features=40) 
dev.off()

pt_XG_D6_O2<-predict(xgboostD6_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D6_O2,RFsampleDefaultO_tr$Default) 

p_XG_D6_O2<-predict(xgboostD6_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D6_O2,RFsampleDefaultO_ts$Default)

#2.7xgboost variables hipotesis,controlLOOCV###############################################################################################################
set.seed(123)
xgboostD7_O2<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlLOOCV,tuneGrid=parametersGrid,scale_pos_weight=5 )

pdf("xgboostD7_O2.pdf")
plot(xgboostD7_O2)
dev.off()

pdf("varImp_XG_D7_O2.pdf")
vip(xgboostD7_O2,num_features=40) 
dev.off()

pt_XG_D7_O2<-predict(xgboostD7_O2,RFsampleDefaultO_tr)
confusionMatrix(pt_XG_D7_O2,RFsampleDefaultO_tr$Default) 

p_XG_D7_O2<-predict(xgboostD7_O2,RFsampleDefaultO_ts)
confusionMatrix(p_XG_D7_O2,RFsampleDefaultO_ts$Default)

#3.rf todas las variables###############################################################################################################
set.seed(123)
rfD_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
               method = "rf", metric='Accuracy',sampsize=c(6,6))#6:83% y 75%

pdf("rfD_O3.pdf")
plot(rfD_O3)
dev.off()

pdf("varImp_RF_D_O3.pdf")
vip(rfD_O3,num_features=40) 
dev.off()

pt_RF_D_O3<-predict(rfD_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D_O3,RFsampleDefaultO_tr$Default)

p_RF_D_O3<-predict(rfD_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D_O3,RFsampleDefaultO_ts$Default)

#3.1rf controlCV5#############################################################################################################################
set.seed(123)
rfD1_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlCV5, metric='Accuracy',sampsize=c(6,6))

pdf("rfD1_O3.pdf")
plot(rfD1_O3)
dev.off()

pdf("varImp_RF_D1_O3.pdf")
vip(rfD1_O3,num_features=40) 
dev.off()

pt_RF_D1_O3<-predict(rfD1_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D1_O3,RFsampleDefaultO_tr$Default)

p_RF_D1_O3<-predict(rfD1_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D1_O3,RFsampleDefaultO_ts$Default)

#3.2rf controlCV5,tunegrid###############################################################################################################
set.seed(123)
rfD2_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
          method = "rf",trControl = controlCV5,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pt_RF_D2_O3<-predict(rfD2_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D2_O3,RFsampleDefaultO_tr$Default)

p_RF_D2_O3<-predict(rfD2_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D2_O3,RFsampleDefaultO_ts$Default)

pdf("rfD2_O3.pdf")
plot(rfD2_O3)
dev.off()

pdf("varImp_RF_D2_O3.pdf")
vip(rfD2_O3,num_features=40) 
dev.off()

#3.3rf controlCV10,tunegrid##################################################################################################################
set.seed(123)
rfD3_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pt_RF_D3_O3<-predict(rfD3_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D3_O3,RFsampleDefaultO_tr$Default)

p_RF_D3_O3<-predict(rfD3_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D3_O3,RFsampleDefaultO_ts$Default)

pdf("rfD3_O3.pdf")
plot(rfD3_O3)
dev.off()

pdf("varImp_RF_D3_O3.pdf")
vip(rfD3_O3,num_features=40) 
dev.off()

#3.4rf controlRCV,tunegrid##################################################################################################################
set.seed(123)
rfD4_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlRCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pt_RF_D4_O3<-predict(rfD4_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D4_O3,RFsampleDefaultO_tr$Default)

p_RF_D4_O3<-predict(rfD4_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D4_O3,RFsampleDefaultO_ts$Default)

pdf("rfD4_O3.pdf")
plot(rfD4_O3)
dev.off()

pdf("varImp_RF_D4_O3.pdf")
vip(rfD4_O3,num_features=40) 
dev.off()

#3.5rf controlRCV_S,tunegrid##################################################################################################################
set.seed(123)
rfD5_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlRCV_S,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pt_RF_D5_O3<-predict(rfD5_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D5_O3,RFsampleDefaultO_tr$Default)

p_RF_D5_O3<-predict(rfD5_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D5_O3,RFsampleDefaultO_ts$Default)

pdf("rfD5_O3.pdf")
plot(rfD5_O3)
dev.off()

pdf("varImp_RF_D5_O3.pdf")
vip(rfD5_O3,num_features=40) 
dev.off()

#3.6rf controlRCV_G,tunegrid##################################################################################################################
set.seed(123)
rfD6_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlRCV_G,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pt_RF_D6_O3<-predict(rfD6_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D6_O3,RFsampleDefaultO_tr$Default)

p_RF_D6_O3<-predict(rfD6_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D6_O3,RFsampleDefaultO_ts$Default)

pdf("rfD6_O3.pdf")
plot(rfD6_O3)
dev.off()

pdf("varImp_RF_D6_O3.pdf")
vip(rfD6_O3,num_features=40) 
dev.off()

#3.7rf controlLOOCV,tunegrid##################################################################################################################
set.seed(123)
rfD7_O3<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                method = "rf",trControl = controlLOOCV,tuneGrid=tunegrid, sampsize=c(6,6))

pt_RF_D7_O3<-predict(rfD7_O3,RFsampleDefaultO_tr)
confusionMatrix(pt_RF_D7_O3,RFsampleDefaultO_tr$Default)

p_RF_D7_O3<-predict(rfD7_O3,RFsampleDefaultO_ts)
confusionMatrix(p_RF_D7_O3,RFsampleDefaultO_ts$Default)

pdf("rfD7_O3.pdf")
plot(rfD7_O3)
dev.off()

pdf("varImp_RF_D7_O3.pdf")
vip(rfD7_O3,num_features=40) 
dev.off()

#5.ranger todas las variables###############################################################################################################
set.seed(123)
rangerD_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                   method = "ranger", metric='Accuracy')
rangerD_O5$finalModel

pdf("rangerD_O5.pdf")
plot(rangerD_O5)
dev.off()

pt_RANGER_D_O5<-predict(rangerD_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D_O5<-predict(rangerD_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D_O5,RFsampleDefaultO_ts$Default)

#5.1ranger controlCV5#############################################################################################################################
set.seed(123)
rangerD1_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlCV5, metric='Accuracy')

pdf("rangerD1_O5.pdf")
plot(rangerD1_O5)
dev.off()

pt_RANGER_D1_O5<-predict(rangerD1_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D1_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D1_O5<-predict(rangerD1_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D1_O5,RFsampleDefaultO_ts$Default)

#5.3ranger controlCV10##################################################################################################################
set.seed(123)
rangerD3_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlCV10,metric='Accuracy')

pdf("rangerD3_O5.pdf")
plot(rangerD3_O5)
dev.off()

pt_RANGER_D3_O5<-predict(rangerD3_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D3_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D3_O5<-predict(rangerD3_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D3_O5,RFsampleDefaultO_ts$Default)


#5.4ranger controlRCV##################################################################################################################
set.seed(123)
rangerD4_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlRCV, metric='Accuracy')

pdf("rangerD4_O5.pdf")
plot(rangerD4_O5)
dev.off()

pt_RANGER_D4_O5<-predict(rangerD4_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D4_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D4_O5<-predict(rangerD4_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D4_O5,RFsampleDefaultO_ts$Default)

#5.5ranger controlRCV_S##################################################################################################################
set.seed(123)
rangerD5_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlRCV_S, metric='Accuracy')

pdf("rangerD5_O5.pdf")
plot(rangerD5_O5)
dev.off()

pt_RANGER_D5_O5<-predict(rangerD5_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D5_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D5_O5<-predict(rangerD5_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D5_O5,RFsampleDefaultO_ts$Default)

#5.6ranger controlRCV_G##################################################################################################################
set.seed(123)
rangerD6_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlRCV_G, metric='Accuracy')

pdf("rangerD6_O5.pdf")
plot(rangerD6_O5)
dev.off()

pt_RANGER_D6_O5<-predict(rangerD6_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D6_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D6_O5<-predict(rangerD6_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D6_O5,RFsampleDefaultO_ts$Default)

#5.7ranger controlLOOCV##################################################################################################################
set.seed(123)
rangerD7_O5<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "ranger",trControl = controlLOOCV, metric='Accuracy')

pdf("rangerD7_O5.pdf")
plot(rangerD7_O5)
dev.off()

pt_RANGER_D7_O5<-predict(rangerD7_O5,RFsampleDefaultO_tr)
confusionMatrix(pt_RANGER_D7_O5,RFsampleDefaultO_tr$Default)

p_RANGER_D7_O5<-predict(rangerD7_O5,RFsampleDefaultO_ts)
confusionMatrix(p_RANGER_D7_O5,RFsampleDefaultO_ts$Default)


#7.cforest todas las variables###############################################################################################################
set.seed(123)
cforestD_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],
                    method = "cforest", metric='Accuracy')
cforestD_O7$finalModel

pdf("cforestD_O7.pdf")
plot(cforestD_O7)
dev.off()

pdf("varImp_CFOREST_D_O7.pdf")
vip(cforestD_O7,num_features=40) 
dev.off()

pt_CFOREST_D_O7<-predict(cforestD_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D_O7<-predict(cforestD_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D_O7,RFsampleDefaultO_ts$Default)

#7.1cforest controlCV5#############################################################################################################################
set.seed(123)
cforestD1_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlCV5, metric='Accuracy')

pdf("cforestD1_O7.pdf")
plot(cforestD1_O7)
dev.off()

pdf("varImp_CFOREST_D1_O7.pdf")
vip(cforestD1_O7,num_features=40) 
dev.off()

pt_CFOREST_D1_O7<-predict(cforestD1_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D1_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D1_O7<-predict(cforestD1_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D1_O7,RFsampleDefaultO_ts$Default)

#7.2cforest controlCV5,parametersGrid###############################################################################################################
set.seed(123)
cforestD2_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD2_O7.pdf")
plot(cforestD2_O7)
dev.off()

pdf("varImp_CFOREST_D2_O7.pdf")
vip(cforestD2_O7,num_features=40) 
dev.off()

pt_CFOREST_D2_O7<-predict(cforestD2_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D2_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D2_O7<-predict(cforestD2_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D2_O7,RFsampleDefaultO_ts$Default)

#7.3cforest controlCV10,parametersGrid##################################################################################################################
set.seed(123)
cforestD3_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlCV10,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD3_O7.pdf")
plot(cforestD3_O7)
dev.off()

pdf("varImp_CFOREST_D3_O7.pdf")
vip(cforestD3_O7,num_features=40) 
dev.off()

pt_CFOREST_D3_O7<-predict(cforestD3_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D3_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D3_O7<-predict(cforestD3_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D3_O7,RFsampleDefaultO_ts$Default)


#7.4cforest controlRCV,parametersGrid##################################################################################################################
set.seed(123)
cforestD4_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD4_O7.pdf")
plot(cforestD4_O7)
dev.off()

pdf("varImp_CFOREST_D4_O7.pdf")
vip(cforestD4_O7,num_features=40) 
dev.off()

pt_CFOREST_D4_O7<-predict(cforestD4_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D4_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D4_O7<-predict(cforestD4_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D4_O7,RFsampleDefaultO_ts$Default)

#7.5cforest controlRCV_S,parametersGrid##################################################################################################################
set.seed(123)
cforestD5_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD5_O7.pdf")
plot(cforestD5_O7)
dev.off()

pdf("varImp_CFOREST_D5_O7.pdf")
vip(cforestD5_O7,num_features=40) 
dev.off()

pt_CFOREST_D5_O7<-predict(cforestD5_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D5_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D5_O7<-predict(cforestD5_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D5_O7,RFsampleDefaultO_ts$Default)

#7.6cforest controlRCV_G,parametersGrid##################################################################################################################
set.seed(123)
cforestD6_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD6_O7.pdf")
plot(cforestD6_O7)
dev.off()

pdf("varImp_CFOREST_D6_O7.pdf")
vip(cforestD6_O7,num_features=40) 
dev.off()

pt_CFOREST_D6_O7<-predict(cforestD6_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D6_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D6_O7<-predict(cforestD6_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D6_O7,RFsampleDefaultO_ts$Default)

#7.7cforest controlLOOCV,parametersGrid##################################################################################################################
set.seed(123)
cforestD7_O7<- train(Default~., data = RFsampleDefaultO_tr[-c(63,64)],method = "cforest",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD7_O7.pdf")
plot(cforestD7_O7)
dev.off()

pdf("varImp_CFOREST_D7_O7.pdf")
vip(cforestD7_O7,num_features=40) 
dev.off()

pt_CFOREST_D7_O7<-predict(cforestD7_O7,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D7_O7,RFsampleDefaultO_tr$Default)

p_CFOREST_D7_O7<-predict(cforestD7_O7,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D7_O7,RFsampleDefaultO_ts$Default)


#8.cforest variables hipotesis###############################################################################################################
set.seed(123)
cforestD_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "cforest", metric='Accuracy')

pdf("cforestD_O8.pdf")
plot(cforestD_O8)
dev.off()

pdf("varImp_CFOREST_D_O8.pdf")
vip(cforestD_O8,num_features=40) 
dev.off()

pt_CFOREST_D_O8<-predict(cforestD_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D_O8<-predict(cforestD_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D_O8,RFsampleDefaultO_ts$Default)

#8.1cforest variables hipotesis, controlCV5###############################################################################################################
set.seed(123)
cforestD1_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "cforest",trControl = controlCV5, metric='Accuracy')

pdf("cforestD1_O8.pdf")
plot(cforestD1_O8)
dev.off()

pdf("varImp_CFOREST_D1_O8.pdf")
vip(cforestD1_O8,num_features=40) 
dev.off()

pt_CFOREST_D1_O8<-predict(cforestD1_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D1_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D1_O8<-predict(cforestD1_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D1_O8,RFsampleDefaultO_ts$Default)

#8.2cforest variables hipotesis, controlCV5,parametersGrid###############################################################################################################
set.seed(123)
cforestD2_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD2_O8.pdf")
plot(cforestD2_O8)
dev.off()

pdf("varImp_CFOREST_D2_O8.pdf")
vip(cforestD2_O8,num_features=40) 
dev.off()

pt_CFOREST_D2_O8<-predict(cforestD2_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D2_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D2_O8<-predict(cforestD2_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D2_O8,RFsampleDefaultO_ts$Default)


#8.3cforest variables hipotesis,controlCV10,tunegrid###############################################################################################################
set.seed(123)
cforestD3_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy')

pdf("cforestD3_O8.pdf")
plot(cforestD3_O8)
dev.off()

pdf("varImp_CFOREST_D3_O8.pdf")
vip(cforestD3_O8,num_features=40) 
dev.off()

pt_CFOREST_D3_O8<-predict(cforestD3_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D3_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D3_O8<-predict(cforestD3_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D3_O8,RFsampleDefaultO_ts$Default)

#8.4cforest variables hipotesis, controlRCV###############################################################################################################
set.seed(123)
cforestD4_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD4_O8.pdf")
plot(cforestD4_O8)
dev.off()

pdf("varImp_CFOREST_D4_O8.pdf")
vip(cforestD4_O8,num_features=40) 
dev.off()

pt_CFOREST_D4_O8<-predict(cforestD4_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D4_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D4_O8<-predict(cforestD4_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D4_O8,RFsampleDefaultO_ts$Default)


#8.5cforest variables hipotesis, controlRCV_S###############################################################################################################
set.seed(123)
cforestD5_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD5_O8.pdf")
plot(cforestD5_O8)
dev.off()

pdf("varImp_CFOREST_D5_O8.pdf")
vip(cforestD5_O8,num_features=40) 
dev.off()

pt_CFOREST_D5_O8<-predict(cforestD5_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D5_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D5_O8<-predict(cforestD5_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D5_O8,RFsampleDefaultO_ts$Default)

#8.6cforest variables hipotesis, controlRCV_G###############################################################################################################
set.seed(123)
cforestD6_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD6_O8.pdf")
plot(cforestD6_O8)
dev.off()

pdf("varImp_CFOREST_D6_O8.pdf")
vip(cforestD6_O8,num_features=40) 
dev.off()

pt_CFOREST_D6_O8<-predict(cforestD6_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D6_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D6_O8<-predict(cforestD6_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D6_O8,RFsampleDefaultO_ts$Default)

#8.7cforest variables hipotesis, controlLOOCV###############################################################################################################
set.seed(123)
cforestD7_O8<- train(Default~.,data = RFsampleDefaultO_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD7_O8.pdf")
plot(cforestD7_O8)
dev.off()

pdf("varImp_CFOREST_D7_O8.pdf")
vip(cforestD7_O8,num_features=40) 
dev.off()

pt_CFOREST_D7_O8<-predict(cforestD7_O8,RFsampleDefaultO_tr)
confusionMatrix(pt_CFOREST_D7_O8,RFsampleDefaultO_tr$Default) 

p_CFOREST_D7_O8<-predict(cforestD7_O8,RFsampleDefaultO_ts)
confusionMatrix(p_CFOREST_D7_O8,RFsampleDefaultO_ts$Default) 

##RF para Default VARIABLES CONSTRUIDAS (RANGOS)#########################################################################################
#4.0 rf controlCV5###############################################################################################################
for (i in c(1:10,12,16,18,20,22,24)) {
  print(i)
  set.seed(1234)
  rfD_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1:18,20,21,23,26:35,40,41,45,49,50,62:64)],
                method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5,tuneGrid=tunegrid)
  #pt_rfD_R<-predict(rfD_R,RFsampleDefaultR_tr)
  #print(confusionMatrix(pt_rfD_R,RFsampleDefaultR_tr$Default))
  p_rfD_R<-predict(rfD_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD_R,RFsampleDefaultR_ts$Default))
}

vip(rfD_R,num_features=110) 

for (i in c(seq(440,510,10))){
  print(i)
  set.seed(1234)
  rfD_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1:18,20,21,23,26:35,40,41,45,49,50,62:64)],
                method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=i,tuneGrid=tunegrid)
  pt_rfD_R<-predict(rfD_R,RFsampleDefaultR_tr)
  print(confusionMatrix(pt_rfD_R,RFsampleDefaultR_tr$Default))
  p_rfD_R<-predict(rfD_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD_R,RFsampleDefaultR_ts$Default))
}
set.seed(1234)
rfD_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1:18,20,21,23,26:35,40,41,45,49,50,62:64)],
              method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=440,tuneGrid=tunegrid)
pt_rfD_R<-predict(rfD_R,RFsampleDefaultR_tr)
confusionMatrix(pt_rfD_R,RFsampleDefaultR_tr$Default)
p_rfD_R<-predict(rfD_R,RFsampleDefaultR_ts)
confusionMatrix(p_rfD_R,RFsampleDefaultR_ts$Default)
pdf("rfD_R.pdf")
plot(rfD_R)
dev.off()
pdf("varImp_rfD_R.pdf")
vip(rfD_R,num_features=110) 
dev.off()

#4.01 rf controlCV5###############################################################################################################
for (i in c(1:10,12,15,16,18,20,22,24)) {
  print(i)
  set.seed(1234)
  rfD01_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,3,5,6,7,10,12,14:16,19,21,23,26:35,40,41,45,49,50,62:64)],
                  method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5,tuneGrid=tunegrid)
  #pt_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_tr)
  #print(confusionMatrix(pt_rfD01_R,RFsampleDefaultR_tr$Default))
  p_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD01_R,RFsampleDefaultR_ts$Default))
}

vip(rfD01_R,num_features=110) 

for (i in c(seq(490,520,10))){
  print(i)
  set.seed(1234)
  rfD01_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,3,5,6,7,10,12,14:16,19,21,23,26:35,40,41,45,49,50,62:64)],
                  method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=i,tuneGrid=tunegrid)
  pt_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_tr)
  print(confusionMatrix(pt_rfD01_R,RFsampleDefaultR_tr$Default))
  p_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD01_R,RFsampleDefaultR_ts$Default))
}
set.seed(1234)
rfD01_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,3,5,6,7,10,12,14:16,19,21,23,26:35,40,41,45,49,50,62:64)],
                method = "rf", metric='Accuracy',sampsize=c(1,1),trControl = controlCV5,ntree=500,tuneGrid=tunegrid)
pt_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_tr)
confusionMatrix(pt_rfD01_R,RFsampleDefaultR_tr$Default)
p_rfD01_R<-predict(rfD01_R,RFsampleDefaultR_ts)
confusionMatrix(p_rfD01_R,RFsampleDefaultR_ts$Default)
pdf("rfD01_R.pdf")
plot(rfD01_R)
dev.off()
pdf("varImp_rfD01_R.pdf")
vip(rfD01_R,num_features=110) 
dev.off()

#4.02 rf controlCV5###############################################################################################################
for (i in c(1:10,12,15,20,22,24)) {
  print(i)
  set.seed(1234)
  rfD02_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,7,15,16,23,29,31,33,41,45,64)],
                  method = "rf", metric='Accuracy',sampsize=c(i,i),trControl = controlCV5,tuneGrid=tunegrid)#
  #pt_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_tr)
  #print(confusionMatrix(pt_rfD02_R,RFsampleDefaultR_tr$Default))
  p_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD02_R,RFsampleDefaultR_ts$Default))
}

vip(rfD02_R,num_features=110) 

for (i in c(seq(200,900,100))){
  print(i)
  set.seed(1234)
  rfD02_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,3,5,6,7,10,12,14:16,19,21,23,26:35,40,41,45,49,50,62:64)],
                  method = "rf", metric='Accuracy',sampsize=c(6,6),trControl = controlCV5,ntree=i,tuneGrid=tunegrid)
  pt_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_tr)
  print(confusionMatrix(pt_rfD02_R,RFsampleDefaultR_tr$Default))
  p_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_ts)
  print(confusionMatrix(p_rfD02_R,RFsampleDefaultR_ts$Default))
}
set.seed(1234)
rfD02_R<- train(Default~.,data = RFsampleDefaultR_tr[,c(1,3,5,6,7,10,12,14:16,19,21,23,26:35,40,41,45,49,50,62:64)],
                method = "rf", metric='Accuracy',sampsize=c(6,6),trControl = controlCV5,ntree=500,tuneGrid=tunegrid)
pt_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_tr)
confusionMatrix(pt_rfD02_R,RFsampleDefaultR_tr$Default)
p_rfD02_R<-predict(rfD02_R,RFsampleDefaultR_ts)
confusionMatrix(p_rfD02_R,RFsampleDefaultR_ts$Default)
pdf("rfD02_R.pdf")
plot(rfD02_R)
dev.off()
pdf("varImp_rfD02_R.pdf")
vip(rfD02_R,num_features=110) 
dev.off()


#4.rf variables hipotesis###############################################################################################################
set.seed(123)
rfD_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
               method = "rf", metric='Accuracy',sampsize=c(6,6))

pdf("rfD_R4.pdf")
plot(rfD_R4)
dev.off()

pdf("varImp_RF_D_R4.pdf")
vip(rfD_R4,num_features=40) 
dev.off()

pt_RF_D_R4<-predict(rfD_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D_R4,RFsampleDefaultR_tr$Default) 

p_RF_D_R4<-predict(rfD_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D_R4,RFsampleDefaultR_ts$Default)

#4.1rf variables hipotesis, controlCV5###############################################################################################################
set.seed(123)
rfD1_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlCV5, metric='Accuracy',sampsize=c(6,6))

pdf("rfD1_R4.pdf")
plot(rfD1_R4)
dev.off()

pdf("varImp_RF_D1_R4.pdf")
vip(rfD1_R4,num_features=40) 
dev.off()

pt_RF_D1_R4<-predict(rfD1_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D1_R4,RFsampleDefaultR_tr$Default) 

p_RF_D1_R4<-predict(rfD1_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D1_R4,RFsampleDefaultR_ts$Default)

#4.2rf variables hipotesis, controlCV5,tunegrid###############################################################################################################
set.seed(123)
rfD2_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlCV5,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD2_R4.pdf")
plot(rfD2_R4)
dev.off()

pdf("varImp_RF_D2_R4.pdf")
vip(rfD2_R4,num_features=40) 
dev.off()

pt_RF_D2_R4<-predict(rfD2_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D2_R4,RFsampleDefaultR_tr$Default) 

p_RF_D2_R4<-predict(rfD2_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D2_R4,RFsampleDefaultR_ts$Default)


#4.3rf variables hipotesis,controlCV10,tunegrid###############################################################################################################
set.seed(123)
rfD3_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD3_R4.pdf")
plot(rfD3_R4)
dev.off()

pdf("varImp_RF_D3_R4.pdf")
vip(rfD3_R4,num_features=40) 
dev.off()

pt_RF_D3_R4<-predict(rfD3_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D3_R4,RFsampleDefaultR_tr$Default) 

p_RF_D3_R4<-predict(rfD3_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D3_R4,RFsampleDefaultR_ts$Default)

#4.4rf variables hipotesis, controlRCV,tunegrid###############################################################################################################
set.seed(123)
rfD4_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlRCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD4_R4.pdf")
plot(rfD4_R4)
dev.off()

pdf("varImp_RF_D4_R4.pdf")
vip(rfD4_R4,num_features=40) 
dev.off()

pt_RF_D4_R4<-predict(rfD4_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D4_R4,RFsampleDefaultR_tr$Default) 

p_RF_D4_R4<-predict(rfD4_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D4_R4,RFsampleDefaultR_ts$Default)


#4.5rf variables hipotesis, controlRCV_S,tunegrid###############################################################################################################
set.seed(123)
rfD5_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlRCV_S,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD5_R4.pdf")
plot(rfD5_R4)
dev.off()

pdf("varImp_RF_D5_R4.pdf")
vip(rfD5_R4,num_features=40) 
dev.off()

pt_RF_D5_R4<-predict(rfD5_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D5_R4,RFsampleDefaultR_tr$Default) 

p_RF_D5_R4<-predict(rfD5_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D5_R4,RFsampleDefaultR_ts$Default)

#4.6rf variables hipotesis, controlRCV_G,tunegrid###############################################################################################################
set.seed(123)
rfD6_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlRCV_G,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD6_R4.pdf")
plot(rfD6_R4)
dev.off()

pdf("varImp_RF_D6_R4.pdf")
vip(rfD6_R4,num_features=40) 
dev.off()

pt_RF_D6_R4<-predict(rfD6_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D6_R4,RFsampleDefaultR_tr$Default) 

p_RF_D6_R4<-predict(rfD6_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D6_R4,RFsampleDefaultR_ts$Default)

#4.7rf variables hipotesis, controlLOOCV,tunegrid###############################################################################################################
set.seed(123)
rfD7_R4<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                method = "rf",trControl = controlLOOCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD7_R4.pdf")
plot(rfD7_R4)
dev.off()

pdf("varImp_RF_D7_R4.pdf")
vip(rfD7_R4,num_features=40) 
dev.off()

pt_RF_D7_R4<-predict(rfD7_R4,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D7_R4,RFsampleDefaultR_tr$Default) 

p_RF_D7_R4<-predict(rfD7_R4,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D7_R4,RFsampleDefaultR_ts$Default)



#1.xgboost todas las variables###########################################################################################################
set.seed(123)
xgboostD_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "xgbTree",scale_pos_weight=5)
xgboostD_R1$finalModel

pdf("xgboostD_R1.pdf")
plot(xgboostD_R1)
dev.off()

pdf("varImp_XG_D_R1.pdf")
vip(xgboostD_R1,num_features=40) 
dev.off()

pt_XG_D_R1<-predict(xgboostD_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D_R1,RFsampleDefaultR_tr$Default)

p_XG_D_R1<-predict(xgboostD_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D_R1,RFsampleDefaultR_ts$Default)

#1.1xgboost controlCV5#############################################################################################################################
set.seed(123)
xgboostD1_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "xgbTree",trControl = controlCV5,scale_pos_weight=5)

pdf("xgboostD1_R1.pdf")
plot(xgboostD1_R1)
dev.off()

pdf("varImp_XG_D1_R1.pdf")
vip(xgboostD1_R1,num_features=40) 
dev.off()

pt_XG_D1_R1<-predict(xgboostD1_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D1_R1,RFsampleDefaultR_tr$Default)

p_XG_D1_R1<-predict(xgboostD1_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D1_R1,RFsampleDefaultR_ts$Default)

#1.2xgboost controlCV5,parametersGrid###############################################################################################################
set.seed(123)
xgboostD2_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlCV5,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD2_R1.pdf")
plot(xgboostD2_R1)
dev.off()

pdf("varImp_XG_D2_R1.pdf")
vip(xgboostD2_R1,num_features=40) 
dev.off()

pt_XG_D2_R1<-predict(xgboostD2_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D2_R1,RFsampleDefaultR_tr$Default)

p_XG_D2_R1<-predict(xgboostD2_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D2_R1,RFsampleDefaultR_ts$Default)

#1.3xgboost controlCV10,parametersGrid##################################################################################################################
set.seed(123)
xgboostD3_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlCV10,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD3_R1.pdf")
plot(xgboostD3_R1)
dev.off()

pdf("varImp_XG_D3_R1.pdf")
vip(xgboostD3_R1,num_features=40) 
dev.off()

pt_XG_D3_R1<-predict(xgboostD3_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D3_R1,RFsampleDefaultR_tr$Default)

p_XG_D3_R1<-predict(xgboostD3_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D3_R1,RFsampleDefaultR_ts$Default)


#1.4xgboost controlRCV,parametersGrid##################################################################################################################
set.seed(123)
xgboostD4_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlRCV,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD4_R1.pdf")
plot(xgboostD4_R1)
dev.off()

pdf("varImp_XG_D4_R1.pdf")
vip(xgboostD4_R1,num_features=40) 
dev.off()

pt_XG_D4_R1<-predict(xgboostD4_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D4_R1,RFsampleDefaultR_tr$Default)

p_XG_D4_R1<-predict(xgboostD4_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D4_R1,RFsampleDefaultR_ts$Default)

#1.5xgboost controlRCV_S,parametersGrid##################################################################################################################
set.seed(123)
xgboostD5_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlRCV_S,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD5_R1.pdf")
plot(xgboostD5_R1)
dev.off()

pdf("varImp_XG_D5_R1.pdf")
vip(xgboostD5_R1,num_features=40) 
dev.off()

pt_XG_D5_R1<-predict(xgboostD5_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D5_R1,RFsampleDefaultR_tr$Default)

p_XG_D5_R1<-predict(xgboostD5_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D5_R1,RFsampleDefaultR_ts$Default)

#1.6xgboost controlRCV_G,parametersGrid##################################################################################################################
set.seed(123)
xgboostD6_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlRCV_G,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD6_R1.pdf")
plot(xgboostD6_R1)
dev.off()

pdf("varImp_XG_D6_R1.pdf")
vip(xgboostD6_R1,num_features=40) 
dev.off()

pt_XG_D6_R1<-predict(xgboostD6_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D6_R1,RFsampleDefaultR_tr$Default)

p_XG_D6_R1<-predict(xgboostD6_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D6_R1,RFsampleDefaultR_ts$Default)

#1.7xgboost controlLOOCV,parametersGrid##################################################################################################################
set.seed(123)
xgboostD7_R1<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                     method = "xgbTree",trControl = controlLOOCV,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD7_R1.pdf")
plot(xgboostD7_R1)
dev.off()

pdf("varImp_XG_D7_R1.pdf")
vip(xgboostD7_R1,num_features=40) 
dev.off()

pt_XG_D7_R1<-predict(xgboostD7_R1,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D7_R1,RFsampleDefaultR_tr$Default)

p_XG_D7_R1<-predict(xgboostD7_R1,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D7_R1,RFsampleDefaultR_ts$Default)


#2.xgboost variables hipotesis###############################################################################################################
set.seed(123)
xgboostD_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "xgbTree",scale_pos_weight=5)

pdf("xgboostD_R2.pdf")
plot(xgboostD_R2)
dev.off()

pdf("varImp_XG_D_R2.pdf")
vip(xgboostD_R2,num_features=40) 
dev.off()

pt_XG_D_R2<-predict(xgboostD_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D_R2,RFsampleDefaultR_tr$Default) 

p_XG_D_R2<-predict(xgboostD_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D_R2,RFsampleDefaultR_ts$Default)

#2.1xgboost variables hipotesis,controlCV5###############################################################################################################
set.seed(123)
xgboostD1_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV5,scale_pos_weight=5)

pdf("xgboostD1_R2.pdf")
plot(xgboostD1_R2)
dev.off()

pdf("varImp_XG_D1_R2.pdf")
vip(xgboostD1_R2,num_features=40) 
dev.off()

pt_XG_D1_R2<-predict(xgboostD1_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D1_R2,RFsampleDefaultR_tr$Default) 

p_XG_D1_R2<-predict(xgboostD1_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D1_R2,RFsampleDefaultR_ts$Default)

#2.2xgboost variables hipotesis,controlCV5,parametersGrid###############################################################################################################
set.seed(123)
xgboostD2_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV5,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD2_R2.pdf")
plot(xgboostD2_R2)
dev.off()

pdf("varImp_XG_D2_R2.pdf")
vip(xgboostD2_R2,num_features=40) 
dev.off()

pt_XG_D2_R2<-predict(xgboostD2_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D2_R2,RFsampleDefaultR_tr$Default) 

p_XG_D2_R2<-predict(xgboostD2_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D2_R2,RFsampleDefaultR_ts$Default)


#2.3xgboost variables hipotesis,controlCV10,parametersGrid###############################################################################################################
set.seed(123)
xgboostD3_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlCV10,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD3_R2.pdf")
plot(xgboostD3_R2)
dev.off()

pdf("varImp_XG_D3_R2.pdf")
vip(xgboostD3_R2,num_features=40) 
dev.off()

pt_XG_D3_R2<-predict(xgboostD3_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D3_R2,RFsampleDefaultR_tr$Default) 

p_XG_D3_R2<-predict(xgboostD3_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D3_R2,RFsampleDefaultR_ts$Default)

#2.4xgboost variables hipotesis,controlRCV###############################################################################################################
set.seed(123)
xgboostD4_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy',scale_pos_weight=5)

pdf("xgboostD4_R2.pdf")
plot(xgboostD4_R2)
dev.off()

pdf("varImp_XG_D4_R2.pdf")
vip(xgboostD4_R2,num_features=40) 
dev.off()

pt_XG_D4_R2<-predict(xgboostD4_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D4_R2,RFsampleDefaultR_tr$Default) 

p_XG_D4_R2<-predict(xgboostD4_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D4_R2,RFsampleDefaultR_ts$Default)


#2.5xgboost variables hipotesis,controlRCV_S###############################################################################################################
set.seed(123)
xgboostD5_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV_S,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD5_R2.pdf")
plot(xgboostD5_R2)
dev.off()

pdf("varImp_XG_D5_R2.pdf")
vip(xgboostD5_R2,num_features=40) 
dev.off()

pt_XG_D5_R2<-predict(xgboostD5_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D5_R2,RFsampleDefaultR_tr$Default) 

p_XG_D5_R2<-predict(xgboostD5_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D5_R2,RFsampleDefaultR_ts$Default)

#2.6xgboost variables hipotesis,controlRCV_G###############################################################################################################
set.seed(123)
xgboostD6_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlRCV_G,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD6_R2.pdf")
plot(xgboostD6_R2)
dev.off()

pdf("varImp_XG_D6_R2.pdf")
vip(xgboostD6_R2,num_features=40) 
dev.off()

pt_XG_D6_R2<-predict(xgboostD6_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D6_R2,RFsampleDefaultR_tr$Default) 

p_XG_D6_R2<-predict(xgboostD6_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D6_R2,RFsampleDefaultR_ts$Default)

#2.7xgboost variables hipotesis,controlLOOCV###############################################################################################################
set.seed(123)
xgboostD7_R2<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "xgbTree",trControl = controlLOOCV,tuneGrid=parametersGrid,scale_pos_weight=5)

pdf("xgboostD7_R2.pdf")
plot(xgboostD7_R2)
dev.off()

pdf("varImp_XG_D7_R2.pdf")
vip(xgboostD7_R2,num_features=40) 
dev.off()

pt_XG_D7_R2<-predict(xgboostD7_R2,RFsampleDefaultR_tr)
confusionMatrix(pt_XG_D7_R2,RFsampleDefaultR_tr$Default) 

p_XG_D7_R2<-predict(xgboostD7_R2,RFsampleDefaultR_ts)
confusionMatrix(p_XG_D7_R2,RFsampleDefaultR_ts$Default)

#3.rf todas las variables###############################################################################################################
set.seed(123)
rfD_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "rf",sampsize=c(6,6), metric='Accuracy')
rfD_R3$finalModel

pdf("rfD_R3.pdf")
plot(rfD_R3)
dev.off()

pdf("varImp_RF_D_R3.pdf")
vip(rfD_R3,num_features=40) 
dev.off()

pt_RF_D_R3<-predict(rfD_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D_R3,RFsampleDefaultR_tr$Default)

p_RF_D_R3<-predict(rfD_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D_R3,RFsampleDefaultR_ts$Default)

#3.1rf controlCV5#############################################################################################################################
set.seed(123)
rfD1_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlCV5, metric='Accuracy',sampsize=c(6,6))

pdf("rfD1_R3.pdf")
plot(rfD1_R3)
dev.off()

pdf("varImp_RF_D1_R3.pdf")
vip(rfD1_R3,num_features=40) 
dev.off()

pt_RF_D1_R3<-predict(rfD1_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D1_R3,RFsampleDefaultR_tr$Default)

p_RF_D1_R3<-predict(rfD1_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D1_R3,RFsampleDefaultR_ts$Default)

#3.2rf controlCV5,tunegrid###############################################################################################################
set.seed(123)
rfD2_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlCV5,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD2_R3.pdf")
plot(rfD2_R3)
dev.off()

pdf("varImp_RF_D2_R3.pdf")
vip(rfD2_R3,num_features=40) 
dev.off()

pt_RF_D2_R3<-predict(rfD2_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D2_R3,RFsampleDefaultR_tr$Default)

p_RF_D2_R3<-predict(rfD2_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D2_R3,RFsampleDefaultR_ts$Default)

#3.3rf controlCV10,tunegrid##################################################################################################################
set.seed(123)
rfD3_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlCV10,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD3_R3.pdf")
plot(rfD3_R3)
dev.off()

pdf("varImp_RF_D3_R3.pdf")
vip(rfD3_R3,num_features=40) 
dev.off()

pt_RF_D3_R3<-predict(rfD3_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D3_R3,RFsampleDefaultR_tr$Default)

p_RF_D3_R3<-predict(rfD3_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D3_R3,RFsampleDefaultR_ts$Default)


#3.4rf controlRCV,tunegrid##################################################################################################################
set.seed(123)
rfD4_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlRCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(5,5))

pdf("rfD4_R3.pdf")
plot(rfD4_R3)
dev.off()

pdf("varImp_RF_D4_R3.pdf")
vip(rfD4_R3,num_features=40) 
dev.off()

pt_RF_D4_R3<-predict(rfD4_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D4_R3,RFsampleDefaultR_tr$Default)

p_RF_D4_R3<-predict(rfD4_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D4_R3,RFsampleDefaultR_ts$Default)

#3.5rf controlRCV_S,tunegrid##################################################################################################################
set.seed(123)
rfD5_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlRCV_S,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD5_R3.pdf")
plot(rfD5_R3)
dev.off()

pdf("varImp_RF_D5_R3.pdf")
vip(rfD5_R3,num_features=40) 
dev.off()

pt_RF_D5_R3<-predict(rfD5_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D5_R3,RFsampleDefaultR_tr$Default)

p_RF_D5_R3<-predict(rfD5_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D5_R3,RFsampleDefaultR_ts$Default)

#3.6rf controlRCV_G,tunegrid##################################################################################################################
set.seed(123)
rfD6_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlRCV_G,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD6_R3.pdf")
plot(rfD6_R3)
dev.off()

pdf("varImp_RF_D6_R3.pdf")
vip(rfD6_R3,num_features=40) 
dev.off()

pt_RF_D6_R3<-predict(rfD6_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D6_R3,RFsampleDefaultR_tr$Default)

p_RF_D6_R3<-predict(rfD6_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D6_R3,RFsampleDefaultR_ts$Default)

#3.7rf controlLOOCV,tunegrid##################################################################################################################
set.seed(123)
rfD7_R3<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],
                method = "rf",trControl = controlLOOCV,tuneGrid=tunegrid, metric='Accuracy',sampsize=c(6,6))

pdf("rfD7_R3.pdf")
plot(rfD7_R3)
dev.off()

pdf("varImp_RF_D7_R3.pdf")
vip(rfD7_R3,num_features=40) 
dev.off()

pt_RF_D7_R3<-predict(rfD7_R3,RFsampleDefaultR_tr)
confusionMatrix(pt_RF_D7_R3,RFsampleDefaultR_tr$Default)

p_RF_D7_R3<-predict(rfD7_R3,RFsampleDefaultR_ts)
confusionMatrix(p_RF_D7_R3,RFsampleDefaultR_ts$Default)



#5.ranger todas las variables###############################################################################################################
set.seed(1234)
rangerD_R5<- train(Default~., data = RFsampleDefaultR_tr,method = "ranger", metric='Accuracy')
rangerD_R5$finalModel

pdf("rangerD_R5.pdf")
plot(rangerD_R5)
dev.off()

pt_RANGER_D_R5<-predict(rangerD_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D_R5<-predict(rangerD_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D_R5,RFsampleDefaultR_ts$Default)

#5.1ranger controlCV5#############################################################################################################################
set.seed(123)
rangerD1_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlCV5, metric='Accuracy')

pdf("rangerD1_R5.pdf")
plot(rangerD1_R5)
dev.off()

pdf("varImp_RANGER_D1_R5.pdf")
vip(rangerD1_R5,num_features=40) 
dev.off()

pt_RANGER_D1_R5<-predict(rangerD1_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D1_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D1_R5<-predict(rangerD1_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D1_R5,RFsampleDefaultR_ts$Default)

#5.2ranger controlCV5,parametersGrid###############################################################################################################
set.seed(123)
rangerD2_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD2_R5.pdf")
plot(rangerD2_R5)
dev.off()

pdf("varImp_RANGER_D2_R5.pdf")
vip(rangerD2_R5,num_features=40) 
dev.off()

pt_RANGER_D2_R5<-predict(rangerD2_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D2_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D2_R5<-predict(rangerD2_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D2_R5,RFsampleDefaultR_ts$Default)

#5.3ranger controlCV10,parametersGrid##################################################################################################################
set.seed(123)
rangerD3_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlCV10,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD3_R5.pdf")
plot(rangerD3_R5)
dev.off()

pdf("varImp_RANGER_D3_R5.pdf")
vip(rangerD3_R5,num_features=40) 
dev.off()

pt_RANGER_D3_R5<-predict(rangerD3_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D3_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D3_R5<-predict(rangerD3_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D3_R5,RFsampleDefaultR_ts$Default)


#5.4ranger controlRCV,parametersGrid##################################################################################################################
set.seed(123)
rangerD4_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD4_R5.pdf")
plot(rangerD4_R5)
dev.off()

pdf("varImp_RANGER_D4_R5.pdf")
vip(rangerD4_R5,num_features=40) 
dev.off()

pt_RANGER_D4_R5<-predict(rangerD4_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D4_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D4_R5<-predict(rangerD4_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D4_R5,RFsampleDefaultR_ts$Default)

#5.5ranger controlRCV_S,parametersGrid##################################################################################################################
set.seed(123)
rangerD5_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD5_R5.pdf")
plot(rangerD5_R5)
dev.off()

pdf("varImp_RANGER_D5_R5.pdf")
vip(rangerD5_R5,num_features=40) 
dev.off()

pt_RANGER_D5_R5<-predict(rangerD5_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D5_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D5_R5<-predict(rangerD5_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D5_R5,RFsampleDefaultR_ts$Default)

#5.6ranger controlRCV_G,parametersGrid##################################################################################################################
set.seed(123)
rangerD6_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD6_R5.pdf")
plot(rangerD6_R5)
dev.off()

pdf("varImp_RANGER_D6_R5.pdf")
vip(rangerD6_R5,num_features=40) 
dev.off()

pt_RANGER_D6_R5<-predict(rangerD6_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D6_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D6_R5<-predict(rangerD6_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D6_R5,RFsampleDefaultR_ts$Default)

#5.7ranger controlLOOCV,parametersGrid##################################################################################################################
set.seed(123)
rangerD7_R5<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "ranger",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD7_R5.pdf")
plot(rangerD7_R5)
dev.off()

pdf("varImp_RANGER_D7_R5.pdf")
vip(rangerD7_R5,num_features=40) 
dev.off()

pt_RANGER_D7_R5<-predict(rangerD7_R5,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D7_R5,RFsampleDefaultR_tr$Default)

p_RANGER_D7_R5<-predict(rangerD7_R5,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D7_R5,RFsampleDefaultR_ts$Default)



#6.ranger variables hipotesis###############################################################################################################
set.seed(123)
rangerD_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "ranger", metric='Accuracy')

pdf("rangerD_R6.pdf")
plot(rangerD_R6)
dev.off()

pdf("varImp_RANGER_D_R6.pdf")
vip(rangerD_R6,num_features=40) 
dev.off()

pt_RANGER_D_R6<-predict(rangerD_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D_R6<-predict(rangerD_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D_R6,RFsampleDefaultR_ts$Default)

#6.1ranger variables hipotesis, controlCV5###############################################################################################################
set.seed(123)
rangerD1_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "ranger",trControl = controlCV5, metric='Accuracy')

pdf("rangerD1_R6.pdf")
plot(rangerD1_R6)
dev.off()

pdf("varImp_RANGER_D1_R6.pdf")
vip(rangerD1_R6,num_features=40) 
dev.off()

pt_RANGER_D1_R6<-predict(rangerD1_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D1_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D1_R6<-predict(rangerD1_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D1_R6,RFsampleDefaultR_ts$Default)

#6.2ranger variables hipotesis, controlCV5,parametersGrid###############################################################################################################
set.seed(123)
rangerD2_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD2_R6.pdf")
plot(rangerD2_R6)
dev.off()

pdf("varImp_RANGER_D2_R6.pdf")
vip(rangerD2_R6,num_features=40) 
dev.off()

pt_RANGER_D2_R6<-predict(rangerD2_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D2_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D2_R6<-predict(rangerD2_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D2_R6,RFsampleDefaultR_ts$Default)


#6.3ranger variables hipotesis,controlCV10,parametersGrid###############################################################################################################
set.seed(123)
rangerD3_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlCV10,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD3_R6.pdf")
plot(rangerD3_R6)
dev.off()

pdf("varImp_RANGER_D3_R6.pdf")
vip(rangerD3_R6,num_features=40) 
dev.off()

pt_RANGER_D3_R6<-predict(rangerD3_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D3_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D3_R6<-predict(rangerD3_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D3_R6,RFsampleDefaultR_ts$Default)

#6.4ranger variables hipotesis, controlRCV###############################################################################################################
set.seed(123)
rangerD4_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD4_R6.pdf")
plot(rangerD4_R6)
dev.off()

pdf("varImp_RANGER_D4_R6.pdf")
vip(rangerD4_R6,num_features=40) 
dev.off()

pt_RANGER_D4_R6<-predict(rangerD4_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D4_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D4_R6<-predict(rangerD4_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D4_R6,RFsampleDefaultR_ts$Default)


#6.5ranger variables hipotesis, controlRCV_S###############################################################################################################
set.seed(123)
rangerD5_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD5_R6.pdf")
plot(rangerD5_R6)
dev.off()

pdf("varImp_RANGER_D5_R6.pdf")
vip(rangerD5_R6,num_features=40) 
dev.off()

pt_RANGER_D5_R6<-predict(rangerD5_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D5_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D5_R6<-predict(rangerD5_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D5_R6,RFsampleDefaultR_ts$Default)

#6.6ranger variables hipotesis, controlRCV_G###############################################################################################################
set.seed(123)
rangerD6_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD6_R6.pdf")
plot(rangerD6_R6)
dev.off()

pdf("varImp_RANGER_D6_R6.pdf")
vip(rangerD6_R6,num_features=40) 
dev.off()

pt_RANGER_D6_R6<-predict(rangerD6_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D6_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D6_R6<-predict(rangerD6_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D6_R6,RFsampleDefaultR_ts$Default)

#6.7ranger variables hipotesis, controlLOOCV###############################################################################################################
set.seed(123)
rangerD7_R6<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                    method = "ranger",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("rangerD7_R6.pdf")
plot(rangerD7_R6)
dev.off()

pdf("varImp_RANGER_D7_R6.pdf")
vip(rangerD7_R6,num_features=40) 
dev.off()

pt_RANGER_D7_R6<-predict(rangerD7_R6,RFsampleDefaultR_tr)
confusionMatrix(pt_RANGER_D7_R6,RFsampleDefaultR_tr$Default) 

p_RANGER_D7_R6<-predict(rangerD7_R6,RFsampleDefaultR_ts)
confusionMatrix(p_RANGER_D7_R6,RFsampleDefaultR_ts$Default) 

#7.cforest todas las variables###############################################################################################################
set.seed(123)
cforestD_R7<- train(Default~., data = RFsampleDefaultR_tr,method = "cforest", metric='Accuracy')
cforestD_R7$finalModel

pdf("cforestD_R7.pdf")
plot(cforestD_R7)
dev.off()

pdf("varImp_CFOREST_D_R7.pdf")
vip(cforestD_R7,num_features=100) 
dev.off()

pt_CFOREST_D_R7<-predict(cforestD_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D_R7<-predict(cforestD_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D_R7,RFsampleDefaultR_ts$Default)

#7.1cforest controlCV5#############################################################################################################################
set.seed(123)
cforestD1_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlCV5, metric='Accuracy')

pdf("cforestD1_R7.pdf")
plot(cforestD1_R7)
dev.off()

pdf("varImp_CFOREST_D1_R7.pdf")
vip(cforestD1_R7,num_features=40) 
dev.off()

pt_CFOREST_D1_R7<-predict(cforestD1_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D1_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D1_R7<-predict(cforestD1_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D1_R7,RFsampleDefaultR_ts$Default)

#7.2cforest controlCV5,parametersGrid###############################################################################################################
set.seed(123)
cforestD2_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD2_R7.pdf")
plot(cforestD2_R7)
dev.off()

pdf("varImp_CFOREST_D2_R7.pdf")
vip(cforestD2_R7,num_features=40) 
dev.off()

pt_CFOREST_D2_R7<-predict(cforestD2_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D2_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D2_R7<-predict(cforestD2_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D2_R7,RFsampleDefaultR_ts$Default)

#7.3cforest controlCV10,parametersGrid##################################################################################################################
set.seed(123)
cforestD3_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlCV10,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD3_R7.pdf")
plot(cforestD3_R7)
dev.off()

pdf("varImp_CFOREST_D3_R7.pdf")
vip(cforestD3_R7,num_features=40) 
dev.off()

pt_CFOREST_D3_R7<-predict(cforestD3_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D3_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D3_R7<-predict(cforestD3_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D3_R7,RFsampleDefaultR_ts$Default)


#7.4cforest controlRCV,parametersGrid##################################################################################################################
set.seed(123)
cforestD4_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD4_R7.pdf")
plot(cforestD4_R7)
dev.off()

pdf("varImp_CFOREST_D4_R7.pdf")
vip(cforestD4_R7,num_features=40) 
dev.off()

pt_CFOREST_D4_R7<-predict(cforestD4_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D4_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D4_R7<-predict(cforestD4_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D4_R7,RFsampleDefaultR_ts$Default)

#7.5cforest controlRCV_S,parametersGrid##################################################################################################################
set.seed(123)
cforestD5_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD5_R7.pdf")
plot(cforestD5_R7)
dev.off()

pdf("varImp_CFOREST_D5_R7.pdf")
vip(cforestD5_R7,num_features=40) 
dev.off()

pt_CFOREST_D5_R7<-predict(cforestD5_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D5_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D5_R7<-predict(cforestD5_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D5_R7,RFsampleDefaultR_ts$Default)

#7.6cforest controlRCV_G,parametersGrid##################################################################################################################
set.seed(123)
cforestD6_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD6_R7.pdf")
plot(cforestD6_R7)
dev.off()

pdf("varImp_CFOREST_D6_R7.pdf")
vip(cforestD6_R7,num_features=40) 
dev.off()

pt_CFOREST_D6_R7<-predict(cforestD6_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D6_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D6_R7<-predict(cforestD6_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D6_R7,RFsampleDefaultR_ts$Default)

#7.7cforest controlLOOCV,parametersGrid##################################################################################################################
set.seed(123)
cforestD7_R7<- train(Default~., data = RFsampleDefaultR_tr[-c(63,64)],method = "cforest",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD7_R7.pdf")
plot(cforestD7_R7)
dev.off()

pdf("varImp_CFOREST_D7_R7.pdf")
vip(cforestD7_R7,num_features=40) 
dev.off()

pt_CFOREST_D7_R7<-predict(cforestD7_R7,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D7_R7,RFsampleDefaultR_tr$Default)

p_CFOREST_D7_R7<-predict(cforestD7_R7,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D7_R7,RFsampleDefaultR_ts$Default)


#8.cforest variables hipotesis###############################################################################################################
set.seed(123)
cforestD_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "cforest", metric='Accuracy')

pdf("cforestD_R8.pdf")
plot(cforestD_R8)
dev.off()

pdf("varImp_CFOREST_D_R8.pdf")
vip(cforestD_R8,num_features=40) 
dev.off()

pt_CFOREST_D_R8<-predict(cforestD_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D_R8<-predict(cforestD_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D_R8,RFsampleDefaultR_ts$Default)

#8.1cforest variables hipotesis, controlCV5###############################################################################################################
set.seed(123)
cforestD1_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],method = "cforest",trControl = controlCV5, metric='Accuracy')

pdf("cforestD1_R8.pdf")
plot(cforestD1_R8)
dev.off()

pdf("varImp_CFOREST_D1_R8.pdf")
vip(cforestD1_R8,num_features=40) 
dev.off()

pt_CFOREST_D1_R8<-predict(cforestD1_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D1_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D1_R8<-predict(cforestD1_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D1_R8,RFsampleDefaultR_ts$Default)

#8.2cforest variables hipotesis, controlCV5,parametersGrid###############################################################################################################
set.seed(123)
cforestD2_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlCV5,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD2_R8.pdf")
plot(cforestD2_R8)
dev.off()

pdf("varImp_CFOREST_D2_R8.pdf")
vip(cforestD2_R8,num_features=40) 
dev.off()

pt_CFOREST_D2_R8<-predict(cforestD2_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D2_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D2_R8<-predict(cforestD2_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D2_R8,RFsampleDefaultR_ts$Default)


#8.3cforest variables hipotesis,controlCV10,parametersGrid###############################################################################################################
set.seed(123)
cforestD3_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlCV10,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD3_R8.pdf")
plot(cforestD3_R8)
dev.off()

pdf("varImp_CFOREST_D3_R8.pdf")
vip(cforestD3_R8,num_features=40) 
dev.off()

pt_CFOREST_D3_R8<-predict(cforestD3_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D3_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D3_R8<-predict(cforestD3_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D3_R8,RFsampleDefaultR_ts$Default)

#8.4cforest variables hipotesis, controlRCV###############################################################################################################
set.seed(123)
cforestD4_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD4_R8.pdf")
plot(cforestD4_R8)
dev.off()

pdf("varImp_CFOREST_D4_R8.pdf")
vip(cforestD4_R8,num_features=40) 
dev.off()

pt_CFOREST_D4_R8<-predict(cforestD4_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D4_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D4_R8<-predict(cforestD4_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D4_R8,RFsampleDefaultR_ts$Default)


#8.5cforest variables hipotesis, controlRCV_S###############################################################################################################
set.seed(123)
cforestD5_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV_S,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD5_R8.pdf")
plot(cforestD5_R8)
dev.off()

pdf("varImp_CFOREST_D5_R8.pdf")
vip(cforestD5_R8,num_features=40) 
dev.off()

pt_CFOREST_D5_R8<-predict(cforestD5_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D5_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D5_R8<-predict(cforestD5_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D5_R8,RFsampleDefaultR_ts$Default)

#8.6cforest variables hipotesis, controlRCV_G###############################################################################################################
set.seed(123)
cforestD6_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlRCV_G,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD6_R8.pdf")
plot(cforestD6_R8)
dev.off()

pdf("varImp_CFOREST_D6_R8.pdf")
vip(cforestD6_R8,num_features=40) 
dev.off()

pt_CFOREST_D6_R8<-predict(cforestD6_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D6_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D6_R8<-predict(cforestD6_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D6_R8,RFsampleDefaultR_ts$Default)

#8.7cforest variables hipotesis, controlLOOCV###############################################################################################################
set.seed(123)
cforestD7_R8<- train(Default~.,data = RFsampleDefaultR_tr[c(1,3:7,10:16,18:23,25:36,38,40,41,43:50,53,55:57,59:62)],
                     method = "cforest",trControl = controlLOOCV,tuneGrid=parametersGrid, metric='Accuracy')

pdf("cforestD7_R8.pdf")
plot(cforestD7_R8)
dev.off()

pdf("varImp_CFOREST_D7_R8.pdf")
vip(cforestD7_R8,num_features=40) 
dev.off()

pt_CFOREST_D7_R8<-predict(cforestD7_R8,RFsampleDefaultR_tr)
confusionMatrix(pt_CFOREST_D7_R8,RFsampleDefaultR_tr$Default) 

p_CFOREST_D7_R8<-predict(cforestD7_R8,RFsampleDefaultR_ts)
confusionMatrix(p_CFOREST_D7_R8,RFsampleDefaultR_ts$Default) 


##RF para Riesgo VARIABLES ORIGINALES#####################################################################################################
#1.rf todas las variables###########################################################################################################################
modRF_R <- train(Riesgo ~ ., data=RFsampleRiskO_tr[-c(63,64)], method="rf")
modRF_R$finalModel
summary(modRF_R)
modRF_R$bestTune #best accuaracy found with the mtry
modRF_R$results #table with each mtry and accuaracy 

pdf("mtry_R_O1.pdf")
plot(modRF_R) #mtry vs accuaracy
dev.off()

pdf("varImp_R_O1.pdf")
vip(modRF_R,num_features =40)
dev.off()

prob_trainR<- predict(modRF_R,RFsampleRiskO_tr,type = "raw")
confusionMatrix(prob_trainR,RFsampleRiskO_tr$Riesgo)

prob_testR<- predict(modRF_R,RFsampleRiskO_ts,type = "raw")
confusionMatrix(prob_testR,RFsampleRiskO_ts$Riesgo)

#1.1rf todas las variables,controlCV############################################################################################################################
set.seed(123)
modRF_R1_O1<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)],
                    method="rf", trControl=controlCV, tuneGrid = gridSE,metric='Accuracy',sampsize=c(2,2,2)) 
summary(modRF_R1_O1)
modRF_R1_O1$bestTune #best accuaracy found with the mtry
modRF_R1_O1$results #table with each mtry and accuaracy 
modRF_R1_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_O1.pdf")
plot(modRF_R1_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_O1.pdf")
vip(modRF_R1_O1,num_features =40)
dev.off()

pt_modRF_R1_O1<- predict(modRF_R1_O1,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R1_O1,RFsampleRiskO_tr$Riesgo)

p_modRF_R1_O1<- predict(modRF_R1_O1,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R1_O1,RFsampleRiskO_ts$Riesgo)

#1.2rf todas las variables,controlRCV############################################################################################################################
set.seed(123)
modRF_R2_O1<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)],
                    method="rf", trControl=controlRCV, tuneGrid = tunegrid,metric='Accuracy',sampsize=c(2,2,2)) 
summary(modRF_R2_O1)
modRF_R2_O1$bestTune #best accuaracy found with the mtry
modRF_R2_O1$results #table with each mtry and accuaracy 
modRF_R2_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_O1.pdf")
plot(modRF_R2_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_O1.pdf")
vip(modRF_R2_O1,num_features =40)
dev.off()

pt_modRF_R2_O1<- predict(modRF_R2_O1,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R2_O1,RFsampleRiskO_tr$Riesgo)

p_modRF_R2_O1<- predict(modRF_R2_O1,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R2_O1,RFsampleRiskO_ts$Riesgo)

#1.3rf todas las variables,controlRCV_S############################################################################################################################
set.seed(123)
modRF_R3_O1<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)],
                    method="rf", trControl=controlRCV_S, tuneGrid = gridSE,metric='Accuracy',sampsize=c(2,2,2)) 
summary(modRF_R3_O1)
modRF_R3_O1$bestTune #best accuaracy found with the mtry
modRF_R3_O1$results #table with each mtry and accuaracy 
modRF_R3_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_O1.pdf")
plot(modRF_R3_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_O1.pdf")
vip(modRF_R3_O1,num_features =40)
dev.off()

pt_modRF_R3_O1<- predict(modRF_R3_O1,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R3_O1,RFsampleRiskO_tr$Riesgo)

p_modRF_R3_O1<- predict(modRF_R3_O1,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R3_O1,RFsampleRiskO_ts$Riesgo)

#1.4rf todas las variables,controlRCV_G############################################################################################################################
set.seed(123)
modRF_R4_O1<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="rf", trControl=controlRCV_G, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_R4_O1)
modRF_R4_O1$bestTune #best accuaracy found with the mtry
modRF_R4_O1$results #table with each mtry and accuaracy 
modRF_R4_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_O1.pdf")
plot(modRF_R4_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_O1.pdf")
vip(modRF_R4_O1,num_features =40)
dev.off()

pt_modRF_R4_O1<- predict(modRF_R4_O1,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R4_O1,RFsampleRiskO_tr$Riesgo)

p_modRF_R4_O1<- predict(modRF_R4_O1,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R4_O1,RFsampleRiskO_ts$Riesgo)

#1.5rf todas las variables,controlLOOCV############################################################################################################################
set.seed(123)
modRF_R5_O1<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="rf", trControl=controlLOOCV, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_R5_O1)
modRF_R5_O1$bestTune #best accuaracy found with the mtry
modRF_R5_O1$results #table with each mtry and accuaracy 
modRF_R5_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_O1.pdf")
plot(modRF_R5_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_O1.pdf")
vip(modRF_R5_O1,num_features =40)
dev.off()

pt_modRF_R5_O1<- predict(modRF_R5_O1,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R5_O1,RFsampleRiskO_tr$Riesgo)

p_modRF_R5_O1<- predict(modRF_R5_O1,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R5_O1,RFsampleRiskO_ts$Riesgo)

#2.ranger todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_O2.pdf")
plot(modRF_R_O2)
dev.off()
modRF_R_O2$finalModel

pdf("varImp_R_O2.pdf")
vip(modRF_R_O2,num_features =40)
dev.off()

pt_modRF_R_O2<- predict(modRF_R_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R_O2,RFsampleRiskO_tr$Riesgo)

p_modRF_R_O2<- predict(modRF_R_O2,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R_O2,RFsampleRiskO_ts$Riesgo)

#2.1ranger todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_O2)
modRF_R1_O2$bestTune #best accuaracy found with the mtry
modRF_R1_O2$results #table with each mtry and accuaracy 
modRF_R1_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_O2.pdf")
plot(modRF_R1_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_O2.pdf")
vip(modRF_R1_O2,num_features =40)
dev.off()

pt_modRF_R1_O2<- predict(modRF_R1_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R1_O2,RFsampleRiskO_tr$Riesgo)

p_modRF_R1_O2<- predict(modRF_R1_O2,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R1_O2,RFsampleRiskO_ts$Riesgo)

#2.2ranger todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_O2)
modRF_R2_O2$bestTune #best accuaracy found with the mtry
modRF_R2_O2$results #table with each mtry and accuaracy 
modRF_R2_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_O2.pdf")
plot(modRF_R2_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_O2.pdf")
vip(modRF_R2_O2,num_features =40)
dev.off()

pt_modRF_R2_O2<- predict(modRF_R2_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R2_O2,RFsampleRiskO_tr$Riesgo)

p_modRF_R2_O2<- predict(modRF_R2_O2,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R2_O2,RFsampleRiskO_ts$Riesgo)

#2.3ranger todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_O2)
modRF_R3_O2$bestTune #best accuaracy found with the mtry
modRF_R3_O2$results #table with each mtry and accuaracy 
modRF_R3_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_O2.pdf")
plot(modRF_R3_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_O2.pdf")
vip(modRF_R3_O2,num_features =40)
dev.off()

pt_modRF_R3_O2<- predict(modRF_R3_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R3_O2,RFsampleRiskO_ts$Riesgo)

p_modRF_R3_O2<- predict(modRF_R3_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(p_modRF_R3_O2,RFsampleRiskO_ts$Riesgo)

#2.4ranger todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_O2)
modRF_R4_O2$bestTune #best accuaracy found with the mtry
modRF_R4_O2$results #table with each mtry and accuaracy 
modRF_R4_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_O2.pdf")
plot(modRF_R4_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_O2.pdf")
vip(modRF_R4_O2,num_features =40)
dev.off()

pt_modRF_R4_O2<- predict(modRF_R4_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R4_O2,RFsampleRiskO_tr$Riesgo)

p_modRF_R4_O2<- predict(modRF_R4_O2,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R4_O2,RFsampleRiskO_ts$Riesgo)

#2.5ranger todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_O2<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="ranger", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_O2)
modRF_R5_O2$bestTune #best accuaracy found with the mtry
modRF_R5_O2$results #table with each mtry and accuaracy 
modRF_R5_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_O2.pdf")
plot(modRF_R5_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_O2.pdf")
vip(modRF_R5_O2,num_features =40)
dev.off()

pt_modRF_R5_O2<- predict(modRF_R5_O2,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R5_O2,RFsampleRiskO_tr$Riesgo)

p_modRF_R5_O2<- predict(modRF_R5_O2,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R5_O2,RFsampleRiskO_ts$Riesgo)

#3.cforest todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_O3.pdf")
plot(modRF_R_O3)
dev.off()
modRF_R_O3$finalModel

pdf("varImp_R_O3.pdf")
vip(modRF_R_O3,num_features =40)
dev.off()

pt_modRF_R_O3<- predict(modRF_R_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R_O3,RFsampleRiskO_tr$Riesgo)

p_modRF_R_O3<- predict(modRF_R_O3,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R_O3,RFsampleRiskO_ts$Riesgo)

#3.1cforest todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_O3)
modRF_R1_O3$bestTune #best accuaracy found with the mtry
modRF_R1_O3$results #table with each mtry and accuaracy 
modRF_R1_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_O3.pdf")
plot(modRF_R1_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_O3.pdf")
vip(modRF_R1_O3,num_features =40)
dev.off()

pt_modRF_R1_O3<- predict(modRF_R1_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R1_O3,RFsampleRiskO_tr$Riesgo)

p_modRF_R1_O3<- predict(modRF_R1_O3,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R1_O3,RFsampleRiskO_ts$Riesgo)

#3.2cforest todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_O3)
modRF_R2_O3$bestTune #best accuaracy found with the mtry
modRF_R2_O3$results #table with each mtry and accuaracy 
modRF_R2_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_O3.pdf")
plot(modRF_R2_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_O3.pdf")
vip(modRF_R2_O3,num_features =40)
dev.off()

pt_modRF_R2_O3<- predict(modRF_R2_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R2_O3,RFsampleRiskO_tr$Riesgo)

p_modRF_R2_O3<- predict(modRF_R2_O3,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R2_O3,RFsampleRiskO_ts$Riesgo)

#3.3cforest todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_O3)
modRF_R3_O3$bestTune #best accuaracy found with the mtry
modRF_R3_O3$results #table with each mtry and accuaracy 
modRF_R3_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_O3.pdf")
plot(modRF_R3_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_O3.pdf")
vip(modRF_R3_O3,num_features =40)
dev.off()

pt_modRF_R3_O3<- predict(modRF_R3_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R3_O3,RFsampleRiskO_ts$Riesgo)

p_modRF_R3_O3<- predict(modRF_R3_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(p_modRF_R3_O3,RFsampleRiskO_ts$Riesgo)

#3.4cforest todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_O3)
modRF_R4_O3$bestTune #best accuaracy found with the mtry
modRF_R4_O3$results #table with each mtry and accuaracy 
modRF_R4_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_O3.pdf")
plot(modRF_R4_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_O3.pdf")
vip(modRF_R4_O3,num_features =40)
dev.off()

pt_modRF_R4_O3<- predict(modRF_R4_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R4_O3,RFsampleRiskO_tr$Riesgo)

p_modRF_R4_O3<- predict(modRF_R4_O3,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R4_O3,RFsampleRiskO_ts$Riesgo)

#3.5cforest todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_O3<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="cforest", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_O3)
modRF_R5_O3$bestTune #best accuaracy found with the mtry
modRF_R5_O3$results #table with each mtry and accuaracy 
modRF_R5_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_O3.pdf")
plot(modRF_R5_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_O3.pdf")
vip(modRF_R5_O3,num_features =40)
dev.off()

pt_modRF_R5_O3<- predict(modRF_R5_O3,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R5_O3,RFsampleRiskO_tr$Riesgo)

p_modRF_R5_O3<- predict(modRF_R5_O3,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R5_O3,RFsampleRiskO_ts$Riesgo)

#4.xgbTree todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_O4.pdf")
plot(modRF_R_O4)
dev.off()
modRF_R_O4$finalModel

pdf("varImp_R_O4.pdf")
vip(modRF_R_O4,num_features =40)
dev.off()

pt_modRF_R_O4<- predict(modRF_R_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R_O4<- predict(modRF_R_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R_O4,RFsampleRiskO_ts$Riesgo)

#4.1xgbTree todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_O4)
modRF_R1_O4$bestTune #best accuaracy found with the mtry
modRF_R1_O4$results #table with each mtry and accuaracy 
modRF_R1_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_O4.pdf")
plot(modRF_R1_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_O4.pdf")
vip(modRF_R1_O4,num_features =40)
dev.off()

pt_modRF_R1_O4<- predict(modRF_R1_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R1_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R1_O4<- predict(modRF_R1_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R1_O4,RFsampleRiskO_ts$Riesgo)

#4.2xgbTree todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_O4)
modRF_R2_O4$bestTune #best accuaracy found with the mtry
modRF_R2_O4$results #table with each mtry and accuaracy 
modRF_R2_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_O4.pdf")
plot(modRF_R2_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_O4.pdf")
vip(modRF_R2_O4,num_features =40)
dev.off()

pt_modRF_R2_O4<- predict(modRF_R2_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R2_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R2_O4<- predict(modRF_R2_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R2_O4,RFsampleRiskO_ts$Riesgo)

#4.3xgbTree todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_O4)
modRF_R3_O4$bestTune #best accuaracy found with the mtry
modRF_R3_O4$results #table with each mtry and accuaracy 
modRF_R3_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_O4.pdf")
plot(modRF_R3_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_O4.pdf")
vip(modRF_R3_O4,num_features =40)
dev.off()

pt_modRF_R3_O4<- predict(modRF_R3_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R3_O4,RFsampleRiskO_ts$Riesgo)

p_modRF_R3_O4<- predict(modRF_R3_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(p_modRF_R3_O4,RFsampleRiskO_ts$Riesgo)

#4.4xgbTree todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_O4)
modRF_R4_O4$bestTune #best accuaracy found with the mtry
modRF_R4_O4$results #table with each mtry and accuaracy 
modRF_R4_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_O4.pdf")
plot(modRF_R4_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_O4.pdf")
vip(modRF_R4_O4,num_features =40)
dev.off()

pt_modRF_R4_O4<- predict(modRF_R4_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R4_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R4_O4<- predict(modRF_R4_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R4_O4,RFsampleRiskO_ts$Riesgo)

#4.5xgbTree todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_O4)
modRF_R5_O4$bestTune #best accuaracy found with the mtry
modRF_R5_O4$results #table with each mtry and accuaracy 
modRF_R5_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_O4.pdf")
plot(modRF_R5_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_O4.pdf")
vip(modRF_R5_O4,num_features =40)
dev.off()

pt_modRF_R5_O4<- predict(modRF_R5_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R5_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R5_O4<- predict(modRF_R5_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R5_O4,RFsampleRiskO_ts$Riesgo)

#4.6xgbTree todas las variables,controlCV5#########################################################################################################################
set.seed(123)
modRF_R6_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV5, metric='Accuracy') 
summary(modRF_R6_O4)
modRF_R6_O4$bestTune #best accuaracy found with the mtry
modRF_R6_O4$results #table with each mtry and accuaracy 
modRF_R6_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R6_O4.pdf")
plot(modRF_R6_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R6_O4.pdf")
vip(modRF_R6_O4,num_features =40)
dev.off()

pt_modRF_R6_O4<- predict(modRF_R6_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R6_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R6_O4<- predict(modRF_R6_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R6_O4,RFsampleRiskO_ts$Riesgo)



#4.7xgbTree todas las variables,controlCV10#########################################################################################################################
set.seed(123)
modRF_R7_O4<- train(Riesgo ~ ., data=RFsampleRiskO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV10, metric='Accuracy') 
summary(modRF_R7_O4)
modRF_R7_O4$bestTune #best accuaracy found with the mtry
modRF_R7_O4$results #table with each mtry and accuaracy 
modRF_R7_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R7_O4.pdf")
plot(modRF_R7_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_R7_O4.pdf")
vip(modRF_R7_O4,num_features =40)
dev.off()

pt_modRF_R7_O4<- predict(modRF_R7_O4,RFsampleRiskO_tr,type = "raw")
confusionMatrix(pt_modRF_R7_O4,RFsampleRiskO_tr$Riesgo)

p_modRF_R7_O4<- predict(modRF_R7_O4,RFsampleRiskO_ts,type = "raw")
confusionMatrix(p_modRF_R7_O4,RFsampleRiskO_ts$Riesgo)


##RF para Riesgo VARIABLES CONSTRUIDAS (RANGOS)############################################################################################
#1.1rf todas las variables,controlCV############################################################################################################################
set.seed(123)
modRF_R1_R1<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], 
                    method="rf", trControl=controlCV, tuneGrid = gridSE,metric='Accuracy',sampsize=c(2,2,2)) 
summary(modRF_R1_R1)
modRF_R1_R1$bestTune #best accuaracy found with the mtry
modRF_R1_R1$results #table with each mtry and accuaracy 
modRF_R1_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_R1.pdf")
plot(modRF_R1_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_R1.pdf")
vip(modRF_R1_R1,num_features =40)
dev.off()

pt_modRF_R1_R1<- predict(modRF_R1_R1,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R1_R1,RFsampleRiskR_tr$Riesgo)

p_modRF_R1_R1<- predict(modRF_R1_R1,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R1_R1,RFsampleRiskR_ts$Riesgo)

#1.2rf todas las variables,controlRCV############################################################################################################################
set.seed(123)
modRF_R2_R1<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="rf", trControl=controlRCV, tuneGrid = tunegrid,metric='Accuracy') 
summary(modRF_R2_R1)
modRF_R2_R1$bestTune #best accuaracy found with the mtry
modRF_R2_R1$results #table with each mtry and accuaracy 
modRF_R2_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_R1.pdf")
plot(modRF_R2_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_R1.pdf")
vip(modRF_R2_R1,num_features =40)
dev.off()

pt_modRF_R2_R1<- predict(modRF_R2_R1,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R2_R1,RFsampleRiskR_tr$Riesgo)

p_modRF_R2_R1<- predict(modRF_R2_R1,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R2_R1,RFsampleRiskR_ts$Riesgo)

#1.3rf todas las variables,controlRCV_S############################################################################################################################
set.seed(123)
modRF_R3_R1<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="rf", trControl=controlRCV_S, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_R3_R1)
modRF_R3_R1$bestTune #best accuaracy found with the mtry
modRF_R3_R1$results #table with each mtry and accuaracy 
modRF_R3_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_R1.pdf")
plot(modRF_R3_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_R1.pdf")
vip(modRF_R3_R1,num_features =40)
dev.off()

pt_modRF_R3_R1<- predict(modRF_R3_R1,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R3_R1,RFsampleRiskR_tr$Riesgo)

p_modRF_R3_R1<- predict(modRF_R3_R1,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R3_R1,RFsampleRiskR_ts$Riesgo)

#1.4rf todas las variables,controlRCV_G############################################################################################################################
set.seed(123)
modRF_R4_R1<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="rf", trControl=controlRCV_G, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_R4_R1)
modRF_R4_R1$bestTune #best accuaracy found with the mtry
modRF_R4_R1$results #table with each mtry and accuaracy 
modRF_R4_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_R1.pdf")
plot(modRF_R4_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_R1.pdf")
vip(modRF_R4_R1,num_features =40)
dev.off()

pt_modRF_R4_R1<- predict(modRF_R4_R1,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R4_R1,RFsampleRiskR_tr$Riesgo)

p_modRF_R4_R1<- predict(modRF_R4_R1,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R4_R1,RFsampleRiskR_ts$Riesgo)

#1.5rf todas las variables,controlLOOCV############################################################################################################################
set.seed(123)
modRF_R5_R1<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="rf", trControl=controlLOOCV, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_R5_R1)
modRF_R5_R1$bestTune #best accuaracy found with the mtry
modRF_R5_R1$results #table with each mtry and accuaracy 
modRF_R5_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_R1.pdf")
plot(modRF_R5_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_R1.pdf")
vip(modRF_R5_R1,num_features =40)
dev.off()

pt_modRF_R5_R1<- predict(modRF_R5_R1,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R5_R1,RFsampleRiskR_tr$Riesgo)

p_modRF_R5_R1<- predict(modRF_R5_R1,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R5_R1,RFsampleRiskR_ts$Riesgo)

#2.ranger todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_R2.pdf")
plot(modRF_R_R2)
dev.off()
modRF_R_R2$finalModel

pdf("varImp_R_R2.pdf")
vip(modRF_R_R2,num_features =40)
dev.off()

pt_modRF_R_R2<- predict(modRF_R_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R_R2,RFsampleRiskR_tr$Riesgo)

p_modRF_R_R2<- predict(modRF_R_R2,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R_R2,RFsampleRiskR_ts$Riesgo)

#2.1ranger todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_R2)
modRF_R1_R2$bestTune #best accuaracy found with the mtry
modRF_R1_R2$results #table with each mtry and accuaracy 
modRF_R1_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_R2.pdf")
plot(modRF_R1_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_R2.pdf")
vip(modRF_R1_R2,num_features =40)
dev.off()

pt_modRF_R1_R2<- predict(modRF_R1_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R1_R2,RFsampleRiskR_tr$Riesgo)

p_modRF_R1_R2<- predict(modRF_R1_R2,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R1_R2,RFsampleRiskR_ts$Riesgo)

#2.2ranger todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_R2)
modRF_R2_R2$bestTune #best accuaracy found with the mtry
modRF_R2_R2$results #table with each mtry and accuaracy 
modRF_R2_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_R2.pdf")
plot(modRF_R2_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_R2.pdf")
vip(modRF_R2_R2,num_features =40)
dev.off()

pt_modRF_R2_R2<- predict(modRF_R2_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R2_R2,RFsampleRiskR_tr$Riesgo)

p_modRF_R2_R2<- predict(modRF_R2_R2,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R2_R2,RFsampleRiskR_ts$Riesgo)

#2.3ranger todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_R2)
modRF_R3_R2$bestTune #best accuaracy found with the mtry
modRF_R3_R2$results #table with each mtry and accuaracy 
modRF_R3_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_R2.pdf")
plot(modRF_R3_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_R2.pdf")
vip(modRF_R3_R2,num_features =40)
dev.off()

pt_modRF_R3_R2<- predict(modRF_R3_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R3_R2,RFsampleRiskR_ts$Riesgo)

p_modRF_R3_R2<- predict(modRF_R3_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(p_modRF_R3_R2,RFsampleRiskR_ts$Riesgo)

#2.4ranger todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_R2)
modRF_R4_R2$bestTune #best accuaracy found with the mtry
modRF_R4_R2$results #table with each mtry and accuaracy 
modRF_R4_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_R2.pdf")
plot(modRF_R4_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_R2.pdf")
vip(modRF_R4_R2,num_features =40)
dev.off()

pt_modRF_R4_R2<- predict(modRF_R4_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R4_R2,RFsampleRiskR_tr$Riesgo)

p_modRF_R4_R2<- predict(modRF_R4_R2,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R4_R2,RFsampleRiskR_ts$Riesgo)

#2.5ranger todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_R2<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="ranger", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_R2)
modRF_R5_R2$bestTune #best accuaracy found with the mtry
modRF_R5_R2$results #table with each mtry and accuaracy 
modRF_R5_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_R2.pdf")
plot(modRF_R5_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_R2.pdf")
vip(modRF_R5_R2,num_features =40)
dev.off()

pt_modRF_R5_R2<- predict(modRF_R5_R2,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R5_R2,RFsampleRiskR_tr$Riesgo)

p_modRF_R5_R2<- predict(modRF_R5_R2,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R5_R2,RFsampleRiskR_ts$Riesgo)

#3.cforest todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_R3.pdf")
plot(modRF_R_R3)
dev.off()
modRF_R_R3$finalModel

pdf("varImp_R_R3.pdf")
vip(modRF_R_R3,num_features =40)
dev.off()

pt_modRF_R_R3<- predict(modRF_R_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R_R3,RFsampleRiskR_tr$Riesgo)

p_modRF_R_R3<- predict(modRF_R_R3,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R_R3,RFsampleRiskR_ts$Riesgo)

#3.1cforest todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_R3)
modRF_R1_R3$bestTune #best accuaracy found with the mtry
modRF_R1_R3$results #table with each mtry and accuaracy 
modRF_R1_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_R3.pdf")
plot(modRF_R1_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_R3.pdf")
vip(modRF_R1_R3,num_features =40)
dev.off()

pt_modRF_R1_R3<- predict(modRF_R1_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R1_R3,RFsampleRiskR_tr$Riesgo)

p_modRF_R1_R3<- predict(modRF_R1_R3,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R1_R3,RFsampleRiskR_ts$Riesgo)

#3.2cforest todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_R3)
modRF_R2_R3$bestTune #best accuaracy found with the mtry
modRF_R2_R3$results #table with each mtry and accuaracy 
modRF_R2_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_R3.pdf")
plot(modRF_R2_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_R3.pdf")
vip(modRF_R2_R3,num_features =40)
dev.off()

pt_modRF_R2_R3<- predict(modRF_R2_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R2_R3,RFsampleRiskR_tr$Riesgo)

p_modRF_R2_R3<- predict(modRF_R2_R3,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R2_R3,RFsampleRiskR_ts$Riesgo)

#3.3cforest todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_R3)
modRF_R3_R3$bestTune #best accuaracy found with the mtry
modRF_R3_R3$results #table with each mtry and accuaracy 
modRF_R3_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_R3.pdf")
plot(modRF_R3_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_R3.pdf")
vip(modRF_R3_R3,num_features =40)
dev.off()

pt_modRF_R3_R3<- predict(modRF_R3_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R3_R3,RFsampleRiskR_ts$Riesgo)

p_modRF_R3_R3<- predict(modRF_R3_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(p_modRF_R3_R3,RFsampleRiskR_ts$Riesgo)

#3.4cforest todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_R3)
modRF_R4_R3$bestTune #best accuaracy found with the mtry
modRF_R4_R3$results #table with each mtry and accuaracy 
modRF_R4_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_R3.pdf")
plot(modRF_R4_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_R3.pdf")
vip(modRF_R4_R3,num_features =40)
dev.off()

pt_modRF_R4_R3<- predict(modRF_R4_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R4_R3,RFsampleRiskR_tr$Riesgo)

p_modRF_R4_R3<- predict(modRF_R4_R3,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R4_R3,RFsampleRiskR_ts$Riesgo)

#3.5cforest todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_R3<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="cforest", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_R3)
modRF_R5_R3$bestTune #best accuaracy found with the mtry
modRF_R5_R3$results #table with each mtry and accuaracy 
modRF_R5_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_R3.pdf")
plot(modRF_R5_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_R3.pdf")
vip(modRF_R5_R3,num_features =40)
dev.off()

pt_modRF_R5_R3<- predict(modRF_R5_R3,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R5_R3,RFsampleRiskR_tr$Riesgo)

p_modRF_R5_R3<- predict(modRF_R5_R3,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R5_R3,RFsampleRiskR_ts$Riesgo)

#4.xgbTree todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_R_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlBT, metric='Accuracy')
pdf("modRF_R_R4.pdf")
plot(modRF_R_R4)
dev.off()
modRF_R_R4$finalModel

pdf("varImp_R_R4.pdf")
vip(modRF_R_R4,num_features =40)
dev.off()

pt_modRF_R_R4<- predict(modRF_R_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R_R4<- predict(modRF_R_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R_R4,RFsampleRiskR_ts$Riesgo)

#4.1xgbTree todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_R1_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV,metric='Accuracy')
summary(modRF_R1_R4)
modRF_R1_R4$bestTune #best accuaracy found with the mtry
modRF_R1_R4$results #table with each mtry and accuaracy 
modRF_R1_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R1_R4.pdf")
plot(modRF_R1_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R1_R4.pdf")
vip(modRF_R1_R4,num_features =40)
dev.off()

pt_modRF_R1_R4<- predict(modRF_R1_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R1_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R1_R4<- predict(modRF_R1_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R1_R4,RFsampleRiskR_ts$Riesgo)

#4.2xgbTree todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_R2_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV,metric='Accuracy') 
summary(modRF_R2_R4)
modRF_R2_R4$bestTune #best accuaracy found with the mtry
modRF_R2_R4$results #table with each mtry and accuaracy 
modRF_R2_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R2_R4.pdf")
plot(modRF_R2_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R2_R4.pdf")
vip(modRF_R2_R4,num_features =40)
dev.off()

pt_modRF_R2_R4<- predict(modRF_R2_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R2_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R2_R4<- predict(modRF_R2_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R2_R4,RFsampleRiskR_ts$Riesgo)

#4.3xgbTree todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_R3_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_R3_R4)
modRF_R3_R4$bestTune #best accuaracy found with the mtry
modRF_R3_R4$results #table with each mtry and accuaracy 
modRF_R3_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R3_R4.pdf")
plot(modRF_R3_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R3_R4.pdf")
vip(modRF_R3_R4,num_features =40)
dev.off()

pt_modRF_R3_R4<- predict(modRF_R3_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R3_R4,RFsampleRiskR_ts$Riesgo)

p_modRF_R3_R4<- predict(modRF_R3_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(p_modRF_R3_R4,RFsampleRiskR_ts$Riesgo)

#4.4xgbTree todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_R4_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_R4_R4)
modRF_R4_R4$bestTune #best accuaracy found with the mtry
modRF_R4_R4$results #table with each mtry and accuaracy 
modRF_R4_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R4_R4.pdf")
plot(modRF_R4_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R4_R4.pdf")
vip(modRF_R4_R4,num_features =40)
dev.off()

pt_modRF_R4_R4<- predict(modRF_R4_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R4_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R4_R4<- predict(modRF_R4_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R4_R4,RFsampleRiskR_ts$Riesgo)

#4.5xgbTree todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_R5_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_R5_R4)
modRF_R5_R4$bestTune #best accuaracy found with the mtry
modRF_R5_R4$results #table with each mtry and accuaracy 
modRF_R5_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R5_R4.pdf")
plot(modRF_R5_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R5_R4.pdf")
vip(modRF_R5_R4,num_features =40)
dev.off()

pt_modRF_R5_R4<- predict(modRF_R5_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R5_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R5_R4<- predict(modRF_R5_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R5_R4,RFsampleRiskR_ts$Riesgo)

#4.6xgbTree todas las variables,controlCV5#########################################################################################################################
set.seed(123)
modRF_R6_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV5, metric='Accuracy') 
summary(modRF_R6_R4)
modRF_R6_R4$bestTune #best accuaracy found with the mtry
modRF_R6_R4$results #table with each mtry and accuaracy 
modRF_R6_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R6_R4.pdf")
plot(modRF_R6_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R6_R4.pdf")
vip(modRF_R6_R4,num_features =40)
dev.off()

pt_modRF_R6_R4<- predict(modRF_R6_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R6_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R6_R4<- predict(modRF_R6_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R6_R4,RFsampleRiskR_ts$Riesgo)



#4.7xgbTree todas las variables,controlCV10#########################################################################################################################
set.seed(123)
modRF_R7_R4<- train(Riesgo ~ ., data=RFsampleRiskR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV10, metric='Accuracy') 
summary(modRF_R7_R4)
modRF_R7_R4$bestTune #best accuaracy found with the mtry
modRF_R7_R4$results #table with each mtry and accuaracy 
modRF_R7_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_R7_R4.pdf")
plot(modRF_R7_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_R7_R4.pdf")
vip(modRF_R7_R4,num_features =40)
dev.off()

pt_modRF_R7_R4<- predict(modRF_R7_R4,RFsampleRiskR_tr,type = "raw")
confusionMatrix(pt_modRF_R7_R4,RFsampleRiskR_tr$Riesgo)

p_modRF_R7_R4<- predict(modRF_R7_R4,RFsampleRiskR_ts,type = "raw")
confusionMatrix(p_modRF_R7_R4,RFsampleRiskR_ts$Riesgo)


##RF para Cuotas en mora VARIABLES ORIGINALES#############################################################################################
#1.rf todas las variables,controlBT############################################################################################################################
set.seed(123)
modRF_C_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr,method="rf", trControl=controlBT, metric='Accuracy') 
summary(modRF_C_O1)
modRF_C_O1$bestTune #best accuaracy found with the mtry
modRF_C_O1$results #table with each mtry and accuaracy 
modRF_C_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C_O1.pdf")
plot(modRF_C_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C_O1.pdf")
vip(modRF_C_O1,num_features =40)
dev.off()

pt_modRF_C_O1<- predict(modRF_C_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C_O1<- predict(modRF_C_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C_O1,RFsampleMoraO_ts$CuotMora)

#1.1rf todas las variables,controlCV############################################################################################################################
set.seed(123)
modRF_C1_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], 
                    method="rf", trControl=controlCV, tuneGrid = gridSE,metric='Accuracy',sampsize=c(2,2,2,2,2,2,2)) 
summary(modRF_C1_O1)
modRF_C1_O1$bestTune #best accuaracy found with the mtry
modRF_C1_O1$results #table with each mtry and accuaracy 
modRF_C1_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_O1.pdf")
plot(modRF_C1_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_O1.pdf")
vip(modRF_C1_O1,num_features =40)
dev.off()

pt_modRF_C1_O1<- predict(modRF_C1_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C1_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C1_O1<- predict(modRF_C1_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C1_O1,RFsampleMoraO_ts$CuotMora)

#1.2rf todas las variables,controlRCV############################################################################################################################
set.seed(123)
modRF_C2_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="rf", trControl=controlRCV, tuneGrid = tunegrid,metric='Accuracy') 
summary(modRF_C2_O1)
modRF_C2_O1$bestTune #best accuaracy found with the mtry
modRF_C2_O1$results #table with each mtry and accuaracy 
modRF_C2_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_O1.pdf")
plot(modRF_C2_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_O1.pdf")
vip(modRF_C2_O1,num_features =40)
dev.off()

pt_modRF_C2_O1<- predict(modRF_C2_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C2_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C2_O1<- predict(modRF_C2_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C2_O1,RFsampleMoraO_ts$CuotMora)

#1.3rf todas las variables,controlRCV_S############################################################################################################################
set.seed(123)
modRF_C3_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="rf", trControl=controlRCV_S, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C3_O1)
modRF_C3_O1$bestTune #best accuaracy found with the mtry
modRF_C3_O1$results #table with each mtry and accuaracy 
modRF_C3_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_O1.pdf")
plot(modRF_C3_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_O1.pdf")
vip(modRF_C3_O1,num_features =40)
dev.off()

pt_modRF_C3_O1<- predict(modRF_C3_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C3_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C3_O1<- predict(modRF_C3_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C3_O1,RFsampleMoraO_ts$CuotMora)

#1.4rf todas las variables,controlRCV_G############################################################################################################################
set.seed(123)
modRF_C4_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="rf", trControl=controlRCV_G, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C4_O1)
modRF_C4_O1$bestTune #best accuaracy found with the mtry
modRF_C4_O1$results #table with each mtry and accuaracy 
modRF_C4_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_O1.pdf")
plot(modRF_C4_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_O1.pdf")
vip(modRF_C4_O1,num_features =40)
dev.off()

pt_modRF_C4_O1<- predict(modRF_C4_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C4_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C4_O1<- predict(modRF_C4_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C4_O1,RFsampleMoraO_ts$CuotMora)

#1.5rf todas las variables,controlLOOCV############################################################################################################################
set.seed(123)
modRF_C5_O1<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="rf", trControl=controlLOOCV, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C5_O1)
modRF_C5_O1$bestTune #best accuaracy found with the mtry
modRF_C5_O1$results #table with each mtry and accuaracy 
modRF_C5_O1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_O1.pdf")
plot(modRF_C5_O1) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_O1.pdf")
vip(modRF_C5_O1,num_features =40)
dev.off()

pt_modRF_C5_O1<- predict(modRF_C5_O1,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C5_O1,RFsampleMoraO_tr$CuotMora)

p_modRF_C5_O1<- predict(modRF_C5_O1,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C5_O1,RFsampleMoraO_ts$CuotMora)

#2.ranger todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_O2.pdf")
plot(modRF_C_O2)
dev.off()
modRF_C_O2$finalModel

pdf("varImp_C_O2.pdf")
vip(modRF_C_O2,num_features =40)
dev.off()

pt_modRF_C_O2<- predict(modRF_C_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C_O2,RFsampleMoraO_tr$CuotMora)

p_modRF_C_O2<- predict(modRF_C_O2,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C_O2,RFsampleMoraO_ts$CuotMora)

#2.1ranger todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_O2)
modRF_C1_O2$bestTune #best accuaracy found with the mtry
modRF_C1_O2$results #table with each mtry and accuaracy 
modRF_C1_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_O2.pdf")
plot(modRF_C1_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_O2.pdf")
vip(modRF_C1_O2,num_features =40)
dev.off()

pt_modRF_C1_O2<- predict(modRF_C1_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C1_O2,RFsampleMoraO_tr$CuotMora)

p_modRF_C1_O2<- predict(modRF_C1_O2,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C1_O2,RFsampleMoraO_ts$CuotMora)

#2.2ranger todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_O2)
modRF_C2_O2$bestTune #best accuaracy found with the mtry
modRF_C2_O2$results #table with each mtry and accuaracy 
modRF_C2_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_O2.pdf")
plot(modRF_C2_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_O2.pdf")
vip(modRF_C2_O2,num_features =40)
dev.off()

pt_modRF_C2_O2<- predict(modRF_C2_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C2_O2,RFsampleMoraO_tr$CuotMora)

p_modRF_C2_O2<- predict(modRF_C2_O2,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C2_O2,RFsampleMoraO_ts$CuotMora)

#2.3ranger todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_O2)
modRF_C3_O2$bestTune #best accuaracy found with the mtry
modRF_C3_O2$results #table with each mtry and accuaracy 
modRF_C3_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_O2.pdf")
plot(modRF_C3_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_O2.pdf")
vip(modRF_C3_O2,num_features =40)
dev.off()

pt_modRF_C3_O2<- predict(modRF_C3_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C3_O2,RFsampleMoraO_ts$CuotMora)

p_modRF_C3_O2<- predict(modRF_C3_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(p_modRF_C3_O2,RFsampleMoraO_ts$CuotMora)

#2.4ranger todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_O2)
modRF_C4_O2$bestTune #best accuaracy found with the mtry
modRF_C4_O2$results #table with each mtry and accuaracy 
modRF_C4_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_O2.pdf")
plot(modRF_C4_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_O2.pdf")
vip(modRF_C4_O2,num_features =40)
dev.off()

pt_modRF_C4_O2<- predict(modRF_C4_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C4_O2,RFsampleMoraO_tr$CuotMora)

p_modRF_C4_O2<- predict(modRF_C4_O2,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C4_O2,RFsampleMoraO_ts$CuotMora)

#2.5ranger todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_O2<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="ranger", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_O2)
modRF_C5_O2$bestTune #best accuaracy found with the mtry
modRF_C5_O2$results #table with each mtry and accuaracy 
modRF_C5_O2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_O2.pdf")
plot(modRF_C5_O2) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_O2.pdf")
vip(modRF_C5_O2,num_features =40)
dev.off()

pt_modRF_C5_O2<- predict(modRF_C5_O2,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C5_O2,RFsampleMoraO_tr$CuotMora)

p_modRF_C5_O2<- predict(modRF_C5_O2,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C5_O2,RFsampleMoraO_ts$CuotMora)

#3.cforest todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_O3.pdf")
plot(modRF_C_O3)
dev.off()
modRF_C_O3$finalModel

pdf("varImp_C_O3.pdf")
vip(modRF_C_O3,num_features =40)
dev.off()

pt_modRF_C_O3<- predict(modRF_C_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C_O3,RFsampleMoraO_tr$CuotMora)

p_modRF_C_O3<- predict(modRF_C_O3,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C_O3,RFsampleMoraO_ts$CuotMora)

#3.1cforest todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_O3)
modRF_C1_O3$bestTune #best accuaracy found with the mtry
modRF_C1_O3$results #table with each mtry and accuaracy 
modRF_C1_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_O3.pdf")
plot(modRF_C1_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_O3.pdf")
vip(modRF_C1_O3,num_features =40)
dev.off()

pt_modRF_C1_O3<- predict(modRF_C1_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C1_O3,RFsampleMoraO_tr$CuotMora)

p_modRF_C1_O3<- predict(modRF_C1_O3,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C1_O3,RFsampleMoraO_ts$CuotMora)

#3.2cforest todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_O3)
modRF_C2_O3$bestTune #best accuaracy found with the mtry
modRF_C2_O3$results #table with each mtry and accuaracy 
modRF_C2_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_O3.pdf")
plot(modRF_C2_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_O3.pdf")
vip(modRF_C2_O3,num_features =40)
dev.off()

pt_modRF_C2_O3<- predict(modRF_C2_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C2_O3,RFsampleMoraO_tr$CuotMora)

p_modRF_C2_O3<- predict(modRF_C2_O3,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C2_O3,RFsampleMoraO_ts$CuotMora)

#3.3cforest todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_O3)
modRF_C3_O3$bestTune #best accuaracy found with the mtry
modRF_C3_O3$results #table with each mtry and accuaracy 
modRF_C3_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_O3.pdf")
plot(modRF_C3_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_O3.pdf")
vip(modRF_C3_O3,num_features =40)
dev.off()

pt_modRF_C3_O3<- predict(modRF_C3_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C3_O3,RFsampleMoraO_ts$CuotMora)

p_modRF_C3_O3<- predict(modRF_C3_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(p_modRF_C3_O3,RFsampleMoraO_ts$CuotMora)

#3.4cforest todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_O3)
modRF_C4_O3$bestTune #best accuaracy found with the mtry
modRF_C4_O3$results #table with each mtry and accuaracy 
modRF_C4_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_O3.pdf")
plot(modRF_C4_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_O3.pdf")
vip(modRF_C4_O3,num_features =40)
dev.off()

pt_modRF_C4_O3<- predict(modRF_C4_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C4_O3,RFsampleMoraO_tr$CuotMora)

p_modRF_C4_O3<- predict(modRF_C4_O3,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C4_O3,RFsampleMoraO_ts$CuotMora)

#3.5cforest todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_O3<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="cforest", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_O3)
modRF_C5_O3$bestTune #best accuaracy found with the mtry
modRF_C5_O3$results #table with each mtry and accuaracy 
modRF_C5_O3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_O3.pdf")
plot(modRF_C5_O3) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_O3.pdf")
vip(modRF_C5_O3,num_features =40)
dev.off()

pt_modRF_C5_O3<- predict(modRF_C5_O3,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C5_O3,RFsampleMoraO_tr$CuotMora)

p_modRF_C5_O3<- predict(modRF_C5_O3,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C5_O3,RFsampleMoraO_ts$CuotMora)

#4.xgbTree todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_O4.pdf")
plot(modRF_C_O4)
dev.off()
modRF_C_O4$finalModel

pdf("varImp_C_O4.pdf")
vip(modRF_C_O4,num_features =40)
dev.off()

pt_modRF_C_O4<- predict(modRF_C_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C_O4<- predict(modRF_C_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C_O4,RFsampleMoraO_ts$CuotMora)

#4.1xgbTree todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_O4)
modRF_C1_O4$bestTune #best accuaracy found with the mtry
modRF_C1_O4$results #table with each mtry and accuaracy 
modRF_C1_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_O4.pdf")
plot(modRF_C1_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_O4.pdf")
vip(modRF_C1_O4,num_features =40)
dev.off()

pt_modRF_C1_O4<- predict(modRF_C1_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C1_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C1_O4<- predict(modRF_C1_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C1_O4,RFsampleMoraO_ts$CuotMora)

#4.2xgbTree todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_O4)
modRF_C2_O4$bestTune #best accuaracy found with the mtry
modRF_C2_O4$results #table with each mtry and accuaracy 
modRF_C2_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_O4.pdf")
plot(modRF_C2_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_O4.pdf")
vip(modRF_C2_O4,num_features =40)
dev.off()

pt_modRF_C2_O4<- predict(modRF_C2_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C2_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C2_O4<- predict(modRF_C2_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C2_O4,RFsampleMoraO_ts$CuotMora)

#4.3xgbTree todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_O4)
modRF_C3_O4$bestTune #best accuaracy found with the mtry
modRF_C3_O4$results #table with each mtry and accuaracy 
modRF_C3_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_O4.pdf")
plot(modRF_C3_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_O4.pdf")
vip(modRF_C3_O4,num_features =40)
dev.off()

pt_modRF_C3_O4<- predict(modRF_C3_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C3_O4,RFsampleMoraO_ts$CuotMora)

p_modRF_C3_O4<- predict(modRF_C3_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(p_modRF_C3_O4,RFsampleMoraO_ts$CuotMora)

#4.4xgbTree todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_O4)
modRF_C4_O4$bestTune #best accuaracy found with the mtry
modRF_C4_O4$results #table with each mtry and accuaracy 
modRF_C4_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_O4.pdf")
plot(modRF_C4_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_O4.pdf")
vip(modRF_C4_O4,num_features =40)
dev.off()

pt_modRF_C4_O4<- predict(modRF_C4_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C4_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C4_O4<- predict(modRF_C4_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C4_O4,RFsampleMoraO_ts$CuotMora)

#4.5xgbTree todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_O4)
modRF_C5_O4$bestTune #best accuaracy found with the mtry
modRF_C5_O4$results #table with each mtry and accuaracy 
modRF_C5_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_O4.pdf")
plot(modRF_C5_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_O4.pdf")
vip(modRF_C5_O4,num_features =40)
dev.off()

pt_modRF_C5_O4<- predict(modRF_C5_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C5_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C5_O4<- predict(modRF_C5_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C5_O4,RFsampleMoraO_ts$CuotMora)

#4.6xgbTree todas las variables,controlCV5#########################################################################################################################
set.seed(123)
modRF_C6_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV5, metric='Accuracy') 
summary(modRF_C6_O4)
modRF_C6_O4$bestTune #best accuaracy found with the mtry
modRF_C6_O4$results #table with each mtry and accuaracy 
modRF_C6_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C6_O4.pdf")
plot(modRF_C6_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C6_O4.pdf")
vip(modRF_C6_O4,num_features =40)
dev.off()

pt_modRF_C6_O4<- predict(modRF_C6_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C6_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C6_O4<- predict(modRF_C6_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C6_O4,RFsampleMoraO_ts$CuotMora)



#4.7xgbTree todas las variables,controlCV10#########################################################################################################################
set.seed(123)
modRF_C7_O4<- train(CuotMora ~ ., data=RFsampleMoraO_tr[,-c(63:64)], method="xgbTree", trControl=controlCV10, metric='Accuracy') 
summary(modRF_C7_O4)
modRF_C7_O4$bestTune #best accuaracy found with the mtry
modRF_C7_O4$results #table with each mtry and accuaracy 
modRF_C7_O4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C7_O4.pdf")
plot(modRF_C7_O4) #mtry vs accuaracy
dev.off()

pdf("varImp_C7_O4.pdf")
vip(modRF_C7_O4,num_features =40)
dev.off()

pt_modRF_C7_O4<- predict(modRF_C7_O4,RFsampleMoraO_tr,type = "raw")
confusionMatrix(pt_modRF_C7_O4,RFsampleMoraO_tr$CuotMora)

p_modRF_C7_O4<- predict(modRF_C7_O4,RFsampleMoraO_ts,type = "raw")
confusionMatrix(p_modRF_C7_O4,RFsampleMoraO_ts$CuotMora)



##RF para Cuotas en mora VARIABLES CONSTRUIDAS (RANGOS)#############################################################################################
#0.rf todas las variables,controlBT############################################################################################################################
set.seed(123)
modRF_C_R<- train(CuotMora ~ Scoring.binned, data=RFsampleMoraR_tr,method="rf", metric='Accuracy') 
summary(modRF_C_R)
modRF_C_R$bestTune #best accuaracy found with the mtry
modRF_C_R$results #table with each mtry and accuaracy 
modRF_C_R$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C_R.pdf")
plot(modRF_C_R) #mtry vs accuaracy
dev.off()

pdf("varImp_C_R.pdf")
vip(modRF_C_R,num_features =40)
dev.off()

pt_modRF_C_R<- predict(modRF_C_R,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C_R,RFsampleMoraR_tr$CuotMora)

p_modRF_C_R<- predict(modRF_C_R,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C_R,RFsampleMoraR_ts$CuotMora)

#1.rf todas las variables, controlBT############################################################################################################################
set.seed(123)
modRF_C_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr, method="rf", trControl=controlBT, metric='Accuracy') 
summary(modRF_C_R1)
modRF_C_R1$bestTune #best accuaracy found with the mtry
modRF_C_R1$results #table with each mtry and accuaracy 
modRF_C_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C_R1.pdf")
plot(modRF_C_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C_R1.pdf")
vip(modRF_C_R1,num_features =40)
dev.off()

pt_modRF_C_R1<- predict(modRF_C_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C_R1<- predict(modRF_C_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C_R1,RFsampleMoraR_ts$CuotMora)

#1.1rf todas las variables, controlCV############################################################################################################################
set.seed(123)
modRF_C1_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="rf", trControl=controlCV, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C1_R1)
modRF_C1_R1$bestTune #best accuaracy found with the mtry
modRF_C1_R1$results #table with each mtry and accuaracy 
modRF_C1_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_R1.pdf")
plot(modRF_C1_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_R1.pdf")
vip(modRF_C1_R1,num_features =40)
dev.off()

pt_modRF_C1_R1<- predict(modRF_C1_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C1_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C1_R1<- predict(modRF_C1_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C1_R1,RFsampleMoraR_ts$CuotMora)

#1.2rf todas las variables, controlRCV############################################################################################################################
set.seed(123)
modRF_C2_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="rf", trControl=controlRCV, tuneGrid = tunegrid,metric='Accuracy') 
summary(modRF_C2_R1)
modRF_C2_R1$bestTune #best accuaracy found with the mtry
modRF_C2_R1$results #table with each mtry and accuaracy 
modRF_C2_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_R1.pdf")
plot(modRF_C2_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_R1.pdf")
vip(modRF_C2_R1,num_features =40)
dev.off()

pt_modRF_C2_R1<- predict(modRF_C2_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C2_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C2_R1<- predict(modRF_C2_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C2_R1,RFsampleMoraR_ts$CuotMora)

#1.3rf todas las variables, controlRCV_S############################################################################################################################
set.seed(123)
modRF_C3_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="rf", trControl=controlRCV_S, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C3_R1)
modRF_C3_R1$bestTune #best accuaracy found with the mtry
modRF_C3_R1$results #table with each mtry and accuaracy 
modRF_C3_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_R1.pdf")
plot(modRF_C3_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_R1.pdf")
vip(modRF_C3_R1,num_features =40)
dev.off()

pt_modRF_C3_R1<- predict(modRF_C3_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C3_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C3_R1<- predict(modRF_C3_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C3_R1,RFsampleMoraR_ts$CuotMora)

#1.4rf todas las variables, controlRCV_G############################################################################################################################
set.seed(123)
modRF_C4_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="rf", trControl=controlRCV_G, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C4_R1)
modRF_C4_R1$bestTune #best accuaracy found with the mtry
modRF_C4_R1$results #table with each mtry and accuaracy 
modRF_C4_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_R1.pdf")
plot(modRF_C4_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_R1.pdf")
vip(modRF_C4_R1,num_features =40)
dev.off()

pt_modRF_C4_R1<- predict(modRF_C4_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C4_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C4_R1<- predict(modRF_C4_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C4_R1,RFsampleMoraR_ts$CuotMora)

#1.5rf todas las variables, controlLOOCV############################################################################################################################
set.seed(123)
modRF_C5_R1<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="rf", trControl=controlLOOCV, tuneGrid = gridSE,metric='Accuracy') 
summary(modRF_C5_R1)
modRF_C5_R1$bestTune #best accuaracy found with the mtry
modRF_C5_R1$results #table with each mtry and accuaracy 
modRF_C5_R1$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_R1.pdf")
plot(modRF_C5_R1) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_R1.pdf")
vip(modRF_C5_R1,num_features =40)
dev.off()

pt_modRF_C5_R1<- predict(modRF_C5_R1,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C5_R1,RFsampleMoraR_tr$CuotMora)

p_modRF_C5_R1<- predict(modRF_C5_R1,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C5_R1,RFsampleMoraR_ts$CuotMora)

#2.ranger todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_R2.pdf")
plot(modRF_C_R2)
dev.off()
modRF_C_R2$finalModel

pdf("varImp_C_R2.pdf")
vip(modRF_C_R2,num_features =40)
dev.off()

pt_modRF_C_R2<- predict(modRF_C_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C_R2,RFsampleMoraR_tr$CuotMora)

p_modRF_C_R2<- predict(modRF_C_R2,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C_R2,RFsampleMoraR_ts$CuotMora)

#2.1ranger todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_R2)
modRF_C1_R2$bestTune #best accuaracy found with the mtry
modRF_C1_R2$results #table with each mtry and accuaracy 
modRF_C1_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_R2.pdf")
plot(modRF_C1_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_R2.pdf")
vip(modRF_C1_R2,num_features =40)
dev.off()

pt_modRF_C1_R2<- predict(modRF_C1_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C1_R2,RFsampleMoraR_tr$CuotMora)

p_modRF_C1_R2<- predict(modRF_C1_R2,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C1_R2,RFsampleMoraR_ts$CuotMora)

#2.2ranger todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_R2)
modRF_C2_R2$bestTune #best accuaracy found with the mtry
modRF_C2_R2$results #table with each mtry and accuaracy 
modRF_C2_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_R2.pdf")
plot(modRF_C2_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_R2.pdf")
vip(modRF_C2_R2,num_features =40)
dev.off()

pt_modRF_C2_R2<- predict(modRF_C2_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C2_R2,RFsampleMoraR_tr$CuotMora)

p_modRF_C2_R2<- predict(modRF_C2_R2,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C2_R2,RFsampleMoraR_ts$CuotMora)

#2.3ranger todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_R2)
modRF_C3_R2$bestTune #best accuaracy found with the mtry
modRF_C3_R2$results #table with each mtry and accuaracy 
modRF_C3_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_R2.pdf")
plot(modRF_C3_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_R2.pdf")
vip(modRF_C3_R2,num_features =40)
dev.off()

pt_modRF_C3_R2<- predict(modRF_C3_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C3_R2,RFsampleMoraR_ts$CuotMora)

p_modRF_C3_R2<- predict(modRF_C3_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(p_modRF_C3_R2,RFsampleMoraR_ts$CuotMora)

#2.4ranger todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_R2)
modRF_C4_R2$bestTune #best accuaracy found with the mtry
modRF_C4_R2$results #table with each mtry and accuaracy 
modRF_C4_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_R2.pdf")
plot(modRF_C4_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_R2.pdf")
vip(modRF_C4_R2,num_features =40)
dev.off()

pt_modRF_C4_R2<- predict(modRF_C4_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C4_R2,RFsampleMoraR_tr$CuotMora)

p_modRF_C4_R2<- predict(modRF_C4_R2,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C4_R2,RFsampleMoraR_ts$CuotMora)

#2.5ranger todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_R2<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="ranger", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_R2)
modRF_C5_R2$bestTune #best accuaracy found with the mtry
modRF_C5_R2$results #table with each mtry and accuaracy 
modRF_C5_R2$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_R2.pdf")
plot(modRF_C5_R2) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_R2.pdf")
vip(modRF_C5_R2,num_features =40)
dev.off()

pt_modRF_C5_R2<- predict(modRF_C5_R2,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C5_R2,RFsampleMoraR_tr$CuotMora)

p_modRF_C5_R2<- predict(modRF_C5_R2,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C5_R2,RFsampleMoraR_ts$CuotMora)

#3.cforest todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_R3.pdf")
plot(modRF_C_R3)
dev.off()
modRF_C_R3$finalModel

pdf("varImp_C_R3.pdf")
vip(modRF_C_R3,num_features =40)
dev.off()

pt_modRF_C_R3<- predict(modRF_C_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C_R3,RFsampleMoraR_tr$CuotMora)

p_modRF_C_R3<- predict(modRF_C_R3,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C_R3,RFsampleMoraR_ts$CuotMora)

#3.1cforest todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_R3)
modRF_C1_R3$bestTune #best accuaracy found with the mtry
modRF_C1_R3$results #table with each mtry and accuaracy 
modRF_C1_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_R3.pdf")
plot(modRF_C1_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_R3.pdf")
vip(modRF_C1_R3,num_features =40)
dev.off()

pt_modRF_C1_R3<- predict(modRF_C1_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C1_R3,RFsampleMoraR_tr$CuotMora)

p_modRF_C1_R3<- predict(modRF_C1_R3,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C1_R3,RFsampleMoraR_ts$CuotMora)

#3.2cforest todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_R3)
modRF_C2_R3$bestTune #best accuaracy found with the mtry
modRF_C2_R3$results #table with each mtry and accuaracy 
modRF_C2_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_R3.pdf")
plot(modRF_C2_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_R3.pdf")
vip(modRF_C2_R3,num_features =40)
dev.off()

pt_modRF_C2_R3<- predict(modRF_C2_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C2_R3,RFsampleMoraR_tr$CuotMora)

p_modRF_C2_R3<- predict(modRF_C2_R3,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C2_R3,RFsampleMoraR_ts$CuotMora)

#3.3cforest todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_R3)
modRF_C3_R3$bestTune #best accuaracy found with the mtry
modRF_C3_R3$results #table with each mtry and accuaracy 
modRF_C3_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_R3.pdf")
plot(modRF_C3_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_R3.pdf")
vip(modRF_C3_R3,num_features =40)
dev.off()

pt_modRF_C3_R3<- predict(modRF_C3_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C3_R3,RFsampleMoraR_ts$CuotMora)

p_modRF_C3_R3<- predict(modRF_C3_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(p_modRF_C3_R3,RFsampleMoraR_ts$CuotMora)

#3.4cforest todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_R3)
modRF_C4_R3$bestTune #best accuaracy found with the mtry
modRF_C4_R3$results #table with each mtry and accuaracy 
modRF_C4_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_R3.pdf")
plot(modRF_C4_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_R3.pdf")
vip(modRF_C4_R3,num_features =40)
dev.off()

pt_modRF_C4_R3<- predict(modRF_C4_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C4_R3,RFsampleMoraR_tr$CuotMora)

p_modRF_C4_R3<- predict(modRF_C4_R3,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C4_R3,RFsampleMoraR_ts$CuotMora)

#3.5cforest todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_R3<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="cforest", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_R3)
modRF_C5_R3$bestTune #best accuaracy found with the mtry
modRF_C5_R3$results #table with each mtry and accuaracy 
modRF_C5_R3$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_R3.pdf")
plot(modRF_C5_R3) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_R3.pdf")
vip(modRF_C5_R3,num_features =40)
dev.off()

pt_modRF_C5_R3<- predict(modRF_C5_R3,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C5_R3,RFsampleMoraR_tr$CuotMora)

p_modRF_C5_R3<- predict(modRF_C5_R3,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C5_R3,RFsampleMoraR_ts$CuotMora)

#4.xgbTree todas las variables,controlBT#########################################################################################################################
set.seed(123)
modRF_C_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlBT, metric='Accuracy')
pdf("modRF_C_R4.pdf")
plot(modRF_C_R4)
dev.off()
modRF_C_R4$finalModel

pdf("varImp_C_R4.pdf")
vip(modRF_C_R4,num_features =40)
dev.off()

pt_modRF_C_R4<- predict(modRF_C_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C_R4<- predict(modRF_C_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C_R4,RFsampleMoraR_ts$CuotMora)

#4.1xgbTree todas las variables,controlCV#########################################################################################################################
set.seed(123)
modRF_C1_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV,metric='Accuracy')
summary(modRF_C1_R4)
modRF_C1_R4$bestTune #best accuaracy found with the mtry
modRF_C1_R4$results #table with each mtry and accuaracy 
modRF_C1_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C1_R4.pdf")
plot(modRF_C1_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C1_R4.pdf")
vip(modRF_C1_R4,num_features =40)
dev.off()

pt_modRF_C1_R4<- predict(modRF_C1_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C1_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C1_R4<- predict(modRF_C1_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C1_R4,RFsampleMoraR_ts$CuotMora)

#4.2xgbTree todas las variables,controlRCV#########################################################################################################################
set.seed(123)
modRF_C2_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV,metric='Accuracy') 
summary(modRF_C2_R4)
modRF_C2_R4$bestTune #best accuaracy found with the mtry
modRF_C2_R4$results #table with each mtry and accuaracy 
modRF_C2_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C2_R4.pdf")
plot(modRF_C2_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C2_R4.pdf")
vip(modRF_C2_R4,num_features =40)
dev.off()

pt_modRF_C2_R4<- predict(modRF_C2_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C2_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C2_R4<- predict(modRF_C2_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C2_R4,RFsampleMoraR_ts$CuotMora)

#4.3xgbTree todas las variables,controlRCV_S#########################################################################################################################
set.seed(123)
modRF_C3_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_S, metric='Accuracy') 
summary(modRF_C3_R4)
modRF_C3_R4$bestTune #best accuaracy found with the mtry
modRF_C3_R4$results #table with each mtry and accuaracy 
modRF_C3_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C3_R4.pdf")
plot(modRF_C3_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C3_R4.pdf")
vip(modRF_C3_R4,num_features =40)
dev.off()

pt_modRF_C3_R4<- predict(modRF_C3_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C3_R4,RFsampleMoraR_ts$CuotMora)

p_modRF_C3_R4<- predict(modRF_C3_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(p_modRF_C3_R4,RFsampleMoraR_ts$CuotMora)

#4.4xgbTree todas las variables,controlRCV_G#########################################################################################################################
set.seed(123)
modRF_C4_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlRCV_G, metric='Accuracy') 
summary(modRF_C4_R4)
modRF_C4_R4$bestTune #best accuaracy found with the mtry
modRF_C4_R4$results #table with each mtry and accuaracy 
modRF_C4_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C4_R4.pdf")
plot(modRF_C4_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C4_R4.pdf")
vip(modRF_C4_R4,num_features =40)
dev.off()

pt_modRF_C4_R4<- predict(modRF_C4_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C4_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C4_R4<- predict(modRF_C4_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C4_R4,RFsampleMoraR_ts$CuotMora)

#4.5xgbTree todas las variables,controlLOOCV#########################################################################################################################
set.seed(123)
modRF_C5_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlLOOCV, metric='Accuracy') 
summary(modRF_C5_R4)
modRF_C5_R4$bestTune #best accuaracy found with the mtry
modRF_C5_R4$results #table with each mtry and accuaracy 
modRF_C5_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C5_R4.pdf")
plot(modRF_C5_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C5_R4.pdf")
vip(modRF_C5_R4,num_features =40)
dev.off()

pt_modRF_C5_R4<- predict(modRF_C5_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C5_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C5_R4<- predict(modRF_C5_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C5_R4,RFsampleMoraR_ts$CuotMora)

#4.6xgbTree todas las variables, controlCV5#########################################################################################################################
set.seed(123)
modRF_C6_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV5, metric='Accuracy') 
summary(modRF_C6_R4)
modRF_C6_R4$bestTune #best accuaracy found with the mtry
modRF_C6_R4$results #table with each mtry and accuaracy 
modRF_C6_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C6_R4.pdf")
plot(modRF_C6_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C6_R4.pdf")
vip(modRF_C6_R4,num_features =40)
dev.off()

pt_modRF_C6_R4<- predict(modRF_C6_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C6_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C6_R4<- predict(modRF_C6_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C6_R4,RFsampleMoraR_ts$CuotMora)

#4.7xgbTree todas las variables, controlCV10#########################################################################################################################
set.seed(123)
modRF_C7_R4<- train(CuotMora ~ ., data=RFsampleMoraR_tr[,-c(63:64)], method="xgbTree", trControl=controlCV10, metric='Accuracy') 
summary(modRF_C7_R4)
modRF_C7_R4$bestTune #best accuaracy found with the mtry
modRF_C7_R4$results #table with each mtry and accuaracy 
modRF_C7_R4$finalModel #matriz de confusion, ntree, final mtry, OOB error

pdf("mtry_C7_R4.pdf")
plot(modRF_C7_R4) #mtry vs accuaracy
dev.off()

pdf("varImp_C7_R4.pdf")
vip(modRF_C7_R4,num_features =40)
dev.off()

pt_modRF_C7_R4<- predict(modRF_C7_R4,RFsampleMoraR_tr,type = "raw")
confusionMatrix(pt_modRF_C7_R4,RFsampleMoraR_tr$CuotMora)

p_modRF_C7_R4<- predict(modRF_C7_R4,RFsampleMoraR_ts,type = "raw")
confusionMatrix(p_modRF_C7_R4,RFsampleMoraR_ts$CuotMora)
