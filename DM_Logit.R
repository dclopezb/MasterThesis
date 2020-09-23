#___________________________________________________________________________________________________________________________________________________
#___________________________________________________________________________________________________________________________________________________
#_________________________________________Prediccion del riesgo Default en Acuerdos de Ingreso Compartido:__________________________________________
#__________________________________________________MSc.(c) Diana Carolina Lopez Becerra_____________________________________________________________
#____________________________________________________________Ph.D.Hernando Diaz_____________________________________________________________________
#__________________________________________Universidad Nacional de Colombia, sede Bogota D.C._______________________________________________________
#_________________________________________________________________2020______________________________________________________________________________

#install.packages(c("woeBinning","ROCR","arm","caret","gtools","tidyr"))############################################################################
library(ROCR)
library(arm)
library(caret)
library(gtools)#
library(tidyr)#

#construccion modelo############################################################################################################################
sampleR<-read.csv(paste('sampleR.csv',sep=""),header = TRUE, sep="," )
sampleDefaultR<-sampleR[,9:72]
sampleDefaultR<-sampleDefaultR[,-c(2,18,23)]
write.csv(sampleDefaultR,paste('sampleDefaultR.csv',sep=""))

set.seed(1234)
id_entrenamientoR<-createDataPartition(sampleDefaultR$Default, p=0.7, list=FALSE)
sampleDefaultR_tr<-sampleDefaultR[id_entrenamientoR,]
sampleDefaultR_ts<-sampleDefaultR[-id_entrenamientoR,]

start_time <- Sys.time()
h<-1
initial<-1
bonus<-1.2
formula <- list()
model<-data.frame()
best_model_desc<-data.frame()
strCols=names(sampleDefaultR_tr)
final_confusion_matrix<-""

for(j in (ncol(sampleDefaultR_tr)-1):57){
  aux_combinatorias<-data.frame(combinations((ncol(sampleDefaultR_tr)-1),j,colnames(sampleDefaultR_tr)[-1]))
  for(i in 1:nrow(aux_combinatorias)){
    aux<-unite(aux_combinatorias[i,],aux,sep="+")
    formula[[1]] = paste0(strCols[1], " ~ ", aux)
    lm_D_R2<-bayesglm(formula[[1]], data = sampleDefaultR_tr,family=binomial,control = list(maxit = 100),
                      drop.unused.levels   = FALSE)
    y_D_R2_t<- predict(lm_D_R2, sampleDefaultR_tr, type = "response")
    y_D_R2<- predict(lm_D_R2, sampleDefaultR_ts, type = "response")
    
    l<-0.15
    while (l<0.45) {
      ts_D_R2_t = rep("No", dim(sampleDefaultR_tr)[1])
      ts_D_R2_t[y_D_R2_t > l] = "Si"
      ts_D_R2_t<-as.factor(ts_D_R2_t)
      ts_D_R2 = rep("No", dim(sampleDefaultR_ts)[1])
      ts_D_R2[y_D_R2 > l] = "Si"
      ts_D_R2<-as.factor(ts_D_R2)
      
      aux_confusionMatrix_t<-confusionMatrix(ts_D_R2_t,sampleDefaultR_tr$Default)
      aux_confusionMatrix<-confusionMatrix(ts_D_R2,sampleDefaultR_ts$Default)
      
      if(initial==1){
        pre_confusionMatrix<-aux_confusionMatrix
        pre_confusionMatrix_t<-aux_confusionMatrix_t
        pre_model<-lm_D_R2
        pre_formula<-formula[[1]]
        pre_position<-i
        pre_corte<-l
        initial<-2
      }else{
        if((pre_confusionMatrix$byClass[1]+pre_confusionMatrix$byClass[2]*bonus)<(aux_confusionMatrix$byClass[1]+aux_confusionMatrix$byClass[2]*bonus)){
          #Prueba
          best_confusion_matrix<-aux_confusionMatrix
          best_lm_D_R2<-lm_D_R2
          model_desc<-formula[[1]]
          position<-i
          corte<-l
          #Entrenamiento
          best_confusion_matrix_t<-aux_confusionMatrix_t
        }
        else{
          best_confusion_matrix<-pre_confusionMatrix
          best_lm_D_R2<-pre_model
          model_desc<-pre_formula
          position<-pre_position
          corte<-pre_corte
          #Entrenamiento
          best_confusion_matrix_t<-pre_confusionMatrix_t
        }
      }
      l<-l+0.02
    }
  }
  initial<-1
  if(length(final_confusion_matrix)==1){
    #entrenamiento
    best_mc_t<-best_confusion_matrix_t
    print(best_mc_t)
    #prueba
    final_confusion_matrix<-best_confusion_matrix
    print(final_confusion_matrix)
    model<-best_lm_D_R2
    best_model_desc<-model_desc
    best_position<-position
    best_corte<-corte
    best_prediccion_t<-y_D_R2_t
    best_prediccion_s<-y_D_R2
    }else{
    if((final_confusion_matrix$byClass[1]+final_confusion_matrix$byClass[2]*bonus)<=(best_confusion_matrix$byClass[1]+best_confusion_matrix$byClass[2]*bonus)){
      #entrenamiento
      best_mc_t<-best_confusion_matrix_t
      print(best_mc_t)
      #prueba
      final_confusion_matrix<-best_confusion_matrix
      print(final_confusion_matrix)
      model<-best_lm_D_R2
      best_model_desc<-model_desc
      best_position<-position
      best_corte<-corte
      best_prediccion_t<-y_D_R2_t
      best_prediccion_s<-y_D_R2
     }
  }
}
end_time <- Sys.time()
end_time-start_time

#grafica mejor modelo############################################################################################################################
#datos de entrenamiento
a<-data.frame(best_prediccion_t,DefaultReal=sampleDefaultR_tr$Default)
a<-a[order(a$best_prediccion_t, decreasing=FALSE),]
a$rank<-1:nrow(a)
dev.off()
ggplot(data=a, aes(x=rank, y=best_prediccion_t)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de la probabilidad de default")+ggtitle("Datos de entrenamiento")
ggsave("lm_D_R2Tt.pdf")

#datos de prueba
a<-data.frame(best_prediccion_s,DefaultReal=sampleDefaultR_ts$Default)
a<-a[order(a$best_prediccion_s, decreasing=FALSE),]
a$rank<-1:nrow(a)
ggplot(data=a, aes(x=rank, y=best_prediccion_s)) +geom_point(aes(color=DefaultReal), alpha=1, shape=4, stroke=2) +
  xlab("Registro") +ylab("Prediccion de  la probabilidad de default")+ggtitle("Datos de prueba")
ggsave("lm_D_R2Ts.pdf")
