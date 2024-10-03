source("packages.R")
source("functions.R")
source("data-preparation.R")


# my
slidewin <<-10
datamm <<-read.csv("HBEA_Input.csv", header=T, row.names = 1)
val.length=10


######Input######

data.train=train.reg
data.test=test.reg
n=nrow(data.train)
n1=nrow(data.test) 

targettest <- data.test[(slidewin+1):(slidewin+n1), "target", drop = FALSE] 

# data.val=data.train[((n-val.length+1):n), ]  
data.train.n=data.train[(1:(n-val.length)), ]
formula=target~.

predictions.table=read.csv("HBEA_Input.csv", header=T, row.names = 1)
predictions.table=predictions.table[(slidewin+1):(slidewin+n1),]

sapply(1:ncol(predictions.table), function(x) rmse(predictions.table[,1],predictions.table[,x]))



#######The first stage model selection

tp1=14 #number of top models  
lim=0.05

#Once drift alarm, begin model selection
updated_selection=topk.model.sel(models,data.train, data.test,val.length, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"),lim,tp1)
#identify models
alarm0=updated_selection$alarm 
updated_selection1=updated_selection$models.sel 
#identify instant where the drift alarm was triggered


##### DDE method

st=Sys.time()
H=10  #vallen
val.length1=10 #max clusters (predictionss of the models)
tp.cl=compute.cluster.imlec(models,data.train,data.test,val.length1,H, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"),updated_selection)
ed=Sys.time()
pred.tp.imlec=tp.cl$predictions
pos.models=tp.cl$selected_models_id

rmse(data.test$target,pred.tp.imlec)


###################################Save DDE###################################
# save alarm
alarm1 <- data.frame(Value = alarm0)
write.xlsx(alarm1, file = "alarm(valength=10,H=10).xlsx")
# save first stage model selection result
newposmodels0 <- matrix(NA, nrow = n1, ncol = 15)  # null
for (i in 1:n1) {
  len <- length(updated_selection1[[i]])
  if (len >= 15) {
    newposmodels0[i,] <- updated_selection1[[i]][1:15]  
  } else {
    newposmodels0[i,1:len] <- updated_selection1[[i]]  
  }
}
# save second satge model selection result
newposmodels <- matrix(NA, nrow = n1, ncol = 6)  
for (i in 1:n1) {
  len <- length(pos.models[[i]])
  if (len >= 6) {
    newposmodels[i,] <- pos.models[[i]][1:6]  
  } else {
    newposmodels[i,1:len] <- pos.models[[i]]  
  }
}

wb <- createWorkbook()
addWorksheet(wb, sheetName = "pre_selection")
writeData(wb, sheet = "pre_selection", x = newposmodels0)
addWorksheet(wb, sheetName = "second_selection")
writeData(wb, sheet = "second_selection", x = newposmodels)
saveWorkbook(wb, "selected_models(valength=10,H=10).xlsx")

pred.tp.imlec <- data.frame(Value = pred.tp.imlec)
write.xlsx(pred.tp.imlec, file = "pred.tp.imlec(valength=10,H=10).xlsx")


###################################Variant model###################################

H=10# size of the sliding window for computing ensembles candidates error (see the paper)

# VM1: Sliding window ensemble of all models in the pool. No selection has made for the candidate models.
pred.ens.all.sw=sapply(1:nrow(data.test),function(t)ens.step.all(data.test,predictions.table,t,H))

# VM2: Sliding window ensemble of top models. No clustering is used afterward.
pred.top=ens.top.pred(data.test,predictions.table,updated_selection1,H)

# VM3: Sliding window ensemble of all models clustering using IMLE. No top candidate model selection is executed.
pred.ens.all=sapply(1:nrow(data.test),function(t)ens.avg.all(data.test,predictions.table,t))


rmse(data.test$target,pred.top)
rmse(data.test$target,pred.ens.all)
rmse(data.test$target,pred.ens.all.sw)


# VM4: Sliding window ensemble of top models clustering using k-means. The clustering method is replaced with k-means with Euclidean distance, and the clustering number 饾憳 is determined via the average silhouette method.
val.length1=10
cluster.res=update.cluster.kmeans(models,data.train, data.test,val.length1, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"),updated_selection,tp1)
pos.cl=lapply(1:length(cluster.res) ,function(x) if(length(cluster.res[[x]]$list.position)>=4){
  cluster.res[[x]]$list.position[1:4]
}else{c(cluster.res[[x]]$list.position[1],rep(cluster.res[[x]]$list.position[2],3))})

pred.cl.kmeans=ens.top.pred(data.test,predictions.table,pos.cl,H)
rmse(data.test$target,pred.cl.kmeans)

# VM3: Sliding window ensemble of all models clustering using IMLE. No top candidate model selection is executed.
val.length1=14  # Max cluster
pred.cl.or=compute.cluster.or.all(models,data.train, data.test,val.length1,H, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"))
rmse(data.test$target,pred.cl.or)

# VM5: Sliding window ensemble of top models clustering using DWT. The clustering method is replaced with dynamic time warping clustering.
val.length1=10
pred.tw.tp=compute.cluster.tw.tp.all(models,data.train, data.test,t,val.length1, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"),updated_selection)
rmse(data.test$target,pred.tw.tp)

#VM6:  Stacking ensemble of top models. The ensemble method is replaced with stacking rather than SWA ensemble
val.length1=10 #mete learning
pos.models=tp.cl[[2]]#evaluation
st=Sys.time()
pred.st.cl=compute.cl.tp.imlec.stac(models,data.train, data.test,t,val.length1, formula, per.arima,ker=c("rbfdot" ,"polydot" ,"vanilladot", "laplacedot"),pos.models,updated_selection)
ed=Sys.time()

sapply(1:11, function(x) rmse(data.test$target,pred.st.cl[[x]])) 
pred.top.imlec.st=pred.st.cl[[4]] 
rmse(data.test$target,pred.top.imlec.st)

## SAVE
pred.st.cl0 <- matrix(0, nrow = 11, ncol = n1)
for (i in 1:11) {
  pred.st.cl0[i,] <- pred.st.cl[[i]]
}
pred.st.cl0 <- data.frame(Value = t(pred.st.cl0))
colnames(pred.st.cl0) <- c("RandomForest", "Cubist", "GLMNet","ksvm", "mars", "ppr", "monmlp.fit", "gausspr", "plsr", "dt", "mean")
write.xlsx(pred.st.cl0, "pred.st.cl(valength=10,H=10).xlsx",colNames = TRUE)


###########################################ALL result##########################################
data_list <- list(pred.top, pred.ens.all.sw, pred.ens.all,pred.cl.kmeans,pred.cl.or,pred.tw.tp)
combined_matrix <- do.call(cbind, data_list)
combined_matrix <- data.frame(Value = combined_matrix)
colnames(combined_matrix) <- c("pred.top", "pred.ens.all.sw", "pred.ens.all","pred.cl.kmeans","pred.cl.or","pred.tw.tp")
write.xlsx(combined_matrix, "method_variants(valength=10,H=10).xlsx",colNames = TRUE)


