# Quick and dirty solution
library(data.table)
library(xgboost)
source("futil.R")

# input files:
trf = "../data/train_ver2.csv"
numr = 13647309
tsf = "../data/test_ver2.csv"
intrn = names(fread(trf,nrows=0)) # train data features
response_features = c("ind_ahor_fin_ult1","ind_aval_fin_ult1","ind_cco_fin_ult1",
                  "ind_cder_fin_ult1","ind_cno_fin_ult1","ind_ctju_fin_ult1",
                  "ind_ctma_fin_ult1","ind_ecue_fin_ult1","ind_fond_fin_ult1",
                  "ind_hip_fin_ult1","ind_plan_fin_ult1","ind_pres_fin_ult1",
                  "ind_reca_fin_ult1","ind_tjcr_fin_ult1","ind_valo_fin_ult1",
                  "ind_viv_fin_ult1","ind_nomina_ult1","ind_nom_pens_ult1","ind_recibo_ult1")
selected_features = c("pais_residencia","sexo","age","antiguedad","canal_entrada", "cod_prov","renta" , "segmento")           
factor_features   = c("pais_residencia","sexo","canal_entrada","segmento")

# Read train data:
intrd = fread(trf,select = c(selected_features,response_features), nrows=-1)

# Repeat a few times:   SKIP
# extract a random sample for dev (10%)
# chose a random sammpe for ensembling (10%) SKIP
# chose a random sample for CV
# use everything else for CV SKIP
set.seed(100)
selected_rows = sample(1:numr,3*0.1*numr)

devd = intrd[selected_rows[1:(0.1*numr)],]
ensd = intrd[selected_rows[(0.1*numr+1):(2*0.1*numr)],]
cvd  = intrd[selected_rows[(0.1*numr+2):(3*0.1*numr)],]
remove(intrd);gc()
# factor as.mumeric:
for (j in factor_features)
{
 devd[[j]] = as.numeric(as.factor(devd[[j]]))
 ensd[[j]] = as.numeric(as.factor(ensd[[j]]))
 cvd[[j]] = as.numeric(as.factor(cvd[[j]]))
}
for (j in response_features)
{
  print(j)
  v=devd[[j]]
  print(unique(v))
  print(c(sum(v==0,na.rm=T),sum(v==1,na.rm=T),sum(is.na(v))))
  v=ensd[[j]]
  print(unique(v))
  print(c(sum(v==0,na.rm=T),sum(v==1,na.rm=T),sum(is.na(v))))
  v=cvd[[j]]
  print(unique(v))
  print(c(sum(v==0,na.rm=T),sum(v==1,na.rm=T),sum(is.na(v))))
}
fit.dev.xgb.model = list()
pred.dev.xgb = list()
pred.cv.xgb = list()
for (iresp in 1:length(response_features))
{
  print(c("iresp=",iresp))
  nnav = which(!is.na(devd[[response_features[iresp]]])) # keep only the nonNA
  dtrain.label = devd[[response_features[iresp]]][nnav]
  dtrain.data = as.matrix(devd[nnav,-response_features,with=F])
  dtrain <- xgb.DMatrix(data = dtrain.data, label = dtrain.label, missing = NA)
  dtest.data = as.matrix(cvd[,-response_features,with=F])
  dtest.label = cvd[[response_features[iresp]]]
  dtest  <- xgb.DMatrix(data = dtest.data, label = dtest.label, missing = NA)

# Fit train data xgb
for (thr in seq(0.15,0.15,0.15))
  for (i in 1:1)
  {
    watchlist <- list(train = dtrain, test = dtest)
    mccEval <- function(preds, dtrain)
    {
      labels = getinfo(dtrain, "label")
      err = as.numeric(errMeasure4(preds,labels,thr))
      return(list(metric="error",value=err))
    }
    for (min_child_w in seq(5,5,1)) {
      for (max_d in seq(4,4,1)) {
        print(c("max_d: ",max_d))
        print(c("min_child_weight: ",min_child_w))
        print(thr)
        nround = 10
        param <- list(  
          #objective           = "multi:softprob", num_class = 4,
          objective           = "binary:logistic",
          #objective           = "reg:linear",
          booster             = "gbtree",
          #booster             = "gblinear",
          base_score          = 0.5,
          eta                 = 0.5,#0.05, #0.02, # 0.06, #0.01,
          max_depth           = max_d, #changed from default of 8
          subsample           = 0.5, #0.9, # 0.7
          colsample_bytree    = 0.5, # 0.7
          #num_parallel_tree   = 2,
          nthread = 2,
          alpha = 0,    #0.0001,
          lambda = 0,
          gamma = 0,
          scale_pos_weight = 1,
          min_child_weight    = min_child_w, #4, #4
          eval_metric         = mccEval,
          #eval_metric         = "rmse",
          early_stopping_rounds    = 2,
          maximize = TRUE
        )
        set.seed(100)
        fit.dev = xgb.train(params=param,dtrain,nrounds=nround,print.every.n = 2,maximize = FALSE,watchlist)
      }
    }
    # Model array:
    fit.dev.xgb.model[[iresp]] = fit.dev
    pred.dev.xgb.pred[[iresp]] = predict(fit.dev,dtrain)
    pred.cv.xgb.pred[[iresp]] = predict(fit.dev,dtest)
  }
}
pred = predict(fit.dev,dtrain)




intsn = names(fread(tsf,nrows=0)) # train data names
