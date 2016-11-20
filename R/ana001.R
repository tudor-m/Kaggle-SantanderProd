# DAta analysis
library(data.table)

# input files:
trf = "../data/train_ver2.csv"
tsf = "../data/test_ver2.csv"

# read the feature names:
intrn = names(fread(trf,nrows=0)) # train data names
intsn = names(fread(tsf,nrows=0)) # train data names

# count the unique values:
print("Train data:")
dta = fread(trf,nrows=-1)
for (i in names(dta))
{
  u = length(unique(dta[[i]]))
  print(c(i,u))
  intrd[,.N,by=i]
}

rm(dta);gc()
print("Test data:")
dta = fread(tsf,nrows=-1)
for (i in names(dta))
{
  u = length(unique(dta[[i]]))
  print(c(i,u))
  intrd[,.N,by=i]
}

rm(dta);gc()
