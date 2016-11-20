library(data.table)
nrtr = 10
nrts = 10
intrd = fread("../data/train_ver2.csv",nrows=nrtr) # inTestData
intsd = fread("../data/test_ver2.csv",nrows=nrts) # inTrainData

