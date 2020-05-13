######################################################################
### BEAT-PD DREAM Challenge
### Renata Retkute, 2020
######################################################################


library(gbm)

source("train.R")
source("infer.R")


######################################################################
#  Upload data
######################################################################

#  individuals
idsC<-read.csv("cis-pd.CIS-PD_Test_Data_IDs.csv", colClasses="character")
indC<-unique(idsC[,2])

idsR<-read.csv("real-pd.REAL-PD_Test_Data_IDs.csv", colClasses="character")
indR<-unique(idsR[,2])

idsCA<-read.csv("data_labels/CIS-PD_Ancillary_Data_IDs_Labels.csv", colClasses="character")
indCA<-unique(idsCA[,2])

idsRA<-read.csv("data_labels/REAL-PD_Ancillary_Data_IDs_Labels.csv", colClasses="character")
indRA<-unique(idsRA[,2])


# data labels
datlabC<-read.csv("data_labels/CIS-PD_Training_Data_IDs_Labels.csv", colClasses="character")
datlabC[,3]<-as.numeric(datlabC[,3]); datlabC[,4]<-as.numeric(datlabC[,4]); datlabC[,5]<-as.numeric(datlabC[,5])

datlabR<-read.csv("data_labels/REAL-PD_Training_Data_IDs_Labels.csv", colClasses="character")
datlabR[,3]<-as.numeric(datlabR[,3]); datlabR[,4]<-as.numeric(datlabR[,4]); datlabR[,5]<-as.numeric(datlabR[,5])

lfC<-list.files(path="cis-pd.training_data")
lfR1<-list.files(path="real-pd.training_data_updated/smartphone_accelerometer")
lfR2<-list.files(path="real-pd.training_data_updated/smartwatch_accelerometer")
lfR3<-list.files(path="real-pd.training_data_updated/smartwatch_gyroscope")

######################################################################
#  Train model on data
######################################################################

m.add<-0.1
p.add<-10^(-5)

brks<-c(seq(log10(m.add), 1,0.01),3)
data.train.C <- get.features.cis(m.add, p.add, brks)

brks.R1 <- c(seq(log10(m.add), 1,0.01),3); 
brks.R2 <- c(seq(log10(m.add), 2,0.01),3); 
brks.R3 <- c(seq(log10(m.add), 1,0.01),3); 

data.train.R <- get.features.real(m.add, p.add, brks.R1, brks.R2, brks.R3)
data.train.R1<-data.train.R[[1]]
data.train.R2<-data.train.R[[2]]
data.train.R3<-data.train.R[[3]]


######################################################################
#  Make predictions 
######################################################################

#  Predictions: challenge 1
chall<-"on_off"
data.train.C.all<-get.features.cis.all(chall)
data.train.R.all <- get.features.real.all(chall)
data.train.R1.all<-data.train.R.all[[1]]
data.train.R2.all<-data.train.R.all[[2]]
data.train.R3.all<-data.train.R.all[[3]]

test<-read.csv("BEAT-PD_SC1_OnOff_Submission_Template.csv", header=T, colClasses="character")

for(ii in 1:nrow(test)){
  info<-get_study(test[ii,1])
  
  if(info[[2]]==1){
    pred<-predict_cis_pd(info, chall, m.add, p.add, brks)
    test[ii,2]<-pred
  }
  if(info[[2]]==2){
    pred<-predict_real_pd(info, chall, m.add, p.add, brks.R1, brks.R2, brks.R3)
    test[ii,2]<-pred
    cat(c(ii, "", info[[3]], pred, "\n"))
  }
}

write.csv(test,"BEAT-PD_SC1_OnOff_Submission_Template_final.csv", row.names = FALSE)

#  Predictions: challenge 2
chall<-"dyskinesia"
data.train.C.all<-get.features.cis.all(chall)
data.train.R.all <- get.features.real.all(chall)
data.train.R1.all<-data.train.R.all[[1]]
data.train.R2.all<-data.train.R.all[[2]]
data.train.R3.all<-data.train.R.all[[3]]

test<-read.csv("BEAT-PD_SC2_Dyskinesia_Submission_Template.csv", header=T, colClasses="character")

for(ii in 1:nrow(test)){
  info<-get_study(test[ii,1])
  
  if(info[[2]]==1){
    pred<-predict_cis_pd(info, chall, m.add, p.add, brks)
    test[ii,2]<-pred
  }
  if(info[[2]]==2){
    pred<-predict_real_pd(info, chall, m.add, p.add, brks.R1, brks.R2, brks.R3)
    test[ii,2]<-pred
    cat(c(ii, "", info[[3]], pred, "\n"))
  }
}

write.csv(test,"BEAT-PD_SC2_Dyskinesia_Submission_Template_final.csv", row.names = FALSE)

#  Predictions: challenge 3
chall<-"tremor"
data.train.C.all<-get.features.cis.all(chall)
data.train.R.all <- get.features.real.all(chall)
data.train.R1.all<-data.train.R.all[[1]]
data.train.R2.all<-data.train.R.all[[2]]
data.train.R3.all<-data.train.R.all[[3]]

test<-read.csv("BEAT-PD_SC3_Tremor_Submission_Template.csv", header=T, colClasses="character")

for(ii in 1:nrow(test)){
  info<-get_study(test[ii,1])
  
  if(info[[2]]==1){
    pred<-predict_cis_pd(info, chall, m.add, p.add, brks)
    test[ii,2]<-pred
  }
  if(info[[2]]==2){
    pred<-predict_real_pd(info, chall, m.add, p.add, brks.R1, brks.R2, brks.R3)
    test[ii,2]<-pred
    cat(c(ii, "", info[[3]], pred, "\n"))
  }
}

write.csv(test,"BEAT-PD_SC3_Tremor_Submission_Template_final.csv", row.names = FALSE)
