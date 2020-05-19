



# Which study
get_study<-function(meas){
  wh<-which(idsC[,1]==test[ii,1])
  if(length(wh)>0){
    study<-1
    prs<-idsC[wh,2]
  } else {
    study<-2
    wh<-which(idsR[,1]==test[ii,1])
    if(length(wh)>0){
      prs<-idsR[wh,2]
    } else {
      study<-0
      prs<-"NA"
    }
  }
  return(list(meas, as.numeric(study), prs))
}

get_ft_cis_pd<-function(tr, m.add, p.add, brks){
  cnt<-hist(log10(abs(diff(tr[,2]))+m.add), breaks=brks, plot = F)
  cnt<-cnt$counts
  cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,3]))+m.add), breaks=brks, plot = F)
  cnt<-cnt$counts
  cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,4]))+m.add), breaks=brks, plot = F)
  cnt<-cnt$counts
  cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  return(c(log10(cnt1), log10(cnt2), log10(cnt3)))
}

get_ft_real_pd1<-function(tr, m.add, p.add, brks.R1){
  cnt<-hist(log10(abs(diff(tr[,2]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,3]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,4]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  return(c(log10(cnt1), log10(cnt2), log10(cnt3)))
}

get_ft_real_pd2<-function(tr, m.add, p.add, brks.R1){
  cnt<-hist(log10(abs(diff(tr[,3]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,4]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  cnt<-hist(log10(abs(diff(tr[,5]))+m.add), breaks=brks.R1, plot = F)
  cnt<-cnt$counts
  cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
  return(c(log10(cnt1), log10(cnt2), log10(cnt3)))
}


predict_cis_pd<-function(info, chall, m.add, p.add, brks){
  lv<-which(colnames(datlabC)==chall)
  n.trees<-5
  interaction.depth<-1
  tst<-read.csv(paste0("cis-pd.testing_data/", info[[1]],".csv"))
  fts<-get_ft_cis_pd(tst,m.add, p.add, brks)
  test<-matrix(0, ncol=length(fts), nrow=1)
  test[1,]<-fts
  prs<-which(indC==info[[3]])
  data.prt<-data.train.C[[prs]]
  wh<-which(datlabC[,2]==info[[3]])
  xx<-datlabC[wh,]
  ind<-which(!(is.na(xx[,lv])))
  if(length( ind)>0){
    train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
    fit.model <- gbm(
      formula = label ~ .,
      data = train,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    pred<-pmin(pmax(predict(fit.model, as.data.frame(test), n.trees=n.trees, type='response'),0),4)
  }  else {
    fit.model <- gbm(
      formula = label ~ .,
      data = data.train.C.all,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    pred<-pmin(pmax(predict(fit.model, as.data.frame(test), n.trees=n.trees, type='response'),0),4)
  }
  return(pred)
}

predict_real_pd<-function(info, chall, m.add, p.add, brks.R1, brks.R2, brks.R3){
  lv<-which(colnames(datlabC)==chall)
  n.trees<-10
  interaction.depth<-3
  tst.R1<-matrix(-1000, ncol=3*(length(brks.R1)-3), nrow=1)
  tst.R2<-matrix(-1000, ncol=3*(length(brks.R2)-3), nrow=1)
  tst.R3<-matrix(-1000, ncol=3*(length(brks.R3)-3), nrow=1)
  
  if(paste0(info[[1]],".csv") %in% list.files(path="real-pd.testing_data_updated/smartphone_accelerometer/" )){  
    tst1<-read.csv(paste0("real-pd.testing_data_updated/smartphone_accelerometer/", info[[1]],".csv")) 
    tst.R1[1,]<-as.numeric(get_ft_real_pd1(tst1,  m.add, p.add, brks.R1))}
  if(paste0(info[[1]],".csv") %in% list.files(path="real-pd.testing_data_updated/smartwatch_accelerometer/")){   
    tst2<-read.csv(paste0("real-pd.testing_data_updated/smartwatch_accelerometer/", info[[1]],".csv"))
    tst.R2[1,]<-as.numeric(get_ft_real_pd2(tst2,  m.add, p.add, brks.R2))}
  if(paste0(info[[1]],".csv") %in% list.files(path="real-pd.testing_data_updated/smartwatch_gyroscope/")){  
    tst3<-read.csv(paste0("real-pd.testing_data_updated/smartwatch_gyroscope/", info[[1]],".csv"))
    tst.R3[1,]<-as.numeric(get_ft_real_pd2(tst3,  m.add, p.add, brks.R3))}
  test<-matrix(0, ncol=3*(length(brks.R1)-3)+3*(length(brks.R2)-3)+3*(length(brks.R3)-3), nrow=1)
  test[1,]<-c(tst.R1, tst.R2, tst.R3)
  test<- as.data.frame(test)
  prs<-which(indR==info[[3]])
  data.prt1<- data.train.R1[[prs]]
  data.prt2<- data.train.R2[[prs]]
  data.prt3<- data.train.R3[[prs]]
  wh<-which(datlabR[,2]==info[[3]])
  xx<-datlabR[wh,]
  ind<-which(!(is.na(xx[,lv])))
  if(length( ind)>0){
    train<-cbind(as.data.frame(cbind(data.prt1[ind,], data.prt2[ind,], data.prt3[ind,])), label=xx[ind,lv])
    fit.model <- gbm(
      formula = label ~ .,
      data = train,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    pred<-pmin(pmax(predict(fit.model, as.data.frame(test), n.trees=n.trees, type='response'),0),4)
  } else {
    fit.model.R1 <- gbm(
      formula = label ~ .,
      data = data.train.R1.all,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    fit.model.R2 <- gbm(
      formula = label ~ .,
      data = data.train.R2.all,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    fit.model.R3 <- gbm(
      formula = label ~ .,
      data = data.train.R3.all,
      distribution = "gaussian",
      n.trees = n.trees,
      interaction.depth = interaction.depth,
      n.minobsinnode = 3,
      verbose = FALSE
    )  
    pred<-pmin(pmax(mean(c(predict(fit.model.R1, as.data.frame(tst.R1), n.trees=n.trees, type='response'), predict(fit.model.R2, as.data.frame(tst.R2), n.trees=n.trees, type='response'), predict(fit.model.R3, as.data.frame(tst.R3), n.trees=n.trees, type='response'))),0),4)
  }
  return(pred)
}

