

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))


#  training on each individual: cis
get.features.cis<-function(m.add, p.add, brks){
data.train.C<-list(0)
for(ii in 1:length(indC)){
  wh<-which(datlabC[,2]==indC[ii])
  if(length(wh)>0){
    xx<-datlabC[wh,]
    jj<-1
    tr<-read.csv(paste0("cis-pd.training_data/", xx[jj,1],".csv"))
    cnt<-hist(log10(pmax(abs(diff(tr[,2])),m.add)), breaks=brks, plot = F)
    br<-cnt$mids
    cnt<-cnt$counts
    cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
    cnt<-hist(log10(pmax(abs(diff(tr[,3])),m.add)), breaks=brks, plot = F)
    br<-cnt$mids
    cnt<-cnt$counts
    cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
    cnt<-hist(log10(pmax(abs(diff(tr[,4])),m.add)), breaks=brks, plot = F)
    br<-cnt$mids
    cnt<-cnt$counts
    cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
    data.prt<-matrix(0, ncol=length(cnt1)+length(cnt2)+length(cnt3), nrow=length(wh))
    data.prt[jj,]<-c(log10(cnt1), log10(cnt2), log10(cnt3))
    for(jj in 2:length(wh)){
      tr<-read.csv(paste0("cis-pd.training_data/", xx[jj,1],".csv"))
      cnt<-hist(log10(pmax(abs(diff(tr[,2])),m.add)), breaks=brks, plot = F)
      br<-cnt$mids
      cnt<-cnt$counts
      cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
      cnt<-hist(log10(pmax(abs(diff(tr[,3])),m.add)), breaks=brks, plot = F)
      br<-cnt$mids
      cnt<-cnt$counts
      cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
      cnt<-hist(log10(pmax(abs(diff(tr[,4])),m.add)), breaks=brks, plot = F)
      br<-cnt$mids
      cnt<-cnt$counts
      cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
      data.prt[jj,]<-c(log10(cnt1), log10(cnt2), log10(cnt3))
    }
    data.train.C[[ii]]<-data.prt
  } else{
    data.train.C[[ii]]<-"NA"
  }
}
return(data.train.C)
}


get.features.cis.all<-function(chall){
  lv<-which(colnames(datlabC)==chall)
ii<-1;
wh<-which(datlabC[,2]==indC[ii])
xx<-datlabC[wh,]
ind<-which(!(is.na(xx[,lv])))
data.prt<-data.train.C[[ii]]
train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
data.train.C.all<-train
for(ii in 2:length(data.train.C)){
  wh<-which(datlabC[,2]==indC[ii])
  xx<-datlabC[wh,]
  ind<-which(!(is.na(xx[,lv])))
  data.prt<-data.train.C[[ii]]
  train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
  data.train.C.all<-rbind(data.train.C.all, train)
}
return(data.train.C.all)
}

#  training on each individual: real
get.features.real<-function(m.add, p.add, brks.R1, brks.R2, brks.R3){
  data.train.R1<-list(0)
  data.train.R2<-list(0)
  data.train.R3<-list(0)
  for(ii in 1:length(indR)){
    wh<-which(datlabR[,2]==indR[ii])
    if(length(wh)>0){
      xx<-datlabR[wh,]
      data.prt.R1<-matrix(0, ncol=3*(length(brks.R1)-3), nrow=length(wh))
      data.prt.R2<-matrix(0, ncol=3*(length(brks.R2)-3), nrow=length(wh))
      data.prt.R3<-matrix(0, ncol=3*(length(brks.R3)-3), nrow=length(wh))
      for(jj in 1:length(wh)){
        if(paste0(xx[jj,1],".csv") %in% lfR1) {
          tr<-read.csv(paste0("real-pd.training_data_updated/smartphone_accelerometer/", xx[jj,1],".csv"))
          cnt<-hist(log10(pmax(abs(diff(tr[,2])),m.add)), breaks=brks.R1, plot = F)
          cnt<-cnt$counts
          cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          cnt<-hist(log10(pmax(abs(diff(tr[,3])),m.add)), breaks=brks.R1, plot = F)
          cnt<-cnt$counts
          cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          cnt<-hist(log10(pmax(abs(diff(tr[,4])), m.add)), breaks=brks.R1, plot = F)
          cnt<-cnt$counts
          cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          data.prt.R1[jj,]<-c(log10(cnt1), log10(cnt2), log10(cnt3))
        }  else {
          data.prt.R1[jj,]<--NA
        }
        if(paste0(xx[jj,1],".csv") %in% lfR2) {
          tr<-read.csv(paste0("real-pd.training_data_updated/smartwatch_accelerometer/", xx[jj,1],".csv"))
          cnt<-hist(log10(pmax(abs(diff(tr[,3])),m.add)), breaks=brks.R2, plot = F)
          cnt<-cnt$counts
          cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          cnt<-hist(log10(pmax(abs(diff(tr[,4])),m.add)), breaks=brks.R2, plot = F)
          cnt<-cnt$counts
          cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          cnt<-hist(log10(pmax(abs(diff(tr[,5])),m.add)), breaks=brks.R2, plot = F)
          cnt<-cnt$counts
          cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          data.prt.R2[jj,]<-c(log10(cnt1), log10(cnt2), log10(cnt3))
        }  else {
          data.prt.R2[jj,]<-NA
        }
        if(paste0(xx[jj,1],".csv") %in% lfR3) {
          tr<-read.csv(paste0("real-pd.training_data_updated/smartwatch_gyroscope/", xx[jj,1],".csv"))
         cnt<-hist(log10(pmax(abs(diff(tr[,3])),m.add)), breaks=brks.R3, plot = F)
          cnt<-cnt$counts
          cnt1<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
           cnt<-hist(log10(pmax(abs(diff(tr[,4])),m.add)), breaks=brks.R3, plot = F)
          cnt<-cnt$counts
          cnt2<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          cnt<-hist(log10(pmax(abs(diff(tr[,5])),m.add)), breaks=brks.R3, plot = F)
          cnt<-cnt$counts
          cnt3<-sapply(2:(length(cnt)-1), function(a) sum(cnt) - sum(cnt[1:(a-1)]))/sum(cnt)+p.add
          plot(log10(cnt3), type='l')
          data.prt.R3[jj,]<-c(log10(cnt1), log10(cnt2), log10(cnt3))
        }  else {
          data.prt.R3[jj,]<-NA
        }
      }
      data.train.R1[[ii]]<-data.prt.R1
      data.train.R2[[ii]]<-data.prt.R2
      data.train.R3[[ii]]<-data.prt.R3
     } 
  }
  return(list(data.train.R1, data.train.R2, data.train.R3))
}

get.features.real.all<-function(chall){
  lv<-which(colnames(datlabC)==chall)
  ii<-1;
  wh<-which(datlabR[,2]==indR[ii])
  xx<-datlabR[wh,]
  ind<-which(!(is.na(xx[,lv])))
  data.prt<-data.train.R1[[ii]]
  train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
  data.train.R1.all<-train
  for(ii in 2:length(data.train.R1)){
    wh<-which(datlabR[,2]==indR[ii])
    xx<-datlabR[wh,]
    ind<-which(!(is.na(xx[,lv])))
    data.prt<-data.train.R1[[ii]]
    train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
    data.train.R1.all<-rbind(data.train.R1.all, train)
  }
  ii<-1;
  wh<-which(datlabR[,2]==indR[ii])
  xx<-datlabR[wh,]
  ind<-which(!(is.na(xx[,lv])))
  data.prt<-data.train.R2[[ii]]
  train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
  data.train.R2.all<-train
  for(ii in 2:length(data.train.R2)){
    wh<-which(datlabR[,2]==indR[ii])
    xx<-datlabR[wh,]
    ind<-which(!(is.na(xx[,lv])))
    data.prt<-data.train.R2[[ii]]
    train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
    data.train.R2.all<-rbind(data.train.R2.all, train)
  }
  ii<-1;
  wh<-which(datlabR[,2]==indR[ii])
  xx<-datlabR[wh,]
  ind<-which(!(is.na(xx[,lv])))
  data.prt<-data.train.R3[[ii]]
  train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
  data.train.R3.all<-train
  for(ii in 2:length(data.train.R3)){
    wh<-which(datlabR[,2]==indR[ii])
    xx<-datlabR[wh,]
    ind<-which(!(is.na(xx[,lv])))
    data.prt<-data.train.R3[[ii]]
    train<-cbind( as.data.frame(data.prt[ind,]), label=xx[ind,lv])
    data.train.R3.all<-rbind(data.train.R3.all, train)
  }
 return(list(data.train.R1.all, data.train.R2.all, data.train.R3.all)) 
}


