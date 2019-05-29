khemet<-function(seth=11){
  horus=c(2,3,6)
  osiris=NULL
  for (i in 1:1e7){
    api=c(1,2,3,6)
    while (length(api)<seth){
      ra=sample(1:length(api),1,prob=1/api)
      beh=ifelse(runif(1)<1/2,isis<-api[ra]*horus,isis<-(api[ra]+1)*c(1,api[ra]))
      api=c(api[-ra],isis)
    }
    if((length(api)==seth)&(sum(duplicated(api))==0)){
     if(is.matrix(osiris)){
       if(min(apply(abs(api-t(osiris)),2,max))==0) api=NULL}
        osiris=rbind(osiris,sort(api))}
  }
  osiris[!duplicated.matrix(osiris,1),]}
