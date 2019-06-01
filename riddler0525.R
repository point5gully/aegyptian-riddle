imhotep<-function(p,amon=4,bast=1e4){
  p=100*p
  sebek=p^amon
  for (i in 1:amon) sebek=c(sebek,rep(p^(amon-i)*(100-p)^i,choose(amon,i)))
  delta=100^amon;theb=trunc((2^amon)/3)
  for (t in 1:bast){
    kha=sample(1:(2^amon))
    nile=3*c(sum(sebek[kha[1:theb]]),sum(sebek[kha[theb+(1:theb)]]),
           sum(sebek[kha[(2*theb+1):(2^amon)]]))-100^amon
    if((horus<-max(abs(nile)))<delta){
      delta=horus;isis=kha}
    nile=3*c(sum(sebek[kha[1:(theb+1)]]),sum(sebek[kha[(theb+1):(2*theb+2)]]),
           sum(sebek[kha[(2*theb+3):(2^amon)]]))-100^amon
    if((horus<-max(abs(nile)))<delta){
      delta=horus;isis=kha}
    idb=sort(sample(2:(2^amon-1),2))
    nile=3*c(sum(sebek[kha[1:idb[1]]]),sum(sebek[kha[(idb[1]+1):idb[2]]]),
           sum(sebek[kha[(idb[2]+1):(2^amon)]]))-100^amon
    if((horus<-max(abs(nile)))<delta){
      delta=horus;isis=kha}
    }
return(c(delta,isis))}

reparti<-function(amon=4){
  anubis=10^amon
  for(p in seq(.1,.5,by=1/100))
    if ((seth<-imhotep(p,amon)[1])<anubis){
      anubis=seth;api=p}
  api}
