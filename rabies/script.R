require(skygrowth)
require(ape)

tree=read.tree('rabies.nwk')
MST=2005
covar=as.matrix(read.table('rabies.tab'))

colnames(covar)<-c('time','var')
covar=as.data.frame(covar)
covar$var=scale(covar$var)

useCovar=T
if (useCovar) 
  res=skygrowth.mcmc.covar(tree,~var,covar,maxSampleTime=MST,res=100,tau0=10,iter=1e8,control=list(thin=1e5)) else 
  res=skygrowth.mcmc(tree,tau0=10,res=100,mhsteps=1e8,control=list(thin=1e5))
