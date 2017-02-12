library(rootSolve)
library(ggplot2)
library(reshape2)
library(pushoverr)

source('~/Concordia/Research/Git/Djehiche_Giesecke/functions - Bistability.R')

set.seed(1234)
x0<-0.75
#lambda_0<-5
#lambda_1<-50
varepsilon_vec<-0.005
Omega_a<-0.5
Omega_b<-1
mark_set<-c(-1,1)
low_t<-0
T_vec<-low_t+80
parameters<-expand.grid(varepsilon_vec,T_vec)
low_t<-0
N_steps<-2000
sample_size<-10^3
sys_cap<-100
gamma_tilde<-1

#####################################################
# Program #
###########

# prueba<-chain(x0 = x0,t0 = low_t,T_ = parameters[1,2],epsilon = parameters[1,1],Nsteps = N_steps,c_=0,new = FALSE)#,i = 0,A = 0,E_n=rexp(n = 1,rate = 1))
# qplot(x = prueba$s, y = prueba$Z)



for(k in 1:length(parameters[,1])){
  time_trials<-indicators<-prom<-c()
  for(j in 1:sample_size){
    if(x0>0.5){
      Omega_a<-0.5
      Omega_b<-1
    }
    else{
      Omega_a<-0
      Omega_b<-0.5
    }
    ZandL<-chain(x0 = x0,t0 = low_t,T_ = parameters[k,2],epsilon = parameters[k,1],Nsteps = N_steps,c_=0,new = FALSE)
    indicators<-c(indicators,ZandL$s[length(ZandL$s)]-low_t)
    print(c(k,j))
    x0<-ZandL$Z
    low_t<-ZandL$s[length(ZandL$s)]
  }
  #estim<-c(mean(indicators),(1/sample_size)*sqrt((1/mean(indicators))-1))
  #write.table(x=c(date(),"## BEFORE ##",sample_size,N_steps,parameters[k,],estim),file=paste("C:/Users/User/Google Drive/Research/Code/[Djehiche+Giesecke] Epsilon Jump Diffusion v",vers,".txt"), append=TRUE,row.names = F, col.names = F, quote=F, sep="//")
}
pushover(message = 'hieeee')
# time_used.old<-proc.time()-start_time.old

###############################################################################################################  
# Intermediate #
################

c_vec<-rep(0,length(parameters[,2]))
for(k in 1:length(parameters[,2])){
  c_vec[k]<-optimize(min_max,interval =c(c_H(),100000),maximum =TRUE ,cap_t=parameters[k,2], low_t=low_t,a=Omega_a,b=Omega_b)$maximum
}

###############################################################################################################  
# After that #
##############

for(k in 1:length(parameters[,1])){
  time_trials<-indicators<-prom<-c()
  #c_<-c_vec[k]
  for(j in 1:sample_size){
    ZandL<-chain(x0 = x0,t0 = low_t,T_ = parameters[k,2],epsilon = parameters[k,1],Nsteps = N_steps,c_=c_vec[k],new = TRUE)    #prom<-c(prom,exp(-(1/parameters[k,1])*g_function(ZandL$Z[length(ZandL$Z)])))
    indicators<-c(indicators,1*(ZandL$s[length(ZandL$s)]<parameters[k,2])*(ZandL$L[length(ZandL$L)])^-1)
    print(c(k,j))
  }
  estim<-c(mean(indicators),(1/sample_size)*sqrt((1/mean(indicators))-1))
  write.table(x=c(date(),"## AFTER ##",sample_size,N_steps,parameters[k,],estim),file=paste("C:/Users/User/Google Drive/Research/Code/[Djehiche+Giesecke] Epsilon Jump Diffusion v",vers,".txt"), append=TRUE,row.names = F, col.names = F, quote=F, sep="//")
}
#pushover(message="Your shit is fresh")
