library(rootSolve)
library(ggplot2)
library(reshape2)
library(pushoverr)

source('~/Concordia/Research/Git/Djehiche_Giesecke/functions - Copy.R')

set.seed(1234)
x0<-0.75
varepsilon_vec<-c(0.004)
Omega_a<-0.5
Omega_b<-1
mark_set<-c(-1,1)
T_vec<-80
low_t<-0
N_steps<-2000
h<-(T_vec-low_t)/N_steps
sample_size<-207
sys_cap<-100
gamma_tilde<-1

#####################################################
# Program #
###########

# prueba<-chain(Z = x0,s = low_t,A = 0,E_n = rexp(n = 1,rate = 1),i = 0,L = 1,T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
# qplot(x = prueba$s,y = prueba$Z)

initial<-data.frame(Z=x0,s=low_t,A=0,E_n=rexp(n = 1,rate = 1),i = 0,L = 1)
time<-low_t
big_chain_Z<-big_chain_s<-indicators<-c()


while(time<T_vec){
  small_chain<-chain(Z = initial$Z,s = initial$s,A = initial$A,E_n = initial$E_n,i = initial$i,L = initial$L,T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
  big_chain_Z<-c(big_chain_Z,small_chain$Z[-length(small_chain$Z)])
  big_chain_s<-c(big_chain_s,small_chain$s[-length(small_chain$s)])
  indicators<-c(indicators,small_chain$s[length(small_chain$s)]-small_chain$s[1])
  initial<-data.frame(Z =  small_chain$Z[length(small_chain$Z)],s =  small_chain$s[length(small_chain$s)],A =  small_chain$A[length(small_chain$A)],E_n =  small_chain$E_n[length(small_chain$E_n)],i =  small_chain$i[length( small_chain$i)],L =  small_chain$L[length( small_chain$L)])
  Omega_a<-0.5*(small_chain$Z[length(small_chain$Z)]>0.5)
  Omega_b<-0.5+0.5*(small_chain$Z[length(small_chain$Z)]>0.5)
  time<-small_chain$s[length(small_chain$s)]
  print(c(time,"//",T_vec))
}

big_chain<-data.frame(s=big_chain_s,Z=big_chain_Z)

p<-ggplot(big_chain,aes(x = s,y = Z))
p+geom_line()

pushover(message = "Hieeee")
# 
# Omega_a<-0.5*(prueba$Z[length(prueba$Z)]>0.5)
# Omega_b<-0.5+0.5*(prueba$Z[length(prueba$Z)]>0.5)
# prueba2<-chain(Z = prueba$Z[length(prueba$Z)],s = prueba$s[length(prueba$Z)],A = prueba$A[length(prueba$Z)],E_n = prueba$E_n[length(prueba$Z)],i = prueba$i[length(prueba$Z)],L = prueba$L[length(prueba$Z)],T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
# 
# p2<-ggplot()
# 
# Omega_a<-0.5*(prueba2$Z[length(prueba2$Z)]>0.5)
# Omega_b<-0.5+0.5*(prueba2$Z[length(prueba2$Z)]>0.5)
# prueba3<-chain(Z = prueba2$Z[length(prueba2$Z)],s = prueba2$s[length(prueba2$Z)],A = prueba2$A[length(prueba2$Z)],E_n = prueba2$E_n[length(prueba2$Z)],i = prueba2$i[length(prueba2$Z)],L = prueba2$L[length(prueba2$Z)],T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
# 
# Omega_a<-0.5*(prueba3$Z[length(prueba3$Z)]>0.5)
# Omega_b<-0.5+0.5*(prueba3$Z[length(prueba3$Z)]>0.5)
# prueba4<-chain(Z = prueba3$Z[length(prueba3$Z)],s = prueba3$s[length(prueba3$Z)],A = prueba3$A[length(prueba3$Z)],E_n = prueba3$E_n[length(prueba3$Z)],i = prueba3$i[length(prueba3$Z)],L = prueba3$L[length(prueba3$Z)],T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
# 
# 
# prueba<-chain(x0 = x0,T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps,c_ = 0,new = F)
# qplot(x = prueba$s,y = prueba$Z)
# count<-0
# store<-c()
# for(i in 2:length(prueba$Z)){
#   if (((prueba$Z[i-1]>0.5)&&(prueba$Z[i]<0.5))||((prueba$Z[i-1]<0.5)&&(prueba$Z[i]>0.5))) {
#     count<-count+1
#     store<-c(store,prueba$s[i])
#   }
# }
# difference<-store[-1]-store[-length(store)]
# 
# # #This is for the long chain
# # prueba<-chain(x0 = x0,T_ = T_vec,t0 = low_t,epsilon = varepsilon_vec,Nsteps = N_steps*(236/80),c_ = 0,new = F)
# # 
# 
# 
# #This is for the split chain
# #for(k in 1:length(parameters[,1])){
# indicators<-c()
# for(j in 1:sample_size){
#   ZandL<-chain(x0 = x0,t0 = low_t,T_ = T_vec,epsilon = varepsilon_vec,Nsteps = N_steps,c_=0,new = FALSE)
#   indicators<-c(indicators,ZandL$s[length(ZandL$s)])
#   x0<-ZandL$Z[length(ZandL$Z)]
#   Omega_a<-0.5*(x0>0.5)
#   Omega_b<-0.5+0.5*(x0>0.5)
#   print(j)
# }
# #cum_indicators<-cumsum(indicators)
# 
# 
# #  estim<-c(mean(indicators),(1/sample_size)*sqrt((1/mean(indicators))-1))
# #  write.table(x=c(date(),"## BEFORE ##",sample_size,N_steps,parameters[k,],estim),file=paste("C:/Users/User/Google Drive/Research/Code/[Djehiche+Giesecke] Epsilon Jump Diffusion v",vers,".txt"), append=TRUE,row.names = F, col.names = F, quote=F, sep="//")
# #}
# 

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
