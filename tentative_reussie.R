#les données
library(invgamma)
K <- 120
r1 <-
  c(3, 5, 2, 7, 7, 2, 5, 3, 5, 11, 6, 6, 11, 4, 4, 2, 8, 8, 6, 
    5, 15, 4, 9, 9, 4, 12, 8, 8, 6, 8, 12, 4, 7, 16, 12, 9, 4, 7, 
    8, 11, 5, 12, 8, 17, 9, 3, 2, 7, 6, 5, 11, 14, 13, 8, 6, 4, 8, 
    4, 8, 7, 15, 15, 9, 9, 5, 6, 3, 9, 12, 14, 16, 17, 8, 8, 9, 5, 
    9, 11, 6, 14, 21, 16, 6, 9, 8, 9, 8, 4, 11, 11, 6, 9, 4, 4, 9, 
    9, 10, 14, 6, 3, 4, 6, 10, 4, 3, 3, 10, 4, 10, 5, 4, 3, 13, 1, 
    7, 5, 7, 6, 3, 7)
n1 <-
  c(28, 21, 32, 35, 35, 38, 30, 43, 49, 53, 31, 35, 46, 53, 61, 
    40, 29, 44, 52, 55, 61, 31, 48, 44, 42, 53, 56, 71, 43, 43, 43, 
    40, 44, 70, 75, 71, 37, 31, 42, 46, 47, 55, 63, 91, 43, 39, 35, 
    32, 53, 49, 75, 64, 69, 64, 49, 29, 40, 27, 48, 43, 61, 77, 55, 
    60, 46, 28, 33, 32, 46, 57, 56, 78, 58, 52, 31, 28, 46, 42, 45, 
    63, 71, 69, 43, 50, 31, 34, 54, 46, 58, 62, 52, 41, 34, 52, 63, 
    59, 88, 62, 47, 53, 57, 74, 68, 61, 45, 45, 62, 73, 53, 39, 45, 
    51, 55, 41, 53, 51, 42, 46, 54, 32)
r0 <-
  c(0, 2, 2, 1, 2, 0, 1, 1, 1, 2, 4, 4, 2, 1, 7, 4, 3, 5, 3, 2, 
    4, 1, 4, 5, 2, 7, 5, 8, 2, 3, 5, 4, 1, 6, 5, 11, 5, 2, 5, 8, 
    5, 6, 6, 10, 7, 5, 5, 2, 8, 1, 13, 9, 11, 9, 4, 4, 8, 6, 8, 6, 
    8, 14, 6, 5, 5, 2, 4, 2, 9, 5, 6, 7, 5, 10, 3, 2, 1, 7, 9, 13, 
    9, 11, 4, 8, 2, 3, 7, 4, 7, 5, 6, 6, 5, 6, 9, 7, 7, 7, 4, 2, 
    3, 4, 10, 3, 4, 2, 10, 5, 4, 5, 4, 6, 5, 3, 2, 2, 4, 6, 4, 1)
n0 <-
  c(28, 21, 32, 35, 35, 38, 30, 43, 49, 53, 31, 35, 46, 53, 61, 
    40, 29, 44, 52, 55, 61, 31, 48, 44, 42, 53, 56, 71, 43, 43, 43, 
    40, 44, 70, 75, 71, 37, 31, 42, 46, 47, 55, 63, 91, 43, 39, 35, 
    32, 53, 49, 75, 64, 69, 64, 49, 29, 40, 27, 48, 43, 61, 77, 55, 
    60, 46, 28, 33, 32, 46, 57, 56, 78, 58, 52, 31, 28, 46, 42, 45, 
    63, 71, 69, 43, 50, 31, 34, 54, 46, 58, 62, 52, 41, 34, 52, 63, 
    59, 88, 62, 47, 53, 57, 74, 68, 61, 45, 45, 62, 73, 53, 39, 45, 
    51, 55, 41, 53, 51, 42, 46, 54, 32)
year<-
  c(-10, -9, -9, -8, -8, -8, -7, -7, -7, -7, -6, -6, -6, -6, -6, 
    -5, -5, -5, -5, -5, -5, -4, -4, -4, -4, -4, -4, -4, -3, -3, -3, 
    -3, -3, -3, -3, -3, -2, -2, -2, -2, -2, -2, -2, -2, -2, -1, -1, 
    -1, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 
    3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 
    6, 6, 6, 7, 7, 7, 7, 8, 8, 8, 9, 9, 10)

logpsi<- function(year,mu,alpha,beta1,beta2,b,K){
  log<-rep(0, K)
  for (i in 1:K){
    log[i]<-mu[i]+alpha+beta1*year[i]+beta2*(year[i]^2-22)+b[i]}
  return(log)
}
invlogit<-function(x){
  return(exp(x)/(1+exp(x)))
}

oxford0 <- function(year,n0,r0,K, nchain = 10^4, prop_sd = rep(1,120)){
  #les valeurs initiales 
  
  init <- rep(0,K)
  #les valeurs initiales pour b,mu,beta1,beta2,sigma_sq sont données 
  #!!!!!! pour b je ne sais pas quoi faire :( car elle suit une lois normale de paramètre(0,sigma_sq) et sigma  _sq change....
  
  #la chaine vide K+4 variables les mu ,alpha ,beta1 ,beta2,sigma_sg
  
  chain <- matrix(NA, nchain + 1, K)
  chain[1,] <- init
  acc.rates <- rep(0, K)
  
  sig<-1000 #c'est l'écart type pour les lois à priori de beta1,beta2,sugma_sq,mu
  
  for (iter in 1:nchain){
    current <- chain[iter,]
    for (i in 1:K){
       prop <- current
      #pour les mu !!!
      prop[i] <- rnorm(1, current[i], prop_sd[i])
        ##noyau symétrique
        #les formules données
    
      
      top<-dnorm(prop[i],0,sig,log=TRUE)+dbinom(r0[i],n0[i],invlogit(prop[i]),log=TRUE)
      bottom<-dnorm(current[i],0,sig,log=TRUE)+dbinom(r0[i],n0[i],invlogit(current[i]),log=TRUE)
     acc.prob<-exp(top-bottom)
      
      if (runif(1) < acc.prob){
        current <- prop
        acc.rates[i] <- acc.rates[i] + 1}
    }
    ## Sauvegardons le nouvel etat
    chain[iter+1,] <- current
    
  }
  
  return(list(chain = chain, acc.rates = acc.rates / nchain))
}
out0 <-oxford0(year,n0,r0,K)
plot(invlogit(out0$chain[,5]))
#verifier pout p05

plot(density(invlogit(out0$chain[,5])))

r0[5]/n0[5]

oxford <- function(year,n1,r1,mu,K, nchain = 10^4, prop_sd = rep(0.1,4)){
  #les valeurs initiales 
  alpha <- 0
  beta1 <- 0
  beta2 <- 0
  sigma_sq <- 1
  prop_sdb<-rep(1,K)
  b <- rep(0,K)
  init <- c(alpha,beta1,beta2,sigma_sq) #vecteurs de valeurs initiales
  
  
  chain <- matrix(NA, nchain + 1, 4)
  chain[1,] <- init
  acc_rates <- rep(0, 4)
  
  sig<-1000 #c'est l'écart type pour les lois à priori de beta1,beta2,sugma_sq,mu
  
  for (iter in 1:nchain){
    current <- chain[iter,]
    
    for (i in 1:4){
      
      
      prop <- current
      if(i==4){#pour sigma_square 
        
        prop[i] <- rtruncnorm(1, a=0.01 , b=10^10 , current[i], prop_sd[i])
        bottom_kernel <- dtruncnorm(prop[i], a=0.01 , b=10^10 , current[i], prop_sd[i])  #on a pas un noyau symetrique !
        top_kernel <- dtruncnorm(current[i], a=0.01, b=10^10 , prop[i], prop_sd[i]) #on a pas un noyau symetrique !
        # prop[i]<-rlnorm(1,log(current[i]),prop_sd[i])
      
                      
        top<-dinvgamma(prop[i],0.001,rate=1000,log=TRUE)+sum(dnorm(b,0,sqrt(prop[i]),log=TRUE))+ log(top_kernel)
        bottom <-  dinvgamma(current[i],0.001,rate=1000,log=TRUE)+sum(dnorm(b,0,sqrt(current[i]),log=TRUE))+log(bottom_kernel)
        # #mise à jou de b
        # top<-sum(dnorm(b,0,sqrt(prop[i]),log=TRUE))+ log(prop[i])
        # bottom <-  sum(dnorm(b,0,sqrt(current[i]),log=TRUE))+log(prop[i])
        # #mise à jou de b
        
      }
      else{#pour alpha ,beta1 ,beta2
        
        prop[i] <- rnorm(1, current[i], prop_sd[i])
        prob.prop<-logpsi(year,mu,prop[1],prop[2],prop[3],b,K)
        #juste pour simplifier les calculs
        prob.current<-logpsi(year,mu,current[1],current[2],current[3],b,K)
        #top
        top <-  dnorm(prop[i],0,sig,log=TRUE)+sum(dbinom(r1,n1,invlogit(prob.prop),log=TRUE))
        bottom <-   dnorm(current[i],0,sig,log=TRUE)+sum(dbinom(r1,n1,invlogit(prob.current),log=TRUE))
        # top <-  sum(dbinom(r1,n1,invlogit(prob.prop),log=TRUE))
        # bottom <-   sum(dbinom(r1,n1,invlogit(prob.current),log=TRUE))
        
      }
     
      acc_prob <- exp(top - bottom)
     
      
      if (runif(1) < acc_prob){
        current <- prop
        acc_rates[i] <- acc_rates[i] + 1}
    }
    chain[iter+1,] <- current
    # propb<-b
    # for (j in 1:K){
    #   propb[j] <- rnorm(1, b[j], prop_sdb[j])
    #   prob.propb<-logpsi(year[j],mu[j],current[1],current[2],current[3],propb[j],K)
    #   #juste pour simplifier les calculs
    #   prob.currentb<-logpsi(year[j],mu[j],current[1],current[2],current[3],b[j],K)
    #   #top
    #   topb <-  dnorm(propb[j],0,sqrt(current[4]),log=TRUE)+dbinom(r1[j],n1[j],invlogit(prob.propb),log=TRUE)
    #   bottomb <-   dnorm(current[i],0,sqrt(current[4]),log=TRUE)+dbinom(r1[j],n1[j],invlogit(prob.currentb),log=TRUE)
    #   
    # }
    # acc_probb <- exp(topb - bottomb)
    # 
    # if (runif(1) < acc_probb){
    #   b <- propb}
    
    b<-rnorm(K,0,sqrt(current[4]))
    
    
    
    
  }
  
  return(list(chain = chain, acc_rates = acc_rates / nchain))
}


out <-oxford(year,n1,r1, out0$chain[10^4],K)


# out <-oxford(year,n1,r1, colMeans(out0$chain),K)

for (j in 1:4)
  plot(density(out$chain[,j]), type = "l", ylab = j)