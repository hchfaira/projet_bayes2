#les données
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

#un fonction qui nouq aide pour des calculs

logit<- function(year,mu,alpha,beta1,beta2,b,K){
  log<-rep(0, K)
  for (i in 1:K){
    log[i]<-mu[i]+alpha+beta1*year[i]+beta2*(year[i]^2-22)+b[i]}
  return(log)
}


oxford <- function(year,n0,n1,r0,r1,K, nchain = 10^4, prop_sd = c(rep(2,K),2,6,2,2)){
  #les valeurs initiales 
  alpha <- 0
  beta1 <- 0
  beta2 <- 0
  sigma_sq <- 1
  mu <- rep(0,K)
  init <- c(mu,alpha,beta1,beta2,sigma_sq) #vecteurs de valeurs initiales
  #les valeurs initiales pour b,mu,beta1,beta2,sigma_sq sont données 
  #!!!!!! pour b je ne sais pas quoi faire :( car elle suit une lois normale de paramètre(0,sigma_sq) et sigma  _sq change....
  b  <- rep(0,K)
  
  #la chaine vide K+4 variables les mu ,alpha ,beta1 ,beta2,sigma_sg

  chain <- matrix(NA, nchain + 1, K+4)
  chain[1,] <- init
  acc_rates <- rep(0, K+4)
  
  sig<-1000 #c'est l'écart type pour les lois à priori de beta1,beta2,sugma_sq,mu
  
  for (iter in 1:nchain){
    current <- chain[iter,]
    for (i in 1:K+4){
      
      
      prop <- current
      if(i<=K){#pour les mu !!!
      prop[i] <- rnorm(1, current[i], prop_sd[i])
      ##noyau symétrique
      #les formules données
      top <-  prop[i]*r0[i]-(prop[i]^2)/(2*sig^2)+n0[i]*log(1+exp(prop[i]))
      bottom <-  current[i]*r0[i]-(current[i]^2)/(2*sig^2)+n0[i]*log(1+exp(current[i]))
      }
      else if(i<K+4){#pour alpha ,beta1 ,beta2
        
      prop[i] <- rnorm(1, current[i], prop_sd[i])
      logit.prop<-logit(year,prop[1:K],prop[K+1],prop[K+2],prop[K+3],b,K)#juste pour simplifier les calculs
      #top
      top <-  sum(logit.prop*r1-n1*log(1+exp(logit.prop)))-prop[i]^2/(2*sig^2)
      logit.current<-logit(year,current[1:K],current[K+1],current[K+2],current[K+3],b,K)
      
      bottom <-  sum(logit.current*r1-n1*log(1+exp(logit.current)))-current[i]^2/(2*sig^2)
      }
      
      else{#pour sigma_sq
        #paramètre de la loi à prioro inverse gamma 
        shape<-0.001
        scale<-0.001
        #sigma doit être positif strictement !!!!!!!on a choisit donc une normale tronquée de borne inf proche de 0 et de borne sup infini
        #le noyau n'est pas symétrique dans ce cas !!!!!!!!
        
        prop[i] <- rtruncnorm(1, a=10^(-10) , b=10^10 , current[i], prop_sd[i])
        bottom_kernel <- dtruncnorm(prop[i], a=10^(-10) , b=10^10 , current[i], prop_sd[i])  #on a pas un noyau symetrique !
        top_kernel <- dtruncnorm(current[i], a=10^(-10) , b=10^10 , prop[i], prop_sd[i]) #on a pas un noyau symetrique !
        
        top <-  -(K+1+shape)*log(prop[i])-scale/prop[i]-sum(b^2/(2*prop[i]^2))+log(top_kernel)
       
        bottom <-  -(K+1+shape)*log(current[i])-scale/current[i]-sum(b^2/(2*current[i]^2))*+log(bottom_kernel)
        }
      
      
      
      acc_prob <- exp(top - bottom)
      
      
      if (runif(1) < acc_prob){
        current <- prop
        acc_rates[i] <- acc_rates[i] + 1}
    }
    ## Sauvegardons le nouvel etat
    chain[iter+1,] <- current
    
  }
  
  return(list(chain = chain, acc_rates = acc_rates / nchain))
}
out <-oxford(year,n0,n1,r0,r1,K, nchain = 10^4, prop_sd = c(rep(2,K),2,6,2,2))