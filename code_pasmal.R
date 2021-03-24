library(invgamma)

pi1 <- function(year, mu, alpha, beta1, beta2, b){
  return(exp(mu + alpha + beta1*year + beta2*(year^2-22) + b)/(exp(mu + alpha + beta1*year + beta2*(year^2-22) + b)+1))
}

pi0 <- function(mu){
  return(exp(mu)/(exp(mu)+1))
}

oxford <- function(year, n1, n0, r1, r0, nchain = 10^4, prop_sd = c(0.05, 0.03, 0.003, 0.1, 1)){
  K = length(year)
  chain <- matrix(NA, nchain + 1, 4 + 2*K)
  colnames(chain) <- c("alpha", "beta1", "beta2", paste("b", 1:K, sep =""), paste("mu", 1:K, sep =""), "sigma2")
  #initialization
  chain[1,] <- rep(0, 4 + 2*K)
  acc_rates <- rep(0, 3 + 2*K)
  
  #alpha, beta1, beta2, b, mu, sigma2
  
  for (iter in 1:nchain){
    current <- chain[iter,]
    
    cat("iter")
    cat(iter)
    
    ##Mise à jour sigma2
    update_shape <- 10^(-3) + K/2
    update_scale <- 10^(-3) + sum(current[4:(3+K)]^2)/2
    sigma2 <- rinvgamma(1, shape=update_shape, scale=update_scale)
    current[4+2*K] <- sigma2
    
    ## Mise a jour de alpha
    prop <- current
    prop[1] <- rnorm(1, current[1], prop_sd[1])
    top <- -0.5*prop[1]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], prop[1], current[2], current[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], prop[1], current[2], current[3], current[4:(3+K)])))
    bottom <- -0.5*current[1]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)])))
    acc_prob <- exp(top-bottom)
    
    if (runif(1) < acc_prob){
      current <- prop
      acc_rates[1] <- acc_rates[1] + 1
    }
    
    ## Mise a jour de bata1
    prop <- current
    prop[2] <- rnorm(1, current[2], prop_sd[2])
    
    top <- -0.5*prop[2]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], current[1], prop[2], current[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], current[1], prop[2], current[3], current[4:(3+K)])))
    bottom <- -0.5*current[2]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)])))
    acc_prob <- exp(top-bottom)
    
    if (runif(1) < acc_prob){
      current <- prop
      acc_rates[2] <- acc_rates[2] + 1
    }
    
    
    ## Mise a jour de bata2
    prop <- current
    prop[3] <- rnorm(1, current[3], prop_sd[3])
    
    top <- -0.5*prop[3]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], current[1], current[2], prop[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], current[1], current[2], prop[3], current[4:(3+K)])))
    bottom <- -0.5*current[3]^2/10^3 + sum(r1*log(pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)]))+(n1-r1)*log(rep(1,120)-pi1(year, current[(4+K):(3+2*K)], current[1], current[2], current[3], current[4:(3+K)])))
    acc_prob <- exp(top-bottom)
    
   if (runif(1) < acc_prob){
      current <- prop
      acc_rates[3] <- acc_rates[3] + 1
    }
    
    ## Mise a jour de b
    
    for (i in 1:K){
      prop <- current
      prop[3+i] <- rnorm(1, current[3+i], prop_sd[4])
      top <- -0.5*prop[3+i]^2/current[4+2*K]^2 + r1[i]*log(pi1(year[i], current[3+K+i], current[1], current[2], current[3], prop[3+i]))+(n1[i]-r1[i])*log(1-pi1(year[i], current[3+K+i], current[1], current[2], current[3], prop[3+i]))
      bottom <- -0.5*current[3+i]^2/current[4+2*K]^2 + r1[i]*log(pi1(year[i], current[3+K+i], current[1], current[2], current[3], current[3+i]))+(n1[i]-r1[i])*log(1-pi1(year[i], current[3+K+i], current[1], current[2], current[3], current[3+i]))
      acc_prob <- exp(top-bottom)
      
      if (runif(1) < acc_prob){
        current <- prop
        acc_rates[3+i] <- acc_rates[3+i] + 1
      }
    }
    
    ## Mise a jour de mu
    for (j in 1:K){
      prop <- current
      prop[3+K+j] <- rnorm(1, current[3+K+j], prop_sd[5])
      top <- -0.5*prop[3+K+j]^2/10^3 + r0[j]*log(pi0(prop[3+K+j])) + (n0[j]-r0[j])*log(1-pi0(prop[3+K+j])) + r1[j]*log(pi1(year[j], current[3+K+j], current[1], current[2], current[3], prop[3+j]))+(n1[j]-r1[j])*log(1-pi1(year[j], current[3+K+j], current[1], current[2], current[3], prop[3+j]))
      bottom <- -0.5*current[3+K+j]^2/10^3 + r0[j]*log(pi0(current[3+K+j])) + (n0[j]-r0[j])*log(1-pi0(current[3+K+j])) + r1[j]*log(pi1(year[j], current[3+K+j], current[1], current[2], current[3], current[3+j]))+(n1[j]-r1[j])*log(1-pi1(year[j], current[3+K+j], current[1], current[2], current[3], current[3+j]))
      acc_prob <- exp(top-bottom)
      
      if (runif(1) < acc_prob){
        current <- prop
        acc_rates[3+K+j] <- acc_rates[3+K+j] + 1
      }
    }

     ## Sauvegardons le nouvel etat
      chain[iter+1,] <- current
    }
    return(list(chain = chain, acc_rates = acc_rates / nchain))
  }
  
  ## Application
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
  
  out <- oxford(year, n1, n0, r1, r0)
  out$chain <- out$chain[-(1:1000),]
  
  par(mfrow = c(4, 2), mar = c(4, 5, 0.5, 0.5))
  for (j in 1:3){
    plot(out$chain[,j], type = "l", main = "")
    plot(density(out$chain[,j]), type = "l", main = "")
  }
    
  plot(out$chain[,244], type = "l", main = "")
  plot(density(out$chain[,244]), type = "l", main = "")
  
  
  moy_alpha <- mean(out$chain[8000:9001,1])
  moy_beta1 <- mean(out$chain[8000:9001,2])
  moy_beta2 <- mean(out$chain[8000:9001,3])
  moy_sigma <- mean(sqrt(out$chain[8000:9001,244]))
