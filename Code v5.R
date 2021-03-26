
library(rstanarm) # for the function logit() and invlogit()
library(invgamma) # for the invgamma


# DATASET
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



# logpsi - single value
  # mu = mu[i], b = b[i]
  # para = alpha, beta1, beta2 := current/prop [2*K+1:2*K+3]
f_logpsi <- function(b, para, year){
  alpha <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  return(alpha + beta1 * year + beta2 * (year^2 - 22) + b)
}

# p0 - single value
  # mu = mu[i]
f_p0 <- function(mu){
  return(invlogit(mu))
}

# p1 - single value
  # mu = mu[i], b = b[i]
  # para = alpha, beta1, beta2 := current/prop [2*K+1:2*K+3]
f_p1 <- function(mu, b, para, year ){
  return(invlogit(f_logpsi(b, para, year) + mu))
}
  


fatigue <- function(year = year, n0 = n0, n1 = n1, r0 = r0, r1 = r1,
                     K = 120, prop.sd = c(0.1,0.005,0.01,0.005,0.005), nchain = 10^4, priors = c(1, 0.1, 1, 1, 0, 1000)){
  mu <- rep(0, K)
  b <- rep(0, K)
  alpha <- 0
  beta1 <- 0
  beta2 <- 0
  sigma2 <- 10^6
  
  # 1:K = mu ; K+1:2K = b ; 2K+1 = alpha ; 2K+2 = beta1; 2K+3 = beta2, 2K+4 = sigma
  chain <- matrix(NA, nchain + 1, 2 * K + 4)
  chain[1,] <- c(mu, b, alpha, beta1, beta2, sigma2)

  
  acc.rates <- rep(0, 2*K + 4)
  # episode
  for (iter in 1:nchain){
    cat(" iteration ")
    cat(iter)
    cat("\n")
    current <- chain[iter,]
    # update of mu
    for (i in 1:K){ 
      prop <- current # initialization 
      prop[i] <- rnorm(1, prop[i], prop.sd[1]) # update of mu[i]

      # symmetric kernel
      p0 <- f_p0(prop[i])
      p1 <- f_p1(prop[i], prop[K+i], prop[(2*K+1):(2*K+3)], year[i])
      logtop <- log(p0^r0[i] * (1-p0)^(n0[i]-r0[i]) * p1^r1[i] * (1-p1)^(n1[i] - r1[i])) -prop[i]^2/(2*10^3)
      
      
      p0 <- f_p0(current[i])
      p1 <- f_p1(current[i], current[K + i], current[(2*K+1):(2*K+3)],year[i])
      logbottom <- log(p0^r0[i] * (1-p0)^(n0[i]-r0[i]) * p1^r1[i] * (1-p1)^(n1[i] - r1[i])) -current[i]^2/(2*10^3)
      acc.prob <- exp(logtop-logbottom)
      if (runif(1)<acc.prob){ # accept
        current <- prop # update
        acc.rates[i] <- acc.rates[i] + 1
      }
    }

    # update of b
    for (i in 1:K){
      prop <- current
      prop[K+i] <- rnorm(1, prop[K+i], prop.sd[2]) # update of b[i]
      
      # symmetric kernel
      p1 <- f_p1(prop[i], prop[K+i], prop[(2*K+1):(2*K+3)], year[i])
      logtop <- log(p1^r1[i] * (1-p1)^(n1[i] - r1[i])) -prop[K+i]^2/2
      
      p1 <- f_p1(current[i], current[K + i], current[(2*K+1):(2*K+3)],year[i])
      logbottom <- log(p1^r1[i] * (1-p1)^(n1[i] - r1[i])) - current[K+i]^2/2
      acc.prob <- exp(logtop-logbottom)
      if (runif(1)<acc.prob){ # accept
        current <- prop # update
        acc.rates[K+i] <- acc.rates[K+i] + 1
      }
    }
    
    # update of alpha, beta1, beta2
    for (i in ((2*K+1):(2*K+3))){
      prop <- current
      prop[i] <- rnorm(1, prop[i], prop.sd[3]) # update of alpha, beta1, or beta2
      #symmetric kernel
      logtop <- (-prop[i]^2/(2*10^3)) # the exp factor
      for (j in 1:K){ #calculate p for the top
        p1 <- f_p1(prop[j], prop[K + j], prop[(2*K+1) : (2*K+3)],year[j])
        logtop <- logtop + log(p1^r1[j] * (1-p1)^(n1[j] - r1[j]))
      }
      
      logbottom <- (-current[i]^2/(2*10^3)) # the exp factor
      for (j in 1:K){ # calculate p for the bottom
        p1 <- f_p1(current[j], current[K + j], current[(2*K+1) : (2*K+3)],year[j])
        logbottom <- logbottom + log(p1^r1[j] * (1-p1)^(n1[j] - r1[j]))
      }
      acc.prob <- exp(logtop-logbottom)
      
      if (runif(1)<acc.prob){
        current <- prop # update
        acc.rates[i] <- acc.rates[i] + 1
      }
    
    }
    
    #update of sigma!
    
    sigma2 <- 1/rgamma(1,10^(-3)+K/2, rate=10^(-3)+sum(current[(K+1):(2*K)]^2)/2)
    
    chain[iter+1,] <- current
    chain[iter+1, 2*K + 4] <- sigma2
  }
  return(list(chain = chain, acc.rates = acc.rates / nchain))
}

result <- fatigue(year = year, n0 = n0, n1 = n1, r0 = r0, r1 = r1)

chainval <- result$chain[1001:10001,]
means <- colSums(chainval)/9001
means_mu <- means[1:120]
means_b <- means[121:240]
m_alpha <- means[241]
m_beta1 <- means[242]
m_beta2 <- means[243]
m_sigma2 <- means[244]

m_alpha
m_beta1
m_beta2
m_sigma2