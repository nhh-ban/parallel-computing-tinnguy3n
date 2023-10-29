# Assignment 1:  
library(tweedie)
library(ggplot2)
library(doParallel)
library(foreach)


simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 


# Assignment 2:  
MTweedieTests <- function(N, M, sig) {
  
  simTweedieTest <- 
    function(N){ 
      t.test( 
        rtweedie(N, mu=10000, phi=100, power=1.9), 
        mu=10000 
      )$p.value 
    } 
  
  # Register parallel backend
  cores <- detectCores()
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  # Run M simulations in parallel with foreach
  p_values <- foreach(m = 1:M,
                      .combine = c,
                      .packages = "tweedie") %dopar% {
                        simTweedieTest(M)
                      }
  
  # Calculate proportion of p-values less than sig
  final_result <- sum(p_values < sig) / M
  
  # Stop Cluster
  stopCluster(cl)
  
  return(final_result)
}


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 


