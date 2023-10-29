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
MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 


# Assignment 3:  
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 




# Instantiate the cores:
cores <- detectCores()
cl <- makeCluster(cores - 1)

# Next we register the cluster.
registerDoParallel(cl)

# Rewritten line for Parallel Computing
results <-
  foreach(i = 1:nrow(df),
# Combines results from each ireation.
          .combine = c,
# Ensures tweedie package is available for each parallel, because of MTweedieTests
          .packages = "tweedie") %dopar% {    
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05)
          }
# Storing results
df$share_reject <- unlist(results)

# Stop Cluster
stopCluster(cl)


