# Timer file

library(tictoc)

library(foreach)
library(iterators)
library(parallel)
library(tidyverse)

# Clear timing log 
tic.clearlog()

# 1. Timing solution 1 script
tic("Timing Solution 1")
source("scripts/hw4_solution1.R")
toc(log = TRUE)     # 54.909 sec elapsed


# 2. Timing solution 2 script
tic("Timing Solution 2")
source("scripts/hw4_solution2.R")
toc(log = TRUE)     # 34.523 sec elapsed


# 3. Timing solution 3 script
tic("Timing Solution 3")
source("scripts/hw4_solution3.R")
toc(log = TRUE)     # 15.029 sec elapsed


printTicTocLog <- 
  function() {
    tic.log() %>% 
      unlist %>% 
      tibble(logvals = .) %>% 
      separate(logvals, 
               sep = ":", 
               into = c("Solutions", "log")) %>% 
      mutate(log = str_trim(log)) %>% 
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }

# Print results
printTicTocLog() %>% 
  knitr::kable()


# The third solution is the fastest, and was 40 seconds faster than the
# original solution. 

# The reason for it being faster could be because it works with "M" simulations
# that could be more computationally intensive and might yield a higher speedup
# when parallelized. 



