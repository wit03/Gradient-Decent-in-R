# gradient descent function takes five arguments
gradDesc <- function(df, x, y, alpha = 0.01, max_iter = 100000){
  
  # scale x for faster training
  start_time <- proc.time()
  n <- nrow(df)
  x <- as.vector(scale(df[[x]]))
  y <- df[[y]]
  plot(x, y, pch = 16) 
  
  # initialize random weights
  m <- runif(1,0,1)
  c <- runif(1,0,1)
  yhat <- m * x + c
  mse <- (1/n) * sum((y - yhat) ** 2)
  converged <- F
  iteration <- 0
  
  # update weight using GD algorithm
  cat("=== Implementing gradient descent algorithm ===")
  while(converged == F){
    iteration <- iteration + 1
    m_new <- m - alpha * (1/n) * sum((yhat - y) * x)
    c_new <- c - alpha * (1/n) * sum(yhat - y)
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    mse_new <- (1/n) * sum((y - yhat) ** 2)
    
    # if iteration hits max_iter, program ends
    if(iteration == max_iter){
      converged <- T
      abline(c, m)
      return(cat("\nOptimal intercept:", c,
                 "\nOptimal slope:", m,
                 "\nIteration:", iteration,
                 "\nFinal MSE:", mse_new,
                 "\nTime for training:", 
                 (proc.time() - start_time)[1], "seconds."))
    }
  }
}

# test function x=wt, y=mpg 
gradDesc(mtcars, x = "wt", y = "mpg", alpha = 0.001, max_iter = 100000)