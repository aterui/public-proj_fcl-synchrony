fclfun <- function(q = c(0.5,0.5,0.5), A = c(10,100,1000), E = 10,
                   r = 100:1000, lambda = 1, beta){
  # Declare parameters
  ES <- pi*r^2
  N_bar <- lambda*pi*r^2
  N_bar_dash <- lambda*pi*10^(2*mean(range(log(r,10)) ) )
  
  r_m <- 10^mean(range(log(r,10)))
  rho_fun_m <- function(x){ exp(-beta*x)*(((4*x)*acos(x/(2*r_m)) )/(pi*r_m^2) - ((x^2)*sqrt((4*r_m^2 - x^2)) )/(pi*r_m^4)) }
  rho_bar_dash <- integrate(rho_fun_m, 0, 2*r_m)$value
  
  C <- matrix(NA, nrow = length(N_bar), ncol = 3)
  alpha <- rho_bar <- NULL
  p1 <- p2 <- p3 <- p1_bar <- p2_bar <- p3_bar <- NULL
  p_L1_bar <- p_L2_bar <- p_L3_bar <- NULL
  fcl <- NULL
  
  for(i in 1:length(N_bar)){
    ## Colonization rate
    for(j in 1:3){ 
      if(ES[i] > A[j]){ ### dispersal limited
        C[i,j] <- q[j]*lambda*A[j]
      }else{ ### ecosystem size limited
        C[i,j] <- q[j]*N_bar[i]
      }
    }
    
    ## Extinction coefficient
    alpha[i] <- 1 + (N_bar_dash - 1)*rho_bar_dash
    
    
    ## Equilibrium occupancy
    p1[i] <- 1 - (alpha[i]*E)/C[i,1]
    p2[i] <- 1 - (alpha[i]*E)/C[i,1] - ((alpha[i] + 1)*E)/C[i,2]
    p3[i] <- 1 - (alpha[i]*E)/C[i,1] - ((alpha[i] + 1)*E)/C[i,2] - ((alpha[i] + 2)*E)/C[i,3]
    
    ### transform negative values to zero
    p1_bar[i] <- ifelse(p1[i] <= 0, 0, p1[i])
    p2_bar[i] <- ifelse(p2[i] <= 0, 0, p2[i])
    p3_bar[i] <- ifelse(p3[i] <= 0, 0, p3[i])
    
    p_L1_bar[i] <- p1_bar[i] - p2_bar[i]
    p_L2_bar[i] <- p2_bar[i] - p3_bar[i]
    p_L3_bar[i] <- p3_bar[i]
  }
  
  fcl <- p_L1_bar + 2*p_L2_bar + 3*p_L3_bar
  state <- cbind(p_L1_bar, p_L2_bar, p_L3_bar)
  
  return(list(fcl = fcl, state = state, alpha = alpha) )
}