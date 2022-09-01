# cone roof calculations

# ASCE7-16 cone roof calculation
cone_roof_calc <- function(roof, q_h, gust, gcpi){
  CP_1 <- -0.8
  CP_2 <- -0.5
  
  P_1 <- q_h * (gust * CP_1 - gcpi) # psf
  P_2 <- q_h * (gust * CP_2 - gcpi)# psf
  
  b <- cone_roof_zone(roof$f / (roof$D / 2), roof$D, roof$H) # ft
  b_percent <- (b - roof$D / 2) / (roof$D / 2)
  area_1 <- (roof$D / 2)^2 * acos((roof$D / 2 - b) / (roof$D / 2)) - (roof$D / 2 - b) * sqrt(roof$D * b - b^2) # ft2
  area_2 <- pi * roof$D^2 / 4 - area_1 # ft2
  
  F_1 <- -P_1 * area_1 # lb
  F_2 <- -P_2 * area_2 # lb
  
  x_1 <- roof$D / 2 - (0.53 * b_percent - 0.425) * roof$D / 2 # ft
  x_2 <- roof$D / 2 - (0.53 * b_percent + 0.425) * roof$D / 2 # ft
  
  M_1 <- F_1 * x_1 # lb-ft
  M_2 <- F_2 * x_2 # lb-ft
  
  max_Fx <- 0 # lb # considered negligible
  max_Fy <- F_1 + F_2 # lb
  max_M <- M_1 + M_2 # lb-ft
  
  return(list(Fx = max_Fx, Fy = max_Fy, M = max_M))
}

# cone roof helper function, finds the division between zone 1 and 2 on cone roof
cone_roof_zone <- function(slope, D, H){
  if (slope < 10) {
    if (H / D < 0.25) b <- 0.2 * D
    else if (H / D < 0.5) b <- 0.2 * D + (0.5 * D - 0.2 * D) / (0.5 - 0.25) * (H / D - 0.25)
    else if (H / D < 1)  b <- 0.5 * D + (0.1 * H + 0.6 * D - 0.5 * D) / (1 - 0.5) * (H / D - 0.5)
    else b <- 0.1 * H + 0.6 * D
  } else {
    b <- 0.6 * D
  }
  return(b) # ft
}
