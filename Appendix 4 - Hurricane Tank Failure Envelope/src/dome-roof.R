# dome roof calculations, Cp

# ASCE7-16 dome roof wind loading calculation, divided into 10 strips
dome_roof_calc <- function(roof, q_h, gust, gcpi){
  
  
  xN <- seq(from = -roof$D/2, to = roof$D/2, by = roof$D/10)
  yc <- (roof$f^2 - roof$D^2 / 4) / (2 * roof$f)
  r <- sqrt(yc^2 + roof$D^2 / 4)
  d25 <- 25 * pi / 180
  xdeg25 <- -(2 * yc * tan(d25) + sqrt((-2 * yc * tan(d25))^2 - 4 * (1 + tan(d25)^2) * (yc^2 - r^2))) / (2 * (1 + tan(d25)^2))
  yN <- sqrt(r^2 - xN^2) + yc
  
  b_vals <- seq(from = roof$D/10/2, by = roof$D/10, length.out = length(xN))
  trib_area <- vector(mode = "double", length = length(xN))
  for (x in seq_along(xN)[-length(xN)]) {
    trib_area[x] <- trib_area_func(R = roof$D/2, b = b_vals[x], prev_area = sum(trib_area))
  }
  trib_area[length(trib_area)] <- pi * roof$D^2 / 4 - sum(trib_area)
  
  Cp_func <- Cp_calc(roof, xdeg25)
  CpN_A <- Cp_func$A(xN)
  CpN_B <- Cp_func$B(xN)
  
  P_A <- pmin(q_h * (gust * CpN_A - gcpi), q_h * (gust * CpN_A + gcpi)) # psf
  P_B <- pmin(q_h * (gust * CpN_B - gcpi), q_h * (gust * CpN_B + gcpi)) # psf
  
  F_A <- P_A * trib_area # lb
  F_B <- P_B * trib_area # lb
  Fx_A <- -F_A * (xN / sqrt(xN^2 + (sqrt(r^2 - xN^2) + yc)^2)) # lb
  # Fy_A <- -F_A * (sqrt(r^2 - xN^2) + yc) / sqrt(xN^2 + (sqrt(r^2 - xN^2) + yc)^2) # lb
  Fy_A <- -(F_A*((sqrt(r^2-xN^2)+yc)/(sqrt(xN^2+(sqrt(r^2-xN^2)+yc)^2)))) # lb
  Fx_B <- -F_B * (xN / sqrt(xN^2 + (sqrt(r^2 - xN^2) + yc)^2)) # lb
  # Fy_B <- -F_B * (sqrt(r^2 - xN^2) + yc) / sqrt(xN^2 + (sqrt(r^2 - xN^2) + yc)^2) # lb
  Fy_B <- -(F_B * ((sqrt(r^2 - xN^2) + yc) / (sqrt(xN^2 + (sqrt(r^2 - xN^2) + yc)^2)))) # lb
  
  Mx_A <- abs(sum((yN + roof$H) * Fx_A)) # lb-ft
  My_A <- (sum((xN + roof$D/2) * Fy_A)) # lb-ft
  Mx_B <- abs(sum((yN + roof$H) * Fx_B)) # lb-ft
  My_B <- (sum((xN + roof$D/2) * Fy_B)) # lb-ft
  
  M_A <- Mx_A + My_A # lb-ft
  M_B <- Mx_B + My_B # lb-ft
  
  max_Fx <- max(sum(Fx_A), sum(Fx_B)) # lb
  max_Fy <- max(sum(Fy_A), sum(Fy_B)) # lb
  max_M <- max(M_A, M_B) # lb-ft
  
  return(list(Fx = max_Fx, Fy = max_Fy, M = max_M))
}

# dome roof helper calculation, for pressure zones on roof
Cp_calc <- function(roof, xdeg25){
  A1 <- approxfun(x = c(0.05, 0.5), y = c(0.15, 0.8)) # for hD/D = 0
  A2 <- approxfun(x = c(0.05, 0.5), y = c(-1.4, 0.8)) # for hD/D = 0.25
  A3 <- approxfun(x = c(0.05, 0.1, 0.325, 0.5), y = c(-1.4, -1.7, -1.4, 0.8)) # for hD/D >= 1.0
  B1 <- approxfun(x = c(0.05, 0.5), y = c(-0.25, -1.2)) # for hD/D = 0
  B2 <- approxfun(x = c(0.05, 0.1, 0.25, 0.5), y = c(-0.5, -0.8, -1.2, -1.5)) # for hD/D >= 0.5
  C1 <- approxfun(x = c(0.05, 0.5), y = c(0, 0)) # for hD/D = 0
  C2 <- approxfun(x = c(0.05, 0.5), y = c(-0.5, -0.5)) # for hD/D >= 0.5
  
  fD <- roof$f / roof$D # f/D
  A4 <- approxfun(x = c(0, 0.25, 1.0), y = c(A1(fD), A2(fD), A3(fD)), rule = 2)
  B4 <- approxfun(x = c(0, 0.5), y = c(B1(fD), B2(fD)), rule = 2)
  C4 <- approxfun(x = c(0, 0.5), y = c(C1(fD), C2(fD)), rule = 2)
  hDD <- roof$H / roof$D # hD/D
  CpA <- A4(hDD)
  CpB <- B4(hDD)
  CpC <- C4(hDD)
  
  Cp_A_func <- approxfun(x = c(-roof$D/2, 0, roof$D/2), y = c(CpA, CpB, CpC)) # load case A
  Cp_B_func <- approxfun(x = c(-roof$D/2, xdeg25, 0, roof$D/2), y = c(CpA, CpA, CpB, CpC)) # load case B
  return(list(A = Cp_A_func, B = Cp_B_func))
}

# dome roof helper function, tributary area calc
trib_area_func <- function(R, b, prev_area){
  A <- R^2 * acos((R - b) / R) - (R - b) * sqrt((2 * R * b - b^2)) - prev_area # ft2
  return(A)
}

