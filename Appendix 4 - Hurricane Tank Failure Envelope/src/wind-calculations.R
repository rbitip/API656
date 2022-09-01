# wind calcs

# ASCE7-16 based wind calculations
# assumptions: K_d, K_zt, K_e, gust, gcpi, C_f
wind_calculation <- function(roof){
  # velocity pressure calculation
  K_d <- 1 # directionality factor
  K_zt <- 1 # topographic factor
  K_e <- 1 # ground elevation factor
  gust <- 0.85 # gust effect factor
  gcpi <- 0.18 # # internal pressure coefficient
  alpha_list <- list(B = 7, C = 9.5, D = 11.5)
  zg_list <- list(B = 1200, C = 900, D = 700)
  alpha <- alpha_list[[roof$exposure_category]]
  zg <- zg_list[[roof$exposure_category]] # ft
  K_z <- 2.01 * (roof$H / 2 / zg)^(2 / alpha) # vel. pres. exposure coeff. shell
  K_h <- 2.01 * ((roof$H + roof$f / 2) / zg)^(2 / alpha) # vel. pres. exposure coeff. roof
  q_z <- 0.00256 * K_z * K_zt * K_d * K_e * roof$wind_speed^2 # psf
  q_h <- 0.00256 * K_h * K_zt * K_d * K_e * roof$wind_speed^2 # psf
  
  # shell wind force calculation
  C_f <- 0.63 # force coefficient
  shell_wind <- list()
  shell_wind$Fx <- gust * q_z * C_f * roof$D * roof$H # lb
  shell_wind$Fy <- 0
  shell_wind$M <- shell_wind$Fx * roof$H / 2
  
  # stitch all the values together
  roof <- c(roof, q_z = q_z, q_h = q_h)
  roof$shell_wind <- shell_wind
  
  # roof wind force calculations, depending on type
  if (roof$roof_type == "dome") roof_wind <- dome_roof_calc(roof, q_h, gust, gcpi)
  else if (roof$roof_type == "cone") roof_wind <- cone_roof_calc(roof, q_h, gust, gcpi)
  else roof_wind <- list(Fx = 0, Fy = 0, M = 0)
  
  # stitch it together
  roof$roof_wind <- roof_wind
  
  Fx_wind <- shell_wind$Fx + roof_wind$Fx
  Fy_wind <- shell_wind$Fy + roof_wind$Fy
  M_wind <- shell_wind$M + roof_wind$M
  roof <- c(roof, Fx_wind = Fx_wind, Fy_wind = Fy_wind, M_wind = M_wind)
  
  return(roof)
}
