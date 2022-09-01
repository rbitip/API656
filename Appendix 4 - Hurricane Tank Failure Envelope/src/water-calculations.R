# rain, flooding, waves calculations

# flooding and rainfall loadigs calculation
hydro_calculation <- function(roof){
  # if TRUE, berm breached, calculate hydrostatics and hydrodynamics for flood
  flood_over_berm <- roof$flood_elevation - roof$terminal_elevation > roof$berm_height # bool
  roof <- c(roof, flood_over_berm = flood_over_berm)
  if (flood_over_berm) {
    
    # flooding hydrostatics
    # not using BFE (min 1 ft)
    # design_flood_level <- min(max(roof$flood_elevation - roof$terminal_elevation - roof$tank_elevation + 1, 0), roof$H) # ft
    design_flood_level <- min(max(roof$flood_elevation - roof$terminal_elevation - roof$tank_elevation, 0), roof$H) # ft
    flood_buoy_force <- pi * roof$D^2 / 4 * design_flood_level * 62.4 # lb
    
    roof <- c(roof, flood_buoy_force = flood_buoy_force)
    
    # flooding hydrodynamics
    if ("water_speed" %in% names(roof)) {
      hdyn_height <- roof$water_drag * roof$water_speed^2 / (2 * 32.2) # ft
      hdyn_pressure <- hdyn_height * 62.4 # psf
      hdyn_force <- hdyn_pressure * design_flood_level * roof$D # lb
      stillwater_depth <- min(0.65 * max(roof$flood_elevation - roof$terminal_elevation - roof$tank_elevation, 0), roof$H) # ft
      wave_height <- 0.78 * stillwater_depth # ft
      C_p_list <- list(I = 1.6, II = 2.8, III = 3.2, IV = 3.5)
      C_p <- C_p_list[[roof$risk_category]]
      if (is.null(C_p)) stop("risk category invalid")
      wave_force_per <- (1.1 * C_p * 62.4 * stillwater_depth^2 + (2.42 * 62.4 - 0.5 * 62.4 * roof$SG) * stillwater_depth^2) # lb/ft
      wave_force <- wave_force_per * roof$D / 2 * 1.57 # lb
      flood_lateral_force <- hdyn_force + wave_force # lb
      
      hdyn_overturn <- hdyn_force * design_flood_level / 2 # lb-ft
      wave_overturn <- wave_force * ((0.84 + 1/3) * C_p + 0.888) / (1.1 * C_p + 1.92) * stillwater_depth # lb-ft
      flood_lateral_overturn <- hdyn_overturn + wave_overturn # lb-ft
      
      roof <- c(roof, flood_lateral_force = flood_lateral_force, flood_lateral_overturn = flood_lateral_overturn)
    }
  }
  
  # rain hydrostatics
  berm_rain_level <- max(min(roof$rain/12, roof$berm_height) - roof$tank_elevation, 0) * roof$berm_rain_ratio # ft
  rain_buoy_force <- pi * roof$D^2 / 4 * berm_rain_level * 62.4 # lb
  
  roof <- c(roof, rain_buoy_force = rain_buoy_force)
  return(roof)
}
