# API 656 - envelope

# finding the envelope of safe vs unsafe tank setups and loadings

# concerns: buoyancy failure, overturning failure, sliding failure

# NO breaking waves, moving water, rainfall
# SET flood height (NOT BFE), wind speed, diameter, roof type

# wrapper function
analysis <- function(roof){
  roof <- helper(roof)
  
  # loadings in kips
  return(list(LNW = roof$LNW, Y = vertical(roof)/1000, X = horizontal(roof)/1000, M = overturning(roof)/1000))
}

helper <- function(roof){
  roof$LNW <- roof$flood_elevation - roof$level
  roof$weight_product <- roof$level * pi * roof$D^2 / 4 * 62.4
  # OK with negative friction forces, tank will fail regardless if friction = 0
  # roof$friction <- max((roof$weight_tank + roof$weight_product - roof$flood_buoy_force) * 0.4, 0)
  roof$friction <- (roof$weight_tank + roof$weight_product - roof$flood_buoy_force - roof$Fy_wind) * 0.4
  
  return(roof)
}

# vertical forces check (buoyancy)
vertical <- function(roof){
  # tank weight + product weight + buoyancy + wind uplift
  return(-roof$weight_tank - roof$weight_product + roof$flood_buoy_force + roof$Fy_wind)
}

# horizontal force check (sliding)
horizontal <- function(roof){
  return(-roof$friction + roof$Fx_wind)
}

# moment check (overturning)
overturning <- function(roof){
  return( (-roof$weight_tank - roof$weight_product + roof$flood_buoy_force) * roof$D / 2 + roof$M_wind )
}