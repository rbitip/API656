# main functions

hurricaner_main <- function(roof, summary = TRUE){
  roof <- roof %>%
    add_assumptions(assumptions) %>%
    one_foot_method() %>%
    weight_calculation() %>%
    hydro_calculation() %>%
    wind_calculation()
  if (summary) summary_print(roof)
  return(roof)
}

assumptions <- list(
  # general
  roof_type = "cone",
  SG = 1.0,
  # SG = 0.7,
  risk_category = 3,
  mu = 0.4,
  # flooding
  terminal_elevation = 0,
  berm_height = 0,
  tank_elevation = 0,
  wave_bool = FALSE,
  berm_rain_ratio = 1,
  water_drag = 2,
  # wind
  exposure_category = "C"
)

# adds assumed values, if not specified in the input list
# assumptions: see assumption document
add_assumptions <- function(roof, assumptions) {
  # simple assumptions from assumption list
  for (x in seq_len(length(assumptions))) {
    if (!names(assumptions[x]) %in% names(roof)) roof <- c(roof, assumptions[x])
  }
  # complicated assumptions
  # add calc for dome roof height calc AKA dome rise, f
  if (!"f" %in% names(roof)) {
    # roof pitch now set to 1/16
    # if (roof$roof_type == "cone") roof$f <- roof$D / 2 * (2/12)
    if (roof$roof_type == "cone") roof$f <- roof$D / 2 * (1/16)
    # dome roof is similarly lowered
    else if (roof$roof_type == "dome") roof$f <- round(1 / 10 * (8 - sqrt(39)) * roof$D, 1)
    # else if (roof$roof_type == "dome") roof$f <- round(1 / 10 * (8 - sqrt(39)) * roof$D * 1/2, 1)
    else roof$f <- 0
  }
  
  return(roof)
}


summary_print <- function(roof){
  
  cat(paste0(
    "flooding ---- ", roof$flood_over_berm, "\n"
  ))
  if (roof$flood_over_berm) {
    cat(paste0(
      "flood Fx: ", roof$flood_lateral_force, "\n",
      "flood Fy: ", roof$flood_buoy_force, "\n",
      "flood M: ", roof$flood_lateral_overturn, "\n"
    ))
  }
  
  cat(paste0(
    "rain ---- ", "\n",
    "rain Fy: ", roof$rain_buoy_force, "\n"
  ))
  
  cat(paste0(
    "wind ---- ", "\n",
    "wind Fx: ", roof$Fx_wind, "\n",
    "wind Fy: ", roof$Fy_wind, "\n",
    "wind M: ", roof$M_wind, "\n"
  ))
}

summary_calc <- function(roof){
  # TBD
  # note: ASD load combination for wind is 60% of calculated
  # consider horizontal, vertical, and overturning
  # consider tank weight, flood buoyancy, rain buoyancy, wind uplift
  # consider flood loading, wind loading, 
  # consider flood, rain overturning, wind overturning
  # consider tank weight resistance to all of these
  # consider how much additional product to add friction and/or overturning resistance
}

