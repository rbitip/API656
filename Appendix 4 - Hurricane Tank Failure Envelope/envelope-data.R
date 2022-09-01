# API 656 - envelope
# script to generate data
# AW @ PEMY

library(here)
library(magrittr)

# call source function on source code files in src
invisible(
  lapply(
    list.files(here("src")),
    function(x){source(here("src",x))}
  )
)

# SET flood height (NOT BFE), wind speed, diameter, roof type, internal water height
# everything else assumed

# this is the base roof we are using
sample_roof <- list(
  # general
  D = 100,
  H = 48,
  level = 10,
  # rainfall
  flood_elevation = 10,
  rain = 0,
  water_speed = 0,
  # wind
  wind_speed = 120,
  risk_category = 3,
  SG = 1
)

# set range of parameters to combine
vals <- list()
vals$D <- seq(from = 40, to = 300, by = 10)
# vals$D <- seq(from = 10, to = 300, by = 5)
vals$flood <- seq(from = 7, to = 13, by = 0.02)
# vals$flood <- seq(from = 1, to = 13, by = 0.02)
vals$wind <- c(90, 120, 170, 220)
vals$roof <- c("cone", "dome", "open")

# vals$D <- seq(from = 20, to = 20, by = 5)
# vals$flood <- seq(from = 1, to = 15, by = 0.02) # make level = 15 in sample_roof list
# vals$wind <- c(220)
# vals$roof <- c("dome")

# preallocate vectors
len <- prod(sapply(vals, length))
D_vec <- vector(length = len)
LNW_vec <- vector(length = len)
wind_vec <- vector(length = len)
roof_vec <- vector(length = len)
vertical_vec <- vector(length = len)
horizontal_vec <- vector(length = len)
overturning_vec <- vector(length = len)
windy_vec <- vector(length = len)
windx_vec <- vector(length = len)
windm_vec <- vector(length = len)
weight_vec <- vector(length = len)

x <- 1

# big loop over all the parameters, running with each combination
for(D in vals$D){
  for(flood in vals$flood){
    for(wind in vals$wind){
      for(roof in vals$roof){
        
        rf <- sample_roof
        
        rf$D <- D
        rf$flood_elevation <- flood
        rf$wind_speed <- wind
        rf$roof_type <- roof
        
        D_vec[x] <- D
        wind_vec[x] <- wind
        roof_vec[x] <- roof
        
        rf <- hurricaner_main(rf, summary = FALSE)
        out <- analysis(rf)
        
        LNW_vec[x] <- out$LNW
        
        vertical_vec[x] <- out$Y
        horizontal_vec[x] <- out$X
        overturning_vec[x] <- out$M
        windy_vec[x] <- rf$Fy_wind/1000
        windx_vec[x] <- rf$Fx_wind/1000
        windm_vec[x] <- rf$M_wind/1000
        weight_vec[x] <- rf$weight_tank
        
        x <- x + 1
        
      }
    }
  }
}

# make data frame
df <- data.frame(D_vec, LNW_vec, wind_vec, roof_vec, vertical_vec, horizontal_vec, overturning_vec, windy_vec, windx_vec, windm_vec, weight_vec)
colnames(df) <- c("D", "LNW", "wind_speed", "roof", "buoyancy", "sliding", "overturning", "wind_Fy", "wind_Fx", "wind_M", "weight_tank")
