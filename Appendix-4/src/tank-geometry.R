# tank geometry and such

# one foot method to calculate course thicknesses
# assumptions: tank designed with SG = 1 (water)
one_foot_method <- function(roof) {
  course_heights <- rep_len(8, roof$H %/% 8) # ft
  if (roof$H %% 8 != 0) course_heights <- c(course_heights, roof$H %% 8)
  
  Dlim <- c(0, 50, 120, 200)
  tklim <- c(3/16, 1/4, 5/16, 3/8)
  t_min <- tklim[findInterval(roof$D, Dlim)]
  t_d <- 2.6 * roof$D * (rev(cumsum(rev(course_heights))) - 1) * 1 / 23200
  t_t <- 2.6 * roof$D * (rev(cumsum(rev(course_heights))) - 1) / 24900
  course_t <- pmax(t_min, t_d, t_t)
  course_t <- as.vector(ceiling(course_t * 16) / 16) # in
  
  roof <- c(roof, list(course_heights = course_heights, course_t = course_t))
  return(roof)
}

# tank weight calculation
# assumptions: bottom thickness 1/4 in, weight of small roofs is same as flat plate
weight_calculation <- function(roof) {
  bottom_t <- 1/4 # in
  roof_t <- 3/16 # in
  a36_density <- 0.284 # lb/in3
  
  weight_bottom <- pi * roof$D^2 / 4 * bottom_t * a36_density * 12^2 # lb
  weight_shell <- pi * roof$D * sum(roof$course_heights * roof$course_t) * a36_density * 12^2 # lb
  
  # roof weight applicable if cone roof; girder weight applicable if open/dome roof
  weight_roof <- ifelse(roof$D < 30, pi * roof$D ^ 2 / 4 * roof_t * a36_density * 12^2, 1.014 * roof$D^2.6229) * (roof$roof_type == "cone") # lb
  weight_girder <- ifelse(roof$D < 50, 0, 3.1927 * roof$D^2 - 108.13 * roof$D - 993.48) * (roof$roof_type != "cone") # lb
  
  weight_tank <- weight_bottom + weight_shell + weight_roof + weight_girder # lb
  
  roof <- c(roof, weight_bottom = weight_bottom, weight_shell = weight_shell,
            weight_roof = weight_roof, weight_girder = weight_girder, weight_tank = weight_tank)
  return(roof)
}
