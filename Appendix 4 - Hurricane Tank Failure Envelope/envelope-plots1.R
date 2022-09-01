# API 656 - envelope
# plotting (1)
# AW @ PEMY

# get data from envelope-data.R

library(here)
library(ggplot2)
library(RColorBrewer)
# library(car)

# data already generated and saved
df <- readRDS(here("hurricaner_data_2021-04-13.rds"))

# df columns: D, LNW (water level), wind speed, roof, buoyancy, sliding, overturning

# # scatter plot matrix ----
# 
# scatterplotMatrix(~D + LNW + buoyancy + sliding + overturning | roof,
#                   data = df, main = "Scatterplot Matrix for Hurricaner Envelopes", col = brewer.pal(3, "Pastel1")[unclass(df$roof)])

# wind-related plots ----

df1 <- df[df$LNW == 0 & df$wind_speed == 120,]
pl1 <- ggplot(data = df1, aes(x = D, y = overturning, color = roof)) + 
  geom_line() + 
  labs(title = "Net Moment With Only Wind Loadings", caption = "LNW = 0 ft, Wind Speed = 120 mph; moment below zero is good") + 
  xlab("diameter, ft") + ylab("net moment, kip-ft")
print(pl1)

df2 <- df[df$LNW == 0 & df$wind_speed == 120,]
pl2 <- ggplot(data = df2, aes(x = D, y = wind_Fy, color = roof)) + 
  geom_line() +
  labs(title = "Total Vertical Wind Force", caption = "LNW = 0 ft, Wind Speed = 120 mph") + 
  xlab("diameter, ft") + ylab("vertical force, kip")
print(pl2)


df3 <- df[df$LNW == 0 & df$wind_speed == 220,]
pl3 <- ggplot(data = df3, aes(x = D, y = wind_Fx, color = roof)) + 
  geom_line() + geom_jitter(alpha = 0.4) +
  labs(title = "Total Horizontal Wind Force", subtitle = "Shell and Roof", caption = "LNW = 0 ft, Wind Speed = 120 mph") + 
  xlab("diameter, ft") + ylab("horizontal force, kip")
print(pl3)

df4 <- df[df$LNW == 0 & df$wind_speed == 120,]
pl4 <- ggplot(data = df4, aes(x = D, y = wind_M, color = roof)) + 
  geom_line() +
  labs(title = "Total Wind Overturning Moment", subtitle = "Shell and Roof", caption = "LNW = 0 ft, Wind Speed = 120 mph") + 
  xlab("diameter, ft") + ylab("overturning moment, kip-ft")
print(pl4)

# buoyancy plot ----

df5 <- df[df$LNW == 0 & df$wind_speed == 120 & df$roof %in% c("cone", "dome") & !is.na(df$roof),]
df5$roof <- as.vector(df5$roof)
df5$roof[df5$roof == "dome"] <- "dome/open"
df5$LNW <- df5$weight_tank / 62.4 / (pi * df5$D^2 / 4)
pl5 <- ggplot(data = df5, aes(x = D, y = LNW, color = roof)) + 
  # geom_point(position = position_dodge(width = 5))
  geom_line() +
  # geom_jitter() + 
  labs(title = expression(paste("", RFL[t], "")), subtitle = "RFL at Buoyancy Failure") + 
  xlab("diameter, ft") + ylab("relative flood level, ft")
print(pl5)

# contour plots ----

# library(metR)
# speed range is 90, 120, 170, 220
rft <- "open"
{spd <- 220
df6 <- df[df$wind_speed == spd & df$roof == rft,]
pl6 <- ggplot(data = df6, aes(x = D, y = LNW, z = sliding)) +
  geom_contour_filled() +
  geom_contour(color = "white", size = 2, breaks = c(0)) +
  labs(title = paste0("Net Sliding Forces For Open Roofs, ", spd, " mph wind"), caption = "positive net sliding forces = failure", fill = "sliding force, kip") + 
  # labs(title = "Net Sliding Forces For Dome Roofs, 170 mph wind", caption = "positive net sliding forces = failure; Wind Fx minimum of 0", fill = "sliding force, kip") + 
  xlab("diameter, ft") + ylab("level, net water, ft")
print(pl6)
}

df7 <- df[df$wind_speed == 170 & df$roof == "dome",]
pl7 <- ggplot(data = df7, aes(x = D, y = LNW, z = overturning)) +
  geom_contour_filled() +
  geom_contour(color = "white", size = 2, breaks = c(0)) +
  labs(title = "Net Moment For Dome Roofs, 170 mph wind", caption = "positive net moment = failure", fill = "moment, kip-ft") + 
  xlab("diameter, ft") + ylab("level, net water, ft")
print(pl7)

df8 <- df[df$wind_speed == 170 & df$roof == "dome",]
pl8 <- ggplot(data = df8, aes(x = D, y = LNW, z = buoyancy)) +
  geom_contour_filled() +
  geom_contour(color = "white", size = 2, breaks = c(0)) +
  labs(title = "Net Vertical Forces For Dome Roofs, 170 mph wind", caption = "positive net vertical forces = failure", fill = "force, kip") + 
  xlab("diameter, ft") + ylab("level, net water, ft")
print(pl8)
