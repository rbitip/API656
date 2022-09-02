# API 656 - envelope
# plotting (2) to find which failure mode occurs first
# AW @ PEMY

# get data from envelope-data.R

library(here)
library(ggplot2)
library(gridExtra)
# library(RColorBrewer)

# data already generated and saved
df <- readRDS(here("hurricaner_data_2021-04-08.rds"))

len <- length(unique(df$wind_speed)) * length(unique(df$roof)) * length(unique(df$D)) * 3
lnw <- vector(length = len)
d <- vector(length = len)
failure_mode <- vector(length = len)
roofs <- vector(length = len)
wind_speeds <- vector(length = len)
counter <- 1

for(wind_speed in unique(df$wind_speed)){
  for(roof in unique(df$roof)){
    for(D in unique(df$D)){
      dfn <- df[df$wind_speed == wind_speed & df$roof == roof & df$D == D,]
      x <- dfn[which.max(dfn$LNW[dfn$sliding <= 0]),]
      lnw[counter] <- x$LNW[1]
      d[counter] <- x$D[1]
      roofs[counter] <- x$roof[1]
      wind_speeds[counter] <- x$wind_speed[1] 
      failure_mode[counter] <- "sliding"
      
      counter <- counter + 1
      
      x <- dfn[which.max(dfn$LNW[dfn$buoyancy <= 0]),]
      lnw[counter] <- x$LNW[1]
      d[counter] <- x$D[1]
      roofs[counter] <- x$roof[1]
      wind_speeds[counter] <- x$wind_speed[1] 
      failure_mode[counter] <- "buoyancy"
      
      counter <- counter + 1
      
      x <- dfn[which.max(dfn$LNW[dfn$overturning <= 0]),]
      lnw[counter] <- x$LNW[1]
      d[counter] <- x$D[1]
      roofs[counter] <- x$roof[1]
      wind_speeds[counter] <- x$wind_speed[1] 
      failure_mode[counter] <- "overturning"
      
      counter <- counter + 1
    }
  }
}

df1 <- data.frame(D = d, LNW = lnw, mode = failure_mode, roof = roofs, wind_speed = wind_speeds)
df1$roof[df1$roof == 1] <- "cone"
df1$roof[df1$roof == 2] <- "dome"
df1$roof[df1$roof == 3] <- "open"

# df1[!is.na(df1$D) & df1$roof == "cone" & df1$wind_speed == 90 & df1$D == 40,]

# individual plots ----
# speed range is 90, 120, 170, 220
rft <- "cone"
{spd <- 120
pl1 <- ggplot(data = df1[df1$roof == rft & df1$wind_speed == spd & !is.na(df1$wind_speed),], aes(x = D, y = LNW, color = mode)) + 
  geom_line() + 
  labs(title = "Failure Modes Overview", subtitle = paste0(rft, " roof tank, ", spd, " mph winds"),
       caption = "lowest LNW for which a tank fails by the given mode", fill = "failure mode") +
  # ylim(c(-5,1)) +
  xlab("diameter, ft") + ylab("level, net water, ft")
  
print(pl1)
}

# # grid of plots, diff y-scales ----
# # method using gridExtra; deprecated
# 
# #extract legend
# 
# pl1 <- ggplot(data = df1[df1$roof == rft & df1$wind_speed == spd & !is.na(df1$wind_speed),], aes(x = D, y = LNW, color = mode)) + 
#   geom_line() + 
#   labs(subtitle = paste0(rft, " roof tank, ", spd, " mph winds"),
#        caption = "lowest LNW for which a tank fails by the given mode", fill = "failure mode") +
#   theme(legend.position = "bottom") +
#   # ylim(c(-5,1)) +
#   xlab("diameter, ft") + ylab("level, net water, ft")
# 
# #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
# g_legend <- function(a.gplot){
#   tmp <- ggplot_gtable(ggplot_build(a.gplot))
#   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#   legend <- tmp$grobs[[leg]]
#   return(legend)}
# 
# mylegend <- g_legend(pl1)
# 
# pls <- vector(mode = "list", length = 3 * 4) 
# x1 <- 1
# for(rft in c("cone", "dome", "open")){
#   for(spd in c(90, 120, 170, 220)){
#     pls[[x1]] <- ggplot(data = df1[df1$roof == rft & df1$wind_speed == spd & !is.na(df1$wind_speed),], aes(x = D, y = LNW, color = mode)) + 
#       geom_line() + 
#       labs(subtitle = paste0(rft, " roof tank, ", spd, " mph winds"),
#            # caption = "lowest LNW for which a tank fails by the given mode",
#            fill = "failure mode") +
#       theme(legend.position = "none") +
#       # ylim(c(-5,1)) +
#       xlab("diameter, ft") + ylab("level, net water, ft")
#     
#     x1 <- x1 + 1
#   }
# }
# 
# pl1 <- grid.arrange(
#   arrangeGrob(pls[[1]], pls[[2]], pls[[3]], pls[[4]], pls[[5]], pls[[6]], pls[[7]],
#                pls[[8]], pls[[9]], pls[[10]], pls[[11]], pls[[12]],
#               nrow = 3, ncol = 4),
#   mylegend, nrow = 2, ncol = 1, top = "Failure Modes Overview", heights = c(10,1))
# 
# # grid of plots, same y-scale ----
# 
# pl1 <- ggplot(data = df1[!is.na(df1$wind_speed) & !is.na(df1$roof),], aes(x = D, y = LNW, color = mode)) + 
#   geom_line() + 
#   labs(title = "Failure Modes Overview", subtitle = "wind speeds in mph",
#        caption = "lowest LNW for which a tank fails by the given mode", fill = "failure mode") +
#   # ylim(c(-5,1)) +
#   xlab("diameter, ft") + ylab("level, net water, ft") +
#   facet_grid(roof ~ wind_speed, labeller = label_both)
# 
# print(pl1)

# grid of plots, diameter range, WRAP ----

pl1 <- ggplot(data = df1[!is.na(df1$wind_speed) & !is.na(df1$roof) & df1$D >= 40,], aes(x = D, y = LNW, color = mode)) + 
  geom_line() + 
  labs(title = "Failure Modes Overview, RFL*", subtitle = "wind speeds in mph",
       caption = "RFL* is the lowest RFL for which a tank fails by any of the modes", fill = "failure mode") +
  # theme(legend.position = "bottom") +
  # ylim(c(-5,1)) +
  xlab("diameter, ft") + ylab("RFL, ft") +
  facet_wrap(roof ~ wind_speed, labeller = label_both, scales = "free")

print(pl1)

# grid of plots, diameter range, GRID ----

pl1 <- ggplot(data = df1[!is.na(df1$wind_speed) & !is.na(df1$roof) & df1$D >= 40,], aes(x = D, y = LNW, color = mode)) + 
  geom_line() + 
  labs(title = "Failure Modes Overview, RFL*", subtitle = "wind speeds in mph",
       caption = "RFL* is the lowest RFL for which a tank fails by any of the modes", fill = "failure mode") +
  # theme(legend.position = "bottom") +
  # ylim(c(-5,1)) +
  xlab("diameter, ft") + ylab("RFL, ft") +
  facet_grid(roof ~ wind_speed, labeller = label_both)

print(pl1)
