library(ggplot2)
library("RColorBrewer")

{
  perc = 0.7
  alal <- 2.5
  
  Degradation <- 1:4
  off <- (1:4) * perc
  normal <- (1:4) * (alal * perc - alal + 1)
  test <- (1:4) * perc + 1 - perc
  test2 <- 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1)
  
  da <- data.frame(Time = 1:4, Degradation = 1:4, 
                   test = (1:4) * perc + 1 - perc, 
                   off = (1:4) * perc,
                   normal = (1:4) * (alal * perc - alal + 1),
                   test2 = 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1))
  
  da2 <- data.frame(x1 = 0:3, x2 = 1:4, 
                    y1 = (0:3) * perc, y2 = (1:4) * perc + 1 - perc,
                    z1 = (0:3) * (alal * perc - alal + 1))
  da2$z2 <- da2$z1 + 1
  
  da3 <- data.frame(Time = 0:4, normal = (0:4) * (alal * perc - alal + 1))
  
  da4 <- data.frame(Time = 0:4, Degradation = 0:4)
  
  da5 <- data.frame(Time = rep(1:4, 3), 
                    point = c(Degradation, off, normal), 
                    shape = factor(rep(1:3, each = 4)))
  
  da.plot <- ggplot(data = da, aes(x = Time, y = Degradation))
  
  da.plot + #    geom_point(size = 3, shape = 1) + #圈
    #    geom_point(aes(y = off), size = 3, shape = 2) + #三角
    #    geom_point(aes(y = test), size = 3, shape = 4) + #×
    #    geom_point(aes(y = normal), size = 3, shape = 2) +  #三角
    #    geom_point(aes(y = test2), size = 3, shape = 4) + #×
    geom_linerange(aes(ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
    geom_path(color = "#009900", data = da4) + #高温
    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
    geom_point(data = da5, aes(y = point, shape = shape), size = 3) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          panel.grid.major = element_line(linetype = 5, color = "grey80"),
          legend.position = c(0.01, 0.8), legend.justification = c(0,1),
          axis.text = element_blank(), 
#          legend.background = element_rect(fill = NA)
          ) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") + 
    annotate("text", x = 3.7, y = 3.6,label = "Threshold", color = "grey40")
}

{
  perc = 0.7
  num <- 1000
  t <- (0:num) * pi / 2 / num
  sht <- sin(t) + sin(t * 5) / 11
  plot(t, sht)
  dt <- c(0, diff(sht))
  dxt <- rnorm(dt, dt, dt * 1.5)
  xt <- cumsum(dxt)
  plot(t, xt)
  xtn <- xt * perc
  xtoff <- rep(0, 1001)
  xtoff[(1:3) * 250] <- -0.05
  xtoff <- xt + cumsum(xtoff)
  plot(t, xtoff)
  
  da1 <- data.frame(t = t, x = xt, xx = xtn, xxx = xtoff)
  
  da.nonlinear.plot <- ggplot(data = da1, aes(x = t, y = x))
  da.nonlinear.plot + geom_path(aes(y = xxx) , color = "#0000ff") +
    geom_path(aes(y = xx) , color = "#666666") +
    geom_path() + 
    geom_hline(yintercept = 0.8, color = "grey50") + #threshold
#    geom_linerange(data = da, aes(x = Time, ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
#    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
#    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
#    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
#    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
#    geom_path(color = "#009900", data = da4) + #高温
#    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
#    geom_point(data = da5, aes(y = point, shape = shape), size = 3) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
                                         panel.grid.major = element_line(linetype = 5, color = "grey80"),
                                         legend.position = c(0.01, 0.8), legend.justification = c(0,1),
                                         axis.text = element_blank(), 
                                         #          legend.background = element_rect(fill = NA)
    ) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") + xlab("Time") + 
    annotate("text", x = 1.6, y = 0.8, label = "Threshold", color = "grey40")
  
  da.plot + #    geom_point(size = 3, shape = 1) + #圈
    #    geom_point(aes(y = off), size = 3, shape = 2) + #三角
    #    geom_point(aes(y = test), size = 3, shape = 4) + #×
    #    geom_point(aes(y = normal), size = 3, shape = 2) +  #三角
    #    geom_point(aes(y = test2), size = 3, shape = 4) + #×
    geom_linerange(aes(ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
    geom_path(color = "#009900", data = da4) + #高温
    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
    geom_point(data = da5, aes(y = point, shape = shape), size = 3) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          panel.grid.major = element_line(linetype = 5, color = "grey80"),
          legend.position = c(0.01, 0.8), legend.justification = c(0,1),
          axis.text = element_blank(), 
          #          legend.background = element_rect(fill = NA)
    ) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") + 
    annotate("text", x = 3.7, y = 3.6,label = "Threshold", color = "grey40")
}

{
  perc = 0.7
  alal <- 2.5
  
  Degradation <- 1:4
  off <- (1:4) * perc
  normal <- (1:4) * (alal * perc - alal + 1)
  test <- (1:4) * perc + 1 - perc
  test2 <- 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1)
  
  da <- data.frame(Time = 1:4, Degradation = 1:4, 
                   test = (1:4) * perc + 1 - perc, 
                   off = (1:4) * perc,
                   normal = (1:4) * (alal * perc - alal + 1),
                   test2 = 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1))
  
  da2 <- data.frame(x1 = 0:3, x2 = 1:4, 
                    y1 = (0:3) * perc, y2 = (1:4) * perc + 1 - perc,
                    z1 = (0:3) * (alal * perc - alal + 1))
  da2$z2 <- da2$z1 + 1
  
  da3 <- data.frame(Time = 0:4, normal = (0:4) * (alal * perc - alal + 1))
  
  da4 <- data.frame(Time = 0:4, Degradation = 0:4)
  
  da5 <- data.frame(Time = rep(1:4, 3), 
                    point = c(Degradation, off, normal), 
                    shape = factor(rep(1:3, each = 4)))
  
  da.comment <- data.frame(Measurement = c("Online", "Offline", "Degradation"), 
                           Time = 0.1, Degradation = c(3.4, 3.2, 3.1)-1, shape = c(1, 2, 4))
  
  da.plot <- ggplot(data = da, aes(x = Time, y = Degradation), show.legend = F) 
  
  da.plot + #    geom_point(size = 3, shape = 1) + #圈
    #    geom_point(aes(y = off), size = 3, shape = 2) + #三角
    #    geom_point(aes(y = test), size = 3, shape = 4) + #×
    #    geom_point(aes(y = normal), size = 3, shape = 2) +  #三角
    #    geom_point(aes(y = test2), size = 3, shape = 4) + #×
    geom_linerange(aes(ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
    geom_path(color = "#009900", data = da4) + #高温
    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
    geom_point(data = da5, aes(y = point, shape = shape), size = 3) +
#    geom_point(data = da.comment, aes(x = x, y = y, shape = c(1, 2, 4)), size = 3) +
#    geom_point(data = da.comment, fill = 0.1, size = 3, shape = c(1, 2, 0)) + 
#    ylim(0, 4) + xlim(0, 4) + 
#    scale_x_continuous(limits = c(0,4)) + scale_y_continuous(limits = c(0,4)) +
    coord_cartesian(xlim = c(0.19, 4), ylim = c(0.19, 4)) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
#          panel.grid.major.x = element_line(linetype = 5, color = "grey80"),
          panel.grid.major = element_blank(),
          legend.position = c(0.01, 0.87), legend.justification = c(0.04,1),
          axis.text = element_blank(), 
          legend.background = element_rect(fill = NA),
          panel.border = element_rect(color = "white", fill = F),
          axis.line = element_line(colour = "black"), 
          legend.key = element_blank(), 
          legend.text = element_text(size = 8)) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement/\nOffline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") +
    annotate("text", x = 3.2, y = 3.65,label = "Threshold") 
#    annotate("text", x = -Inf + 3, y = 3.6,label = "Online In-Chamber Measurement \nOffline Ex-Chamber Measurement with Non-Recovery", color = "grey40") + 
#    annotate("text", x = 3.2, y = 3.6,label = "Offline Ex-Chamber Measurement with Partial-Recovery", color = "grey40") + 
#    annotate("text", x = 3.2, y = 3.6,label = "Offline Ex-Chamber Measurement with Full-Recovery", color = "grey40") 
    
}

{
  perc = 0.7
  alal <- 2.5
  
  Degradation <- 1:4
  off <- (1:4) * perc
  normal <- (1:4) * (alal * perc - alal + 1)
  test <- (1:4) * perc + 1 - perc
  test2 <- 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1)
  
  da <- data.frame(Time = 1:4, Degradation = 1:4, 
                   test = (1:4) * perc + 1 - perc, 
                   off = (1:4) * perc,
                   normal = (1:4) * (alal * perc - alal + 1),
                   test2 = 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1))
  
  da2 <- data.frame(x1 = 0:3, x2 = 1:4, 
                    y1 = (0:3) * perc, y2 = (1:4) * perc + 1 - perc,
                    z1 = (0:3) * (alal * perc - alal + 1))
  da2$z2 <- da2$z1 + 1
  
  da3 <- data.frame(Time = 0:4, normal = (0:4) * (alal * perc - alal + 1))
  
  da4 <- data.frame(Time = 0:4, Degradation = 0:4)
  
  da5 <- data.frame(Time = rep(1:4, 3), 
                    point = c(Degradation, off, normal), 
                    shape = factor(rep(1:3, each = 4)))
  
  da.comment <- data.frame(Measurement = c("Online", "Offline", "Degradation"), 
                           Time = 0.13, Degradation = c(3.4, 3.1, 2.95), shape = c(1, 2, 4))
  
  da.plot <- ggplot(data = da, aes(x = Time, y = Degradation), show.legend = F) 
  
  da.plot + #    geom_point(size = 3, shape = 1) + #圈
    #    geom_point(aes(y = off), size = 3, shape = 2) + #三角
    #    geom_point(aes(y = test), size = 3, shape = 4) + #×
    #    geom_point(aes(y = normal), size = 3, shape = 2) +  #三角
    #    geom_point(aes(y = test2), size = 3, shape = 4) + #×
    geom_linerange(aes(ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
    geom_path(color = "#009900", data = da4) + #高温
    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
    geom_point(data = da5, aes(y = point, shape = shape), size = 3, show.legend = F) +
    #    geom_point(data = da.comment, aes(x = x, y = y, shape = c(1, 2, 4)), size = 3) +
    geom_point(data = da.comment, fill = 0.1, size = c(3.5, 3, 3), shape = c(1, 2, 0), show.legend = F) + 
    #    ylim(0, 4) + xlim(0, 4) + 
    #    scale_x_continuous(limits = c(0,4)) + scale_y_continuous(limits = c(0,4)) +
    coord_cartesian(xlim = c(0.19, 4), ylim = c(0.19, 4)) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          #          panel.grid.major.x = element_line(linetype = 5, color = "grey80"),
          panel.grid.major = element_blank(),
          axis.text = element_blank(), 
          panel.border = element_rect(color = "white", fill = F),
          axis.line = element_line(colour = "black")
    ) +
    scale_shape(solid = F) +
        scale_shape(solid = F, 
                    labels = c("Online In-Chamber Measurement", # /Offline Ex-Chamber Measurement with Non-Recovery", 
                               "Offline Ex-Chamber Measurement with Partial-Recovery", 
                               "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") +
    annotate("text", x = 3.2, y = 3.6,label = "Threshold", color = "grey40") + 
    annotate("text", x = -Inf, y = 3.4,label = "Online In-Chamber Measurement/", hjust = -0.141) + 
    annotate("text", x = -Inf, y = 3.25,label = "Offline Ex-Chamber Measurement with Non-Recovery", hjust = -0.089) + 
    annotate("text", x = -Inf, y = 3.1,label = "Offline Ex-Chamber Measurement with Partial-Recovery", hjust = -0.085) + 
    annotate("text", x = -Inf, y = 2.95,label = "Offline Ex-Chamber Measurement with Full-Recovery", hjust = -0.09) 
  
}

{
  perc = 0.72
  alal <- 2.6
  da <- data.frame(Time = 1:4, Degradation = 1:4, 
                   test = (1:4) * perc + 1 - perc, 
                   off = (1:4) * perc,
                   normal = (1:4) * (alal * perc - alal + 1),
                   test2 = 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1))
  
  da2 <- data.frame(x1 = 0:3, x2 = 1:4, 
                    y1 = (0:3) * perc, y2 = (1:4) * perc + 1 - perc,
                    z1 = (0:3) * (alal * perc - alal + 1))
  da2$z2 <- da2$z1 + 1
  
  da3 <- data.frame(Time = 0:4, normal = (0:4) * (alal * perc - alal + 1))
  
  da4 <- data.frame(Time = 0:4, Degradation = 0:4)
  
  da.plot <- ggplot(data = da, aes(x = Time, y = Degradation))
  
  da.plot  + geom_point(size = 3, shape = 1) +
    geom_linerange(aes(ymin = off, ymax = test, group = Time), 
                   linetype = "11", size = 1, color = "#FF000090") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), 
                 size = 1, color = "#0000ff90") +
    geom_point(aes(y = off), size = 3, shape = 2) + 
    geom_point(aes(y = test), size = 3, shape = 4) +
    geom_point(aes(y = normal), size = 3, shape = 2) + 
    geom_point(aes(y = test2), size = 3, shape = 4) +
    geom_hline(yintercept = 3.5, color = "grey50") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), linetype = "11",
                   size = 1, color = "#FF000090") +
    geom_path(data = da3, aes(y = normal), color = "#66666690", size = 1, linetype = "21") + 
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), size = 1
                 , color = "#6a13ef90", linetype = "21") + 
    geom_path(size = 1, color = "#00990090", linetype = "32", data = da4) + 
    theme(panel.background = element_rect(color = "black", fill = NA), 
          panel.grid.major = element_line(linetype = 5, color = "grey80"))
}

{
  colmap.da <- data.frame(col = c(colors(), 0, 0, 0))
  colmap.da$alp <- 1
  colmap.da$alp[658:660] <- 0
  colmap.da$x <- 1:22
  colmap.da$y <- rep(1:30, each = 22)
  colmap <- ggplot(data = colmap.da, aes(x = x, y = y, color = col), show.legend = F)
  colmap + geom_point(size = 10, show.legend = F, color = c(colors(), 0, 0, 0))
  
  # The palette with grey:
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # The palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # To use for fills, add
  scale_fill_manual(values=cbPalette)
  
  # To use for line and point colors, add
  scale_colour_manual(values=cbPalette)
}


{#nonlinear
  perc = 0.7
  alal <- 2.5
  
  Degradation <- 1:4
  off <- (1:4) * perc
  normal <- (1:4) * (alal * perc - alal + 1)
  test <- (1:4) * perc + 1 - perc
  test2 <- 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1)
  
  da <- data.frame(Time = 1:4, Degradation = 1:4, 
                   test = (1:4) * perc + 1 - perc, 
                   off = (1:4) * perc,
                   normal = (1:4) * (alal * perc - alal + 1),
                   test2 = 1 - (alal * perc - alal + 1) + (1:4) * (alal * perc - alal +1))
  
  da2 <- data.frame(x1 = 0:3, x2 = 1:4, 
                    y1 = (0:3) * perc, y2 = (1:4) * perc + 1 - perc,
                    z1 = (0:3) * (alal * perc - alal + 1))
  da2$z2 <- da2$z1 + 1
  
  da3 <- data.frame(Time = 0:4, normal = (0:4) * (alal * perc - alal + 1))
  
  da4 <- data.frame(Time = 0:4, Degradation = 0:4)
  
  da5 <- data.frame(Time = rep(1:4, 3), 
                    point = c(Degradation, off, normal), 
                    shape = factor(rep(1:3, each = 4)))
  
  da.comment <- data.frame(Measurement = c("Online", "Offline", "Degradation"), 
                           Time = 0.13, Degradation = c(3.4, 3.1, 2.95), shape = c(1, 2, 4))
  
  da.plot <- ggplot(data = da, aes(x = Time, y = Degradation), show.legend = F) 
  
  da.plot + #    geom_point(size = 3, shape = 1) + #圈
    #    geom_point(aes(y = off), size = 3, shape = 2) + #三角
    #    geom_point(aes(y = test), size = 3, shape = 4) + #×
    #    geom_point(aes(y = normal), size = 3, shape = 2) +  #三角
    #    geom_point(aes(y = test2), size = 3, shape = 4) + #×
    geom_linerange(aes(ymin = off, ymax = test, group = Time), color = "#0000ff", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = y1, xend = x2, yend = y2), color = "#0000ff") +
    geom_linerange(aes(ymin = normal, ymax = test2, group = Time), color = "#6a13ef", linetype = "33") +
    geom_segment(data = da2, aes(x = x1, y = z1, xend = x2, yend = z2), color = "#6a13ef") + 
    geom_path(data = da3, aes(y = normal), color = "#666666") + #低温
    geom_path(color = "#009900", data = da4) + #高温
    geom_hline(yintercept = 3.5, color = "grey50") + #threshold
    geom_point(data = da5, aes(y = point, shape = shape), size = 3, show.legend = F) +
    #    geom_point(data = da.comment, aes(x = x, y = y, shape = c(1, 2, 4)), size = 3) +
    geom_point(data = da.comment, fill = 0.1, size = c(3.5, 3, 3), shape = c(1, 2, 0), show.legend = F) + 
    #    ylim(0, 4) + xlim(0, 4) + 
    #    scale_x_continuous(limits = c(0,4)) + scale_y_continuous(limits = c(0,4)) +
    coord_cartesian(xlim = c(0.19, 4), ylim = c(0.19, 4)) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          #          panel.grid.major.x = element_line(linetype = 5, color = "grey80"),
          panel.grid.major = element_blank(),
          axis.text = element_blank(), 
          panel.border = element_rect(color = "white", fill = F),
          axis.line = element_line(colour = "black")
    ) +
    scale_shape(solid = F) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement", # /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") +
    annotate("text", x = 3.2, y = 3.6,label = "Threshold", color = "grey40") + 
    annotate("text", x = -Inf, y = 3.4,label = "Online In-Chamber Measurement/", hjust = -0.141) + 
    annotate("text", x = -Inf, y = 3.25,label = "Offline Ex-Chamber Measurement with Non-Recovery", hjust = -0.089) + 
    annotate("text", x = -Inf, y = 3.1,label = "Offline Ex-Chamber Measurement with Partial-Recovery", hjust = -0.085) + 
    annotate("text", x = -Inf, y = 2.95,label = "Offline Ex-Chamber Measurement with Full-Recovery", hjust = -0.09) 
  
}

{
  n <- 400
  shape <- ((1:n)+1) ^ (1/2) + sin(1:n) * 0.03
  deg.del <- rnorm(n, 0, 0.08) 
  deg <- cumsum(deg.del) + shape
  no <- deg * 0.6
 # no <- no + no[1] + deg[1]
  off1 <- deg - 1.5 * c(rep(0:3, each = 100)[-1], 4)
  off2 <- c(deg[1:99], deg[100:199] - deg[100] + no[100],
            deg[200:299] - deg[200] + no[200],
            deg[300:399] - deg[300] + no[300], no[400])
  plot(1:n, deg)
  lines(1:n, no)
  lines(1:n, off1)
  lines(1:n, off2)
  
  da <- data.frame(Time = 1:n, Degradation = c(deg, no, off1, off2), 
                   type = factor(rep(1:4, each = n)))
  da1 <- data.frame(Time = 1:n, Degradation = deg)
  da2 <- data.frame(Time = 1:n, Degradation = no)
  da3 <- data.frame(Time = 1:n, Degradation = off1)
  da4 <- data.frame(Time = 1:n, Degradation = off2)
  da5 <- data.frame(x1 = (1:4)*100 - 1, x2 = (1:4)*100, 
                    y1 = off1[(1:4)*100 - 1], y2 = off1[(1:4)*100],
                    y3 = off2[(1:4)*100 - 1], y4 = off2[(1:4)*100])
  da6 <- data.frame(x = c((1:4)*100 - 1, (1:4)*100, (1:4)*100),
                    y = c(deg[(1:4)*100 - 1], off1[(1:4)*100], off2[(1:4)*100]),
                    shape = factor(rep(c(1, 2, 4), each = 4)))
  da7 <- data.frame(Measurement = c("Online", "Offline", "Degradation"), 
                    Time = -6, Degradation = max(deg) * c(0.975, 0.89, 0.83), shape = c(1, 2, 4))
  
  da.plot <- ggplot(data = da1, aes(x = Time, y = Degradation), size = 1, show.legend = F)
  da.plot + geom_path(da = da3, color = "#0000ff") + 
    geom_path(da = da4, color = "#6a13ef") +
    geom_path(da = da1, color = "#009900") + 
    geom_path(da = da2, color = "#666666") +
    geom_segment(data = da5, aes(x = x1, y = y1, xend = x2, yend = y2), color = "white") + 
    geom_segment(data = da5, aes(x = x1, y = y3, xend = x2, yend = y4), color = "white") +
    geom_segment(data = da5, aes(x = x1, y = y3, xend = x2, yend = y4), 
                 color = "#6a13ef", linetype = "33") +
      geom_segment(data = da5, aes(x = x1, y = y1, xend = x2, yend = y2), 
                   color = "#0000ff", linetype = "33") +
    geom_hline(yintercept = max(deg) * 0.75, color = "grey50", linetype = "33") + #threshold
    geom_point(data = da6, aes(y = y, x = x, shape = shape), size = 3, show.legend = F) + 
    geom_point(data = da7, fill = 0.1, size = c(3.5, 3, 3), shape = c(1, 2, 0), show.legend = F) + 
    coord_cartesian(xlim = c(0, 401), ylim = c(min(deg), max(deg))) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          #          panel.grid.major.x = element_line(linetype = 5, color = "grey80"),
          panel.grid.major = element_blank(),
          axis.text = element_blank(), 
          panel.border = element_rect(color = "white", fill = F),
          axis.line = element_line(colour = "black")
    ) +
    scale_shape(solid = F) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement", # /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") +
    annotate("text", x = 350, y = max(deg) * 0.78, label = "Threshold", color = "grey40") + 
    annotate("text", x = -Inf, y = max(deg) * 0.99, size = 3, 
             label = "Online In-Chamber Measurement/", hjust = -0.141) + 
    annotate("text", x = -Inf, y = max(deg) * 0.96, size = 3,
             label = "Offline Ex-Chamber Measurement with Non-Recovery", hjust = -0.089) + 
    annotate("text", x = -Inf, y = max(deg) * 0.89, size = 3,
             label = "Offline Ex-Chamber Measurement with Partial-Recovery", hjust = -0.085) + 
    annotate("text", x = -Inf, y = max(deg) * 0.83, size = 3,
             label = "Offline Ex-Chamber Measurement with Full-Recovery", hjust = -0.09) 
}

{
  n <- 400
  shape <- ((1:n)+1) ^ (1/2) + sin(1:n) * 0.03
  deg.del <- rnorm(n, 0, 0.08) 
  deg <- cumsum(deg.del) + shape
  no <- deg * 0.6
  # no <- no + no[1] + deg[1]
  off1 <- deg - 1.5 * c(rep(0:3, each = 100)[-1], 4)
  off2 <- c(deg[1:99], deg[100:199] - deg[100] + no[100],
            deg[200:299] - deg[200] + no[200],
            deg[300:399] - deg[300] + no[300], no[400])
  plot(1:n, deg)
  lines(1:n, no)
  lines(1:n, off1)
  lines(1:n, off2)
  
  da <- data.frame(Time = 1:n, Degradation = c(deg, no, off1, off2), 
                   type = factor(rep(1:4, each = n)))
  da1 <- data.frame(Time = 1:n, Degradation = deg)
  da2 <- data.frame(Time = 1:n, Degradation = no)
  da3 <- data.frame(Time = 1:n, Degradation = off1)
  da4 <- data.frame(Time = 1:n, Degradation = off2)
  da5 <- data.frame(x1 = (1:4)*100 - 1, x2 = (1:4)*100, 
                    y1 = off1[(1:4)*100 - 1], y2 = off1[(1:4)*100],
                    y3 = off2[(1:4)*100 - 1], y4 = off2[(1:4)*100])
  da6 <- data.frame(x = c((1:4)*100 - 1, (1:4)*100, (1:4)*100),
                    y = c(deg[(1:4)*100 - 1], off1[(1:4)*100], off2[(1:4)*100]),
                    shape = factor(rep(c(1, 2, 4), each = 4)))
  da7 <- data.frame(Measurement = c("Online", "Offline", "Degradation"), 
                    Time = 190, Degradation = max(deg) * c(0.275, 0.19, 0.13), shape = c(1, 2, 4))
  da8 <- data.frame(x1 = c(400, 400) + 1, x2 = c(400, 400) + 7, 
                    y1 = off1[c(399, 400)], y2 = off1[c(399, 400)])
  
  da.plot <- ggplot(data = da1, aes(x = Time, y = Degradation), size = 1, show.legend = F)
  da.plot + geom_path(da = da3, color = "#0000ff") + 
    geom_path(da = da4, color = "#6a13ef") +
    geom_path(da = da1, color = "#009900") + 
    geom_path(da = da2, color = "#666666") +
    geom_segment(data = da5, aes(x = x1, y = y1, xend = x2, yend = y2), color = "white") + 
    geom_segment(data = da5, aes(x = x1, y = y3, xend = x2, yend = y4), color = "white") +
    geom_segment(data = da5, aes(x = x1, y = y3, xend = x2, yend = y4), 
                 color = "#6a13ef", linetype = "33") +
    geom_segment(data = da5, aes(x = x1, y = y1, xend = x2, yend = y2), 
                 color = "#0000ff", linetype = "33") +
    geom_hline(yintercept = max(deg) * 0.8, color = "grey50", linetype = "33") + #threshold
    geom_point(data = da6, aes(y = y, x = x, shape = shape), size = 3, show.legend = F) + 
    geom_point(data = da7, fill = 0.1, size = c(3.5, 3, 3), shape = c(1, 2, 0), show.legend = F) + 
    coord_cartesian(xlim = c(0, 401), ylim = c(min(deg), max(deg))) +
    theme(panel.background = element_rect(color = "black", fill = NA), 
          #          panel.grid.major.x = element_line(linetype = 5, color = "grey80"),
          panel.grid.major = element_blank(),
          axis.text = element_blank(), 
          panel.border = element_rect(color = "white", fill = F),
          axis.line = element_line(colour = "black")
    ) +
    scale_shape(solid = F) +
    scale_shape(solid = F, 
                labels = c("Online In-Chamber Measurement", # /Offline Ex-Chamber Measurement with Non-Recovery", 
                           "Offline Ex-Chamber Measurement with Partial-Recovery", 
                           "Offline Ex-Chamber Measurement with Full-Recovery")) + 
    labs(shape = NULL) + ylab("Degradation Level") +
    annotate("text", x = 100, y = max(deg) * 0.82, label = "Threshold", color = "grey40") + 
    annotate("text", x = 180, y = max(deg) * 0.29, size = 3, 
             label = "Online In-Chamber Measurement/", hjust = -0.141) + 
    annotate("text", x = 180, y = max(deg) * 0.26, size = 3,
             label = "Offline Ex-Chamber Measurement with Non-Recovery", hjust = -0.089) + 
    annotate("text", x = 180, y = max(deg) * 0.19, size = 3,
             label = "Offline Ex-Chamber Measurement with Partial-Recovery", hjust = -0.085) + 
    annotate("text", x = 180, y = max(deg) * 0.13, size = 3,
             label = "Offline Ex-Chamber Measurement with Full-Recovery", hjust = -0.09) +
    geom_segment(data = da8, aes(x = x1, y = y1, xend = x2, yend = y2)) + 
    geom_segment(aes(x = mean(as.numeric(da8[1, 1:2])), y = da8[1, 3], 
                     xend = mean(as.numeric(da8[1, 1:2])), yend = da8[2, 3]), 
                 arrow = arrow(length = unit(0.2, "cm"), ends = "both")) + 
    annotate("text", x = mean(as.numeric(da8[1, 1:2])) - 3, y = mean(da8[2, 3]) - 0.4, size = 3, angle = 0,
             label = "Recovery")
#  geom_segment(aes(x = 5, y = 30, xend = 3.5, yend = 25),
#               arrow = arrow(length = unit(0.5, "cm")))
}


