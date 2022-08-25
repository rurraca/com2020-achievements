library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(grid)
library(gridExtra)
library(RColorBrewer)
#library(graticule)
library(scales)
library(ggthemes)
#library(ggpubr) # wrapper around cowplot


# themes -------------------------------------------------------------------------------------------
theme_ruben <- function(box = FALSE) {
  
  theme_out = 
    theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      strip.background = element_rect(colour = NA, fill = NA), 
      strip.text.x =  element_text(face = 'plain', size = 10), 
      strip.text.y =  element_text(face = 'plain', size = 10), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.1, "cm"),
      plot.caption = element_text(size = 9, hjust = 0),
      axis.text=element_text(size = 9)
      )
  
  if (box) {
    theme_out = theme_out + 
      theme(
        panel.border = element_rect(size = .3, fill = NA),
        axis.line = element_blank()
      ) 
  } else {
    theme_out = theme_out + 
      theme(
        panel.border = element_blank(),
        axis.line = element_line(size = .3)
      ) 
  }
  return(theme_out)
} 

theme_maps <- function(...) {
  theme_bw(base_size = 11, base_family = "Helvetica") +
    theme(
      strip.text.x = element_text(face = 'bold', size = 10), 
      strip.text.y = element_text(face = 'bold', size = 10), 
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(color = "#a6a6a6"),
      panel.grid.major = element_line(color = "#a6a6a6", size = 0.25),
      axis.title = element_blank(), axis.ticks = element_blank(),
      ...)
}


theme_heatmap <- function(box = TRUE) {
  theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(
      strip.background = element_rect(colour = NA, fill = NA), 
      strip.text.x =  element_text(face = 'plain', size = 10), 
      strip.text.y =  element_text(face = 'plain', size = 10), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.1, "cm"),
      plot.caption = element_text(size = 9, hjust = 0),
      panel.background = element_rect(fill = '#cccccc', color = '#cccccc'),
      #plot.background = element_rect(fill = 'white'),
      panel.border = element_rect(color = 'grey')
    )
}



# palettes and labels ------------------------------------------------------------------------------
library(viridis)
library(colorRamps)
# function to reduce the saturation for fill-border combinations
desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}
view_pal <- function(pal) {
  image(1:length(pal), 1, as.matrix(1:length(pal)),  col= pal,  
    xlab="", ylab = "", xaxt = "n", yaxt = "n", bty = "n")}

# elevation map
pal_topo = c("#00cc99", "#ffff80", "#e6ac00", "#cc9900", "#ccccff", "#ffffff")

# low-2-high
p_matlab <- matlab.like2(15)[-c(2, 5)]
p_spec <- c('#9e0142','#d53e4f','#f46d43','#fdae61','#fee08b',
  '#ffffbf','#e6f598','#abdda4','#66c2a5','#3288bd','#5e4fa2')

# blue-0-red scale
# negative - 0 - positive
p_grass <- c("#220b5b", "#4d1acf", "#312de2", "#2a5ff0", "#51b6fa", "#6cdcf0", "white",
  "#eff465", "#f9d451", "#f18534", "#d4491f", "#ad2b1a", "#59160d")
p_nasa <- c("#0000dd", "#1947ff", "#3c94ff", "#69c0ff", "#82daff", "#9aefff", "#adf6ff", "#ceffff", 
  "white", "#ffff58", "#ffee00", "#ffc500", "#ff9000", "#ff4800", "#ff0000", "#d70000", "#a10000")
p_jma <- c("#431616", "#a22325", "#da1f22", "#dd3720", "#e5671b", "#f3b103", "#f9e700", "#f0ec4d", "#f7f39c", 
  "white", "#afdce1", "#7ecbd2", "#5ec3d9", "#45a8d5", "#436eaa", "#3c5498", "#344a91", "#292c76", "#131334")
p_pu2br <- c("#220b5b", "#4d1acf", "#312de2", "#2a5ff0", "#51b6fa", "#6cdcf0", "white",
  "#eff465", "#f9d451", "#f18534", "#d4491f", "#ad2b1a", "#59160d")
p_bl2re <-  c("#0000ff", "#66ffff", "white", "#ff9999", "#ff0000")
p_seismic <- c("#01044b", "#030e85", "#0619c0", "#0c24fa", "#575dfb", "#aaacfc", "#ffffff",
  "#fdabac", "#fc575a", "#fc0d1b", "#d20915", "#a7050e", "#7d0308")
p_b2r_c3s = c('#2865a7', '#4189bb', '#74b0d1', '#abd1e3', '#d7e8f1', '#ffffff', '#fce0d2', '#f7baa0', '#e7896f', '#cd564d', '#b8404e')



# expressions --------------------------------------------------------------------------------------
e_mbd = expression("MBD [W/m"^2*"]")
e_rmbd = expression("rMBD [%]")
e_mad = expression("MAD [W/m"^2*"]")
e_rmad = expression("rMAD [%]")
e_wm2 = expression("[W/m"^2*"]")
e_dmbe = expression(Delta * "MBD [W/m"^2*"]")
e_drmbe = expression(Delta * "rMBD [%])")
e_kt_d = expression("K"["T,d"]*" [-]")
e_kt_est_d = expression("KT"["d"]^"est"*" [-]")
e_g_d = expression("G"["H,d"]*" [W/m"^2*"]")
e_rd_d = expression("r"*delta["d"]*" [%]")
e_d_d = expression(delta["d"]*" [W/m"^2*"]")
