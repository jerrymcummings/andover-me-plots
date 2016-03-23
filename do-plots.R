# do-plots.R

library(gridExtra)
library(ggplot2)
library(ggmap)
library(geosphere)

DO_LABELS <- TRUE


# set number 1
coord.list.1 <- list(
  c("N 44 38.941", "W 70 46.986"),
  c("N 44 39.085", "W 70 47.215"),
  c("N 44 39.336", "W 70 47.151"),
  c("N 44 39.358", "W 70 47.299"),
  c("N 44 39.413", "W 70 47.293"),
  c("N 44 39.365", "W 70 47.055"),
  c("N 44 39.361", "W 70 47.023"),
  c("N 44 39.319", "W 70 46.972"),
  c("N 44 39.291", "W 70 46.885"),
  
  c("N 44 38.941", "W 70 46.986")
)

# set number 2
coord.list.2 <- list(
  c("N 44 39.040", "W 70 47.132"),
  c("N 44 38.926", "W 70 47.199"),
  c("N 44 38.935", "W 70 47.249"),
  c("N 44 38.904", "W 70 47.266"),
  c("N 44 38.911", "W 70 47.356"),
  c("N 44 38.757", "W 70 47.399"),
  c("N 44 38.780", "W 70 47.499"),
  c("N 44 38.671", "W 70 47.530"),
  c("N 44 38.604", "W 70 47.246"),
  c("N 44 38.861", "W 70 47.171"),
  c("N 44 38.848", "W 70 47.009"),
  c("N 44 38.930", "W 70 46.991"),
  
  c("N 44 39.040", "W 70 47.132")
)

# set number 3
coord.list.3 <- list(
  c("N 44 39.040", "W 70 47.132"),
  c("N 44 38.926", "W 70 47.199"),
  # c("N 44 38.935", "W 70 47.249"),
  # c("N 44 38.904", "W 70 47.266"),
  # c("N 44 38.911", "W 70 47.356"),
  # c("N 44 38.757", "W 70 47.399"),
  # c("N 44 38.780", "W 70 47.499"),
  # c("N 44 38.671", "W 70 47.530"),
  #c("N 44 38.604", "W 70 47.246"),
  c("N 44 38.861", "W 70 47.171"),
  c("N 44 38.848", "W 70 47.009"),
  c("N 44 38.930", "W 70 46.991"),
  
  c("N 44 39.040", "W 70 47.132")
)


####################################################################

# convert to numerical format
decimalize <- function(s) {
  chunks <- strsplit(sub("([N|S|W|E]) ([0-9]+) ([0-9.]+)", "\\1 \\2 \\3", s), ' ')
  chunks <- chunks[[1]]
  x <- as.numeric(chunks[2]) + as.numeric(chunks[3])/60.0
  if (chunks[1] %in% c('S','W')) {
    x <- -1.0 * x
  }
  x
}


####################################################################

do.map <- function(desc, coord.list) {
  lat <- sapply(coord.list, function(x) decimalize(x[1]))
  lon <- sapply(coord.list, function(x) decimalize(x[2]))
  df <- data.frame(lon, lat)
  df$n <- 1:nrow(df) - 1 # allow 0th point to get overwritten by last point
  
  area.square.meters <- areaPolygon(df)
  area.acres <- area.square.meters * 0.000247105
  
  map <- get_map(location=c(lon=mean(df$lon),
                            lat=mean(df$lat)),
                 source="google",
                 maptype="terrain",
                 zoom=15,
                 scale=2)
  
  p1 <- ggmap(map) +
    geom_polygon(data=df,
                 aes(x=lon, y=lat, size=0.025, alpha=0.25)) +
    geom_point(data=df,
               aes(x=lon, y=lat, fill="red", alpha=0.3),
               size=3,
               shape=21)
  
  if (DO_LABELS) {
    p1 <- p1 + 
      geom_label(data = df, aes(x=lon, y=lat, label = n, alpha = 0.4))
  }
  
  p1 <- p1 + 
    ggtitle(paste0(desc, ' - ', format(round(area.acres,2), nsmall=2), ' acres')) +
    theme(legend.position='none')
  
  pdf(paste0(desc, '-', 'terrain.pdf'))
  print(p1)
  dev.off()
  
  map <- get_map(location=c(lon=mean(df$lon),
                            lat=mean(df$lat)),
                 source="google",
                 maptype="satellite",
                 zoom=15,
                 scale=2)
  
  p2 <- ggmap(map) +
    geom_polygon(data=df,
                 aes(x=lon, y=lat, alpha=0.25, color='yellow')) +
    geom_point(data=df,
               aes(x=lon, y=lat, fill="red", alpha=0.3),
               size=3,
               shape=21)
  
  if (DO_LABELS) {
    p2 <- p2 +
      geom_label(data = df, aes(x=lon, y=lat, label = n, alpha = 0.4))
  }
  
  p2 <- p2 +
    ggtitle(paste0(desc, ' - ', format(round(area.acres,2), nsmall=2), ' acres')) +
    theme(legend.position='none')
  
  pdf(file=paste0(desc, '-', 'satellite.pdf'), onefile=FALSE)
  print(p2)
  dev.off()
}


####################################################################

do.layered.map <- function(desc, coord.list.1, coord.list.2) {
  lat.1 <- sapply(coord.list.1, function(x) decimalize(x[1]))
  lon.1 <- sapply(coord.list.1, function(x) decimalize(x[2]))
  df.1 <- data.frame(lon.1, lat.1)
  df.1$n <- 1:nrow(df.1) - 1 # allow 0th point to get overwritten by last point

  lat.2 <- sapply(coord.list.2, function(x) decimalize(x[1]))
  lon.2 <- sapply(coord.list.2, function(x) decimalize(x[2]))
  df.2 <- data.frame(lon.2, lat.2)
  df.2$n <- 1:nrow(df.2) - 1 # allow 0th point to get overwritten by last point
  
  area.square.meters.1 <- areaPolygon(df.1)
  area.acres.1 <- area.square.meters.1 * 0.000247105

  area.square.meters.2 <- areaPolygon(df.2)
  area.acres.2 <- area.square.meters.2 * 0.000247105
  
  map <- get_map(location=c(lon=mean(df.1$lon),
                            lat=mean(df.1$lat)),
                 source="google",
                 maptype="terrain",
                 zoom=15,
                 scale=2)
  
  p1 <- ggmap(map) +
    geom_polygon(data=df.1,
                 aes(x=lon.1, y=lat.1, size=0.025, alpha=0.25)) +
    geom_polygon(data=df.2,
                 aes(x=lon.2, y=lat.2, size=0.025, alpha=0.25))
    #geom_point(data=df,
    #           aes(x=lon, y=lat, fill="red", alpha=0.3),
    #           size=3,
    #           shape=21)
  
  if (DO_LABELS) {
    p1 <- p1 + 
      geom_label(data = df.2, aes(x=lon.2, y=lat.2, label = n, alpha = 0.4))
  }
  
  p1 <- p1 + 
    ggtitle(paste0(desc, ' - ', format(round(area.acres.2, 2), nsmall=2), ' acres')) +
    theme(legend.position='none')
  
  map <- get_map(location=c(lon=mean(df.1$lon),
                            lat=mean(df.1$lat)),
                 source="google",
                 maptype="satellite",
                 zoom=15,
                 scale=2)
  
  p2 <- ggmap(map) +
    geom_polygon(data=df.1,
                 aes(x=lon.1, y=lat.1, alpha=0.25, color='yellow')) +
    geom_polygon(data=df.2,
                 aes(x=lon.2, y=lat.2, alpha=0.25, color='yellow'))
    # geom_point(data=df.2,
    #            aes(x=lon.2, y=lat.2, fill="red", alpha=0.3),
    #            size=3,
    #            shape=21)
  
  if (DO_LABELS) {
    p2 <- p2 +
      geom_label(data = df.2, aes(x=lon.2, y=lat.2, label = n, alpha = 0.4))
  }
  
  p2 <- p2 +
    ggtitle(paste0(desc, ' - ', format(round(area.acres.2,2), nsmall=2), ' acres')) +
    theme(legend.position='none')
  
  # well, this is butt ugly
  df.3 <- data.frame(apply(sapply(coord.list.3, function(x) x), 1, rev))
  rownames(df.3) <- 1:nrow(df.3)
  colnames(df.3) <- c('latitude', 'longitude')
  tbl <- tableGrob(head(df.3, -1))
  
  pdf(file=paste0(desc, '.pdf'), onefile=TRUE)
  grid.arrange(p2, tbl,
               nrow=2,
               as.table=TRUE,
               heights=c(3,1))
  dev.off()
}


####################################################################

# do.map('Plot 1', coord.list.1)
# do.map('Plot 2', coord.list.2)
do.layered.map('Plot 3', coord.list.2, coord.list.3)
