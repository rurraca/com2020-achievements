source('config.R')

# projections --------------------------------------------------------------------------------------
proj_robin <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
proj_laea_eu <- "+proj=laea +lat_0=52 +lon_0=10 +ellps=GRS80 +x_0=4321000 +y_0=3210000 
+towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj_laea_spa <- "+proj=laea +lat_0=40 +lon_0=-2.5 +ellps=GRS80 +units=m +no_defs"
proj_lonlat <- "+proj=longlat +ellps=WGS84"
proj_utm_spa <- "+proj=utm +zone=30 +datum=WGS84"


sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


calc_bb <- function(crs, xmin, xmax, ymin, ymax) {
  st_sfc(
    st_polygon(list(cbind(
      c(xmin, xmax, xmax, xmin, xmin), 
      c(ymin, ymin, ymax, ymax, ymin)))),
    crs = proj_lonlat) %>%
    st_transform(crs = crs) %>%
    st_bbox()}


