require(proto)
stat_plsmo <- function (mapping = NULL, data = NULL, geom = "smooth",
                        position = "identity",
                        n = 80, fullrange = FALSE, span=2/3, fun=function(x) x,
                        na.rm = FALSE, ...) {
  
  StatPlsmo$new(mapping = mapping, data = data, geom = geom,
                position = position, n=n,
                fullrange = fullrange, span=span, fun=fun,
                na.rm = na.rm, ...)
}

StatPlsmo <- proto(ggplot2:::Stat, {
  objname <- "plsmo"
  
  calculate_groups <- function(., data, scales, ...) {
    rows <- daply(data, .(group), function(df) length(unique(df$x)))
    
    if (all(rows == 1) && length(rows) > 1) {
      message("geom_plsmo: Only one unique x value each group.",
              "Maybe you want aes(group = 1)?")
      return(data.frame())
    }
    
    .super$calculate_groups(., data, scales, ...)
  }
  
  calculate <- function(., data, scales, n=80, span=2/3, fun=function(x) x,
                        fullrange=FALSE,
                        xseq = NULL, na.rm = FALSE, ...) {
    
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_plsmo")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }
    
    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scale_dimension(scales$x, c(0, 0))
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scale_dimension(scales$x, c(0, 0))
        } else {
          range <- range(data$x, na.rm=TRUE)
        }
        xseq <- seq(range[1], range[2], length=n)
      }
    }
    
    z <- with(data,
              lowess(x, y,
                     iter=if(length(unique(y[! is.na(y)])) < 3) 0 else 3,
                     f=span))
    z <- approx(z, xout=xseq)
    z$y <- fun(z$y)
    as.data.frame(z)
  }
  
  required_aes <- c("x", "y")
  default_geom <- function(.) GeomPlsmo
})