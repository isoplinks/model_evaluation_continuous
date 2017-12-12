stat_plsmo <- function (mapping = NULL, data = NULL, geom = "smooth",
                        position = "identity",
                        n = 80, fullrange = FALSE, span=2/3, fun=function(x) x,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                        ...) {

  layer(
    stat = StatPlsmo, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      n = n,
      fullrange = fullrange,
      span = span,
      fun = fun,
      na.rm = na.rm,
      ...)
  )
}

StatPlsmo <- ggplot2::ggproto("StatPlsmo", ggplot2::Stat,
  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    rows <- plyr::daply(data, "group", function(df) length(unique(df$x)))

    if (all(rows == 1) && length(rows) > 1) {
      message("geom_plsmo: Only one unique x value each group.",
        "Maybe you want aes(group = 1)?")
      return(data.frame())
    }

    data
  },

  compute_group = function(., data, scales, n=80, span=2/3, fun=function(x) x,
                        fullrange=FALSE, xseq = NULL, na.rm = FALSE) {
    data <- remove_missing(data, na.rm, c("x", "y"), name="stat_plsmo")
    if (length(unique(data$x)) < 2) {
      # Not enough data to perform fit
      return(data.frame())
    }

    if (is.null(xseq)) {
      if (is.integer(data$x)) {
        if (fullrange) {
          xseq <- scales$x$dimension()
        } else {
          xseq <- sort(unique(data$x))
        }
      } else {
        if (fullrange) {
          range <- scales$x$dimension()
        } else {
          range <- range(data$x, na.rm = TRUE)
        }
        xseq <- seq(range[1], range[2], length.out = n)
      }
    }

    n_y <- length(unique(data$y[!is.na(data$y)]))

    z <- lowess(data$x, data$y,
      iter = if (n_y < 3) 0 else 3,
      f = span
    )
    z <- approx(z, xout = xseq)
    z$y <- fun(z$y)
    as.data.frame(z)
  }
)