ggcorr_fc <- function (data, method = c("pairwise", "pearson"), cor_matrix = NULL,
          nbreaks = NULL, digits = 2, name = "", low = "#3B9AB2", mid = "#EEEEEE",
          high = "#F21A00", midpoint = 0, palette = NULL, geom = "tile", tile_color = "white",
          min_size = 2, max_size = 6, label = FALSE, label_alpha = FALSE,
          label_color = "black", label_round = 1, label_size = 4, limits = c(-1, 1),
          drop = is.null(limits) || identical(limits, FALSE),
          layout.exp = 0, legend.position = "right", legend.size = 9,
          ...)
{
  if (is.numeric(limits)) {
    if (length(limits) != 2) {
      stop("'limits' must be of length 2 if numeric")
    }
  }
  if (is.logical(limits)) {
    if (limits) {
      limits <- c(-1, 1)
    }
    else {
      limits <- NULL
    }
  }
  if (length(geom) > 1 || !geom %in% c("blank", "circle", "text",
                                       "tile")) {
    stop("incorrect geom value")
  }
  if (length(method) == 1) {
    method = c(method, "pearson")
  }
  if (!is.null(data)) {
    if (!is.data.frame(data)) {
      data = as.data.frame(data)
    }
    x = which(!sapply(data, is.numeric))
    if (length(x) > 0) {
      warning(paste("data in column(s)", paste0(paste0("'",
                                                       names(data)[x], "'"), collapse = ", "), "are not numeric and were ignored"))
      data = data[, -x]
    }
  }
  if (is.null(cor_matrix)) {
    cor_matrix = cor(data, use = method[1], method = method[2])
  }
  m = cor_matrix
  colnames(m) = rownames(m) = gsub(" ", "_", colnames(m))
  m = data.frame(m * lower.tri(m))
  rownames(m) = names(m)
  m$.ggally_ggcorr_row_names = rownames(m)
  m = reshape2::melt(m, id.vars = ".ggally_ggcorr_row_names")
  names(m) = c("x", "y", "coefficient")
  m$coefficient[m$coefficient == 0] = NA
  if (!is.null(nbreaks)) {
    x = seq(-1, 1, length.out = nbreaks + 1)
    if (!nbreaks%%2) {
      x = sort(c(x, 0))
    }
    m$breaks = cut(m$coefficient, breaks = unique(x), include.lowest = TRUE,
                   dig.lab = digits)
  }
  if (is.null(midpoint)) {
    midpoint = median(m$coefficient, na.rm = TRUE)
    message(paste("Color gradient midpoint set at median correlation to",
                  round(midpoint, 2)))
  }
  m$label = round(m$coefficient, label_round)
  p = ggplot(na.omit(m), aes(x, y))
  if (geom == "tile") {
    if (is.null(nbreaks)) {
      p = p + geom_tile(aes(fill = coefficient), color = tile_color)
    }
    else {
      p = p + geom_tile(aes(fill = breaks), color = tile_color)
    }
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_fill_gradient2(name, low = low, mid = mid,
                                   high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_fill_gradient2(name, low = low, mid = mid,
                                   high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_fill_manual(name, values = x, drop = drop)
    }
    else {
      p = p + scale_fill_manual(name, values = palette, drop = FALSE)
    }
  }
  else if (geom == "circle") {
    p = p + geom_point(aes(size = abs(coefficient) * 1.25),
                       color = "grey50")
    if (is.null(nbreaks)) {
      p = p + geom_point(aes(size = abs(coefficient), color = coefficient))
    }
    else {
      p = p + geom_point(aes(size = abs(coefficient), color = breaks))
    }
    p = p + scale_size_continuous(range = c(min_size, max_size)) +
      guides(size = FALSE)
    r = list(size = (min_size + max_size)/2)
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_color_manual(name, values = x, drop = drop) +
        guides(color = guide_legend(override.aes = r))
    }
    else {
      p = p + scale_color_gradientn(colors = palette) + guides(color = guide_legend(override.aes = r))
    }
  }
  else if (geom == "text") {
    if (is.null(nbreaks)) {
      p = p + geom_text(aes(label = label, color = coefficient),
                        size = label_size)
    }
    else {
      p = p + geom_text(aes(label = label, color = breaks),
                        size = label_size)
    }
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_color_manual(name, values = x, drop = drop)
    }
    else {
      p = p + scale_color_gradientn(colors = palette)
    }
  }
  if (label) {
    if (isTRUE(label_alpha)) {
      p = p + geom_text(aes(x, y, label = label, alpha = abs(coefficient)),
                        color = label_color, size = label_size, show.legend = FALSE)
    }
    else if (label_alpha > 0) {
      p = p + geom_text(aes(x, y, label = label, show_guide = FALSE),
                        alpha = label_alpha, color = label_color, size = label_size)
    }
    else {
      p = p + geom_text(aes(x, y, label = label), color = label_color,
                        size = label_size)
    }
  }
  textData <- m[m$x == m$y & is.na(m$coefficient), ]
  xLimits <- levels(textData$y)
  textData$diagLabel <- textData$x
  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  }
  else if (layout.exp > 0) {
    layout.exp <- as.integer(layout.exp)
    textData <- rbind(textData[1:layout.exp, ], textData)
    spacer <- paste(".ggally_ggcorr_spacer_value", 1:layout.exp,
                    sep = "")
    textData$x[1:layout.exp] <- spacer
    textData$diagLabel[1:layout.exp] <- NA
    xLimits <- c(spacer, levels(m$y))
  }
  p = p + geom_text(data = textData, aes_string(label = "diagLabel"),
                    ..., na.rm = TRUE) + scale_x_discrete(breaks = NULL,
                                                          limits = xLimits) + scale_y_discrete(breaks = NULL, limits = levels(m$y)) +
    labs(x = NULL, y = NULL) + coord_equal() + theme(panel.background = element_blank(),
                                                     legend.key = element_blank(), legend.position = legend.position,
                                                     legend.title = element_text(size = legend.size), legend.text = element_text(size = legend.size))
  return(p)
}

ggcorr_fc2 <- function (data, nbreaks = NULL, digits = 2, name = "", low = "#3B9AB2", mid = "#EEEEEE",
                       high = "#F21A00", midpoint = 0, palette = NULL, geom = "tile", tile_color = "white",
                       min_size = 2, max_size = 6, label = FALSE, label_alpha = FALSE,
                       label_color = "black", label_round = 1, label_size = 4, limits = c(-1, 1),
                       drop = is.null(limits) || identical(limits, FALSE),
                       layout.exp = 0, legend.position = "right", legend.size = 9,
                       ...)
{
  if (is.numeric(limits)) {
    if (length(limits) != 2) {
      stop("'limits' must be of length 2 if numeric")
    }
  }
  if (is.logical(limits)) {
    if (limits) {
      limits <- c(-1, 1)
    }
    else {
      limits <- NULL
    }
  }
  if (length(geom) > 1 || !geom %in% c("blank", "circle", "text",
                                       "tile")) {
    stop("incorrect geom value")
  }

  m <- data

  p = ggplot(na.omit(m), aes(x, y))
  if (geom == "tile") {
    if (is.null(nbreaks)) {
      p = p + geom_tile(aes(fill = coefficient), color = tile_color)
    }
    else {
      p = p + geom_tile(aes(fill = breaks), color = tile_color)
    }
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_fill_gradient2(name, low = low, mid = mid,
                                   high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_fill_gradient2(name, low = low, mid = mid,
                                   high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_fill_manual(name, values = x, drop = drop)
    }
    else {
      p = p + scale_fill_manual(name, values = palette, drop = FALSE)
    }
  }
  else if (geom == "circle") {
    p = p + geom_point(aes(size = abs(coefficient) * 1.25),
                       color = "grey50")
    if (is.null(nbreaks)) {
      p = p + geom_point(aes(size = abs(coefficient), color = coefficient))
    }
    else {
      p = p + geom_point(aes(size = abs(coefficient), color = breaks))
    }
    p = p + scale_size_continuous(range = c(min_size, max_size)) +
      guides(size = FALSE)
    r = list(size = (min_size + max_size)/2)
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_color_manual(name, values = x, drop = drop) +
        guides(color = guide_legend(override.aes = r))
    }
    else {
      p = p + scale_color_gradientn(colors = palette) + guides(color = guide_legend(override.aes = r))
    }
  }
  else if (geom == "text") {
    if (is.null(nbreaks)) {
      p = p + geom_text(aes(label = label, color = coefficient),
                        size = label_size)
    }
    else {
      p = p + geom_text(aes(label = label, color = breaks),
                        size = label_size)
    }
    if (is.null(nbreaks) && !is.null(limits)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint, limits = limits)
    }
    else if (is.null(nbreaks)) {
      p = p + scale_color_gradient2(name, low = low, mid = mid,
                                    high = high, midpoint = midpoint)
    }
    else if (is.null(palette)) {
      x = colorRampPalette(c(low, mid, high))(length(levels(m$breaks)))
      p = p + scale_color_manual(name, values = x, drop = drop)
    }
    else {
      p = p + scale_color_gradientn(colors = palette)
    }
  }
  if (label) {
    if (isTRUE(label_alpha)) {
      p = p + geom_text(aes(x, y, label = label, alpha = abs(coefficient)),
                        color = label_color, size = label_size, show.legend = FALSE)
    }
    else if (label_alpha > 0) {
      p = p + geom_text(aes(x, y, label = label, show_guide = FALSE),
                        alpha = label_alpha, color = label_color, size = label_size)
    }
    else {
      p = p + geom_text(aes(x, y, label = label), color = label_color,
                        size = label_size)
    }
  }
  textData <- m[m$x == m$y & is.na(m$coefficient), ]
  xLimits <- levels(textData$y)
  textData$diagLabel <- textData$x
  if (!is.numeric(layout.exp) || layout.exp < 0) {
    stop("incorrect layout.exp value")
  }
  else if (layout.exp > 0) {
    layout.exp <- as.integer(layout.exp)
    textData <- rbind(textData[1:layout.exp, ], textData)
    spacer <- paste(".ggally_ggcorr_spacer_value", 1:layout.exp,
                    sep = "")
    textData$x[1:layout.exp] <- spacer
    textData$diagLabel[1:layout.exp] <- NA
    xLimits <- c(spacer, levels(m$y))
  }
  p = p + geom_text(data = textData, aes_string(label = "diagLabel"),
                    ..., na.rm = TRUE) + scale_x_discrete(breaks = NULL,
                                                          limits = xLimits) + scale_y_discrete(breaks = NULL, limits = levels(m$y)) +
    labs(x = NULL, y = NULL) + coord_equal() + theme(panel.background = element_blank(),
                                                     legend.key = element_blank(), legend.position = legend.position,
                                                     legend.title = element_text(size = legend.size), legend.text = element_text(size = legend.size))
  return(p)
}