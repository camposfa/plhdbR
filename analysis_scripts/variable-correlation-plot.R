library(corrplot)

temp <- climate_predictors %>%
  filter(year_of < 2014 & !grepl("spei", var))

temp1 <- temp %>%
  filter(site == "amboseli") %>%
  spread(var, value)

m <- cor(temp1[4:6])

cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
      uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

# res1 <- cor.mtest(temp1[3:18], 0.95)
# res1 <- cor.mtest(temp1[3:18], 0.99)
#
# corrplot(m, p.mat = res1[[1]], sig.level = 0.05, tl.col = "black",
#          tl.cex = 0.7, col = colorRampPalette(brewer.pal(11, "PuOr"))(100),
#          pch.col = "gray60")

temp$var <- revalue(temp$var, var_map)
temp1 <- temp %>%
  filter(site == "ssr")

temp1$quarter <- revalue(temp1$quarter, quarter_map)

temp2 <- temp1 %>%
  ungroup() %>%
  spread(var, value) %>%
  select(-site, -year_of)

par(mfrow = c(1, 5))
for (i in seq_along(levels(factor(temp1$quarter)))) {

  cur_qua <- levels(factor(temp1$quarter))[i]

  temp2 <- temp1 %>%
    ungroup() %>%
    filter(quarter == cur_qua) %>%
    spread(var, value) %>%
    select(-site, -year_of, -quarter)

  temp3 <- cor(temp2, use = "na.or.complete", method = "spearman")

  res1 <- cor.mtest(temp2, 0.95)

  corrplot.mixed(temp3, col = colorRampPalette(brewer.pal(11, "PuOr"))(100),
                 # mar = c(3, 1, 0, 1),
                 tl.col = "black", tl.cex = 0.8,
                 cl.cex = 0.8, cl.align.text = "l")

  # corrplot(temp3, p.mat = res1[[1]], sig.level = 0.05, tl.col = "black",
  #          tl.cex = 0.7, col = colorRampPalette(brewer.pal(11, "PuOr"))(100),
  #          pch.col = "gray60")

  title(cur_qua, line = -2)
}



# ---- ggcorr_plots -------------------------------------------------------

g_list <- list(35)

temp <- climate_predictors %>%
  filter(year_of < 2014 & !grepl("spei", var))


for (i in seq_along(levels(factor(temp$site)))) {

  cur_site <- levels(factor(temp$site))[i]

  for (j in seq_along(levels(factor(temp$quarter)))) {

    cur_quarter <- levels(factor(temp$quarter))[j]

    temp1 <- temp %>%
      ungroup() %>%
      filter(quarter == cur_quarter & site == cur_site) %>%
      spread(var, value) %>%
      select(-year_of)

    g_list[[((i - 1) * 5) + j]] <- ggcorr_fc(temp1, method = c("na.or.complete", method = "spearman"),
                                     geom = "tile", label = TRUE, hjust = 0.75, digits = 2, label_size = 3,
                                     label_round = 2, palette = rev(brewer.pal(11, "PuOr")),
                                     nbreaks = 10, size = 3, layout.exp = 1, color = "gray50") +
      labs(title = ifelse(cur_quarter == "annual", capitalize(cur_quarter),
                          paste(capitalize(cur_quarter), "Quarter"))) +
      guides(fill = FALSE)
  }
}

cowplot::plot_grid(plotlist = g_list, nrow = 7, ncol = 5)


temp <- climate_predictors %>%
  filter(year_of < 2014 & !grepl("spei", var))

temp1 <- temp %>%
  group_by(site, quarter) %>%
  spread(var, value) %>%
  select(-year_of) %>%
  do(co = cor(cbind(.$index_nino3.4, .$mean_temp, .$rainfall),
      use = "na.or.complete", method = "spearman"))

cor_df <- function(data, nbreaks, digits = 2) {
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
  cor_matrix = cor(data, use = "na.or.complete", method = "spearman")
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
  m$label = round(m$coefficient, 2)

  return(m)
}

temp1 <- temp %>%
  group_by(site, quarter) %>%
  spread(var, value) %>%
  select(-year_of) %>%
  do(co = cor_df(., nbreaks = 10)) %>%
  unnest()

temp1$x <- revalue(temp1$x, var_map)
temp1$y <- revalue(temp1$y, var_map)

ggcorr_fc2(temp1, geom = "tile", label = TRUE, hjust = 0.75, digits = 2,
           label_size = 3, label_round = 2, tile_color = "black",
           palette = rev(brewer.pal(11, "PuOr")), name = "Correlation",
           nbreaks = 10, size = 3, layout.exp = 1, color = "gray50") +
  facet_grid(site ~ quarter, switch = "y", labeller = global_labeller) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"))