library(corrplot)

temp <- climate_predictors %>%
  filter(year_of < 2014) %>%
  select(1:2, one_of(keep))

temp1 <- filter(temp, site == "rppn-fma")
m <- cor(temp1[3:18])

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

res1 <- cor.mtest(temp1[3:18], 0.95)
res1 <- cor.mtest(temp1[3:18], 0.99)

corrplot(m, p.mat = res1[[1]], sig.level = 0.05, tl.col = "black",
         tl.cex = 0.7, col = colorRampPalette(brewer.pal(11, "PuOr"))(100),
         pch.col = "gray60")
