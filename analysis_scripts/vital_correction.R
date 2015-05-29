
get_trials <- function(df){

  fates <- list()

  for (i in 1:nrow(df)){
    fate <- c(rep(1, df[i, ]$successes),
              rep(0, df[i, ]$trials - df[i, ]$successes))

    f <- data.frame(fate = fate)

    f$Study.Id <- df[i, ]$Study.Id
    f$year_of <- df[i, ]$year_of
    f$age_class <- df[i, ]$age_class

    fates[[i]] <- f
  }

  fates <- bind_rows(fates)

  return(fates)
}

m <- stage_specific_survival(lh)

surv_trials <- bind_rows(dlply(filter(m, trials != 0), .(Study.Id, age_class), get_trials))

surv_trials$age_class <- factor(surv_trials$age_class, levels = c("newborn", "juvenile", "adult"))







# models <- list()
# count <- 1
#
# for(i in 1:length(levels(temp$Study.Id))){
#
#   current_study <- levels(temp$Study.Id)[i]
#
#   for(j in 1:length(levels(factor(temp$age_class)))){
#     current_ac <- levels(temp$age_class)[j]
#
#     set <- temp %>%
#       filter(Study.Id == current_study & age_class == current_ac) %>%
#       data.frame()
#
#     models[[count]] <- glmer(fate ~ 1 + (1 | year_of), data = set, family = "binomial")
#
#     count <- count + 1
#   }
#
# }
#
# out <- models[[19]]
#
# x=coef(out)$year_of
# x=as.vector(x[,1])
# x=exp(x)/(1+exp(x))

