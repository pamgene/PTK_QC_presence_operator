library(tercen)
library(dplyr)

presence <- function(df) {
  aLm <- try(lm(.y ~ .x, data = df),  silent = TRUE)
  if(!inherits(aLm, 'try-error')){
    slope     <- aLm$coefficients[[2]]
    intercept <- aLm$coefficients[[1]]
    r2        <- summary(aLm)$r.squared		
    p         <- anova(aLm)[['Pr(>F)']][1]
    presence  <- -log10(p) * sign(slope)
  } else {
    slope <- intercept <- r2 <- p <- present <- NaN
  }
  data.frame(.ri = df$.ri[1], .ci = df$.ci[1], slope = slope, intercept = intercept, r2 = r2, p = p, presence = presence)
}

count_presence <- function(df, cutOff) {
  if (any(is.na(df$presence))) {
    fractionPresent <- NaN
  } else {
    fractionPresent <- sum(df$presence > cutOff)/dim(df)[1]
  }
  data.frame(df, fractionPresent = fractionPresent)
}

ctx = tercenCtx()
seed <- ifelse(is.null(ctx$op.value('seed')), -1, as.double(ctx$op.value('seed')))
if(seed > 0){
  set.seed(seed)  
}


present_cut_off <- ifelse(is.null(ctx$op.value('present cut-off')), 2, as.double(ctx$op.value('present cut-off')))

ctx %>% 
  select(.ci, .ri, .y, .x) %>%
  group_by(.ri, .ci) %>%
  do(presence(.)) %>%
  ungroup() %>%
  group_by(.ri) %>%
  do(count_presence(., present_cut_off)) %>%
  ctx$addNamespace() %>%
  ctx$save()
