library(dplyr)

setwd('/Users/jillian/Dropbox (Personal)/bts writing/btsCB 150620/revision_150620/code')

dorn <- readr::read_csv('/Users/jillian/Downloads/Dornelas_alpha_diversty.csv')

dorn_slopes <- 
  dorn %>%
    group_by(ID) %>% 
    do({
      mod <- lm(log(S) ~ Year, data = .)
      slope <- coefficients(mod)[[2]]
      return(data_frame(slope = slope))
    })

dorn_slopes %>% 
  mutate(back_slope = exp(slope)) %>% 
  ggplot(data = ., aes(x = back_slope, colour = ID)) + 
  geom_histogram()

dorn_slopes %>% 
  mutate(back_slope = exp(slope)) %>% 
  summarise(range(back_slope))

test <- 
  richDat %>% 
    filter(Scale == 'alpha', sppCode == 'R') %>% 
    group_by(subSiteID) %>% 
    do({
      mod <- lme(fixed = richLN ~ I(year0Z + 1990), 
                 data = ., method = "REML", 
                 random =  randList1, 
                 correlation = corAR1())
      mod_df <- summary(mod)$tTable %>% as_data_frame %>% mutate(predictor = rownames(summary(mod)$tTable))
      return(mod_df)
    })

test %>% 
  filter(predictor == 'I(year0Z + 1990)') %>% 
  ungroup() %>% 
  summarise(min(Value), max(Value))


test <- ddply(filter(richDat2, sppCode == 'R'), .(subSiteID), summarize, 
      slope = coefficients(lm(richLN ~ year0Z))[2], 
      slopeSE = summary(lm(richLN ~ year0Z))[[4]][4], 
      r2 = summary(lm(richLN ~ year0Z))$r.squared,
      pVal = summary(lm(richLN ~ year0Z))[[4]][8], 
      tempN = length(richLN), 
      meanS = mean(richLN),
      fitSlope = coefficients(lm(fitted ~ year0Z))[2]
      ) 


slopeRich %>% 
  mutate(subSiteID = as.character(subSiteID)) %>% 
  tidyr::extract(col = subSiteID, into = c('subSiteID', 'richType'), regex = '(.*)(_R|_S)$') %>% 
  filter(richType == '_R') %>% 
  as_data_frame() %>% 
  mutate(back_slope = exp(fitSlope)) %>% 
  ggplot(data = ., aes(x = back_slope)) + 
    geom_histogram()

slopeRich %>% 
  mutate(back_slope = exp(fitSlope)) %>% 
  summarise(range(back_slope))