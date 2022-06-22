# Created 2022 06 from demo-yz-oos.R
# Rolling window strategy selection and testing


# Setup -------------------------------------------------------------------

# rm(list =ls())
source('0_Environment.R')
source('0_functions.R')

# training sample
train_start = 1963
train_end   = 1993

# test sample
test_start = train_end+1
test_end = test_start + 2

useData = list(
  dataset = 'all_data'  # all_data
  , signal_form = 'ratio'
  , portnum = 10
  , weights = 'ew'
  , longshort_form = 'ls_extremes'
  , scale_vars = 'ScaleVars' # 'Scalevars' or 'NoScaleVars'
)

# number of strategies to sample 
nstrat = 100*1000


# Prepare data ------------------------------------------------------------
# Load data


if (useData$dataset == 'all_data') {
  
  tmp = readRDS(
    paste0('../Data/LongShortPortfolios/stratdat_',
           useData$signal_form, '_', 
           useData$longshort_form,
           useData$portnum, 
           useData$weights, '_',
           useData$scale_vars, '.RData')
  )
  
  rets = tmp$ret %>% 
    transmute(signalname, 
              date,
              ret)
  
  plotString = paste0(useData$dataset, '_',  # For saving plots
                      useData$signal_form, '_', 
                      useData$longshort_form,
                      useData$portnum, 
                      useData$weights)
  
  # yz_data
} else if (useData$dataset == 'yz_data') {
  
  temp = read_sas('../Data Yan-Zheng/Yan_Zheng_RFS_Data.sas7bdat')
  
  # clean, select ew or vw
  temp1 = temp %>%
    mutate(
      signalname = paste(transformation, fsvariable, sep = '.')
    ) %>%
    transmute(
      signalname, date = DATE, ret = 100*ddiff_ew
    )
  
  # sample strats (aka signals)
  stratlist = temp1$signalname %>% unique()
  nstrat = min(nstrat, length(stratlist))
  stratselect = tibble(
    signalname = sample(stratlist, nstrat, replace = F)
    , strati = 1:nstrat
  )
  
  
  rets = temp1 %>%
    inner_join(
      stratselect, by = 'signalname'
    ) %>%
    select(signalname, date, ret)
  
  plotString = paste0(useData$dataset, '_',
                      useData$signal_form, '_', 
                      useData$longshort_form,
                      useData$portnum, 
                      useData$weights)
  
  # clean up
  rm(list = ls(pattern = 'temp'))
  gc()
  
} else {
  
  message(paste('dataset must be one of yz_data or all_data'))
  
}



# Rolling window stats -----------------------------------------------


## Evaluate strategies ----
# maybe want to do each month eventually
datelist = rets %>% 
  distinct(date) %>% 
  filter(month(date) == 6) %>% 
  arrange(date) %>% 
  pull


past_stat = tibble()
for (i in 1:length(datelist)){
  
  
  currdate = datelist[i]
  
  print(currdate)
  
  samp = rets %>% filter(date <= currdate, date >= currdate %m-% months(240) )
  
  sampsum = samp %>% 
    group_by(signalname) %>% 
    filter(!is.na(ret)) %>% 
    summarize(
      rbar = mean(ret), vol = sd(ret), n = sum(!is.na(ret)), tstat = rbar/vol*sqrt(n)
    ) 
  
  sampsum$tradedate = currdate # date when this info can be used to trade
  
  past_stat = rbind(past_stat, sampsum)
  
} # for i
 
# rename to sanity
past_stat = 
  past_stat %>% 
  rename(
    past_rbar = rbar, past_tstat = tstat, past_vol = vol, past_n = n
  )

## Add shrinkage  ----

temp = past_stat %>% 
  group_by(tradedate) %>% 
  filter(past_n >= 60) %>% 
  summarize(
    past_shrink = var(past_tstat)^-1
  ) 

past_stat = past_stat %>% 
  left_join(
    temp, by = c('tradedate')
  )


## Sign strategies----

# add past stats to rets, careful with timing
temp = rets %>% 
  left_join(
    past_stat %>% mutate(date = tradedate %m+% months(1)) # stats from tradedate used for returns next month
    , by = c('signalname', 'date')
  )  %>% 
  arrange(
    signalname, date
  )


# fill past_stats (for non-july ret months) and sign returns
ret_signed = temp %>% 
  group_by(signalname) %>% 
  fill(starts_with('past_')) %>% 
  mutate(
    ret = ret*sign(past_rbar)
  ) %>% 
  filter(!is.na(ret))


# Past Stat Sorts  --------------------------------------------------------

# sort strats into portfolios based on past stats
# this is easier to conceptualize but hard to map to YZ and multiple testing

port_past_perf = ret_signed %>% 
  filter(!is.na(ret), !is.na(past_rbar), !is.na(past_shrink)) %>% 
  group_by(date) %>% 
  mutate(
    past_perf = abs(past_tstat)
    , port = ntile(past_perf, 20)
  ) %>% 
  group_by(port,date) %>% 
  summarize(
    ret = mean(ret)
    , past_perf = mean(past_perf)
    , past_rbar_shrink = mean(abs(past_rbar)*(1-past_shrink))
  )

# ----
plotme = port_past_perf %>% 
  filter(year(date) <= 2005, year(date) >= 1963) %>% 
  group_by(port) %>% 
  summarize(
    rbar = mean(ret), past_rbar_shrink = mean(past_rbar_shrink)
    , vol = sd(ret), ndate = n(), tstat = rbar/vol*sqrt(ndate)
    , se = vol/sqrt(ndate)
  )


plotme %>% 
  ggplot(aes(x=port)) +
  geom_point(
    aes(y=rbar), color = 'black', size = 4
  ) +
  geom_errorbar(
    aes(ymin = rbar - 2*se, ymax = rbar + 2*se)
  ) +
  geom_point(
    data = plotme, aes(y=past_rbar_shrink), color = 'blue', size = 3
  ) +
  geom_abline(
    slope = 0
  )

# ----
plotme = port_past_perf %>% 
  filter(year(date) > 2005) %>% 
  group_by(port) %>% 
  summarize(
    rbar = mean(ret), past_rbar_shrink = mean(past_rbar_shrink)
    , vol = sd(ret), ndate = n(), tstat = rbar/vol*sqrt(ndate)
    , se = vol/sqrt(ndate)
  )


plotme %>% 
  ggplot(aes(x=port)) +
  geom_point(
    aes(y=rbar), color = 'black', size = 4
  ) +
  geom_errorbar(
    aes(ymin = rbar - 2*se, ymax = rbar + 2*se)
  ) +
  geom_point(
    data = plotme, aes(y=past_rbar_shrink), color = 'blue', size = 3
  ) +
  geom_abline(
    slope = 0
  )




# Box plot w/ shrinkage ---------------------------------------------------

nbin = 50

# Summarize shrunk rbar by bins of past stats
binrets = ret_signed %>% 
  mutate(
    past_rbar_shrink = past_rbar * (1-past_shrink)
  ) %>% 
  filter(!is.na(past_rbar_shrink)) %>% 
  group_by(date) %>% 
  mutate(
    bin = ntile(past_tstat, nbin)
  ) %>% 
  group_by(date,bin) %>% 
  summarize(
    ret = mean(ret)
    , past_alpha_shrink = mean(past_rbar_shrink)
    , past_alpha = mean(past_rbar)
  ) 
  
binsum = binrets %>% 
  group_by(bin) %>% 
  summarize()
  


binshrink = stratSum %>% 
  filter(stattype == 'stein_ez', samp == 'train') %>% 
  mutate(
    bin = ntile(alpha, nbin)
  ) %>% 
  group_by(bin) %>% 
  summarize(
    alpha = mean(alpha), tstat= mean(tstat)
  )

# merge all data together
plotme = stratSum %>% 
  filter(stattype == 'stein_ez', samp == 'train') %>% 
  mutate(
    bin = ntile(alpha, nbin)
  ) %>%  
  select(signalname, bin) %>% 
  left_join(
    binshrink %>% rename(alpha_shrink = alpha, tstat_shrink = tstat)
    , by = 'bin'
  ) %>% 
  left_join(
    stratSum %>% 
      filter(stattype == 'classical', samp == 'test') %>% 
      rename(alpha_test = alpha, tstat_test = tstat)
    , by = 'signalname'
  )


# plot
outliersU = quantile(plotme$alpha_test, .995, na.rm = TRUE)
outliersD = quantile(plotme$alpha_test, .005, na.rm = TRUE)
plotme %>% 
  ggplot(aes(x=bin)) +
  geom_boxplot(aes(y=alpha_test, group = bin), outlier.shape = '')  +
  geom_point(
    aes(y=alpha_shrink ), color = 'blue', size = 3
  ) +
  coord_cartesian(ylim = c(outliersD -.1, outliersU + .1)) +
  labs(x = 'Training data alpha bin', y = 'Test data alpha') +
  theme_bw(base_size = 14)

ggsave(paste0('../Results/TrainVsTest_', plotString, '.png'), width = 12, height = 10)



# Old stuff ---------------------------------------------------------------


# Sign strategies ====
# based on past rbar

# summarize
signalsum = ret_signed %>% 
  group_by(signalname) %>% 
  summarize(
    past_tabs = abs(mean(past_tstat)), rbar = mean(ret), vol = sd(ret)
    , ndate = n(), tstat = rbar/vol*sqrt(ndate)
    , past_tstat = mean(past_tstat)
  )


# Check ====

# should see a positive slope
signalsum %>% 
  ggplot(aes(x=past_tstat, y = rbar)) +
  geom_point()

# should see more in tails thatn standard normal
signalsum %>% 
  ggplot(aes(x=tstat)) +
  geom_histogram() +
  xlab('out of sample long-short t-stat') +
  scale_x_continuous(breaks = seq(-3,5,1))

# more precisely
F_t_oos = ecdf(signalsum$past_tabs)

tlist = c(2,2.5,3)

fdrmax = (1-pnorm(tlist))/(1-F_t_oos(tlist))
  
print('fdr max estimates')
print(fdrmax)



# simple FDR ====
# super simple Storey 2002 algo
pcut = 0.9

pval = past_stat %>% 
  mutate(
    pval = 2*(1-pnorm(abs(past_tstat)))
  ) %>% 
  arrange(pval) %>% 
  pull(pval)

Fp = ecdf(pval)

pif = (1-Fp(pcut))/(1-pcut)

pif  

fdr_bh = 0.05/Fp(0.05)

fdr_bh

fdr_bh*pif



# strats grouped by past stats ====


