# Appendix exhibit: economic-theme decay robustness tables.
#
# How to run: normally run through SA_Appendices.R from flex-mining/.
# Inputs:  cleaned published returns and chapter-2 mined strategies
# Outputs: ../Results/theme_ez_decayinSampEnd*.tex
#          ../Results/theme_corr_academic.tex  (DM-theme vs published correlations)
# Setup --------------------------------------------------------

rm(list = ls())
source("0_Environment.R")
library(kableExtra)

## User Settings ------------------------------------------------

# define predictor
pred_min_tabs = globalSettings$benchmark$t_min # min abs(tstat)
pred_top_n = globalSettings$benchmark$t_rankpct_min  # min t-stat rank

# min data requirements
nstock_min = globalSettings$benchmark$gates$minNumStocks/2
nmonth_min = globalSettings$journals$nmonth_min

# sample periods
samplePeriods = tibble(insampEnd = c(1990, 2000, 2010),
                       oos1End   = c(2004, 2004, 2014)
)

# name of compustat LS file
dmcomp <- list()
dmcomp$name <- paste0('../Data/Processed/'
                      , globalSettings$universe$dataVersion, ' LongShort.RData')

# Data load -----------------------------------------------------

tic0 = Sys.time()

## Load CZ ----------------------------------------------------
inclSignals = restrictInclSignals(restrictType = globalSettings$inclusion$restrictType, 
                                  topT = globalSettings$inclusion$topT)

# published
czsum <- readRDS("../Data/Processed/czsum_allpredictors.RDS") %>%
  filter(Keep) %>% 
  filter(signalname %in% inclSignals) %>% 
  setDT()

czcat <- fread("DataInput/SignalsTheoryChecked.csv") %>%
  select(signalname, Year, theory) %>% 
  filter(signalname %in% inclSignals)

czret <- readRDS("../Data/Processed/czret_keeponly.RDS") %>%
  filter(signalname %in% inclSignals) %>% 
  left_join(czcat, by = "signalname") %>%
  mutate(ret_scaled = ret / rbar * 100)


## Load DM --------------------------------------------------
# read in DM strats (only used in this section)
DMname <- paste0(
  "../Data/Processed/",
  globalSettings$universe$dataVersion,
  " LongShort.RData"
)
dm_rets <- readRDS(DMname)$ret
dm_info <- readRDS(DMname)$port_list

dm_rets <- dm_rets %>%
  left_join(
    dm_info %>% select(portid, sweight),
    by = c("portid")
  ) %>%
  transmute(
    sweight,
    dmname = signalid, yearm, ret, nstock_long, nstock_short
  ) %>%
  setDT()

# tighten up for leaner computation
dm_rets[, id := paste0(sweight, '|', dmname)][
  , ':=' (sweight = NULL, dmname = NULL)]
setcolorder(dm_rets, c('id', 'yearm', 'ret'))  

## Load signal docs --------------------------------------------

dm_linktable = import_docs()

# Loop over in-sample periods ---------------------------------------------
for (j in 1:nrow(samplePeriods)) {
  
  print(paste0('Running themes for in-sample period from 1963 to ', samplePeriods$insampEnd[j]))  
  
  ## Create dmpred: data for data mined predictors ------------------------
  
  insamp = tibble(
    start = as.yearmon('Jul 1963')
    , end = as.yearmon(paste0('Dec ',samplePeriods$insampEnd[j]))
  )
  
  oos1 = tibble(
    start = as.yearmon(paste0('Jan ', samplePeriods$insampEnd[j] + 1))
    , end = as.yearmon(paste0('Dec ', samplePeriods$oos1End[j]))
  )
  
  oos2 = tibble(
    start = as.yearmon(paste0('Jan ', samplePeriods$insampEnd[j] + 1))
    , end = as.yearmon('Dec 2022')
  )
  
  
  dmpred = list()
  
  # select predictors, summarize in-samp, sign
  dmpred$sum = dm_rets[yearm <= insamp$end & yearm >= insamp$start &
                         nstock_long>=nstock_min & nstock_short>=nstock_min, ] %>% 
    .[, .(rbar=mean(ret), tstat=mean(ret)/sd(ret)*sqrt(.N)
          , nmonth=.N), by=id] %>% 
    mutate(sign = sign(rbar)) %>% 
    transmute(id, sign, rbar=sign*rbar, tstat=sign*tstat, nmonth) %>% 
    filter(nmonth >= nmonth_min) 
  
  # get signed returns
  dmpred$ret = merge(dm_rets, dmpred$sum, by=c('id')) %>% 
    filter(nstock_long>=nstock_min & nstock_short>=nstock_min) %>%
    mutate(ret_signed = sign*ret) %>% 
    select(id, yearm, ret_signed, starts_with('nstock')) 
  
  # add OOS ret
  dmpred$sum = dmpred$sum %>% 
    merge(dmpred$ret[yearm >= oos1$start & yearm <= oos1$end
                     , .(rbaroos = mean(ret_signed)), by = 'id']
          , by = 'id') 
  
  dmpred$sum = dmpred$sum %>% 
    merge(dmpred$ret[yearm >= oos2$start & yearm <= oos2$end
                     , .(rbaroos2 = mean(ret_signed)), by = 'id']
          , by = 'id')     
  
  # Define themes ---------------------------------------------------
  
  # for easy editing of xlsx
  dm_info = import_docs()
  
  stratsum = dmpred$sum %>% 
    merge(dm_info %>% 
            transmute(id=dmcode
                      , v1
                      , numer = paste0(signal_form, v1long)
                      , denom = v2long)    
          , by = 'id') %>% 
    mutate(sweight=substr(id,1,2)
           , numer = str_replace_all(numer, 'd_','$\\\\Delta$')
           , numer = str_replace_all(numer, '&','\\\\&'))
  
  # stats by sweight-numer group
  groupsum = stratsum %>% 
    group_by(sweight, v1, numer) %>%
    summarize(
      nstrat = n()
      , pctshort = round(mean(sign==-1)*100, 0)
      , pctsignif = round(mean(tstat>1.96)*100, 0)
      , tstat = round(mean(tstat), 1)
      , rbar = mean(rbar)
      , rbaroos = mean(rbaroos)
      , rbaroos2 = mean(rbaroos2)
      , rbaroos_rbar = mean(rbaroos)/mean(rbar)
      #    , rsqlit = round(mean(rsq_lit)*100, 0)
      , .groups = 'drop'
    ) %>% 
    arrange(-tstat) %>% 
    mutate(rank = row_number()) 
  
  # save groupsum to csv for manual categorization (if needed)
  # (this gets copy-pasted into DataInput/DM-Numerator-LitCat.xlsx)
  if (FALSE) {
    groupsum %>% distinct(v1, numer) %>%  write_csv('../Results/numer_list.csv')
  }
  
  # themes are then the top 20 groups by t-stat
  themesum = groupsum %>% 
    arrange(-tstat) %>% 
    head(20) 
  
  # check to console 
  themesum %>% select(numer, pctshort, tstat, rbar, rbaroos, rbaroos2)
  
  # sketch table
  tab1 = groupsum %>% 
    arrange(-tstat) %>%
    head(20) %>% 
    mutate(blank1 = '') %>% 
    transmute(group = paste0(numer, ' (', sweight, ')')
              , pctshort = round(pctshort, 0)
              , tstat = round(tstat, 1)
              , rbar = round(rbar, 2)
              , blank1
              , decay1=round(1*((rbaroos)/rbar), 2)
              , decay2=round(1*((rbaroos2)/rbar), 2)) %>% 
    arrange(-tstat) 
  
  # export to temp.tex
  tab1 %>% 
    kable('latex', booktabs = T, linesep='', escape=F, digits=2) %>% 
    cat(file='../Results/temp.tex')
  
  # Make it beautiful ----------------------------------------
  
  # setup
  tex = readLines('../Results/temp.tex')
  mcol = function(x) paste0('\\multicolumn{1}{c}{', x, '}')
  strsamp = paste0(year(insamp$start), '-', year(insamp$end))
  stroos1 = paste0(year(oos1$start), '-', year(oos1$end))
  stroos2 = paste0(year(oos2$start), '-', year(oos2$end))
  lhead = function(x) paste0('\\multicolumn{', ncol(tab2), '}{l}{', x, '} \\\\ \\hline')
  
  # expand the header
  tex = tex %>% append('', after=4) %>% append('', after=5)
  
  tex[4] = paste(
    ''
    , paste0('\\multicolumn{3}{c}{', strsamp, ' (IS)}')
    , '' # blank
    , mcol(stroos1)
    , paste0(mcol(stroos2), ' \\\\ \\cmidrule{2-4} \\cmidrule{6-7}')
    , sep=' & ')
  
  tex[5] = paste(
    'Numerator (Stock Weight)'
    , mcol('Pct'), '\\multirow{2}{*}{t-stat}', mcol('Mean')
    , '' # blank  
    , '\\multicolumn{2}{c}{Mean Return} \\\\  '
    , sep = ' & '
  )
  
  tex[6] = paste(
    ''
    , mcol('Short'), '', mcol('Return')
    , '' # blank  
    , '\\multicolumn{2}{c}{OOS / IS} \\\\ '
    , sep = ' & '
  )
  
  writeLines(tex, paste0('../Results/theme_ez_decay', 'inSampEnd', samplePeriods$insampEnd[j], '.tex'))

} # End loop over in-sample periods


# =====================================================================
# Correlations of the top DM themes with published predictors
# =====================================================================
# For the single most in-sample-significant DM numerator group in each
# economic theme (the litcats used in theme_ez_decay.tex), find the academic
# (CZ) predictors whose full-sample long-short returns are most correlated
# with it. Uses the baseline 1963-1980 in-sample window (as in the main
# theme_ez_decay.tex table), and reuses dm_rets / czret / czsum / dm_linktable
# already loaded above.

# how many academic predictors to show per DM anchor
n_acad_per_anchor <- 10
# minimum overlapping months to trust a correlation
nmonth_corr_min <- 60

# baseline in-sample window (matches theme_ez_decay.tex)
insampBase <- tibble(start = as.yearmon('Jul 1963'), end = as.yearmon('Dec 1980'))

# select DM predictors and sign them in the baseline window
corrpred <- list()
corrpred$sum <- dm_rets[
  yearm <= insampBase$end & yearm >= insampBase$start &
    nstock_long >= nstock_min & nstock_short >= nstock_min
] %>%
  .[, .(rbar = mean(ret), tstat = mean(ret) / sd(ret) * sqrt(.N), nmonth = .N), by = id] %>%
  mutate(sign = sign(rbar)) %>%
  transmute(id, sign, tstat = sign * tstat, nmonth) %>%
  filter(nmonth >= nmonth_min)

# signed full-sample DM returns
corrpred$ret <- merge(dm_rets, corrpred$sum[, .(id, sign)], by = 'id') %>%
  filter(nstock_long >= nstock_min & nstock_short >= nstock_min) %>%
  transmute(id, yearm, ret_signed = sign * ret) %>%
  setDT()

# map id -> numerator group (keep signal_form for the litcat join)
corrstrat <- corrpred$sum %>%
  merge(dm_linktable %>% transmute(id = dmcode, v1, signal_form,
                                   numer = paste0(signal_form, v1long)), by = 'id') %>%
  mutate(sweight = substr(id, 1, 2),
         numer = str_replace_all(numer, 'd_', '$\\\\Delta$'),
         numer = str_replace_all(numer, '&', '\\\\&'))

corrgroup <- corrstrat %>%
  group_by(sweight, signal_form, v1, numer) %>%
  summarize(tstat = mean(tstat), .groups = 'drop') %>%
  arrange(-tstat)

# litcats (same source as theme_ez_decay.tex)
litinfo <- tibble(
  litcat0 = c('investment', 'diff investment', 'external financing', 'accruals',
              'diff profitability', 'debt structure')
) %>%
  mutate(order = row_number(), litcat = paste(order, litcat0, sep = '|')) %>%
  select(-order) %>%
  mutate(litcat = if_else(litcat == '2|diff investment', '1|investment', litcat))

groupsumcat <- readxl::read_xlsx('DataInput/DM-Numerator-LitCat.xlsx') %>%
  transmute(signal_form, v1, litcat0 = LitCat) %>%
  left_join(litinfo, by = 'litcat0')

# ANCHOR = most in-sample-significant group within each litcat (from top 20)
anchors <- corrgroup %>%
  head(20) %>%
  left_join(groupsumcat %>% select(signal_form, v1, litcat), by = c('signal_form', 'v1')) %>%
  filter(!is.na(litcat)) %>%
  group_by(litcat) %>%
  slice_max(tstat, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(litcat) %>%
  mutate(anchor_label = paste0(numer, ' (', sweight, '), $t=$ ', sprintf('%.1f', tstat)))

# group-averaged signed return series per anchor
id2group <- corrstrat %>% transmute(id, sweight, signal_form, v1) %>% setDT()
groupret <- merge(corrpred$ret, id2group, by = 'id') %>%
  merge(as.data.table(anchors %>% select(sweight, signal_form, v1, anchor_label)),
        by = c('sweight', 'signal_form', 'v1')) %>%
  .[, .(gret = mean(ret_signed)), by = .(anchor_label, yearm)]

# correlate each anchor with every published predictor (raw full-sample LS returns)
czslim <- czret[, .(signalname, yearm = date, ret)]
corr_tab <- lapply(anchors$anchor_label, function(lab) {
  g <- groupret[anchor_label == lab, .(yearm, gret)]
  merge(czslim, g, by = 'yearm') %>%
    .[, .(corr = cor(ret, gret), nmonth = .N), by = signalname] %>%
    .[nmonth >= nmonth_corr_min] %>%
    .[order(-corr)] %>%
    head(n_acad_per_anchor) %>%
    .[, anchor_label := lab]
}) %>% rbindlist()

# attach published descriptions / author-year, order within anchor
corr_tab <- corr_tab %>%
  merge(czsum[, .(signalname, LongDescription, Authors, Year)], by = 'signalname') %>%
  mutate(authoryear = paste0(Authors, ' ', Year),
         anchor_label = factor(anchor_label, levels = anchors$anchor_label)) %>%
  arrange(anchor_label, -corr)

# escape latex specials in free text
escLatex <- function(x) {
  x <- str_replace_all(x, '&', '\\\\&')
  x <- str_replace_all(x, '%', '\\\\%')
  x <- str_replace_all(x, '_', '\\\\_')
  x
}

tabout <- corr_tab %>%
  transmute(Description = escLatex(LongDescription),
            `Author-Year` = escLatex(authoryear),
            Corr = sprintf('%.2f', corr),
            anchor_label)

grp <- tabout %>% count(anchor_label)
grp_index <- setNames(grp$n, as.character(grp$anchor_label))

tabout %>%
  select(-anchor_label) %>%
  kable('latex', booktabs = TRUE, linesep = '', escape = FALSE, align = 'llr') %>%
  pack_rows(index = grp_index, escape = FALSE, bold = FALSE, italic = TRUE) %>%
  as.character() %>%
  writeLines('../Results/theme_corr_academic.tex')

print(paste0('Wrote ../Results/theme_corr_academic.tex: ', nrow(tabout),
             ' published rows across ', nrow(anchors), ' DM anchors.'))
