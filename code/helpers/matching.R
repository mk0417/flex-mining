# Matching helpers: DM-vs-published summary stats, selection, event returns.
#
# Sourced by 0_Environment.R after packages and config.R. These functions
# rely on objects from the sourcing environment (e.g. globalSettings and
# chapter-local data frames); do not source this file in isolation.



# function for restricting included published signals
restrictInclSignals = function(restrictType = NULL, topT = 2) {
  
  dt = readRDS('../Data/Processed/czsum_allpredictors.RDS')
  
  if (is.null(restrictType)) {
    
    signals = dt %>% 
      pull(signalname)
    message('Using all ', nrow(dt), ' signals')
    
  } else if (restrictType == 'topT') {
    
    # There are a bunch of papers that contain a lot of signals
    # 1 Heston and Sadka             2008 JFE        10
    # 2 Richardson et al.            2005 JAE         7
    # 3 Daniel and Titman            2006 JF          6
    # 4 Nagel                        2005 JFE         4
    # 5 An, Ang, Bali, Cakici        2014 JF          3
    # 6 Ang et al.                   2006 JF          3
    # 7 Barber et al.                2001 JF          3
    # 8 Bradshaw, Richardson, Sloan  2006 JAE         3
    
    # To mitigate the effect of those papers on the agnostic and mispricing cats, 
    # we pick at most topT signals from each paper 
    # We consider the topT signals with the highest t-stats per paper
    # For Ang et al (2006), we keep the betaVIX signal because it is one of the 
    # relatively few risk signals (it is also the signal with the highest IS 
    # t-stat from that paper, so the filter below does not do anything)
    
    signals = dt %>% 
      group_by(Authors, Year, Journal) %>%
      arrange(desc(abs(tstat))) %>% 
      mutate(tmp = row_number()) %>% 
      filter(tmp <= topT | signalname == 'betaVIX') %>%
      pull(signalname)
    
    message('Using ', length(signals), ' out of ', nrow(dt), ' signals')
  } else {
    stop('Invalid restrictType')
  }
  
  return(signals)
}



# function for computing DM strat sumstats in pub samples
sumstats_for_DM_Strats <- function(
    DMname = paste0('../Data/Processed/',
                    globalSettings$universe$dataVersion, 
                    ' LongShort.RData'),
    nsampmax = Inf,
    ncores = globalSettings$compute$num_cores) {
  
  # convert czsum to data.table (this should be done more globally)
  setDT(czsum)
  
  stratdat <- readRDS(DMname)
  dm_rets <- stratdat$ret
  dm_info <- stratdat$port_list
  rm(stratdat)
  
  dm_rets <- dm_rets %>%
    left_join(
      dm_info %>% select(portid, sweight), by = c("portid")
    ) %>%
    transmute(
      sweight, dmname = signalid, yearm, ret, nstock_long, nstock_short
    ) %>%
    setDT()
  
  # Finds sum stats for dm in each pub sample
  # the output for this can be used for all dm selection methods
  samplist <- czsum %>%
    distinct(sampstart, sampend) %>%
    arrange(sampstart, sampend)
  
  # set up for parallel
  cl <- NULL
  if (.Platform$OS.type == "unix") {
    doParallel::registerDoParallel(cores = ncores)
  } else {
    fallback_cores <- min(ncores, 2L)
    warning(
      "sumstats_for_DM_Strats uses at most two PSOCK workers on non-Unix systems ",
      "to limit copies of the mined-return panel."
    )
    cl <- parallel::makePSOCKcluster(fallback_cores)
    doParallel::registerDoParallel(cl)
  }
  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  
  # loop setup
  nsamp <- dim(samplist)[1]
  nsamp <- min(nsamp, nsampmax)
  dm_insamp <- list()
  
  # dopar in a function needs some special setup
  # https://stackoverflow.com/questions/6689937/r-problem-with-foreach-dopar-inside-function-called-by-optim
  dm_insamp <- foreach(
    sampi = 1:nsamp,
    .combine = rbind,
    .packages = c("data.table", "tidyverse", "zoo"),
    .export = c("samplist", "dm_rets", "czsum", "czret", "nsamp")
  ) %dopar% {
    # ) %do% {
    # feedback
    print(paste0("DM sample stats for sample ", sampi, " of ", nsamp))
    
    # find sum stats for the current sample
    sampcur <- samplist[sampi, ]
    sumcur <- dm_rets[
      yearm >= sampcur$sampstart & yearm <= sampcur$sampend &
        !is.na(ret),
      .(
        rbar = mean(ret), tstat = mean(ret) / sd(ret) * sqrt(.N),
        min_nstock_long = min(nstock_long),
        min_nstock_short = min(nstock_short),
        nmonth = sum(!is.na(ret))
      ),
      by = c("sweight", "dmname")
    ]
    # find number of obs in the last year of the sample
    filtcur <- dm_rets[
      floor(yearm) == year(sampcur$sampend) &
        !is.na(ret),
      .(nlastyear = .N),
      by = c("sweight", "dmname")
    ]
    
    # combine sum stats with last year nobs
    sumcur <- sumcur %>%
      left_join(filtcur, by = c("sweight", "dmname")) %>%
      mutate(
        sampstart = sampcur$sampstart, sampend = sampcur$sampend
      )
    
    # expand with published signalnames and reorg
    pubnamelist = czsum[sampstart == sampcur$sampstart
                        & sampend == sampcur$sampend, .(signalname)] %>% 
      rename(pubname = signalname)
    pubsumcur = expand_grid(pubnamelist, sumcur) %>% 
      select(pubname, sampstart, sampend, everything()) %>% 
      setDT()
    
    # add pairwise correlations    
    # with data.table takes only 2 sec per pubname
    for (pubi in 1:nrow(pubnamelist)) {      
      pubname = pubnamelist[pubi, ]$pubname
      
      # merge pub returns onto dm returns, temporarily
      tempret = czret[signalname == pubname & date >= sampcur$sampstart
                      & date <= sampcur$sampend, .(date,ret)]
      dm_rets[tempret, temppubret := i.ret, on = .(yearm = date)]
      
      # # Perform PPCA on the wide version of tempret
      # tempret_wide <- dcast(tempret, date ~ pubname, value.var = "ret") # this line throws an error on my system - ac
      # pca_model <- pca(tempret_wide[,-1, with=FALSE], method = "ppca", nPcs = 5)
      # pca_scores <- scores(pca_model)
      # # Add the date back to PCA scores
      # pca_scores <- data.table(date = tempret_wide$date, pca_scores)
      
      # compute correlation
      tempcor = dm_rets[yearm >= sampcur$sampstart & yearm <= sampcur$sampend
                        , .(cor = cor(ret, temppubret, use = "pairwise")), by = c("dmname", "sweight")]
      tempcor$pubname = pubname
      
      # merge back onto sumcur
      pubsumcur[tempcor, cor := i.cor, on = c("pubname", "sweight", "dmname")]
      
      # clean up
      dm_rets[ , temppubret := NULL]
    } # end for pubi
    
    return(pubsumcur)
  } # end dm_insamp loop
  if (!is.null(cl)) parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  on.exit(NULL, add = FALSE)
  
  # Merge with czsum
  # insampsum key is c(pubname,dmname). Each row is a dm strat that matches a pub
  insampsum <- czsum %>%
    transmute(
      pubname = signalname, rbar_op = rbar, tstat_op = tstat, sampstart, sampend,
      sweight = tolower(sweight)
    ) %>%
    left_join(
      dm_insamp,
      by = c('pubname', 'sweight','sampstart', 'sampend'),
    ) %>%
    arrange(pubname, desc(tstat))
  
  setDT(insampsum)
  
  return(insampsum)
} # end Sumstats function





# Common accounting-ratio gates -------------------------------------------
# docs/benchmark-logic.md lists five gates that every accounting-ratio
# benchmark shares. The two functions below are the only implementation of
# them: selectors call apply_common_gates() first and then add whatever screen
# distinguishes their benchmark. Keeping the gates in one place is what makes
# the doc's "Common accounting-ratio gates" section verifiable rather than a
# claim repeated across selectors.
#
# Three gates are numeric thresholds and live in globalSettings$benchmark$gates.
# The remaining two are structural and are applied here unconditionally:
#   * orientation is taken from the in-sample mean, as `orientation`; ratios
#     with an exactly zero mean have no defined orientation and are dropped.
#   * correlations are signed by that orientation, as `rho`, whenever the input
#     carries a raw `cor` column.
validate_common_gates <- function(gates) {
  required <- c("minNumStocks", "nmonth_min", "nlastyear_required")
  missing <- setdiff(required, names(gates))
  if (length(missing) > 0L) {
    stop("Common gate settings are missing: ", paste(missing, collapse = ", "))
  }
  bad <- vapply(
    gates[required],
    function(x) !is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0,
    logical(1)
  )
  if (any(bad)) {
    stop("Common gate settings must be single non-negative finite numbers: ",
         paste(required[bad], collapse = ", "))
  }
  gates[required]
}

apply_common_gates <- function(pairs, gates = globalSettings$benchmark$gates) {
  gates <- validate_common_gates(gates)
  gated <- data.table::as.data.table(pairs)
  required <- c(
    "rbar", "min_nstock_long", "min_nstock_short", "nmonth", "nlastyear"
  )
  missing <- setdiff(required, names(gated))
  if (length(missing) > 0L) {
    stop("Cannot apply the common accounting-ratio gates; input is missing ",
         "column(s): ", paste(missing, collapse = ", "))
  }
  gated <- gated[
    !is.na(rbar) &
      min_nstock_long >= gates$minNumStocks / 2 &
      min_nstock_short >= gates$minNumStocks / 2 &
      nmonth >= gates$nmonth_min &
      nlastyear == gates$nlastyear_required
  ]
  gated[, orientation := sign(rbar)]
  gated <- gated[orientation != 0]
  if ("cor" %in% names(gated)) {
    gated[, rho := cor * orientation]
  }
  data.table::setattr(gated, "common_gates", gates)
  gated[]
}

# Correlation screen shared by benchmarks A2 and A4. Expects the signed
# correlation `rho` written by apply_common_gates(); pairs without a measured
# correlation cannot be shown to be uncorrelated and are dropped.
apply_uncorrelated_screen <- function(
    pairs, corr_max = globalSettings$benchmark$corr_max) {
  screened <- data.table::as.data.table(pairs)
  if (!"rho" %in% names(screened)) {
    stop("The uncorrelated screen needs the signed correlation `rho`; run ",
         "apply_common_gates() on an input carrying `cor` first.")
  }
  screened[!is.na(rho) & rho <= corr_max]
}

SelectDMStrats <- function(insampsum, settings,
                           gates = globalSettings$benchmark$gates) {
  # input:
  #     insampsum = summary stats for each pubname, dmname combination
  #     dmset = settings for selection
  #     gates = common accounting-ratio gates (see apply_common_gates)
  # output: matchcur = all pubname, dmname that satisfy dmset

  # add derivative statistics
  insampsum <- insampsum %>%
    # The same mining universe is repeated for every publication sharing a
    # sample window. Rank each publication's copy separately so absolute-rank
    # restrictions retain the intended number of mined predictors per paper.
    group_by(pubname, sweight, sampstart, sampend) %>%
    arrange(desc(abs(tstat))) %>%
    mutate(rank_tstat = row_number()) %>%
    arrange(desc(abs(rbar))) %>%
    mutate(rank_rbar = row_number(), n_dm_tot = n()) %>%
    mutate(
      diff_rbar = abs(rbar * sign(rbar) - rbar_op),
      diff_tstat = abs(tstat * sign(rbar) - tstat_op)
    ) %>%
    setDT()

  # Impose the common gates. Ranks are deliberately computed above, on the
  # ungated universe: t_rankpct_min selects the top x% of all mined ratios and
  # then keeps the ones that clear the gates, not the top x% of gated ratios.
  insampsum <- apply_common_gates(insampsum, gates)

  # filter
  matchcur <- insampsum[
    diff_rbar <= settings$r_tol &
      diff_tstat <= settings$t_tol &
      diff_rbar / abs(rbar_op) <= settings$r_reltol &
      diff_tstat / abs(tstat_op) <= settings$t_reltol &
      abs(tstat) > settings$t_min &
      abs(tstat) < settings$t_max &
      rank_tstat / n_dm_tot <= settings$t_rankpct_min / 100
  ]

  print("summary of matching:")
  matchcur[, .(n_dm_match = .N, sampstart = min(sampstart), sampend = min(sampend)), by = "pubname"] %>%
    arrange(-n_dm_match) %>%
    print()
  
  return(matchcur)
  
  print("end selectStrats")
}

# Select the broad accounting-mining benchmark used by the raw and
# factor-adjusted research-versus-data-mining comparisons.  This named
# selector deliberately ignores published-return and published-t-stat
# distances: factor adjustment must start from the same raw |t| > 2 universe
# as the raw benchmark.
select_accounting_t2_pairs <- function(
    insampsum,
    t_threshold = globalSettings$benchmark$t_min,
    gates = globalSettings$benchmark$gates,
    pubnames = NULL) {
  pairs <- data.table::copy(data.table::as.data.table(insampsum))
  required <- c(
    "pubname", "sampstart", "sampend", "sweight", "dmname", "rbar",
    "tstat", "min_nstock_long", "min_nstock_short", "nmonth", "nlastyear"
  )
  missing <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop("Accounting t>2 catalog is missing column(s): ",
         paste(missing, collapse = ", "))
  }

  pairs[, sweight := tolower(sweight)]
  pairs <- apply_common_gates(pairs, gates)
  # A1's own screen: significant raw in-sample t-statistic.
  pairs <- pairs[!is.na(tstat) & abs(tstat) > t_threshold]
  if (!is.null(pubnames)) {
    pairs <- pairs[pubname %in% pubnames]
  }
  data.table::setorder(pairs, pubname, sweight, dmname)
  if (anyDuplicated(pairs, by = c("pubname", "sweight", "dmname"))) {
    stop("Accounting t>2 pair keys are not unique.")
  }
  pairs
}

accounting_t2_pair_fingerprint <- function(pairs) {
  pairs <- data.table::copy(data.table::as.data.table(pairs))
  required <- c("pubname", "sweight", "dmname", "sampstart", "sampend")
  missing <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop("Cannot fingerprint accounting t>2 pairs; missing: ",
         paste(missing, collapse = ", "))
  }
  data.table::setorderv(pairs, required)
  keys <- paste(
    pairs$pubname, pairs$sweight, pairs$dmname,
    as.numeric(pairs$sampstart), as.numeric(pairs$sampend), sep = "\t"
  )
  digest::digest(paste(keys, collapse = "\n"),
                 algo = "sha256", serialize = FALSE)
}

# Select the canonical mined/published candidate-pair universe. This is the
# compact replacement for the pair keys formerly implicit in MatchPub.RData.
select_matched_dm_pairs <- function(
    insampsum,
    t_tol = globalSettings$benchmark$t_tol,
    r_tol = globalSettings$benchmark$r_tol,
    t_reltol = globalSettings$benchmark$matched_uncorr_t_reltol,
    r_reltol = globalSettings$benchmark$matched_uncorr_r_reltol,
    gates = globalSettings$benchmark$gates,
    pubnames = NULL) {
  pairs <- data.table::copy(data.table::as.data.table(insampsum))
  required <- c(
    "pubname", "sweight", "dmname", "rbar_op", "tstat_op", "sampstart",
    "sampend", "rbar", "tstat", "min_nstock_long", "min_nstock_short",
    "nmonth", "nlastyear"
  )
  missing <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop("Pair catalog is missing column(s): ", paste(missing, collapse = ", "))
  }

  pairs[, sweight := tolower(sweight)]
  pairs <- apply_common_gates(pairs, gates)
  # `sign` is the downstream name for the orientation; materialize_matched_dm_
  # returns() and the Section 5 inspection tables both read it.
  pairs[, `:=`(
    sign = orientation,
    diff_rbar = abs(rbar * orientation - rbar_op),
    diff_tstat = abs(tstat * orientation - tstat_op)
  )]
  # A3's own screen: in-sample t-statistic and mean return each within a
  # relative tolerance of the published signal's.
  pairs <- pairs[
    diff_rbar <= r_tol &
      diff_tstat <= t_tol &
      diff_rbar / abs(rbar_op) <= r_reltol &
      diff_tstat / abs(tstat_op) <= t_reltol
  ]
  if (!is.null(pubnames)) {
    pairs <- pairs[pubname %in% pubnames]
  }
  data.table::setorder(pairs, pubname, sweight, dmname)
  if (anyDuplicated(pairs, by = c("pubname", "sweight", "dmname"))) {
    stop("Selected mined/published pair keys are not unique.")
  }
  pairs
}

# Materialize pair-month returns only for selected keys. The durable inputs are
# the compact pair catalog and mined long-short universe; callers should keep
# this large panel in memory only as long as their calculation requires it.
materialize_matched_dm_returns <- function(pair_catalog, DMname) {
  pairs <- data.table::copy(data.table::as.data.table(pair_catalog))
  required <- c(
    "pubname", "sweight", "dmname", "sampstart", "sampend", "sign"
  )
  missing <- setdiff(required, names(pairs))
  if (length(missing) > 0L) {
    stop("Selected pairs are missing column(s): ", paste(missing, collapse = ", "))
  }
  pairs <- unique(pairs[, ..required])
  pairs[, sweight := tolower(sweight)]
  if (anyDuplicated(pairs, by = c("pubname", "sweight", "dmname"))) {
    stop("Pair-month materialization requires unique composite pair keys.")
  }

  stratdat <- readRDS(DMname)
  dm_rets <- data.table::as.data.table(stratdat$ret)
  dm_info <- data.table::as.data.table(stratdat$port_list)[, .(portid, sweight)]
  rm(stratdat)
  dm_rets <- merge(dm_rets, dm_info, by = "portid", all.x = TRUE)
  dm_rets <- dm_rets[, .(
    sweight = tolower(sweight), dmname = signalid, yearm, ret
  )]

  candidate_returns <- dm_rets[
    pairs,
    on = c("sweight", "dmname"),
    nomatch = 0L,
    allow.cartesian = TRUE
  ]
  candidate_returns <- candidate_returns[, .(
    candSignalname = dmname,
    eventDate = as.integer(round(12 * (yearm - sampend))),
    sign,
    ret = ret * sign,
    samptype = data.table::fcase(
      yearm >= sampstart & yearm <= sampend, "insamp",
      yearm > sampend, "oos",
      default = NA_character_
    ),
    actSignal = pubname,
    sweight
  )]
  data.table::setcolorder(
    candidate_returns,
    c("candSignalname", "eventDate", "sign", "ret", "samptype",
      "actSignal", "sweight")
  )
  candidate_returns
}

# Build the matched and matched-uncorrelated pair universes in memory. The
# returned candidate-month panel is intentionally transient: Chapter 3 uses it
# to aggregate the benchmark, while Appendix Table B.1 reuses it to estimate
# individual-DM regressions without a durable pair-cache file.
build_matched_uncorr_pair_data <- function(
    insampsum,
    published_metadata,
    DMname,
    gates = globalSettings$benchmark$gates,
    maximum_pairwise_correlation = globalSettings$benchmark$corr_max) {
  gates <- validate_common_gates(gates)
  published_metadata <- data.table::copy(
    data.table::as.data.table(published_metadata)
  )
  required_published <- c(
    "pubname", "published_rbar", "published_tstat", "sampstart",
    "sampend", "sweight", "pubdate"
  )
  missing <- setdiff(required_published, names(published_metadata))
  if (length(missing) > 0L) {
    stop(
      "Published matching metadata is missing column(s): ",
      paste(missing, collapse = ", ")
    )
  }
  published_metadata[, sweight := tolower(sweight)]

  pair_catalog <- select_matched_dm_pairs(
    insampsum,
    gates = gates,
    pubnames = published_metadata$pubname
  )
  candidate_returns <- materialize_matched_dm_returns(pair_catalog, DMname)

  diagnostics <- candidate_returns[
    samptype == "insamp",
    .(
      sign = data.table::first(sign),
      nmonth_insamp = sum(!is.na(ret)),
      rbar_insamp_matched = mean(ret),
      tstat_insamp_matched = {
        n <- sum(!is.na(ret))
        s <- stats::sd(ret, na.rm = TRUE)
        if (n > 1L && is.finite(s) && s > 0) {
          mean(ret, na.rm = TRUE) / s * sqrt(n)
        } else {
          NA_real_
        }
      }
    ),
    by = .(pubname = actSignal, matched_name = candSignalname)
  ]
  diagnostics <- merge(
    diagnostics,
    published_metadata,
    by = "pubname",
    all.x = TRUE
  )

  # The signed correlation is carried over from the gated pair catalog rather
  # than re-derived, so A2 and A4 cannot drift apart in how `rho` is signed.
  correlations <- pair_catalog[, .(
    pubname, sweight, matched_name = dmname, rho
  )]
  if (anyDuplicated(correlations, by = c("pubname", "sweight", "matched_name"))) {
    stop("Matched-pair correlations do not have unique composite keys.")
  }
  diagnostics <- merge(
    diagnostics,
    correlations,
    by = c("pubname", "sweight", "matched_name"),
    all.x = TRUE
  )
  diagnostics[, `:=`(
    mean_return_rel_distance =
      abs(rbar_insamp_matched - published_rbar) / abs(published_rbar),
    tstat_rel_distance =
      abs(tstat_insamp_matched - published_tstat) / abs(published_tstat),
    # select_matched_dm_pairs() already imposed the history gate on the summary
    # statistics. Re-checking it against the materialized panel is a cheap guard
    # against the two ever disagreeing; it should never bind.
    passes_history = nmonth_insamp >= gates$nmonth_min
  )]
  if (any(!diagnostics$passes_history)) {
    warning(
      sum(!diagnostics$passes_history),
      " gated pair(s) fall short of the in-sample history gate when recomputed ",
      "from the materialized panel; summary statistics and returns disagree."
    )
  }
  data.table::setorder(diagnostics, pubname, sweight, matched_name)

  # A3 is the history-qualified set; A4 adds the correlation screen that A2
  # also uses.
  history_pairs <- diagnostics[passes_history == TRUE]
  uncorr_pairs <- apply_uncorrelated_screen(
    history_pairs, maximum_pairwise_correlation
  )
  if (nrow(uncorr_pairs) == 0L) {
    stop("The matched-uncorrelated screens retained no pairs.")
  }

  list(
    candidate_returns = candidate_returns,
    history_pairs = history_pairs,
    uncorr_pairs = uncorr_pairs
  )
}

matched_pair_fingerprint <- function(pairs) {
  pairs <- data.table::as.data.table(pairs)
  keys <- paste(pairs$pubname, pairs$matched_name, sep = "\t")
  digest::digest(paste(keys, collapse = "\n"),
                 algo = "sha256", serialize = FALSE)
}


make_DM_event_returns <- function(
    match_strats,
    DMname = paste0('../Data/Processed/',
                    globalSettings$universe$dataVersion, 
                    ' LongShort.RData'),
    npubmax = Inf,
    czsum,
    use_sign_info = TRUE,
    ncores = globalSettings$compute$num_cores
) {
  # input: match_strats = summary stats for each selected pubname, dmname pair
  #     outname = name of RDS output
  # you need to pass in czsum (can't use the global) because of
  # a mysterious dopar error (object 'czsum' not found)
  # output: for each pubname-eventDate, average dm returns
  gc()
  
  # Read the large serialized object once. Reading each member separately
  # deserializes the complete file twice and creates a large transient copy.
  stratdat <- readRDS(DMname)
  dm_rets <- stratdat$ret
  dm_info <- stratdat$port_list
  rm(stratdat)
  
  dm_rets <- dm_rets %>%
    left_join(
      dm_info %>% select(portid, sweight),
      by = c("portid")
    ) %>%
    transmute(
      sweight,
      dmname = signalid,
      yearm,
      ret,
      nstock_long,
      nstock_short
    ) %>%
    setDT()
  
  # The Unix multicore backend shares the read-only mined-return panel
  # copy-on-write. PSOCK workers serialize a separate multi-GB copy to every
  # worker and previously exhausted a 62 GB machine in chapter 3. Retain a
  # conservative portable fallback for other platforms.
  cl <- NULL
  if (.Platform$OS.type == "unix") {
    doParallel::registerDoParallel(cores = ncores)
  } else {
    fallback_cores <- min(ncores, 2L)
    warning(
      "make_DM_event_returns uses at most two PSOCK workers on non-Unix systems ",
      "to limit copies of the mined-return panel."
    )
    cl <- parallel::makePSOCKcluster(fallback_cores)
    doParallel::registerDoParallel(cl)
  }
  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  npub <- dim(czsum)[1]
  npub <- min(npub, npubmax)
  event_dm_scaled <- foreach(
    pubi = 1:npub,
    .combine = rbind,
    .packages = c("data.table", "tidyverse", "zoo")
  ) %dopar% {
    # feedback
    print(paste0("pubi ", pubi, " of ", npub))
    
    pubcur <- czsum[pubi, ]
    
    # select matching dm strats for the current pubname
    matchcur <- match_strats[pubname == pubcur$signalname]
    
    matchcur <- matchcur %>%
      transmute(sweight, dmname, sign = sign(rbar), rbar)
    
    # make an event time panel
    eventpan <- dm_rets %>%
      inner_join(matchcur, by = c("sweight", "dmname")) %>%
      transmute(
        candSignalname = dmname,
        eventDate = as.integer(round(12 * (yearm - pubcur$sampend))),
        sign,
        # scale returns
        ret_scaled = ret * sign / abs(rbar) * 100,
        ret_unscaled = ret * sign * 100,
        # # sign returns (sanity check)
        # ret_scaled = ifelse(use_sign_info, sign*ret_scaled, ret_scaled),
        samptype = case_when(
          (yearm >= pubcur$sampstart) & (yearm <= pubcur$sampend) ~ "insamp",
          (yearm > pubcur$sampend) ~ "oos",
          TRUE ~ NA_character_
        )
      )
    
    if (use_sign_info==FALSE){
      # remove sign_info if requested (for testing)
      eventpan[ , ret_scaled := sign*ret_scaled]
      eventpan[ , ret_unscaled := sign*ret_unscaled]
    }
    
    # average down to one matched return per event date
    eventsumscaled <- eventpan[, .(dm_mean = mean(ret_scaled),
                                   dm_mean_unscaled = mean(ret_unscaled),
                                   dm_sd = sd(ret_scaled), dm_n = .N),
                               by = c("eventDate",'samptype')
    ] %>%
      mutate(pubname = pubcur$signalname)
    
    return(eventsumscaled)
  } # end do pubi = 1:npub
  
  if (!is.null(cl)) parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  on.exit(NULL, add = FALSE)
  
  return(event_dm_scaled)
} # end MakeMatchedPanel

adj_R2_with_PPCA <- function(
    DMname = paste0('../Data/Processed/',
                    globalSettings$universe$dataVersion, 
                    ' LongShort.RData'),
    nsampmax = Inf,
    ncores = globalSettings$compute$num_cores) {
  # convert czsum to data.table (this should be done more globally)
  setDT(czsum)
  
  # Read the large serialized object once; see make_DM_event_returns().
  stratdat <- readRDS(DMname)
  dm_rets <- stratdat$ret
  dm_info <- stratdat$port_list
  rm(stratdat)
  
  dm_rets <- dm_rets %>%
    left_join(
      dm_info %>% select(portid, sweight), by = c("portid")
    ) %>%
    transmute(
      sweight, dmname = signalid, yearm, ret, nstock_long, nstock_short
    ) %>%
    setDT()
  
  # Finds sum stats for dm in each pub sample
  # the output for this can be used for all dm selection methods
  samplist <- czsum %>%
    distinct(sampstart, sampend) %>%
    arrange(sampstart, sampend)
  
  # set up for parallel
  cl <- NULL
  if (.Platform$OS.type == "unix") {
    doParallel::registerDoParallel(cores = ncores)
  } else {
    fallback_cores <- min(ncores, 2L)
    warning(
      "adj_R2_with_PPCA uses at most two PSOCK workers on non-Unix systems ",
      "to limit copies of the mined-return panel."
    )
    cl <- parallel::makePSOCKcluster(fallback_cores)
    doParallel::registerDoParallel(cl)
  }
  on.exit({
    if (!is.null(cl)) parallel::stopCluster(cl)
    foreach::registerDoSEQ()
  }, add = TRUE)
  
  # loop setup
  nsamp <- dim(samplist)[1]
  nsamp <- min(nsamp, nsampmax)
  dm_insamp <- list()
  
  # dopar in a function needs some special setup
  # https://stackoverflow.com/questions/6689937/r-problem-with-foreach-dopar-inside-function-called-by-optim
  start_time <- Sys.time()
  print(start_time)
  dm_insamp <- foreach(
    sampi = 1:nsamp,
    .combine = rbind,
    .packages = c("data.table", "tidyverse", "zoo", "pcaMethods", "broom"),
    .export = c("samplist", "dm_rets", "czsum", "czret", "nsamp")
  ) %dopar% {
    #) %do% {
    # feedback
    print(paste0("DM sample stats for sample ", sampi, " of ", nsamp))
    print(Sys.time())
    # find sum stats for the current sample
    sampcur <- samplist[sampi, ]
    sumcur <- dm_rets[
      yearm >= sampcur$sampstart & yearm <= sampcur$sampend &
        !is.na(ret),
      .(
        rbar = mean(ret), tstat = mean(ret) / sd(ret) * sqrt(.N),
        min_nstock_long = min(nstock_long),
        min_nstock_short = min(nstock_short),
        nmonth = sum(!is.na(ret))
      ),
      by = c("sweight", "dmname")
    ]
    # find number of obs in the last year of the sample
    filtcur <- dm_rets[
      floor(yearm) == year(sampcur$sampend) &
        !is.na(ret),
      .(nlastyear = .N),
      by = c("sweight", "dmname")
    ]
    # Perform the left join
    sumcur <- sumcur[filtcur, on = .(sweight, dmname)]
    
    # Add sampstart and sampend columns
    sumcur[, `:=`(sampstart = sampcur$sampstart, sampend = sampcur$sampend)]
    
    # expand with published signalnames available by then and reorg
    pubnamelist = czsum[sampend <= sampcur$sampend, .(signalname)] %>% 
      rename(pubname = signalname)
    # pubsumcur = expand_grid(pubnamelist, sumcur) %>% 
    #   select(pubname, sampstart, sampend, everything()) %>% 
    #   setDT()
    # merge pub returns onto dm returns, temporarily
    tempret_pca = czret[signalname %in% pubnamelist$pubname & date >= sampcur$sampstart
                        & date <= sampcur$sampend, .(signalname, date, ret)]
    npcs <- min(5, pubnamelist[, .N])
    # Pivot the data to wide format
    # Pivot the data to wide format
    tempret_wide <- dcast(tempret_pca, date ~ signalname, value.var = "ret")
    
    # Check the number of columns
    if (ncol(tempret_wide) < 7) {
      # Run regression with the columns in tempret_wide
      formula_temp <- paste('ret ~ ',  colnames(tempret_wide)[-1] %>% paste(., collapse = ' + ')) %>% as.formula()
      dm_rets2 <- dm_rets[tempret_wide, on = .(yearm = date)]
      dm_rets2 <- dm_rets2[!is.na(dmname)]
      dm_rets2[, available_obs := .N, by  = c("dmname", "sweight")]
      sumcur[, npcs := 0]
      
    } else {
      # Perform PCA and run regression with PCA scores
      pca_model <- pca(tempret_wide[,-1, with=FALSE] %>% as.matrix(), method = "ppca", nPcs = npcs)
      pca_scores <- scores(pca_model)
      formula_pca <- paste('ret ~ ',  colnames(pca_scores) %>% paste(., collapse = ' + ')) %>% as.formula()
      pca_scores <- data.table(date = tempret_wide$date, pca_scores)
      dm_rets2 <- dm_rets[pca_scores, on = .(yearm = date)]
      dm_rets2 <- dm_rets2[!is.na(dmname)]
      dm_rets2[, available_obs := .N, by  = c("dmname", "sweight")]
      dm_rets2[available_obs > 30, adj_r2 := summary(lm(formula = formula_pca, data = .SD))$adj.r.squared, by = c("dmname", "sweight")]
      sumcur[, npcs := npcs]
    }
    
    adj_r2_dt <- dm_rets2[, {
      model <- lm(formula = if (ncol(tempret_wide) < 7) formula_temp else formula_pca, data = .SD)
      model_summary <- summary(model)
      .(r2 = model_summary$r.squared,
        adj_r2 = model_summary$adj.r.squared,
        N_pca = .N)
    }, by = c("dmname", "sweight")]
    
    test <- merge(sumcur, adj_r2_dt)
    return(test)
  } # end dm_insamp loop
  stop_time <- Sys.time()
  print(stop_time - start_time)
  if (!is.null(cl)) parallel::stopCluster(cl)
  foreach::registerDoSEQ()
  on.exit(NULL, add = FALSE)
  
  return(dm_insamp)
} # end Sumstats function




# Function for outputting tables (Table of DM predictors that performed similarly to published signal "name")
inspect_one_pub = function(name){
  
  # make small dat with doc for dm signals
  smallsum = allret[
    actSignal == name & !is.na(samptype) & !is.na(ret)
    , .(rbar = mean(ret), n = .N, t = mean(ret)/sd(ret)*sqrt(.N), sign = mean(sign))
    , by = c('source','actSignal','candSignalname','samptype')
  ] %>% 
    pivot_wider(names_from = samptype, values_from = c(rbar,n,t)) %>% 
    left_join(
      stratdat$signal_list %>% rename(candSignalname = signalid)
      , by = 'candSignalname'    
    ) %>% 
    arrange(desc(source)) %>% 
    select(actSignal, source, v1, v2, signal_form, everything()) %>% 
    select(-c(candSignalname, t_oos)) %>% 
    setDT() 
  
  # add mean
  smallsum = smallsum %>% 
    bind_rows(
      smallsum %>% 
        filter(source == '2_dm') %>% 
        summarize(across(where(is.numeric), mean)) %>% 
        mutate(source = '3_dm_mean')
    )
  
  # plug in and format
  smallsum2 = smallsum %>%
    # change format of formulas
    mutate(
      signal_form = if_else(signal_form == 'v1/v2','(v1)/(v2)', signal_form)
      , signal_form = str_replace_all(signal_form, '\\(', '\\[')
      , signal_form = str_replace_all(signal_form, '\\)', '\\]')    
      , signal_form = str_replace_all(signal_form, 'pdiff', '%$\\\\Delta$')    
      , signal_form = str_replace_all(signal_form, 'diff', '$\\\\Delta$')    
    ) %>% 
    left_join(
      compdoc %>% transmute(v1 = acronym, v1long = substr(shortername,1,24))
    ) %>% 
    left_join(
      compdoc %>% transmute(v2 = acronym, v2long = substr(shortername,1,24))
    ) %>%   
    mutate(
      signal = str_replace(signal_form, 'v1', v1long)
      , signal = str_replace(signal, 'v2', v2long)
    ) %>% 
    # select(-c(actSignal, ends_with('long'))) %>%
    select(-c(actSignal)) %>%     
    select(source, signal, everything()) 
  
  # clean up for output
  #   compute sample periods
  tempsamp = paste(
    year(czsum2[signalname == name, ]$sampstart) 
    , year(czsum2[signalname == name, ]$sampend)
    , sep = '-'
  )
  tempoos = paste(
    year(czsum2[signalname == name, ]$sampend) +1
    , min(as.numeric(floor(max(stratdat$ret$yearm)))
          , max(year(czret2[signalname == name]$date)))
    , sep = '-'
  )  
  
  # make table
  tabout  = smallsum2 %>% 
    as_tibble() %>% 
    mutate(dist = abs(rbar_insamp - smallsum2[source == '1_pub']$rbar_insamp)) %>% 
    select(
      source,signal,sign,starts_with('rbar_')
      , dist, v1, v2, signal_form
      , t_insamp
      , v1long, v2long
    ) %>% 
    mutate(across(where(is.numeric), round, 2)) %>% 
    rename(setNames('rbar_oos', tempoos)) %>% 
    rename(setNames('rbar_insamp', tempsamp)) %>% 
    arrange(source, dist) %>% 
    group_by(source) %>% 
    mutate(id = if_else(source == '2_dm', row_number(), NA_integer_)) %>% 
    ungroup() %>% 
    select(source, id, everything())
  
} # end inspect_one_pub


# wrap in function for easy editing of xlsx
import_docs = function(){
  # read compustat acronyms
  dmdoc = readRDS(dmcomp$name)$signal_list %>%  setDT() 
  yzdoc = readxl::read_xlsx('DataInput/Updated_Yan-Zheng-Compustat-Vars.xlsx') %>% 
    transmute(acronym = tolower(acronym), shortername ) %>% 
    setDT() 
  
  # merge
  dmdoc = dmdoc[ 
    , signal_form := if_else(signal_form == 'diff(v1)/lag(v2)', 'd_', '')] %>% 
    merge(yzdoc[,.(acronym,shortername)], by.x = 'v1', by.y = 'acronym') %>%
    rename(v1long = shortername) %>%
    merge(yzdoc[,.(acronym,shortername)], by.x = 'v2', by.y = 'acronym') %>%
    rename(v2long = shortername) 
  
  # create link table
  dm_linktable = expand_grid(sweight = c('ew','vw'), dmname =  dmdoc$signalid) %>% 
    mutate(dmcode = paste0(sweight, '|', dmname))  %>% 
    left_join(dmdoc, by = c('dmname' = 'signalid')) %>%
    mutate(shortdesc = paste0(substr(dmcode,1,3), signal_form, v1, '/', v2)
           , desc = if_else(signal_form=='d_'
                            , paste0('d_[', v1long, ']/lag[', v2long, ']')
                            , paste0('[', v1long, ']/[', v2long, ']')
           )) %>% 
    setDT()
  
  return(dm_linktable)
  
} # end import_docs
