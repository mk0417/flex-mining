# Section 2 calendar-standard-error comparison figure.
# How to run: Rscript S2a_ResearchVsDMPlots.R from code/.
# Inputs: cleaned published returns and chapter-3 plot panels.
# Outputs: ../Results/Fig_DM_t_min_2_se_indicators_calendar.pdf

# Fast Setup --------------------------------------------------------

rm(list = ls())
source("0_Environment.R")

## Load chapter-3 plot panel -------------------------------------------
ret_for_plot0 <- readRDS("../Data/Processed/ret_for_plot0.RDS")

# Default Plot Settings --------------------------------------------------

# aesthetic settings
fontsizeall = 28
ylaball = 'Trailing 5-Year Return (bps pm)'
linesizeall = 1.5

# x-axis range for all plots
global_xl = -360  # x-axis lower bound
global_xh = 300   # x-axis upper bound

## plot with Calendar SE --------------------------------------------------

tempsuffix = "t_min_2_se_indicators_calendar"

printme = ReturnPlotsWithDM_std_errors_indicators(
  dt = ret_for_plot0 %>% filter(!is.na(matchRet)),
  basepath = "../Results/temp_",
  suffix = tempsuffix,
  rollmonths = 60,
  colors = colors,
  labelmatch = FALSE,
  yl = -0,
  yh = 125,
  xl = global_xl,
  xh = global_xh,
  legendlabels =
    c(
      paste0("Published (and Peer Reviewed)"),
      paste0("Data-Mined for |t|>2.0 in Original Sample"),
      'N/A'
    ),
  legendpos = c(35,20)/100,
  fontsize = fontsizeall,
  yaxislab = ylaball,
  linesize = linesizeall
)

# custom edits 
(
  printme + theme(
  legend.background = element_rect(fill = "white", color = "black"
    , size = 0.3)
  # remove space where legend would be
  , legend.margin = margin(-1.0, 0.5, 0.5, 0.5, "cm")
  , legend.position  = c(44,15)/100
  # add space between legend items
  , legend.spacing.y = unit(0.2, "cm")
  ) +
  guides(color = guide_legend(byrow = TRUE))
) %>% 
  ggsave(filename = paste0("../Results/Fig_DM_", tempsuffix, '.pdf'), width = 10, height = 8)

file.remove(paste0("../Results/temp__", tempsuffix, ".pdf"))
