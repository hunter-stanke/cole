## -----------------------------------------------------------------------------
## Download state subsets of the FIADB, and save a reduced version for COLE
##
## Hunter Stanke, 02-21-2022
## -----------------------------------------------------------------------------

## Load packages ---------------------------------------------------------------
library(rFIA)
library(dplyr)


## Set directories -------------------------------------------------------------
# Wherever Git repo is downloaded to
setwd('/home/hunter/cole/') # Change this to your own directory


## Tables/ columns to retain in reduced version --------------------------------
req.tabs <- c('PLOT', 'TREE', 'COND', 'POP_PLOT_STRATUM_ASSGN',
              'POP_ESTN_UNIT', 'POP_EVAL',
              'POP_STRATUM', 'POP_EVAL_TYP', 'POP_EVAL_GRP')
req.vars <- read.csv('./reduced_fiadb_variables.csv')



## Loop over states to download, subset, and save ------------------------------
# Vector of states
allStates <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'ID',
               'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI',
               'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY',
               'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN',
               'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY', 'HI', 'AK')

for (state in allStates) {

  ## Download state subset of FIADB, but don't save
  db <- rFIA::getFIA(states = state)

  ## Drop unneccesary columns
  reduced <- db[req.tabs]
  for (tab in unique(req.vars$table)) {
    reduced[[tab]] <- dplyr::select(reduced[[tab]],
                                    dplyr::filter(req.vars, table == tab)$variable)
  }


  ## Drop unnecessary rows
  pops <- rFIA::getDesignInfo(db,
                              type = c('CURR', 'VOL', 'ALL'),
                              mostRecent = FALSE)
  reduced$PLOT <- dplyr::filter(reduced$PLOT, CN %in% unique(pops$PLT_CN))
  reduced$COND <- dplyr::filter(reduced$COND, PLT_CN %in% unique(pops$PLT_CN))
  reduced$TREE <- dplyr::filter(reduced$TREE, PLT_CN %in% unique(pops$PLT_CN))
  reduced$POP_PLOT_STRATUM_ASSGN <- dplyr::filter(reduced$POP_PLOT_STRATUM_ASSGN,
                                                  EVALID %in% unique(pops$EVALID))
  reduced$POP_STRATUM <- dplyr::filter(reduced$POP_STRATUM,
                                       CN %in% unique(pops$STRATUM_CN))
  reduced$POP_ESTN_UNIT <- dplyr::filter(reduced$POP_ESTN_UNIT,
                                         CN %in% unique(pops$ESTN_UNIT_CN))
  reduced$POP_EVAL <- dplyr::filter(reduced$POP_EVAL,
                                    EVALID %in% unique(pops$EVALID))


  ## Save reduced version of FIADB
  class(reduced) <- 'FIA.Database'
  rFIA::writeFIA(reduced,
                 dir = './FIA-Reduced/',
                 byState = TRUE)

}
