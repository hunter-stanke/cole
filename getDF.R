getDFCarbon <- function(inputStates, 
                        most_recent,
                        inputYearMin, 
                        inputYearMax,
                        fiaMethod = carbon, 
                        aoi = NULL, 
                        byPool = NULL,
                        byComponent = NULL,
                        landType = "forest",
                        grpBy = c(),
                        areaDomain = NULL,
                        fia_dir = "./FIA") {
  # Check that areaDomain string isn't empty
  if(length(areaDomain) == 0){
    areaDomain = NULL
  }
  
  # If no states are selected, return empty df
  if(is.null(inputStates)) {
    return(as.data.frame(c()))
  }

  ## Set up remote FIA.Database
  db <- readFIA(states = inputStates, dir = fia_dir, inMemory = FALSE)
  
  ## If most recent subset, clip
  if (most_recent) {
    db <- clipFIA(db, mostRecent = TRUE)
  }
  
  # Hit rFIA
  df <- as.data.frame(fiaMethod(
    db = db,
    polys = aoi,
    byPool = byPool, 
    byComponent = byComponent,
    landType = landType,
    grpBy = eval(grpBy),
    areaDomain = eval(parse(text = areaDomain)),
    totals = TRUE,
    nCores = 8)) 
  
  if (!most_recent) {
    df <- df %>%
      ## Time range filter - faster to do this than subset ahead of time
    dplyr::filter(YEAR >= lubridate::year(inputYearMin),
                  YEAR <= lubridate::year(inputYearMax))
  }

  
  
  ## Set up example code
  read.string <- paste0(
    "## Set up remote FIA Database\ndb <- readFIA(dir = '", fia_dir, 
    "',\n\tstates = c(", paste(paste0("'", inputStates, "'"), collapse = ', '), '),',
    "\n\tinMemory = FALSE)\n\n"
  )
  clip.string <- paste0(
    "## Select most recent inventories by state\n",
    "db <- clipFIA(db, mostRecent = TRUE)\n\n"
  )
  function.string <- paste0("## Produce estimates w/ carbon function\n", "carbon(db,\n\t", 
                            "polys = ", if (is.null(aoi)) 'NULL,\n\t' else 'aoi,\n\t',
                            "byPool = ", byPool, ",\n\tbyComponent = ", byComponent, ", \n\t",
                            "landType = ", '"', landType, '", \n\t',
                            "grpBy = ", if(length(grpBy)) parse(text = grpBy) else "NULL", ",\n\t",
                            "areaDomain = ", if(nchar(areaDomain)) parse(text = areaDomain) else "NULL\n\t",
                            "totals = TRUE)") 
  if (most_recent) {
    output.string <- paste0(read.string, clip.string, function.string)
  } else {
    output.string <- paste0(read.string, function.string)
  }
  return(list(df, output.string))
}


getDFArea <- function(inputStates, 
                      most_recent,
                      inputYearMin, 
                      inputYearMax,
                      fiaMethod = area, 
                      aoi = NULL, 
                      landType = "forest",
                      grpBy = c(),
                      areaDomain = NULL,
                      fia_dir = "./FIA") {
  # Check that areaDomain string isn't empty
  if(length(areaDomain) == 0){
    areaDomain = NULL
  }
  
  # If no states are selected, return empty df
  if(is.null(inputStates)) {
    return(as.data.frame(c()))
  }
  
  ## Set up remote FIA.Database
  db <- readFIA(states = inputStates, dir = fia_dir, inMemory = FALSE)
  
  ## If most recent subset, clip
  if (most_recent) {
    db <- clipFIA(db, mostRecent = TRUE)
  }
  


  # Hit rFIA
  df <- rFIA::area(
    db = db,
    polys = aoi,
    landType = landType,
    grpBy = eval(grpBy),
    areaDomain = eval(parse(text = areaDomain)),
    totals = TRUE,
    nCores = 8) 
  
  if (!most_recent) {
    df <- df %>%
      ## Time range filter - faster to do this than subset ahead of time
      dplyr::filter(YEAR >= lubridate::year(inputYearMin),
                    YEAR <= lubridate::year(inputYearMax))
  }
  
  
  ## Set up example code
  read.string <- paste0(
    "## Set up remote FIA Database\ndb <- readFIA(dir = '", fia_dir, 
    "',\n\tstates = c(", paste(paste0("'", inputStates, "'"), collapse = ', '), '),',
    "\n\tinMemory = FALSE)\n\n"
  )
  clip.string <- paste0(
    "## Select most recent inventories by state\n",
    "db <- clipFIA(db, mostRecent = TRUE)\n\n"
  )
  function.string <- paste0("## Produce estimates w/ area function\n", "area(db,\n\t", 
                            "polys = ", if (is.null(aoi)) 'NULL,\n\t' else 'aoi,\n\t',
                            "landType = ", '"', landType, '", \n\t',
                            "grpBy = ", if(length(grpBy)) parse(text = grpBy) else "NULL", ",\n\t",
                            "areaDomain = ", if(nchar(areaDomain)) parse(text = areaDomain) else "NULL\n\t",
                            "totals = TRUE)") 
  if (most_recent) {
    output.string <- paste0(read.string, clip.string, function.string)
  } else {
    output.string <- paste0(read.string, function.string)
  }
  return(list(df, output.string))
}


getDFTree <- function(inputStates, 
                      most_recent,
                      inputYearMin, 
                      inputYearMax,
                      fiaMethod = tpa, 
                      fiaMethodString = "tpa",
                      aoi = NULL, 
                      landType = "forest",
                      grpBy = c(),
                      areaDomain = NULL,
                      treeDomain = NULL,
                      fia_dir = "./FIA") {
  # Check that areaDomain string isn't empty
  if(length(areaDomain) == 0){
    areaDomain = NULL
  }
  if (length(treeDomain) == 0){
    treeDomain <- NULL
  }
  
  # If no states are selected, return empty df
  if(is.null(inputStates)) {
    return(as.data.frame(c()))
  }
  
  ## Set up remote FIA.Database
  db <- readFIA(states = inputStates, dir = fia_dir, inMemory = FALSE)
  
  ## If most recent subset, clip
  if (most_recent) {
    db <- clipFIA(db, mostRecent = TRUE)
  }
  
  # Hit rFIA
  df <- fiaMethod(
    db = db,
    polys = aoi,
    landType = landType,
    grpBy = eval(grpBy),
    areaDomain = eval(parse(text = areaDomain)),
    treeDomain = eval(parse(text = treeDomain)),
    totals = TRUE,
    nCores = 8)
  
  if (!most_recent) {
    df <- df %>%
      ## Time range filter - faster to do this than subset ahead of time
      dplyr::filter(YEAR >= lubridate::year(inputYearMin),
                    YEAR <= lubridate::year(inputYearMax))
  }
  
  
  
  ## Set up example code
  read.string <- paste0(
    "## Set up remote FIA Database\ndb <- readFIA(dir = '", fia_dir, 
    "',\n\tstates = c(", paste(paste0("'", inputStates, "'"), collapse = ', '), '),',
    "\n\tinMemory = FALSE)\n\n"
  )
  clip.string <- paste0(
    "## Select most recent inventories by state\n",
    "db <- clipFIA(db, mostRecent = TRUE)\n\n"
  )
  function.string <- paste0("## Produce estimates w/ ", fiaMethodString, " function\n",fiaMethodString, "(db,\n\t", 
                            "polys = ", if (is.null(aoi)) 'NULL,\n\t' else 'aoi,\n\t',
                            "landType = ", '"', landType, '", \n\t',
                            "grpBy = ", if(length(grpBy)) parse(text = grpBy) else "NULL", ",\n\t",
                            "areaDomain = ", if(nchar(areaDomain)) parse(text = areaDomain) else "NULL\n\t",
                            "treeDomain = ", if(nchar(treeDomain)) parse(text = treeDomain) else "NULL\n\t",
                            "totals = TRUE)") 
  if (most_recent) {
    output.string <- paste0(read.string, clip.string, function.string)
  } else {
    output.string <- paste0(read.string, function.string)
  }
  return(list(df, output.string))
}
