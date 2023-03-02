# add different provider tiles to the output map
addBaseMapSelector <- function(x) {
  x %>%
    addProviderTiles(
      "OpenStreetMap",
      # give the layer a name
      group = "Default"
    ) %>%
    addProviderTiles(
      "Stamen.Terrain",
      group = "Terrain"
    ) %>%
    addProviderTiles(
      "Esri.WorldImagery",
      group = "Imagery"
    ) %>%
    # add a layers control
    addLayersControl(
      baseGroups = c(
        "Default", "Terrain", "Imagery"
      ),
      # position it on the topleft
      position = "bottomleft"
    )
}


getGroupByOptions <- function(grouping.variables.df, 
                              carbonSelected, 
                              treeLevel = FALSE) {
  if(carbonSelected) {
    optionsDF <- grouping.variables.df[!grouping.variables.df$treeLevel, ]
  } else {
    optionsDF <- grouping.variables.df
  }
  
  if(treeLevel) {
    optionsDF <- optionsDF[optionsDF$treeLevel, ]
  }
  
  codes <- as.character(optionsDF$code)
  names(codes) <- as.character(optionsDF$name)
  
  return(codes)
}

# https://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
getShapefile <- function(inputShapefile) {
  if(!is.null(inputShapefile)) {
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- inputShapefile
    shpdf$datapath <- stringr::str_replace(shpdf$datapath, "//", "/")

    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])

    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        # paste0("/private", shpdf$datapath[i]),
        paste0(shpdf$datapath[i]),
        # paste0("/private", dirname(shpdf$datapath[i]), "/", shpdf$name[i])
        paste0(dirname(shpdf$datapath[i]), "/", shpdf$name[i])
      )
    }

    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.

    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)
    return(readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"))
    )
  } else {
    return(NULL)
  }
}


pull_polygon_coordinates <- function(shape) {
  shape.coords <- shape$geometry$coordinates[[1]]
  coords.df <- purrr::map_dfr(.x = shape.coords,
                              .f = ~ data.frame(lon = .x[[1]],
                                                lat = .x[[2]])) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
}

prep_aoi <- function(spatial_domain,
                     merge_spatial_domain,
                     inputStates,
                     inputCounties,
                     inputHUC6,
                     inputForests,
                     inputRegions,
                     states,
                     counties,
                     huc6,
                     national_forests,
                     usfs_regions,
                     drawn_shapes,
                     uploaded_shapes) {
  
  ## Grab the areas needed
  if (spatial_domain == 'states') {
    return(list(aoi = NULL, states = states$State_Abb[states$State %in% inputStates]))
  } else if (spatial_domain == 'counties') {
    aoi <- dplyr::filter(counties, County %in% inputCounties)
  } else if (spatial_domain == 'watersheds') {
    aoi <- dplyr::filter(huc6, HUC6 %in% inputHUC6)
  } else if (spatial_domain == 'national_forests') {
    aoi <- dplyr::filter(national_forests, Forest %in% inputForests)
  } else if (spatial_domain == 'usfs_regions') {
    aoi <- dplyr::filter(usfs_regions, Region %in% inputRegions)
  } else if (spatial_domain == 'draw_polygons') {

    aoi <- purrr::map_dfr(.x = drawn_shapes,
                          .f = pull_polygons) 
    
    ## Force merge
    merge_spatial_domain <- TRUE
    
  } else if (spatial_domain == 'draw_circles') {
    aoi <- purrr::map_dfr(drawn_shapes,
                          .f = ~ data.frame(longitude = .x$geometry$coordinates[[1]],
                                            latitude = .x$geometry$coordinates[[2]],
                                            r = .x$properties$radius)) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
      sf::st_transform(crs = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs") %>%
      sf::st_buffer(dist = .$r) %>%
      sf::st_transform(crs = 4326)
    
    ## Force merge
    merge_spatial_domain <- TRUE
    
  } else if (spatial_domain == 'shapefile') {
    aoi <- getShapefile(uploaded_shapes) %>%
      sf::st_as_sf() %>%
      sf::st_transform(4326) %>%
      sf::st_make_valid()
    
  }
  
  ## Get intersecting states
  if (!(spatial_domain == 'counties')) {
    states_overlap <- sf::st_intersects(states, aoi, sparse = FALSE)
    selected_states <- states$State_Abb[unlist(apply(states_overlap, 1, FUN = function(x) {any(x)}))]
  } else {
    selected_states <- states$State_Abb[states$State %in% unique(aoi$State)]
  }
  
  ## Merge unless told otherwise
  if (merge_spatial_domain) {
    aoi <- aoi %>%
      dplyr::summarise()
  }
  
  return(list(aoi = aoi, states = selected_states))
}


write_rFIA_code <- function(data_type,
                            spatial_domain,
                            merge_spatial_domain,
                            shapefile,
                            states,
                            fia_dir,
                            most_recent,
                            by_pool,
                            tree_type,
                            land_type,
                            grp_by,
                            method,
                            lambda
) {
  
  ## Package setup
  load.packages <- '## Load required packages\nlibrary(rFIA)\nlibrary(dplyr)'
  if (!is.null(shapefile)) load.packages <- paste0(load.packages, '\nlibrary(sf)')
  
  
  ## Set up FIADB
  read.fia <- paste0(
    "## Set up remote FIA Database\ndb <- readFIA(dir = '", fia_dir, 
    "',\n\tstates = c(", paste(paste0("'", states, "'"), collapse = ', '), '),',
    "\n\tinMemory = FALSE)"
  )
  
  
  ## Most recent subset
  clip.fia <- paste0(
    "## Select most recent inventories by state\ndb <- clipFIA(db, mostRecent = TRUE)"
  )
  if (!most_recent) clip.fia <- NULL
  
  ## Read user defined shapefile
  read.shape <- paste0("## Load user-defined spatial data\nshp <- st_read('", shapefile, "')")
  if (is.null(shapefile)) read.shape <- NULL
  
  ## Handle names of canned datasets & any merging
  if (spatial_domain %in% c('counties', 'watersheds', 
                            'usfs_regions', 'national_forests')) {
    shp.name <- paste0('selected_', spatial_domain)
    merge.shape <- paste0("## Dissolve polygon boundaries\n", shp.name,' <- ', shp.name, ' %>% \n\tsummarize()')
    polys <- paste0('\n\tpolys = ', 'selected_', spatial_domain, ',')
  } else if (spatial_domain == 'shapefile') {
    merge.shape <- '## Dissolve polygon boundaries\nshp <- shp %>% \n\tsummarize()'
    polys <- '\n\tpolys = shp,'
  } else if (spatial_domain %in% c('draw_polygons', 'draw_circles')){
    shp.name <- stringr::str_replace(spatial_domain, 'draw', 'user_drawn')
    merge.shape <- paste0("## Dissolve polygon boundaries\n", shp.name,' <- ', shp.name, ' %>% \n\tsummarize()')
    polys <- paste0('\n\tpolys = ', 'selected_', spatial_domain, ',')
  } else {
    polys <- NULL
    merge.shape <- NULL
  }
  if (!merge_spatial_domain) merge.shape <- NULL
  
  
  ## Handle land type
  land <- paste0('\n\tlandType = ', "'", land_type, "'", ',')
  
  ## Handle group by 
  if (!is.null(grp_by)) {
    grp <- paste0('\n\tgrpBy = ', paste0('c(', paste0(grp_by, collapse = ', '), ')'))    
  } else {
    grp <- NULL
  }
  
  ## Handle method
  if (method == 'EMA') {
    lambda <- paste0('\n\tlambda = ', lambda, ',')
  } else {
    lambda <- NULL  
  }
  est <- paste0('\n\tmethod = ', "'", method, "'", ',')
  if (method == 'TI') est <- NULL
  
  
  ## Handle function specific arguments
  if (data_type == 'carbon') {
    pool <- paste0('\n\tbyPool = ', by_pool, ',')
  } else {
    pool <- NULL
  }
  if (data_type %in% c('tpa', 'volume', 'biomass')) {
    type <- paste0('\n\ttreeType = ', "'", tree_type, "'", ',')
  } else {
    type <- NULL
  }
  
  ## Build rFIA function call
  rfia.call <- paste0(
    '## Generate estimates\n',
    data_type, '(db = db,',
    grp,
    polys,
    pool,
    land,
    type,
    est, 
    lambda,
    '\n\ttotals = TRUE)'
  )
  
  ## Build full script
  out <- list(
    load.packages, 
    read.fia, 
    clip.fia, 
    read.shape, 
    merge.shape, 
    rfia.call
    ) %>%
    ## get rid of NULLs without extra \n\n
    unlist() %>%
    paste0(collapse = '\n\n')
  
  return(out)
}
