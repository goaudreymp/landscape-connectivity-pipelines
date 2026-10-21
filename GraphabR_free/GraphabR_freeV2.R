# LIBRARIES AND FUNCTIONS ####
library(graph4lg)
library(tidyverse)
library(terra)
library(igraph)
library(viridis)
library(sf)
library(raster)
library(readxl)
## NEW GRAPHAB_METRIC() with NC, MNC, SLC, deltaIIC capability and deltadPC renewed ####
graphab_metric <- function (proj_name, graph, metric, multihab = FALSE, dist = NULL, 
                            prob = 0.05, beta = 1, cost_conv = FALSE, return_val = TRUE, 
                            proj_path = NULL, alloc_ram = NULL, obj = c("patch","links"))  
{
  if (!is.null(proj_path)) {
    if (!dir.exists(proj_path)) stop(paste0(proj_path, " is not an existing directory"))
    proj_path <- normalizePath(proj_path)
  } else {
    proj_path <- normalizePath(getwd())
  }
  
  if (!inherits(proj_name, "character")) stop("'proj_name' must be a character string")
  if (!(paste0(proj_name, ".xml") %in% list.files(path = file.path(proj_path, proj_name)))) {
    stop("The project you refer to does not exist. Use graphab_project() before.")
  }
  
  proj_end_path <- file.path(proj_path, proj_name, paste0(proj_name, ".xml"))
  
  if (!inherits(graph, "character")) stop("'graph' must be a character string")
  if (!(paste0(graph, "-voronoi.shp") %in% list.files(path = file.path(proj_path, proj_name)))) {
    stop("The graph you refer to does not exist")
  }
  
  list_all_metrics <- c("PC", "EC", "ECh", "IIC", "dPC", "dIIC", "F", "BC", "IF", 
                        "Dg", "CCe", "CF", "Fh", "IFh", "BCh", "NC", "MSC", "SLC")
  list_glob_metrics <- c("PC", "EC", "ECh", "IIC", "dPC", "dIIC", "NC", "MSC", "SLC")
  list_loc_metrics <- setdiff(list_all_metrics, list_glob_metrics)
  list_dist_metrics <- c("PC", "EC", "ECh", "F", "Fh", "BC", "BCh", "IF", "IFh", "dPC")
  # dIIC deliberately excluded from dist metrics
  
  if (metric %in% list_dist_metrics) {
    if (is.null(dist)) stop(paste0("To compute ", metric, ", specify a distance"))
  }
  
  if (metric %in% c("dPC", "dIIC") && cost_conv) {
    stop("Option 'cost_conv = TRUE' is not available with dPC or dIIC")
  }
  
  level <- ifelse(metric %in% list_glob_metrics, "graph", "patch")
  
  gr <- get_graphab(res = FALSE, return = TRUE)
  java.path <- Sys.which("java")
  version <- "graphab-2.8.jar"
  path_to_graphab <- file.path(rappdirs::user_data_dir(), "graph4lg_jar", version)
  
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab, 
           "--project", proj_end_path, "--usegraph", graph)
  
  # default gmetric/lmetric
  if (level == "graph") cmd <- c(cmd, "--gmetric", metric)
  if (level == "patch") cmd <- c(cmd, "--lmetric", metric)
  
  # add parameters if needed
  if (metric %in% list_dist_metrics) {
    cmd <- c(cmd, paste0("d=", dist), paste0("p=", prob), paste0("beta=", beta))
  }
  if (metric == "CF") {
    cmd <- c(cmd, paste0("beta=", beta))
  }
  
  # --- handle dPC separately ---
  if (metric == "dPC") {
    cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab,
             "--project", proj_end_path, "--usegraph", graph,
             "--delta", "dPC", paste0("d=", dist), paste0("p=", prob),
             paste0("beta=", beta), paste0("obj=", obj))
  }
  
  # --- handle dIIC separately ---
  if (metric == "dIIC") {
    cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab,
             "--project", proj_end_path, "--usegraph", graph,
             "--delta", "IIC", paste0("obj=", obj))
  }
  
  if (!is.null(alloc_ram)) {
    cmd <- c(paste0("-Xmx", alloc_ram, "g"), cmd)
  }
  
  rs <- system2(java.path, args = cmd, stdout = TRUE)
  
  if (return_val) {
    if (metric == "dPC") {
      name_txt <- paste0("delta-dPC_d", dist, "_p", prob, "_", graph, ".txt")
      full_file_path <- file.path(proj_path, proj_name, name_txt)
      if (!file.exists(full_file_path)) stop(paste("Missing:", full_file_path))
      new_name_txt <- paste0("delta-dPC_d", dist, "_p", prob, "_", graph, "_", obj, ".txt")
      new_path <- file.path(proj_path, proj_name, new_name_txt)
      file.rename(full_file_path, new_path)
      res_table <- utils::read.table(new_path, header = TRUE)[-1, ]
      res <- list(
        c(paste0("Project : ", proj_name),
          paste0("Graph : ", graph),
          paste0("Metric : ", metric),
          paste0("Dist : ", dist),
          paste0("Prob : ", prob),
          paste0("Beta : ", beta),
          paste0("Object : ", obj)),
        res_table
      )
    } else if (metric == "dIIC") {
      # Graphab outputs "delta-IIC" (no "d")
      name_txt <- paste0("delta-IIC_", graph, ".txt")
      full_file_path <- file.path(proj_path, proj_name, name_txt)
      if (!file.exists(full_file_path)) stop(paste("Missing:", full_file_path))
      
      # Rename to delta-dIIC to keep consistent with dPC
      new_name_txt <- paste0("delta-dIIC_", graph, "_", obj, ".txt")
      new_path <- file.path(proj_path, proj_name, new_name_txt)
      file.rename(full_file_path, new_path)
      
      res_table <- utils::read.table(new_path, header = TRUE)[-1, ]
      res <- list(
        c(paste0("Project : ", proj_name),
          paste0("Graph : ", graph),
          paste0("Metric : ", metric),
          paste0("Object : ", obj)),
        res_table
      )
    } else if (metric %in% c("PC", "EC", "IIC", "ECh", "NC", "MSC", "SLC")) {
      name_txt <- paste0(metric, ".txt")
      res_val <- utils::read.table(file.path(proj_path, proj_name, name_txt), header = TRUE)
      res <- list(
        c(paste0("Project : ", proj_name),
          paste0("Graph : ", graph),
          paste0("Metric : ", metric),
          paste0("Dist : ", dist),
          paste0("Prob : ", prob),
          paste0("Beta : ", beta)),
        res_val
      )
    }
    return(res)
  }
}

## NEW GRAPHAB_LINK() ####
graphab_link <- function (
    proj_name,
    distance = "cost",
    name,
    cost = NULL,
    topo = "planar",
    remcrosspath = FALSE,
    proj_path = NULL,
    alloc_ram = NULL,
    maxcost = NULL
) {
  
  if (!is.null(proj_path)) {
    if (!dir.exists(proj_path)) {
      stop(paste0(proj_path, " is not an existing directory or the path is incorrectly specified."))
    } else {
      proj_path <- normalizePath(proj_path)
    }
  } else {
    proj_path <- normalizePath(getwd())
  }
  
  if (!inherits(proj_name, "character")) {
    stop("'proj_name' must be a character string")
  } else if (!(paste0(proj_name, ".xml") %in% list.files(path = paste0(proj_path, "/", proj_name)))) {
    stop("The project you refer to does not exist.\nPlease use graphab_project() before.")
  }
  
  proj_end_path <- paste0(proj_path, "/", proj_name, "/", proj_name, ".xml")
  
  if (!inherits(distance, "character")) {
    stop("'distance' must be a character string")
  } else if (!(distance %in% c("cost", "euclid"))) {
    stop("'distance' must be equal to 'cost' or 'euclid'")
  }
  
  if (!inherits(remcrosspath, "logical")) {
    stop("'remcrosspath' must be a logical.")
  }
  
  ## --- validate maxcost ---
  if (!is.null(maxcost)) {
    if (!inherits(maxcost, c("numeric", "integer")) || length(maxcost) != 1) {
      stop("'maxcost' must be a single numeric value")
    }
  }
  
  if (distance == "cost") {
    
    if (inherits(cost, "data.frame")) {
      
      if (!all(c("code", "cost") %in% colnames(cost))) {
        stop("The columns of cost must include 'code' and 'cost'")
      } else if (any(is.na(as.numeric(cost$code)))) {
        stop("'code' column must include numeric values")
      } else if (any(is.na(as.numeric(cost$cost)))) {
        stop("'cost' column must include numeric values")
      }
      
      if (inherits(cost$code, c("factor", "character"))) {
        cost$code <- as.numeric(as.character(cost$code))
      }
      if (inherits(cost$cost, c("factor", "character"))) {
        cost$cost <- as.numeric(as.character(cost$cost))
      }
      
      rast_codes <- graph4lg::get_graphab_raster_codes(
        proj_name = proj_name,
        mode = "all",
        proj_path = proj_path
      )
      
      if (!all(rast_codes %in% cost$code)) {
        stop("'code' column must include all the raster code values.")
      }
      
      vec_cost <- paste0(cost$code, "=", cost$cost)
      
    } else if (inherits(cost, "character")) {
      
      if (stringr::str_sub(cost, start = -4L) == ".tif") {
        extcost <- cost
        if (!file.exists(normalizePath(extcost, mustWork = FALSE))) {
          stop(paste0(extcost, " must be an existing cost surface raster file ('.tif')"))
        }
      } else {
        stop("'cost' must be a data.frame or a cost surface raster file ('.tif')")
      }
      
    } else {
      stop("'cost' must be a data.frame or a cost surface raster file ('.tif')")
    }
  } else if (!is.null(cost)) {
    message("'cost' argument is ignored with 'distance = euclid'")
  }
  
  if (!inherits(name, "character")) {
    stop("'name' must be a character string")
  }
  
  gr <- get_graphab(res = FALSE, return = TRUE)
  if (gr == 1) {
    message("Graphab has been downloaded")
  }
  
  java.path <- Sys.which("java")
  version <- "graphab-2.8.jar"
  path_to_graphab <- paste0(rappdirs::user_data_dir(), "/graph4lg_jar/", version)
  
  cmd <- c(
    "-Djava.awt.headless=true",
    "-jar", path_to_graphab,
    "--project", proj_end_path,
    "--linkset",
    paste0("distance=", distance),
    paste0("name=", name)
  )
  
  if (topo == "complete") cmd <- c(cmd, "complete")
  if (remcrosspath) cmd <- c(cmd, "remcrosspath")
  
  if (distance == "cost") {
    if (inherits(cost, "data.frame")) {
      cmd <- c(cmd, vec_cost)
    } else {
      cmd <- c(cmd, paste0("extcost=", extcost))
    }
  }
  
  ## --- add maxcost only if supplied ---
  if (!is.null(maxcost)) {
    cmd <- c(cmd, paste0("maxcost=", maxcost))
  }
  
  if (!is.null(alloc_ram)) {
    if (inherits(alloc_ram, c("integer", "numeric"))) {
      cmd <- c(paste0("-Xmx", alloc_ram, "g"), cmd)
    } else {
      stop("'alloc_ram' must be a numeric or an integer")
    }
  }
  
  rs <- system2(java.path, args = cmd, stdout = TRUE)
  
  if (file.exists(paste0(proj_path, "/", proj_name, "/", name, "-links.shp"))) {
    message(paste0("Link set '", name, "' has been created in the project ", proj_name))
  } else {
    message("The link set creation did not succeed.")
  }
}

# PARAMETER SPACE CONNECTIVITY ####
results <- list()
total_start <- Sys.time()
graphab_jar <- file.path(getwd(), "graphab-2.8.8.jar")

ram = 40 # max allocated ram change as needed

proj_dir <- normalizePath(getwd())
#ctl <- read_csv("params.csv")
ctl <- read_excel("ctl.xlsx", sheet = 1) #or this depending on which input is used

## INITIAL RUN ####
for (i in seq_len(nrow(ctl))) {
  proj_dir <- normalizePath(getwd())
  
  runname <- ctl$runname[i]
  hab_file <- ctl$hab_file[i]
  res_file <- ctl$res_file[i]
  
  minarea  <- 1 # change if initial run wants to be done w/ different min habitat size
  
  ## ---- Sanity checks ----
  if (!file.exists(graphab_jar)) {
    warning(paste("⚠️ Skipping run", i, "- Graphab jar not found:", graphab_jar))
    next
  }
  if (!file.exists(hab_file)) {
    warning(paste("⚠️ Skipping run", i, "- Habitat file not found:", hab_file))
    next
  }
  if (!file.exists(res_file)) {
    warning(paste("⚠️ Skipping run", i, "- Resistance file not found:", res_file))
    next
  }
  
  cat("\n============================\n")
  cat("▶ Running initial run","\n")
  cat("============================\n")
  
  # Extract habitat code and no-data code from the filename
  hab_code <- sub(".*habcode(\\d+)_.*", "\\1", basename(hab_file))
  nodata_code <- sub(".*nodata(\\d+).*", "\\1", basename(hab_file))   # e.g., "nodata9999" -> 9999
  
  proj_name  <- paste0(runname, "_ma", minarea)
  
  cat("\n--- Creating project:", proj_name, "---\n")
  t_start <- Sys.time()
  
  ## ---- Create project ----
  graphab_project(
    proj_name = proj_name,
    proj_path = proj_dir,
    raster    = hab_file,
    habitat   = as.numeric(hab_code),
    minarea   = minarea,
    nodata    = as.numeric(nodata_code),
    con8      = TRUE,
    alloc_ram = ram
  )
  cat("Project created in", round(difftime(Sys.time(), t_start, units = "secs"), 2), "sec\n")
  
  ### ---- Check CRS/res/extent ----
  hab_rast <- terra::rast(hab_file)
  res_rast <- terra::rast(res_file)
  res_match <- all(terra::res(hab_rast) == terra::res(res_rast))
  crs_match <- terra::compareGeom(hab_rast, terra::rast(res_rast), crs = TRUE)
  ext_match <- terra::ext(hab_rast) == terra::ext(res_rast)
  
  if (!res_match || !crs_match || !ext_match) {
    warning(paste("⚠️ Skipping run", i, "- mismatch in",
                  if (!res_match) "resolution" else "",
                  if (!crs_match) "CRS" else "",
                  if (!ext_match) "extent"))
    next
  }
  
  res_name <- tools::file_path_sans_ext(basename(res_file))  # Get resistance layer name (without extension)
  
  cat("\n--- Minarea:", minarea, "| Resistance:", res_name, "---\n")
  linkset_name <- paste0("MAX_ma", minarea)
  
  # Snapshot existing links.shp before running Graphab (full path)
  proj_root <- normalizePath(file.path(proj_dir, proj_name))
  before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
  
  ### Pathways ####
  link_err <- NULL
  t_link_start <- Sys.time()
  tryCatch({
    graphab_link(
      proj_name    = proj_name,
      proj_path    = proj_dir,
      distance     = "cost",
      cost         = res_file,
      name         = linkset_name,
      remcrosspath = FALSE,
      alloc_ram = ram
    )
  }, error = function(e) {
    link_err <<- e
  })
  
  # After: look for new/expected links.shp
  after_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
  new_links <- setdiff(after_links, before_links)
  
  # Also accept the case where graphab overwrote or used the requested linkset_name
  has_link_named <- any(grepl(linkset_name, after_links, fixed = TRUE))
  
  # Debug output (optional, remove if noisy)
  cat("  before links:", length(before_links), " after links:", length(after_links),
      " new:", length(new_links), " named:", has_link_named, "\n")
  
  # Decide success/failure
  if (!is.null(link_err) || (length(new_links) == 0 && !has_link_named)) {
    cat("⚠️ Linkset creation failed for", res_file, "\n",
        "   Possibly no links were created (resistance too large or threshold too small) or Graphab returned an error.\n")
    # Mark failure so we skip the rest of this resistance raster
    linkset_failed <- TRUE
    break   # exit thr loop; we'll test linkset_failed below and 'next' the res_file
  }
  
  cat("Linkset created in", round(difftime(Sys.time(), t_link_start, units = "secs"), 2), "sec for", linkset_name, "\n")
  
  ### Longest pathway ####
  if (length(new_links) > 0) {
    link_file <- new_links[1]
  } else if (has_link_named) {
    link_file <- list.files(proj_root, pattern = paste0(linkset_name, ".*links\\.shp$"), 
                            recursive = TRUE, full.names = TRUE)[1]
  } else {
    link_file <- NA
  }
  
  if (!is.na(link_file) && file.exists(link_file)) {
    links_v <- terra::vect(link_file)
    thr <- round(max(perim(links_v), na.rm = TRUE), 2) + 5 # add 5 just to make sure we don't miss anything
    cat("✅ Longest pathway in", linkset_name, ":", thr, "m\n")
  } else {
    cat("⚠️ Could not locate link shapefile for", linkset_name, "\n")
  }
  
  ### Graph creation ####
  graph_name <- paste0("ma", minarea, "_t", thr)
  
  t_graph_start <- Sys.time()
  graphab_graph(
    proj_name = proj_name,
    proj_path = proj_dir,
    linkset   = linkset_name,
    thr       = thr,
    name      = graph_name,
    alloc_ram = ram
  )
  cat("Graph created in", round(difftime(Sys.time(), t_graph_start, units = "secs"), 2), "sec\n")
  
  ### IIC, NC, MSC metric ####
  t_metric_start <- Sys.time()
  metric_out <- graphab_metric(
    proj_name = proj_name,
    proj_path = proj_dir,
    graph     = graph_name,
    metric    = "IIC"
  )
  iic_value <- metric_out[[2]]$IIC
  cat("IIC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
  
  metric_out <- graphab_metric(
    proj_name = proj_name,
    proj_path = proj_dir,
    graph     = graph_name,
    metric    = "PC",
    dist = thr,
    prob = 0.05
  )
  pc_value <- metric_out[[2]]$PC
  cat("PC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
  
  metric_out <- graphab_metric(
    proj_name = proj_name,
    proj_path = proj_dir,
    graph     = graph_name,
    metric    = "NC"
  )
  nc_value <- metric_out[[2]]$NC
  cat("NC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
  
  metric_out <- graphab_metric(
    proj_name = proj_name,
    proj_path = proj_dir,
    graph     = graph_name,
    metric    = "MSC"
  )
  msc_value <- metric_out[[2]]$MSC
  cat("MSC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
  
  metric_out <- graphab_metric(
    proj_name = proj_name,
    proj_path = proj_dir,
    graph     = graph_name,
    metric    = "SLC"
  )
  slc_value <- metric_out[[2]]$SLC
  cat("SLC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
  
  ### When finished: ####
  results[[length(results) + 1]] <- data.frame(
    raster_file = runname,
    cost_file = res_name,
    minarea = minarea,
    thr = thr,
    IIC = iic_value,
    PC = pc_value,
    NC = nc_value,
    MSC = msc_value,
    SLC = slc_value,
    runtime_sec = round(difftime(Sys.time(), t_metric_start, units = "secs"), 2))
  
  results_df <- dplyr::bind_rows(results)
  
  ## GENERATE BOUNDARIES ####
  # max linkset value = thr
  # max component size = slc_value, with mean component size = msc_value
  #dist_seq <- seq(1, thr, length.out = 5) # sequence of numbers between 1 to max linkset
  
  dist_seq <- round(seq(quantile(links_v$DistM)[1], max(links_v$DistM), length.out = 5), 2)
  
  ## Distribution of pathway lengths plot if needed ####
  hist(log10(links_v$DistM),
       breaks = 30,
       main = "Distribution of pathway lengths",
       xlab = expression(log[10]~Length~"(ha)"))
  abline(v = log10(dist_seq), col = "red", lty = 2, lwd = 2)

  plot(seq_along(dist_seq),
       dist_seq,
       pch = 16,
       xaxt = "n",
       xlab = "",
       ylab = "Interpatch distance (m)")
  abline(h = thr, col = "red", lty = 2, lwd = 2)
  text(x = par("usr")[2] * 0.95,
       y = thr * 0.95,
       labels = paste0("Longest pathway"),
       pos = 2,
       col = "red")
  
  ### Distribution of patch sizes plot if needed####
  patch_v <- vect(paste0(proj_name, "/patches.shp"))
  area_seq <- round(seq(min(patch_v$Area), tail(sort(patch_v$Area))[5], length.out = 5)/10000, 2)
  
  hist(log10(patch_v$Area/10000),
       breaks = 30,
       main = "Distribution of patch sizes",
       xlab = expression(log[10]~Area~"(ha)"))
  abline(v = log10(area_seq), col = "red", lty = 2, lwd = 2)

  plot(seq_along(area_seq),
       area_seq,
       pch = 16,
       xaxt = "n",
       xlab = "",
       ylab = "Minimum habitat size (ha)")
  abline(h = (tail(sort(patch_v$Area))[5]/10000), col = "red", lty = 2, lwd = 2)
  text(x = par("usr")[2] * 0.95,
       y = (tail(sort(patch_v$Area))[5]/10000) * 0.95,
       labels = paste0("Second largest patch size"),
       pos = 2,
       col = "red")
  
  #different ways to select values
  #dist_seq <- round(exp(seq(log(1), log(thr), length.out = 10)), 2)
  #dist_seq <- round(quantile(links_v$DistM, probs = seq(0.1, 1, length.out = 10), na.rm = TRUE), 2)
  #area_seq <- quantile(patch_v$Area, probs = seq(0.1, 1, length.out = 9), na.rm = TRUE)/10000
  #area_seq <- exp(seq(log(lower), log(upper), length.out = 9))/10000
  #area_seq <- round(sort(unique(c(area_seq, 1))),2)
  #area_seq <- round(seq(min(patch_v$Area), sort(unique(patch_v$Area))[5], length.out = 10) / 10000, 2)
  
  ### Final combinations ####
  params <- expand.grid(thr = dist_seq, minarea = area_seq)
  selected <- expand.grid(thr = c(min(dist_seq), max(dist_seq)), 
                          minarea = c(min(area_seq), max(area_seq)))
  
  ## LOOPING COMBINATIONS ####
  for (i in seq_along(area_seq)) {
    minarea  <- area_seq[i]
    
    cat("\n============================\n")
    cat("▶ Run", i, "of", length(area_seq), "\n")
    cat("============================\n")
    
    proj_name  <- paste0(runname, "_ma", minarea)
    
    cat("\n--- Creating project:", proj_name, "---\n")
    t_start <- Sys.time()
    
    ### ---- Create project ----
    graphab_project(
      proj_name = proj_name,
      proj_path = proj_dir,
      raster    = hab_file,
      habitat   = as.numeric(hab_code),
      minarea   = minarea,
      nodata    = as.numeric(nodata_code),
      con8      = TRUE,
      alloc_ram = ram
    )
    cat("Project created in", round(difftime(Sys.time(), t_start, units = "secs"), 2), "sec\n")
    
    #### ---- Check CRS/res/extent ----
    hab_rast <- terra::rast(hab_file)
    res_rast <- terra::rast(res_file)
    res_match <- all(terra::res(hab_rast) == terra::res(res_rast))
    crs_match <- terra::compareGeom(hab_rast, terra::rast(res_rast), crs = TRUE)
    ext_match <- terra::ext(hab_rast) == terra::ext(res_rast)
    
    if (!res_match || !crs_match || !ext_match) {
      warning(paste("⚠️ Skipping run", i, "- mismatch in",
                    if (!res_match) "resolution" else "",
                    if (!crs_match) "CRS" else "",
                    if (!ext_match) "extent"))
      next
    }
    
    res_name <- tools::file_path_sans_ext(basename(res_file))  # Get resistance layer name (without extension)
    
    cat("\n--- Minarea:", minarea, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0("ma", minarea)
    
    # Snapshot existing links.shp before running Graphab (full path)
    proj_root <- file.path(proj_dir, proj_name)
    before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    
    # Run graphab_link and capture R errors (if any)
    link_err <- NULL
    t_link_start <- Sys.time()
    tryCatch({
      graphab_link(
        proj_name = proj_name,
        proj_path = proj_dir,
        distance = "cost",
        cost = res_file,
        name = linkset_name,
        remcrosspath = FALSE,
        #topo = "complete",
        alloc_ram   = ram
      )
    }, error = function(e) {
      link_err <<- e
    })
    
    # After: look for new/expected links.shp
    after_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    new_links <- setdiff(after_links, before_links)
    
    # Also accept the case where graphab overwrote or used the requested linkset_name
    has_link_named <- any(grepl(linkset_name, after_links, fixed = TRUE))
    
    # Debug output (optional, remove if noisy)
    cat("  before links:", length(before_links), " after links:", length(after_links),
        " new:", length(new_links), " named:", has_link_named, "\n")
    
    # Decide success/failure
    if (!is.null(link_err) || (length(new_links) == 0 && !has_link_named)) {
      cat("⚠️ Linkset creation failed for", res_file, "\n",
          "   Possibly no links were created (resistance too large or threshold too small) or Graphab returned an error.\n")
      # Mark failure so we skip the rest of this resistance raster
      linkset_failed <- TRUE
      next   
    }
    
    cat("Linkset created in", round(difftime(Sys.time(), t_link_start, units = "secs"), 2), "sec for", linkset_name, "\n")
    
    ### Looping across different interpatch distances ####
    for(i in seq_along(dist_seq)){
      thr <- dist_seq[i]
      
      # keep links only <= thr
      if (length(new_links) > 0) {
        link_file <- new_links[1]
      } else {
        link_file <- list.files(proj_root, pattern = paste0(linkset_name, ".*links\\.shp$"),
                                recursive = TRUE, full.names = TRUE)[1]
      }
      
      if (!is.na(link_file) && file.exists(link_file)) {
        cat("Filtering links in:", link_file, "\n")
        
        v <- terra::vect(link_file)
        
        # Filter by threshold
        v_filt <- v[v$Dist <= thr, ]
        
        cat("  Kept", nrow(v_filt), "of", nrow(v), "links (<= ", thr, ")\n")
        
        # Skip this threshold if no links remain
        if (nrow(v_filt) == 0) {
          cat("  No links remain. Skipping threshold:", thr, "\n")
          next
        }
        
        # Save filtered shapefile
        out_file <- sub("\\.shp$", paste0("_thr", thr, ".shp"), link_file)
        terra::writeVector(v_filt, out_file, overwrite = TRUE)
        cat("  Saved filtered links to:", out_file, "\n")
      } else {
        cat("⚠️ Could not locate link shapefile for filtering.\n")
      }
      
      # Graph creation
      graph_name <- paste0("ma", minarea, "_t", thr)
      
      t_graph_start <- Sys.time()
      graphab_graph(
        proj_name = proj_name,
        proj_path = proj_dir,
        linkset   = linkset_name,
        thr       = thr,
        name      = graph_name,
        alloc_ram = ram
      )
      cat("Graph created in", round(difftime(Sys.time(), t_graph_start, units = "secs"), 2), "sec\n")
      
      # IIC, PC, NC, MSC metric
      t_metric_start <- Sys.time()
      metric_out <- graphab_metric(
        proj_name = proj_name,
        proj_path = proj_dir,
        graph     = graph_name,
        metric    = "IIC"
      )
      iic_value <- metric_out[[2]]$IIC
      cat("IIC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
      
      metric_out <- graphab_metric(
        proj_name = proj_name,
        proj_path = proj_dir,
        graph     = graph_name,
        metric    = "PC",
        dist = thr,
        prob = 0.05
      )
      pc_value <- metric_out[[2]]$PC
      cat("PC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
      
      metric_out <- graphab_metric(
        proj_name = proj_name,
        proj_path = proj_dir,
        graph     = graph_name,
        metric    = "NC"
      )
      nc_value <- metric_out[[2]]$NC
      cat("NC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
      
      metric_out <- graphab_metric(
        proj_name = proj_name,
        proj_path = proj_dir,
        graph     = graph_name,
        metric    = "MSC"
      )
      msc_value <- metric_out[[2]]$MSC
      cat("MSC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
      
      metric_out <- graphab_metric(
        proj_name = proj_name,
        proj_path = proj_dir,
        graph     = graph_name,
        metric    = "SLC"
      )
      slc_value <- metric_out[[2]]$SLC
      cat("SLC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
      
      # When finished:
      results[[length(results) + 1]] <- data.frame(
        raster_file = runname,
        cost_file = res_name,
        minarea = minarea,
        thr = thr,
        IIC = iic_value,
        PC = pc_value,
        NC = nc_value,
        MSC = msc_value,
        SLC = slc_value,
        runtime_sec = round(difftime(Sys.time(), t_metric_start, units = "secs"), 2))
      
      results_df <- dplyr::bind_rows(results)
      write.csv(results_df, "graphab_results_summary.csv", row.names = FALSE)
    }
    if (minarea %in% selected$minarea) {
      cat("Keeping project folder for minarea =", minarea, "\n")
    } else {
      unlink(proj_name, recursive = TRUE)
      cat("Deleted project folder for minarea =", minarea, "\n")
    }
  }  
}

write.csv(results_df, "graphab_results_summary.csv", row.names = FALSE)
total_end <- Sys.time()
cat("\n=== Total runtime:",
    round(difftime(total_end, total_start, units = "mins"), 2),
    "minutes ===\n")  

## PLOTTING ####
ggplot(results_df,
       aes(x = thr, y = minarea, color = IIC)) +
  geom_point(size = 4) +
  facet_wrap(~ cost_file, nrow = 1) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c(name = "IIC") +
  labs(x = "Interpatch distance (m)", y = "Minimum habitat patch size (ha)") +
  theme_bw() +
  theme(legend.position = "right")
