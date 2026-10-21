#### LIBRARIES ####
library(graph4lg)
library(tidyverse)
library(terra)
library(igraph)
library(viridis)
library(sf)
library(raster)
library(readr)
library(readxl)

ram = 40 #allocated ram change as needed

#### NEW GRAPHAB_METRIC() with NC, MNC, SLC, deltaIIC capability and deltadPC renewed ####
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

#### NEW GRAPHAB_METAPATCH() FUNCTION ####
graphab_metapatch <- function(proj_name, graph_name, mincapa = 10, proj_dir = getwd()) {
  cat("Running Graphab metapatch for graph:", graph_name, "\n\n")
  
  java.path <- Sys.which("java")
  version <- "graphab-2.8.jar"
  path_to_graphab <- file.path(rappdirs::user_data_dir(), "graph4lg_jar", version)
  
  xml_path <- file.path(proj_dir, proj_name, paste0(proj_name, ".xml"))
  meta_folder <- file.path(proj_dir, proj_name, paste0(proj_name, "-", graph_name))
  
  cmd <- c("-Djava.awt.headless=true", "-jar", path_to_graphab,
           "--project", xml_path,
           "--usegraph", graph_name,
           "--metapatch", paste0("mincapa=", mincapa))
  
  rs <- system2(java.path, args = cmd, stdout = TRUE, stderr = TRUE)
  cat(rs, sep = "\n")
  
  if (dir.exists(meta_folder)) {
    cat("✅ Metapatch project successfully created at:\n   ", meta_folder, "\n")
  } else {
    cat("❌ Metapatch project creation did not succeed or folder not found.\n")
  }
}


#### NEW GRAPHAB_CORRIDOR() TO DEAL WITH CHANGING DIRECTORIES FOR METAPATCH ####
graphab_corridor <- function(proj_name, graph, maxcost, format = "raster", 
                             cost_conv = FALSE, proj_path = NULL, alloc_ram = NULL) {
  
  # Set project path
  if (is.null(proj_path)) proj_path <- getwd()
  if (!dir.exists(proj_path)) stop(proj_path, " does not exist.")
  
  proj_path <- normalizePath(proj_path, winslash = "/")
  
  # Full path to XML
  proj_xml <- normalizePath(file.path(proj_path, proj_name, paste0(proj_name, ".xml")),
                            winslash = "/", mustWork = TRUE)
  
  # Check graph
  graph_file <- file.path(proj_path, proj_name, paste0(graph, "-voronoi.shp"))
  if (!file.exists(graph_file)) stop("Graph file does not exist: ", graph_file)
  
  if (!is.numeric(maxcost)) stop("'maxcost' must be numeric")
  if (!format %in% c("raster", "vector")) stop("'format' must be 'raster' or 'vector'")
  
  java.path <- Sys.which("java")
  path_to_graphab <- normalizePath(file.path(rappdirs::user_data_dir(), "graph4lg_jar", "graphab-2.8.jar"),
                                   winslash = "/")
  
  # Construct command
  cmd <- c("-Djava.awt.headless=true", "-jar", shQuote(path_to_graphab),
           "--project", shQuote(proj_xml),
           "--usegraph", shQuote(graph),
           "--corridor", paste0("maxcost=", maxcost),
           paste0("format=", format))
  
  # Optional RAM
  if (!is.null(alloc_ram)) {
    if (!is.numeric(alloc_ram)) stop("'alloc_ram' must be numeric")
    cmd <- c(paste0("-Xmx", alloc_ram, "g"), cmd)
  }
  
  # Run
  res <- system2(java.path, args = cmd, stdout = TRUE, stderr = TRUE)
  message(paste(res, collapse = "\n"))
  
  message(paste0("A ", format, " file with corridors has been created."))
}

#### NEW GRAPHAB_LINK() ####
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
#### GRAPHAB PROJECT ANALYSIS ####
params <- read_csv("params.csv")

results <- list()
total_start <- Sys.time()
graphab_jar <- file.path(getwd(), "graphab-2.8.8.jar")

for (i in seq_len(nrow(params))) {
  proj_dir <- normalizePath(getwd())
  
  hab_file <- params$hab_file[i]
  res_file <- params$res_file[i]
  minarea  <- params$minarea[i]
  thr      <- params$thr[i]
  runname <- params$runname[i]
  metapatch <- params$metapatch[i]
  metadist <- params$metadist[i]
  
  # ---- Sanity checks ----
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
  cat("▶ Run", i, "of", nrow(params), "\n")
  cat("============================\n")
  
  # Extract habitat code and no-data code from the filename
  hab_code <- sub(".*habcode(\\d+)_.*", "\\1", basename(hab_file))
  nodata_code <- sub(".*nodata(\\d+).*", "\\1", basename(hab_file))   # e.g., "nodata9999" -> 9999
  
  if (is.na(metapatch) || is.na(metadist)) {
    cat("\n--- Running ORIGINAL Graphab code (no metapatch/metadist) ---\n")
    
    proj_name  <- paste0(tools::file_path_sans_ext(basename(hab_file)), "_ma", minarea)
    
    cat("\n--- Creating project:", proj_name, "---\n")
    t_start <- Sys.time()
    
    # ---- Create project ----
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
    
    # ---- Check CRS/res/extent ----
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
    
    cat("\n--- Minarea:", minarea, "Thr:", thr, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0("ma", minarea, "_t", thr)
    
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
        maxcost = thr,
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
      break   # exit thr loop; we'll test linkset_failed below and 'next' the res_file
    }
    
    cat("Linkset created in", round(difftime(Sys.time(), t_link_start, units = "secs"), 2), "sec for", linkset_name, "\n")
    
    # # keep links only <= thr
    # if (length(new_links) > 0) {
    #   link_file <- new_links[1]
    # } else {
    #   link_file <- list.files(proj_root, pattern = paste0(linkset_name, ".*links\\.shp$"),
    #                           recursive = TRUE, full.names = TRUE)[1]
    # }
    # 
    # if (!is.na(link_file) && file.exists(link_file)) {
    #   cat("Filtering links in:", link_file, "\n")
    #   
    #   v <- terra::vect(link_file)
    #   
    #   # Filter by threshold
    #   v_filt <- v[v$Dist <= thr, ]
    #   
    #   cat("  Kept", nrow(v_filt), "of", nrow(v), "links (<= ", thr, ")\n")
    #   
    #   # Save filtered shapefile
    #   out_file <- sub("\\.shp$", paste0("_lt", thr, ".shp"), link_file)
    #   terra::writeVector(v_filt, out_file, overwrite = TRUE)
    #   cat("  Saved filtered links to:", out_file, "\n")
    # } else {
    #   cat("⚠️ Could not locate link shapefile for filtering.\n")
    # }
    
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
    
    # Corridor creatoin
    graphab_corridor(
      proj_name = proj_name,
      graph = graph_name,
      maxcost = thr,
      format = "raster",
      proj_path = proj_dir,
      alloc_ram = ram
    )
    
    # cleaning patches for futher processing
    patch_file <- file.path(proj_dir, proj_name, "patches.tif")
    r_clean <- classify(rast(patch_file), rbind(c(-Inf, 0, NA)))
    r_clean <- classify(r_clean, cbind(-Inf, Inf, 1))
    r_clean <- resample(r_clean, res_rast)
    
    out_dir <- file.path(proj_dir, "habomni")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, paste0(graph_name, ".tif"))
    
    writeRaster(r_clean, out_file, overwrite = TRUE)
    
    cat("Clean patches written to:", out_file, "\n")
    
    corr_file_base <- file.path(proj_dir, proj_name, paste0(graph_name, "-corridor-", thr))
    corr_file1 <- paste0(corr_file_base, ".tif")
    corr_file2 <- paste0(corr_file_base, ".0.tif")
    
    if (file.exists(corr_file1)) {
      corr_file <- corr_file1
    } else if (file.exists(corr_file2)) {
      corr_file <- corr_file2
    } else {
      stop("Corridor file not found: neither .tif nor .0.tif exists.")
    }
    
    corr_clean <- classify(rast(corr_file), rbind(c(-Inf, 0, NA)))
    corr_clean <- resample(corr_clean, r_clean, method = "near")
    
    combined <- cover(r_clean, corr_clean)
    combined <- resample(combined, res_rast)
    res_new <- mask(res_rast, combined)
    
    out_dir <- file.path(proj_dir, "resomni")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, paste0(graph_name, "-corridor-", sprintf("%.0f", thr), ".tif"))
    #out_file <- file.path(out_dir, paste0(graph_name, "-corridor-", t, ".0.tif"))
    
    writeRaster(res_new, out_file, overwrite = TRUE)
    
    cat("Corridor resistance written to:", out_file, "\n")
    
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
    
    
    # # Delta PC for nodes
    # #deltaPC creates separate .txt files
    # t_dPCnode_start <- Sys.time()
    # #create dPC for nodes
    # graphab_metric(
    #   proj_name = proj_name,
    #   proj_path = proj_dir,
    #   graph     = graph_name,
    #   metric = "dPC",
    #   dist = thr,
    #   prob = 0.05,
    #   obj = "patch"
    # )
    # cat("dPC nodes calculated in", round(difftime(Sys.time(), t_dPCnode_start, units = "secs"), 2), "sec\n")
    # 
    # #create dPC edges
    # t_dPCedges_start <- Sys.time()
    # graphab_metric(
    #   proj_name = proj_name,
    #   proj_path = proj_dir,
    #   graph     = graph_name,
    #   metric = "dPC",
    #   dist = thr,
    #   prob = 0.05,
    #   obj = "links"
    # )
    # cat("dPC edges calculated in", round(difftime(Sys.time(), t_dPCedges_start, units = "secs"), 2), "sec\n")
    # 
    # Delta IIC for nodes
    #deltaIIC creates separate .txt files
    t_dIICnode_start <- Sys.time()
    #create dPC for nodes
    graphab_metric(
      proj_name = proj_name,
      proj_path = proj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "patch"
    )
    cat("dIIC nodes calculated in", round(difftime(Sys.time(), t_dIICnode_start, units = "secs"), 2), "sec\n")
    
    #create dIICC edges
    t_dIICedges_start <- Sys.time()
    graphab_metric(
      proj_name = proj_name,
      proj_path = proj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "links"
    )
    cat("dIIC edges calculated in", round(difftime(Sys.time(), t_dIICedges_start, units = "secs"), 2), "sec\n")
    
    # When finished:
    results[[length(results) + 1]] <- data.frame(
      raster_file = hab_file,
      cost_file = res_file,
      minarea = minarea,
      thr = thr,
      IIC = iic_value,
      PC = pc_value,
      NC = nc_value,
      MSC = msc_value,
      SLC = slc_value,
      runtime_sec = round(difftime(Sys.time(), t_metric_start, units = "secs"), 2))
    
    results_df <- dplyr::bind_rows(results)
    
    #### Merge dIIC with links/patches file ####
    proj_folders <- normalizePath(list.dirs(proj_dir, full.names = TRUE, recursive = FALSE))
    proj_folders <- proj_folders[grepl(paste0("^", runname, ".*_habcode[0-9]+.*$"), basename(proj_folders))]
    proj_folders <- proj_folders[basename(proj_folders) == proj_name]
    
    for (proj_folder in proj_folders) {
      message("Processing: ", proj_folder)
      
      # read shapefiles
      patches_shp <- vect(file.path(proj_folder, "patches.shp"))
      patches_shp <- patches_shp[patches_shp$Id > 0, ]
      pts <- centroids(patches_shp)
      patch.df <- as.data.frame(patches_shp)
      
      # create output dirs
      out_links_dir <- file.path(proj_dir, "dIIC_links")
      out_patch_dir <- file.path(proj_dir, "dIIC_patch")
      out_node_dir <- file.path(proj_dir, "dIIC_node")
      dir.create(out_links_dir, showWarnings = FALSE)
      dir.create(out_patch_dir, showWarnings = FALSE)
      dir.create(out_node_dir, showWarnings = FALSE)
      
      # locate links shapefiles
      links_shp_files <- list.files(proj_folder, pattern = "*-links.*\\.shp$", full.names = TRUE)
      
      # locate dIIC text files
      links_txt_files <- list.files(proj_folder, pattern = "^delta-dIIC.*links.*\\.txt$", full.names = TRUE)
      patch_txt_files <- list.files(proj_folder, pattern = "^delta-dIIC.*patch.*\\.txt$", full.names = TRUE)
      
      for (links in links_shp_files) {
        links_base <- sub("-links\\.shp$", "", basename(links))
        links_shp <- vect(links)
        links_df <- as.data.frame(links_shp)
        
        for (txt in links_txt_files) {
          txt_base <- basename(txt)
          
          # match on both minarea (ma) and threshold (thr)
          if (grepl(links_base, txt_base)) {
            message("Processing matching pair: ", links_base, " <-> ", txt_base)
            
            diic_edges <- read_tsv(txt, col_types = cols()) %>%
              filter(Id != "Init") %>%
              mutate(Id = as.character(Id),
                     d_IICCon = as.numeric(d_IIC))
            
            links_df <- links_df %>%
              mutate(Id = as.character(Id)) %>%
              left_join(diic_edges %>% dplyr::select(Id, d_IICCon), by = "Id")
            
            # ensure the column is actually attached
            out_links <- links_shp
            out_links$d_IICCon <- links_df$d_IICCon
            
            # verify before writing
            if (!"d_IICCon" %in% names(out_links)) {
              stop("Failed to attach d_IICCon to shapefile ", basename(links))
            }
            
            base_name <- tools::file_path_sans_ext(basename(txt))
            base_name <- gsub("[^A-Za-z0-9_]", "_", base_name)
            
            out_file <- file.path(out_links_dir, paste0(base_name, ".shp"))
            writeVector(out_links, out_file, overwrite = TRUE)
            message("✅ Saved links shapefile with d_IICCon: ", out_file)
          }
        }
      }
      
      # --- process patches ---
      for (txt in patch_txt_files) {
        diic_tbl <- read_delim(txt, delim = "\t", trim_ws = TRUE, show_col_types = FALSE) %>%
          filter(Id != "Init") %>%
          mutate(Id = as.integer(Id)) %>%
          dplyr::select(Id, d_IIC)
        
        out_attr <- left_join(patch.df, diic_tbl, by = "Id")
        
        out_patches <- patches_shp
        out_patches$dIIC <- out_attr$d_IIC
        
        out_file <- file.path(out_patch_dir,
                              paste0(tools::file_path_sans_ext(basename(txt)), ".shp"))
        out_file <- normalizePath(out_file, mustWork = FALSE)
        
        if (nrow(out_patches) > 0) {
          terra::writeVector(out_patches, out_file, overwrite = TRUE)
          message("Saved patches shapefile: ", out_file)
        } else {
          message("Skipped (no patches): ", txt)
        }
        
        pts$dIIC <- out_attr$d_IIC[match(pts$Id, out_attr$Id)]
        out_file <- file.path(out_node_dir,
                              paste0(tools::file_path_sans_ext(basename(txt)), ".shp"))
        out_file <- normalizePath(out_file, mustWork = FALSE)
        
        if (nrow(pts) > 0) {
          terra::writeVector(pts, out_file, overwrite = TRUE)
          message("Saved nodes shapefile: ", out_file)
        } else {
          message("Skipped (no nodes): ", txt)
        }
      }
    }
  }
  else {
    cat("\n--- Running METAPATCH Graphab code (metapatch + metadist provided) ---\n")
    
    proj_name <- paste0(tools::file_path_sans_ext(basename(hab_file)), "_mp", metapatch)
    cat("\n--- Creating project:", proj_name, "---\n")
    
    t_start <- Sys.time()
    graphab_project(
      proj_name = proj_name,
      proj_path = proj_dir,
      raster    = hab_file,  # Habitat file path
      habitat   = as.numeric(hab_code),  # Habitat code
      minarea   = metapatch,
      nodata    = as.numeric(nodata_code),  # No-data code
      con8      = TRUE,
      alloc_ram = ram
    )
    cat("Project created in", round(difftime(Sys.time(), t_start, units = "secs"), 2), "sec\n")
    
    # Loop through resistance layers and thresholds
    res_name <- tools::file_path_sans_ext(basename(res_file))  # Get resistance layer name (without extension)
    
    
    # Check resolution/CRS/extent match
    hab_rast <- terra::rast(hab_file)
    res_rast <- terra::rast(res_file)
    #res_rast <- terra::resample(res_rast, hab_rast, method = "near")
    res_match  <- all(terra::res(hab_rast) == terra::res(res_rast))
    #crs_match  <- terra::same.crs(hab_rast, res_rast)
    crs_match <- terra::compareGeom(hab_rast, terra::rast(res_rast), crs = TRUE)
    ext_match  <- terra::ext(hab_rast) == terra::ext(res_rast)
    
    if (!res_match || !crs_match || !ext_match) {
      cat("Skipping:", res_file, "- mismatch in",
          if (!res_match) "resolution" else "",
          if (!crs_match) "CRS" else "",
          if (!ext_match) "extent", "\n")
      next  # Skip to next res_file
    }
    
    
    linkset_failed <- FALSE
    
    cat("\n--- metapatch:", metapatch, "metadist:", metadist, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0("mp", metapatch, "_md", metadist)
    
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
        maxcost = metadist,
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
    
    graph_name <- paste0("mp", metapatch, "_md", metadist)
    
    graphab_graph(
      proj_name = proj_name,
      proj_path = proj_dir,
      linkset   = linkset_name,
      thr       = metadist,
      name      = graph_name,
      alloc_ram = ram
    )
    
    # running metapatch project
    mincapa <- minarea * 10000
    graphab_metapatch(proj_name, graph_name, mincapa)
    
    # Update global variables
    nproj_name <- paste0(proj_name, "-", graph_name)
    #nproj_dir  <- file.path(proj_dir, proj_name)
    nproj_dir <- normalizePath(file.path(proj_dir, proj_name), winslash = "/", mustWork = TRUE)
    # proj_place <- file.path(nproj_dir, nproj_name)
    #proj_name <- "metatest_metanew_habcode1_nodata255_minarea2-metatest_metanew_habcode1_nodata255_newresclean_minarea2_metadist89_graph"
    
    cat("\n--- Minarea:", minarea, "Thr:", thr, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0("mp", metapatch, "_md", metadist, "_ma", minarea, "_t", thr)
    
    # Snapshot existing links.shp before running Graphab (full path)
    proj_root <- file.path(proj_dir, proj_name, nproj_name)
    before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    
    # Run graphab_link and capture R errors (if any)
    link_err <- NULL
    t_link_start <- Sys.time()
    tryCatch({
      graphab_link(
        proj_name = nproj_name,
        proj_path = nproj_dir,
        distance = "cost",
        cost = res_file,
        name  = linkset_name,
        maxcost = thr,
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
    
    # # keep links only <= thr
    # if (length(new_links) > 0) {
    #   link_file <- new_links[1]
    # } else {
    #   link_file <- list.files(proj_root, pattern = paste0(linkset_name, ".*links\\.shp$"),
    #                           recursive = TRUE, full.names = TRUE)[1]
    # }
    # 
    # if (!is.na(link_file) && file.exists(link_file)) {
    #   cat("Filtering links in:", link_file, "\n")
    #   
    #   v <- terra::vect(link_file)
    #   
    #   # Filter by threshold
    #   v_filt <- v[v$Dist <= thr, ]
    #   
    #   cat("  Kept", nrow(v_filt), "of", nrow(v), "links (<= ", thr, ")\n")
    #   
    #   # Save filtered shapefile
    #   out_file <- sub("\\.shp$", paste0("_lt", thr, ".shp"), link_file)
    #   terra::writeVector(v_filt, out_file, overwrite = TRUE)
    #   cat("  Saved filtered links to:", out_file, "\n")
    # } else {
    #   cat("⚠️ Could not locate link shapefile for filtering.\n")
    # }
    # 
    # ---- Graph creation (only runs if linkset created) ----
    graph_name <- paste0("mp", metapatch, "_md", metadist, "_ma", minarea, "_t", thr)
    
    t_graph_start <- Sys.time()
    graphab_graph(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      linkset   = linkset_name,
      thr       = thr,
      name      = graph_name,
      alloc_ram = ram
    )
    cat("Graph created in", round(difftime(Sys.time(), t_graph_start, units = "secs"), 2), "sec\n")
    
    graphab_corridor(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph = graph_name,
      maxcost = thr,
      format = "raster",
      alloc_ram = ram
    )
    
    # cleaning patches for futher processing
    patch_file <- file.path(proj_root, "patches.tif")
    r_clean <- classify(rast(patch_file), rbind(c(-Inf, 0, NA)))
    r_clean <- classify(r_clean, cbind(-Inf, Inf, 1))
    r_clean <- resample(r_clean, res_rast)
    
    out_dir <- file.path(proj_dir, "habomni")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, paste0("mp", metapatch, "_md", metadist, "_ma", minarea, "_t", thr, ".tif"))
    writeRaster(r_clean, out_file, overwrite = TRUE)
    
    cat("Clean patches written to:", out_file, "\n")
    
    corr_file_base <- file.path(nproj_dir, nproj_name, paste0(graph_name, "-corridor-", thr))
    corr_file1 <- paste0(corr_file_base, ".tif")
    corr_file2 <- paste0(corr_file_base, ".0.tif")
    
    if (file.exists(corr_file1)) {
      corr_file <- corr_file1
    } else if (file.exists(corr_file2)) {
      corr_file <- corr_file2
    } else {
      stop("Corridor file not found: neither .tif nor .0.tif exists.")
    }
    
    corr_clean <- classify(rast(corr_file), rbind(c(-Inf, 0, NA)))
    corr_clean <- resample(corr_clean, r_clean, method = "near")
    
    combined <- cover(r_clean, corr_clean)
    combined <- resample(combined, res_rast)
    res_new <- mask(res_rast, combined)
    
    out_dir <- file.path(proj_dir, "resomni")
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    
    out_file <- file.path(out_dir, paste0("mp", metapatch, "_md", metadist, "_ma", minarea, "-corridor-", thr, ".tif"))
    writeRaster(res_new, out_file, overwrite = TRUE)
    
    cat("Corridor resistance written to:", out_file, "\n")
    
    # IIC, PC, NC, MSC metric
    t_metric_start <- Sys.time()
    metric_out <- graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric    = "IIC",
      alloc_ram = ram
    )
    iic_value <- metric_out[[2]]$IIC
    cat("IIC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
    
    metric_out <- graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric    = "PC",
      dist = thr,
      prob = 0.05,
      alloc_ram = ram
    )
    pc_value <- metric_out[[2]]$PC
    cat("PC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
    
    metric_out <- graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric    = "NC",
      alloc_ram = ram
    )
    nc_value <- metric_out[[2]]$NC
    cat("NC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
    
    metric_out <- graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric    = "MSC",
      alloc_ram = ram
    )
    msc_value <- metric_out[[2]]$MSC
    cat("MSC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
    
    metric_out <- graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric    = "SLC"
    )
    slc_value <- metric_out[[2]]$SLC
    cat("SLC calculated in", round(difftime(Sys.time(), t_metric_start, units = "secs"), 2), "sec\n")
    
    # # Delta PC for nodes
    # #deltaPC creates separate .txt files
    # t_dPCnode_start <- Sys.time()
    # #create dPC for nodes
    # graphab_metric(
    #   proj_name = nproj_name,
    #   proj_path = nproj_dir,
    #   graph     = graph_name,
    #   metric = "dPC",
    #   dist = t,
    #   prob = 0.05,
    #   obj = "patch"
    # )
    # cat("dPC nodes calculated in", round(difftime(Sys.time(), t_dPCnode_start, units = "secs"), 2), "sec\n")
    # 
    # #create dPC edges
    # t_dPCedges_start <- Sys.time()
    # graphab_metric(
    #   proj_name = nproj_name,
    #   proj_path = nproj_dir,
    #   graph     = graph_name,
    #   metric = "dPC",
    #   dist = t,
    #   prob = 0.05,
    #   obj = "links"
    # )
    # cat("dPC edges calculated in", round(difftime(Sys.time(), t_dPCedges_start, units = "secs"), 2), "sec\n")
    # 
    # Delta IIC for nodes
    #deltaIIC creates separate .txt files
    t_dIICnode_start <- Sys.time()
    #for nodes
    graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "patch",
      alloc_ram = ram
    )
    cat("dIIC nodes calculated in", round(difftime(Sys.time(), t_dIICnode_start, units = "secs"), 2), "sec\n")
    
    #edges
    t_dIICedges_start <- Sys.time()
    graphab_metric(
      proj_name = nproj_name,
      proj_path = nproj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "links",
      alloc_ram = ram
    )
    cat("dIIC edges calculated in", round(difftime(Sys.time(), t_dIICedges_start, units = "secs"), 2), "sec\n")
    
    
    # Store results
    results[[length(results) + 1]] <- data.frame(
      raster_file = hab_file,
      cost_file   = res_file,
      minarea     = minarea,
      thr         = thr,
      metapatch = metapatch,
      metadist = metadist,
      IIC = iic_value,
      PC = pc_value,
      NC = nc_value,
      MSC = msc_value,
      SLC = slc_value,
      runtime_sec = round(difftime(Sys.time(), t_link_start, units = "secs"), 2))
    
    results_df <- dplyr::bind_rows(results)
    
    #### Merge dIIC with links/patches file ####
    proj_folders <- list.dirs(proj_dir, full.names = TRUE, recursive = FALSE)
    proj_folders <- proj_folders[grepl(paste0("^", runname, ".*_habcode[0-9]+.*$"), basename(proj_folders))]
    proj_folders <- proj_folders[basename(proj_folders) == proj_name]
    
    for (proj_folder in proj_folders) {
      message("Processing: ", proj_folder)
      nproj_folder <- list.dirs(proj_folder, full.names = TRUE, recursive = FALSE)
      message("Processing: ", nproj_folder)
      
      # read shapefiles
      patches_shp <- vect(file.path(nproj_folder, "patches.shp"))
      patches_shp <- patches_shp[patches_shp$Id > 0, ]
      pts <- centroids(patches_shp)
      patch.df <- as.data.frame(patches_shp)
      
      # create output dirs
      out_links_dir <- file.path(proj_dir, "dIIC_links")
      out_patch_dir <- file.path(proj_dir, "dIIC_patch")
      out_node_dir <- file.path(proj_dir, "dIIC_node")
      dir.create(out_links_dir, showWarnings = FALSE)
      dir.create(out_patch_dir, showWarnings = FALSE)
      dir.create(out_node_dir, showWarnings = FALSE)
      
      # locate links shapefiles
      links_shp_files <- list.files(nproj_folder, pattern = "*-links.*\\.shp$", full.names = TRUE)
      
      # locate dIIC text files
      links_txt_files <- list.files(nproj_folder, pattern = "^delta-dIIC.*links.*\\.txt$", full.names = TRUE)
      patch_txt_files <- list.files(nproj_folder, pattern = "^delta-dIIC.*patch.*\\.txt$", full.names = TRUE)
      
      for (links in links_shp_files) {
        links_base <- sub("-links\\.shp$", "", basename(links))
        links_key  <- sub("^(.*_)?(ma[0-9]+_).*", "\\2", links_base)
        links_shp <- vect(links)
        links_df <- as.data.frame(links_shp)
        
        for (txt in links_txt_files) {
          txt_base <- basename(txt)
          txt_key  <- sub("^(.*_)?(ma[0-9]+_).*", "\\2", txt_base)
          
          # match on both minarea (ma) and threshold (thr)
          if (grepl(links_key, txt_base)) {
            message("Processing matching pair: ", links_base, " <-> ", txt_base)
            
            diic_edges <- read_tsv(txt, col_types = cols()) %>%
              filter(Id != "Init") %>%
              mutate(Id = as.character(Id),
                     d_IICCon = as.numeric(d_IIC))
            
            links_df <- links_df %>%
              mutate(Id = as.character(Id)) %>%
              left_join(diic_edges %>% dplyr::select(Id, d_IICCon), by = "Id")
            
            # ensure the column is actually attached
            out_links <- links_shp
            out_links$d_IICCon <- links_df$d_IICCon
            
            # verify before writing
            if (!"d_IICCon" %in% names(out_links)) {
              stop("Failed to attach d_IICCon to shapefile ", basename(links))
            }
            
            base_name <- tools::file_path_sans_ext(basename(txt))
            base_name <- gsub("[^A-Za-z0-9_]", "_", base_name)
            
            out_file <- file.path(out_links_dir, paste0(base_name, ".shp"))
            writeVector(out_links, out_file, overwrite = TRUE)
            message("✅ Saved links shapefile with d_IICCon: ", out_file)
          }
        }
      }
      
      # --- process patches ---
      for (txt in patch_txt_files) {
        diic_tbl <- read_delim(txt, delim = "\t", trim_ws = TRUE, show_col_types = FALSE) %>%
          filter(Id != "Init") %>%
          mutate(Id = as.integer(Id)) %>%
          dplyr::select(Id, d_IIC)
        
        out_attr <- left_join(patch.df, diic_tbl, by = "Id")
        
        out_patches <- patches_shp
        out_patches$dIIC <- out_attr$dIIC
        
        out_file <- file.path(out_patch_dir,
                              paste0(tools::file_path_sans_ext(basename(txt)), ".shp"))
        out_file <- normalizePath(out_file, mustWork = FALSE)
        
        if (nrow(out_patches) > 0) {
          terra::writeVector(out_patches, out_file, overwrite = TRUE)
          message("Saved patches shapefile: ", out_file)
        } else {
          message("Skipped (no patches): ", txt)
        }
        
        pts$dIIC <- out_attr$d_IIC[match(pts$Id, out_attr$Id)]
        out_file <- file.path(out_node_dir,
                              paste0(tools::file_path_sans_ext(basename(txt)), ".shp"))
        out_file <- normalizePath(out_file, mustWork = FALSE)
        
        if (nrow(pts) > 0) {
          terra::writeVector(pts, out_file, overwrite = TRUE)
          message("Saved nodes shapefile: ", out_file)
        } else {
          message("Skipped (no nodes): ", txt)
        }
      }
    }
    
  }
  write.csv(results_df, "graphab_results_summary.csv", row.names = FALSE)
  total_end <- Sys.time()
  cat("\n=== Total runtime:",
      round(difftime(total_end, total_start, units = "mins"), 2),
      "minutes ===\n")  
}

  