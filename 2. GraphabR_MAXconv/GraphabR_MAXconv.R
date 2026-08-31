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

ram = 45 #allocated ram change as needed

#### NEW GRAPHAB_METRIC() with NC, MNC, deltaIIC capability and deltadPC renewed ####
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
                        "Dg", "CCe", "CF", "Fh", "IFh", "BCh", "NC", "MSC")
  list_glob_metrics <- c("PC", "EC", "ECh", "IIC", "dPC", "dIIC", "NC", "MSC")
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
    } else if (metric %in% c("PC", "EC", "IIC", "ECh", "NC", "MSC")) {
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


#### GRAPHAB PROJECT ANALYSIS ####
#params <- read_csv("params.csv")
rules <- "ctl.xlsx"
params <- read_excel(rules, sheet = 1)

total_start <- Sys.time()
graphab_jar <- file.path(getwd(), "graphab-2.8.8.jar")

for (i in seq_len(nrow(params))) {
  proj_dir <- normalizePath(getwd())
  
  hab_file <- params$hab_file[i]
  res_file <- params$res_file[i]
  minarea  <- params$minarea[i]
  runname <- "MAX"
  spname <- params$runname[i]
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
    
    proj_name  <- paste0(runname, "_", spname, "_ma", minarea)
    
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
    
    cat("\n--- Minarea:", minarea, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0(spname, "_ma", minarea)
    
    # Snapshot existing links.shp before running Graphab (full path)
    proj_root <- normalizePath(file.path(proj_dir, proj_name))
    before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    
    # Run graphab_link and capture R errors (if any)
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
    
    # --- Check longest pathway in resulting linkset ---
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
    
    # Graph creation
    graph_name <- paste0(spname, "_ma", minarea, "_t", thr)
    
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
    
    # Delta IIC for nodes
    #deltaIIC creates separate .txt files
    t_dIICnode_start <- Sys.time()
    graphab_metric(
      proj_name = proj_name,
      proj_path = proj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "patch",
      alloc_ram = ram
    )
    cat("dIIC nodes calculated in", round(difftime(Sys.time(), t_dIICnode_start, units = "secs"), 2), "sec\n")
    
    t_dIICedges_start <- Sys.time()
    graphab_metric(
      proj_name = proj_name,
      proj_path = proj_dir,
      graph     = graph_name,
      metric = "dIIC",
      obj = "links",
      alloc_ram = ram
    )
    cat("dIIC edges calculated in", round(difftime(Sys.time(), t_dIICedges_start, units = "secs"), 2), "sec\n")
    
  #### Merge dIIC with links/patches file ####
  proj_folders <- normalizePath(list.dirs(proj_dir, full.names = TRUE, recursive = FALSE))
  proj_folders <- proj_folders[grepl("^MAX", basename(proj_folders))]
  proj_folders <- proj_folders[basename(proj_folders) == proj_name]
  
  for (proj_folder in proj_folders) {
    message("Processing: ", proj_folder)
    
    # read shapefiles
    patches_shp <- vect(file.path(proj_folder, "patches.shp"))
    patches_shp <- patches_shp[patches_shp$Id > 0, ]
    patch.df <- as.data.frame(patches_shp)
    
    # create output dirs
    out_links_dir <- file.path(proj_dir, "TOPlinks")
    out_patch_dir <- file.path(proj_dir, "dIIC_patch")
    dir.create(out_links_dir, showWarnings = FALSE)
    dir.create(out_patch_dir, showWarnings = FALSE)
    
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
    }
  }
  
    #### RANKING + BUFFER PATHWAYS ####
    #### Picking top path, add species-specific buffer on each side as polygon
    message("Ranking and buffering pathways")
    
    # helper to extract species code
    get_sp_from_name <- function(x) {
      sub(".*delta_dIIC_([A-Z]+)_.*", "\\1", basename(x))
    }
    
    for (proj_folder in proj_folders) {
      
      links_dir <- file.path(proj_dir, "TOPlinks")
      if (!dir.exists(links_dir)) {
        message("Skipping: no TOPlinks folder in ", proj_dir)
        next
      }
      
      shp_files <- list.files(links_dir, pattern = "\\.shp$", full.names = TRUE)
      if (length(shp_files) == 0) {
        message("No shapefiles found in ", links_dir)
        next
      }
      
      for (shp in shp_files) {
        
        v <- try(vect(shp))
        if (inherits(v, "try-error")) {
          message("Failed to read shapefile: ", shp)
          next
        }
        
        if (!"d_IICCon" %in% names(v)) {
          message("No 'd_IICCon' column in ", shp)
          next
        }
        
        df <- as.data.frame(v)
        
        # ranking
        df$rank   <- rank(-df$d_IICCon, ties.method = "first")
        df$w_rank <- df$rank / sum(df$rank)
        
        # selection rule
        if (nrow(df) > 50) {
          sel <- df$rank <= 50
        } else {
          q50 <- quantile(df$d_IICCon, 0.5, na.rm = TRUE)
          sel <- df$d_IICCon >= q50
        }
        
        v_filtered <- v[sel, ]
        v_filtered$rank   <- df$rank[sel]
        v_filtered$w_rank <- df$w_rank[sel]
        
        # ---- species-specific buffer width ----
        sp <- get_sp_from_name(shp)
        
        buf_width <- params$buffer[params$runname == sp]
        if (length(buf_width) != 1 || is.na(buf_width)) {
          stop("Invalid or missing buffer width for species: ", sp)
        }
        
        message("Buffering ", basename(shp), " (", sp, ") with width = ", buf_width)
        
        df_buf <- buffer(v_filtered, width = buf_width)
        
        # rasterise
        r_template <- rast(df_buf, res = res(hab_rast))
        r_buf <- rasterize(df_buf, r_template, field = 1, background = NA)
        
        out_file <- sub(".shp$", "_top.tif", shp)
        writeRaster(r_buf, out_file, overwrite = TRUE)
        
        message("Processed: ", basename(shp), " — kept ", nrow(df_buf), " top links.")
      }
    }
  } else {
    cat("\n--- Running METAPATCH Graphab code (metapatch + metadist provided) ---\n")
    proj_name <- paste0(runname, "_", spname, "_mp", metapatch)
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
    linkset_name <- paste0(spname, "_metadist", metadist)
    
    # Snapshot existing links.shp before running Graphab (full path)
    proj_root <- file.path(proj_dir, proj_name)
    before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    
    # Run graphab_link and capture R errors (if any)
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
    
    graph_name <- paste0(spname, "_metadist", metadist)
    
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
    
    cat("\n--- Minarea:", minarea, "| Resistance:", res_name, "---\n")
    linkset_name <- paste0(spname, "_ma", minarea)
    
    # Snapshot existing links.shp before running Graphab (full path)
    proj_root <- file.path(proj_dir, proj_name, nproj_name)
    before_links <- list.files(proj_root, pattern = "links\\.shp$", recursive = TRUE, full.names = TRUE)
    
    # Run graphab_link and capture R errors (if any)
    link_err <- NULL
    t_link_start <- Sys.time()
    tryCatch({
      graphab_link(
        proj_name    = nproj_name,
        proj_path    = nproj_dir,
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
    
    # --- Check longest pathway in resulting linkset ---
    if (length(new_links) > 0) {
      link_file <- new_links[1]
    } else if (has_link_named) {
      link_file <- list.files(nproj_root, pattern = paste0(linkset_name, ".*links\\.shp$"), 
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
    
    # ---- Graph creation (only runs if linkset created) ----
    graph_name <- paste0(spname, "_ma", minarea, "_thr", thr, "_graph")
    
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
    
    # IIC, PC, NC, MSC metric
    t_metric_start <- Sys.time()
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
    
  }
  
  #### Merge dIIC with links/patches file ####
  proj_folders <- list.dirs(proj_dir, full.names = TRUE, recursive = FALSE)
  proj_folders <- proj_folders[ grepl("^MAX.*_mp[0-9]+(\\.[0-9]+)?$", basename(proj_folders))]
  proj_folders <- proj_folders[basename(proj_folders) == proj_name]
  
  for (proj_folder in proj_folders) {
    message("Processing: ", proj_folder)
    nproj_folder <- list.dirs(proj_folder, full.names = TRUE, recursive = FALSE)
    message("Processing: ", nproj_folder)
    
    # read shapefiles
    patches_shp <- vect(file.path(nproj_folder, "patches.shp"))
    patches_shp <- patches_shp[patches_shp$Id > 0, ]
    patch.df <- as.data.frame(patches_shp)
    
    # create output dirs
    out_links_dir <- file.path(proj_dir, "TOPlinks")
    out_patch_dir <- file.path(proj_dir, "dIIC_patch")
    dir.create(out_links_dir, showWarnings = FALSE)
    dir.create(out_patch_dir, showWarnings = FALSE)
    
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
    }
  }
  
  #### RANKING + BUFFER PATHWAYS ####
  #### Picking top path, add species-specific buffer on each side as polygon
  message("Ranking and buffering pathways")
  
  # helper to extract species code
  get_sp_from_name <- function(x) {
    sub(".*delta_dIIC_([A-Z]+)_.*", "\\1", basename(x))
  }
  
  for (proj_folder in proj_folders) {
    
    links_dir <- file.path(proj_dir, "TOPlinks")
    if (!dir.exists(links_dir)) {
      message("Skipping: no TOPlinks folder in ", proj_dir)
      next
    }
    
    shp_files <- list.files(links_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_files) == 0) {
      message("No shapefiles found in ", links_dir)
      next
    }
    
    for (shp in shp_files) {
      
      v <- try(vect(shp))
      if (inherits(v, "try-error")) {
        message("Failed to read shapefile: ", shp)
        next
      }
      
      if (!"d_IICCon" %in% names(v)) {
        message("No 'd_IICCon' column in ", shp)
        next
      }
      
      df <- as.data.frame(v)
      
      # ranking
      df$rank   <- rank(-df$d_IICCon, ties.method = "first")
      df$w_rank <- df$rank / sum(df$rank)
      
      # selection rule
      if (nrow(df) > 50) {
        sel <- df$rank <= 50
      } else {
        q50 <- quantile(df$d_IICCon, 0.5, na.rm = TRUE)
        sel <- df$d_IICCon >= q50
      }
      
      v_filtered <- v[sel, ]
      v_filtered$rank   <- df$rank[sel]
      v_filtered$w_rank <- df$w_rank[sel]
      
      # ---- species-specific buffer width ----
      sp <- get_sp_from_name(shp)
      
      buf_width <- params$buffer[params$runname == sp]
      if (length(buf_width) != 1 || is.na(buf_width)) {
        stop("Invalid or missing buffer width for species: ", sp)
      }
      
      message("Buffering ", basename(shp), " (", sp, ") with width = ", buf_width)
      
      df_buf <- buffer(v_filtered, width = buf_width)
      
      # rasterise
      r_template <- rast(df_buf, res = res(hab_rast))
      r_buf <- rasterize(df_buf, r_template, field = 1, background = NA)
      
      out_file <- sub(".shp$", "_top.tif", shp)
      writeRaster(r_buf, out_file, overwrite = TRUE)
      
      message("Processed: ", basename(shp), " — kept ", nrow(df_buf), " top links.")
    }
  }
}

# #### checking % overlap btween paths ####
# message("Checking % overlap")
# top_dir <- file.path(proj_dir, "TOPlinks") #folder where top pathways are
# paths <- list.files(top_dir, pattern = "\\.tif$", full.names = TRUE) #pathway files
# LULC <- rast("conv.tif")
# 
# # load rasters
# r_list <- lapply(paths, function(p) {
#   r <- rast(p)
#   resample(r, LULC, method = "near")   # assuming categorical/binary pathways
# })
# 
# n <- length(r_list)
# overlap_mat <- matrix(0, n, n)
# rownames(overlap_mat) <- basename(paths)
# colnames(overlap_mat) <- basename(paths)
# 
# # compute pairwise overlap
# for(i in 1:n){
#   for(j in i:n){
#     r1 <- r_list[[i]]
#     r2 <- r_list[[j]]
# 
#     # overlap: cells where both have value (assumes 1 = presence)
#     both <- (r1 == 1 & r2 == 1)
# 
#     # union: cells where either has presence
#     union <- (r1 == 1 | r2 == 1)
# 
#     # proportions
#     both_n  <- global(both,  "sum", na.rm = TRUE)[[1]]
#     union_n <- global(union, "sum", na.rm = TRUE)[[1]]
# 
#     pct <- ifelse(union_n == 0, 0, 100 * both_n / union_n)
# 
#     overlap_mat[i,j] <- pct
#     overlap_mat[j,i] <- pct
#   }
# }
# 
# overlap_mat #shows how much overlap there is between species pathways
# write.csv(overlap_mat, "overlapmx.csv")

#### OVERLAY WITH LULC ####
message("Starting overlay with LULC")
top_dir <- file.path(proj_dir, "TOPlinks") #folder where top pathways are
paths <- list.files(top_dir, pattern = "\\.tif$", full.names = TRUE) #pathway files

#### change value in pathways to assigned new value for that species ####
message("Assigning unique values to pathways")
sp_values <- read_excel(rules, sheet = 2)

for (p in paths) {
  
  # ---- Extract species name (letters before first "_") ----
  fname <- basename(p)
  sp <- sub(".*delta_dIIC_([^_]+).*", "\\1", fname)
  
  # ---- Lookup value in Excel ----
  value_row <- sp_values %>% filter(sp == !!sp)
  
  if (nrow(value_row) == 0) {
    warning(paste("Species", sp, "not found in Excel. Skipping:", fname))
    next
  }
  
  sp_value <- value_row$value
  
  path_r <- rast(p)
  path_r_updated <- classify(
    path_r,
    rcl = matrix(c(1, 1, sp_value), ncol = 3, byrow = TRUE),
    include.lowest = TRUE
  )
  
  path_r_updated <- resample(path_r_updated, LULC, method = "near")
  
  writeRaster(path_r_updated, p, overwrite = TRUE)
  
  cat("Updated and overwritten:", fname, "→ value =", sp_value, "\n")
}

paths <- list.files(top_dir, pattern = "\\.tif$", full.names = TRUE) # reload pathway files

#### converting convertible pathways ####
message("Converting pathways")
maxsheets <- excel_sheets(rules)[grepl("^conv[0-9]+$", excel_sheets(rules))]

for (sheets in maxsheets) {
  cat("Processing scenario:", sheets, "\n")
  
  convert_tbl <- read_excel(rules, sheet = sheets)

  for (p in paths) {
    
    sp <- sub(".*delta_dIIC_([^_]+).*", "\\1", basename(p))
    
    cat("Processing:", fname, "Species:", sp, "\n")
    
    path_r <- rast(p)
    
    # lookup the species column from table
    if (!(sp %in% names(convert_tbl))) {
      warning(sp, " not found in table. Skipping.")
      next
    }
    
    # create a lookup vector: index = allowed class, value = species value
    lut <- convert_tbl[[sp]]            # species column
    names(lut) <- convert_tbl$No   # key is original LULC value
    
    # reclassify LULC based on table
    # This replaces each LULC code with its species-specific converted value
    LULC_conv <- classify(LULC, 
                          rcl = cbind(convert_tbl$No, lut),
                          include.lowest = TRUE)
    LULC_conv <- as.factor(LULC_conv)
    
    # mask by pathway presence
    # keep only where pathway==1, else NA
    out <- mask(LULC_conv, path_r == 1)
    
    # save output with same filename (overwrite)
    dir.create("FINpaths", showWarnings = FALSE)
    outfile <- file.path("FINpaths", paste0(sp, "_", sheets, ".tif"))
    
    writeRaster(out, outfile, overwrite = TRUE)
  }
}

#### excluding habitat patch from some pathways
message("Excluding habitat from pathways")
# folders
path_dir <- "FINpaths"
hab_dir  <- "convhab"
out_dir  <- "FINpathsconv"
dir.create(out_dir, showWarnings = FALSE)

# read rules table (sheet 3)
tab <- read_excel(rules, sheet = "habexc")
tab <- as.data.frame(tab)

# first column = pathway values
colnames(tab)[1] <- "path_value"
species_cols <- colnames(tab)[-1]

# list rasters
path_files <- list.files(path_dir, pattern = "\\.tif$", full.names = TRUE)
hab_files  <- list.files(hab_dir,  pattern = "\\.tif$", full.names = TRUE)

# helper to extract species code from habitat filenames
get_sp <- function(x) sub("_.*", "", basename(x))

for (pf in path_files) {
  
  p <- rast(pf)
  
  # pathway values actually present
  vals_present <- unique(values(p))
  vals_present <- vals_present[!is.na(vals_present)]
  
  # keep only values that exist in table
  vals_present <- intersect(vals_present, tab$path_value)
  
  for (v in vals_present) {
    
    # row corresponding to this pathway value
    row_v <- tab[tab$path_value == v, , drop = FALSE]
    if (nrow(row_v) == 0) next
    
    # species to exclude for this pathway value
    exclude_sp <- species_cols[
      colSums(row_v[, species_cols] > 1, na.rm = TRUE) > 0
    ]
    
    # nothing to exclude → skip
    if (length(exclude_sp) == 0) next
    
    # exclude habitats one by one
    for (sp in exclude_sp) {
      
      hf <- hab_files[get_sp(hab_files) == sp]
      if (length(hf) == 0) next
      
      h <- rast(hf[1])
      h <- resample(h, p, method = "near")
      
      # remove ONLY this pathway value where habitat == 1
      mask <- (p == v) & (h == 1)
      p[mask] <- NA
    }
  }
  
  out_path <- file.path(out_dir, basename(pf))
  writeRaster(p, out_path, overwrite = TRUE)
}

#### Put together path with different order configuration as final combined land cover rasters ####
message("Combining all pathways")
# folders
fin_dir  <- "FINpathsconv"
lulc_dir <- "LULC"
out_dir  <- "convLULC"
dir.create(out_dir, showWarnings = FALSE)

# species priority
species_order <- sp_values$sp

# list files
fin_files  <- list.files(fin_dir,  full.names = TRUE, pattern = "\\.tif$")
lulc_files <- list.files(lulc_dir, full.names = TRUE, pattern = "\\.tif$")

# extract species + conversion
get_conv <- function(x) sub(".*_(conv[0-9]+)\\.tif$", "\\1", basename(x))

fin_info <- data.frame(
  file = fin_files,
  sp   = sub("_conv[0-9]+\\.tif$", "", basename(fin_files)),
  conv = get_conv(fin_files),
  stringsAsFactors = FALSE
)

# loop over each LULC map
for (lulc_path in lulc_files) {
  message("Starting on:", lulc_path)
  lulc_name <- basename(lulc_path)
  ras_lulc  <- rast(lulc_path)
  
  # loop over each conversion type
  for (conv_id in unique(fin_info$conv)) {
    
    # FINpaths for this conversion, ordered by priority
    fin_conv <- fin_info %>%
      filter(conv == conv_id) %>%
      arrange(match(sp, species_order))
    
    final <- ras_lulc
    
    # tracks cells already claimed by higher-priority pathways
    path_mask <- rast(ras_lulc)
    values(path_mask) <- NA
    
    for (fp in fin_conv$file) {
      
      p <- rast(fp)
      p <- resample(p, ras_lulc, method = "near")
      
      # pathway overwrites LULC, but not other pathways
      write_cells <- !is.na(p) & is.na(path_mask)
      
      final[write_cells] <- p[write_cells]
      path_mask[write_cells] <- 1
    }
    
    #final <- as.factor(final)
    
    # output name
    out_name <- sub("\\.tif$", paste0("_", conv_id, ".tif"), lulc_name)
    out_path <- file.path(out_dir, out_name)
    
    writeRaster(final, out_path, overwrite = TRUE)
  }
}

#### PATCH ADDITION ####
maxsheets <- excel_sheets(rules)[grepl("^pconv[0-9]+$", excel_sheets(rules))]

lulc_dir <- "LULC"
hab_dir  <- "convhab"
out_dir  <- "patchconv"
proj_dir <- normalizePath(getwd())

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

LULC <- rast("conv.tif")

lulc_files <- list.files(lulc_dir, full.names = TRUE, pattern = "\\.tif$")
hab_files  <- list.files(hab_dir, full.names = TRUE, pattern = "\\.tif$")

get_sp <- function(x) sub("_.*", "", basename(x))

# ----------------------------
# GLOBAL STORAGE FOR FINAL MERGE
# ----------------------------
all_species_scenarios <- list()

# ----------------------------
# MAIN LOOP: SCENARIOS
# ----------------------------
for (sheets in maxsheets) {
  
  cat("Processing scenario:", sheets, "\n")
  
  convert_tbl <- read_excel(rules, sheet = sheets)
  
  scenario_dir <- file.path(out_dir, sheets)
  dir.create(scenario_dir, showWarnings = FALSE, recursive = TRUE)
  
  tab <- read_excel(rules, sheet = "phabexc")
  colnames(tab)[1] <- "path_value"
  species_cols <- colnames(tab)[-1]
  
  for (patch in lulc_files) {
    start_time <- Sys.time()
    
    fname <- basename(patch)
    sp <- sub("_.*$", "", fname)
    
    cat("Processing:", fname, "Species:", sp, "\n")
    
    if (!(sp %in% names(convert_tbl))) next
    
    lut <- convert_tbl[[sp]]
    names(lut) <- convert_tbl$No
    
    valid_vals <- unique(lut[!is.na(lut)])
    
    LULC_conv <- classify(
      LULC,
      rcl = cbind(convert_tbl$No, lut),
      include.lowest = TRUE
    )
    
    vals_present <- intersect(valid_vals, tab$path_value)
    
    minarea <- params$minarea[params$runname == sp]
    
    habexc <- LULC_conv
    
    # ----------------------------
    # STORE PER SPECIES RESULT (scenario level)
    # ----------------------------
    v_rasters <- list()
    
    for (v in vals_present) {
      cat("Processing Species:", sp, "Value:", v, "\n")
      
      row_v <- tab[tab$path_value == v, , drop = FALSE]
      
      exclude_sp <- species_cols[
        colSums(row_v[, species_cols] > 1, na.rm = TRUE) > 0
      ]
      
      if (length(exclude_sp) == 0) next
      
      # ----------------------------
      # BUILD COMBINED EXCLUSION MASK
      # ----------------------------
      combined_mask <- NULL
      
      for (sp_ex in exclude_sp) {
        cat("Preparing exclusion for", sp_ex, "\n")
        
        hf <- hab_files[get_sp(hab_files) == sp_ex]
        if (length(hf) == 0) next
        
        h <- rast(hf[1])
        h <- resample(h, habexc, method = "near")
        
        # accumulate: TRUE where species is present
        if (is.null(combined_mask)) {
          combined_mask <- (h == 1)
        } else {
          combined_mask <- combined_mask | (h == 1)
        }
      }
      
      # if no valid masks were created
      if (is.null(combined_mask)) next
      
      # ----------------------------
      # APPLY ALL EXCLUSIONS AT ONCE
      # ----------------------------
      sel <- ifel((habexc == v) & (!combined_mask), v, NA)
      
      writeRaster(
        sel,
        "temp.tif",
        datatype = "INT4S",
        NAflag = 9999,
        overwrite = TRUE
      )
      
      # ----------------------------
      # GRAPHAB
      # ----------------------------
      cat("Graphab and filtering area size", "\n")
      proj_name <- "tempproj"
      proj_path_full <- file.path(proj_dir, proj_name)
      
      if (dir.exists(proj_path_full)) {
        unlink(proj_path_full, recursive = TRUE)
      }
      
      result <- tryCatch({
        
        suppressWarnings(
          graphab_project(
            proj_name = proj_name,
            proj_path = proj_dir,
            raster    = "temp.tif",
            habitat   = v,
            minarea   = minarea,
            nodata    = 9999,
            con8      = TRUE,
            alloc_ram = ram
          )
        )
        
        TRUE  # success flag
        
      }, error = function(e) {
        cat("Graphab failed for value", v, ":", e$message, "\n")
        FALSE
      })
      
      patch_file <- file.path(proj_path_full, "patches.tif")
      
      if (!result || !file.exists(patch_file)) {
        cat("No valid patches for value", v, "species", patch, "- skipping\n")
        next
      }
      
      v_rast <- rast(patch_file)
      v_rast <- ifel(v_rast > 0, v, NA)
      
      v_rasters[[length(v_rasters) + 1]] <- v_rast
    }
    
    # ----------------------------
    # ALIGN + PRIORITISE PER SPECIES
    # ----------------------------
    
    if (length(v_rasters) == 0) {
      cat("No rasters created for species", patch, "- skipping\n")
      next
    }
    
    if (length(v_rasters) == 1) {
      cat("Only one raster for species", patch, "- skipping mosaic\n")
      sp_raster <- v_rasters[[1]]
    } else {
      sp_raster <- do.call(mosaic, c(v_rasters, fun = "first"))
    }
    
    # v_rasters <- lapply(v_rasters, function(r) {
    #   project(r, LULC, method = "near")
    # })
    
    # # convert patches to single value rasters (priority = first occurrence)
    # v_rasters <- lapply(v_rasters, function(r) {
    #   ifel(r > 0, 1, NA)
    # })
    
    # save per species per scenario
    out_file <- file.path(scenario_dir, paste0(sp, ".tif"))
    cat("Saving per species per scenario:", out_file, "\n")
    
    writeRaster(sp_raster, out_file, overwrite = TRUE)
    
    # # store for final merge
    # all_species_scenarios[[length(all_species_scenarios) + 1]] <- sp_raster
    end_time <- Sys.time()
    cat("Time for species =", patch, ":", end_time - start_time, "\n\n")
  }
  tif_files <- list.files(scenario_dir, pattern = "\\.tif$", full.names = TRUE)
  tif_names <- tools::file_path_sans_ext(basename(tif_files))
  
  # keep only valid species
  valid_idx <- tif_names %in% species_cols
  
  if (!any(valid_idx)) {
    cat("No valid species rasters found for scenario", sheets, "- skipping\n")
    next
  }
  
  tif_files <- tif_files[valid_idx]
  tif_names <- tif_names[valid_idx]
  
  # order by species priority
  ord <- match(species_cols, tif_names)
  ord <- ord[!is.na(ord)]
  
  tif_files <- tif_files[ord]
  
  all_species_scenarios <- lapply(tif_files, rast)
  
  #### CHECKING OVERLAP
  # convert to presence/absence (important)
  rasters <- lapply(all_species_scenarios, function(r) {
    ifel(!is.na(r), 1, NA)
  })

  n <- length(rasters)
  overlap_mat <- matrix(NA, n, n)

  names <- tools::file_path_sans_ext(basename(tif_files))
  rownames(overlap_mat) <- names
  colnames(overlap_mat) <- names

  for (i in 1:n) {
    for (j in i:n) {

      r1 <- rasters[[i]]
      r2 <- rasters[[j]]

      # intersection
      inter <- global((r1 == 1) & (r2 == 1), "sum", na.rm = TRUE)[1,1]

      # union
      union <- global((r1 == 1) | (r2 == 1), "sum", na.rm = TRUE)[1,1]

      val <- if (union == 0) NA else inter / union

      overlap_mat[i, j] <- val
      overlap_mat[j, i] <- val
    }
  }

  overlap_mat <- overlap_mat * 100
  round(overlap_mat, 2)
  
  # MERGING
  if (length(all_species_scenarios) == 0) {
    cat("No rasters to merge for scenario", sheets, "\n")
    next
  }
  
  if (length(all_species_scenarios) == 1) {
    final_raster <- all_species_scenarios[[1]]
  } else {
    final_raster <- do.call(mosaic, c(all_species_scenarios, fun = "first"))
  }
  
  final_raster <- resample(final_raster, LULC, method = "near")
  CB
  out_final <- file.path(scenario_dir, paste0(sheets, "_fin.tif"))
  
  cat("Saving final scenario raster:", out_final, "\n")
  writeRaster(final_raster, out_final, overwrite = TRUE)
  
}

#### MERGING PATCH AND PATH CONVERSIONS ####
path_dir  <- "convLULC"
patch_dir <- "patchconv"

# output folder
out_dir <- "FINpathpatch"
dir.create(out_dir, showWarnings = FALSE)

path_files <- list.files(
  path_dir,
  pattern = "\\.tif$",
  full.names = TRUE
)

# loop through

for (path in path_files) {
  
  fname <- basename(path)
  
  cat("Processing:", fname, "\n")
  
  # species = first 2 letters
  sp <- substr(fname, 1, 2)
  
  # extract conv scenario
  conv_match <- regmatches(
    fname,
    regexpr("conv[0-9]+", fname)
  )
  
  if (length(conv_match) == 0) {
    cat("No conversion scenario found - skipping\n")
    next
  }
  
  conv_scn <- conv_match
  
  # corresponding patch folder/file
  # conv1 -> pconv1
  patch_scn <- paste0("p", conv_scn)
  
  patch_file <- file.path(
    patch_dir,
    patch_scn,
    paste0(patch_scn, "_fin.tif")
  )
  
  if (!file.exists(patch_file)) {
    cat("Patch file not found:", patch_file, "\n")
    next
  }

  conv_rast  <- rast(path)
  patch_rast <- rast(patch_file)
  
  # align patch to conv raster
  patch_rast <- resample(patch_rast, conv_rast, method = "near")

  # PRIORITY TO PATCH RASTER
  
  final_rast <- mosaic(
    patch_rast,
    conv_rast,
    fun = "first"
  )

  # OUTPUT 

  out_file <- file.path(
    out_dir,
    paste0(sp, "_fin_", conv_scn, ".tif")
  )
  
  cat("Saving:", out_file, "\n")
  
  writeRaster(
    final_rast,
    out_file,
    overwrite = TRUE
  )
}
