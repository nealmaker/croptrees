# Function to build and save a decision tree for pre-filtered data, which must
# include drate, spp, and site as variables, in addition to any predictors that
# will be used.
decision_tree <- function(data) {
  # Load required packages
  require(rpart)
  require(grid)
  require(gridExtra)

  # Extract species, site, and discount rate from the data
  # (all rows should have same values since data is pre-filtered)
  spp <- unique(data$spp)
  site <- unique(data$site)
  drate <- unique(data$drate)

  # Validation checks
  if (length(spp) != 1 | length(site) != 1 | length(drate) != 1) {
    stop("Data must be filtered to a single species/site/discount rate combination")
  }

  # Convert discount rate to percentage for labeling
  drate_pct <- drate * 100

  # Prepare data for modeling - exclude grouping variables
  model_data <- data[, !names(data) %in% c("spp", "site", "drate")]

  # Check if there are any crop trees in the data
  if (sum(model_data$crop == TRUE) == 0) {
    # No crop trees - create informative outputs and return NULL tree
    message(paste0("No crop trees found for ", spp, " on ", site, " at ", drate_pct, "% discount rate"))

    # Create filename base
    file_base <- paste0("output/decision-trees/dt_", spp, "_", site, "_", drate_pct, "pct")

    # Save a placeholder RDS (NULL or a note)
    saveRDS(list(
      note = "No crop trees for this combination",
      spp = spp,
      site = site,
      drate = drate
    ), file = paste0(file_base, ".rds"))

    # Create informative visualization
    png(paste0(file_base, ".png"), width = 8.5, height = 4, units = "in", res = 300)
    grid.newpage()
    title <- paste0(spp, " | ", site, " | ", drate_pct, "% Discount Rate")
    grid.text(title, x = 0.5, y = 0.7,
              gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text("NO CROP TREES", x = 0.5, y = 0.5,
              gp = gpar(fontsize = 18, fontface = "bold", col = "#e74c3c"))
    grid.text("All trees should be harvested immediately", x = 0.5, y = 0.35,
              gp = gpar(fontsize = 12, col = "gray30"))
    dev.off()

    # Return a simple list with informative content
    obj_name <- paste0("tree_", spp, "_", site, "_", drate_pct, "pct")
    result <- list(
      status = "no_crop_trees",
      name = obj_name,
      spp = spp,
      site = site,
      drate = drate
    )
    return(invisible(result))
  }

  # Check if there are any non-crop trees in the data
  if (sum(model_data$crop == FALSE) == 0) {
    # All trees are crop trees - create informative outputs
    message(paste0("All trees are crop trees for ", spp, " on ", site, " at ", drate_pct, "% discount rate"))

    # Create filename base
    file_base <- paste0("output/decision-trees/dt_", spp, "_", site, "_", drate_pct, "pct")

    # Save a placeholder RDS
    saveRDS(list(
      note = "All trees are crop trees for this combination",
      spp = spp,
      site = site,
      drate = drate
    ), file = paste0(file_base, ".rds"))

    # Create informative visualization
    png(paste0(file_base, ".png"), width = 8.5, height = 4, units = "in", res = 300)
    grid.newpage()
    title <- paste0(spp, " | ", site, " | ", drate_pct, "% Discount Rate")
    grid.text(title, x = 0.5, y = 0.7,
              gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text("ALL TREES ARE CROP TREES", x = 0.5, y = 0.5,
              gp = gpar(fontsize = 18, fontface = "bold", col = "#27ae60"))
    grid.text("All trees should be retained", x = 0.5, y = 0.35,
              gp = gpar(fontsize = 12, col = "gray30"))
    dev.off()

    # Return a simple list with informative content
    obj_name <- paste0("tree_", spp, "_", site, "_", drate_pct, "pct")
    result <- list(
      status = "all_crop_trees",
      name = obj_name,
      spp = spp,
      site = site,
      drate = drate
    )
    return(invisible(result))
  }

  # Fit decision tree with parameters balanced for practitioner use
  tree_fit <- tryCatch(
    {
      rpart(
        crop ~ .,
        data = model_data,
        method = "class",
        control = rpart.control(
          cp = 0.01,           # Prune away splits that don't improve fit by 1%
          minsplit = 20,       # Need at least 20 obs to consider splitting
          minbucket = 10,      # Need at least 10 obs in each terminal node
          maxdepth = 5         # Limit tree depth for interpretability
        )
      )
    },
    error = function(e) {
      message(paste0("Error fitting tree for ", spp, " on ", site, " at ", drate_pct, "%: ", e$message))
      return(NULL)
    }
  )

  # If tree fitting failed, return NULL
  if (is.null(tree_fit)) {
    # Create filename base
    file_base <- paste0("output/decision-trees/dt_", spp, "_", site, "_", drate_pct, "pct")

    # Save a placeholder RDS
    saveRDS(list(
      note = "Tree fitting failed for this combination",
      spp = spp,
      site = site,
      drate = drate
    ), file = paste0(file_base, ".rds"))

    # Create informative visualization
    png(paste0(file_base, ".png"), width = 8.5, height = 4, units = "in", res = 300)
    grid.newpage()
    title <- paste0(spp, " | ", site, " | ", drate_pct, "% Discount Rate")
    grid.text(title, x = 0.5, y = 0.7,
              gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text("TREE FITTING FAILED", x = 0.5, y = 0.5,
              gp = gpar(fontsize = 18, fontface = "bold", col = "#e74c3c"))
    grid.text("Unable to create decision tree for this combination", x = 0.5, y = 0.35,
              gp = gpar(fontsize = 12, col = "gray30"))
    dev.off()

    # Return a simple list with informative content
    obj_name <- paste0("tree_", spp, "_", site, "_", drate_pct, "pct")
    result <- list(
      status = "fitting_failed",
      name = obj_name,
      spp = spp,
      site = site,
      drate = drate
    )
    return(invisible(result))
  }

  # Create filename base (no spaces, appropriate for file systems)
  file_base <- paste0("output/decision-trees/dt_", spp, "_", site, "_", drate_pct, "pct")

  # Save the tree object as RDS
  saveRDS(tree_fit, file = paste0(file_base, ".rds"))

  # Create table visualization
  create_table_visualization(tree_fit, spp, site, drate_pct, paste0(file_base, ".png"))

  # Create object name for return value
  obj_name <- paste0("tree_", spp, "_", site, "_", drate_pct, "pct")

  # Return the fitted tree with informative name as attribute
  attr(tree_fit, "name") <- obj_name
  return(tree_fit)
}

# Helper functions for table visualization

extract_crop_rules <- function(tree_fit) {
  # Get all paths to terminal nodes
  all_paths <- path.rpart(tree_fit,
                          nodes = as.numeric(rownames(tree_fit$frame)[tree_fit$frame$var == "<leaf>"]),
                          print.it = FALSE)

  # Get frame to identify which terminal nodes predict CROP
  frame <- tree_fit$frame
  terminal_rows <- which(frame$var == "<leaf>")

  # For classification trees, yval shows the predicted class
  # Check which nodes predict the "positive" class (crop = TRUE)
  # In rpart, yval for classification is the class number (1-based index into levels)
  response_levels <- attr(tree_fit, "ylevels")

  # Find which level corresponds to TRUE/crop
  if (is.null(response_levels)) {
    # Binary response coded as 0/1 or FALSE/TRUE
    crop_value <- 1  # or TRUE
  } else {
    # Factor response
    crop_value <- which(response_levels %in% c("TRUE", "1", "Yes"))
    if (length(crop_value) == 0) crop_value <- 2  # Default to second level
  }

  # Identify crop terminal nodes
  crop_rows <- terminal_rows[frame$yval[terminal_rows] == crop_value]
  crop_node_nums <- as.numeric(rownames(frame)[crop_rows])

  if (length(crop_node_nums) == 0) {
    return(NULL)  # No crop trees in this model
  }

  # Get paths for crop nodes
  crop_paths <- all_paths[as.character(crop_node_nums)]

  # Parse each path into variable ranges
  all_rules <- lapply(crop_paths, function(path_text) {
    # path_text is a character vector of conditions
    # Skip the first element which is "root"
    if (length(path_text) <= 1) return(list())

    conditions <- path_text[-1]

    var_ranges <- list()

    for (condition in conditions) {
      # Parse condition (e.g., "dbh< 15" or "dbh>=9")
      # Handle different spacing patterns
      condition <- gsub(" ", "", condition)  # Remove all spaces

      if (grepl(">=", condition)) {
        parts <- strsplit(condition, ">=")[[1]]
        var_name <- parts[1]
        value <- as.numeric(parts[2])

        if (!var_name %in% names(var_ranges)) {
          var_ranges[[var_name]] <- list(min = -Inf, max = Inf)
        }
        var_ranges[[var_name]]$min <- max(var_ranges[[var_name]]$min, value)

      } else if (grepl("<", condition)) {
        parts <- strsplit(condition, "<")[[1]]
        var_name <- parts[1]
        value <- as.numeric(parts[2])

        if (!var_name %in% names(var_ranges)) {
          var_ranges[[var_name]] <- list(min = -Inf, max = Inf)
        }
        var_ranges[[var_name]]$max <- min(var_ranges[[var_name]]$max, value)
      }
    }

    return(var_ranges)
  })

  return(all_rules)
}

format_range <- function(var_range, var_name) {
  # Format a variable range nicely for display
  min_val <- var_range$min
  max_val <- var_range$max

  # Handle infinite bounds
  if (is.infinite(min_val) && is.infinite(max_val)) {
    return("Any")
  } else if (is.infinite(min_val)) {
    # Only upper bound (condition was < max_val in the tree)
    # Convert to inclusive: < x becomes <= floor(x) for discrete variables
    if (var_name %in% c("dbh", "cr", "crown_ratio")) {
      if (var_name == "dbh") {
        # For DBH: < 18.5 becomes <= 18
        return(paste0("≤ ", floor(max_val)))
      } else {
        return(paste0("< ", round(max_val, 1)))
      }
    } else {
      # For discrete variables like best_log: < 2.5 becomes <= 2
      return(paste0("≤ ", floor(max_val)))
    }
  } else if (is.infinite(max_val)) {
    # Only lower bound (condition was >= min_val in the tree)
    # >= x becomes >= ceil(x) for discrete variables (already inclusive)
    if (var_name %in% c("dbh", "cr", "crown_ratio")) {
      if (var_name == "dbh") {
        # For DBH: >= 8.5 becomes >= 9
        return(paste0("≥ ", ceiling(min_val)))
      } else {
        return(paste0("≥ ", round(min_val, 1)))
      }
    } else {
      # For discrete variables like best_log: >= 1.5 becomes >= 2
      return(paste0("≥ ", ceiling(min_val)))
    }
  } else {
    # Both bounds exist - this is a range
    # min_val comes from >= conditions (round up with ceiling)
    # max_val comes from < conditions (round down with floor)

    if (var_name == "dbh") {
      min_display <- ceiling(min_val)
      max_display <- floor(max_val)

      # Ensure proper ordering and check if equal
      if (min_display > max_display) {
        temp <- min_display
        min_display <- max_display
        max_display <- temp
      }

      if (min_display == max_display) {
        return(as.character(min_display))
      } else {
        return(paste0(min_display, "-", max_display))
      }
    } else if (var_name %in% c("cr", "crown_ratio")) {
      min_display <- round(min_val, 1)
      max_display <- round(max_val, 1)

      # Ensure proper ordering
      if (min_display > max_display) {
        temp <- min_display
        min_display <- max_display
        max_display <- temp
      }

      if (abs(min_display - max_display) < 0.01) {
        return(as.character(min_display))
      } else {
        return(paste0(min_display, "-", max_display))
      }
    } else {
      # For discrete categorical/integer variables like best_log
      min_display <- ceiling(min_val)
      max_display <- floor(max_val)

      # Ensure proper ordering
      if (min_display > max_display) {
        temp <- min_display
        min_display <- max_display
        max_display <- temp
      }

      if (min_display == max_display) {
        return(as.character(min_display))
      } else {
        return(paste0(min_display, "-", max_display))
      }
    }
  }
}

format_variable_name <- function(var_name) {
  # Format variable names nicely for display
  display_names <- list(
    "dbh" = "DBH (inches)",
    "crown_ratio" = "Crown Ratio (%)",
    "cr" = "Crown Ratio (%)",
    "best_log" = "Best Log Grade",
    "grade" = "Grade",
    "quality" = "Timber Quality"
  )

  if (var_name %in% names(display_names)) {
    return(display_names[[var_name]])
  } else {
    # Capitalize first letter, replace underscores
    return(paste0(toupper(substring(var_name, 1, 1)),
                  gsub("_", " ", substring(var_name, 2))))
  }
}

create_table_visualization <- function(tree_fit, spp, site, drate_pct, output_file) {
  # Extract rules
  rules <- extract_crop_rules(tree_fit)

  if (is.null(rules) || length(rules) == 0) {
    # No crop trees - create a simple message
    png(output_file, width = 10, height = 6, units = "in", res = 300)
    plot.new()
    title_text <- paste0(spp, " | ", site, " | ", drate_pct, "% Discount Rate")
    text(0.5, 0.6, title_text, cex = 1.2, font = 2)
    text(0.5, 0.4, "No crop trees identified for this combination",
         cex = 1)
    dev.off()
    return(invisible())
  }

  # Get all variables used
  all_vars <- unique(unlist(lapply(rules, names)))

  # Create header row
  header_row <- sapply(all_vars, format_variable_name)

  # Create data rows
  data_rows <- lapply(rules, function(rule) {
    row <- character(length(all_vars))
    for (i in seq_along(all_vars)) {
      var <- all_vars[i]
      if (var %in% names(rule)) {
        row[i] <- format_range(rule[[var]], var)
      } else {
        row[i] <- "Any"
      }
    }
    return(row)
  })

  # Combine into matrix
  table_matrix <- rbind(
    header_row,
    do.call(rbind, data_rows)
  )

  # Add outcome column
  table_matrix <- cbind(table_matrix, c("", rep("✓ CROP", length(rules))))
  colnames(table_matrix) <- NULL

  # Determine figure height based on number of rows
  fig_height <- max(4, 2 + nrow(table_matrix) * 0.4)

  # Create visualization with narrower width
  png(output_file, width = 8.5, height = fig_height, units = "in", res = 300)

  grid.newpage()

  # Title
  title <- paste0(spp, " | ", site, " | ", drate_pct, "% Discount Rate")
  grid.text(title, x = 0.5, y = 0.95,
            gp = gpar(fontsize = 16, fontface = "bold"))

  # Subtitle
  grid.text("CROP TREE CONDITIONS", x = 0.5, y = 0.90,
            gp = gpar(fontsize = 12, fontface = "italic"))

  # Create table theme with proper colors and high contrast
  table_theme <- ttheme_default(
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11, col = "black"),
      bg_params = list(fill = "white")
    ),
    colhead = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11, col = "white"),
      bg_params = list(fill = "#3498db")
    )
  )

  # Create table
  table_grob <- tableGrob(
    table_matrix,
    rows = NULL,
    theme = table_theme
  )

  # Get dimensions
  n_cols <- ncol(table_matrix)
  n_rows <- nrow(table_matrix)

  # Color cells by modifying the gtable directly
  # In gridExtra's tableGrob, cells are arranged in the gtable layout
  # We need to find which grobs correspond to which cells

  # Get the gtable layout to understand structure
  layout <- table_grob$layout

  # Find all the grobs that are cells (not borders)
  for (i in seq_along(table_grob$grobs)) {
    grob_layout <- layout[i, ]

    # Get position in the table
    row_pos <- grob_layout$t
    col_pos <- grob_layout$l

    # Check if this is a data row (not header, which is typically row 1-2 in layout)
    # Data rows start after the header
    if (row_pos > 2) {  # Skip header rows
      # Calculate actual data row number
      data_row <- row_pos - 2

      # Color the outcome column (last column) green
      if (col_pos == n_cols + 1) {  # +1 because layout includes row names column even if NULL
        # This is the outcome column
        current_grob <- table_grob$grobs[[i]]
        if (inherits(current_grob, "gTree") && !is.null(current_grob$children)) {
          for (j in seq_along(current_grob$children)) {
            if (inherits(current_grob$children[[j]], "rect")) {
              current_grob$children[[j]]$gp$fill <- "#27ae60"
            } else if (inherits(current_grob$children[[j]], "text")) {
              current_grob$children[[j]]$gp$col <- "white"
              current_grob$children[[j]]$gp$fontface <- "bold"
            }
          }
          table_grob$grobs[[i]] <- current_grob
        }
      } else {
        # Regular data cells - alternate row colors
        row_color <- if ((data_row %% 2) == 1) "white" else "#f8f9fa"
        current_grob <- table_grob$grobs[[i]]
        if (inherits(current_grob, "gTree") && !is.null(current_grob$children)) {
          for (j in seq_along(current_grob$children)) {
            if (inherits(current_grob$children[[j]], "rect")) {
              current_grob$children[[j]]$gp$fill <- row_color
            } else if (inherits(current_grob$children[[j]], "text")) {
              current_grob$children[[j]]$gp$col <- "black"
            }
          }
          table_grob$grobs[[i]] <- current_grob
        }
      }
    }
  }

  # Position table
  table_height <- min(0.75, 0.1 + (n_rows * 0.05))
  pushViewport(viewport(x = 0.5, y = 0.45, width = 0.9, height = table_height))
  grid.draw(table_grob)
  popViewport()

  # Add note
  note_y <- max(0.05, 0.45 - table_height / 2 - 0.05)
  grid.text("All other combinations are NOT CROP TREES",
            x = 0.5, y = note_y,
            gp = gpar(fontsize = 10, fontface = "italic", col = "gray50"))

  dev.off()

  invisible()
}

# decision_tree <- function(data) {
#   # Load required packages
#   require(rpart)
#   require(rpart.plot)
#
#   # Extract species, site, and discount rate from the data
#   # (all rows should have same values since data is pre-filtered)
#   spp <- unique(data$spp)
#   site <- unique(data$site)
#   drate <- unique(data$drate)
#
#   # Validation checks
#   if (length(spp) != 1 | length(site) != 1 | length(drate) != 1) {
#     stop("Data must be filtered to a single species/site/discount rate combination")
#   }
#
#   # Convert discount rate to percentage for labeling
#   drate_pct <- drate * 100
#
#   # Prepare data for modeling - exclude grouping variables
#   model_data <- data[, !names(data) %in% c("spp", "site", "drate")]
#
#   # Fit decision tree with parameters balanced for practitioner use
#   # cp = complexity parameter (lower = more complex tree)
#   # minsplit = minimum observations to attempt a split
#   # minbucket = minimum observations in terminal node
#   tree_fit <- rpart(
#     crop ~ .,
#     data = model_data,
#     method = "class",
#     control = rpart.control(
#       cp = 0.01,           # Prune away splits that don't improve fit by 1%
#       minsplit = 20,       # Need at least 20 obs to consider splitting
#       minbucket = 10,      # Need at least 10 obs in each terminal node
#       maxdepth = 5         # Limit tree depth for interpretability
#     )
#   )
#
#   # Create filename base (no spaces, appropriate for file systems)
#   file_base <- paste0("output/decision-trees/dt_", spp, "_", site, "_",
#                       drate_pct, "pct")
#
#   # Save the tree object as RDS
#   saveRDS(tree_fit, file = paste0(file_base, ".rds"))
#
#   # Create and save visualization
#   png(
#     filename = paste0(file_base, ".png"),
#     width = 10,
#     height = 8,
#     units = "in",
#     res = 300
#   )
#
#   # Create title for the plot
#   plot_title <- paste0(
#     spp, " | ", site, " | ", drate_pct, "% Discount Rate"
#   )
#
#   # Plot the tree with clean formatting
#   rpart.plot(
#     tree_fit,
#     main = plot_title,
#     type = 4,              # Label all nodes
#     extra = 2,             # Show number of observations and proportions
#     under = TRUE,          # Put class labels under nodes
#     fallen.leaves = TRUE,  # Align terminal nodes at bottom
#     box.palette = "auto",  # Color code by outcome
#     shadow.col = "gray",   # Add subtle shadow for depth
#     cex.main = 1.2         # Larger title text
#   )
#
#   dev.off()
#
#   # Create object name for return value
#   obj_name <- paste0("tree_", spp, "_", site, "_", drate_pct, "pct")
#
#   # Return the fitted tree with informative name as attribute
#   attr(tree_fit, "name") <- obj_name
#   return(tree_fit)
# }
