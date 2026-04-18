#' Compare Two Entities with Path Tracking
#' @param old_ent The original R6 Entity
#' @param new_ent The new R6 Entity
#' @param path Current breadcrumb path (e.g., "Report > Intro")
#' @export
diff_entities <- function(old_ent, new_ent, path = "") {
  # If path is not set, use object label
  current_path <- if(path == "" || is.na(path)) new_ent$label %||% "Unlabeled" else path

  changes <- data.frame(
    path = character(),      # PATH IS NOW INCLUDED
    component = character(),
    key = character(),
    old_value = character(),
    new_value = character(),
    status = character(),
    stringsAsFactors = FALSE
  )

  # Helper function to add rows
  add_change <- function(df, comp, key, old_v, new_v, stat) {
    rbind(df, data.frame(
      path = current_path, # Record path
      component = comp, key = key,
      old_value = as.character(old_v),
      new_value = as.character(new_v),
      status = stat,
      stringsAsFactors = FALSE
    ))
  }

  # 1. Compare Label
  if (!identical(old_ent$label, new_ent$label)) {
    changes <- add_change(changes, "Core", "label", old_ent$label %||% "NULL", new_ent$label %||% "NULL", "Changed")
  }

  # 2. Compare Metadata
  all_keys <- union(names(old_ent$metadata), names(new_ent$metadata))
  for (key in all_keys) {
    old_val <- old_ent$get_prop(key)
    new_val <- new_ent$get_prop(key)

    # Handle NULL cases
    if (is.null(old_val) && !is.null(new_val)) {
      changes <- add_change(changes, "Metadata", key, "NULL", new_val, "Added")
      next
    } else if (!is.null(old_val) && is.null(new_val)) {
      changes <- add_change(changes, "Metadata", key, old_val, "NULL", "Removed")
      next
    } else if (is.null(old_val) && is.null(new_val)) {
      # Both NULL, no change
      next
    }

    # Handle NA cases (after NULL check, both are not NULL)
    old_is_na <- is.na(old_val)
    new_is_na <- is.na(new_val)

    if (old_is_na && !new_is_na) {
      changes <- add_change(changes, "Metadata", key, "NA", new_val, "Changed")
      next
    } else if (!old_is_na && new_is_na) {
      changes <- add_change(changes, "Metadata", key, old_val, "NA", "Changed")
      next
    } else if (old_is_na && new_is_na) {
      # Both NA, no change
      next
    } else if (!identical(old_val, new_val)) {
      # Neither NULL nor NA, and values differ
      changes <- add_change(changes, "Metadata", key, old_val, new_val, "Changed")
    }
  }

  # 3. Compare structure (presence of children)
  old_rel <- if(!is.null(old_ent$relations$contains)) old_ent$relations$contains$object_id else character(0)
  new_rel <- if(!is.null(new_ent$relations$contains)) new_ent$relations$contains$object_id else character(0)

  for (child in setdiff(new_rel, old_rel)) {
    changes <- add_change(changes, "Structure", "child", "", child, "Added")
  }
  for (child in setdiff(old_rel, new_rel)) {
    changes <- add_change(changes, "Structure", "child", child, "", "Removed")
  }

  return(changes)
}

#' Deep Recursive Comparison
#' @export
diff_hierarchy <- function(old_ent, new_ent, path = "") {
  # Call flat diff for current level
  changes <- diff_entities(old_ent, new_ent, path)

  # Define path to pass to children
  current_path <- if(path == "" || is.na(path)) new_ent$label %||% "Unlabeled" else path

  # Recursion over common children
  old_rel <- if(!is.null(old_ent$relations$contains)) old_ent$relations$contains$object_id else character(0)
  new_rel <- if(!is.null(new_ent$relations$contains)) new_ent$relations$contains$object_id else character(0)
  common_ids <- intersect(old_rel, new_rel)

  if (length(common_ids) > 0) {
    for (cid in common_ids) {
      o_child <- .instantiate_entity(cid, old_ent$con)
      n_child <- .instantiate_entity(cid, new_ent$con)

      # Form path: "Parent > Child"
      child_path <- paste0(current_path, " > ", n_child$label %||% "Unlabeled")

      # Recursive call
      child_changes <- diff_hierarchy(o_child, n_child, child_path)
      changes <- rbind(changes, child_changes)
    }
  }

  return(changes)
}

#' Render Diff as Markdown
#' @param diff_df The data.frame returned by diff_entities
#' @export
render_diff_markdown <- function(diff_df) {
  if (nrow(diff_df) == 0) return("No changes detected.")

  lines <- c("### Change Log", "")

  for (i in 1:nrow(diff_df)) {
    row <- diff_df[i, ]
    line <- sprintf("- **[%s]** %s: ", row$status, row$key)

    if (row$status == "Changed") {
      line <- paste0(line, "~~", row$old_value, "~~ -> **", row$new_value, "**")
    } else if (row$status == "Added") {
      line <- paste0(line, "Added **", row$new_value, "**")
    } else if (row$status == "Removed") {
      line <- paste0(line, "Removed ~~", row$old_value, "~~")
    }

    lines <- c(lines, line)
  }

  return(paste(lines, collapse = "\n"))
}
