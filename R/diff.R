#' Compare Two Entities with Path Tracking
#' @param old_ent The original R6 Entity
#' @param new_ent The new R6 Entity
#' @param path Current breadcrumb path (e.g., "Report > Intro")
#' @export
diff_entities <- function(old_ent, new_ent, path = "") {
  # Если путь не задан, используем лейбл объекта
  current_path <- if(path == "") new_ent$label else path

  changes <- data.frame(
    path = character(),      # ТУТ ТЕПЕРЬ ЕСТЬ PATH
    component = character(),
    key = character(),
    old_value = character(),
    new_value = character(),
    status = character(),
    stringsAsFactors = FALSE
  )

  # Функция-помощник для добавления строк
  add_change <- function(df, comp, key, old_v, new_v, stat) {
    rbind(df, data.frame(
      path = current_path, # Записываем путь
      component = comp, key = key,
      old_value = as.character(old_v),
      new_value = as.character(new_v),
      status = stat,
      stringsAsFactors = FALSE
    ))
  }

  # 1. Сравнение Label
  if (old_ent$label != new_ent$label) {
    changes <- add_change(changes, "Core", "label", old_ent$label, new_ent$label, "Changed")
  }

  # 2. Сравнение Метаданных
  all_keys <- union(names(old_ent$metadata), names(new_ent$metadata))
  for (key in all_keys) {
    old_val <- old_ent$get_prop(key)
    new_val <- new_ent$get_prop(key)

    if (is.null(old_val) && !is.null(new_val)) {
      changes <- add_change(changes, "Metadata", key, "NULL", new_val, "Added")
    } else if (!is.null(old_val) && is.null(new_val)) {
      changes <- add_change(changes, "Metadata", key, old_val, "NULL", "Removed")
    } else if (!is.null(old_val) && !is.null(new_val) && old_val != new_val) {
      changes <- add_change(changes, "Metadata", key, old_val, new_val, "Changed")
    }
  }

  # 3. Сравнение структуры (наличие детей)
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
  # Вызываем плоский дифф для текущего уровня
  changes <- diff_entities(old_ent, new_ent, path)

  # Определяем путь для передачи детям
  current_path <- if(path == "" || is.na(path)) new_ent$label else path

  # Рекурсия по общим детям
  old_rel <- if(!is.null(old_ent$relations$contains)) old_ent$relations$contains$object_id else character(0)
  new_rel <- if(!is.null(new_ent$relations$contains)) new_ent$relations$contains$object_id else character(0)
  common_ids <- intersect(old_rel, new_rel)

  if (length(common_ids) > 0) {
    for (cid in common_ids) {
      o_child <- .instantiate_entity(cid, old_ent$con)
      n_child <- .instantiate_entity(cid, new_ent$con)

      # Формируем путь: "Родитель > Ребенок"
      child_path <- paste0(current_path, " > ", n_child$label)

      # Рекурсивный вызов
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

`%||%` <- function(a, b) if (!is.null(a)) a else b
