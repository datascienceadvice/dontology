#' Internal Factory for Ontological Entity Instantiation
#'
#' @description
#' This helper function implements a factory pattern to instantiate the correct
#' R6 class for a given ID. It looks up the \code{class_id} in the \code{instances}
#' table and determines whether to return a specialized \code{DocumentInstance}
#' or a generic \code{Entity}.
#'
#' @details
#' Using this factory instead of direct \code{Entity$new()} calls is crucial for
#' maintaining polymorphic behavior. It ensures that when a document structure
#' is traversed, specialized methods (like \code{render} or \code{validate})
#' are correctly dispatched for specific classes.
#'
#' @param id Character. The unique identifier (\code{instance_id}) of the entity.
#' @param con DBIConnection. An active SQLite database connection.
#'
#' @return An R6 object of class \code{DocumentInstance} or \code{Entity}.
#'
#' @note
#' This is an internal utility (prefixed with a dot). It is intended for use
#' within the package's recursive methods and query functions.
#'
#' @keywords internal
#' @noRd
.instantiate_entity <- function(id, con) {
  res <- DBI::dbGetQuery(con, "SELECT class_id FROM instances WHERE instance_id = ?", params = list(id))

  if (nrow(res) > 0) {
    cls <- res$class_id[1]
    if (toupper(cls) == "DOCUMENT") {
      return(DocumentInstance$new(id, con))
    }
  }
  return(Entity$new(id, con))
}

#' Apply metadata placeholders to text
#' @param text Character string with {{key}} placeholders
#' @param metadata List of key-value pairs
#' @return Processed string
.fill_placeholders <- function(text, metadata) {
  if (is.null(text) || is.na(text) || nchar(text) == 0) return(text)
  if (length(metadata) == 0) return(text)

  for (key in names(metadata)) {
    pattern <- paste0("\\{\\{", key, "\\}\\}")
    value <- as.character(metadata[[key]])
    text <- gsub(pattern, value, text)
  }
  return(text)
}

#' Resolve Cross-References between documents
#' @param text Text containing [[REF:ID]] or [[VAL:ID:prop]]
#' @param con Database connection
.resolve_cross_refs <- function(text, con) {
  if (is.null(text) || is.na(text) || nchar(text) == 0) return(text)

  # 1. Обработка [[REF:ID]] -> заменяется на Label этого объекта
  # Регулярка ищет [[REF:любой_ID]]
  ref_matches <- regmatches(text, gregexpr("\\[\\[REF:[^\\]]+\\]\\]", text))[[1]]
  for (match in ref_matches) {
    target_id <- gsub("\\[\\[REF:|\\]\\]", "", match)
    res <- DBI::dbGetQuery(con, "SELECT label FROM instances WHERE instance_id = ?", params = list(target_id))

    if (nrow(res) > 0) {
      # Создаем ссылку на секцию (даже если она в другом файле, Pandoc это обработает)
      replacement <- paste0("[", res$label, "](#sec-", target_id, ")")
      text <- gsub(match, replacement, text, fixed = TRUE)
    }
  }

  # 2. Обработка [[VAL:ID:property]] -> заменяется на значение метаданных
  val_matches <- regmatches(text, gregexpr("\\[\\[VAL:[^:\\]]+:[^\\]]+\\]\\]", text))[[1]]
  for (match in val_matches) {
    parts <- unlist(strsplit(gsub("\\[\\[VAL:|\\]\\]", "", match), ":"))
    target_id <- parts[1]
    prop_id <- parts[2]

    res <- DBI::dbGetQuery(con,
                           "SELECT value FROM attributes WHERE instance_id = ? AND property_id = ?",
                           params = list(target_id, prop_id))

    if (nrow(res) > 0) {
      text <- gsub(match, res$value, text, fixed = TRUE)
    }
  }

  return(text)
}
