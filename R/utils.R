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
  if (nrow(res) > 0 && res$class_id[1] == "Document") {
    return(DocumentInstance$new(id, con))
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
