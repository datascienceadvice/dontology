#' Find Instances by Class or Label
#'
#' @description
#' Searches the database for instances based on their ontology class and/or
#' a naming pattern. Returns a list of initialized R6 objects.
#'
#' @param con DBIConnection. Active SQLite database connection.
#' @param class_id Character. Optional. The ID of the ontology class to filter by.
#' @param pattern Character. Optional. A search pattern for the label (uses SQL LIKE syntax).
#'
#' @return A list of initialized R6 objects (\code{DocumentInstance} if class is "Document",
#' otherwise \code{Entity}).
#' @importFrom DBI dbGetQuery
#' @export
find_instances <- function(con, class_id = NULL, pattern = NULL) {
  query <- "SELECT instance_id, class_id FROM instances WHERE 1=1"
  params <- list()

  if (!is.null(class_id)) {
    query <- paste(query, "AND class_id = ?")
    params <- c(params, class_id)
  }

  if (!is.null(pattern)) {
    query <- paste(query, "AND label LIKE ?")
    params <- c(params, paste0("%", pattern, "%"))
  }

  res <- DBI::dbGetQuery(con, query, params = params)

  if (nrow(res) == 0) return(list())

  # Instantiate the correct R6 class based on the class_id stored in the database
  lapply(1:nrow(res), function(i) {
    if (res$class_id[i] == "Document") {
      DocumentInstance$new(res$instance_id[i], con)
    } else {
      Entity$new(res$instance_id[i], con)
    }
  })
}

#' Find Instances by Attribute Value
#'
#' @description
#' Searches for entities that have a specific metadata property matching
#' the provided value (EAV model search).
#'
#' @param con DBIConnection. Active SQLite database connection.
#' @param property_id Character. The attribute key to search for (e.g., "status").
#' @param value Character. The value to match (e.g., "Draft").
#'
#' @return A list of initialized \code{Entity} objects.
#' @export
find_by_attribute <- function(con, property_id, value) {
  query <- "SELECT instance_id FROM attributes WHERE property_id = ? AND value = ?"
  res <- DBI::dbGetQuery(con, query, params = list(property_id, value))

  if (nrow(res) == 0) return(list())

  lapply(res$instance_id, function(id) Entity$new(id, con))
}

#' Impact Analysis: Where is this object used?
#'
#' @description
#' Provides traceability by finding all entities (subjects) that reference
#' the specified object ID via any predicate in the relations table.
#'
#' @param con DBIConnection. Active SQLite database connection.
#' @param object_id Character. The ID of the object being referenced.
#'
#' @return A data.frame containing the subject_id and predicate_id of
#' referencing entities, or NULL if no usages are found.
#' @export
where_used <- function(con, object_id) {
  query <- "SELECT subject_id, predicate_id FROM relations WHERE object_id = ?"
  res <- DBI::dbGetQuery(con, query, params = list(object_id))

  if (nrow(res) == 0) return(NULL)

  message("Object '", object_id, "' is referenced in the following locations:")
  return(res)
}

#' Full-Text Content Search
#'
#' @description
#' Performs a keyword search within the 'content' attribute of all instances
#' stored in the database.
#'
#' @param con DBIConnection. Active SQLite database connection.
#' @param term Character. The search term or keyword.
#'
#' @return A data.frame containing instance_id, label, and the matching content.
#' @export
search_content <- function(con, term) {
  query <- "
    SELECT i.instance_id, i.label, a.value as content
    FROM instances i
    JOIN attributes a ON i.instance_id = a.instance_id
    WHERE a.property_id = 'content' AND a.value LIKE ?
  "
  res <- DBI::dbGetQuery(con, query, params = list(paste0("%", term, "%")))
  return(res)
}
