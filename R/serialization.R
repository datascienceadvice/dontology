#' Export Document Hierarchy to JSON
#'
#' @description
#' Serializes a document and all its nested sections, metadata, and relations into
#' a structured JSON file. This is ideal for version control (Git), template sharing,
#' and providing human-readable snapshots for regulatory audits.
#'
#' @param doc An object of class \code{DocumentInstance} or \code{Entity} representing the root node.
#' @param file_path Character. Destination path for the JSON file.
#'
#' @return Invisible NULL.
#'
#' @importFrom jsonlite write_json
#' @importFrom DBI dbGetQuery
#' @export
export_to_json <- function(doc, file_path) {

  # Recursive internal function to build a nested list representation of the hierarchy
  build_list_tree <- function(entity) {
    # Capture current node attributes and metadata
    node <- list(
      id = entity$id,
      class_id = entity$class_id,
      label = entity$label,
      metadata = entity$metadata,
      children = list()
    )

    # Fetch child instances linked via the 'contains' predicate
    query <- "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains' ORDER BY sort_order ASC"
    res <- DBI::dbGetQuery(entity$con, query, params = list(entity$id))

    if (nrow(res) > 0) {
      for (cid in res$object_id) {
        child_entity <- .instantiate_entity(cid, entity$con)
        node$children[[cid]] <- build_list_tree(child_entity)
      }
    }
    return(node)
  }

  # Generate the tree structure in memory
  message("Preparing data for export: ", doc$id)
  tree_data <- build_list_tree(doc)

  # Write the structured list to a JSON file
  # auto_unbox = TRUE ensures that single-length vectors are not stored as arrays
  jsonlite::write_json(tree_data, file_path, pretty = TRUE, auto_unbox = TRUE)

  message("Document successfully exported to JSON: ", file_path)
  return(invisible(NULL))
}


#' Import Document Hierarchy from JSON
#'
#' @description
#' Reconstructs a document and its entire tree structure from a JSON file into
#' the SQLite database. It restores all metadata and re-establishes parent-child
#' relations (predicates) with correct sorting.
#'
#' @param file_path Character. Path to the source JSON file.
#' @param con DBIConnection. An active connection to the SQLite database.
#'
#' @return Character (invisibly). The ID of the root document instance.
#'
#' @importFrom jsonlite fromJSON
#' @export
import_from_json <- function(file_path, con) {
  if (!file.exists(file_path)) {
    stop(paste0("JSON file not found at: ", normalizePath(file_path, mustWork = FALSE)))
  }

  # Load JSON data into a nested list
  json_text <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  data <- jsonlite::fromJSON(file_path, simplifyVector = FALSE)

  # Recursive internal function to persist nodes and relations to the database
  restore_node <- function(node, parent_id = NULL, order = 0) {
    # Instantiate and save the entity (Entity$save handles the SQL INSERT/UPDATE)
    entity <- Entity$new(node$id, con, label = node$label, class_id = node$class_id)

    # Restore metadata attributes
    if (length(node$metadata) > 0) {
      entity$metadata <- node$metadata
    }

    # If a parent context exists, establish the hierarchical link
    if (!is.null(parent_id)) {
      parent <- Entity$new(parent_id, con)
      parent$add_relation("contains", entity$id, sort_order = order)
      parent$save()
    }

    # Persist the current node state
    entity$save()

    # Recurse through children if present
    if (length(node$children) > 0) {
      child_ids <- names(node$children)
      for (i in seq_along(child_ids)) {
        restore_node(node$children[[i]], parent_id = entity$id, order = i)
      }
    }
    return(entity$id)
  }

  message("Importing document from: ", file_path)

  # Execute reconstruction process
  root_id <- restore_node(data)

  message("Document successfully imported. Root ID: ", root_id)
  return(invisible(root_id))
}
