#' Add a Child Entity
#'
#' @description
#' A high-level helper function to create a new entity, establish a
#' relationship with a parent entity (e.g., nesting a section within a document),
#' and persist both objects to the database.
#'
#' @param parent An R6 \code{Entity} (or subclass) object acting as the parent.
#' @param id Character. Unique identifier for the new child entity.
#' @param label Character. Human-readable name for the new child entity.
#' @param class_id Character. Ontology class of the new child. Defaults to "Section".
#' @param predicate Character. The type of relationship. Defaults to "contains".
#'
#' @return The newly created R6 \code{Entity} object.
#' @export
add_child <- function(parent, id, label, class_id = "Section", predicate = "contains") {
  child <- Entity$new(id = id, con = parent$con, label = label, class_id = class_id)
  child$save()

  parent$add_relation(predicate, id)

  parent$save()

  return(child)
}

#' Get Related Entities
#'
#' @description
#' Retrieves a list of R6 \code{Entity} objects that are linked to the
#' provided entity via a specific predicate.
#'
#' @param entity The source R6 \code{Entity} object.
#' @param predicate Character. The relationship type to follow. Defaults to "contains".
#'
#' @return A list of initialized R6 \code{Entity} objects. Returns an empty list
#' if no relations are found.
#' @export
get_related <- function(entity, predicate = "contains") {
  # Extract the relationship data frame for the given predicate
  rel_df <- entity$relations[[predicate]]

  if (is.null(rel_df) || nrow(rel_df) == 0) return(list())

  # Instantiate R6 objects for each related ID
  lapply(rel_df$object_id, function(id) {
    Entity$new(id = id, con = entity$con)
  })
}

#' Build Structure from a Nested List
#'
#' @description
#' Recursively builds a multi-level document hierarchy from a nested R list
#' structure. This is particularly useful for programmatically generating
#' complex document trees (like CTD structures) from configuration files.
#'
#' @param parent The root R6 \code{Entity} object where the structure will be attached.
#' @param structure_list A nested list where names are IDs and elements contain
#' \code{label} and optional \code{children} list.
#'
#' @return Invisible NULL.
#'
#' @examples
#' \dontrun{
#' template <- list(
#'   "SEC_1" = list(label = "Introduction", children = list(
#'     "SUB_1_1" = list(label = "Scope")
#'   ))
#' )
#' build_from_list(doc_object, template)
#' }
#' @export
build_from_list <- function(parent, structure_list) {
  for (id in names(structure_list)) {
    item <- structure_list[[id]]

    # Create the current node and link to parent
    child <- add_child(parent, id = id, label = item$label)

    # If the item defines nested children, recurse deeper into the tree
    if (!is.null(item$children) && is.list(item$children)) {
      build_from_list(child, item$children)
    }
  }
  return(invisible(NULL))
}



#' Print Entity Hierarchy Tree
#'
#' @description
#' A diagnostic utility that prints a visual representation of the
#' document/section hierarchy to the console.
#'
#' @param entity The root R6 \code{Entity} or \code{DocumentInstance} to start from.
#' @param level Integer. Internal parameter for indentation depth.
#'
#' @return Invisible NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' print_tree(my_doc)
#' }
print_tree <- function(entity, level = 0) {
  # Создаем отступ в зависимости от глубины
  indent <- paste(rep("  ", level), collapse = "")

  # Формируем строку: [ID] Label (Class)
  cat(sprintf("%s[%s] %s (Class: %s)\n",
              indent,
              entity$id,
              entity$label,
              entity$class_id))

  # Получаем всех детей через связь 'contains'
  # Мы используем нашу функцию get_related, которая уже есть в этом файле
  children <- get_related(entity, "contains")

  if (length(children) > 0) {
    for (child in children) {
      # Рекурсивный вызов для каждого ребенка
      print_tree(child, level + 1)
    }
  }

  return(invisible(NULL))
}
