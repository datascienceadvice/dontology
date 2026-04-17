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
  query <- "SELECT instance_id FROM instances WHERE 1=1"
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

  # Используем фабрику для каждого найденного ID
  lapply(res$instance_id, function(id) {
    .instantiate_entity(id, con)
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

#' Semantic Search by Class
#' @description Finds instances of a class and all its subclasses.
#' @export
find_by_class_deep <- function(con, target_class) {
  # 1. Сначала найдем все подклассы (рекурсивно)
  get_subclasses <- function(cls) {
    query <- "SELECT class_id FROM ont_classes WHERE parent_class_id = ?"
    sub <- DBI::dbGetQuery(con, query, params = list(cls))$class_id
    if (length(sub) == 0) return(cls)
    return(c(cls, unlist(lapply(sub, get_subclasses))))
  }

  all_relevant_classes <- unique(get_subclasses(target_class))

  # 2. Ищем все инстанции этих классов
  sql <- sprintf(
    "SELECT instance_id, class_id, label FROM instances WHERE class_id IN (%s)",
    paste(DBI::dbQuoteString(con, all_relevant_classes), collapse = ",")
  )

  res <- DBI::dbGetQuery(con, sql)
  return(res)
}

#' Find Orphan Entities
#' @description Finds entities that are not contained by any other entity.
#' @export
find_orphans <- function(con) {
  query <- "
    SELECT instance_id, label, class_id
    FROM instances
    WHERE instance_id NOT IN (
      SELECT object_id FROM relations WHERE predicate_id = 'contains'
    )
    AND class_id != 'Document'" # Документы сами по себе корни, они не сироты

  return(DBI::dbGetQuery(con, query))
}

#' Semantic Attribute Search (with Inheritance)
#' @description Finds entities where a property matches a value,
#' either directly or via inheritance from a parent.
#' @export
find_semantic_attribute <- function(con, property_id, value) {
  # 1. Находим все сущности, у которых этот атрибут прописан явно
  query <- "SELECT instance_id FROM attributes WHERE property_id = ? AND value = ?"
  direct_hits <- DBI::dbGetQuery(con, query, params = list(property_id, value))$instance_id

  if (length(direct_hits) == 0) return(NULL)

  # 2. Для каждой найденной сущности найдем всех её "потомков"
  # (т.к. они наследуют это свойство семантически)
  get_all_descendants <- function(parent_id) {
    q <- "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains'"
    children <- DBI::dbGetQuery(con, q, params = list(parent_id))$object_id
    if (length(children) == 0) return(parent_id)
    return(c(parent_id, unlist(lapply(children, get_all_descendants))))
  }

  all_affected_ids <- unique(unlist(lapply(direct_hits, get_all_descendants)))

  # Возвращаем информацию об этих объектах
  sql <- sprintf("SELECT instance_id, label, class_id FROM instances WHERE instance_id IN (%s)",
                 paste(DBI::dbQuoteString(con, all_affected_ids), collapse = ","))
  return(DBI::dbGetQuery(con, sql))
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

