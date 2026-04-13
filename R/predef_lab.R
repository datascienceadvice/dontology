#' Initialize the Standard Phrases Library
#'
#' @description
#' Creates a standardized repository of reusable text clauses (Standard Clauses)
#' in the database. This allows common pharmaceutical formulations to be managed
#' centrally and referenced across multiple documents.
#'
#' @param con DBIConnection. An active connection to the SQLite database.
#'
#' @return Invisible NULL.
#' @export
init_standard_library <- function(con) {
  # 1. Register the StandardClause class in the ontology
  ont <- Ontology$new(con)
  ont$add_class("StandardClause", description = "Reusable standardized text clause")

  # 2. Create the library container
  lib <- Entity$new("LIB_STD", con, "Standard Phrases Library", "Document")
  lib$save()

  # 3. Add Cold storage condition clause
  clause1 <- Entity$new("STP_STORAGE_COLD", con, "Storage Conditions (Cold)", "StandardClause")
  clause1$set_prop("content", "Store protected from light at a temperature between 2 and 8 C. Do not freeze.")
  clause1$save()
  lib$add_relation("contains", "STP_STORAGE_COLD", sort_order = 1)

  # 4. Add Room temperature storage condition clause
  clause2 <- Entity$new("STP_STORAGE_ROOM", con, "Storage Conditions (Room Temp)", "StandardClause")
  clause2$set_prop("content", "Store at a temperature not exceeding 25 C.")
  clause2$save()
  lib$add_relation("contains", "STP_STORAGE_ROOM", sort_order = 2)

  # Save the library with updated relations
  lib$save()

  message("Standard phrases library has been initialized successfully.")
  return(invisible(NULL))
}
