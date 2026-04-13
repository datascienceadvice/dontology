suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("Predefined Library Initialization")

test_that("init_standard_library populates the database correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Run initialization
  init_standard_library(con)

  # 1. Verify Ontology Class
  ont <- Ontology$new(con)
  expect_true(ont$class_exists("StandardClause"))

  # 2. Verify Library Instance
  lib <- Entity$new("LIB_STD", con)
  expect_equal(lib$label, "Standard Phrases Library")

  # 3. Verify specific clauses exist and have content
  cold_clause <- Entity$new("STP_STORAGE_COLD", con)
  expect_equal(cold_clause$class_id, "StandardClause")
  expect_match(cold_clause$get_prop("content"), "2 and 8 C")

  # 4. Verify relations (links)
  # Check if LIB_STD contains the clauses
  rel_query <- "SELECT object_id FROM relations WHERE subject_id = 'LIB_STD' AND predicate_id = 'contains'"
  rels <- dbGetQuery(con, rel_query)

  expect_true("STP_STORAGE_COLD" %in% rels$object_id)
  expect_true("STP_STORAGE_ROOM" %in% rels$object_id)
})

test_that("Standard clauses can be linked to new documents", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  init_standard_library(con)

  # Create a new Specification document
  spec <- DocumentInstance$new("SPEC_001", con, "Product Specification")

  # Link an existing standard clause from the library to this new spec
  spec$add_relation("contains", "STP_STORAGE_COLD")
  spec$save()

  # Render to check if content is pulled correctly
  output <- spec$render()
  expect_true(grepl("Store protected from light", output))
})
