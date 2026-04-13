suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

context("Database Schema and Initialization")

test_that("create_schema creates all required tables", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # Run the initialization
  create_schema(con)

  # Check for table existence
  tables <- dbListTables(con)
  expected_tables <- c(
    "ont_classes", "ont_properties", "instances",
    "relations", "attributes", "ont_constraints"
  )

  expect_true(all(expected_tables %in% tables),
              info = paste("Missing tables:", paste(setdiff(expected_tables, tables), collapse = ", ")))
})

test_that("create_schema seeds mandatory classes and properties", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  create_schema(con)

  # Check seeded classes
  res_classes <- dbGetQuery(con, "SELECT class_id FROM ont_classes")
  expected_classes <- c("Entity", "Document", "Section", "MandatorySection", "Conclusion")
  expect_true(all(expected_classes %in% res_classes$class_id))

  # Check seeded properties
  res_props <- dbGetQuery(con, "SELECT property_id FROM ont_properties")
  expected_props <- c("content", "version", "table_data", "plot_path")
  expect_true(all(expected_props %in% res_props$property_id))
})

test_that("create_schema is idempotent (can run multiple times)", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # First run
  expect_error(create_schema(con), NA)

  # Second run should not throw errors due to IF NOT EXISTS and INSERT OR IGNORE
  expect_error(create_schema(con), NA)

  # Verify counts haven't doubled (IGNORE worked)
  count_classes <- dbGetQuery(con, "SELECT COUNT(*) as n FROM ont_classes")$n
  expect_equal(count_classes, 6)
})

test_that("Foreign key constraints are active", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  create_schema(con)

  # In your schema, 'relations' has a FOREIGN KEY on 'subject_id' referencing 'instances'
  # Try to insert a relation for a non-existent instance
  expect_error(
    dbExecute(con, "INSERT INTO relations (subject_id, predicate_id, object_id) VALUES ('NONE', 'test', 'NONE')"),
    "FOREIGN KEY constraint failed"
  )
})

test_that("Attributes table has correct primary key behavior", {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  create_schema(con)

  # Insert a dummy instance to satisfy FK
  dbExecute(con, "INSERT INTO instances (instance_id, class_id) VALUES ('OBJ1', 'Entity')")

  # Insert an attribute
  dbExecute(con, "INSERT INTO attributes (instance_id, property_id, value) VALUES ('OBJ1', 'version', '1.0')")

  # Try to insert the same key for same instance again (should fail PK)
  expect_error(
    dbExecute(con, "INSERT INTO attributes (instance_id, property_id, value) VALUES ('OBJ1', 'version', '2.0')"),
    "UNIQUE constraint failed"
  )
})
