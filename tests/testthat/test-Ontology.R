suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

# Helper function to set up an in-memory database for each test
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("Ontology Meta-Model Management")

test_that("Ontology can add and retrieve classes", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Add a new root class
  ont$add_class("StabilityStudy", description = "Standard stability study")

  # Add a child class
  ont$add_class("AcceleratedStudy", parent_class_id = "StabilityStudy")

  # Retrieve classes
  classes <- ont$get_classes()

  expect_true("StabilityStudy" %in% classes$class_id)
  expect_true("AcceleratedStudy" %in% classes$class_id)

  # Check parent-child relation
  child_row <- classes[classes$class_id == "AcceleratedStudy", ]
  expect_equal(child_row$parent_class_id, "StabilityStudy")
})

test_that("Ontology handles NULL parent_class_id by converting to NA", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Explicitly pass NULL to test the internal conversion logic
  ont$add_class("RootClass", parent_class_id = NULL)

  classes <- ont$get_classes()
  row <- classes[classes$class_id == "RootClass", ]

  # SQLite driver requires NA to represent SQL NULL in a parameter list
  expect_true(is.na(row$parent_class_id))
})

test_that("Ontology can manage properties", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Add new properties
  ont$add_property("batch_number", value_type = "text", description = "Lot identifier")
  ont$add_property("potency", value_type = "numeric")

  props <- ont$get_properties()

  expect_true("batch_number" %in% props$property_id)
  expect_equal(props[props$property_id == "potency", ]$value_type, "numeric")
})

test_that("Ontology can define structural constraints", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Define a constraint: A 'Specification' document must have an 'Analysis' section
  ont$add_constraint("Specification", "Analysis")

  # Verify database storage
  res <- dbGetQuery(con, "SELECT * FROM ont_constraints WHERE parent_class_id = 'Specification'")

  expect_equal(nrow(res), 1)
  expect_equal(res$required_child_class_id, "Analysis")
})

test_that("Ontology inspection helpers work correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Standard classes from create_schema seeds
  expect_true(ont$class_exists("Document"))
  expect_true(ont$class_exists("Section"))

  # Non-existent class
  expect_false(ont$class_exists("NonExistentClass"))
})

test_that("Ontology returns a sorted class hierarchy", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Add hierarchy
  ont$add_class("Level_1", parent_class_id = NA)
  ont$add_class("Level_2", parent_class_id = "Level_1")

  hierarchy <- ont$get_class_hierarchy()

  # 1. Check if Level_1 is among the root classes (where parent is NA)
  root_classes <- hierarchy$class_id[is.na(hierarchy$parent_class_id)]
  expect_true("Level_1" %in% root_classes)

  # 2. Check if Level_2 is present and has the correct parent
  child_row <- hierarchy[hierarchy$class_id == "Level_2", ]
  expect_equal(nrow(child_row), 1)
  expect_equal(child_row$parent_class_id, "Level_1")

  # 3. Check if all standard seeded classes are also present
  expect_true("Entity" %in% hierarchy$class_id)
  expect_true("Document" %in% hierarchy$class_id)
})

test_that("Ontology add_class uses REPLACE logic", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)

  # Add initial
  ont$add_class("TestClass", description = "Version 1")

  # Update same ID
  ont$add_class("TestClass", description = "Version 2")

  classes <- ont$get_classes()
  row <- classes[classes$class_id == "TestClass", ]

  expect_equal(nrow(classes[classes$class_id == "TestClass", ]), 1)
  expect_equal(row$description, "Version 2")
})
