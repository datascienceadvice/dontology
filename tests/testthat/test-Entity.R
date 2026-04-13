suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

# Helper function to set up an in-memory database for each test
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con) # Using the schema creation function we defined
  return(con)
}

context("Entity Class Core Logic")

test_that("Entity basic properties and metadata persistence works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Initialize and set properties
  ent <- Entity$new("ENT_01", con, label = "Original Label", class_id = "Section")
  ent$set_prop("status", "Active")
  ent$set_prop("version", "1.0")

  # Check in-memory state
  expect_equal(ent$get_prop("status"), "Active")

  # 2. Save to database
  ent$save()

  # 3. Create a NEW R6 object with same ID to test loading
  ent_loaded <- Entity$new("ENT_01", con)

  expect_equal(ent_loaded$label, "Original Label")
  expect_equal(ent_loaded$class_id, "Section")
  expect_equal(ent_loaded$get_prop("version"), "1.0")
  expect_equal(ent_loaded$get_prop("status"), "Active")
})

test_that("Entity table serialization (JSON) works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("TBL_01", con)

  # Create a dummy data frame
  test_df <- data.frame(
    Analyte = c("Impurity A", "Impurity B"),
    Limit = c(0.15, 0.10),
    stringsAsFactors = FALSE
  )

  # Set and save
  ent$set_table("test_table", test_df)
  ent$save()

  # Load in a fresh object
  ent_loaded <- Entity$new("TBL_01", con)
  loaded_df <- ent_loaded$get_table("test_table")

  expect_s3_class(loaded_df, "data.frame")
  expect_equal(nrow(loaded_df), 2)
  expect_equal(loaded_df$Analyte[1], "Impurity A")
  expect_equal(loaded_df$Limit[2], 0.10)
})

test_that("Entity relations management works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Create two entities
  parent <- Entity$new("PARENT", con, class_id = "Document")
  child1 <- Entity$new("CHILD_1", con, class_id = "Section")
  child2 <- Entity$new("CHILD_2", con, class_id = "Section")

  child1$save()
  child2$save()

  # Add relations with sorting
  parent$add_relation("contains", "CHILD_1", sort_order = 1)
  parent$add_relation("contains", "CHILD_2", sort_order = 2)
  parent$add_relation("references", "EXT_DOC_99", sort_order = 0)

  parent$save()

  # Load in fresh object
  parent_loaded <- Entity$new("PARENT", con)

  # Check if relations are correctly split by predicate and sorted
  expect_length(parent_loaded$relations, 2) # contains and references
  expect_equal(parent_loaded$relations$contains$object_id[1], "CHILD_1")
  expect_equal(parent_loaded$relations$contains$object_id[2], "CHILD_2")
  expect_equal(parent_loaded$relations$references$object_id[1], "EXT_DOC_99")
})

test_that("Entity plot path handling works with dynamic naming and cleanup", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Setup: Use a unique ID for the entity
  ent_id <- "PLOT_TEST_01"
  ent <- Entity$new(ent_id, con)

  # 2. Check for ggplot2 availability
  skip_if_not_installed("ggplot2")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = "Test Plot")

  # 3. Action: Save the plot
  # Note: If you updated set_plot to use timestamps, the filename will be dynamic
  ent$set_plot(p)
  ent$save()

  # 4. Verification
  plot_path <- ent$get_prop("plot_path")

  # Check if metadata was actually saved
  expect_false(is.null(plot_path), info = "plot_path metadata is missing")

  # Check if the file exists on disk
  expect_true(file.exists(plot_path), info = paste("File does not exist at:", plot_path))

  # Check if the filename contains the entity ID and a timestamp-like pattern
  # Matches: PLOT_TEST_01 followed by _ and digits, ending in .png
  filename <- basename(plot_path)
  expect_match(filename, paste0("^", ent_id, "_[0-9]+\\.png$"),
               all = FALSE,
               info = "Filename does not match the expected pattern {ID}_{timestamp}.png")

  # 5. Cleanup
  # Delete the specific file created by this test
  if (file.exists(plot_path)) unlink(plot_path)

  # Only remove the directory if it's empty to avoid breaking other parallel tests
  plot_dir <- dirname(plot_path)
  if (dir.exists(plot_dir) && length(list.files(plot_dir)) == 0) {
    unlink(plot_dir, recursive = TRUE)
  }
})

test_that("Entity update logic (Overwrite) works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("UPD_01", con, label = "Old Label")
  ent$set_prop("temp", "Old Value")
  ent$save()

  # Update properties
  ent$label <- "New Label"
  ent$set_prop("temp", "New Value")
  ent$save()

  # Verify database only has the new values (no duplicates in EAV)
  res_attr <- dbGetQuery(con, "SELECT count(*) as n FROM attributes WHERE instance_id = 'UPD_01'")
  expect_equal(res_attr$n, 1)

  ent_loaded <- Entity$new("UPD_01", con)
  expect_equal(ent_loaded$label, "New Label")
  expect_equal(ent_loaded$get_prop("temp"), "New Value")
})

test_that("get_table returns NULL if property is missing", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("MISSING_TBL", con)
  # property "non_existent" has never been set
  expect_null(ent$get_table("non_existent"))
})

test_that("save() handles attribute values that are NA or NULL", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("NULL_ATTR", con)
  ent$set_prop("empty_key", NA)
  ent$set_prop("null_key", NULL)
  ent$save()

  # Verify they are stored as empty strings in the DB
  res <- dbGetQuery(con, "SELECT value FROM attributes WHERE instance_id = 'NULL_ATTR' AND property_id = 'empty_key'")
  expect_equal(res$value, "")
})

test_that("save() handles unknown user via environment override", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("USER_TEST", con)

  Sys.setenv("DONTOLOGY_USER" = "")

  ent$save()

  Sys.unsetenv("DONTOLOGY_USER")

  res <- dbGetQuery(con, "SELECT updated_by FROM instances WHERE instance_id = 'USER_TEST'")
  expect_equal(res$updated_by, Sys.info()[["user"]])
})

test_that("save() throws custom error message on DB failure", {
  con <- setup_test_db()
  # We do NOT use on.exit(dbDisconnect(con)) here because we want to close it manually

  ent <- Entity$new("ERR_TEST", con)

  # Manually close connection to force a DBI error during save
  dbDisconnect(con)

  # Should trigger the catch block and line 223
  expect_error(ent$save(), "FAIL TO SAVE \\[ERR_TEST\\]")
})

test_that("initialize() handles missing label and class_id correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Case: id exists in DB, but we pass NULLs to initialize
  dbExecute(con, "INSERT INTO instances (instance_id, class_id, label) VALUES ('EXISTS', 'Section', 'Existing')")

  ent <- Entity$new("EXISTS", con, label = NULL, class_id = NULL)
  expect_equal(ent$label, "Existing")
  expect_equal(ent$class_id, "Section")

  # Case: id does NOT exist and we pass NULLs (Triggering lines 48-49)
  ent_new <- Entity$new("NEW_ID", con, label = NULL, class_id = NULL)
  expect_equal(ent_new$label, "Untitled")
  expect_equal(ent_new$class_id, "Entity")
})
