suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))
source("helper-db.R")

context("Comprehensive Diffing and Hierarchy Comparison")

# --- Tests for diff_entities (Flat Comparison) ---

test_that("diff_entities returns an empty data frame for identical entities", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  e1 <- Entity$new("E1", con, label = "Test")
  e1$set_prop("key1", "val1")
  e1$save()

  # Compare e1 with itself
  res <- diff_entities(e1, e1)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})

test_that("diff_entities detects label changes and respects class defaults", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  # OLD: Explicit title
  e_old <- Entity$new("E", con1, label = "Old Title")

  # NEW: Pass NULL, but the Class will convert it to "Untitled"
  e_new <- Entity$new("E", con2, label = NULL)

  res <- diff_entities(e_old, e_new)

  row <- res[res$key == "label", ]
  expect_equal(row$status, "Changed")
  expect_equal(row$old_value, "Old Title")

  # The class constructor forces NULL to "Untitled"
  expect_equal(row$new_value, "Untitled")

  # The path will be "Untitled" because that is the current label
  expect_equal(row$path, "Untitled")
})

test_that("diff_entities handles unlabelled entities gracefully using fallbacks", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  # To truly test the "Unlabeled" or "NULL" fallback in diff_entities,
  # we must bypass the R6 constructor by manually setting the field to NULL
  e_old <- Entity$new("E1", con1, label = "Something")
  e_new <- Entity$new("E1", con2, label = "Something else")

  # Force null to test diff_entities logic
  e_new$label <- NULL

  res <- diff_entities(e_old, e_new)

  # Now the diff_entities logic will see a true NULL and use the fallback
  expect_equal(res[res$key == "label", ]$new_value, "NULL")
  expect_equal(res$path[1], "Unlabeled")
})

test_that("diff_entities handles metadata Added, Removed, and Changed states", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  e_old <- Entity$new("M1", con1)
  e_old$set_prop("p_modify", "old_val")
  e_old$set_prop("p_remove", "gone")
  e_old$save()

  e_new <- Entity$new("M1", con2)
  e_new$set_prop("p_modify", "new_val") # Changed
  e_new$set_prop("p_add", "fresh")       # Added
  # p_remove is missing                # Removed
  e_new$save()

  res <- diff_entities(e_old, e_new)

  expect_equal(res[res$key == "p_modify", ]$status, "Changed")
  expect_equal(res[res$key == "p_add", ]$status, "Added")
  expect_equal(res[res$key == "p_add", ]$old_value, "NULL")
  expect_equal(res[res$key == "p_remove", ]$status, "Removed")
  expect_equal(res[res$key == "p_remove", ]$new_value, "NULL")
})

test_that("diff_entities specifically handles NA vs NULL vs Values", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  e_old <- Entity$new("NA_TEST", con1)
  e_old$set_prop("prop_na", NA)
  e_old$set_prop("prop_val", "SomeData")
  e_old$save()

  e_new <- Entity$new("NA_TEST", con2)
  e_new$set_prop("prop_na", "NowAValue") # NA -> Value
  e_new$set_prop("prop_val", NA)         # Value -> NA
  e_new$save()

  res <- diff_entities(e_old, e_new)

  # Check NA -> Value transition
  row_na_to_val <- res[res$key == "prop_na", ]
  expect_equal(row_na_to_val$status, "Changed")
  expect_equal(row_na_to_val$old_value, "NA")

  # Check Value -> NA transition
  row_val_to_na <- res[res$key == "prop_val", ]
  expect_equal(row_val_to_na$status, "Changed")
  expect_equal(row_val_to_na$new_value, "NA")
})

test_that("diff_entities detects structural changes (children)", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  e_old <- Entity$new("P", con1)
  e_old$add_relation("contains", "CHILD_A")
  e_old$save()

  e_new <- Entity$new("P", con2)
  e_new$add_relation("contains", "CHILD_B") # Removed A, Added B
  e_new$save()

  res <- diff_entities(e_old, e_new)

  struct_changes <- res[res$component == "Structure", ]
  expect_equal(nrow(struct_changes), 2)
  expect_true("Added" %in% struct_changes$status)
  expect_true("Removed" %in% struct_changes$status)
})

# --- Tests for diff_hierarchy (Recursive Comparison) ---

test_that("diff_hierarchy tracks deep paths and nested metadata changes", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  # Build Hierarchy: Root -> Sec -> SubSec
  # OLD version
  r_old <- DocumentInstance$new("ROOT", con1, "Root")
  r_old$save()
  s_old <- add_child(r_old, "SEC", "Section")
  ss_old <- add_child(s_old, "SUB", "SubSection")
  ss_old$set_prop("content", "Original Text")
  ss_old$save()

  # NEW version
  r_new <- DocumentInstance$new("ROOT", con2, "Root")
  r_new$save()
  s_new <- add_child(r_new, "SEC", "Section")
  ss_new <- add_child(s_new, "SUB", "SubSection")
  ss_new$set_prop("content", "Modified Text") # Deep change
  ss_new$save()

  # Run recursive diff
  res <- diff_hierarchy(r_old, r_new)

  # Target the deep change
  deep_change <- res[res$key == "content", ]
  expect_equal(nrow(deep_change), 1)
  expect_equal(deep_change$status, "Changed")

  # CRITICAL: Verify path breadcrumbs
  expect_equal(deep_change$path, "Root > Section > SubSection")
})

test_that("diff_hierarchy handles branching common children", {
  con1 <- setup_test_db()
  con2 <- setup_test_db()
  on.exit({
    dbDisconnect(con1)
    dbDisconnect(con2)
  })

  r_old <- DocumentInstance$new("R", con1, "Root")
  add_child(r_old, "C1", "Child1")
  add_child(r_old, "C2", "Child2")
  r_old$save()

  r_new <- DocumentInstance$new("R", con2, "Root")
  c1_new <- add_child(r_new, "C1", "Child1")
  c1_new$set_prop("note", "changed") # Change only in branch 1
  add_child(r_new, "C2", "Child2")   # Branch 2 remains identical
  r_new$save()
  c1_new$save()

  res <- diff_hierarchy(r_old, r_new)

  # Only 1 change should be found in total
  expect_equal(nrow(res), 1)
  expect_equal(res$path, "Root > Child1")
})

# --- Tests for render_diff_markdown (Formatting) ---

test_that("render_diff_markdown produces expected strings and handles empty data", {
  # Mock diff data
  mock_diff <- data.frame(
    path = "Root",
    component = "Metadata",
    key = "version",
    old_value = "1.0",
    new_value = "2.0",
    status = "Changed",
    stringsAsFactors = FALSE
  )

  # Add an 'Added' row
  mock_diff <- rbind(mock_diff, data.frame(
    path = "Root", component = "Metadata", key = "author",
    old_value = "NULL", new_value = "Admin", status = "Added",
    stringsAsFactors = FALSE
  ))

  md <- render_diff_markdown(mock_diff)

  expect_true(grepl("### Change Log", md))
  # Check strike-through for Changed
  expect_true(grepl("~~1.0~~ -> \\*\\*2.0\\*\\*", md))
  # Check bold for Added
  expect_true(grepl("Added \\*\\*Admin\\*\\*", md))

  # Check empty case
  expect_equal(render_diff_markdown(data.frame()), "No changes detected.")
})

test_that("diff_hierarchy handles missing connections gracefully", {
  # Logic check: ensures instantiate_entity works with the provided con
  con_old <- setup_test_db()
  con_new <- setup_test_db()
  on.exit({
    dbDisconnect(con_old)
    dbDisconnect(con_new)
  })

  e_old <- Entity$new("E1", con_old, label = "Title")
  e_old$save()
  e_new <- Entity$new("E1", con_new, label = "Title")
  e_new$save()

  # Even if connections are distinct, if data is same, result is empty
  res <- diff_hierarchy(e_old, e_new)
  expect_equal(nrow(res), 0)
})

