suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

# Helper function to set up an in-memory database
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("Database Query and Traceability Functions")

test_that("find_instances filters by class and pattern correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Setup test data
  # Document
  doc1 <- DocumentInstance$new("DOC_01", con, "Validation Protocol", "Document")
  doc1$save()

  # Entity (Section)
  sec1 <- Entity$new("SEC_01", con, "Introduction Section", "Section")
  sec1$save()

  # 2. Test filtering by class_id
  docs <- find_instances(con, class_id = "Document")
  expect_length(docs, 1)
  expect_s3_class(docs[[1]], "DocumentInstance") # Verify class factory logic
  expect_equal(docs[[1]]$id, "DOC_01")

  # 3. Test filtering by pattern (LIKE)
  found_pattern <- find_instances(con, pattern = "Intro")
  expect_length(found_pattern, 1)
  expect_equal(found_pattern[[1]]$id, "SEC_01")
  expect_s3_class(found_pattern[[1]], "Entity") # Verify it is NOT a DocumentInstance

  # 4. Test combined filter
  none <- find_instances(con, class_id = "Document", pattern = "Intro")
  expect_length(none, 0)
})

test_that("find_by_attribute finds entities based on EAV metadata", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Setup metadata
  e1 <- Entity$new("E1", con)$set_prop("status", "Draft")$save()
  e2 <- Entity$new("E2", con)$set_prop("status", "Approved")$save()
  e3 <- Entity$new("E3", con)$set_prop("status", "Draft")$save()

  # 2. Query
  drafts <- find_by_attribute(con, "status", "Draft")

  expect_length(drafts, 2)
  ids <- sapply(drafts, function(x) x$id)
  expect_true(all(c("E1", "E3") %in% ids))

  # 3. Non-existent value
  expect_length(find_by_attribute(con, "status", "Archived"), 0)
})

test_that("where_used provides correct traceability", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Setup relations (Parent contains Child)
  parent <- Entity$new("P1", con)$save()
  child  <- Entity$new("C1", con)$save()

  parent$add_relation("contains", "C1")$save()

  # 2. Query: Where is C1 used?
  usage <- where_used(con, "C1")

  expect_s3_class(usage, "data.frame")
  expect_equal(nrow(usage), 1)
  expect_equal(usage$subject_id, "P1")
  expect_equal(usage$predicate_id, "contains")

  # 3. Unreferenced object
  expect_null(where_used(con, "UNKNOWN"))
})

test_that("search_content performs keyword search in attributes", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Setup content
  Entity$new("SEC_A", con, "Section A")$set_prop("content", "Stability testing results")$save()
  Entity$new("SEC_B", con, "Section B")$set_prop("content", "Method validation")$save()

  # 2. Search for keyword
  results <- search_content(con, "stability")

  expect_equal(nrow(results), 1)
  expect_equal(results$instance_id, "SEC_A")
  expect_equal(results$label, "Section A")
  expect_match(results$content, "Stability")

  # 3. Case-insensitive check (SQLite LIKE is usually case-insensitive)
  results_upper <- search_content(con, "STABILITY")
  expect_equal(nrow(results_upper), 1)
})
