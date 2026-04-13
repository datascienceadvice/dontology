suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

# Helper function to set up an in-memory database for each test
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con) # Using the new schema creation function
  return(con)
}

context("DocumentInstance Logic and Validation")

test_that("DocumentInstance works in memory", {
  con <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  # This now contains "Entity", "Document", "Section", and "version"
  create_schema(con)

  doc <- DocumentInstance$new("DOC_1", con, "Test Report", "Document")
  doc$set_prop("version", "1.0")
  doc$save() # This will now work because 'Document' and 'version' exist in schema

  # add_child uses 'Section' by default, which is also in our seeds
  s1 <- add_child(doc, "SEC_1", "Introduction")

  expect_equal(dbGetQuery(con, "SELECT count(*) as n FROM instances")$n, 2)
})

test_that("Basic lifecycle and metadata rendering works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Initialize document
  doc <- DocumentInstance$new("DOC_001", con, label = "Stability Report", class_id = "Document")
  doc$set_prop("version", "v2.1.0")
  doc$save()

  # Test basic rendering of title and version
  output <- doc$render()

  expect_true(grepl("# Stability Report", output))
  expect_true(grepl("Version: v2.1.0", output))

  # Verify data persistence in SQLite
  res <- dbGetQuery(con, "SELECT label FROM instances WHERE instance_id = 'DOC_001'")
  expect_equal(res$label, "Stability Report")
})

test_that("Hierarchical numbering and deep recursion works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Create and save ROOT node
  doc <- DocumentInstance$new("DOC_ROOT", con, "Master Document", class_id = "Document")
  doc$save() # Обязательно сохраняем, чтобы он появился в базе

  # 2. Add children (add_child handles saving)
  s1 <- add_child(doc, "SEC_1", "Introduction")
  s1_1 <- add_child(s1, "SEC_1_1", "Scope")
  s1_1_1 <- add_child(s1_1, "SEC_1_1_1", "Details")

  # 3. Save content of the deepest node
  s1_1_1$set_prop("content", "Technical details here.")
  s1_1_1$save()

  rels <- DBI::dbGetQuery(con, "SELECT * FROM relations")
  message("Total relations in DB: ", nrow(rels))

  output <- doc$render()

  expect_true(grepl("## Introduction", output))
  expect_true(grepl("#### Details", output))
})

test_that("Table rendering from JSON works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_TBL", con, "Table Test")
  s1 <- add_child(doc, "SEC_TBL", "Results Table")

  # Prepare dummy data frame
  results <- data.frame(
    Parameter = c("Assay", "pH"),
    Result = c("99.5%", "7.2"),
    stringsAsFactors = FALSE
  )

  s1$set_table("table_data", results)
  s1$save()

  output <- doc$render()

  # Verify Markdown Pipe Table structure
  expect_true(grepl("\\|Parameter *\\|Result", output))
  expect_true(grepl("Assay", output))
  expect_true(grepl("99.5%", output))
})

test_that("Validation: Missing mandatory sections", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Setup Ontology Constraints
  ont <- Ontology$new(con)
  ont$add_class("MandatorySection", parent_class_id = "Section")

  # Define rule: 'Document' must contain at least one 'MandatorySection'
  ont$add_constraint("Document", "MandatorySection")

  doc <- DocumentInstance$new("DOC_VAL", con, "Validation Test", class_id = "Document")
  doc$save()

  # Should fail and throw a warning about missing section
  expect_warning(val_result <- doc$validate(), "Missing mandatory sections")
  expect_false(val_result)

  # Fix: Add the missing mandatory section
  m_sec <- add_child(doc, "M_01", "Required Content", class_id = "MandatorySection")
  m_sec$set_prop("content", "Fixed content.")
  m_sec$save()

  # Should now pass
  expect_true(doc$validate())
})

test_that("Validation: Empty section check", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_EMPTY", con, "Empty Check")

  # Add a child but provide NO content, NO table, and NO sub-children
  add_child(doc, "EMPTY_SEC", "Empty Section")

  # Should fail validation due to empty content
  expect_warning(val_result <- doc$validate(), "is empty")
  expect_false(val_result)

  # Fix: Add content
  sec <- Entity$new("EMPTY_SEC", con)
  sec$set_prop("content", "Now I have data.")
  sec$save()

  expect_true(doc$validate())
})

test_that("Cross-referencing and internal links work", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("REF_DOC", con, "Reference Test")
  doc$save()

  # Create Target Section
  # The ID is "TARGET", but the code will render it as {#sec-TARGET}
  s_target <- add_child(doc, "TARGET", "Target Section")
  s_target$set_prop("content", "I am the target.")
  s_target$save()

  # Create Source Section with a link pointing to the new prefixed anchor
  s_source <- add_child(doc, "SOURCE", "Source Section")
  s_source$set_prop("content", "See [Target Section](#sec-TARGET)")
  s_source$save()

  output <- doc$render()

  # 1. Verify that the anchor exists with the 'sec-' prefix
  expect_true(grepl("\\{#sec-TARGET\\}", output),
              info = "Markdown anchor should have 'sec-' prefix")

  # 2. Verify that the link exists and matches the prefixed anchor
  expect_true(grepl("\\[Target Section\\]\\(#sec-TARGET\\)", output),
              info = "Internal link should match the 'sec-' prefixed anchor")
})
