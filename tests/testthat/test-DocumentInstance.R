suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))
source("helper-db.R")

context("DocumentInstance Lifecycle and Integrity")

# --- 1. Initialization and Inheritance ---

test_that("DocumentInstance enforces 'Document' class_id", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Even if we pass NULL or something else, initialize should force 'Document'
  doc <- DocumentInstance$new("DOC_INIT", con, label = "Test Doc", class_id = "Entity")

  expect_equal(doc$class_id, "Document")

  doc$save()

  # Verify database record
  res <- dbGetQuery(con, "SELECT class_id FROM instances WHERE instance_id = 'DOC_INIT'")
  expect_equal(res$class_id, "Document")
})

# --- 2. Rendering, Context, and Placeholders ---

test_that("render handles recursive placeholders and context merging", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Setup root document with a variable
  doc <- DocumentInstance$new("DOC_REND", con, "Report for {{drug}}")
  doc$set_prop("drug", "Aspirin")
  doc$set_prop("site", "Global")
  doc$save()

  # Setup child section that overrides a variable and uses an inherited one
  sec <- add_child(doc, "SEC_1", "Analysis at {{site}}")
  sec$set_prop("site", "Local Lab") # Override parent context
  sec$set_prop("content", "Testing {{drug}}...") # Inherit parent drug
  sec$save()

  output <- doc$render()

  # Check parent placeholder resolution
  expect_match(output, "# Report for Aspirin")
  # Check child placeholder with overridden context
  expect_match(output, "## Analysis at Local Lab")
  # Check child placeholder with inherited context
  expect_match(output, "Testing Aspirin...")
})

test_that("render resolves cross-references and nested placeholders", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Target for reference
  prot <- DocumentInstance$new("PROT_01", con, "Master Protocol")
  prot$set_prop("limit", "{{val}}%")
  prot$save()

  # Source document
  doc <- DocumentInstance$new("DOC_REF", con, "Study")
  doc$set_prop("val", "0.5") # Value for the placeholder inside the reference
  doc$set_prop("content", "See [[REF:PROT_01]]. The limit is [[VAL:PROT_01:limit]].")
  doc$save()

  output <- doc$render()

  # Check reference resolution
  expect_match(output, "\\[Master Protocol\\]\\(#sec-PROT_01\\)")
  # Check value resolution AND nested placeholder resolution
  expect_match(output, "The limit is 0.5%")
})

# --- 3. Validation Chain ---

test_that("validate_structure detects missing mandatory sections", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ont <- Ontology$new(con)
  ont$add_class("MandatorySection", parent_class_id = "Section")
  # Constraint: Document must contain MandatorySection
  ont$add_constraint("Document", "MandatorySection")

  doc <- DocumentInstance$new("DOC_STRUCT", con)
  doc$save()

  expect_warning(
    doc$validate_structure(),
    "Structure: Missing mandatory classes: MandatorySection"
  )

  # Add the section
  add_child(doc, "M_SEC", "Required", class_id = "MandatorySection")
  expect_true(doc$validate_structure())
})

test_that("validate_content detects empty sections and recursive issues", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_CONT", con)
  doc$save()

  # 1. Test empty section
  sec <- add_child(doc, "EMPTY", "Empty Section")
  expect_warning(res <- doc$validate_content(), "Section is empty")
  expect_false(res)

  # 2. Fix the section
  sec$set_prop("content", "Now has data")
  sec$save()
  expect_true(doc$validate_content())

  # 3. Test recursive validation of nested documents
  sub_doc <- add_child(doc, "SUB_DOC", "Inner Doc", class_id = "Document")
  sub_doc_obj <- .instantiate_entity("SUB_DOC", con)
  # Force empty state for sub-doc child
  add_child(sub_doc_obj, "SUB_EMPTY", "Inner Empty")

  # We expect at least one warning about emptiness or compliance failure
  expect_warning(
    res <- doc$validate_content(),
    "Section is empty"
  )
  expect_false(res)
})

test_that("validate_consistency detects semantic conflicts", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Define a consistency rule: 'compound' must be consistent
  dbExecute(con, "INSERT INTO ont_consistency_rules VALUES ('compound', 'ERROR')")

  doc <- DocumentInstance$new("DOC_CONS", con)
  doc$set_prop("compound", "Drug-A")
  doc$save()

  # Conflict: child claims a different compound
  sec <- add_child(doc, "SEC_CONS", "Results")
  sec$set_prop("compound", "Drug-B")
  sec$save()

  # Expect both a warning and a FALSE return value
  expect_warning(
    res <- doc$validate_consistency(),
    "CONTRADICTION \\[ERROR\\]: Property 'compound'"
  )
  expect_false(res)
})

test_that("validate_links detects broken internal references", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_LINKS", con)
  doc$set_prop("content", "See [details](#sec-MISSING)")
  doc$save()

  expect_warning(res <- doc$validate_links(), "BROKEN LINK")
  expect_false(res)

  # Fix: create the missing target
  add_child(doc, "MISSING", "Target Section")
  expect_true(doc$validate_links())
})

# --- 4. Signatures and Integrity ---

test_that("sign() creates hashes and locks document", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_SIGN", con)
  doc$set_prop("version", "1.0")
  doc$set_prop("content", "Authentic Content")
  doc$save()

  # 1. Sign as Author
  doc$sign(role = "Author", meaning = "Drafting done")

  sigs <- doc$get_signatures()
  expect_equal(nrow(sigs), 1)
  expect_false(is.null(sigs$content_hash))

  # 2. Sign as Approver and verify lock
  doc$sign(role = "Approver")
  expect_equal(doc$get_prop("status"), "FINAL")

  # Attempt to save after approval should fail
  expect_error(doc$save(), "DATA INTEGRITY ERROR")
})

test_that("verify_signatures detects content tampering", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_INTEG", con)
  doc$set_prop("content", "Original Value")
  doc$save()

  # Sign the original state
  doc$sign(role = "Author")

  # Verify it's valid initially
  expect_true(doc$verify_signatures()$is_valid)

  # Bypass the R6 system and modify database directly (Tampering)
  dbExecute(con, "UPDATE attributes SET value = 'Hacked' WHERE instance_id = 'DOC_INTEG' AND property_id = 'content'")

  # Reload and verify
  doc_reloaded <- .instantiate_entity("DOC_INTEG", con)
  expect_warning(res <- doc_reloaded$verify_signatures(), "DATA INTEGRITY BREACH")
  expect_false(res$is_valid)
})

# --- 5. Snapshots and Versioning ---

test_that("bump_version enforces minor/major logic", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_VER", con)
  doc$set_prop("version", "1.5")

  expect_equal(doc$bump_version("minor"), "1.6")
  expect_equal(doc$bump_version("major"), "2.0")
})

test_that("compare_with_snapshot detects differences against JSON", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_SNAP", con, "Version 1")
  doc$save()

  # Create snapshot
  tmp_file <- tempfile(fileext = ".json")
  export_to_json(doc, tmp_file)

  # Modify current doc
  doc$label <- "Version 2"
  doc$save()

  diffs <- doc$compare_with_snapshot(tmp_file)
  expect_equal(nrow(diffs), 1)
  expect_equal(diffs$key, "label")
  expect_equal(diffs$status, "Changed")
})

