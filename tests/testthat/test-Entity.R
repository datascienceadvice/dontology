suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))
source("helper-db.R")

context("Entity Advanced Ontological and Integrity Logic")

# --- Helper for checking audit log content ---
get_last_audit_delta <- function(con, id) {
  res <- dbGetQuery(con, "SELECT delta FROM audit_log WHERE instance_id = ? ORDER BY log_id DESC LIMIT 1", params = list(id))
  if (nrow(res) == 0) return(NULL)
  return(jsonlite::fromJSON(res$delta))
}

test_that("Ontological relation constraints (add_relation) work", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Register the new class 'User' in the ontology first!
  # This satisfies the Foreign Key constraint on the instances table.
  dbExecute(con, "INSERT INTO ont_classes (class_id) VALUES ('User')")

  # 2. Setup rules: 'Document' can only contain 'Section'
  dbExecute(con, "INSERT INTO ont_relation_rules VALUES ('Document', 'contains', 'Section')")

  doc <- Entity$new("DOC_1", con, class_id = "Document")
  sec <- Entity$new("SEC_1", con, class_id = "Section")
  other <- Entity$new("WRONG", con, class_id = "User") # Now 'User' is a valid class

  # Now save() will work
  sec$save()
  other$save()

  # Valid relation: Document contains Section (Matches rule)
  expect_error(doc$add_relation("contains", "SEC_1"), NA)

  # Invalid relation: Document contains User (Violates rule: expected Section)
  expect_error(doc$add_relation("contains", "WRONG"), "Ontology Violation")
})

test_that("Circular dependency detection (is_descendant_of) works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Setup: A -> B -> C
  ent_a <- Entity$new("A", con)$save()
  ent_b <- Entity$new("B", con)$save()
  ent_c <- Entity$new("C", con)$save()

  ent_a$add_relation("contains", "B")$save()
  ent_b$add_relation("contains", "C")$save()

  # Check ancestry
  expect_true(ent_c$is_descendant_of("A")) # A is ancestor of C
  expect_true(ent_c$is_descendant_of("B")) # B is ancestor of C
  expect_false(ent_a$is_descendant_of("C")) # C is NOT ancestor of A

  # Attempt to create cycle: C -> A
  expect_error(ent_c$add_relation("contains", "A"), "Circular reference detected")
})

test_that("validate_ontology_types handles unknown types correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Register a property with a type not handled by our switch ('blob')
  dbExecute(con, "INSERT INTO ont_properties (property_id, value_type) VALUES ('weird', 'blob')")

  ent <- Entity$new("T2", con)
  ent$set_prop("weird", "some data")

  # We expect a warning about Type Mismatch and a FALSE return value
  expect_warning(
    res <- ent$validate_ontology_types(),
    "expects blob"
  )
  expect_false(res)
})

test_that("is_descendant_of returns FALSE for root entities", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("ROOT", con)$save()
  expect_false(ent$is_descendant_of("ANYBODY"))
})

test_that("Property inheritance and context merging work", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Setup hierarchy: Parent -> Child
  parent <- Entity$new("P", con)
  parent$set_prop("drug", "Insulin")
  parent$set_prop("site", "Factory A")
  parent$save()

  child <- Entity$new("C", con)
  child$set_prop("site", "Factory B") # Override parent prop
  child$save()

  parent$add_relation("contains", "C")$save()

  # Test get_prop_inherited
  expect_equal(child$get_prop_inherited("drug"), "Insulin") # From parent
  expect_equal(child$get_prop_inherited("site"), "Factory B") # Local

  # Test get_context
  ctx <- child$get_context()
  expect_equal(ctx$drug, "Insulin")
  expect_equal(ctx$site, "Factory B")
})

test_that("Ontology type validation (validate_ontology_types) works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  dbExecute(con, "INSERT INTO ont_properties (property_id, value_type) VALUES ('age', 'numeric')")
  dbExecute(con, "INSERT INTO ont_properties (property_id, value_type) VALUES ('is_valid', 'boolean')")
  dbExecute(con, "INSERT INTO ont_properties (property_id, value_type) VALUES ('exp_date', 'date')")

  ent <- Entity$new("T1", con)

  # Case 1: Valid
  ent$set_prop("age", "25")$set_prop("is_valid", "TRUE")$set_prop("exp_date", "2024-12-31")
  expect_true(ent$validate_ontology_types())

  # Case 2: Invalid numeric
  ent$set_prop("age", "not_a_number")
  expect_warning(res2 <- ent$validate_ontology_types(), "expects numeric")
  expect_false(res2)

  # Case 3: Invalid boolean
  ent$set_prop("age", "25") # Reset age to valid
  ent$set_prop("is_valid", "Maybe")
  expect_warning(res3 <- ent$validate_ontology_types(), "expects boolean")
  expect_false(res3)

  # Case 4: Invalid date
  ent$set_prop("is_valid", "YES") # Reset boolean to valid state
  ent$set_prop("exp_date", "not-a-date") # Definitely invalid

  expect_warning(
    res4 <- ent$validate_ontology_types(),
    "expects date"
  )
  expect_false(res4)
})

test_that("Data integrity checksum (verify_checksum) detects tampering", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("INT_1", con, label = "Original")
  ent$set_prop("secret", "1234")
  ent$save()

  expect_true(ent$verify_checksum())

  # Tamper with DB directly, bypassing save()
  dbExecute(con, "UPDATE attributes SET value = '9999' WHERE instance_id = 'INT_1' AND property_id = 'secret'")

  ent$load()

  # Reload to get fresh state but checksum in DB is now 'outdated' relative to calc
  # Wait, verify_checksum compares DB checksum column with current calculated state
  # If I change the value, calculated hash changes, but column 'checksum' stays same.
  expect_warning(
    res <- ent$verify_checksum(),
    "External modification detected"
  )
  expect_false(res)
})

test_that("Lifecycle locking prevents modification of FINAL entities", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("LOCK_1", con)
  ent$set_prop("status", "DRAFT")
  ent$save()

  # 1. Set to FINAL
  ent$set_prop("status", "FINAL")
  ent$save() # This save should pass as we are moving TO final

  # 2. Attempt further modification
  ent$label <- "Modified Label"
  expect_error(ent$save(), "Modification denied")
})

test_that("Semantic 'is_a' hierarchy check works", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Use REPLACE because 'Section' and 'Entity' already exist in the seeds
  # Entity -> Section -> ProtocolSection
  dbExecute(con, "INSERT OR REPLACE INTO ont_classes (class_id, parent_class_id) VALUES ('Section', 'Entity')")
  dbExecute(con, "INSERT OR REPLACE INTO ont_classes (class_id, parent_class_id) VALUES ('ProtocolSection', 'Section')")

  ent <- Entity$new("P1", con, class_id = "ProtocolSection")

  expect_true(ent$is_a("ProtocolSection"))
  expect_true(ent$is_a("Section"))
  expect_true(ent$is_a("Entity"))
  expect_false(ent$is_a("Document"))
})

test_that("Detailed audit trail (delta) captures changes", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  ent <- Entity$new("AUDIT_1", con, label = "V1")
  ent$save() # Initial CREATE

  # Update label and add prop
  ent$label <- "V2"
  ent$set_prop("note", "first edit")
  ent$save()

  delta <- get_last_audit_delta(con, "AUDIT_1")

  expect_true(any(delta$key == "label" & delta$new_value == "V2"))
  expect_true(any(delta$key == "note" & delta$status == "Added"))
})

test_that("get_incoming_relations retrieves reverse links", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  subj <- Entity$new("SUBJ", con)$save()
  obj  <- Entity$new("OBJ", con)$save()

  subj$add_relation("references", "OBJ")$save()

  # Check from Object's perspective
  incoming <- obj$get_incoming_relations()
  expect_equal(nrow(incoming), 1)
  expect_equal(incoming$subject_id, "SUBJ")
  expect_equal(incoming$predicate_id, "references")
  expect_s3_class(incoming$subject_obj[[1]], "Entity")
})

test_that("find_contradictions detects local vs inherited mismatches", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # Rule: 'compound' must be consistent
  dbExecute(con, "INSERT INTO ont_consistency_rules VALUES ('compound', 'ERROR')")

  parent <- Entity$new("P", con)
  parent$set_prop("compound", "Aspirin")
  parent$save()

  child <- Entity$new("C", con)
  child$set_prop("compound", "Paracetamol") # Conflict!
  child$save()

  parent$add_relation("contains", "C")$save()

  conflicts <- child$find_contradictions()
  expect_equal(nrow(conflicts), 1)
  expect_equal(conflicts$local_value, "Paracetamol")
  expect_equal(conflicts$inherited_value, "Aspirin")
})

