suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
  library(jsonlite)
}))

setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("JSON Serialization (Export/Import)")

test_that("Full round-trip serialization works correctly", {
  con_orig <- setup_test_db()

  # 1. create complex db structure
  doc <- DocumentInstance$new("DOC_JSON", con_orig, "JSON Export Test", "Document")
  doc$set_prop("version", "1.5")
  doc$set_prop("status", "Validated")
  doc$save()

  s1 <- add_child(doc, "SEC_1", "Introduction")
  s1$set_prop("content", "This is the intro.")
  s1$save()

  s1_1 <- add_child(s1, "SEC_1_1", "Sub-section")
  s1_1$set_prop("content", "Deep content.")
  s1_1$save()

  # 2. export to file
  tmp_json <- tempfile(fileext = ".json")
  on.exit(if (file.exists(tmp_json)) unlink(tmp_json))

  export_to_json(doc, tmp_json)
  expect_true(file.exists(tmp_json))

  # 3. import to new db
  con_new <- setup_test_db()
  on.exit(dbDisconnect(con_new), add = TRUE)

  root_id <- import_from_json(tmp_json, con_new)

  # 4. check identity
  expect_equal(root_id, "DOC_JSON")

  # load new object
  doc_new <- DocumentInstance$new(root_id, con_new)

  expect_equal(doc_new$label, "JSON Export Test")
  expect_equal(doc_new$get_prop("version"), "1.5")

  # check
  rels <- dbGetQuery(con_new, "SELECT * FROM relations ORDER BY subject_id")
  # there are 2 relations: DOC_JSON -> SEC_1 и SEC_1 -> SEC_1_1
  expect_equal(nrow(rels), 2)

  # check
  sec_deep <- Entity$new("SEC_1_1", con_new)
  expect_equal(sec_deep$get_prop("content"), "Deep content.")
})

test_that("Order of sections is preserved during JSON round-trip", {
  con <- setup_test_db()
  doc <- DocumentInstance$new("DOC_ORDER", con, "Order Test")
  doc$save()

  # add childs in specific order
  add_child(doc, "A", "First")
  add_child(doc, "B", "Second")
  add_child(doc, "C", "Third")

  tmp_json <- tempfile(fileext = ".json")
  export_to_json(doc, tmp_json)

  # import
  con_res <- setup_test_db()
  import_from_json(tmp_json, con_res)

  # check order
  rels <- dbGetQuery(con_res, "SELECT object_id FROM relations WHERE subject_id = 'DOC_ORDER' ORDER BY sort_order")
  expect_equal(rels$object_id, c("A", "B", "C"))

  if (file.exists(tmp_json)) unlink(tmp_json)
  dbDisconnect(con)
  dbDisconnect(con_res)
})

test_that("Metadata types are handled correctly (Unboxing)", {
  con <- setup_test_db()
  ent <- Entity$new("METADATA_TEST", con)
  ent$set_prop("numeric_val", "100")
  ent$set_prop("text_val", "Hello World")
  ent$save()

  tmp_json <- tempfile(fileext = ".json")
  export_to_json(ent, tmp_json)

  raw_json <- jsonlite::fromJSON(tmp_json)

  expect_type(raw_json$metadata$numeric_val, "character")
  expect_equal(raw_json$label, "Untitled")

  if (file.exists(tmp_json)) unlink(tmp_json)
  dbDisconnect(con)
})

test_that("Import handles empty/minimal documents", {
  con <- setup_test_db()
  doc <- DocumentInstance$new("MINIMAL", con)
  doc$save()

  tmp_json <- tempfile(fileext = ".json")
  export_to_json(doc, tmp_json)

  con_new <- setup_test_db()
  expect_error(import_from_json(tmp_json, con_new), NA)

  doc_imported <- Entity$new("MINIMAL", con_new)
  expect_equal(doc_imported$id, "MINIMAL")
  expect_length(doc_imported$metadata, 0)

  if (file.exists(tmp_json)) unlink(tmp_json)
  dbDisconnect(con)
  dbDisconnect(con_new)
})
