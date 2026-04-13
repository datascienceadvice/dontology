suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
}))

# Хелпер для настройки базы
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("Structure Helpers (Hierarchy Management)")

test_that("add_child creates and links entities correctly", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  parent <- Entity$new("P1", con, "Parent")
  parent$save()

  # Добавляем ребенка
  child <- add_child(parent, "C1", "Child One", class_id = "Section")

  # 1. Проверяем возвращаемый объект
  expect_s3_class(child, "Entity")
  expect_equal(child$id, "C1")
  expect_equal(child$label, "Child One")

  # 2. Проверяем наличие связи в базе данных
  rel_db <- dbGetQuery(con, "SELECT * FROM relations WHERE subject_id = 'P1'")
  expect_equal(nrow(rel_db), 1)
  expect_equal(rel_db$object_id, "C1")
  expect_equal(rel_db$predicate_id, "contains")

  # 3. Проверяем наличие инстанса ребенка в базе
  inst_db <- dbGetQuery(con, "SELECT label FROM instances WHERE instance_id = 'C1'")
  expect_equal(inst_db$label, "Child One")
})

test_that("get_related retrieves linked entities as objects", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  parent <- Entity$new("P_REF", con)
  parent$save()

  # Создаем несколько связей
  add_child(parent, "C_1", "First")
  add_child(parent, "C_2", "Second")

  # Получаем связанные объекты
  related <- get_related(parent, "contains")

  expect_length(related, 2)
  expect_s3_class(related[[1]], "Entity")
  expect_s3_class(related[[2]], "Entity")

  ids <- sapply(related, function(x) x$id)
  expect_true(all(c("C_1", "C_2") %in% ids))

  # Проверка на пустой результат
  expect_length(get_related(parent, "non_existent_predicate"), 0)
})

test_that("build_from_list constructs a deep hierarchy from a list", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- Entity$new("ROOT_DOC", con, "Main Document")
  doc$save()

  # Определяем вложенную структуру
  template <- list(
    "SEC_1" = list(
      label = "Intro",
      children = list(
        "SUB_1_1" = list(label = "Scope"),
        "SUB_1_2" = list(label = "Legal")
      )
    ),
    "SEC_2" = list(label = "Methods")
  )

  # Строим структуру
  build_from_list(doc, template)

  # 1. Проверяем верхний уровень (SEC_1 и SEC_2)
  top_level <- dbGetQuery(con, "SELECT object_id FROM relations WHERE subject_id = 'ROOT_DOC'")
  expect_length(top_level$object_id, 2)
  expect_true(all(c("SEC_1", "SEC_2") %in% top_level$object_id))

  # 2. Проверяем вложенный уровень (SUB_1_1 и SUB_1_2 внутри SEC_1)
  sub_level <- dbGetQuery(con, "SELECT object_id FROM relations WHERE subject_id = 'SEC_1'")
  expect_length(sub_level$object_id, 2)
  expect_true(all(c("SUB_1_1", "SUB_1_2") %in% sub_level$object_id))

  # 3. Проверяем, что объекты созданы с правильными метками
  expect_equal(Entity$new("SUB_1_1", con)$label, "Scope")
})

test_that("add_child works with custom predicates", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  e1 <- Entity$new("E1", con)$save()

  # Используем предикат 'references' вместо 'contains'
  add_child(e1, "E2", "Reference Object", predicate = "references")

  rel <- dbGetQuery(con, "SELECT * FROM relations WHERE subject_id = 'E1'")
  expect_equal(rel$predicate_id, "references")
  expect_equal(rel$object_id, "E2")
})

test_that("Hierarchy logic handles persistence across object recreations", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  p <- Entity$new("P", con)$save()
  add_child(p, "C", "Child")

  # Уничтожаем объект p и создаем его заново из базы
  rm(p)
  p_new <- Entity$new("P", con)

  # Проверяем, что новый объект подтянул связи из базы
  related <- get_related(p_new)
  expect_length(related, 1)
  expect_equal(related[[1]]$id, "C")
})
