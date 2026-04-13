# if (file.exists("database.sqlite")) file.remove("database.sqlite")
# dontology:::init_db("database.sqlite")

# Подключаемся
con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")

# 1. Создаем объекты
doc <- Entity$new("REPORT_001", con, "Отчет по валидации", "Document")
sec1 <- Entity$new("SEC_01", con, "Введение", "Section")
sec2 <- Entity$new("SEC_02", con, "Методология", "Section")

# 2. Устанавливаем иерархию (кто в кого входит)
doc$add_relation("contains", "SEC_01")
doc$add_relation("contains", "SEC_02")

# Можно добавить обратную связь (необязательно, но удобно)
sec1$add_relation("part_of", "REPORT_001")
sec2$add_relation("part_of", "REPORT_001")

# 3. Сохраняем всё
doc$save()
sec1$save()
sec2$save()

# Теперь проверим таблицу связей
DBI::dbReadTable(con, "relations")

# Проверяем таблицу атрибутов
DBI::dbReadTable(con, "attributes")

# ------------------------------------------------------------------------------
# Создаем корень
doc <- Entity$new("SPEC_001", con, "Спецификация", "Document")

# Описываем структуру в виде списка (потом это можно вынести в JSON/YAML)
template <- list(
  "P1" = list(label = "Описание и состав"),
  "P2" = list(label = "Фармацевтическая разработка", children = list(
    "P21" = list(label = "Компоненты препарата"),
    "P22" = list(label = "Лекарственная форма")
  )),
  "P3" = list(label = "Производство")
)

# Строим всё дерево одной командой!
build_from_list(doc, template)

# Проверяем: достаем подразделы P2
p2 <- Entity$new("P2", con)
subsections <- get_related(p2)
sapply(subsections, function(x) x$label)
# [1] "Компоненты препарата" "Лекарственная форма"

# Теперь проверим таблицу связей
DBI::dbReadTable(con, "relations")

# Проверяем таблицу атрибутов
DBI::dbReadTable(con, "attributes")

# ------------------------------------------------------------------------------
# 1. Создаем менеджер онтологии
ont <- Ontology$new(con)

# 2. Определяем иерархию классов
ont$add_class("Document", description = "Базовый тип")
ont$add_class("Protocol", parent_class_id = "Document", description = "Протоколы испытаний")
ont$add_class("Report", parent_class_id = "Document", description = "Отчеты")

# 3. Определяем свойства
ont$add_property("shelf_life", "numeric", "Срок годности в месяцах")
ont$add_property("storage_temp", "text", "Температурный режим")
ont$add_property("approved_by", "text", "Кто утвердил")

# Теперь мы можем проверить, что у нас есть в системе
ont$get_classes()
ont$get_properties()

# ------------------------------------------------------------------------------
# 1. Создаем корень документа
doc <- DocumentInstance$new("DOC_001", con, "Протокол валидации процесса", "Document")
doc$set_prop("version", "1.0")
doc$set_prop("status", "Черновик")
doc$save()

# 2. Создаем разделы (используем наш хелпер add_child)
sec1 <- add_child(doc, "SEC_1", "Введение")
sec1$set_prop("content", "Этот протокол описывает процесс валидации производства таблеток.")
sec1$save()

# 3. Создаем подраздел
sub1 <- add_child(sec1, "SUB_1_1", "Цель валидации")
sub1$set_prop("content", "Подтвердить стабильность выхода продукта (Yield > 98%).")
sub1$save()

# 4. РЕНДЕРИНГ!
full_text <- doc$render()
cat(full_text)

# ------------------------------------------------------------------------------
# Инициализируем библиотеку один раз
init_standard_library(con)

# Создаем Спецификацию
spec <- DocumentInstance$new("SPEC_001", con, "Спецификация на ЛС")

# Добавляем уникальный раздел
s1 <- add_child(spec, "SPEC_S1", "Состав")
s1$set_prop("content", "Активное вещество: 50 мг.")
s1$save()

# ПРИСОЕДИНЯЕМ стандартный пункт из библиотеки
# Нам не нужно создавать новый объект, мы просто создаем СВЯЗЬ
spec$add_relation("contains", "STP_STORAGE_COLD", sort_order = 2)
spec$save()

# Рендерим - в документе появится текст из библиотеки!
cat(spec$render())

# ------------------------------------------------------------------------------
# Допустим, изменились требования к хранению в холодильнике
# Нам нужно найти все документы, где используется пункт "STP_STORAGE_COLD"
usage <- where_used(con, "STP_STORAGE_COLD")

# Результат покажет ID документов и тип связи (например, 'contains')
print(usage)

drafts <- find_by_attribute(con, "status", "Draft")

# Выведем их названия
sapply(drafts, function(x) x$label)

# ------------------------------------------------------------------------------
# 1. Создаем документ
con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
doc <- DocumentInstance$new("DOC_TABLE", con, "Отчет по стабильности")

# 2. Создаем раздел с таблицей
sec <- add_child(doc, "STAB_SEC", "Результаты испытаний")

# 3. Готовим данные
results <- data.frame(
  "Месяц" = c(0, 3, 6),
  "Описание" = c("Белые таблетки", "Белые таблетки", "Слегка желтоватые"),
  "Содержание_мг" = c(50.1, 49.8, 48.5)
)

# 4. Сохраняем таблицу в раздел
sec$set_table("table_data", results)
sec$save()

# 5. Печатаем в Word!
export_document(doc, format = "word", file_path = "Protocol_2024.docx")
export_document(doc, format = "pdf", file_path = "Final_Submission.pdf")
DBI::dbDisconnect(con)

# json export / import ---------------------------------------------------------
library(dontology)
library(DBI)
library(RSQLite)

# 1. ПОДГОТОВКА БАЗЫ
# Создаем временную базу в файле
db_file <- "pharma_data.sqlite"
if (file.exists(db_file)) file.remove(db_file)
init_db(db_file)
con <- dbConnect(SQLite(), db_file)

# 2. СОЗДАНИЕ ДОКУМЕНТА (Data Tree)
message("--- Шаг 1: Создание структуры документа ---")

# Корень: Спецификация
doc <- DocumentInstance$new("SPEC_V1", con, "Спецификация на ЛС", "Document")
doc$set_prop("version", "1.0.0")
doc$set_prop("status", "Draft")

# Раздел 1: Состав (с текстом)
s1 <- add_child(doc, "SEC_COMP", "Состав препарата")
s1$set_prop("content", "Активный ингредиент: Парацетамол - 500 мг.")

# Раздел 2: Испытания (с таблицей)
s2 <- add_child(doc, "SEC_TESTS", "Показатели качества")
test_data <- data.frame(
  Parameter = c("Описание", "Подлинность", "Растворение"),
  Method = c("Визуальный", "ВЭЖХ", "Фарм. статья"),
  Limit = c("Белые таблетки", "Соответствует", "Не менее 75%")
)
s2$set_table("table_data", test_data)

# Сохраняем всё дерево в базу
doc$save()
s1$save()
s2$save()

# 3. ЭКСПОРТ В JSON
message("\n--- Шаг 2: Экспорт в JSON ---")
json_file <- "specification_export.json"
export_to_json(doc, json_file)

# 4. ПОЛНАЯ ОЧИСТКА (Имитируем перенос на другой компьютер)
message("\n--- Шаг 3: Удаление данных из БД ---")
dbExecute(con, "DELETE FROM instances")
dbExecute(con, "DELETE FROM attributes")
dbExecute(con, "DELETE FROM relations")

# Проверяем, что в базе пусто
print(dbGetQuery(con, "SELECT count(*) as count FROM instances")) # Должно быть 0

# 5. ИМПОРТ ИЗ JSON
message("\n--- Шаг 4: Импорт из JSON ---")
new_doc_id <- import_from_json(json_file, con)

# 6. ПРОВЕРКА ВОССТАНОВЛЕНИЯ
message("\n--- Шаг 5: Проверка восстановленных данных ---")

# Создаем объект из восстановленного ID
restored_doc <- DocumentInstance$new(new_doc_id, con)

print(paste("Документ:", restored_doc$label))
print(paste("Версия:", restored_doc$get_prop("version")))

# Проверяем, на месте ли разделы (через хелпер get_related)
sections <- get_related(restored_doc)
for (s in sections) {
  print(paste("  Найдено раздел:", s$label))

  # Проверяем содержимое первого раздела
  if (s$id == "SEC_COMP") {
    print(paste("    Текст состава:", s$get_prop("content")))
  }

  # Проверяем таблицу во втором разделе
  if (s$id == "SEC_TESTS") {
    print("    Таблица показателей:")
    print(s$get_table("table_data"))
  }
}

# 7. ФИНАЛЬНЫЙ ТЕСТ: РЕНДЕРИНГ В WORD
# Теперь проверим, что восстановленный документ может напечатать сам себя
export_document(restored_doc, format = "word", file_path = "Restored_Spec.docx")

dbDisconnect(con)
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# close ------------------------------------------------------------------------
DBI::dbDisconnect(con)
