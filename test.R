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

DBI::dbDisconnect(con)

# ------------------------------------------------------------------------------
library(dontology)
library(DBI)
library(RSQLite)

db_file <- "pharma_data.sqlite"
if (file.exists(db_file)) file.remove(db_file)
con <- dbConnect(SQLite(), db_file)
create_schema(con)
doc <- DocumentInstance$new("DOC_001", con, "Protocol v1")
doc$save()

# Делаем изменения
doc$label <- "Protocol v2"
doc$set_prop("status", "Review")
doc$save()

# Смотрим историю
history <- doc$get_history()
print(history)

DBI::dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), db_file)

# 1. Создаем/загружаем документ (Базовая версия)
doc <- DocumentInstance$new("PROT_001", con)
doc$label <- "Original Protocol 2023"
doc$set_prop("version", "1.0")
doc$save()

# 2. Экспортируем "Снимок" (Snapshot) — это наша точка отсчета
export_to_json(doc, "D:/protocol2023.json")

# 3. ТЕПЕРЬ ВНОСИМ ИЗМЕНЕНИЯ
# Изменяем заголовок
doc$label <- "Revised Protocol 2024 (v2)"

# Изменяем метаданные (версию и статус)
doc$set_prop("version", "2.0")
doc$set_prop("status", "In Review")

# Добавляем новую секцию (структурное изменение)
add_child(doc, "SEC_NEW", "New Safety Assessment Section")

# Сохраняем изменения в основную базу
doc$save()

# 4. ЗАПУСКАЕМ СРАВНЕНИЕ
# Сравниваем ТЕКУЩЕЕ состояние в базе с тем, что мы сохранили в файле D:/protocol2023.json
changes <- doc$compare_with_snapshot("D:/protocol2023.json")

# 5. ВЫВОДИМ РЕЗУЛЬТАТ
cat(render_diff_markdown(changes))

# В конце закрываем соединение
DBI::dbDisconnect(con)

# ------------------------------------------------------------------------------
library(DBI)
library(RSQLite)

con <- dbConnect(SQLite(), ":memory:")
create_schema(con) # Твоя функция инициализации БД

# --- ШАГ 1: СОЗДАНИЕ ДОКУМЕНТА (DRAFT) ---
doc <- DocumentInstance$new("REPORT_001", con, "Stability Report: {{product}}")
doc$set_prop("product", "Insulin-X")
doc$set_prop("status", "DRAFT")
doc$save()

# Создаем вложенную структуру
sec_intro <- add_child(doc, "SEC_1", "Introduction")
sec_details <- add_child(sec_intro, "SEC_1_1", "Technical Details")
sec_details$set_prop("content", "Initial study of {{product}}.")
sec_details$save()

# --- ШАГ 2: ЭКСПОРТ "ЗОЛОТОГО СТАНДАРТА" (SNAPSHOT) ---
# Это версия v1.0, которую мы зафиксируем в JSON
snapshot_file <- "report_v1.json"
export_to_json(doc, snapshot_file)
message("Snapshot v1.0 saved!")

# --- ШАГ 3: ВНЕСЕНИЕ ГЛУБОКИХ ИЗМЕНЕНИЙ ---
# 1. Меняем метаданные в корне
doc$set_prop("product", "Insulin-X (Improved)")
doc$save()

# 2. Меняем текст глубоко внутри (на уровне 1.1)
# Мы используем фабрику, чтобы найти объект в базе
deep_sec <- .instantiate_entity("SEC_1_1", con)
deep_sec$set_prop("content", "Updated study with new stability parameters.")
deep_sec$save()

# --- ШАГ 4: ЗАПУСК ГЛУБОКОГО СРАВНЕНИЯ ---
# Мы хотим увидеть разницу между тем, что в базе, и снимком v1.0
# Вызываем наш новый рекурсивный дифф
changes <- doc$compare_with_snapshot(snapshot_file)

message("\n--- DETECTED CHANGES ---")
print(changes[, c("path", "key", "status", "old_value", "new_value")])
# Ты увидишь изменения и в корне, и в SEC_1_1 (хотя сравнивал только корень)!

# --- ШАГ 5: РЕНДЕРИНГ С ПЛЕЙСХОЛДЕРАМИ ---
# Проверим, как Insulin-X (Improved) подставился во все секции
cat("\n--- RENDERED MARKDOWN ---\n")
cat(doc$render())

# --- ШАГ 6: УСТАНОВКА СТАТУСА FINAL И БЛОКИРОВКА ---
doc$set_prop("status", "FINAL")
doc$save()
message("\nDocument status set to FINAL.")

# ПОПЫТКА ИЗМЕНЕНИЯ (Должна провалиться)
tryCatch({
  doc$label <- "Hack the report"
  doc$save()
}, error = function(e) {
  message("\nBLOCKER WORKS: ", e$message)
})

# --- ШАГ 7: ПРОСМОТР ИСТОРИИ (AUDIT TRAIL) ---
message("\n--- AUDIT TRAIL FOR SEC_1_1 ---")
print(deep_sec$get_history())

DBI::dbDisconnect(con)

# ------------------------------------------------------------------------------

con <- dbConnect(SQLite(), ":memory:")
create_schema(con) # Твоя функция инициализации БД
# --- Контроль версий ---
doc <- DocumentInstance$new("PROT_01", con, "Protocol")
doc$set_prop("version", "1.0")
doc$save()

doc$bump_version("minor") # Изменит на 1.1
doc$bump_version("major") # Изменит на 2.0

# --- Валидация связей ---
# Создаем секцию с битой ссылкой
sec <- add_child(doc, "SEC_1", "Introduction")
sec$set_prop("content", "As shown in [Results](#sec-MISSING_ID)...") # ID не существует
sec$save()

# Запускаем валидацию
doc$validate()
# Выдаст: WARNING: BROKEN LINK: Reference to #sec-MISSING_ID not found in database.
DBI::dbDisconnect(con)

# sign -------------------------------------------------------------------------
# --- ПОДГОТОВКА ---
library(DBI)
library(RSQLite)

# 1. Создаем базу в памяти и инициализируем схему
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

# 2. Создаем документ (Draft v1.0)
doc <- DocumentInstance$new("REP_2024", con, "Stability Report: {{compound}}")
doc$set_prop("compound", "Drug-Alpha")
doc$set_prop("version", "1.0")
doc$set_prop("status", "DRAFT")
doc$save()

# --- ШАГ 1: СОЗДАНИЕ СТРУКТУРЫ И ССЫЛОК ---

# Создаем Секцию 1
sec1 <- add_child(doc, "SEC_INTRO", "Introduction")
sec1$set_prop("content", "This report analyzes {{compound}}. See results in [Detailed Data](#sec-DATA).")
sec1$save()

# Создаем Секцию 2 (Цель ссылки)
sec2 <- add_child(doc, "DATA", "Detailed Data")
sec2$set_prop("content", "Stability at 25C was within limits.")
sec2$save()

# --- ШАГ 2: ВАЛИДАЦИЯ ---

message("\n[Validation Test]")
# Если мы сейчас удалим секцию DATA, валидация ссылок выдаст предупреждение
if(doc$validate()) {
  message("Validation successful: All sections exist and links are intact.")
}

# --- ШАГ 3: КОНТРОЛЬ ВЕРСИЙ (Bump Version) ---

message("\n[Versioning Test]")
doc$bump_version("minor") # Теперь версия 1.1
# В Audit Trail теперь будет видно изменение версии

# --- ШАГ 4: ЭЛЕКТРОННЫЕ ПОДПИСИ ---

message("\n[Signature Test]")
# Симулируем подпись автора
Sys.setenv("DONTOLOGY_USER" = "Scientist_A")
doc$sign(role = "Author", meaning = "Data collection complete")

# Симулируем подпись проверяющего (Approver)
Sys.setenv("DONTOLOGY_USER" = "QC_Manager")
doc$sign(role = "Approver", meaning = "Reviewed and Approved")
# ВНИМАНИЕ: Подпись Approver автоматически ставит статус FINAL и блокирует документ

# --- ШАГ 5: ПРОВЕРКА ЦЕЛОСТНОСТИ (Locking) ---

message("\n[Data Integrity Test]")
tryCatch({
  doc$set_prop("compound", "Cheat-Drug") # Пытаемся изменить данные в финальном отчете
  doc$save()
}, error = function(e) {
  message("BLOCKER ACTIVE: ", e$message)
})

# --- ШАГ 6: ФИНАЛЬНЫЙ РЕНДЕРИНГ ---

message("\n[Final Rendering Output]")
report_md <- doc$render()
cat(report_md)

# --- ШАГ 7: ПРОСМОТР ЖУРНАЛА АУДИТА ---

message("\n[Audit Trail for Document]")
print(doc$get_history())

dbDisconnect(con)

# template ---------------------------------------------------------------------
library(DBI)
library(RSQLite)

# 1. Создаем базу в памяти и инициализируем схему
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

# Создаем структуру-эталон
tmp <- DocumentInstance$new("TPL_STABILITY", con, "TEMPLATE: Stability Report", "Document")
tmp$set_prop("content", "General introduction for all stability studies.")
tmp$save()

# Добавляем стандартные секции
sec_results <- add_child(tmp, "TPL_RESULTS", "Results Table")
sec_results$set_table("table_data", data.frame(Time=c(0,3,6), Result=c("NP","NP","NP")))
sec_results$save()

add_child(tmp, "TPL_CONCLUSION", "Conclusion")
message("Template created.")

doc_drug_x <- instantiate_template(
  template_id = "TPL_STABILITY",
  new_root_id = "STAB_DRUGX_2024",
  new_label = "Stability Report: Insulin Drug-X (Batch B2024)",
  con = con
)

cat(doc_drug_x$render())

print_tree(doc_drug_x)


dbDisconnect(con)

# cross ------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

# --- ДОКУМЕНТ 1: ПРОТОКОЛ ---
prot <- DocumentInstance$new("PROT_001", con, "Master Protocol", "Document")
prot$set_prop("limit", "not more than 0.5%")
prot$save()

# --- ДОКУМЕНТ 2: ОТЧЕТ ---
rep <- DocumentInstance$new("REP_001", con, "Final Report", "Document")
# Мы ссылаемся на название протокола и на его метаданные
rep$set_prop("content", "As defined in [[REF:PROT_001]], the acceptance limit is [[VAL:PROT_001:limit]].")
rep$save()

# --- РЕЗУЛЬТАТ ---
cat(rep$render())

dbDisconnect(con)

# types ------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

ont <- Ontology$new(con)
ont$add_property("potency", "numeric", "Concentration of active ingredient")
ont$add_property("release_date", "date", "Date of batch release")
ont$add_property("is_sterile", "boolean", "Sterility flag")

# Задаем в онтологии, что 'potency' - это число
ont$add_property("potency", "numeric")

doc <- DocumentInstance$new("REP_01", con)
# ОШИБКА: Записываем текст вместо числа
doc$set_prop("potency", "Very High")
doc$save()

# Запускаем валидацию
doc$validate()
# Выдаст: WARNING: Ontology Type Mismatch: Property 'potency' expects numeric, but got 'Very High'

dbDisconnect(con)

# local context ----------------------------------------------------------------
create_schema(con)

# 1. В корневом документе задаем общие данные
doc <- DocumentInstance$new("DOC_MAIN", con, "Report for {{drug}}")
doc$set_prop("drug", "Aspirin")
doc$set_prop("site", "Berlin Lab")
doc$save()

# 2. Создаем секцию без метаданных drug, но с метаданными site
sec <- add_child(doc, "SEC_1", "Analysis at {{site}}")
sec$set_prop("site", "Munich Lab") # Переопределяем местоположение
sec$set_prop("content", "Testing {{drug}} in a new environment.")
sec$save()

# 3. Рендерим
cat(doc$render())

dbDisconnect(con)

# incoming ---------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

# --- НАСТРОЙКА ОНТОЛОГИИ ---
ont <- Ontology$new(con)
ont$add_class("StandardClause", parent_class_id = "Section")

# Правило: Документ может содержать ('contains') только объекты типа 'Section' (или их подклассы)
ont$add_relation_constraint("Document", "contains", "Section")

# --- РАБОТА С ОБЪЕКТАМИ ---
doc <- DocumentInstance$new("DOC_1", con)
clause <- Entity$new("CLAUSE_1", con, class_id = "StandardClause")
clause$save()

# 1. Проверка иерархии
clause$is_a("Section") # TRUE (т.к. StandardClause наследник Section)

# 2. Добавление связи (пройдет успешно, т.к. StandardClause is-a Section)
doc$add_relation("contains", "CLAUSE_1")

# 3. Попытка добавить Документ внутрь Документа (вызовет ошибку)
# doc$add_relation("contains", "ANOTHER_DOC_ID") -> STOP: Ontology Violation

# 4. ОБРАТНАЯ СВЯЗЬ
# Спрашиваем у Клаузы: "Где ты используешься?"
doc$save()
usage <- clause$get_incoming_relations()
print(usage$subject_id) # Выведет "DOC_1"

dbDisconnect(con)

# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)
# 1. Настраиваем правило в онтологии
DBI::dbExecute(con, "INSERT INTO ont_consistency_rules VALUES ('drug_name', 'ERROR')")

# 2. Создаем документ для Aspirin
doc <- DocumentInstance$new("DOC_01", con, "Aspirin Study")
doc$set_prop("drug_name", "Aspirin")
doc$save()

# 3. Создаем секцию и КТО-ТО ОШИБСЯ: написал другое лекарство в метаданных
sec <- add_child(doc, "SEC_01", "Lab Results")
sec$set_prop("drug_name", "Paracetamol") # ПРОТИВОРЕЧИЕ!
sec$save()

# 4. Запускаем валидацию
doc$validate()
dbDisconnect(con)

# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)
# ------------------------------------------------------------------------------
con <- dbConnect(SQLite(), ":memory:")
create_schema(con)

dbDisconnect(con)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# close ------------------------------------------------------------------------
DBI::dbDisconnect(con)
