#' R6 Class Representing a Pharmaceutical Document Instance
#'
#' @description
#' Manages the lifecycle of a specific document, including hierarchical rendering
#' to Markdown with automatic numbering and validation against ontology constraints.
#' Inherits core database and attribute functionality from \code{\link{Entity}}.
#'
#' @importFrom DBI dbGetQuery
#' @importFrom jsonlite fromJSON
#' @importFrom knitr kable
#' @seealso \code{\link{Entity}}
#' @export
DocumentInstance <- R6::R6Class(
  "DocumentInstance",
  inherit = Entity,

  public = list(

    render = function(level = 1) {
      message("Assembling document: ", self$label)

      # 1. Подготовка контекста переменных
      context <- self$metadata
      context$current_date <- as.character(Sys.Date())

      # 2. Заголовок документа
      header_prefix <- paste(rep("#", level), collapse = "")
      lbl <- .fill_placeholders(self$label, context)
      doc_text <- paste0(header_prefix, " ", lbl, "\n\n")

      # 3. Версия (если есть)
      version <- self$get_prop("version")
      if (!is.null(version) && !is.na(version)) {
        doc_text <- paste0(doc_text, "*Version: ", version, "*\n\n")
      }

      # 4. Основной текст корня
      root_content <- self$get_prop("content")
      if (!is.null(root_content)) {
        doc_text <- paste0(doc_text, .fill_placeholders(root_content, context), "\n\n")
      }

      # 5. Сборка дочерних секций (передаем context)
      content_parts <- self$assemble_sections(self$id, level = level + 1, context = context)

      return(paste0(doc_text, content_parts))
    },

    assemble_sections = function(parent_id, level, context) { # Добавили context в аргументы!
      pid <- as.character(parent_id)
      query <- "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains' ORDER BY sort_order ASC"
      res <- DBI::dbGetQuery(self$con, query, params = list(pid))

      if (is.null(res) || nrow(res) == 0) return("")

      output <- ""
      for (i in seq_len(nrow(res))) {
        cid <- res$object_id[i]
        section <- .instantiate_entity(cid, self$con)

        if (inherits(section, "DocumentInstance") && section$id != self$id) {
          # Вложенный документ: рендерим его, передавая текущий уровень
          output <- paste0(output, section$render(level = level))
        } else {
          # Обычная секция: обрабатываем текст с плейсхолдерами
          sec_label <- .fill_placeholders(section$label, context)
          content <- .fill_placeholders(section$get_prop("content"), context)

          # Header
          header_prefix <- paste(rep("#", level), collapse = "")
          output <- paste0(output, header_prefix, " ", sec_label, " {#sec-", cid, "}\n\n")

          if (!is.null(content) && !is.na(content)) {
            output <- paste0(output, content, "\n\n")
          }

          # Таблицы
          table_json <- section$get_prop("table_data")
          if (!is.null(table_json) && !is.na(table_json)) {
            df <- jsonlite::fromJSON(table_json)
            # Используем sec_label для заголовка таблицы
            md_table <- knitr::kable(df, format = "pipe", caption = sec_label)
            output <- paste0(output, paste(md_table, collapse = "\n"), "\n\n")
          }

          # Графики
          plot_path <- section$get_prop("plot_path")
          if (!is.null(plot_path) && !is.na(plot_path)) {
            abs_path <- normalizePath(plot_path, winslash = "/", mustWork = FALSE)
            output <- paste0(
              output,
              "::: {custom-style=\"Figure\"}\n",
              "![", sec_label, "](", abs_path, ")\n", # Используем sec_label
              ":::\n\n"
            )
          }

          # Рекурсия для глубоко вложенных секций (передаем context)
          sub_content <- self$assemble_sections(cid, level + 1, context)
          output <- paste0(output, sub_content)
        }
      }
      return(output)
    },

    #' @description
    #' Performs a full validation of the document:
    #' 1. Internal Links (Referential Integrity)
    #' 2. Mandatory Sections (Ontology Constraints)
    #' 3. Empty Sections (Content Check)
    #' 4. Recursive Child Validation
    #' @return Logical. TRUE if the document and all its children are valid.
    validate = function() {
      message("--- Full Validation for: ", self$label, " ---")
      is_valid <- TRUE

      # 1. Валидация внутренних ссылок (Referential Integrity)
      # Этот метод мы определили отдельно, он ищет битые #sec-ID
      if (!self$validate_links()) {
        is_valid <- FALSE
      }

      # 2. Проверка обязательных онтологических классов
      query_req <- "SELECT required_child_class_id FROM ont_constraints WHERE parent_class_id = ?"
      required_classes <- DBI::dbGetQuery(self$con, query_req, params = list(self$class_id))$required_child_class_id

      query_act <- "
        SELECT i.class_id, i.instance_id, i.label
        FROM relations r
        JOIN instances i ON r.object_id = i.instance_id
        WHERE r.subject_id = ? AND r.predicate_id = 'contains'"
      actual_children <- DBI::dbGetQuery(self$con, query_act, params = list(self$id))

      missing <- setdiff(required_classes, actual_children$class_id)
      if (length(missing) > 0) {
        warning("Missing mandatory sections: ", paste(missing, collapse = ", "))
        is_valid <- FALSE
      }

      # 3. Проверка дочерних элементов
      if (nrow(actual_children) > 0) {
        for (i in seq_len(nrow(actual_children))) {
          # Используем фабрику для корректной инициализации
          child_obj <- .instantiate_entity(actual_children$instance_id[i], self$con)

          # РЕКУРСИЯ: Если ребенок — это документ, запускаем его полную валидацию
          if (inherits(child_obj, "DocumentInstance")) {
            if (!child_obj$validate()) {
              is_valid <- FALSE
            }
          }

          # ПРОВЕРКА НА ПУСТОТУ
          # Считаем секцию пустой, если в ней нет текста, таблиц, графиков И вложенных подсекций
          has_text <- !is.null(child_obj$get_prop("content")) && nchar(trimws(child_obj$get_prop("content"))) > 0
          has_table <- !is.null(child_obj$get_prop("table_data"))
          has_plot <- !is.null(child_obj$get_prop("plot_path"))

          query_sub <- "SELECT COUNT(*) as count FROM relations WHERE subject_id = ? AND predicate_id = 'contains'"
          has_sub <- DBI::dbGetQuery(self$con, query_sub, params = list(child_obj$id))$count > 0

          if (!has_text && !has_table && !has_plot && !has_sub) {
            warning("Section is empty: ", actual_children$label[i], " (ID: ", actual_children$instance_id[i], ")")
            is_valid <- FALSE
          }
        }
      }

      # Финальный отчет в консоль
      if (is_valid) {
        message("PASSED: Document '", self$label, "' is valid.")
      } else {
        message("FAILED: Document '", self$label, "' has validation errors (see warnings).")
      }

      return(is_valid)
    },

    #' @description
    #' Check all internal links (#sec-ID) for broken references.
    validate_links = function() {
      message("Checking internal links for: ", self$label)

      # 1. Собираем весь текст документа (рекурсивно)
      get_all_text <- function(entity) {
        text <- entity$get_prop("content") %||% ""

        # Получаем детей
        rel_df <- entity$relations$contains
        if (!is.null(rel_df) && nrow(rel_df) > 0) {
          for (cid in rel_df$object_id) {
            child <- .instantiate_entity(cid, entity$con)
            text <- paste(text, get_all_text(child))
          }
        }
        return(text)
      }

      full_text <- get_all_text(self)

      # 2. Ищем паттерны ссылок типа (#sec-ID)
      # Регулярное выражение ищет то, что внутри (#sec-...)
      links <- regmatches(full_text, gregexpr("(?<=#sec-)[a-zA-Z0-9_-]+", full_text, perl = TRUE))[[1]]
      links <- unique(links)

      if (length(links) == 0) return(TRUE)

      # 3. Проверяем наличие каждого ID в базе данных
      is_valid <- TRUE
      for (link_id in links) {
        res <- DBI::dbGetQuery(self$con, "SELECT 1 FROM instances WHERE instance_id = ?", params = list(link_id))
        if (nrow(res) == 0) {
          warning("BROKEN LINK: Reference to #sec-", link_id, " not found in database.")
          is_valid <- FALSE
        }
      }

      return(is_valid)
    },

    #' @description
    #' Compare current document state in DB with a saved JSON snapshot.
    #' @param json_path Path to the JSON file representing an older version.
    #' @return A list of differences.
    compare_with_snapshot = function(json_path) {
      if (!file.exists(json_path)) stop("Snapshot not found")

      temp_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
      create_schema(temp_con)
      old_id <- import_from_json(json_path, temp_con)
      old_doc <- .instantiate_entity(old_id, temp_con)

      diff_report <- diff_hierarchy(old_doc, self)

      DBI::dbDisconnect(temp_con)
      return(diff_report)
    },

    #' @description
    #' Automatically increment the version of the document.
    #' @param type Character. "minor" (1.0 -> 1.1) or "major" (1.0 -> 2.0).
    bump_version = function(type = "minor") {
      # Запрещаем менять версию, если статус FINAL
      if (toupper(self$get_prop("status") %||% "") == "FINAL") {
        stop("Cannot bump version of a FINAL document. Set status to DRAFT first.")
      }

      current_v <- self$get_prop("version")
      if (is.null(current_v) || is.na(current_v)) current_v <- "0.0"

      # Парсим версию (major.minor)
      parts <- as.numeric(unlist(strsplit(current_v, "\\.")))
      if (length(parts) < 2) parts <- c(parts, 0)

      if (type == "major") {
        parts[1] <- parts[1] + 1
        parts[2] <- 0
      } else {
        parts[2] <- parts[2] + 1
      }

      new_v <- paste(parts, collapse = ".")
      self$set_prop("version", new_v)
      message("Version bumped to: ", new_v)
      self$save()
      return(new_v)
    }
  )
)
