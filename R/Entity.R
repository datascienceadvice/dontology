#' @import R6
#' @importFrom DBI dbExecute dbGetQuery dbWithTransaction
#' @importFrom stats setNames
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom ggplot2 ggsave
NULL

#' Base R6 Class for Ontology Entities
#'
#' @description
#' Provides the core functionality for managing ontology-based instances.
#' Handles database synchronization (CRUD), metadata attributes (EAV model),
#' and hierarchical or semantic relations.
#'
#' @field id Character. Unique identifier for the instance.
#' @field label Character. Human-readable name/title of the instance.
#' @field class_id Character. The ontology class ID this instance belongs to.
#' @field con DBIConnection. Active SQLite database connection.
#' @field metadata List. Key-value pairs representing instance attributes.
#' @field relations List. Named list of data.frames representing directional links to other instances.
#'
#' @export
Entity <- R6::R6Class(
  "Entity",
  public = list(
    id = NULL,
    label = NULL,
    class_id = NULL,
    con = NULL,
    metadata = list(),
    relations = list(),

    #' @description
    #' Create a new Entity object.
    #' @param id Character. Unique instance ID.
    #' @param con DBIConnection. SQLite connection.
    #' @param label Character. Optional human-readable name.
    #' @param class_id Character. Ontology class.
    initialize = function(id, con, label = NULL, class_id = NULL) {
      self$id <- as.character(id)
      self$con <- con

      self$load()

      if (!is.null(label)) self$label <- as.character(label)
      if (!is.null(class_id)) self$class_id <- as.character(class_id)

      if (is.null(self$label) || is.na(self$label)) self$label <- "Untitled"
      if (is.null(self$class_id) || is.na(self$class_id)) self$class_id <- "Entity"
    },

    #' @description
    #' Set a metadata property (attribute).
    #' @param key Character. Attribute name.
    #' @param value Atomic. Attribute value.
    #' @return The current Entity object (invisibly).
    set_prop = function(key, value) {
      self$metadata[[key]] <- value
      invisible(self)
    },

    #' @description
    #' Retrieve a metadata property.
    #' @param key Character. Attribute name.
    #' @return The value of the property or NULL if not found.
    get_prop = function(key) {
      if (is.null(self$metadata[[key]])) return(NULL)
      return(self$metadata[[key]])
    },

    #' @description
    #' Define a relation to another instance.
    #' @param predicate Character. The type of relation (e.g., "contains", "references").
    #' @param object_id Character. The ID of the target instance.
    #' @param sort_order Integer. Position for ordered relations.
    #' @return The current Entity object (invisibly).
    add_relation = function(predicate, object_id, sort_order = 0) {
      # ОНТОЛОГИЧЕСКАЯ ПРОВЕРКА
      target <- .instantiate_entity(object_id, self$con)

      # Проверяем, есть ли правила для этого предиката
      rules <- DBI::dbGetQuery(self$con,
                               "SELECT object_class FROM ont_relation_rules WHERE subject_class = ? AND predicate_id = ?",
                               params = list(self$class_id, predicate))

      if (nrow(rules) > 0) {
        # Проверяем, подходит ли объект под разрешенные классы (используя наш новый is_a)
        is_allowed <- any(sapply(rules$object_class, function(cls) target$is_a(cls)))

        if (!is_allowed) {
          stop(sprintf("Ontology Violation: %s cannot '%s' a %s (Expected: %s)",
                       self$class_id, predicate, target$class_id, paste(rules$object_class, collapse = "/")))
        }
      }

      if (predicate == "contains") {
        # Создаем временный объект ребенка для проверки
        child_obj <- .instantiate_entity(object_id, self$con)

        # Если ребенок уже является предком для текущего объекта (self)
        if (child_obj$is_ancestor_of(self$id)) {
          stop(sprintf("INTEGRITY ERROR: Circular reference detected! '%s' is already an ancestor of '%s'.",
                       object_id, self$id))
        }
      }

      if (is.null(self$relations[[predicate]])) {
        # Initialize data frame with column names matching the database schema
        self$relations[[predicate]] <- data.frame(
          object_id = character(),
          sort_order = integer(),
          stringsAsFactors = FALSE
        )
      }

      if (!(object_id %in% self$relations[[predicate]]$object_id)) {
        # Append new relation entry
        new_rel <- data.frame(
          object_id = as.character(object_id),
          sort_order = as.integer(sort_order),
          stringsAsFactors = FALSE
        )
        self$relations[[predicate]] <- rbind(self$relations[[predicate]], new_rel)
      }
      invisible(self)
    },

    #' @description
    #' Find all entities that point to this entity via any predicate.
    #' @return A data.frame with subject_id, predicate_id, and the instantiated R6 objects.
    get_incoming_relations = function() {
      query <- "
        SELECT subject_id, predicate_id
        FROM relations
        WHERE object_id = ?"

      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))

      if (nrow(res) == 0) return(NULL)

      # Добавляем колонку с готовыми R6 объектами для удобства манипуляции
      res$subject_obj <- lapply(res$subject_id, function(sid) {
        .instantiate_entity(sid, self$con)
      })

      return(res)
    },

    #' @description
    #' Get a property, searching up the hierarchy if not found locally.
    #' @param key Character. The property ID to find.
    get_prop_inherited = function(key) {
      # 1. Пробуем найти у себя
      val <- self$get_prop(key)
      if (!is.null(val) && !is.na(val) && val != "") return(val)

      # 2. Если нет, ищем родителя через связь 'contains'
      query <- "SELECT subject_id FROM relations WHERE object_id = ? AND predicate_id = 'contains' LIMIT 1"
      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))

      if (nrow(res) > 0) {
        # Рекурсивно запрашиваем у родителя
        parent <- .instantiate_entity(res$subject_id[1], self$con)
        return(parent$get_prop_inherited(key))
      }
      return(NULL)
    },

    #' @description
    #' Build a merged metadata context (parent properties + local properties).
    #' Local properties override parent properties.
    get_context = function() {
      # Ищем родителя
      query <- "SELECT subject_id FROM relations WHERE object_id = ? AND predicate_id = 'contains' LIMIT 1"
      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))

      parent_context <- list()
      if (nrow(res) > 0) {
        parent <- .instantiate_entity(res$subject_id[1], self$con)
        parent_context <- parent$get_context()
      }

      # Объединяем: контекст родителя + наши данные (наши затирают родительские)
      # modifyList — удобная функция для слияния списков
      combined_context <- utils::modifyList(parent_context, self$metadata)
      return(combined_context)
    },

    #' @description
    #' Serialize a data.frame and store it as a metadata property.
    #' @param key Character. Property name for the table.
    #' @param df data.frame. The table data to store.
    #' @return The current Entity object (invisibly).
    set_table = function(key, df) {
      # Convert data.frame to JSON string for storage
      json_str <- jsonlite::toJSON(df, simplifyVector = TRUE)
      self$set_prop(key, json_str)
      invisible(self)
    },

    #' @description
    #' Retrieve a stored table and deserialize it back to a data.frame.
    #' @param key Character. Property name of the table.
    #' @return A data.frame or NULL if the property is missing.
    get_table = function(key) {
      json_str <- self$get_prop(key)
      if (is.null(json_str)) return(NULL)
      return(jsonlite::fromJSON(json_str))
    },

    #' @description
    #' Export a plot to a PNG file and store the file path in metadata.
    #' @param plot_obj A plot object (ggplot2 or base R).
    #' @param width Numeric. Output width in inches.
    #' @param height Numeric. Output height in inches.
    set_plot = function(plot_obj, width = 6, height = 4) {
      plot_dir <- "plots"
      if (!dir.exists(plot_dir)) dir.create(plot_dir)

      timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
      plot_path <- file.path(plot_dir, paste0(self$id, "_", timestamp, ".png"))

      ggplot2::ggsave(plot_path, plot = plot_obj, width = width, height = height, device = "png")
      self$set_prop("plot_path", plot_path)
    },

    #' @description
    #' Retrieve the audit trail for this specific instance.
    #' @return A data.frame with columns user_id, action, and timestamp.
    get_history = function() {
      query <- "
        SELECT user_id, action, timestamp
        FROM audit_log
        WHERE instance_id = ?
        ORDER BY timestamp DESC"

      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))
      return(res)
    },


    #' @description
    #' Detects logical contradictions between local metadata and inherited context.
    #' @return A data.frame of detected conflicts.
    find_contradictions = function() {
      conflicts <- data.frame(
        instance_id = character(),
        property_id = character(),
        local_value = character(),
        inherited_value = character(),
        severity = character(),
        stringsAsFactors = FALSE
      )

      # 1. Получаем список свойств, требующих строгой консистентности
      rules <- DBI::dbGetQuery(self$con, "SELECT property_id, severity FROM ont_consistency_rules")
      if (nrow(rules) == 0) return(conflicts)

      # 2. Ищем родителя, чтобы получить его контекст
      query <- "SELECT subject_id FROM relations WHERE object_id = ? AND predicate_id = 'contains' LIMIT 1"
      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))

      if (nrow(res) > 0) {
        parent <- .instantiate_entity(res$subject_id[1], self$con)
        # Получаем контекст, накопленный всеми родителями выше
        inherited_context <- parent$get_context()

        for (i in seq_len(nrow(rules))) {
          prop <- rules$property_id[i]
          local_val <- self$get_prop(prop)
          inherited_val <- inherited_context[[prop]]

          # Если свойство задано и тут, и там, но они разные — это конфликт!
          if (!is.null(local_val) && !is.null(inherited_val)) {
            if (as.character(local_val) != as.character(inherited_val)) {
              conflicts <- rbind(conflicts, data.frame(
                instance_id = self$id,
                property_id = prop,
                local_value = as.character(local_val),
                inherited_value = as.character(inherited_val),
                severity = rules$severity[i],
                stringsAsFactors = FALSE
              ))
            }
          }
        }
      }
      return(conflicts)
    },

    #' @description
    #' Validates that metadata values match the types defined in the ontology.
    validate_ontology_types = function() {
      keys <- names(self$metadata)
      if (length(keys) == 0) return(TRUE)

      # 1. Получаем определения типов из онтологии для текущих ключей
      sql <- sprintf(
        "SELECT property_id, value_type FROM ont_properties WHERE property_id IN (%s)",
        paste0("'", keys, "'", collapse = ",")
      )
      rules <- DBI::dbGetQuery(self$con, sql)

      if (nrow(rules) == 0) return(TRUE) # Нет правил — нет проблем

      is_valid <- TRUE

      for (i in seq_len(nrow(rules))) {
        prop <- rules$property_id[i]
        type <- rules$value_type[i]
        val <- self$metadata[[prop]]

        if (is.null(val) || is.na(val) || val == "") next

        # 2. Проверка типов
        valid_type <- switch(type,
                             "numeric" = !is.na(suppressWarnings(as.numeric(val))),
                             "date"    = !is.na(suppressWarnings(as.Date(as.character(val)))),
                             "boolean" = toupper(as.character(val)) %in% c("TRUE", "FALSE", "1", "0", "YES", "NO"),
                             "text"    = TRUE,
                             TRUE # По умолчанию верим, если тип неизвестен
        )

        if (!valid_type) {
          warning(sprintf("Ontology Type Mismatch: Property '%s' in '%s' expects %s, but got '%s'",
                          prop, self$id, type, val))
          is_valid <- FALSE
        }
      }
      return(is_valid)
    },

    #' @description
    #' Check if the entity belongs to a class or any of its subclasses (Ontological "is-a").
    #' @param target_class_id The class ID to check against.
    is_a = function(target_class_id) {
      if (self$class_id == target_class_id) return(TRUE)

      # Рекурсивно проверяем иерархию классов в таблице ont_classes
      check_parent = function(current_class) {
        query <- "SELECT parent_class_id FROM ont_classes WHERE class_id = ?"
        res <- DBI::dbGetQuery(self$con, query, params = list(current_class))

        if (nrow(res) == 0 || is.na(res$parent_class_id[1])) return(FALSE)
        if (res$parent_class_id[1] == target_class_id) return(TRUE)

        return(check_parent(res$parent_class_id[1]))
      }

      return(check_parent(self$class_id))
    },

    #' @description
    #' Check if adding a relation would create a circular dependency.
    #' @param potential_child_id The ID of the entity we want to add as a child.
    is_ancestor_of = function(potential_child_id) {
      # 1. Ищем всех родителей текущего объекта (кто содержит нас?)
      query <- "SELECT subject_id FROM relations WHERE object_id = ? AND predicate_id = 'contains'"
      res <- DBI::dbGetQuery(self$con, query, params = list(self$id))

      if (nrow(res) == 0) return(FALSE)

      # 2. Если среди наших родителей есть тот, кого мы хотим добавить в дети - это цикл!
      if (potential_child_id %in% res$subject_id) return(TRUE)

      # 3. Рекурсивно проверяем родителей наших родителей
      for (parent_id in res$subject_id) {
        parent_obj <- .instantiate_entity(parent_id, self$con)
        if (parent_obj$is_ancestor_of(potential_child_id)) return(TRUE)
      }

      return(FALSE)
    },

    calculate_current_hash = function() {
      # Собираем все данные объекта в одну строку для хеширования
      state_string <- paste0(
        self$label,
        self$class_id,
        jsonlite::toJSON(self$metadata, auto_unbox = TRUE, sort_keys = TRUE),
        jsonlite::toJSON(self$relations, auto_unbox = TRUE, sort_keys = TRUE)
      )
      return(digest::digest(state_string, algo = "sha256"))
    },

    verify_checksum = function() {
      # Берем хеш из базы
      res <- DBI::dbGetQuery(self$con, "SELECT checksum FROM instances WHERE instance_id = ?", params = list(self$id))
      if (nrow(res) == 0 || is.na(res$checksum[1])) return(TRUE) # Еще не сохранялся

      current_hash <- self$calculate_current_hash()
      if (current_hash != res$checksum[1]) {
        warning("DATA INTEGRITY CRITICAL: External modification detected for ", self$id)
        return(FALSE)
      }
      return(TRUE)
    },

    #' @description
    #' Load instance state from the SQLite database.
    load = function() {
      res <- DBI::dbGetQuery(self$con,
                             "SELECT label, class_id FROM instances WHERE instance_id = ?",
                             params = list(self$id))

      if (nrow(res) > 0) {
        self$label <- as.character(res[1, "label"])
        self$class_id <- as.character(res[1, "class_id"])

        # Attributes
        attr_res <- DBI::dbGetQuery(self$con, "SELECT property_id, value FROM attributes WHERE instance_id = ?", params = list(self$id))
        if (nrow(attr_res) > 0) {
          self$metadata <- as.list(stats::setNames(attr_res$value, attr_res$property_id))
        }

        # Relations - IMPORTANT: Clear current relations before loading
        rel_res <- DBI::dbGetQuery(self$con,
                                   "SELECT object_id, sort_order, predicate_id
                                    FROM relations
                                    WHERE subject_id = ? ORDER BY sort_order ASC",
                                   params = list(self$id))

        self$relations <- list()
        if (nrow(rel_res) > 0) {
          self$relations <- split(rel_res[, c("object_id", "sort_order")], rel_res$predicate_id)
        }
      }
      invisible(self)
    },

    #' @description
    #' Save instance state to the SQLite database.
    #' Uses a transaction to ensure atomic updates across multiple tables.
    save = function() {
      res_status <- DBI::dbGetQuery(self$con,
                                    "SELECT value FROM attributes WHERE instance_id = ? AND property_id = 'status'",
                                    params = list(self$id))

      if (nrow(res_status) > 0 && toupper(res_status$value) == "FINAL") {
        stop(paste0("DATA INTEGRITY ERROR: [", self$id, "] is APPROVED/FINAL. Modification denied."))
      }

      # Get current system user
      curr_user <- Sys.getenv("DONTOLOGY_USER", unset = Sys.info()[["user"]])

      # 1. Перед сохранением загружаем старое состояние из базы для сравнения
      old_state <- .instantiate_entity(self$id, self$con)
      # Получаем таблицу изменений
      diff_log <- diff_entities(old_state, self)

      # Превращаем diff в компактный JSON
      delta_json <- if(nrow(diff_log) > 0) jsonlite::toJSON(diff_log) else "No changes"

      tryCatch({
        DBI::dbWithTransaction(self$con, {
          res_exists <- DBI::dbGetQuery(self$con,
                                        "SELECT 1 FROM instances WHERE instance_id = ?", params = list(self$id))
          action_type <- if (nrow(res_exists) > 0) "UPDATE" else "CREATE"

          # 1. Instances
          c_label <- if (is.null(self$label) || is.na(self$label)) "Untitled" else as.character(self$label)[1]
          c_class <- if (is.null(self$class_id) || is.na(self$class_id)) "Entity" else as.character(self$class_id)[1]

          affected <- DBI::dbExecute(self$con,
                                     "UPDATE instances SET class_id = ?, label = ?, updated_by = ? WHERE instance_id = ?",
                                     params = list(c_class, c_label, curr_user, self$id))

          if (affected == 0) {
            DBI::dbExecute(self$con,
                           "INSERT INTO instances (instance_id, class_id, label, created_by, updated_by) VALUES (?, ?, ?, ?, ?)",
                           params = list(self$id, c_class, c_label, curr_user, curr_user))
          }

          # 2. Attributes
          DBI::dbExecute(self$con, "DELETE FROM attributes WHERE instance_id = ?", params = list(self$id))
          if (length(self$metadata) > 0) {
            for (key in names(self$metadata)) {
              val <- self$metadata[[key]]
              c_val <- if (is.null(val) || (is.atomic(val) && is.na(val))) "" else as.character(val)[1]
              DBI::dbExecute(self$con, "INSERT INTO attributes (instance_id, property_id, value) VALUES (?, ?, ?)",
                             params = list(self$id, key, c_val))
            }
          }

          # 3. Relations
          DBI::dbExecute(self$con, "DELETE FROM relations WHERE subject_id = ?", params = list(self$id))

          if (length(self$relations) > 0) {
            for (pred in names(self$relations)) {
              rel_df <- self$relations[[pred]]
              if (nrow(rel_df) > 0) {
                for (i in seq_len(nrow(rel_df))) {
                  DBI::dbExecute(
                    self$con,
                    "INSERT OR REPLACE INTO relations (subject_id, predicate_id, object_id, sort_order)
                     VALUES (?, ?, ?, ?)",
                    params = list(self$id, as.character(pred), as.character(rel_df$object_id[i]), as.integer(rel_df$sort_order[i])))
                }
              }
            }
          }

          # 3. Обновляем контрольную сумму (checksum)
          new_checksum <- self$calculate_current_hash()
          DBI::dbExecute(self$con, "UPDATE instances SET checksum = ? WHERE instance_id = ?",
                         params = list(new_checksum, self$id))

          # 4. Записываем в аудит детальный лог
          DBI::dbExecute(self$con,
                         "INSERT INTO audit_log (instance_id, user_id, action, delta) VALUES (?, ?, ?, ?)",
                         params = list(self$id, curr_user, if(nrow(diff_log) > 0) "UPDATE" else "CREATE", delta_json))
        })
      }, error = function(e) {
        stop(paste0("FAIL TO SAVE [", self$id, "]: ", e$message))
      })
      invisible(self)
    }
  )
)
