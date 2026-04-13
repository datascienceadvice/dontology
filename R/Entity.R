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
      # Get current system user
      curr_user <- Sys.getenv("DONTOLOGY_USER", unset = Sys.info()[["user"]])

      tryCatch({
        DBI::dbWithTransaction(self$con, {
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
        })
      }, error = function(e) {
        stop(paste0("FAIL TO SAVE [", self$id, "]: ", e$message))
      })
      invisible(self)
    }
  )
)
