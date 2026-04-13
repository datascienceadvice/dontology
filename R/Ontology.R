#' Ontology Metadata Manager
#'
#' @description
#' Manages the definition of ontology classes, properties, and structural
#' constraints within the pharmaceutical documentation system. This class
#' acts as the "schema manager" for the hierarchical data.
#'
#' @field con DBIConnection. Active SQLite database connection.
#'
#' @importFrom R6 R6Class
#' @importFrom DBI dbExecute dbGetQuery
#' @export
Ontology <- R6::R6Class(
  "Ontology",
  public = list(
    con = NULL,

    #' @description
    #' Initialize the Ontology manager.
    #' @param con DBIConnection. Active SQLite connection.
    initialize = function(con) {
      self$con <- con
    },

    # --- Class Management ---

    #' @description
    #' Register a new class in the ontology.
    #' @param class_id Character. Unique identifier for the class (e.g., "Protocol").
    #' @param parent_class_id Character. ID of the parent class (for inheritance). Defaults to NA.
    #' @param description Character. Optional description of the class's purpose.
    add_class = function(class_id, parent_class_id = NA, description = "") {
      # Handle potential NULL inputs to ensure SQLite compatibility (requires NA for length 1)
      if (is.null(parent_class_id)) parent_class_id <- NA

      DBI::dbExecute(self$con,
                     "INSERT OR REPLACE INTO ont_classes (class_id, parent_class_id, description) VALUES (?, ?, ?)",
                     params = list(class_id, parent_class_id, description)
      )
      message("Class '", class_id, "' added to ontology.")
    },

    #' @description
    #' Retrieve all registered classes.
    #' @return A data.frame containing class definitions.
    get_classes = function() {
      DBI::dbGetQuery(self$con, "SELECT * FROM ont_classes")
    },

    # --- Property Management ---

    #' @description
    #' Register a new property (attribute or relation type).
    #' @param property_id Character. Unique identifier for the property (e.g., "expiry_date").
    #' @param value_type Character. Data type: "text", "numeric", "date", or "reference".
    #' @param description Character. Optional description of the property.
    add_property = function(property_id, value_type = "text", description = "") {
      DBI::dbExecute(self$con,
                     "INSERT OR REPLACE INTO ont_properties (property_id, value_type, description) VALUES (?, ?, ?)",
                     params = list(property_id, value_type, description)
      )
      message("Property '", property_id, "' (", value_type, ") added.")
    },

    #' @description
    #' Retrieve all registered properties.
    #' @return A data.frame containing property definitions.
    get_properties = function() {
      DBI::dbGetQuery(self$con, "SELECT * FROM ont_properties")
    },

    #' @description
    #' Define a structural constraint (mandatory child section).
    #' @param parent_class_id Character. The class that must contain the child.
    #' @param required_child_class_id Character. The class that must be present.
    add_constraint = function(parent_class_id, required_child_class_id) {
      # Ensure constraints table exists (fallback if not initialized via init_db)
      DBI::dbExecute(self$con, "CREATE TABLE IF NOT EXISTS ont_constraints (
        parent_class_id TEXT,
        required_child_class_id TEXT
      )")

      DBI::dbExecute(self$con, "INSERT INTO ont_constraints VALUES (?, ?)",
                     params = list(parent_class_id, required_child_class_id))
      message("Constraint added: ", parent_class_id, " requires ", required_child_class_id)
    },

    # --- Inspection Helpers ---

    #' @description
    #' Check if a specific class exists in the ontology.
    #' @param class_id Character. The class ID to verify.
    #' @return Logical. TRUE if exists, FALSE otherwise.
    class_exists = function(class_id) {
      res <- DBI::dbGetQuery(self$con, "SELECT 1 FROM ont_classes WHERE class_id = ?", params = list(class_id))
      return(nrow(res) > 0)
    },

    #' @description
    #' Retrieve the class hierarchy sorted by parent.
    #' @return A data.frame of classes ordered for hierarchical viewing.
    get_class_hierarchy = function() {
      df <- self$get_classes()
      # Basic sort by parent ID; NA values (roots) appear first
      return(df[order(df$parent_class_id, na.last = FALSE), ])
    }
  )
)
