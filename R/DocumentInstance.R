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

    #' @description
    #' Renders the entire document including title, metadata, and all nested
    #' sections into a single Markdown string.
    #' @return A character string containing the full Markdown text.
    render = function() {
      message("Assembling document: ", self$label)

      # label
      lbl <- if(is.na(self$label) || is.null(self$label)) "Untitled" else self$label
      doc_text <- paste0("# ", lbl, "\n\n")

      # version
      version <- self$get_prop("version")
      if (!is.null(version) && !is.na(version)) {
        doc_text <- paste0(doc_text, "*Version: ", version, "*\n")
      }
      doc_text <- paste0(doc_text, "\n---\n\n")

      # root
      root_content <- self$get_prop("content")
      if (!is.null(root_content) && !is.na(root_content)) {
        doc_text <- paste0(doc_text, root_content, "\n\n")
      }

      # recursion
      content_parts <- self$assemble_sections(self$id, level = 2)

      if (content_parts == "") {
        message("Warning: No child sections found in database for ID: ", self$id)
      }

      full_output <- paste0(doc_text, content_parts)
      return(full_output)
    },

    #' @description
    #' Internal recursive method to assemble sections based on hierarchical relations.
    #' Handles auto-numbering, Markdown headers, and content blocks (text and tables).
    #' @param parent_id Character. The ID of the parent section or document node.
    #' @param level Integer. Markdown header depth (e.g., 2 for ##, 3 for ###).
    #' @return A character string of assembled Markdown fragments.
    assemble_sections = function(parent_id, level) {
      pid <- as.character(parent_id)

      query <- "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains' ORDER BY sort_order ASC"
      res <- DBI::dbGetQuery(self$con, query, params = list(pid))

      if (is.null(res) || nrow(res) == 0) return("")

      output <- ""
      for (i in seq_len(nrow(res))) {
        cid <- res$object_id[i]

        # Load the child entity from DB
        section <- Entity$new(cid, self$con)

        label_text <- if(is.na(section$label) || is.null(section$label)) "Untitled" else section$label

        # Header with anchor
        header_prefix <- paste(rep("#", level), collapse = "")
        output <- paste0(output, header_prefix, " ", label_text, " {#sec-", cid, "}\n\n")

        # Section Text
        content <- section$get_prop("content")
        if (!is.null(content) && !is.na(content)) {
          output <- paste0(output, content, "\n\n")
        }

        # Table processing
        table_json <- section$get_prop("table_data")
        if (!is.null(table_json) && !is.na(table_json)) {
          df <- jsonlite::fromJSON(table_json)
          md_table <- knitr::kable(df, format = "pipe", caption = label_text)
          output <- paste0(output, paste(md_table, collapse = "\n"), "\n\n")
        }

        # Plot processing
        plot_path <- section$get_prop("plot_path")
        if (!is.null(plot_path) && !is.na(plot_path)) {
          # Convert relative path to absolute path so Pandoc can find it
          # winslash = "/" is important for cross-platform compatibility
          abs_path <- normalizePath(plot_path, winslash = "/", mustWork = FALSE)
          output <- paste0(output, "![", label_text, "](", abs_path, ")\n\n")
        }

        # Recursion
        sub_content <- self$assemble_sections(cid, level + 1)
        output <- paste0(output, sub_content)
      }
      return(output)
    },

    #' @description
    #' Performs a structural and content validation of the document.
    #' @details
    #' 1. Verifies the presence of mandatory section classes defined in \code{ont_constraints}.
    #' 2. Flags any section that lacks text, tables, and nested sub-sections.
    #' @return Logical. TRUE if the document is valid, FALSE if errors or warnings were found.
    validate = function() {
      message("Validating document: ", self$label)
      is_valid <- TRUE

      # 1. Check for mandatory ontology classes required for this document type
      query_req <- "SELECT required_child_class_id FROM ont_constraints WHERE parent_class_id = ?"
      required_classes <- DBI::dbGetQuery(self$con, query_req, params = list(self$class_id))$required_child_class_id

      # Fetch actual existing child instances
      query_act <- "
        SELECT i.class_id, i.instance_id, i.label
        FROM relations r
        JOIN instances i ON r.object_id = i.instance_id
        WHERE r.subject_id = ? AND r.predicate_id = 'contains'"
      actual_children <- DBI::dbGetQuery(self$con, query_act, params = list(self$id))

      # Compare required vs actual classes
      missing <- setdiff(required_classes, actual_children$class_id)
      if (length(missing) > 0) {
        warning("Missing mandatory sections: ", paste(missing, collapse = ", "))
        is_valid <- FALSE
      }

      # 2. Check for empty sections (no content, no table, no children)
      for (i in seq_len(nrow(actual_children))) {
        child_obj <- Entity$new(actual_children$instance_id[i], self$con)

        has_text <- !is.null(child_obj$get_prop("content")) && nchar(trimws(child_obj$get_prop("content"))) > 0
        has_table <- !is.null(child_obj$get_prop("table_data"))

        # Check if section contains sub-sections
        query_sub <- "SELECT COUNT(*) as count FROM relations WHERE subject_id = ? AND predicate_id = 'contains'"
        has_sub <- DBI::dbGetQuery(self$con, query_sub, params = list(child_obj$id))$count > 0

        if (!has_text && !has_table && !has_sub) {
          warning("Section is empty: ", actual_children$label[i])
          is_valid <- FALSE
        }
      }

      return(is_valid)
    }
  )
)
