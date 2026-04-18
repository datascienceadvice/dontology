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
    initialize = function(id, con, label = NULL, class_id = "Document") {
      # super$initialize calls the parent Entity class constructor
      super$initialize(id, con, label, class_id = "Document")
    },

    render = function(level = 1, context = NULL, include_signatures = TRUE) {
      message("Assembling document: ", self$label)

      # 1. Prepare variable context
      # If context is not passed (we are at root), start with our own metadata
      # If passed — merge with current
      if (is.null(context)) {
        context <- self$metadata
      } else {
        context <- utils::modifyList(context, self$metadata)
      }
      context$current_date <- as.character(Sys.Date())

      # 2. Document header
      resolved_lbl <- .resolve_cross_refs(self$label, self$con)
      lbl <- .fill_placeholders(resolved_lbl, context)

      header_prefix <- paste(rep("#", level), collapse = "")
      doc_text <- paste0(header_prefix, " ", lbl, "\n\n")

      # 3. Version
      version <- self$get_prop("version")
      if (!is.null(version) && !is.na(version)) {
        doc_text <- paste0(doc_text, "*Version: ", version, "*\n\n")
      }

      # 4. Handle ROOT CONTENT
      root_content_raw <- self$get_prop("content")
      if (!is.null(root_content_raw)) {
        resolved_root <- .resolve_cross_refs(root_content_raw, self$con)
        doc_text <- paste0(doc_text, .fill_placeholders(resolved_root, context), "\n\n")
      }

      # 5. Assemble child sections
      content_parts <- self$assemble_sections(self$id, level = level + 1, context = context)

      full_output <- paste0(doc_text, content_parts)

      # 6. Append signature table at the very END of the document
      if (include_signatures) {
        sigs <- self$get_signatures()
        if (nrow(sigs) > 0) {
          full_output <- paste0(full_output, "\n\n---\n### Electronic Signatures\n\n")
          sig_table <- knitr::kable(sigs, format = "pipe")
          full_output <- paste0(full_output, paste(sig_table, collapse = "\n"), "\n\n")
        }
      }

      full_output <- .resolve_cross_refs(full_output, self$con)
      return(full_output)
    },

    assemble_sections = function(parent_id, level, context) { # Added context to arguments!
      pid <- as.character(parent_id)
      query <- "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains' ORDER BY sort_order ASC"
      res <- DBI::dbGetQuery(self$con, query, params = list(pid))

      if (is.null(res) || nrow(res) == 0) return("")

      output <- ""
      for (i in seq_len(nrow(res))) {
        cid <- res$object_id[i]
        section <- .instantiate_entity(cid, self$con)

        # KEY MOMENT:
        # Each section receives an updated context:
        # what came from above + its own metadata
        local_context <- utils::modifyList(context %||% list(), section$metadata)
        # Add system variables
        local_context$current_date <- as.character(Sys.Date())

        if (inherits(section, "DocumentInstance") && section$id != self$id) {
          # Nested document: render it, passing current level
          output <- paste0(output, section$render(level = level, context = local_context))
        } else {
          # 1. Get "raw" text
          raw_label <- section$label %||% "Untitled"
          raw_content <- section$get_prop("content") %||% ""

          # 2. FIRST resolve cross-references [[REF:...]] and [[VAL:...]]
          # This may bring new placeholders {{...}} into the text
          resolved_label <- .resolve_cross_refs(raw_label, self$con)
          resolved_content <- .resolve_cross_refs(raw_content, self$con)

          # 3. THEN substitute placeholders {{...}}
          sec_label <- .fill_placeholders(resolved_label, local_context)
          content <- .fill_placeholders(resolved_content, local_context)

          header_prefix <- paste(rep("#", level), collapse = "")
          output <- paste0(output, header_prefix, " ", sec_label, " {#sec-", cid, "}\n\n")

          # 4. Then proceed with normal section rendering...
          if (!is.null(content) && !is.na(content)) {
            output <- paste0(output, content, "\n\n")
          }

          # Tables
          table_json <- section$get_prop("table_data")
          if (!is.null(table_json) && !is.na(table_json)) {
            df <- jsonlite::fromJSON(table_json)
            # Use sec_label for table caption
            md_table <- knitr::kable(df, format = "pipe", caption = sec_label)
            output <- paste0(output, paste(md_table, collapse = "\n"), "\n\n")
          }

          # Plots
          plot_path <- section$get_prop("plot_path")
          if (!is.null(plot_path) && !is.na(plot_path)) {
            abs_path <- normalizePath(plot_path, winslash = "/", mustWork = FALSE)
            output <- paste0(
              output,
              "::: {custom-style=\"Figure\"}\n",
              "![", sec_label, "](", abs_path, ")\n", # Use sec_label
              ":::\n\n"
            )
          }

          # Recursion for deeply nested sections (pass context)
          sub_content <- self$assemble_sections(cid, level + 1, local_context)
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
      message("--- Comprehensive Validation: ", self$label, " ---")

      # Run validation chain
      results <- c(
        self$validate_integrity(),   # Hash verification (changes bypassing the system)
        self$validate_structure(),   # Ontology and hierarchy
        self$validate_content(),     # Texts and links
        self$validate_consistency()  # Data contradictions
      )

      is_valid <- all(results)
      if (is_valid) message("SUCCESS: Document is compliant.") else warning("FAILURE: Compliance issues found.")
      return(is_valid)
    },

    validate_integrity = function() {
      # Check if someone modified the database directly (see point 4 below)
      # Method will be defined in Entity
      return(super$verify_checksum())
    },

    validate_structure = function() {
      # Mandatory sections logic (ont_constraints)
      query_req <- "SELECT required_child_class_id FROM ont_constraints WHERE parent_class_id = ?"
      required_classes <- DBI::dbGetQuery(self$con, query_req, params = list(self$class_id))$required_child_class_id

      actual_children <- DBI::dbGetQuery(self$con,
                                         "SELECT class_id FROM relations r JOIN instances i ON r.object_id = i.instance_id
         WHERE r.subject_id = ? AND r.predicate_id = 'contains'", params = list(self$id))

      missing <- setdiff(required_classes, actual_children$class_id)
      if (length(missing) > 0) {
        warning("Structure: Missing mandatory classes: ", paste(missing, collapse = ", "))
        return(FALSE)
      }
      return(TRUE)
    },

    validate_content = function() {
      message("Checking content and recursive structure...")
      links_ok <- self$validate_links()
      is_valid <- TRUE # Local flag for this method

      # Get list of children for emptiness and recursion checks
      query_act <- "
        SELECT class_id, instance_id, label
        FROM relations r
        JOIN instances i ON r.object_id = i.instance_id
        WHERE r.subject_id = ? AND r.predicate_id = 'contains'"
      actual_children <- DBI::dbGetQuery(self$con, query_act, params = list(self$id))

      if (nrow(actual_children) > 0) {
        for (i in seq_len(nrow(actual_children))) {
          child_obj <- .instantiate_entity(actual_children$instance_id[i], self$con)

          # RECURSION
          if (inherits(child_obj, "DocumentInstance")) {
            if (!child_obj$validate()) is_valid <- FALSE
          } else {
            # Type validation in section
            if (!child_obj$validate_ontology_types()) is_valid <- FALSE

            # EMPTY CHECK
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
      }
      # Return combined result
      return(is_valid && links_ok)
    },

    validate_consistency = function() {
      message("Checking semantic consistency...")

      # Helper function to collect conflicts
      # Extracted to avoid deep nesting
      collect_conflicts <- function(ent) {
        conflicts_df <- ent$find_contradictions()

        # Get child IDs directly from relations to avoid creating extra objects
        child_ids <- DBI::dbGetQuery(ent$con,
                                     "SELECT object_id FROM relations WHERE subject_id = ? AND predicate_id = 'contains'",
                                     params = list(ent$id))$object_id

        for (cid in child_ids) {
          child_obj <- .instantiate_entity(cid, ent$con)
          conflicts_df <- rbind(conflicts_df, collect_conflicts(child_obj))
        }
        return(conflicts_df)
      }

      all_conflicts <- collect_conflicts(self)

      if (nrow(all_conflicts) > 0) {
        # Group errors by severity
        has_errors <- any(all_conflicts$severity == "ERROR")

        for (i in seq_len(nrow(all_conflicts))) {
          msg <- sprintf("CONTRADICTION [%s]: Property '%s' in '%s' is '%s', but parent context says '%s'",
                         all_conflicts$severity[i], all_conflicts$property_id[i],
                         all_conflicts$instance_id[i], all_conflicts$local_value[i],
                         all_conflicts$inherited_value[i])

          if (all_conflicts$severity[i] == "ERROR") warning(msg) else message("ADVISORY: ", msg)
        }
        return(!has_errors) # Return FALSE if there is at least one ERROR
      }
      return(TRUE)
    },

    #' @description
    #' Check all internal links (#sec-ID) for broken references.
    validate_links = function() {
      message("Checking internal links for: ", self$label)

      # 1. Collect all document text (recursively)
      get_all_text <- function(entity) {
        text <- entity$get_prop("content") %||% ""

        # Get children
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

      # 2. Search for link patterns like (#sec-ID)
      # Regular expression finds content inside (#sec-...)
      links <- regmatches(full_text, gregexpr("(?<=#sec-)[a-zA-Z0-9_-]+", full_text, perl = TRUE))[[1]]
      links <- unique(links)

      if (length(links) == 0) return(TRUE)

      # 3. Check that each ID exists in the database
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
      on.exit(DBI::dbDisconnect(temp_con))

      create_schema(temp_con)
      old_id <- import_from_json(json_path, temp_con)
      old_doc <- .instantiate_entity(old_id, temp_con)

      diff_report <- diff_hierarchy(old_doc, self)

      return(diff_report)
    },

    #' @description
    #' Automatically increment the version of the document.
    #' @param type Character. "minor" (1.0 -> 1.1) or "major" (1.0 -> 2.0).
    bump_version = function(type = "minor") {
      # Disallow version change if status is FINAL
      if (toupper(self$get_prop("status") %||% "") == "FINAL") {
        stop("Cannot bump version of a FINAL document. Set status to DRAFT first.")
      }

      current_v <- self$get_prop("version")
      if (is.null(current_v) || is.na(current_v)) current_v <- "0.0"

      # Parse version (major.minor)
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
    },

    #' @description
    #' Sign the document version with a cryptographic hash of the content.
    #' @param role Character. "Author", "Reviewer", or "Approver".
    #' @param meaning Character. Reason for signing.
    sign = function(role, meaning = "I approve this document") {
      # 1. Generate final content (with all placeholders and cross-references)
      rendered_text <- self$render(include_signatures = FALSE)

      # 2. Create unique "fingerprint" (SHA-256 hash) of this text
      # Uses digest package. If missing: install.packages("digest")
      content_hash <- digest::digest(rendered_text, algo = "sha256")

      curr_user <- Sys.getenv("DONTOLOGY_USER", unset = Sys.info()[["user"]])
      curr_ver <- self$get_prop("version") %||% "0.0"

      # 3. Write signature WITH HASH to database
      DBI::dbExecute(self$con,
                     "INSERT INTO signatures (instance_id, version, user_id, role, meaning, content_hash)
         VALUES (?, ?, ?, ?, ?, ?)",
                     params = list(self$id, curr_ver, curr_user, role, meaning, content_hash)
      )

      message(sprintf("Document '%s' signed. Fingerprint: %s...", self$id, substr(content_hash, 1, 10)))

      # 4. If Approver signed — lock the document
      if (toupper(role) == "APPROVER") {
        self$set_prop("status", "FINAL")
        self$save()
        message("Lifecycle: Document is now FINAL and LOCKED for changes.")
      }
    },

    #' @description
    #' Get all signatures for this document.
    get_signatures = function() {
      DBI::dbGetQuery(self$con,
                      "SELECT user_id, role, version, timestamp, meaning, content_hash
         FROM signatures WHERE instance_id = ? ORDER BY timestamp ASC",
                      params = list(self$id))
    },

    #' @description
    #' Verify if existing signatures still match the current document content.
    #' @return A data.frame with verification status for each signature.
    verify_signatures = function() {
      sigs <- DBI::dbGetQuery(self$con,
                              "SELECT user_id, role, content_hash FROM signatures WHERE instance_id = ?",
                              params = list(self$id))

      if (nrow(sigs) == 0) {
        message("No signatures found to verify.")
        return(NULL)
      }

      # Generate current hash
      current_content <- self$render(include_signatures = FALSE)
      current_hash <- digest::digest(current_content, algo = "sha256")

      # Verify each signature
      sigs$is_valid <- sigs$content_hash == current_hash

      if (any(!sigs$is_valid)) {
        warning("DATA INTEGRITY BREACH: One or more signatures do not match the current content!")
      } else {
        message("All signatures verified successfully.")
      }

      return(sigs)
    }
  )
)
