#' Export a Document to a Specified Format
#'
#' @description
#' Renders a \code{DocumentInstance} object into Markdown and then converts it to
#' the desired output format (Word, PDF, HTML, or MD) using Pandoc.
#' For Word output, it automatically applies a Lua filter to ensure
#' consistent table styling and borders.
#'
#' @param doc An object of class \code{DocumentInstance}.
#' @param format Character. Output format: "word", "pdf", "html", or "md".
#' Defaults to "word".
#' @param file_path Character. Full destination path including filename
#' (e.g., "C:/Documents/Report_V1.docx").
#'
#' @return The path to the generated file (invisibly).
#'
#' @importFrom rmarkdown render word_document pdf_document
#' @export
export_document <- function(doc, format = "word", file_path) {

  # 1. VALIDATION: Check if the format is supported
  allowed_formats <- c("word", "pdf", "html", "md")
  if (!(format %in% allowed_formats)) {
    stop(paste("Unsupported format:", format, ". Allowed formats are:",
               paste(allowed_formats, collapse = ", ")))
  }

  # Generate Markdown content
  md_content <- doc$render()

  # Handle raw Markdown export
  if (format == "md") {
    writeLines(md_content, file_path, useBytes = TRUE)
    message("Markdown file saved to: ", file_path)
    return(invisible(file_path))
  }

  # Intermediate temp file for Pandoc
  temp_md <- tempfile(fileext = ".md")
  writeLines(md_content, temp_md, useBytes = TRUE)
  on.exit(if (file.exists(temp_md)) unlink(temp_md))

  # 2. Setup format (No more "else" fall-through for unknown formats)
  if (format == "word") {
    tbl_filter <- system.file("lua", "table-filter.lua", package = "dontology")
    img_filter <- system.file("lua", "image-filter.lua", package = "dontology")
    style_path <- system.file("templates", "styles.docx", package = "dontology")

    p_args <- c()
    if (file.exists(img_filter)) p_args <- c(p_args, "--lua-filter", img_filter)
    if (file.exists(tbl_filter)) p_args <- c(p_args, "--lua-filter", tbl_filter)

    output_fmt <- rmarkdown::word_document(
      reference_docx = if (file.exists(style_path)) style_path else NULL,
      pandoc_args = p_args
    )

  } else if (format == "pdf") {
    output_fmt <- rmarkdown::pdf_document(
      latex_engine = "xelatex",
      pandoc_args = c("--variable", "mainfont=Arial")
    )

  } else if (format == "html") {
    output_fmt <- rmarkdown::html_document()
  }

  # 3. Render
  rmarkdown::render(
    input = temp_md,
    output_format = output_fmt,
    output_file = basename(file_path),
    output_dir = dirname(file_path),
    quiet = TRUE
  )

  message("Document (", format, ") successfully created at: ", file_path)
  return(invisible(file_path))
}
