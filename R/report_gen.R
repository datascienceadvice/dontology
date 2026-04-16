#' Generate Summary of Changes Report
#' @param doc DocumentInstance object
#' @param snapshot_path Path to older JSON version
#' @param output_path Where to save the MD report
#' @export
generate_change_summary <- function(doc, snapshot_path, output_path) {
  changes <- doc$compare_with_snapshot(snapshot_path) # Здесь вызовется наш новый глубокий дифф

  md <- c(
    paste("# Summary of Changes"),
    paste("## Document:", doc$label),
    paste("Date:", Sys.Date()),
    "",
    "| Location (Path) | Component | Change Type | Old Value | New Value |",
    "|---------------|-----------|-------------|-----------|-----------|"
  )

  if(nrow(changes) == 0) {
    md <- c(md, "| N/A | N/A | No changes detected | - | - |")
  } else {
    for(i in 1:nrow(changes)) {
      row <- changes[i, ]
      md <- c(md, sprintf("| %s | %s | %s | %s | %s |",
                          row$path, row$component, row$status, row$old_value, row$new_value))
    }
  }

  writeLines(md, output_path)
  message("Change summary generated: ", output_path)
}
