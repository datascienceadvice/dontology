# plot_path <- normalizePath("vignettes/plots/SEC_RES_20260413162019.png", winslash = "/")
#
# md_content <- c(
#   "# Testing Image Styles",
#   "",
#   "This is a paragraph before the image.",
#   "",
#   "::: {custom-style=\"Figure\"}",
#   paste0("![A beautiful plot](", plot_path, ")"),
#   ":::",
#   "",
#   "This is a paragraph after the image."
# )
# writeLines(md_content, "D:/temp.md")
#
# system2(rmarkdown::pandoc_exec(), args = c("-o", "custom-reference.docx", "--print-default-data-file", "reference.docx"))
#
# ps_cmd <- glue::glue(
#   "$word = New-Object -ComObject Word.Application;
#    $word.Visible = $false;
#      try {{
#          $doc = $word.Documents.Open('{docx_path}');
#          if ($doc -ne $null) {{
#              try {{
#                  $style = $doc.Styles.Item('{style_name}')
#              }} catch {{
#                  $style = $doc.Styles.Add('{style_name}', 1)
#              }}
#              $style.ParagraphFormat.Alignment = 1; # Center
#              $doc.Save();
#              $doc.Close();
#              Write-Host 'Success';
#          }} else {{
#              Write-Error 'Could not open document';
#          }}
#      }} finally {{
#          $word.Quit();
#          [System.Runtime.Interopservices.Marshal]::ReleaseComObject($word) | Out-Null
#      }}",
#   docx_path = normalizePath("custom-reference.docx", winslash = "/"),
#   style_name = "Figure")

# system2("powershell.exe", args = c(
#   "-NoProfile",
#   "-ExecutionPolicy", "Bypass",
#   "-Command", paste0("& {", ps_cmd, "}")
# ), stdout = TRUE, stderr = TRUE)

rmarkdown::render(
  input = "D:/temp.md",
  output_format = rmarkdown::word_document(
    reference_docx = normalizePath("inst/templates/styles.docx", winslash = "/"),
    pandoc_args = c("--lua-filter", normalizePath("inst/lua/table-filter.lua", winslash = "/"))
  ),
  output_file = "md.docx",
  output_dir = "D:/Projects/dontology/",
  quiet = FALSE # Set to FALSE to see Pandoc logs
)

