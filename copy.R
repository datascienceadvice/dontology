copy_code_to_clipboard <- function() {
  # 1. Собираем файлы из R/ и из tests/ (рекурсивно)
  r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]$")
  test_files <- list.files("tests", full.names = TRUE, pattern = "\\.[Rr]$", recursive = TRUE)

  all_files <- c(r_files, test_files)

  if (length(all_files) == 0) {
    stop("Файлы в папках R/ или tests/ не найдены.")
  }

  full_text <- ""

  for (f in all_files) {
    # Читаем содержимое файла
    if (file.exists(f)) {
      content <- readLines(f, warn = FALSE)

      # Формируем блок текста с заголовком
      file_block <- paste0(
        "\n==================================================\n",
        "FILE: ", f, "\n",
        "==================================================\n",
        paste(content, collapse = "\n"),
        "\n"
      )

      full_text <- paste0(full_text, file_block)
    }
  }

  # 2. Копируем в буфер обмена (зависит от ОС)
  os <- Sys.info()["sysname"]

  tryCatch({
    if (os == "Windows") {
      writeClipboard(full_text)
    } else {
      # Для Mac/Linux
      con <- pipe("pbcopy", "w") # для Mac
      if (os == "Linux") con <- pipe("xclip -selection clipboard", "w")
      writeLines(full_text, con)
      close(con)
    }
    message("Успех! Содержимое папок R/ и tests/ скопировано в буфер обмена.")
  }, error = function(e) {
    cat(full_text)
    message("\n--- Не удалось скопировать в буфер автоматически. Код выведен в консоль выше. ---")
  })
}

# Запуск
copy_code_to_clipboard()
