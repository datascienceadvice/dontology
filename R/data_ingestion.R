#' Import CSV data into a Document Section Table
#'
#' @param section_id Character. The ID of the section containing the table.
#' @param csv_path Character. Path to the CSV file.
#' @param con DBIConnection.
#' @export
import_table_from_csv <- function(section_id, csv_path, con) {
  if (!file.exists(csv_path)) stop("File not found: ", csv_path)

  # 1. Читаем данные
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # 2. Загружаем объект секции через фабрику
  section <- .instantiate_entity(section_id, con)

  # 3. Записываем таблицу в метаданные
  # Наш метод set_table автоматически конвертирует DF в JSON
  section$set_table("table_data", df)

  # 4. Сохраняем изменения
  section$save()

  message("Successfully imported ", nrow(df), " rows into section: ", section_id)
}
