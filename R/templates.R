#' Instantiate a Template
#'
#' @description
#' Creates a deep copy of a document hierarchy. Metadata is preserved,
#' but lifecycle attributes (status, version) are reset.
#'
#' @param template_id Character. The ID of the document/section to clone.
#' @param new_root_id Character. The ID for the new document.
#' @param new_label Character. The title for the new document.
#' @param con DBIConnection.
#'
#' @return A new DocumentInstance object.
#' @export
instantiate_template <- function(template_id, new_root_id, new_label, con) {

  # Внутренняя функция для рекурсивного клонирования узлов
  clone_node <- function(old_id, is_root = FALSE) {
    # 1. Загружаем старый объект
    old_obj <- .instantiate_entity(old_id, con)

    # 2. Генерируем новый ID
    # Для корня используем присланный new_root_id, для детей - суффикс
    current_new_id <- if(is_root) new_root_id else paste0(new_root_id, "_", old_id)

    # 3. Создаем новый экземпляр
    new_obj <- Entity$new(
      id = current_new_id,
      con = con,
      label = if(is_root) new_label else old_obj$label,
      class_id = old_obj$class_id
    )

    # 4. Копируем метаданные (кроме системных)
    new_metadata <- old_obj$metadata

    # Сбрасываем жизненный цикл для нового документа
    new_metadata$status <- "DRAFT"
    new_metadata$version <- "0.1"
    # Удаляем путь к графику, если он был (в шаблоне графиков обычно нет)
    new_metadata$plot_path <- NULL

    new_obj$metadata <- new_metadata
    new_obj$save()

    # 5. Рекурсивно клонируем детей
    rel_df <- old_obj$relations$contains
    if (!is.null(rel_df) && nrow(rel_df) > 0) {
      for (i in seq_len(nrow(rel_df))) {
        old_child_id <- rel_df$object_id[i]

        # Клонируем ребенка
        new_child_id <- clone_node(old_child_id, is_root = FALSE)

        # Связываем нового родителя с новым ребенком
        new_obj$add_relation("contains", new_child_id, sort_order = i)
      }
      new_obj$save()
    }

    return(current_new_id)
  }

  message("Creating document '", new_label, "' from template '", template_id, "'...")

  # Запускаем процесс с корня
  final_root_id <- clone_node(template_id, is_root = TRUE)

  return(.instantiate_entity(final_root_id, con))
}
