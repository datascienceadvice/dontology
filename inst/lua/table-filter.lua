local tablestyle = "TableCustom"

function Table(elem)
    -- 1. Назначаем стиль (как ты и хотел)
    if elem.attr.attributes["custom-style"] == nil then
        elem.attr.attributes["custom-style"] = tablestyle
    end

    -- 2. Автоподбор ширины (Auto-layout to 100% width)
    -- Мы берем количество колонок и делим 1.0 (100% ширины) на это количество
    local num_columns = #elem.colspecs
    local width_per_column = 1.0 / num_columns

    local new_specs = {}
    for i = 1, num_columns do
        -- Сохраняем выравнивание (alignment), но меняем ширину на долю от страницы
        local align = elem.colspecs[i][1]
        table.insert(new_specs, {align, width_per_column})
    end

    elem.colspecs = new_specs

    return elem
end