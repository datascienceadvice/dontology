local tablestyle = "Table"

function Table(elem)

    if elem.attr.attributes["custom-style"] == nil then
        elem.attr.attributes["custom-style"] = tablestyle
    end

    local num_columns = #elem.colspecs
    local width_per_column = 1.0 / num_columns

    local new_specs = {}
    for i = 1, num_columns do
        local align = elem.colspecs[i][1]
        table.insert(new_specs, {align, width_per_column})
    end

    elem.colspecs = new_specs

    return elem
end