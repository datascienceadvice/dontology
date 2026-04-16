system2(rmarkdown::pandoc_exec(), args = c("-o", "inst/templates/styles.docx", "--print-default-data-file", "reference.docx"))

ps_cmd <- glue::glue(
  "$word = New-Object -ComObject Word.Application;
   $word.Visible = $false;

   Add-Type -AssemblyName 'Microsoft.Office.Interop.Word'
   $pStyle   = [Microsoft.Office.Interop.Word.WdStyleType]::wdStyleTypeParagraph
   $pJustify = [Microsoft.Office.Interop.Word.WdParagraphAlignment]::wdAlignParagraphJustify
   $pLeft    = [Microsoft.Office.Interop.Word.WdParagraphAlignment]::wdAlignParagraphLeft
   $pRight   = [Microsoft.Office.Interop.Word.WdParagraphAlignment]::wdAlignParagraphRight
   $pCenter  = [Microsoft.Office.Interop.Word.WdParagraphAlignment]::wdAlignParagraphCenter

   try {{
      $doc = $word.Documents.Open('{docx_path}');
      $doc.Convert();

      if ($doc -ne $null) {{

          $word.ActiveDocument.Styles('Abstract').QuickStyle = $false
          $word.ActiveDocument.Styles('Abstract Title').QuickStyle = $false
          $word.ActiveDocument.Styles('Author').QuickStyle = $false
          $word.ActiveDocument.Styles('Compact').QuickStyle = $false
          $word.ActiveDocument.Styles('First Paragraph').QuickStyle = $false
          $word.ActiveDocument.Styles('Дата').QuickStyle = $false
          $word.ActiveDocument.Styles('Список литературы').QuickStyle = $false
          $word.ActiveDocument.Styles('Основной текст').QuickStyle = $false
          $word.ActiveDocument.Styles('Заголовок').QuickStyle = $false
          $word.ActiveDocument.Styles('Заголовок оглавления').QuickStyle = $false
          $word.ActiveDocument.Styles('Figure').QuickStyle = $false
          $word.ActiveDocument.Styles('Footnote Block Text').QuickStyle = $false
          $word.ActiveDocument.Styles('Текст сноски').QuickStyle = $false
          $word.ActiveDocument.Styles('Цитата').QuickStyle = $false
          $word.ActiveDocument.Styles('Подзаголовок').QuickStyle = $false

          $word.ActiveDocument.Styles('Обычный').Font.Name                           = 'Times New Roman'
          $word.ActiveDocument.Styles('Обычный').Font.Size                           = 12
          $word.ActiveDocument.Styles('Обычный').Font.Color                          = -16777216
          $word.ActiveDocument.Styles('Обычный').Font.Bold                           = 0
          $word.ActiveDocument.Styles('Обычный').Font.Italic                         = 0
          $word.ActiveDocument.Styles('Обычный').Font.Underline                      = 0
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.FirstLineIndent     = 1.25 / 2.54 * 72
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.LeftIndent          = 0
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.LineSpacing         = 12
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.Alignment           = $pJustify
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.SpaceBeforeAuto     = $false
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.SpaceAfterAuto      = $false
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.SpaceBefore         = 0
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.SpaceAfter          = 0
          $word.ActiveDocument.Styles('Обычный').NoSpaceBetweenParagraphsOfSameStyle = $false
          $word.ActiveDocument.Styles('Обычный').ParagraphFormat.KeepWithNext        = $false

          $style = $doc.Styles.Item('Figure')
          $style.ParagraphFormat.Alignment           = $pCenter
          $style.ParagraphFormat.SpaceAfter          = 6


          # table --------------------------------------------------------------
          $tableNormal = $doc.Styles.Item('Обычная таблица');
          $tableNormal.Font.Size = 12
          $tableNormal.Font.Name = 'Times New Roman'

          $style = $doc.Styles.Item('Table');
          $style.Font.Name = 'Times New Roman'
          $style.Font.Size = 12;

          $style.ParagraphFormat.Alignment           = $pCenter
          $style.ParagraphFormat.LeftIndent          = 0;
          $style.ParagraphFormat.FirstLineIndent     = 0;
          $style.ParagraphFormat.RightIndent         = 0;


          $ts = $style.Table
          $ts.Alignment = 1; # 1 = wdAlignRowCenter

          $ts.Condition(0).Font.Bold = $true;
          $ts.Condition(0).Shading.BackgroundPatternColor = 16777215;
          $ts.Condition(0).Borders.InsideLineStyle        = 1
          $ts.Condition(0).Borders.OutsideLineStyle       = 1
          $ts.Condition(0).Borders.InsideLineWidth        = 4
          $ts.Condition(0).Borders.OutsideLineWidth       = 4

          $ts.Borders.InsideLineStyle       = 1
          $ts.Borders.OutsideLineStyle      = 1
          $ts.Borders.InsideLineWidth       = 4
          $ts.Borders.OutsideLineWidth      = 4




          # HEADINNGS ----------------------------------------------------------
          $template = $word.ListGalleries[3].ListTemplates[2];

          1..9 | ForEach-Object {{
              $lvl = $_;

              # Heading 1 ID = -2, Heading 2 ID = -3 и т.д.
              # Вычисляем системный ID:
              $styleId = -1 - $lvl;

              # Получаем стиль по ID (это надежнее, чем по имени)
              $sObj = $doc.Styles.Item($styleId);

              # 1. Привязка списка к стилю
              $level = $template.ListLevels[$lvl];
              $level.LinkedStyle = $sObj.NameLocal; # Берем имя прямо из системы
              $level.TextPosition = 0;
              $level.NumberPosition = 0;
              $level.TabPosition = 0;

              # 2. Связываем стиль с шаблоном списка
              $sObj.LinkToListTemplate($template);

              # 3. Настройка шрифта
              $f = $sObj.Font;
              $f.Name = 'Times New Roman';
              $f.Size = 14;
              $f.Bold = $true;
              $f.Italic = $false;
              $f.Color = -16777216;

              # 4. Настройка абзаца
              $p = $sObj.ParagraphFormat;
              $p.Alignment = 3; # wdAlignParagraphJustify
              $p.LineSpacing = 12;
              $p.SpaceBefore = 12;
              $p.SpaceAfter = 12;
              $p.KeepWithNext = $true;

              $sObj.NoSpaceBetweenParagraphsOfSameStyle = $false;
          }}

          # Compact ------------------------------------------------------------
          $compactStyle = $doc.Styles.Item('Compact')
          $compactStyle.ParagraphFormat.LeftIndent = 0
          $compactStyle.ParagraphFormat.FirstLineIndent = 0
          $compactStyle.ParagraphFormat.RightIndent = 0

          # 3. ВЫРАВНИВАНИЕ
          # Ставим по центру (1) или как вам нужно для Pharma-отчета
          $compactStyle.ParagraphFormat.Alignment = 1 # 1 = wdAlignParagraphCenter

          # 4. ИНТЕРВАЛЫ (делаем ячейки аккуратными)
          $compactStyle.ParagraphFormat.SpaceBefore = 2
          $compactStyle.ParagraphFormat.SpaceAfter = 2
          $compactStyle.ParagraphFormat.LineSpacingRule = 0 # wdLineSpaceSingle

          # 5. ШРИФТ (опционально, чтобы точно соответствовать отчету)
          $compactStyle.Font.Name = 'Times New Roman'
          $compactStyle.Font.Size = 12







          #$doc.Convert();
          $doc.Save();
          $doc.Close();

          Write-Host 'Success';
      }} else {{
          Write-Error 'Could not open document';
      }}
  }} finally {{
        $word.Quit();
        [System.Runtime.Interopservices.Marshal]::ReleaseComObject($word) | Out-Null
  }}",
  docx_path = normalizePath("inst/templates/styles.docx", winslash = "/"))

system2("powershell.exe", args = c(
  "-NoProfile",
  "-ExecutionPolicy", "Bypass",
  "-Command", paste0("& {", ps_cmd, "}")
), stdout = TRUE, stderr = TRUE)
