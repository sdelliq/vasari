wb <- createWorkbook()
addWorksheet(wb, sheetName = "Report")

showGridLines(wb, sheet = 1, showGridLines = FALSE)


setColWidths(wb, sheet=1,cols = 1:7,widths = "auto")


percentage_rows <- createStyle(numFmt = "0.00%",fontSize = 10,halign = "right",valign = "center",fontColour = "black",wrapText = FALSE)
Milion_rows <- createStyle(
  numFmt = "0.0,,\"M\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
thousands_rows <- createStyle(
  numFmt = "0.0,\"k\"",
  fontSize = 10,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)


stringa <- " Report \n  "
lines <- unlist(strsplit(stringa, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,1,df$Text[i],1,i)
}

writeData(wb, sheet = "Report", x = "Totals", startCol = 1, startRow = 3)
writeDataTable(wb, 1, x = totals , startRow = 4,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = 5, cols = 2 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = thousands_rows, rows = 5, cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Loan by type", startCol = 1, startRow = 9)
writeDataTable(wb, 1, x = loans.by.type , startRow = 10,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(11:14), cols = 5 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(11:14), cols = c(4,6) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Loan by gbv range", startCol = 1, startRow = 19)
writeDataTable(wb, 1, x = loans.by.gbv.range , startRow = 20,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(21:29), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(21:29), cols = 4 ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Entity by type", startCol = 1, startRow = 35)
writeDataTable(wb, 1, x = ent.by.type , startRow = 36,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(37:39), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(37:39), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Entity by area", startCol = 1, startRow = 45)
writeDataTable(wb, 1, x = ent.by.area , startRow = 46,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = thousands_rows, rows = c(47:52), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(47:52), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Top 5 province", startCol = 1, startRow = 60)
writeDataTable(wb, 1, x = Top_5_province_by_gbv , startRow = 61,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
addStyle(wb, sheet = "Report", style = thousands_rows, rows = c(62:66), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(62:66), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)



insertImage(wb,sheet = "Report","File/loan.type.png",startCol = 9, startRow = 9, width = 4.5, height = 4.5, dpi = 300)
insertImage(wb,sheet = "Report","File/entity.type.png",startCol = 9, startRow = 20, width = 4.5, height = 4.5, dpi = 300)
insertImage(wb,sheet = "Report","File/Pie_Chart.png",startCol = 9, startRow = 41, width = 4.5, height = 4.5, dpi = 300)
insertImage(wb,sheet = "Report","File/province_plot.png",startCol = 9, startRow = 31, width = 4.5, height = 4.5, dpi = 300)

saveWorkbook(wb, file = "File/Teaser.xlsx", overwrite = TRUE)