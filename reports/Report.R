wb <- createWorkbook()
addWorksheet(wb, sheetName = "Report")
addWorksheet(wb, sheetName = "Report_Agreement")

showGridLines(wb, sheet = 1, showGridLines = FALSE)
showGridLines(wb, sheet = 2, showGridLines = FALSE)

setColWidths(wb, sheet=1,cols = 1:7,widths = "auto")
setColWidths(wb, sheet=2,cols = 1:7,widths = "auto")

percentage_rows <- createStyle(
  numFmt = "0.0%",
  fontSize = 11,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE)
Milion_rows <- createStyle(
  numFmt = "0.0,,\"M\"",
  fontSize = 11,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
thousands_rows <- createStyle(
  numFmt = "0.0,\"k\"",
  fontSize = 11,
  halign = "right",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
title_rows <- createStyle(
  fontSize = 12,
  textDecoration = "bold",
  halign = "left",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
titolone <- createStyle(
  fontSize = 14,
  textDecoration = c("bold","underline"),
  halign = "left",
  valign = "center",
  fontColour = "black",
  wrapText = FALSE
)
total_rows <- createStyle(
  fontSize = 11,
  fgFill = "#B7DFEF",
  border="bottom",
  borderStyle = "medium",
  borderColour = "#283DBB",
  fontColour = "black",
  wrapText = FALSE
)
subtotal_rows <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  fgFill = "#E4F0FE",
  border="bottom",
  fontColour = "black",
  wrapText = FALSE
)

subtotal_rows_yellow <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  fgFill = "#FFFCF1",
  border="bottom",
  fontColour = "black",
  wrapText = FALSE
)
subtotal_rows_green <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  fgFill = "#EFFFE6",
  border="bottom",
  fontColour = "black",
  wrapText = FALSE
)
subtotal_rows_red <- createStyle(
  textDecoration = "bold",
  fontSize = 11,
  fgFill = "#FFF1F1",
  border="bottom",
  fontColour = "black",
  wrapText = FALSE
)
cell_format <- createStyle(numFmt = "0;0;-")

stringa <- " Report \n  "
lines <- unlist(strsplit(stringa, "\n"))
df <- data.frame(Text = lines)
for(i in 1:nrow(df)){
  writeData(wb,1,df$Text[i],1,i)
}
addStyle(wb, sheet = "Report", style = titolone, rows = 1, cols = 1 ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Totals", startCol = 1, startRow = 3)
addStyle(wb, sheet = "Report", style = title_rows, rows = 3, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = totals , startRow = 4,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = 5, cols = 2 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = thousands_rows, rows = 5, cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Loan by type", startCol = 1, startRow = 7)
addStyle(wb, sheet = "Report", style = title_rows, rows = 7, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = loans.by.type , startRow = 8,
               startCol = 1,  withFilter = FALSE, tableStyle = "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(9:12), cols = 4 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(9:12), cols = c(3,5) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 9, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report", x = "Loan by gbv range", startCol = 1, startRow = 14)
addStyle(wb, sheet = "Report", style = title_rows, rows = 14, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = loans.by.gbv.range , startRow = 15,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(16:24), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(16:24), cols = 4 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 16, cols = c(1:4) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report", x = "Ndg by type", startCol = 1, startRow = 22)
addStyle(wb, sheet = "Report", style = title_rows, rows = 22, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = ent.by.type , startRow = 23,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(24:26), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(24:26), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 24, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report", x = "Ndg by area", startCol = 1, startRow = 28)
addStyle(wb, sheet = "Report", style = title_rows, rows = 28, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = ent.by.area , startRow = 29,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(30:35), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(30:35), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 30, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Top 5 province by GBV ", startCol = 1, startRow = 37)
addStyle(wb, sheet = "Report", style = title_rows, rows = 37, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = Top_5_province_by_gbv , startRow = 38,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(39:45), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(39:45), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 39, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report", x = "Solvency PF for Ndg", startCol = 1, startRow = 47)
addStyle(wb, sheet = "Report", style = title_rows, rows = 47, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = ent.by.solvency , startRow = 48,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(49:55), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(49:55), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)
writeData(wb, sheet = "Report", x = "* refers to corporate", startCol = 1, startRow = 56)
addStyle(wb, sheet = "Report", style = total_rows, rows = 49, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report", x = "Borrowers with Guarantors", startCol = 1, startRow = 58)
addStyle(wb, sheet = "Report", style = title_rows, rows = 58, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = borrowers.with.guarantors , startRow = 59,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")

writeData(wb, sheet = "Report", x = "Corporate's Status", startCol = 1, startRow = 63)
addStyle(wb, sheet = "Report", style = title_rows, rows = 63, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = corporate.status , startRow = 64,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(65:73), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(65:73), cols = c(4:5) ,stack = TRUE,gridExpand = TRUE)
writeData(wb, sheet = "Report", x = "* refers to individual", startCol = 1, startRow = 74)
addStyle(wb, sheet = "Report", style = total_rows, rows = 65, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report", x = "Loans by vintage and GBV range", startCol = 1, startRow = 76)
addStyle(wb, sheet = "Report", style = title_rows, rows = 76, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 1, x = loans.vintage , startRow = 77,
               startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report", style = Milion_rows, rows = c(78:93), cols = 4 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = percentage_rows, rows = c(78:93), cols = 5 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = total_rows, rows = 78, cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report", style = subtotal_rows, rows = c(83,88,93), cols = c(1:5) ,stack = TRUE,gridExpand = TRUE)


insertImage(wb,sheet = "Report","File/loan.type.png",startCol = 8, startRow = 3, width = 4.5, height = 4.85, dpi = 300)
insertImage(wb,sheet = "Report","File/entity.type.png",startCol = 14, startRow = 3, width = 4.5, height = 4.85, dpi = 300)
insertImage(wb,sheet = "Report","File/Pie_Chart.png",startCol = 8, startRow = 30, width = 4, height = 4, dpi = 300)
insertImage(wb,sheet = "Report","File/province_plot.png",startCol = 14, startRow = 30, width = 4, height = 4, dpi = 300)
insertImage(wb,sheet = "Report","File/corporate.status.png",startCol = 10, startRow = 57, width = 6, height = 5, dpi = 300)


# writeData(wb,2,'Report Entities',1,1)
# 
# writeData(wb, sheet = "Report_Entities", x = "Entity by type", startCol = 1, startRow = 3)
# writeDataTable(wb, 2, x = type.entities , startRow = 4, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
# 
# area.entities[is.na(area.entities)] <- "N/A"
# writeData(wb, sheet = "Report_Entities", x = "Entity by area", startCol = 1, startRow = 8)
# writeDataTable(wb, 2, x = area.entities , startRow = 9, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
# 
# writeData(wb, sheet = "Report_Entities", x = "Entity by province (Top 5)", startCol = 1, startRow = 17)
# writeDataTable(wb, 2, x = province.entities , startRow = 18, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")
# 
# solvency.pf.entities[is.na(solvency.pf.entities)] <- "N/A"
# writeData(wb, sheet = "Report_Entities", x = "Entity by solvency", startCol = 1, startRow = 25)
# writeDataTable(wb, 2, x = solvency.pf.entities , startRow = 26, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleMedium2")




writeData(wb,2,'Report Agreement',1,1)
addStyle(wb, sheet = "Report_Agreement", style = titolone, rows = 1, cols = 1 ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report_Agreement", x = "Totals", startCol = 1, startRow = 3)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 3, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = total.pdr , startRow = 4, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report_Agreement", style = thousands_rows, rows = 5, cols = c(2:3) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report_Agreement", x = "Agreement by status", startCol = 1, startRow = 8)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 8, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = agreemeent.by.status , startRow = 9, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report_Agreement", style = percentage_rows, rows = c(10:13), cols = 3 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = total_rows, rows = 10, cols = c(1:3) ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report_Agreement", x = "Agreement by status and discount", startCol = 1, startRow = 16)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 16, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = agreement.summary.discount , startRow = 17, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report_Agreement", style = total_rows, rows = 18, cols = c(1:6) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = subtotal_rows_red, rows = 22, cols = c(1:6) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = subtotal_rows_green, rows = 30, cols = c(1:6) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = subtotal_rows_yellow, rows = 26, cols = c(1:6) ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = thousands_rows, rows = c(18:30), cols = c(4:6) ,stack = TRUE,gridExpand = TRUE)
for(i in 1:nrow(agreement.summary.discount)){
  for (j in 1:ncol(agreement.summary.discount)) {
   if(agreement.summary.discount[[i,j]]==0){
     addStyle(wb, sheet = "Report_Agreement", style = cell_format, rows = i+17, cols = j ,stack = TRUE,gridExpand = TRUE)
   }
  }
}
#addStyle(wb, sheet = "Report_Agreement", style = cell_format, rows = c(19:30), cols = c(4:6) ,stack = TRUE,gridExpand = TRUE)


writeData(wb, sheet = "Report_Agreement", x = "Agreement by role and status", startCol = 1, startRow = 33)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 33, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = agree.by.role.status , startRow = 34, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report_Agreement", style = percentage_rows, rows = c(35:39), cols = 4 ,stack = TRUE,gridExpand = TRUE)
addStyle(wb, sheet = "Report_Agreement", style = thousands_rows, rows = c(35:39), cols = 5 ,stack = TRUE,gridExpand = TRUE)



writeData(wb, sheet = "Report_Agreement", x = "Mean installments by amount", startCol = 1, startRow = 42)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 42, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = agreement.summary.ninstallment , startRow = 43, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
#addStyle(wb, sheet = "Report_Agreement", style = percentage_rows, rows = c(41:), cols = 3 ,stack = TRUE,gridExpand = TRUE)

writeData(wb, sheet = "Report_Agreement", x = "Failed Agreements ", startCol = 1, startRow = 49)
addStyle(wb, sheet = "Report_Agreement", style = title_rows, rows = 49, cols = 1 ,stack = TRUE,gridExpand = TRUE)
writeDataTable(wb, 2, x = failed , startRow = 50, startCol = 1,  withFilter = FALSE, tableStyle =  "TableStyleLight9")
addStyle(wb, sheet = "Report_Agreement", style = percentage_rows, rows = c(51:52), cols = 4 ,stack = TRUE,gridExpand = TRUE)






insertImage(wb,sheet = "Report_Agreement","File/status.plot.png",startCol = 9, startRow = 7, width = 5, height = 5, dpi = 300)
insertImage(wb,sheet = "Report_Agreement","File/ninstallment.png",startCol = 9, startRow = 33, width = 4, height = 4, dpi = 300)


saveWorkbook(wb, file = "File/Teaser.xlsx", overwrite = TRUE)