titolone <- createStyle(fontSize = 14,textDecoration = c("bold","underline"),halign = "left", 
                        valign = "center",fontColour = "black",wrapText = FALSE)
total_rows <- createStyle(fontSize = 11,fgFill = "#B7DFEF",border="bottom",borderStyle = "medium",
                          borderColour = "#283DBB",fontColour = "black",wrapText = FALSE)
subtotal_rows <- createStyle(textDecoration = "bold",fontSize = 11,fgFill = "#E4F0FE",
                             border="bottom",fontColour = "black",wrapText = FALSE)
# highlight_style <- createStyle(fgFill = "#8DB4E2", border = "TopBottom",borderColour ="black",
#                                borderStyle= "medium",textDecoration="bold", valign = "center")
# title_style <- createStyle(fgFill = "white", border = "TopBottomLeftRight",borderColour = "black",
#                            borderStyle= "medium",textDecoration="bold")
# section_style <- createStyle(border = "Bottom",borderColour ="black",borderStyle= "medium")
# 
# group_style <- createStyle(fgFill = "#B7DEE8",textDecoration="bold",border = "Top")



applyStylesToColumns <- function(wb, sheet, df, column_types, startRow, startCol) {
  for (col in startCol:(startCol + ncol(df) - 1)) {
    col_type <- column_types[col - startCol + 1]  # Adjust the column index to match column_types
    rows_to_style <- startRow + 1:nrow(df)  # Start at startRow + 1 and go until the end of the data frame
    
    if (col_type == "currency K") {
      addStyle(wb, sheet, style = createStyle(numFmt = "0.0,\"k\";[=0]\"-\";#0"), rows = rows_to_style, cols = col, gridExpand = TRUE)
    } else if (col_type == "currency M") {
      addStyle(wb, sheet, style = createStyle(numFmt = "0.0,,\"M\";[=0]\"-\";#0"), rows = rows_to_style, cols = col, gridExpand = TRUE)
    }else if (col_type == "percentage") {
      addStyle(wb, sheet, style = createStyle(numFmt = "0.0%;[=0]\"-\""), rows = rows_to_style, cols = col, gridExpand = TRUE)
    } else if (col_type == "number"){
      addStyle(wb,sheet,style=createStyle(numFmt="0;[=0]\"-\""),rows=rows_to_style,cols=col,gridExpand = TRUE,stack = TRUE)
    }
  }
}




wb <- createWorkbook("Tables.xlsx")
addWorksheet(wb, "Report_Loans")
showGridLines(wb, sheet = 1, showGridLines = FALSE)


writeData(wb,1, " Report   ",1,1)
addStyle(wb, sheet = "Report_Loans", style = titolone, rows = 1, cols = 1 ,stack = TRUE,gridExpand = TRUE)


startRow <- 4
startCol <- 2

writeDataTable(wb, 1, totals, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c( "number","currency M","number","currency K","currency K")
applyStylesToColumns(wb,1, totals, column_types, startRow,startCol)
writeData(wb, 1, x = "Totals ", startCol =startCol, startRow = startRow-1)
mergeCells(wb, 1, startCol:(startCol + ncol(totals) - 1), rows = startRow-1)



startColu<-startCol
startRow_updated <- startRow+nrow(totals)+6

writeDataTable(wb, 1, loans.by.type, startRow = startRow_updated, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general", "number","percentage","currency M","percentage")
applyStylesToColumns(wb,1, loans.by.type, column_types, startRow_updated,startColu)
writeData(wb, 1, x = "Loan by type", startCol =startColu, startRow = startRow_updated-1)
mergeCells(wb, 1, startColu:(startColu + ncol(loans.by.type) - 1), rows = startRow_updated-1)
addStyle(wb,1,total_rows,rows=startRow_updated+1,cols=c(startColu:(startColu+ncol(loans.by.type)-1)))

startRow_updated <- startRow_updated+nrow(loans.by.type)+6

writeDataTable(wb, 1, loans.by.gbv.range, startRow = startRow_updated, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general", "number","currency M","percentage")
applyStylesToColumns(wb,1, loans.by.gbv.range, column_types, startRow_updated,startColu)
writeData(wb, 1, x = "Loan by type", startCol =startColu, startRow = startRow_updated-1)
mergeCells(wb, 1, startColu:(startColu + ncol(loans.by.gbv.range) - 1), rows = startRow_updated-1)
addStyle(wb,1,total_rows,startRow_updated+1,c(startColu:startColu+ncol(loans.by.gbv.range)-1))
addStyle(wb,1,subtotal_rows,c(startRow_updated+6,startRow_updated+11,startRow_updated+16),c(startColu:startColu+ncol(loans.by.gbv.range)-1))


startRow_updated <- startRow_updated+nrow(loans.by.gbv.range) + 6

writeDataTable(wb, 1, loans.vintage, startRow = startRow_updated, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general","general", "number","currency M","percentage")
applyStylesToColumns(wb,1, loans.vintage, column_types, startRow_updated,startColu)
writeData(wb, 1, x = "Loan by type", startCol =startColu, startRow = startRow_updated-1)
mergeCells(wb, 1, startColu:(startColu + ncol(loans.vintage) - 1), rows = startRow_updated-1)







saveWorkbook(wb, file = "Tables.xlsx", overwrite = TRUE)
