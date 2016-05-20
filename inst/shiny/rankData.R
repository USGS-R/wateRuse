output$rankData <- DT::renderDataTable({
  
  w.use <- w.use()
  data.elements <- df[["data.element"]]
  areas.rd <- df[["area"]]
  area.column <- df[["area.column"]]
  yearRange <- unique(w.use$YEAR)
  
  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas.rd)
  
  df <- spread_(w.use.sub, "YEAR", data.elements)
  
  df <- df[,colSums(is.na(df))<nrow(df)]
  
  write.csv(df, "rankData.csv",row.names = FALSE)
  
  rankData <- DT::datatable(df, rownames = FALSE,
                            options = list(scrollX = TRUE,
                                           pageLength = nrow(df),
                                           order=list(list(2,'desc'))))
  yearRange <- names(df)[-1]
  colors <- brewer.pal(ifelse(length(yearRange)>=3,length(yearRange),3),"Blues")
  names(colors)[1:length(yearRange)] <- yearRange
  for(i in yearRange){
    rankData <- formatStyle(rankData, as.character(i),
                            background = styleColorBar(range(df[[as.character(i)]],na.rm = TRUE), colors[as.character(i)]),
                            backgroundSize = '100% 90%',
                            backgroundRepeat = 'no-repeat',
                            backgroundPosition = 'center' )
  }
  
  rankData
})

output$downloadRankData <- downloadHandler(
  filename = function() { "rankData.csv" },
  content = function(file) {
    file.copy("rankData.csv", file)
  }
)