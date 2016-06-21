output$rankData <- DT::renderDataTable({
  
  validate(
    need(input$state, 'Choose a State'),
    need(input$area, 'Choose an Area')
  )
  
  w.use <- w.use()
  data.elements <- df[["data.element"]]
  areas.rd <- df[["area"]]
  area.column <- df[["area.column"]]
  yearRange <- unique(w.use$YEAR)
  
  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas.rd)
  w.use.sub <- w.use.sub[!is.na(w.use.sub[,data.elements]),]
  df <- spread_(w.use.sub, "YEAR", data.elements)
  
  df <- df[,colSums(is.na(df))<nrow(df)]
  
  write.csv(df, "rankData.csv",row.names = FALSE)
  
  rankData <- DT::datatable(df[,-2], rownames = FALSE,
                            options = list(scrollX = TRUE,
                                           pageLength = nrow(df),
                                           order=list(list(2,'desc'))))
  yearRange <- names(df)[-1:-2]
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

output$outputRankCode <- renderPrint({
  
  data.elements <- df[["data.element"]]
  areas.pTC <- df[["area"]]
  
  areasOptions <-  df[["areas"]]
  
  if(all(areasOptions %in% areas.pTC)){
    areas.pTC <- NA
  } else {
    areas.pTC <- paste0('c("',paste(areas.pTC, collapse = '","'),'")')
  }
  
  area.column <- df[["area.column"]]
  
  outText <- paste0(
    'library(dplyr)\n',
    'data.elements <- c("',paste0(data.elements,collapse = '","'),'")\n',
    "areas <- ",areas.pTC, "\n",
    'area.column <- "', area.column, '"\n',
    "w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)\n",
    "w.use.rank <- spread_(w.use.sub, 'YEAR', data.elements)\n",
    "w.use.rank <- w.use.rank[,colSums(is.na(w.use.rank))<nrow(w.use.rank)]"
    
  )
  
  HTML(outText)
  
})