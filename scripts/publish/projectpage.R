#' Creates templated html for each project
library(dplyr)
publish.projectpage <- function(viz = as.viz("projectPages")) {
  
  deps <- readDepends(viz)
  
  projects <- deps[['project_table']] # get projects from deps
  
  img.files <- list(
    month_sessions = deps[['viz_month_sessions']],
    year_line_sessions = deps[['viz_y_sessions']],
    month_line_sessions = deps[['viz_m_sessions']],
    week_line_sessions = deps[['viz_w_sessions']],
    day_line_sessions = deps[['viz_d_sessions']],
    device_type = deps[['viz_device_type']],
    source_counts = deps[['viz_source']],
    viz_geo_apps = deps[["viz_geo_apps"]],
    timeDayUse_app = deps[["timeDayUse_app"]],
    app_time = deps[["app_time"]]
  )
  
  for (i in 1:nrow(projects)) {
    proj <- projects[i,]
    viewID <- proj$viewID
    # get relative paths for images
    proj.imgs <- sapply(img.files, function(x){
      img <- "missingImg"
      
      row <- filter(x, id == viewID)
      if (nrow(row) > 0) {
        img <- list(
          location = row[['loc']],
          mimetype = "image/png",
          alttext = row[['type']],
          title = row[['type']]
        )
      }
      img <- as.viz(img)
      img <- as.publisher(img)
      img.out <- publish(img)

      return(img.out)
    })
    
    sectionId <- paste0(viewID, "-section")
    contents <- list(
      id = sectionId,
      publisher = "section",
      template = viz[['template']],
      depends = viz[['depends']],
      context = c(
        viz[['context']],
        list(
          project_name = proj$longName,
          monthly_users_chart = proj.imgs[['month_sessions']],
          year_line_sessions = proj.imgs[['year_line_sessions']],
          month_line_sessions = proj.imgs[['month_line_sessions']],
          week_line_sessions = proj.imgs[['week_line_sessions']],
          day_line_sessions = proj.imgs[['day_line_sessions']],
          device_type = proj.imgs[['device_type']],
          source_counts = proj.imgs[['source_counts']],
          viz_geo_apps = proj.imgs[["viz_geo_apps"]],
          timeDayUse_app = proj.imgs[["timeDayUse_app"]],
          app_time = proj.imgs[["app_time"]]
      ))
    )
    contents <- as.viz(contents)
    contents <- as.publisher(contents)
    
    depends <- viz[['depends']]
    depends[[sectionId]] <- contents
    
    pub <- list(
      id = paste0(viewID, "-page"),
      name = proj$shortName,
      publisher = "page",
      template = "fullpage",
      depends = depends,
      context = list(
        header = viz[['context']][['header']],
        footer = viz[['context']][['footer']],
        resources = viz[['context']][['resources']],
        sections = sectionId
      )
    )
    
    pub <- as.viz(pub)
    pub <- as.publisher(pub)
    publish(pub)
  }
}

publish.projectlist <- function(viz = as.viz("project_list")) {
  required <- c("template")
  checkRequired(viz, required)
  
  deps <- readDepends(viz)
  
  template <- template(viz[['template']])
  context <- list(projects = deps[['project_links']])
  viz[['output']] <- render(template, context)
  return(viz[['output']])
}