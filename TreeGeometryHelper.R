# TreeGeometryHelper.R
library(plotly)
library(RColorBrewer)


# Extracting tree coordinates function
extract_coordinates <- function(tree_positions) {
  result_list <- list()
  
  for (variable in tree_positions$geometry) {
    point <- as.numeric(unlist(variable))
    result_list <- c(result_list, list(point))
  }
  
  return(result_list)
}

# Function to generate dynamic color palette based on number of unique species
generate_species_colors <- function(num_species,unique_species) {
  palette <- colorRampPalette(colors = c("blue", "green", "red"))(num_species)
  # Create a named vector mapping species to colors
  species_colors <- setNames(palette, unique_species)
  return(species_colors)
}

# Function to save the dataframe
save_dataframe <- function(pathInput,dataframe) {
  filename <- "output.rds"  # Name of the file to save
  saveRDS(dataframe, file = paste(pathInput,filename,sep = ""))
  filepath <- normalizePath(filename)  # Get the full path
  return(paste("Dataframe saved as:", filename, "\nPath:", filepath))
}

#create scatter plot
create_scatter_plot <- function(df_param) {
  removed_name <- "Removed"
  not_removed_name <- "Non-removed Trees"
  dead_name <- "Dead"
  # Get unique species
  unique_species <- unique(df_param$status)
  num_species <- length(unique_species)
  # Generate dynamic color palette
  species_colors <- generate_species_colors(num_species,unique_species)
  
  p <- plot_ly(
    data = df_param,
    x = ~x, 
    y = ~y
  )
  p <- add_markers(
    p,
    data = subset(df_param, status == -1), 
    x = ~x, 
    y = ~y, 
    type = 'scatter', 
    symbol = 'x', 
    mode = 'markers', 
    name = dead_name,
    key = ~status,  # Pass tree_id as custom data
    text = ~tooltip,
    marker = list(
      size = ~dbh,
      color = ~as.factor(status),
      colors = species_colors,
      sizemode = 'diameter'  # Set size mode to diameter for image markers
      
    )
  )
  p <- add_markers(
    p,
    data = subset(df_param, status == 1), 
    x = ~x, 
    y = ~y, 
    type = 'scatter', 
    key = ~status,  # Pass tree_id as custom data
    mode = 'markers', 
    name = not_removed_name,
    text = ~tooltip,
    marker = list(
      size = ~dbh,
      color = ~as.factor(status),
      colors = species_colors,
      symbol = paste0("url('tree.jpg')"),  # Use custom image for non-removed trees
      sizemode = 'diameter'  # Set size mode to diameter for image markers
    )
  )
  
  p <- add_markers(
    p,
    data = subset(df_param, status == 0), 
    x = ~x, 
    y = ~y, 
    type = 'scatter', 
    key = ~status,  # Pass tree_id as custom data
    mode = 'markers', 
    name = removed_name,
    text = ~tooltip,
    marker = list(
      size = ~dbh,
      color = ~as.factor(status),
      colors = species_colors,
      symbol = paste0("url('tree.jpg')"),  # Use custom image for non-removed trees
      sizemode = 'diameter'  # Set size mode to diameter for image markers
    )
  )
  # Add event handler for click
  p <- event_register(p, 'plotly_click')
  
  # Define the event handler
  p <- htmlwidgets::onRender(p, '
      function(el, x) {
        el.on("plotly_click", function(eventData) {
          var point = {
            x: eventData.points[0].x,
            y: eventData.points[0].y,
            status: eventData.points[0].data.key[0]  
          };
          console.log(eventData)
          Shiny.setInputValue("point_clicked", point);
        });
      }
    ')
  
  p
}

#convert data format to map format
convert_data_to_map_format <- function(data){
  
  not_removed_name <- "Not Removed"
  removed_name <- "Removed"
  
  data_list <- extract_coordinates(data$tree_positions)
  for (x in 1:length(data_list)) {
    tree_id <- data$trees$tree_id[x]
    tree_species <- data$trees$species_id[x]
    data_list[[x]] <- c(
      data_list[[x]],
      list(tree_id),
      list(get_tree_dbh(data$trees,tree_id)),
      list(data$trees$removal[x]),
      list(tree_species)
    )
  }
  #convert list to dataframe for the scatter plot
  df_value <- data.frame(
    x = sapply(data_list, function(coords) coords[[1]]),
    y = sapply(data_list, function(coords) coords[[2]]),
    removal = sapply(data_list, function(coords) coords[[5]]),
    status = sapply(data_list, function(coords) ifelse(coords[[5]],-1,1)),
    tree_id = sapply(data_list, function(coords) coords[[3]]),
    tree_species = sapply(data_list, function(coords) coords[[6]]),
    is_death = sapply(data_list, function(coords) ifelse(coords[[5]],1,0)),
    dbh = sapply(data_list, function(coords) 
      ifelse(as.numeric(coords[[4]]) == 0,5,as.numeric(coords[[4]])/2)),
    tooltip = sapply(data_list, function(coords) {
      dbh <- coords[[4]]
      tree_id <- coords[[3]]
      tree_type <- coords[[6]]
      ifelse(is.null(dbh) || dbh == "", "", 
             paste("DBH: ",dbh," - Tree ID: ",tree_id," - Tree Species",
                   tree_type))
    })
  )
  
  return(df_value)
}

#get tree dbh from dataframe of trees
get_tree_dbh <- function(trees,tree_id) {
  for (x in 1:length(trees)) {
    if(trees$tree_id[[x]] == tree_id){
      return(trees$dbh_cm[[x]])
    }
  }
  return(0)
}

#convert data to table format
convert_data_to_table_format <- function(data,table_name){
  
  not_removed_name <- "Not Removed"
  removed_name <- "Removed"
  
  data_list <- extract_coordinates(data$tree_positions)
  for (x in 1:length(data_list)) {
    tree_id <- data$trees$tree_id[x]
    data_list[[x]] <- c(
      data_list[[x]],
      list(tree_id),
      list(get_tree_dbh(data$trees,tree_id)),
      list(data$trees$removal[x])
    )
  }
  #convert list to dataframe for the scatter plot
  df_value <- data.frame(
    x = sapply(data_list, function(coords) coords[[1]]),
    y = sapply(data_list, function(coords) coords[[2]]),
    removal = sapply(data_list, function(coords) coords[[5]]),
    tree_id = sapply(data_list, function(coords) coords[[3]]),
    database_name = sapply(data_list, function(coords) table_name),
    dbh = sapply(data_list, function(coords) 
      ifelse(as.numeric(coords[[4]]) == 0,5,as.numeric(coords[[4]])/2))
  )
  
  return(df_value)
}

#delete all files with starting the keyword
delete_forestr_files <- function(keyword) {
  files_to_delete <- list.files(pattern = keyword)
  if (length(files_to_delete) > 0) {
    file.remove(files_to_delete)
    cat("Files starting with keyword have been deleted.\n")
  } else {
    cat("No files starting with keyword found.\n")
  }
}
