#Removing all objects
rm(list=ls())

#Decision Tree for Tests------

#normality_assumption == T && Homoscedasticity <- T --- use one-way ANOVA 

#normality_assumption == T && Homoscedasticity <- F --- use ANOVA Welch's test 

#  normality_assumption == F (&& homoscedasticity <- F or T )--- Kruskal Wallis test

#PostHoc Tests Structure-----------

##One-way ANOVA normality_assumption == T && Homoscedasticity == T
#Tukey test
#Pairwise T-tests with Bonferroni Correction pool.sd = TRUE

##ANOVA WITH UNEQUAl variance #normality_assumption == T && Homoscedasticity <- F 
#Games - Howel

## Kruskal Wallis test  normality_assumption == F (&& homoscedasticity <- F or T )
#Dunn's approach  p.adjust.method = "bonferroni"
#Pairwise comparisons using WilcoxMW’s test with Bonferroni correction 



#####ALL FUNCTIONS------------------------
# Install and load required libraries
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
}

#Set up functions--
load_and_view_excel <- function() {
  
  # Prompt the user for file name
  if(rstudioapi::isAvailable() && !is.null(getActiveDocumentContext()$path)) {
    setwd(dirname(getActiveDocumentContext()$path))
  } else {
    cat("No active document found, can't set working directory.\n")
    print("Save your data to the same directory you have saved this R script")
  }
  
  file_name <- readline(prompt = "Please enter the Excel file name (including extension): ")
  
  # Check if the file exists
  if (!file.exists(file_name)) {
    stop("File not found. Please provide a valid file name.")
  }
  
  # Prompt the user for dataset name
  #my_data <- readline(prompt = "Enter the name to save the dataset in the global environment: ")
  
  
  
  # Load the Excel file
  data <- readxl::read_excel(file_name)
  
  # Save the dataset in the global environment with the given name(dataset_name 1st argument)
  assign("my_data" ,data, envir = .GlobalEnv)
  
  # Display the data 
  #View(data, title = paste("Viewing:", file_name))
  
  
  return(data)
}

set_activity <- function(){
  activity <- readline(prompt = "Type your activity for hypothesis testing:
                                    tests for more than two samples")
  # Save the activity in the global environment
  assign("activity", activity, envir = .GlobalEnv)
  cat("Your activity is:", activity, "\n")
}

set_group <- function(data) {
  
  
  # Get the column names of the dataset
  column_names <- names(data)
  
  # Print the column names to the console
  cat("Available columns in the dataset:\n")
  print(column_names)
  
  # Prompt the user to input the column number
  i <- as.numeric(readline(prompt = "Number of the column of the different groups: "))
  
  # Validate the input
  if (i > 0 && i <= ncol(data)) {
    # Rename the selected column to 'group'
    colnames(data)[i] <- "group"  # Renaming the column
    cat("Column renamed to 'group'\n")
    
    # Assign the modified dataset back to the global environment
    assign("my_data", data, envir = .GlobalEnv)
    #cat("Changes saved to the global environment.\n")
    
    return(data)  # Return the modified data
  } else {
    cat("Invalid column number. Please try again.\n")
    return(NULL)
  }
}

set_continuous_variable <- function(data) {
  #note use for not including the group 
  #print(paste(column_names[-which(column_names == "group")])) but indexes!
  column_names <- names(data)
  cat("Available columns in the dataset:\n")
  print(column_names)
  
  # Prompt the user to input the column number
  i <- as.numeric(readline(prompt = "Number of the column of the continuous variable: "))
  
  # Validate the input
  if (i > 0 && i <= ncol(data)) {
    orig_var <- colnames(data)[i] 
    # Rename the selected column to 'con_var'
    colnames(data)[i] <- "con_var"  # Renaming the column
    cat("Column renamed to 'con_var'\n")
    
    # Assign the modified dataset back to the global environment
    assign("my_data", data, envir = .GlobalEnv)
    assign("original_con_var_name",orig_var, envir = .GlobalEnv)
    #cat("Changes saved to the global environment.\n")
    
    #return(data)  # Return the modified data
  } else {
    cat("Invalid column number. Please try again.\n")
    return(NULL)
  }
}
#for general
#select_type_var <- function(){#(Type : Continuous Variable or Distinct) for the general program}

set_dynamic_labels <- function(data, num_indepen_groups) {
  # Ensure the 'group' column exists in the dataset
  if (!"group" %in% names(data)) {
    cat("Error: 'group' column not found in the data.\n")
    return(NULL)
  }
  
  # Check if num_indepen_groups is valid
  if (num_indepen_groups <= 0 || num_indepen_groups > length(unique(data$group))) {
    cat("Error: num_indepen_groups is not valid.\n")
    return(NULL)
  }
  
  # Get the unique levels in the 'group' column, already checked but some spaghetti to be sure
  group_levels <- unique(data$group)
  
  # Ensure the number of levels matches num_indepen_groups
  if (length(group_levels) != num_indepen_groups) {
    cat("Warning: The number of unique group levels does not match num_indepen_groups.\n")
    return(NULL)
  }
  
  # Prompt the user for labels for each level
  labels <- vector("character", num_indepen_groups)  # Initialize a character vector to store labels
  cat("Please provide labels for the 'group' column levels:\n")
  
  for (i in 1:num_indepen_groups) {
    labels[i] <- readline(prompt = paste("Label for group level", group_levels[i], ": "))
  }
  
  # Apply the labels to the 'group' column
  data <- data |>
    mutate(group = factor(group, levels = group_levels, labels = labels))
  
  cat("The 'group' column has been updated with your labels.\n")
  return(data)  # Return the updated dataset
}

#Plot data and check normality graphically
violin_boxplot_plot<- function(my_data,group,con_var,title_plot){
  ggplot(my_data, aes(x=group, y= con_var)) +
    geom_violin( aes(fill = group),scale = "count") +
    geom_boxplot(width = 0.11, outlier.shape = NA, alpha = 0.5) +
    geom_point(position = position_jitter(width = 0.05),
               size = 1.2, alpha = 0.6) +
    labs(y = "Reduction (percentage)", #original_con_var_name
         title = title_plot) + #"Reduction Headache(%) by group"
    theme_pubr() +
    theme(plot.title.position = "plot",
          legend.position = "none",
          axis.ticks = element_line(size = 1.5, color="black"),
          axis.title.x = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.ticks.length=unit(0.2,"cm"))
  return(plotsgroup)
}

#Decision Functions for Normality and Homoscedasticity
shapiro_decision_normality_test <- function(data){ 
  dh_normality <- data |>
    group_by(group)|>
    shapiro_test(con_var)|>
    ungroup()
  cat("Use of Shapiro test for each group to test the normality \n")
  
  count_normality <- 0
  for(i in 1:length(dh_normality$p)){ 
    if(dh_normality$p[i]>0.05){
      print(paste(dh_normality$group[i],  "'s reduction is normally distributed"))
      count_normality <- count_normality + 1 
    }
    else{
      print(paste(dh_normality$group[i] , "'s reduction  normallity is violated"))
    }
  }
  
  if(count_normality < num_indepen_groups){
    print(paste("Only ", count_normality, "out of ", num_indepen_groups," groups follow normal distributions."))
    print("So, we procced to use Kruskal - Wallis test (non parametric)")
    normality_assumption <- F
  }else{
    print("All groups are following the normal distribution.")
    normality_assumption <- T
  }
  #normality_assumption <<- normality_assumption
  assign("nor_shapiro_results", dh_normality, envir = .GlobalEnv)
  return(normality_assumption)
}

levene_decision_homoscedasticity <-function(data){   
  
  dh_levene <- data |>
    levene_test(con_var ~ group)
  
  if(dh_levene$p > 0.05){ #p > 0.05
    print(paste("Homoscedasticity (for reference p = ", dh_levene$p,")> 0.05"))
    #we can normality_assumption == T && Homoscedasticity <- T --- use ANOVA 
    homoscedasticity <- T
    #return(homoscedasticity)
    #def Decision of test
  }else{ #p > 0.05
    print(paste("No Homoscedasticity  (for reference p = ", dh_levene$p,")< 0.05"))
    #  homoscedasticity <- F normality_assumption == T --- use ANOVA Welch's test
    #  homoscedasticity <- F normality_assumption == F --- Kruskal Wallis test
    homoscedasticity <- F
    #return(homoscedasticity)
    #def Decision of test
  }
  return(homoscedasticity)
  #return()
}

#Hypothesis Result
hypothesis_decision <- function(result){
  if(result$p > 0.05){
    cat("Hypothesis testing : \n")
    cat(paste("All", num_indepen_groups," populations have the same median
              (for reference p =",result$p,"  > 0.05"))
  }else{
    cat("Hypothesis testing : \n")
    cat(paste("Not all of the ",num_indepen_groups,"independent polulation medians are the same (for reference p =",result$p," )<0.05 \n"))
  }
  cat("For reference the results: \n")
  result
}

#Post Hoc Testing Depending on the assumptions and tests!

handle_assumptions <- function(normality_assumption, homoscedasticity) {
  # Validate inputs (not necessary, well)
  if (!is.logical(normality_assumption) || !is.logical(homoscedasticity)) {
    stop("Both inputs must be logical (TRUE or FALSE).")
  }
  
  # Define formula
  y <- con_var ~ group
  
  # Handle assumptions with if-else
  if (normality_assumption & homoscedasticity) {
    cat("Use One-way ANOVA. Normality = TRUE, Homoscedasticity = TRUE.\n")
    res <- my_data |> anova_test(y, detailed = T)
    hypothesis_decision(res)
    # Post Hoc Tests
    cat("Running post hoc test of Tukey's test\n")
    res_Tukey <- my_data %>% tukey_hsd(y)
    print(res_Tukey)
    cat("Running post hoc test of T-tests with Bonferroni Correction\n")
    res_TBonferroni <- my_data %>% pairwise_t_test(y, pool.sd = TRUE, p.adjust.method = "bonferroni")
    print(res_TBonferroni)
    
  } else if (normality_assumption & !homoscedasticity) {
    cat("Use ANOVA Welch's test. Normality = TRUE, Homoscedasticity = FALSE.\n")
    res <- my_data |> welch_anova_test(y)
    hypothesis_decision(res)
    # Post Hoc Test
    cat("Running post hoc test of Games-Howell\n")
    res_GH <- my_data |> games_howell_test(y)
    print(res_GH)
    
  } else if (!normality_assumption & homoscedasticity) {
    cat("Normality is FALSE, but homoscedasticity is TRUE.\n")
    res <- my_data |> kruskal_test(y)
    hypothesis_decision(res)
    # Post Hoc Test
    cat("Running post hoc Pairwise comparisons using WMW’s test with Bonferroni correction\n")
    resWilcox_Bon <- my_data %>% pairwise_wilcox_test(y, p.adjust.method = "bonferroni")
    print(resWilcox_Bon)
    
  } else if (!normality_assumption & !homoscedasticity) {
    cat("Both normality and homoscedasticity are FALSE.\n")
    res <- my_data |> kruskal_test(y)
    hypothesis_decision(res)
    # Post Hoc Test
    cat("Running post hoc test of Dunn's test\n")
    resDunn <- my_data %>% dunn_test(y, p.adjust.method = "bonferroni")
    print(resDunn)
  }
}




#main program-------------------
main <- function(){

  
#########################start of the program----------------------  
  cat("Welcome to the program!\n
       (Requires clean data, Only used for Continuous Variable with more than 2 independent groups)")

  print("Save your data to the same directory you have saved this R script")
  suppressWarnings(load_packages(c("readxl", "ggplot2", "tidyverse", "rstatix","dlookr","dplyr", "ggpol")))
  load_packages(c("readxl", "ggplot2", "tidyverse", "rstatix","dlookr","dplyr", "ggpol","rstudioapi","ggpubr"))
  load_and_view_excel()
 
  #set_activity()
  
  #my_data <- read_excel("data_headache.xlsx")
  my_data_original_copy <- my_data
  view(my_data)
  #reset
  #my_data <- my_data_original_copy
  set_group(my_data)
  #Number of independent groups
  num_indepen_groups <<- length(unique(my_data$group))
  print(paste("Number of independent groups: ", num_indepen_groups ))
  set_continuous_variable(my_data)
  
  #Factor the group and naming all levels
  my_data <- set_dynamic_labels(my_data,num_indepen_groups)
  
  
  sum_stat_h<- my_data|>
              group_by(group) |>
              describe(con_var) |>
              select(described_variables, group, n, mean, sd, p25, p50, p75, skewness, kurtosis) |> 
              ungroup() 
  cat("Summary statistics for each group \n")
  sum_stat_h
  
  cat("Assumptions Checking \n")
  
#Assumptions checking-------------------
  #Dependency------
cat("Independence not violated. We check the normality through plot and Shapiro test \n")
  #Normality 3 steps----  
  title_plot <-readline(prompt = "Title of the plot : ")
      #Check normality graphically #function to set title, measures...
  plotsgroup <-violin_boxplot_plot(my_data,group,con_var,title_plot)
  plotsgroup
  # Save the plot as a PDF
  #ggsave("plots_per_group.pdf", plot = plotsgroup, width = 8, height = 6, units = "in")
  
      #Normality Shapiro test
     normality_assumption <- shapiro_decision_normality_test(my_data)
     nor_shapiro_results
    #"nor_shapiro_results" saved in the Global Env for details
     
  #Homoscedacity----------
     cat("We are checking the homogeneity of variance for each group. \n")
     homoscedasticity <- levene_decision_homoscedasticity(my_data)
     
     #Tests and Post Hoc
     handle_assumptions(normality_assumption, homoscedasticity)


  
  cat("Study completed. Goodbye!\n")
}


main()
#INPUTS BY USER
#data_headache.xlsx
#
# Example usage
# homoscedasticity<-T
# normality_assumption <-T
# handle_assumptions(normality_assumption,homoscedasticity)

