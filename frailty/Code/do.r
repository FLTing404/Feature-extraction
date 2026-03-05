#Supplementary Figure S4&S5
library("gridExtra")
library("data.table")
library(ggplot2)
library(dplyr)
library(tidyr)
library(poLCA)
library('RColorBrewer')
source("LCA\ graph.R")

#Supplementary Figures S9
library(haven)
library(dplyr)
library(lme4)
library(foreach)
library(doParallel)
library(ggplot2)


#Script for Supplementary Figure S4$S5.
#LCA main functions

#LCA_SES: LCA for database CHARLS,HRS,MHAS,SHARE

LCA_SES <- function(raw_data,data_name,cluster){
  #raw_data：Dataset after removing rows with null values in target columns. 
  #         You can find them in data/LCA .
  cSES_lc <- poLCA(cbind(fjob,financial,fedu)~1,
                   raw_data, nclass=cluster, maxiter=7000,
                   tol=1e-10, na.rm=TRUE, graph=TRUE,
                   nrep=30, verbose=TRUE,calc.se=TRUE) #Childhood SES
  
  aSES_lc <- poLCA(cbind(wealth,income,education)~1, 
                   raw_data, nclass=cluster, maxiter=7000, 
                   tol=1e-10, na.rm=TRUE, graph=TRUE, 
                   nrep=30, verbose=TRUE, calc.se=TRUE) #Adulthood SES
  
  cSES_lcentropy_s <- 1 - (poLCA.entropy(cSES_lc) / (nrow(cSES_lc$posterior) * log(ncol(cSES_lc$posterior))))
  aSES_lcentropy_s <- 1 - (poLCA.entropy(aSES_lc) / (nrow(aSES_lc$posterior) * log(ncol(aSES_lc$posterior))))
  
  rawdata_lc <- as.data.table(cbind(raw_data,cSES=cSES_lc$predclass,
                                    aSES=aSES_lc$predclass,
                                    cMean=rowMeans(raw_data[,c("fjob","financial","fedu")])
                                    ,aMean=rowMeans(raw_data[,c("wealth","income","education")])
  ))
  
  cSES_grouping <- rawdata_lc[, .(group_mean = mean(cMean, na.rm = TRUE)), 
                              by = "cSES"][order(group_mean), new_group := 1:.N]
  aSES_grouping <- rawdata_lc[, .(group_mean = mean(aMean, na.rm = TRUE)), 
                              by = "aSES"][order(group_mean), new_group := 1:.N]
  rawdata_lc[cSES_grouping,cSES_level:=new_group,on ="cSES"]
  rawdata_lc[aSES_grouping,aSES_level:=new_group,on ="aSES"]
  
  assign(paste0(data_name,cluster,"_grouped"),rawdata_lc,envir=.GlobalEnv)
  graph <- grid.arrange((LCA_graph(cSES_lc)+labs(caption="childood SES")),
                        (LCA_graph(aSES_lc)+labs(caption="adulthood SES")),
                        ncol=2)
  print(graph)
  assign(paste0(data_name,cluster,"_plist"),
         list(c=cSES_lc, ce=cSES_lcentropy_s,ct=table(rawdata_lc$cSES,rawdata_lc$cSES_level),
              a=aSES_lc, ae=aSES_lcentropy_s,at=table(rawdata_lc$aSES,rawdata_lc$aSES_level),
              g=graph),
         envir=.GlobalEnv)
  
}

#LCA_SES4KLoSA: LCA for database KLoSA

LCA_SES4KLoSA <- function(raw_data,data_name,cluster){
  aSES_lc <- poLCA(cbind(wealth,income,education)~1, raw_data, nclass=cluster, maxiter=7000, 
                   tol=1e-5, na.rm=TRUE, graph=TRUE,
                   nrep=30, verbose=TRUE, calc.se=TRUE) #Adulthood SES
  
  aSES_lcentropy_s <- 1 - (poLCA.entropy(aSES_lc) / (nrow(aSES_lc$posterior) * log(ncol(aSES_lc$posterior))))
  
  rawdata_lc <- as.data.table(cbind(raw_data,aSES=aSES_lc$predclass,
                                    aMean=rowMeans(raw_data[,c("wealth","income","education")])))
  
  aSES_grouping <- rawdata_lc[, .(group_mean = mean(aMean, na.rm = TRUE)), 
                              by = "aSES"][order(group_mean), new_group := 1:.N]
  rawdata_lc[aSES_grouping,aSES_level:=new_group,on ="aSES"]
  
  assign(paste0(data_name,cluster,"_grouped"),rawdata_lc,envir=.GlobalEnv)
  
  graph <- LCA_graph(aSES_lc)+labs(caption="adulthood SES")
  print(graph)
  assign(paste0(data_name,cluster,"_plist"),
         list(a=aSES_lc, ae=aSES_lcentropy_s,at=table(rawdata_lc$aSES,rawdata_lc$aSES_level),
              g=graph),envir=.GlobalEnv)
  
}

#LCA_SES4ELSA: LCA for database ELSA

LCA_SES4ELSA <- function(raw_data,data_name,cluster){
  cSES_lc <- poLCA(cbind(fjob,financial,fedu,disaster)~1,raw_data, nclass=cluster, maxiter=7000, 
                   tol=1e-5, na.rm=TRUE, graph=TRUE,
                   nrep=30, verbose=TRUE, calc.se=TRUE) #Childhood SES
  
  aSES_lc <- poLCA(cbind(wealth,income,education)~1, raw_data, nclass=cluster, maxiter=7000, 
                   tol=1e-5, na.rm=TRUE, graph=TRUE,
                   nrep=30, verbose=TRUE, calc.se=TRUE) #Adulthood SES
  
  cSES_lcentropy_s <- 1 - (poLCA.entropy(cSES_lc) / (nrow(cSES_lc$posterior) * log(ncol(cSES_lc$posterior))))
  aSES_lcentropy_s <- 1 - (poLCA.entropy(aSES_lc) / (nrow(aSES_lc$posterior) * log(ncol(aSES_lc$posterior))))
  
  rawdata_lc <- as.data.table(cbind(raw_data,cSES=cSES_lc$predclass,aSES=aSES_lc$predclass,
                                    cMean=rowMeans(raw_data[,c("fjob","financial","fedu","disaster")]),
                                    aMean=rowMeans(raw_data[,c("wealth","income","education")])))
  
  cSES_grouping <- rawdata_lc[, .(group_mean = mean(cMean, na.rm = TRUE)), 
                              by = "cSES"][order(group_mean), new_group := 1:.N]
  aSES_grouping <- rawdata_lc[, .(group_mean = mean(aMean, na.rm = TRUE)), 
                              by = "aSES"][order(group_mean), new_group := 1:.N]
  rawdata_lc[cSES_grouping,cSES_level:=new_group,on ="cSES"]
  rawdata_lc[aSES_grouping,aSES_level:=new_group,on ="aSES"]
  
  assign(paste0(data_name,cluster,"_grouped"),rawdata_lc,envir=.GlobalEnv)
  
  graph <- grid.arrange((LCA_graph(cSES_lc)+labs(caption="childood SES")),
                        (LCA_graph(aSES_lc)+labs(caption="adulthood SES")),ncol=2)
  print(graph)
  assign(paste0(data_name,cluster,"_plist"),
         list(c=cSES_lc, ce=cSES_lcentropy_s,ct=table(rawdata_lc$cSES,rawdata_lc$cSES_level),
              a=aSES_lc, ae=aSES_lcentropy_s,at=table(rawdata_lc$aSES,rawdata_lc$aSES_level),
              g=graph),envir=.GlobalEnv)
  
}

#Helper function for generating informal LCA graph to debug.
#This function is not for generating stack barchart for Supplementary Figure S4&S5

LCA_graph <-  function(lc){
  lcmodel <- reshape2::melt(lc$probs, level=2)
  zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
  zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
  zp1 <- zp1 + facet_grid(Var1 ~ .) 
  zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") +theme_bw()
  zp1 <- zp1 + labs(x = "SES level",y="SES index-SES level", fill ="SES index")
  zp1 <- zp1 + theme( axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),                    
                      panel.grid.major.y=element_blank())
  zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
  return(zp1)
}

#Generate stacked bar chart for 3-class models.

# Map for relablling indice and SES level
label_map <- c(
  "fedu" = "Education Level",
  "financial" = "Family Income",
  "fjob" = "Employment Status",
  "education"="Education Level",
  "income" = "Family Income",
  "wealth" = "Family Wealth",
  "disaster"="Disaster",
  "High SES" = "High",
  "Middle SES" = "Middle",
  "Low SES" = "Low"
)

#Function for Relabelling indice.
map_labels <- function(x) {
  ifelse(
    x %in% names(label_map),  
    label_map[x],             
    x                         
  )
}


#Generating stack barchart for Supplementary Figure S4&S5
LCA_barchart <- function(data_name,phase) {
  model_plist <- get(paste0(data_name, "_plist")) 
  if (phase=="c"){
    model <- model_plist$c
    probs_df <- model$probs
    rm <- model_plist$crm
    title <- "Childhood"
    mapping_vector <- setNames(rm[["cSES_level"]],paste0("class"," ",rm[["cSES"]], ": "))
    
  }
  else if (phase=="a"){
    model <- model_plist$a
    probs_df <- model$probs
    rm <- model_plist$arm
    title <- "Adulthood"
    mapping_vector <- setNames(rm[["aSES_level"]],paste0("class"," ",rm[["aSES"]], ": "))
  }
  else{
    print("error in input parameters")
  }
  lcmodel <- reshape2::melt(
    probs_df,  
    level=2
  )
  lcmodel$Var1 <- mapping_vector[as.character(lcmodel$Var1)]
  lcmodel <- lcmodel %>%
    mutate(
      Var1 = map_labels(as.character(Var1)),  
      L2 = map_labels(as.character(L2)),      
    )
  lcmodel$Var2 <- factor(
    lcmodel$Var2,
    levels = c("Pr(3)", "Pr(2)", "Pr(1)"),  
    labels = c("2", "1", "0")  
  )
  lcmodel$Var1 <- factor(lcmodel$Var1,levels = c("High", "Middle", "Low"))

  graph <- ggplot(lcmodel, 
                  aes(x = L2, y = value, fill = Var2)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    facet_grid(Var1 ~ .)+
    labs(
      #title = paste("SES-",title,"-",data_name),
      x = paste(title,"SES"),
      y = "Probability",
      fill = "Level") +
    scale_fill_brewer(type="seq", palette="Blues")+
    theme_bw() +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.5), 
    ) +
    guides(fill = guide_legend(
      direction = "horizontal",  
      nrow = 1,                
      reverse = TRUE       
    ))+
    theme(
      plot.title = element_text(
        vjust = 0,                       
        hjust = 0.5,                   
        margin = margin(b = 20)),         
      legend.position = "top",         
      strip.placement.y = "outside",
      panel.grid.major.y=element_blank(),
      panel.grid.minor.y = element_blank())
  return(graph)
}


#Script for Supplementary Table S4.
#Generate model comparison for different models.

model_comparison <- data.frame(
  model_name=character(0),
  cohort=character(0),
  df=numeric(0),
  AIC=numeric(0),
  BIC=numeric(0),
  adjBIC=numeric(0),
  Gsq=numeric(0),
  llik=numeric(0))

for (name in names_list){
  for (num in c("1","2","","4")){
    if (exists(paste0(name,num,"_plist"),envir=.GlobalEnv)){
      print(paste0(name,num,"_plist"))
      list <- get(paste0(name,num,"_plist"),envir=.GlobalEnv)
      if(num==""){
        num <- "3"
      }
      if(exists("c",where=list)){
        model_c <- list$c
        model_comparison <- rbind(
          model_comparison,
          data.frame(
            model_name=paste0(name,"-Childhood"),
            cohort=num,
            df=model_c$resid.df,
            AIC=model_c$aic,
            BIC=model_c$bic,
            adjBIC=calculate_adjBIC(model_c),
            Gsq=model_c$Gsq,
            llik=model_c$llik)
        )}
      model_a <- list$a
      model_comparison <- rbind(
        model_comparison,
        data.frame(
          model_name=paste0(name,"-Adulthood"),
          cohort=num,
          df=model_a$resid.df,
          AIC=model_a$aic,
          BIC=model_a$bic,
          adjBIC=calculate_adjBIC(model_a),
          Gsq=model_a$Gsq,
          llik=model_a$llik))
      
    }}
}


# Import and transform data
raw <- read_dta("Data/pooled.dta")
data <- raw %>%
  mutate(
    sex = factor(sex),
    marital = factor(marital),
    alive = factor(alive),
    smoke = factor(smoke),
    drink = factor(drink),
    exercise = factor(exercise),
    cid = factor(cid),
    cses = factor(cses, levels = c(0, 1, 2), labels = c("Low", "Middle", "High")),
    ases = factor(ases, levels = c(0, 1, 2), labels = c("Low", "Middle", "High")),
    ac = factor(ac,
      levels = c(0, 1, 2, 3, 4),
      labels = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward")
    )
  )

# Setup parallel processing
num_cores <- parallel::detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run mixed-effects models in parallel
models <- foreach(model_type = c("cses", "ases", "ac"), .packages = c("lme4", "dplyr")) %dopar% {
  if (model_type == "cses") {
    model <- glmer(
      depression ~ cses + sex + age + marital + alive + smoke +
        drink + exercise + cid + (1 | id),
      family = binomial(link = "logit"), data = data
    )
  } else if (model_type == "ases") {
    model <- glmer(
      depression ~ ases + sex + age + marital + alive + smoke +
        drink + exercise + cid + (1 | id),
      family = binomial(link = "logit"), data = data
    )
  } else {
    model <- glmer(
      depression ~ ac + sex + age + marital + alive + smoke +
        drink + exercise + cid + (1 | id),
      family = binomial(link = "logit"), data = data
    )
  }
  return(model)
}

stopCluster(cl)

# Assign models to variables
model_cses <- models[[1]]
model_ases <- models[[2]]
model_ac <- models[[3]]

# Save all models together
save(model_cses, model_ases, model_ac, file = "results/depression_models.RData")

# Model summaries
summary(model_cses)
summary(model_ases)
summary(model_ac)

# Helper function to get most common factor level
get_mode <- function(x) {
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

# Function to create prediction data for plotting
create_pred_data <- function(model_name, group_var) {
  age_range <- range(data$age, na.rm = TRUE)
  age_seq <- seq(age_range[1], age_range[2], length.out = 20)

  default_values <- lapply(data[sapply(data, is.factor)], get_mode)
  default_values$id <- NULL

  newdata <- expand.grid(age = age_seq, group = levels(data[[group_var]]), stringsAsFactors = TRUE)
  names(newdata)[2] <- group_var

  for (var in names(default_values)) {
    if (var != group_var) {
      newdata[[var]] <- default_values[[var]]
    }
  }

  model <- get(paste0("model_", model_name))
  pred <- predict(model, newdata, type = "response", re.form = NA)

  plot_data <- data.frame(
    age = rep(age_seq, length(levels(data[[group_var]]))),
    group = factor(rep(levels(data[[group_var]]), each = length(age_seq)),
      levels = levels(data[[group_var]])
    ),
    fit = pred
  )
  names(plot_data)[2] <- group_var

  pred_with_ci <- predict_with_ci(model, newdata)
  plot_data$ci_lower <- pred_with_ci$ci_lower
  plot_data$ci_upper <- pred_with_ci$ci_upper

  return(plot_data)
}

# Calculate confidence intervals for predictions
predict_with_ci <- function(model, newdata, alpha = 0.05) {
  X <- model.matrix(delete.response(terms(model)), newdata)
  betas <- fixef(model)
  vcov_mat <- vcov(model)
  eta <- X %*% betas
  se <- sqrt(diag(X %*% vcov_mat %*% t(X)))

  qnorm_val <- qnorm(1 - alpha / 2)
  ci_lower_link <- eta - qnorm_val * se
  ci_upper_link <- eta + qnorm_val * se

  fit <- plogis(eta)
  ci_lower <- plogis(ci_lower_link)
  ci_upper <- plogis(ci_upper_link)

  return(data.frame(fit = fit, ci_lower = ci_lower, ci_upper = ci_upper))
}

# Generate prediction data
plot_data_cses <- create_pred_data("cses", "cses")
plot_data_ases <- create_pred_data("ases", "ases")
plot_data_ac <- create_pred_data("ac", "ac")

# Custom color palette
custom_colors <- c(
  "#6A61AF", # R:106, G:97, B:175
  "#A888AC", # R:168, G:136, B:172
  "#D691AD", # R:214, G:145, B:173
  "#F8A299", # R:248, G:162, B:153
  "#FAD797" # R:250, G:215, B:151
)

# Function to create plots with consistent styling
create_plot <- function(plot_data, group_var, title) {
  n_levels <- length(unique(plot_data[[group_var]]))
  colors_to_use <- custom_colors[1:min(n_levels, length(custom_colors))]

  p <- ggplot(plot_data, aes_string(x = "age", y = "fit", color = group_var)) +
    geom_ribbon(aes_string(ymin = "ci_lower", ymax = "ci_upper", fill = group_var),
      alpha = 0.1, color = NA
    ) +
    geom_line(size = 1.2) +
    labs(
      title = title,
      x = "Age",
      y = "Predicted Probability of Depression"
    ) +
    theme_minimal(base_size = 14) +
    scale_color_manual(values = colors_to_use) +
    scale_fill_manual(values = colors_to_use) +
    guides(color = guide_legend(title = NULL), fill = guide_legend(title = NULL)) +
    scale_x_continuous(limits = c(50, 100), breaks = seq(50, 100, by = 10)) +
    scale_y_continuous(limits = c(0.1, 0.7)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = NA, color = NA),
      legend.margin = margin(6, 6, 6, 6),
      legend.key.size = unit(1, "lines"),
      legend.text = element_text(size = 13),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    )

  return(p)
}

# Create individual plots
p1 <- create_plot(plot_data_cses, "cses", "Childhood SES")
p2 <- create_plot(plot_data_ases, "ases", "Adult SES")
p3 <- create_plot(plot_data_ac, "ac", "SES Change")

# Save each plot separately
ggsave("depression_cses_trajectory.pdf", p1, width = 10, height = 7)
ggsave("depression_ases_trajectory.pdf", p2, width = 10, height = 7)
ggsave("depression_ac_trajectory.pdf", p3, width = 10, height = 7)



#Table1, Supplementary Tables S8-S13, Supplementary Tables S29-S34
library(haven)
library(dplyr)
library(purrr)
library(compareGroups)
library(parallel) # For parallel computing
library(foreach) # For parallel loops
library(doParallel) # For parallel backend

## Setup parallel computing environment
# Detect available cores and reserve one for the operating system
no_cores <- detectCores() - 1
if (no_cores < 1) no_cores <- 1 # Ensure at least one core is available
cat(sprintf("Setting up parallel computing with %d cores\n", no_cores))

# Create cluster and register
cl <- makeCluster(no_cores)
registerDoParallel(cl)

# Start timing the analysis
start_time <- Sys.time()
cat(sprintf("Analysis started at: %s\n", start_time))

## List all .dta files in the Data directory
data_files <- list.files(path = "Data", pattern = "\\.dta$", full.names = TRUE)

## Create a named vector of file paths
file_names <- basename(data_files) # Extract just the filename
file_names <- sub("\\.dta$", "", file_names) # Remove the .dta extension
names(data_files) <- file_names

## Read all data files in parallel and store them in a list
cat("Loading data files in parallel...\n")
data_list <- foreach(file = data_files, .packages = "haven") %dopar% {
  read_dta(file)
}
names(data_list) <- file_names

## Assign each dataset to its own variable in the global environment
list2env(data_list, envir = .GlobalEnv)

## Print information about loaded datasets
cat("Successfully loaded the following datasets:\n")
for (name in names(data_list)) {
  cat(sprintf(
    "%s: %d rows * %d columns\n",
    name,
    nrow(data_list[[name]]),
    ncol(data_list[[name]])
  ))
}

## Data cleaning function
clean_data <- function(data, factor_vars = list()) {
  # Create a copy to avoid modifying original data
  cleaned_data <- data
  
  # Step 1: Convert all variables to numeric (if possible)
  # cat("Converting all variables to numeric...\n")
  for (col in names(cleaned_data)) {
    # Try to convert variable to numeric
    tryCatch(
      {
        # Check if haven_labelled type (Stata labeled values)
        if (inherits(cleaned_data[[col]], "haven_labelled")) {
          # Get labels
          labels <- haven::as_factor(cleaned_data[[col]])
          # Convert to numeric
          cleaned_data[[col]] <- as.numeric(cleaned_data[[col]])
          # Save label information to attributes
          attr(cleaned_data[[col]], "labels") <- labels
        } else {
          # Try direct conversion to numeric
          cleaned_data[[col]] <- as.numeric(as.character(cleaned_data[[col]]))
        }
      },
      error = function(e) {
        # cat(sprintf("Warning: Unable to convert variable '%s' to numeric. Error: %s\n", col, e$message))
      }
    )
  }
  
  # Step 2: Convert specified variables to factors
  # cat("Converting specified variables to factors...\n")
  for (var_name in names(factor_vars)) {
    # Check if variable exists in the dataset
    if (var_name %in% names(cleaned_data)) {
      # cat(sprintf("Processing variable: %s\n", var_name))
      # Get factor levels and labels
      levels_and_labels <- factor_vars[[var_name]]
      
      # Create factor variable
      cleaned_data[[var_name]] <- factor(
        cleaned_data[[var_name]],
        levels = levels_and_labels$levels,
        labels = levels_and_labels$labels
      )
      
      # Print factorization results
      # cat(sprintf(
      #   "  - Successfully converted '%s' to factor with %d levels\n",
      #   var_name, length(levels_and_labels$levels)
      # ))
    } else {
      # cat(sprintf("Skipping: Variable '%s' does not exist in the current dataset\n", var_name))
    }
  }
  
  return(cleaned_data)
}

## Define factors with their levels and labels
base_levels <- list(
  levels = c(0, 1),
  labels = c("0", "1")
)

depression_levels <- list(
  levels = c(0, 1),
  labels = c("No", "Yes")
)

sex_levels <- list(
  levels = c(1, 2),
  labels = c("Male", "Female")
)

marital_status_levels <- list(
  levels = c(0, 1),
  labels = c("Other", "Married")
)

parent_alive_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Both dead", "One alive", "Both alive")
)

smoke_levels <- list(
  levels = c(0, 1),
  labels = c("Never / ever smoke", "Current smoke")
)

drink_levels <- list(
  levels = c(0, 1),
  labels = c("No", "Yes")
)

exercise_levels <- list(
  levels = c(0, 1),
  labels = c("No", "Yes")
)

father_job_levels <- list(
  levels = c(0, 1, 2),
  labels = c("0", "1", "2")
)

father_income_levels <- list(
  levels = c(0, 1, 2),
  labels = c("0", "1", "2")
)

family_disaster_levels <- list(
  levels = c(0, 1),
  labels = c("Low", "Middle")
)

father_education_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Below high school", "High school", "Above high school")
)

childhood_ses_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Low", "Middle", "High")
)

family_wealth_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Low", "Middle", "High")
)

family_income_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Low", "Middle", "High")
)

education_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Below high school", "High school", "Above high school")
)

adult_ses_levels <- list(
  levels = c(0, 1, 2),
  labels = c("Low", "Middle", "High")
)

ses_change_levels <- list(
  levels = c(0, 1, 2, 3, 4),
  labels = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward")
)

# Create a list with all variables to be factorized
factor_variables <- list(
  base = base_levels,
  depression = depression_levels,
  sex = sex_levels,
  marital = marital_status_levels,
  alive = parent_alive_levels,
  smoke = smoke_levels,
  drink = drink_levels,
  exercise = exercise_levels,
  fjob = father_job_levels,
  financial = father_income_levels,
  disaster = family_disaster_levels,
  fedu = father_education_levels,
  cses = childhood_ses_levels,
  wealth = family_wealth_levels,
  income = family_income_levels,
  education = education_levels,
  ases = adult_ses_levels,
  ac = ses_change_levels
)

## Clean data in parallel, preserving NA values
cat("\nCleaning datasets in parallel...\n")
cleaned_data_list <- foreach(i = 1:length(data_list), .packages = c("haven")) %dopar% {
  name <- names(data_list)[i]
  cat(sprintf("Starting cleaning dataset: %s\n", name))
  
  # Clean data without replacing NA values
  cleaned_data <- clean_data(data_list[[i]], factor_variables)
  
  # Return cleaned data
  cleaned_data
}
names(cleaned_data_list) <- names(data_list)

## Replace NA values in 'base' variable with 0 (base variable only)
cat("\nReplacing NA values in 'base' variable with 0...\n")
for (name in names(cleaned_data_list)) {
  if ("base" %in% names(cleaned_data_list[[name]])) {
    na_count <- sum(is.na(cleaned_data_list[[name]]$base))
    if (na_count > 0) {
      cleaned_data_list[[name]]$base[is.na(cleaned_data_list[[name]]$base)] <- 0
      cat(sprintf(
        "  Dataset %s: Replaced %d NA values in 'base' variable with 0\n",
        name, na_count
      ))
    } else {
      cat(sprintf("  Dataset %s: No NA values found in 'base' variable\n", name))
    }
  } else {
    cat(sprintf("  Dataset %s: No 'base' variable found\n", name))
  }
}

## Create output directories
if (!dir.exists("results/groupby_depression_base1")) {
  dir.create("results/groupby_depression_base1", recursive = TRUE)
}

if (!dir.exists("results/groupby_base")) {
  dir.create("results/groupby_base", recursive = TRUE)
}

# Create job list, each item containing dataset name and analysis parameters
cat("\nPreparing descriptive statistics jobs...\n")
jobs <- list()

# Depression grouping jobs
# HRS
jobs[["HRS_depression"]] <- list(
  dataset = "HRS",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/HRS.csv"
)

# CHARLS
jobs[["CHARLS_depression"]] <- list(
  dataset = "CHARLS",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/CHARLS.csv"
)

# SHARE
jobs[["SHARE_depression"]] <- list(
  dataset = "SHARE",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/SHARE.csv"
)

# ELSA
jobs[["ELSA_depression"]] <- list(
  dataset = "ELSA",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + disaster + fedu + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/ELSA.csv"
)

# MHAS
jobs[["MHAS_depression"]] <- list(
  dataset = "MHAS",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/MHAS.csv"
)

# KLoSA
jobs[["KLoSA_depression"]] <- list(
  dataset = "KLoSA",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + cses + wealth + income + education + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/KLoSA.csv"
)

# Pooled data
jobs[["pooled_depression"]] <- list(
  dataset = "pooled",
  formula = "depression ~ time + sex + age + marital + alive + smoke + drink + exercise + cses + ases + ac",
  subset = expression(base == 1),
  output_file = "results/groupby_depression_base1/Pool.csv"
)

# Base grouping jobs
# HRS
jobs[["HRS_base"]] <- list(
  dataset = "HRS",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/HRS.csv"
)

# CHARLS
jobs[["CHARLS_base"]] <- list(
  dataset = "CHARLS",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/CHARLS.csv"
)

# SHARE
jobs[["SHARE_base"]] <- list(
  dataset = "SHARE",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/SHARE.csv"
)

# ELSA
jobs[["ELSA_base"]] <- list(
  dataset = "ELSA",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + disaster + fedu + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/ELSA.csv"
)

# MHAS
jobs[["MHAS_base"]] <- list(
  dataset = "MHAS",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + fjob + financial + fedu + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/MHAS.csv"
)

# KLoSA
jobs[["KLoSA_base"]] <- list(
  dataset = "KLoSA",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + cses + wealth + income + education + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/KLoSA.csv"
)

# Pooled data
jobs[["pooled_base"]] <- list(
  dataset = "pooled",
  formula = "base ~ depression + time + sex + age + marital + alive + smoke + drink + exercise + cses + ases + ac",
  subset = NULL,
  output_file = "results/groupby_base/Pool.csv"
)

# Run all jobs in parallel
cat("Switching to sequential processing...\n")

# Stop parallel cluster
stopCluster(cl)
cat("Parallel cluster stopped\n")

# Process all tasks sequentially
for (job_name in names(jobs)) {
  cat(sprintf("\n===== Task: %s =====\n", job_name))
  job <- jobs[[job_name]]
  
  dataset <- cleaned_data_list[[job$dataset]]
  if (is.null(dataset)) {
    cat(sprintf("Error: Dataset '%s' not found\n", job$dataset))
    next
  }
  
  formula_str <- job$formula
  cat(sprintf("Formula: %s\n", formula_str))
  formula_obj <- as.formula(formula_str)
  
  tryCatch(
    {
      if (is.null(job$subset)) {
        result <- descrTable(
          formula_obj,
          data = dataset,
          digits = 2,
          show.all = TRUE,
          include.miss = TRUE
        )
      } else {
        subset_result <- eval(job$subset, dataset)
        result <- descrTable(
          formula_obj,
          data = dataset,
          digits = 2,
          show.all = TRUE,
          include.miss = TRUE,
          subset = subset_result
        )
      }
      
      dir.create(dirname(job$output_file), recursive = TRUE, showWarnings = FALSE)
      
      cat(sprintf("Exporting to: %s\n", job$output_file))
      export2csv(result, file = job$output_file)
      
      if (file.exists(job$output_file)) {
        cat(sprintf("Success: File created (%d bytes)\n", file.size(job$output_file)))
      } else {
        cat("Error: File not created\n")
      }
    },
    error = function(e) {
      cat(sprintf("Error: %s\n", e$message))
    }
  )
}

# Calculate total runtime
end_time <- Sys.time()
total_time <- difftime(end_time, start_time, units = "mins")
cat(sprintf("\nAnalysis completed at: %s\n", end_time))
cat(sprintf("Total execution time: %.2f minutes\n", as.numeric(total_time)))



#Supplementary Figure S6-S8. Kaplan-Meier survival curves
#HRS
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/HRS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/HRS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/HRS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#CHARLS
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/CHARLS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/CHARLS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/CHARLS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#SHARE
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/SHARE.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/SHARE.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/SHARE.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#ELSA
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/ELSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/ELSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/ELSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#MHAS
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/MHAS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/MHAS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/MHAS.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#KLoSA
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/KLoSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/KLoSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/KLoSA.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



#Pooled
library(haven)         
library(survival)      
library(survminer)     
library(ggplot2)       
data <- read_dta("G:/2025/SESDEP/data/pooled.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ cses, data = data_filtered)

cox_model <- coxph(surv_obj ~ cses + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["cses", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["cses", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["cses", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ cses, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279","#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Childhood SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/pooled.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ases, data = data_filtered)

cox_model <- coxph(surv_obj ~ ases + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ases", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ases", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ases", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ases, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Low", "Middle", "High"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#FFD699"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("Adult SES") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 8),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)


data <- read_dta("G:/2025/SESDEP/data/pooled.dta")
data_filtered <- subset(data, base == 1)

surv_obj <- Surv(time = data_filtered$time, event = data_filtered$depression)

fit_km <- survfit(surv_obj ~ ac, data = data_filtered)

cox_model <- coxph(surv_obj ~ ac + age + sex + marital + alive + smoke + drink + exercise + cid, 
                   data = data_filtered)

hr <- summary(cox_model)$coefficients["ac", "exp(coef)"]
ci_lower <- summary(cox_model)$conf.int["ac", "lower .95"]
ci_upper <- summary(cox_model)$conf.int["ac", "upper .95"]
log_rank_p <- survdiff(surv_obj ~ ac, data = data_filtered)$p
p_value_text <- ifelse(log_rank_p < 0.001, "Log rank p < 0.001", sprintf("Log rank p = %.3f", log_rank_p))

km_plot <- ggsurvplot(fit_km, data = data_filtered, 
                      pval = FALSE,            
                      conf.int = TRUE,        
                      legend.title = " ",     
                      legend.labs = c("Downward", "Stable low", "Stable middle", "Stable high", "Upward"),
                      risk.table = FALSE,      
                      ggtheme = theme_minimal(), 
                      palette = c("#F88279", "#19C4C9", "#EBCC2D","#FFD699","#E6D0DE"), 
                      xlab = "Time (Years)")   

km_plot$plot <- km_plot$plot +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.12,  
           label = sprintf("HR (95%% CI): %.3f (%.3f - %.3f)", hr, ci_lower, ci_upper),
           size = 4, hjust = 0, color = "black") +
  annotate("text", x = max(data_filtered$time) * 0.05, y = 0.04,  
           label = p_value_text,
           size = 4, hjust = 0, color = "black") +
  ggtitle("SES Change") + 
  theme(axis.line = element_line(color = "black"),  
        panel.grid = element_blank(),               
        legend.text = element_text(size = 6),       
        legend.title = element_text(size = 8),      
        plot.title = element_text(size = 16, hjust = 0.5))  

print(km_plot)



# Supplementary Figure S16 (A) HRS: Health and Retirement Study;
# Supplementary Figure S17 (A) HRS: Health and Retirement Study;
# Supplementary Table S18 The edge weights matrices of the HRS network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("HRS.dta")  
data_12w <- data_all


# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 11 # Number of nodes (with out covariates)
num_Cov <- 7 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:8), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 8), rep("SES", 3))
myname <- c("depressed","effort","sleep","happy","lonely","enjoy","sad","going","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",8),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数



# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)



# Supplementary Figure S16 (B) CHARLS: China Health and Retirement Longitudinal Study;
# Supplementary Figure S17 (B) CHARLS: China Health and Retirement Longitudinal Study;
# Supplementary Table S19 The edge weights matrices of the CHARLS network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("CHARLS.dta")  
data_12w <- data_all  


# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 13 # Number of nodes (with out covariates)
num_Cov <- 7 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:10), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 10), rep("SES", 3))
myname <- c("bother","trouble","depressed","effort","hopeful","fearful","restless","happy","lonely","going","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",10),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数


# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)



# Supplementary Figure S16 (C) SHARE: Survey of Health, Ageing and Retirement in Europe;
# Supplementary Figure S17 (C) SHARE: Survey of Health, Ageing and Retirement in Europe;
# Supplementary Table S20 The edge weights matrices of the SHARE network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("SHARE.dta")  
data_12w <- data_all  

# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 15 # Number of nodes (with out covariates)
num_Cov <- 8 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:12), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise","cid") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 12), rep("SES", 3))
myname <- c("depressed","hopeful","dead","guilty","sleep","interest","irritable","appetite","energy","concentration","enjoy","cry","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",12),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数


# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)



# Supplementary Figure S16 (D) ELSA: English Longitudinal Study of Ageing;
# Supplementary Figure S17 (D) ELSA: English Longitudinal Study of Ageing;
# Supplementary Table S21 The edge weights matrices of the ELSA network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("ELSA.dta")  
data_12w <- data_all  

# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 11 # Number of nodes (with out covariates)
num_Cov <- 7 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:8), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 8), rep("SES", 3))
myname <- c("depressed","effort","restless","happy","lonely","enjoy","sad","going","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",8),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数



# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)



# Supplementary Figure S16  (E) MHAS: Mexican Health and Aging Study;
# Supplementary Figure S17  (E) MHAS: Mexican Health and Aging Study;
# Supplementary Table S21 The edge weights matrices of the ELSA network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("MHAS.dta")  
data_12w <- data_all  


# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 12 # Number of nodes (with out covariates)
num_Cov <- 7 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:9), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 9), rep("SES", 3))
myname <- c("depressed","effort","restless","happy","lonely","enjoy","sad","tired","energy","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",9),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数


# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)



# Supplementary Figure S16 (F) KLoSA, Korean Longitudinal Study of Aging;
# Supplementary Figure S17 (F) KLoSA, Korean Longitudinal Study of Aging;
# Supplementary Table S22 The edge weights matrices of the MHAS network.
library(readxl)
library(haven)
library(mgm) 
library(Hmisc)
library(bootnet)
library(qgraph)
library(glmnet)
library(lavaan)
library(dplyr)
library(gridExtra)
library(networktools)
library(ggplot2)
library(patchwork)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data_all <- read_dta("KLoSA.dta")  
data_12w <- data_all  


# Convert to matrices (先读数据，数据要求宽格式，变量顺序类似T1x1，T1x2，T1x3，T1y1,T1y2,T1y3,T2x1，T2x2，T2x3，T2y1,T2y2,T2y3,age，gender，ses，第一波数据、第二波数据、协变量)
df1 <- as.matrix(data_12w)

k <- 12 # Number of nodes (with out covariates)
num_Cov <- 7 # Number of covariates
adjMat_Cov <- matrix(0, nrow=(k+num_Cov),ncol=(k+num_Cov)) # Empty adjacency matrix including covariates

# Cross-lagged panel network function
CLPN.fun <- function(df) {
  for (i in 1:k){
    lassoreg_Cov<-cv.glmnet(x=as.matrix(df[,c(1:k,(k*2+1):(k*2+num_Cov))]),
                            y=df[,(k+i)], 
                            nfolds=10,
                            family="gaussian", 
                            alpha=1, 
                            standardize=T)
    lambda_Cov<-lassoreg_Cov$lambda.min
    adjMat_Cov[(1:(k+num_Cov)),i] <- coef(lassoreg_Cov, 
                                          s=lambda_Cov, 
                                          exact=F)[2:(num_Cov+k+1)]
  }
  adjMat_Cov1 <- getWmat(adjMat_Cov,
                         nNodes=k+num_Cov, 
                         labels=labels_cov, 
                         diRested=T) 
  adjMat_Cov<- adjMat_Cov1[1:k, 1:k]
  return(adjMat_Cov)
}


# Variable labels and groups (变量分组，比如有两个组，x组和y组，x组有13个变量，y组有21个变量)
labels <- c(paste0("D",1:9), paste0("SES",1:3)) # 变量标签，网络上节点的名字
labels_cov <- c(labels, "age","sex","marital","alive","smoke","drink","exercise") # 协变量标签，根据实际情况替换"age", "gender", "ses"这一部分
groups <- c(rep("Depression", 9), rep("SES", 3))
myname <- c("bother","trouble","depressed","effort","hopeful","fearful","restless","happy","lonely","wealth","income","education") # 图例，根据实际情况替换
communities <- c( rep("1",9),
                  rep("2",3)) # 社区，做桥接症状要用，“1”和“2”是两个社区，旁边的数字代表该社区的节点数


# Estimate network
mynetwork1 <- estimateNetwork(df1, fun = CLPN.fun, labels = labels, directed = T)
write.csv(mynetwork1$graph, "edgeWeight_T1→T2.csv")
summary(mynetwork1)

# Prepare matrix for visualization （这里去掉了自回归系数，画图的时候别的边可以明显一点，如果要保留，就不要运行第150行）
mat12 <- mynetwork1$graph
diag(mat12) <- 0

# Function to find top n edge weights
find_top_values <- function(mat, n = 3) {
  abs_mat <- abs(mat)
  top_n_indices <- order(abs_mat, decreasing = TRUE)[1:n]
  top_n_values <- mat[top_n_indices]
  top_n_rows <- row(mat)[top_n_indices]
  top_n_cols <- col(mat)[top_n_indices]
  row_names <- rownames(mat)[top_n_rows]
  col_names <- colnames(mat)[top_n_cols]
  data.frame(Value = top_n_values, Row = row_names, Column = col_names)
}

# Get top edges
top_edges_T1_T2 <- find_top_values(mat12, n = 3)

# Network visualization
pdf("NCTstruc1.pdf", width = 13, height = 10)
qgraph(mat12, groups = groups, labels = labels, title="T1→T2", title.cex=1.5,
       posCol="#6095ce", negCol="#e87d72", nodeNames=myname, 
       color = c("#decbe0","#c9d8eb"))
dev.off()

# Centrality analysis （中心性折线图，同时会导出中心性数据，可以自己另外画图）
plot_centrality <- function(network_list, group_names, use = "ExpectedInfluence", centralize = TRUE, sort_by_first_network = TRUE, group_colors = NULL) {
  library(dplyr)
  library(ggplot2)
  
  if (length(network_list) != length(group_names)) {
    stop("The number of networks and group names must be the same.")
  }
  
  # 设置默认颜色，如果未提供
  if (is.null(group_colors)) {
    group_colors <- scales::hue_pal()(length(group_names))
  }
  
  if (length(group_colors) != length(group_names)) {
    stop("The number of group colors must match the number of groups.")
  }
  
  # 创建颜色映射
  group_colors <- setNames(group_colors, group_names)
  
  # 初始化空数据框
  combined_data <- data.frame()
  
  # 遍历每个网络并计算中心性
  for (i in seq_along(network_list)) {
    network <- network_list[[i]]
    group_name <- group_names[i]
    
    centrality_table <- centralityTable(network, standardized = FALSE)
    
    # 保存中心性表到文件
    file_name <- paste0("Centrality_", group_name, ".csv")
    write.csv(centrality_table, file_name, row.names = FALSE)
    
    centrality_filtered <- centrality_table %>% filter(measure == use)
    
    # 标准化中心性值
    if (centralize) {
      centrality_filtered$z <- (centrality_filtered$value - mean(centrality_filtered$value, na.rm = TRUE)) / 
        sd(centrality_filtered$value, na.rm = TRUE)
    } else {
      centrality_filtered$z <- centrality_filtered$value
    }
    
    # 添加组信息
    centrality_filtered$group <- group_name
    
    # 合并数据
    combined_data <- rbind(combined_data, centrality_filtered)
  }
  
  # 按照第一个网络的中心性排序
  if (sort_by_first_network) {
    sorted_nodes <- combined_data %>% 
      filter(group == group_names[1]) %>% 
      arrange(value) %>% 
      pull(node) %>% 
      unique()
    
    combined_data$node <- factor(combined_data$node, levels = sorted_nodes)
  } else {
    combined_data$node <- factor(combined_data$node)
  }
  
  # 绘制图表
  p_Centrality <- ggplot(data = combined_data,
                         aes(x = node, y = z, group = group, colour = group)) +
    geom_point(size = 1.8) +
    labs(x = "Nodes", y = use) +
    geom_line(size = 1) +
    scale_colour_manual(values = group_colors) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank()) +
    coord_flip()
  print(p_Centrality)
  return(p_Centrality)
}
p1 <- plot_centrality (list(mynetwork1),c("T1→T2"),"OutExpectedInfluence",group_colors = c("#9bc0c5"))
p2 <- plot_centrality (list(mynetwork1),c("T1→T2"),"InExpectedInfluence",group_colors = c("#9bc0c5"))

pdf("Centrality_plots.pdf", width = 8, height = 8)  
print(p1 / p2)
dev.off() 

# Stability analysis
b1 <- bootnet(mynetwork1, nCores = 1, nBoots=1000, directed = T, type = "nonparametric", communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

b2 <- bootnet(mynetwork1, nBoots=1000, nCores = 1, type = "case", directed = T, communities=communities,
              statistics = c("outExpectedInfluence", "inExpectedInfluence", "edge"))

# Stability visualization
pdf("OEIdiff.pdf", width = 10, height = 8)
p_oeidiff <- plot(
  b1,
  "outExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_oeidiff)    ## 修改：显式打印 ggplot 对象
dev.off()


pdf("IEIdiff.pdf", width = 10, height = 8)
p_ieidiff <- plot(
  b1,
  "inExpectedInfluence",
  plot = "difference",
  labels = TRUE,
  order = "sample"
)
print(p_ieidiff)    ## 修改：显式打印 ggplot 对象
dev.off()

# pdf("bridgeEIdiff.pdf", width = 10, height = 8) # 不做桥接不用跑这个
# plot(b1, "bridgeExpectedInfluence", plot = "difference", labels = TRUE, order = "sample")
# dev.off()

pdf("edgeStability.pdf", width = 10, height = 8)
p_edgeStab <- plot(b1,
                   "edge",
                   order  = "sample",
                   labels = FALSE)
print(p_edgeStab) 
dev.off()

pdf("centralityStability.pdf", width = 10, height = 8)
p_centralStab <- plot(b2,
                      c("inExpectedInfluence", "outExpectedInfluence"), 
                      facet = TRUE)
print(p_centralStab) 
dev.off()

# Stability coefficients
corStability(b2)