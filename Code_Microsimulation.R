library(tidyverse)
library(ipfp)

ind <- read_csv("individuals.csv")
AgeSex_con <- read_csv("age_sex.csv")
Ethnicity_con <- read_csv("Ethnicity.csv")
Health_con <- read_csv("Health.csv")
Work_con <- read_csv("Work.csv")

# Loading Functions to perform integerization of fractional weights
# each line of code follows what the code does
int_trs <- function(x){
  xv <- as.vector(x)  # Convert input to a vector
  xint <- floor(xv)  # Get the integer part (floor) of each element
  r <- xv - xint  # Get the fractional part of each element
  def <- round(sum(r))  # Determine how many units need to be added to integers to preserve totals
  topup <- sample(length(x), size = def, prob = r)  # Sample indices to increment, proportional to fractional parts
  xint[topup] <- xint[topup] + 1  # Add 1 to sampled indices
  dim(xint) <- dim(x)  # Reshape to original dimensions
  dimnames(xint) <- dimnames(x)  # Preserve original dimnames
  xint  # Return integerized vector
}

# Function to expand a vector of counts into repeated indices
int_expand_vector <- function(x){
  index <- 1:length(x)  # Create index vector
  rep(index, round(x))  # Repeat each index based on the corresponding count
}

# Function to convert a categorical variable column into a dummy matrix
cat_list_fun <- function(ind_var){
  cat_var <- model.matrix(~ as.data.frame(ind)[,ind_var]-1)  # Create dummy variables for the given column
  colnames(cat_var) <- levels(ind[,ind_var])  # Set column names to factor levels
  return(cat_var)  # Return dummy matrix
}

# Main microsimulation function
microsim <- function(ind_var_cols, cons, food_vars){
  
  cat_list <- lapply(ind_var_cols, cat_list_fun)  # Create list of dummy matrices for all categorical variables
  ind_cat <- as.data.frame(do.call(cbind, cat_list))  # Combine all dummy matrices into a single data frame
  
  ind_catt <- t(ind_cat)  # Transpose the indicator matrix
  x0 <- rep(1, nrow(ind))  # Initial weights (vector of ones)
  
  mean_food <- NULL  # Initialize output for mean food consumption
  replicates <- NULL  # Initialize output for simulated individuals
  
  set.seed(42)  # Set seed for reproducibility
  for(i in 1:nrow(cons)){  # Loop over all zones
    zoneid <- as.character(cons[i,"zone_id"])  # Extract zone ID as a character
    print(paste("Calculating consumption estimates ", zoneid, " ", i, " of ", nrow(cons), sep = ""))  # Progress message
    cons_zone <- as.numeric(cons[i,!(names(cons) %in% c("zone_name", "zone_id"))])  # Extract target marginal totals for the zone
    weights <- ipfp(cons_zone, ind_catt, x0, maxit = 20)  # Run IPFP (Iterative Proportional Fitting) to get weights
    ints <- int_expand_vector(int_trs(weights))  # Integerize and expand weights into replicated indices
    tib <- tibble(ind[ints,])  # Create tibble of individuals based on replicated indices
    agg <- group_by(tib,id) %>% summarise(n_indv = length(id))  # Count number of individuals per ID
    food_msim <- left_join(agg,ind, by = "id")  # Merge aggregated individuals with original data
    
    reps_zone <- agg %>%  # Prepare replicate info for zone
      mutate(zone_id = zoneid)
    
    replicates <- rbind(reps_zone, replicates)  # Append to replicate data
    
    agg_food <- as_tibble(t(colSums(food_msim[,c("n_indv",food_vars)]))) %>%  # Aggregate food variables and number of individuals
      mutate(across(all_of(food_vars), ~ ./n_indv)) %>%  # Calculate per-individual mean food consumption
      mutate(zone_id = zoneid)  # Add zone ID
    mean_food <- rbind(mean_food, agg_food)  # Append to mean food output
  }
  return (list(mean_food = mean_food, replicates = replicates))  # Return results as a list
}

# PERFORMING SPATIAL MICROSIMULATION for a single variable
# how pork consumption varies within the Exeter region
cons <- filter(Ethnicity_con, grepl('Exeter', zone_name)) 

# Specify which ind(data) variables to use (i.e. Ethnicity only)
ind_var_cols <- c("Ethnicity")

# Prepare Ethnicity col for microsimulation
# for the code to function correctly, the 'Ethnicity' column needs to be in the form of a factor (in alphabetical order) rather than as a plain text
# its just another way of storing the data within a column
ind <- ind %>%
  mutate(Ethnicity = factor(Ethnicity, levels = sort(unique(Ethnicity))))

# List of food products to create estimates for. in this case we only need porn
food_vars <- c("Pork")

# Run Microsimulation (Ethnicity only)
Ethnicity_microsim  <- microsim(ind_var_cols, cons, food_vars)


# RUNNING A MICROSIMULATION WITH ALL VARIABLES


# Join together all geographical (spatial) data sets for which to utilize in microsimulation
# this will contain all LSOA zones (33,753) but will also have 33 columns (18 age-sex, 5 eth, 5 health,3 work, zone name and zone)
cons_all <- reduce(list(AgeSex_con, Ethnicity_con, Health_con, Work_con),left_join, by = (c("zone_name", "zone_id")))

# Filter constraint dataset for Oxford only
cons <- filter(cons_all, grepl('Oxford ', zone_name))

# Create new column (Age-Sex) to match the constraint table format  
# 'ind; have seperate age and sex, cons have combined (in the form of Fa0_02)
# we will create a new variable in the ind dataset by linking the age and sex variable
ind <- ind %>%
  mutate(AgeSex = paste(Sex,Age, sep = ""))

# Convert columns to factor type (levels in alphabetical order) (already done for ethnicity)
ind <- ind %>%
  mutate(AgeSex = factor(AgeSex, levels = sort(unique(AgeSex))),
         Health = factor(Health, levels = sort(unique(Health))),
         Work = factor(Work, levels = sort(unique(Work))))

# we need to specify the variables to be sued for microsimulation
# Specify which ind variables to use (i.e. all) - ensure in same order as in cons dataset
ind_var_cols <- c("AgeSex","Ethnicity","Health","Work")

# also need to specify the food products we wish to analyse
# List of food products to create estimates for (all)
food_vars <- c("Beef", "Lamb", "Pork", "ProcessedRedMeat", "Burgers", "Sausages", "Offal", "Poultry", "GameBirds",  "Fish", "Alcohol", "Fruit", "Veg")

Full_microsim  <- microsim(ind_var_cols, cons, food_vars)

#view the microsimulation results
View(Full_microsim[["mean_food"]])

# Save results to file
write_csv(Full_microsim[["mean_food"]], "output_mean_foods_Oxford.csv")

# Spatially visualize the results by joining the "output_mean_foods.csv" with Oxford's LSOA geopackage.
# distribution of Alcohol across Oxford's LSOA