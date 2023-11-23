# Project:   evs-lookalike
# Objective: Generate data that looks like EVS data
# Author:    Edoardo Costantini
# Created:   2023-11-09
# Modified:  2023-11-09
# Notes: 

#
library(dplyr)

# Load EVS data
EVS <- readRDS("data/ZA7500_processed.rds") # smaller version

# Load variable types
var_types <- readRDS("data/var_types.rds")

# Define subset of rows
EVS <- EVS %>%
    filter(
        country %in% c(
            "Netherlands"
        )
    )
EVS$country <- droplevels(EVS$country)

# Define model variables
model_vars <- c(
    # Left / Right voting
    lr = "v174_LR",

    # Female
    sex = "v225",

    # Employment Status
    SES = "v246_egp",

    # Native attitudes (mean of items)
    nativ_1 = "v185", # jobs
    nativ_2 = "v186", # crime
    nativ_3 = "v187", # strain on welfare

    # Authoritarian Attitudes
    # Low and order attitudes
    strongL = "v145",
    order = "v110",

    # Political Interest
    pol_1 = "v97",

    # Political Action (mean of items)
    pa_1 = "v98",
    pa_2 = "v99",
    pa_3 = "v100",
    pa_4 = "v101",

    # Covariates
    age = "age_r3",
    edu = "v243_ISCED_1",
    mat = "v234",

    # Religiousness
    rel = "v54",

    # Denomination
    denom = "v52_r"
)

# Keep 5 variables per group on top of the model variables
var_subset <- unlist(
    lapply(
        var_types[1:3],
        function(x) {
            head(x[!x %in% model_vars], 5)
        }
    )
)

var_subset <- c(
    var_types$ord[2:6],
    var_types$bin[1:5],
    var_types$cat[c(16:19, 31)]
)

#
EVS_toy <- EVS[, c(model_vars, var_subset)]

# Store data
saveRDS(EVS_toy, "data/EVS_toy.rds")