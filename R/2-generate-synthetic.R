# Project:   evs-lookalike
# Objective: Synthesize the toy data for distribution
# Author:    Edoardo Costantini
# Created:   2023-11-23
# Modified:  2024-01-31
# Notes: 

# Load packages
library(mice)      # to create the synthetic data
library(ggmice)    # to make visualizations of the synthetic data
library(ggplot2)   # required when using ggmice
library(patchwork) # to stitch multiple figures together
library(psych)     # to obtain descriptive statistics
library(purrr)     # to work with multiply imputed synthetic datasets
library(synthpop)  # to assess the utility of our synthetic data

# Read toy data set
EVS_toy <- readRDS("./data/EVS_toy.rds")

# Explore data -----------------------------------------------------------------

# Summaries
summary(EVS_toy)

# Distributions
describe(EVS_toy)

# 1. Where ---------------------------------------------------------------------

where <- make.where(EVS_toy, "all")

# 2. Synthesize data -----------------------------------------------------------

# Single imputation
syn_cart <- mice(
    EVS_toy,
    m = 1,
    maxit = 20,
    method = "cart",
    where = where,
    printFlag = TRUE
)

# Store the data set
EVS_syn <- complete(syn_cart)

# 3. Utility -------------------------------------------------------------------

# Univariate utility

colnames(EVS_toy)[map_lgl(EVS_toy, is.factor)] %>%
    map(~ ggmice(syn_cart, mapping = aes_string(.x, group = ".imp")) +
        geom_bar(
            mapping = aes(y = ..prop..),
            position = position_dodge2(),
            fill = "transparent",
            show.legend = TRUE
        )) %>%
    patchwork::wrap_plots()

# Multivariate utility
mult_utility <- utility.gen.list(EVS_syn, EVS_toy)

# 4. Check disclosure ----------------------------------------------------------

# Check if observations in the original data occur in the synthetic data
complete(syn_cart, "long") %>%
    dplyr::select(-c(.imp, .id)) %>%
    dplyr::bind_rows(EVS_toy) %>%
    duplicated() %>%
    which()

# 4. Re-impose missing values --------------------------------------------------

# Impose the EVS missing data mask
EVS_syn[is.na(EVS_toy)] <- NA

# Save it 
saveRDS(EVS_syn, "./data/EVS_syn.rds")