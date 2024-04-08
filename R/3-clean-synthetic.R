# Project:   evs-lookalike
# Objective: Clean the synthetic data
# Author:    Edoardo Costantini
# Created:   2024-04-08
# Modified:  2024-04-08
# Notes: 

# packages
library(tidyverse)

# Read data
EVS <- readRDS("./data/EVS_syn.rds")

# Get rid of observations with edge cases for certain variables (too few observations with this category)
EVS <- EVS %>%
  filter(
    v267 != "does not apply to me (spontaneous)",
    v246_egp != "not applicable"
  )

# Drop empty levels for factors
index_factors <- sapply(EVS, is.factor)

# Check if there are empty levels
index_empty <- sapply(
  EVS[, index_factors, drop = FALSE],
  function(x) {
    any(table(x) == 0)
  }
)

# What factors have empty categories
index <- names(index_factors) %in% names(which(index_empty))

# Check if any factors met the condition
if (any(index)) {
  # Apply drop levels to all
  slim_factors <- lapply(
    EVS[, index, drop = FALSE],
    droplevels
  )

  # Replace original factors with empty catories with slim versions
  EVS[, index] <- as.data.frame(slim_factors)
}

# Check variables with few categories
EVS_check <- lapply(EVS, function(x) {
  # Proportions
  props <- table(x) / sum(table(x)) * 100

  # Extract names and correct order
  data.frame(
    levels = names(props),
    proportion = as.vector(props)
  )
})

# Check the first few
head(EVS_check)

# Combine categories for values that are too small
EVS_recoded <-
  within(
    data = EVS,
    expr = {
      v174_LR <- fct_collapse(EVS$v174_LR,
        `8plus` = levels(EVS$v174_LR)[7:8]
      )
      v246_egp <- fct_collapse(EVS$v246_egp,
        others = levels(EVS$v246_egp)[c(3, 4)],
        IV = levels(EVS$v246_egp)[c(5, 6, 11)],
        VII = levels(EVS$v246_egp)[c(9:10)]
      )
      v4 <- fct_collapse(EVS$v4,
        `not important` = levels(EVS$v4)[1:2]
      )
      v3 <- fct_collapse(EVS$v3,
        `not important` = levels(EVS$v3)[1:2]
      )
      v2 <- fct_collapse(EVS$v2,
        `not important` = levels(EVS$v2)[1:2]
      )
      v1 <- fct_collapse(EVS$v1,
        `not important` = levels(EVS$v1)[1:2]
      )
      v54 <- fct_collapse(EVS$v54,
        fequently = levels(EVS$v54)[1:3],
        rarely = levels(EVS$v54)[3:6],
        never = levels(EVS$v54)[7]
      )
      v234 <- fct_collapse(EVS$v234,
        `not married` = levels(EVS$v234)[2:5],
        `never married` = levels(EVS$v234)[6]
      )
      v243_ISCED_1 <- fct_collapse(EVS$v243_ISCED_1,
        low = levels(EVS$v243_ISCED_1)[1:3],
        medium = levels(EVS$v243_ISCED_1)[4:6],
        high = levels(EVS$v243_ISCED_1)[7:9]
      )
    }
  )

# Check variables with few categories
EVS_check <- lapply(EVS_recoded, function(x) {
  # Proportions
  props <- table(x) / sum(table(x)) * 100

  # Extract names and correct order
  data.frame(
    levels = names(props),
    proportion = as.vector(props)
  )
})

# Inspect categories are as expected
EVS_check

# Commit to EVS_recoded
saveRDS(EVS_recoded, "./data/EVS.rds")