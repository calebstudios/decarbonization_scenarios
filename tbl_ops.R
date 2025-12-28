

f.arr_op <- function(df_a, df_b, op, 
                     operand_A, operand_B, join_by_col = "meas") {
  
  # Define the name of the new results column
  results_col <- paste(operand_A, operand_B, "result", sep = "_")
  
  # Use a named vector for 'by' argument in join
  matching_indexes <- setNames(join_by_col, join_by_col)
  
  # Perform the operation using the appropriate operator function
  # The 'op' argument should be a function (e.g., `+`, `-`, etc.)
  op_func <- match.fun(op)
  
  joined_df <- left_join(df_a, df_b, by = matching_indexes) %>% 
    mutate(
      # Assign the new column using the `:=` operator for dynamic naming
      # and the embrace operator {{ to use the string `results_col` as the name
      "{{results_col}}" := op_func(!!sym(operand_A), !!sym(operand_B)), 
      # Keep only necessary columns
      .keep = "unused"
    )
  
  return(joined_df)
}

# For testing: 
df_a <- data.frame(sector = c("X", "X", "Y", "Y"), 
                   meas = c("A", "B", "A", "B"), 
                   count = c(200, 400, 27, 49), 
                   total_cost = c(200, 800, 27, 98)
)

df_b <- data.frame(meas = c("A", "B"), 
                   cost = c(1000, 800), 
                   total_cost = c(227000, 3600))

f.arr_op(df_a, df_b, '+', 'count', 'cost')
f.arr_op(df_a, df_b, '*', 'count', 'cost', 'total_cost')
