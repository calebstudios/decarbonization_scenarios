#/******************************************************************************
# PROGRAM NAME: tbl_ops.R
# PROJECT: Decarbonization Scenarios
# DESCRIPTION: General-purpose table operations and array-style math utilities
# DATE: 2025-12-28
# R-VERSION: R version 4.5.2 (2025-11-01)
#******************************************************************************/

f_arr_op <- function(df_a, 
                     df_b, 
                     op, 
                     operand_a, 
                     operand_b, 
                     by = "meas", 
                     result_col = NULL, 
                     suffix = c(".a", ".b"), 
                     keep = c("all", "keys_and_result")) {
  
  keep <- match.arg(keep)
  op_func <- match.fun(op)
  
  if (is.null(result_col)) {
    result_col <- paste0(operand_a, "_", operand_b, "_result")
  }
  
  joined <- dplyr::left_join(df_a, df_b, by = by, suffix = suffix)
  
  # Figure out which column name operand_b has after join
  # If operand_b exists in df_a too, it will come in as operand_b.b
  operand_b_joined <- if (operand_b %in% names(df_a)) paste0(
    operand_b, suffix[2]) else operand_b
  operand_a_joined <- if (operand_a %in% names(df_b)) paste0(
    operand_a, suffix[1]) else operand_a
  
  out <- joined %>% 
    dplyr::mutate(
      "{result_col}" := op_func(.data[[operand_a_joined]], 
                                .data[[operand_b_joined]])
    )
  
  if (keep == "keys_and_result") {
    out <- out %>% 
      dplyr::sekect(dplyr::all_of(by), dplyr::all_of(result_col))
  }
  
  out
}

# df_a <- data.frame(
#   sector = c("X", "X", "Y", "Y"), 
#   meas = c("A", "B", "A", "B"), 
#   count = c(200, 400, 27, 49), 
#   total_cost = c(200, 800, 27, 98)
# )
# 
# df_b <- data.frame(
#   meas = c("A", "B"), 
#   cost = c(1000, 800), 
#   total_cost = c(227000, 3600)
# )
# 
# f_arr_op(df_a, df_b, '+', 'count', 'cost', by = "meas")
# f_arr_op(df_a, df_b, '*', 'count', 'cost', by = "meas", 
#          result_col = "count_x_cost")
