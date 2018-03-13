
table_info <- function(df, width=100){
  return(
    formattable::format_table(
      df,
      align="l",
      table.attr = paste("class=\"table table-condensed\"; style=\"width: ", 
                         width, "%\"", sep=""),
      list(
        Parameter = formattable::formatter("span", style = "font-weight:bold"),
        Description = formattable::formatter("span", style = "")
      )
    )
  )
}
