library(stringr)

extract_functions <- function(x, evaluate = TRUE, verbose = TRUE, 
                              envir = parent.frame()) {
  lines <- readLines(x)
  
  extracted_functions <- list()
  brace_count <- 0
  in_function <- FALSE
  in_definition <- FALSE
  
  for(current_line in lines) {
    # first check if already in a function
    if (!in_function | in_definition){
      # if not, check if current_line is the beginning of a function
      if (str_detect(current_line, "<-[[:blank:]]*function[[:blank:]]*\\(")) {
        # and add the current_line to the next list item of extracted_functions
        extracted_functions <- append_to_extracts(extracted_functions, 
                                                  current_line, where = "new")
        if (str_detect(current_line, "\\{")) {
          # no longer in definition
          in_definition <- FALSE
          # reset brace_count
          brace_count <- 0
          # if so indicate
          in_function <- TRUE
          # increase the brace_count
          brace_count <- brace_count + count_braces(current_line)
        } else {
          in_definition <- TRUE
        }
      } else {
        if (in_definition) {
        # and add the current_line to the last list item of extracted_functions
        extracted_functions <- append_to_extracts(extracted_functions, 
                                                  current_line, where = "last")
          if (str_detect(current_line, "\\{")) {
            # no longer in definition
            in_definition <- FALSE
            # reset brace_count
            brace_count <- 0
            # if so indicate
            in_function <- TRUE
            # increase the brace_count
            brace_count <- brace_count + count_braces(current_line)
          }
        }
      }
    } else {
      # if already in a function, first update brace count with the current_line
      brace_count <- brace_count + count_braces(current_line)
      
      # if brace_count is higher than 0, function has not ended yet
      if (brace_count > 0) {
        # append line to current item of extracted_functions
        extracted_functions <- append_to_extracts(extracted_functions, 
                                                  current_line, where = "last")
        
      # if brace_count is 0 the function has ended correctly
      } else if (brace_count == 0) {
        in_function <- FALSE
        extracted_functions <- append_to_extracts(extracted_functions, 
                                                  current_line, where = "last")
        
      # else the function has ended incorrectly
      } else {
        extracted_functions <- append_to_extracts(extracted_functions, 
                                                  where = "corrupt")
        brace_count <- 0
        in_function <- FALSE
      }
    }
  }
  
  extracted_functions <- lapply(extracted_functions, paste, collapse = "\n")
  
  if (verbose) {
    catch <- lapply(extracted_functions, function(x){
        cat(x)
        cat("\n\n----\n\n")
      })
  }
  
  if (evaluate) {
    valid_functions <- sapply(extracted_functions, function(x) {
      x != "corrupt function"
    })
     
    extracted_functions_to_eval <- lapply(extracted_functions[valid_functions], 
                                          function(x){parse(text = x)})
    
    invisible(
      lapply(extracted_functions_to_eval, function(x, envir) {
        eval(x, envir = envir)
      }, envir = envir)
    )
  }
}


count_braces <- function(x) {
  opening_braces <- sum(nchar(unlist(str_extract_all(x, "\\{"))))
  opening_braces <- ifelse(length(opening_braces) == 1, opening_braces, 0)
  closing_braces <- sum(nchar(unlist(str_extract_all(x, "\\}"))))
  closing_braces <- ifelse(length(closing_braces) == 1, closing_braces, 0)
  
  out <- 0 + opening_braces - closing_braces
  return(out)
}

append_to_extracts <- function(extracts_list, x, where) {
  if (where == "last") {
    extracts_list[[length(extracts_list)]] <- 
      c(extracts_list[[length(extracts_list)]], x)
  } 
  if (where == "new") {
    extracts_list[[length(extracts_list) + 1]] <- x
  }
  if (where == "corrupt") {
    # TODO Add line number or other indicator
    extracts_list[[length(extracts_list)]] <- "corrupt function"
  }
  return(extracts_list)
}



#### TEST #### 
# extract_functions("test.R", evaluate = TRUE, verbose = TRUE)
