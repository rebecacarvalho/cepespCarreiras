#https://github.com/GV-CEPESP/cepesputils/tree/master/R
cpp_check_cpf <- function(cpfs){

  if(!is.character(cpfs)){
    stop("cpfs must be character vector.")
  }

  if(!all(nchar(cpfs) == 11)){
    stop("cpfs must have 11 characters. Use cpp_cpf to adjust that.")
  }

  ## Split
  split <- stringr::str_split_fixed(cpfs, "", 11)
  split <- matrix(as.numeric(split), ncol = 11)

  ## First digit
  weights <- matrix(seq(10, 2), ncol = 1)
  sum <- as.numeric(split[,1:9] %*% weights)
  remainder <- sum %% 11
  first_digit <- 11 - remainder
  first_digit <- ifelse(first_digit > 9, 0, first_digit)

  ## Second digit
  split_fd <- cbind(split, first_digit, deparse.level = 0)
  weights <- matrix(c(11, weights), ncol = 1)
  sums <- as.numeric(split_fd[,c(1:9, 12)] %*% weights)
  remainders <- sums %% 11
  second_digit <- 11 - remainders
  second_digit <- ifelse(second_digit > 9, 0, second_digit)

  ## Validade
  cod_validate <- cbind(first_digit, second_digit, deparse.level = 0)
  cod_disponivel <-  split[,10:11]
  valid_code <- ifelse(cod_validate == cod_disponivel, 1, 0)

  cpf_is_valid <- (valid_code[,1] == 1) & (valid_code[,2] == 1)

  return(cpf_is_valid)
}
