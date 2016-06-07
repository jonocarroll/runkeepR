# https://github.com/smbache/magrittr/issues/29
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".")) ## alternatively, . <- "Shut up"
