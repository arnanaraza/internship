### Script to switch temporary R files in spacious directory ###

TempFiles <- function(tempdir){
  ifelse(!dir.exists(file.path(tempdir)), dir.create(file.path(tempdir)), FALSE)
  write(paste0('"TMP = ',tempdir,'"'), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
  print(paste0('temp files will now be stored at ',tempdir ))
}
