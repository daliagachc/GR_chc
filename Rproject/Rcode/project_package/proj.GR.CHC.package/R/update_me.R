#' update me
#'
#' function to automatically update the project package.
#'
#' @return Nuthn'
#' @export
#'
#' @examples update_me()
update_me=function ()
{
  temp = list.files(path = paste(here::here(), "Rcode/project_package/",
                                 sep = "/"), pattern = "*.tar.gz")
  install.packages(paste(here::here(), "Rcode/project_package",
                         temp[length(temp)], sep = "/"), repos = NULL, type = "source")
}
