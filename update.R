#' Update PHEI_functions module
#' 
#' The \code {PHEI_functions/update} looks for updates in the remote repo and updates the module if needed. This function is present in other modules as well. 

'.__module__.'
#' Looks for new commits in the GitLab repo for PHEI_functions. Changes can be pulled if found.
#' @param pull Set to TRUE by default. If FALSE, the function will only look for changes and update
#' @export

update <- function(pull = TRUE) {
  
  proj_url <- "@hertscc.managed.mango-solutions.com/git/hcc_phei/tools/phei_functions.git"
  
  # check status of local folder by fetching data and then checking git status
  branch_status <- system(paste0("cd ../../phei_functions \n
                                git fetch \n 
                                git status"), intern = TRUE)[2]
  
  # if up to date, the second line should say it is up to date with origin/master
  
  if (branch_status == "Your branch is up to date with 'origin/master'.") {
    
    message("You are up to date with the latest version.")
    
  } else {
    
    if (pull == T) {
      
      # fetch once again, and pull updates
      system(paste0("cd ../../phei_functions
                  git fetch https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url,
                  "\n git pull https://", Sys.getenv("GIT_PH_USER"), ":", Sys.getenv("GIT_PH_PASS"), proj_url, " master"))
      
      message("Updates pulled. You are up to date with the latest version.")
      
    } else {
      
      # if pull is set to FALSE, just state that updates were found without pulling. 
      message("Updates found. Please pull the latest version.")
      
    }
  }
  
}