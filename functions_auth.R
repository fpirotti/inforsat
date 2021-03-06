library(shinyalert)
require(bcrypt)
require(shinyvalidate) ### remotes::install_github("rstudio/shinyvalidate")

#### METTERE in server.R
####  source("functions_auth.R", local=T)

ra<- reactiveValues( LOGGED.USER=NULL, IS.LOGGED=F, IS.UNIPD=F)

iv <- InputValidator$new()
iv$add_rule("password", sv_required())
iv$add_rule("email", sv_required())
iv$add_rule("email", sv_email())
iv$enable()

if(file.exists("users.pwds.rda")) load("users.pwds.rda") else users.pwds<-list()

checkPass<-T

password_accept = function(x){ 
  print(input$email)
  
  
  print(input$resetpwd)
  print("==========")
  
  if(is.null(users.pwds[[ input$email ]]) ){
    title<-"EMail not recognized"
    message<-sprintf("Use the 'Send password' buttonto sent a password to %s. <br>
                    Access will be monitored and blacklisted if abused.", input$email)
  } else {
    is.correct.pw<-F
    if(is.character(users.pwds[[ input$email ]]) && 
       length(input$password)>2 ) {
      is.correct.pw<-bcrypt::checkpw(input$password, users.pwds[[ input$email ]])
    } 
    if( is.correct.pw ){
      ra$IS.LOGGED<-T
      ra$LOGGED.USER<-input$email
      if(is.null(logs[[ input$email ]])) {
        logs[[ input$email ]]<-Sys.time()
      }
      else {
        logs[[ input$email ]]<-c(logs[[ input$email ]], Sys.time())
      }
      save(logs, file="logs.rda")
      
      return()
    }
    
    title<-"EMail recognized but wrong password"
    message<-sprintf("Try again.")
  }
     
  
  shinyalert( title, html = TRUE, closeOnEsc = F, showConfirmButton = F,
              closeOnClickOutside = F, 
              text = tagList(  
                HTML(message), 
                textInput("email", NULL),
                passwordInput("password", NULL),
                div(style="cursor:pointer; width:150px;height:36px; corner-radius:4px; padding:5px; background-color:#FF000044;", 
                    onclick="Shiny.setInputValue('resetpwd', true,  {priority: 'event'});", 
                    title="A mail with a password will be sent - can also be used to reset your password if you forget it", 
                    "Send password" ) ,
                actionButton("ok_pw", "Log IN", icon = icon("user"))
              ) , callbackR = password_accept  )
    
  }


observeEvent(input$resetpwd, {
  
  req(input$resetpwd)
  
  print("here33")
  if(!isValidEmail(input$email)){
    shinyalert("Email non corretta")
    return()
  }
  setPassword(input$email)
})

isValidEmail <- function(x) {
  grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}$\\>", as.character(x), ignore.case=TRUE)
}


#' setPassword
#'
#' @param x username (email address of user)
#'
#' @return
#' @export
#'
#' @examples #
setPassword<-function(x){
  print("here")
  pw<-fun::random_password(12, extended = F)
  if(!exists("users.pwds")) users.pwds<<-list()
  users.pwds[[x]]<<- bcrypt::hashpw(pw)
  save(users.pwds, file="users.pwds.rda")
  comm<-sprintf('echo "Your password is: %s - do not reply to this mail.\\n\\nInForSat Team\\nCIRGEO Interdepartmenta Research Center in Geomatics" | mailx -s "InForSat password" --append "From: CIRGEO InForSAT <donotreply@unipd.it>" --append="BCC:<francesco.pirotti@unipd.it>"  %s',
                pw, input$email  ) 
  res<-system(comm, intern = T)
  print(res)
}


observeEvent( input$email, {
  
   shinyjs::toggleState("resetpwd", isValidEmail( input$email ) )
})

shinyalert( html = TRUE, closeOnEsc = F, showConfirmButton = F,
            closeOnClickOutside = F, 
            text = tagList(  
              HTML("<b>Email and password</b><br>if your email is not enabled push the button 'Send password' - password will be sent to the address"), 
              textInput("email", NULL),
              passwordInput("password", NULL),
              div(style="cursor:pointer; width:150px;height:36px; corner-radius:4px; padding:5px; background-color:#FF000044;", 
                  onclick="Shiny.setInputValue('resetpwd', true,  {priority: 'event'});", 
                  title="A mail with a password will be sent - can also be used to reset your password if you forget it", 
                  "Send password" ) ,
              actionButton("ok_pw", "Log IN", icon = icon("user"))
            ) , callbackR = password_accept  )

