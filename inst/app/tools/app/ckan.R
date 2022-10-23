#######################################
# Save to CKAN menu
#######################################
decode_apikey<-function(sss){
  key='jtjqyoLMzPJiZGY='
  keya=strsplit(key,"")[[1]]
  stringa=urlsafebase64becode(sss,mode="raw")
  stringa=paste("0x",stringa,sep="")
  encoded_chars = NULL
  for(i in 1:length(stringa)){
    rd=i %% length(keya)
    if(rd==0){rd=length(keya)}
    key_c = keya[rd ]
    encoded_c = intToUtf8((strtoi(stringa[i]) - utf8ToInt(key_c) +256 ) %% 256)
    encoded_chars=c(encoded_chars,encoded_c)
  }
  return(paste(encoded_chars,collapse=""))

}
# lowercase<-function(input){
#   if (length(input)==0){
#     return(NULL)
#   }
#   if (grepl("^[[:upper:]]+$", input)==TRUE){
#     return(FALSE)
#   }else{
#     return(NULL)
#   }
# }
# nospace<-function(input){
#   if (length(input)==0){
#     return(NULL)
#   }
#   if (grepl("\\s+",input$ckan_authemail, perl = TRUE)==TRUE){
#     return(FALSE)
#   }else{
#     return(NULL)
#   }
# }
# email<-function(input){
#   if (length(input)==0){
#     return(NULL)
#   }
#   if (grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input)==FALSE){
#     return(FALSE)
#   }else{
#     return(NULL)
#   }
# }

output$ckan<-renderUI({fluidPage(
  titlePanel("Send File to Server"),
  div(
    id = "state_ckan_inputs",
    textInput("ckan_author", "Author", ""),
    textInput("ckan_authemail", "Author Email (requires valid form)"),
    textInput("ckan_title", "Title (lowercase only, spaces allowed"),
    textInput("ckan_name", "URL - igenomed.stanford.edu/dataset/... (no spaces)"),
    textInput("ckan_des", "Description"),
    actionButton("ckan_submit", "Submit", class = "btn-primary"),
  )
)})

# ui_validating<- reactive({
#   validate(
#     need(lowercase(input$ckan_title), "Only lower cases for Title"),
#     need(email(input$ckan_authemail), "Not a valid Author Email"),
#     need(nospace(input$ckan_ckan_name), "No spaces for Name")
#   )
# })


saveckan1 <- function(author, authemail, title_ckan, name_ckan, description_ckan){
  title_ckan2<- paste(title_ckan, ".state.rda", sep="")
  isolate({
        LiveInputs <- toList(input)
        r_state[names(LiveInputs)] <- LiveInputs
        r_data <- active2list(r_data)
        r_info <- toList(r_info)
        path2<- paste("C:/Users/timot/Documents/", title_ckan2, sep="") ## CHANGE FOR SERVER ##
        save(r_state, r_data, r_info, file = path2)
      }
    )
  key1=decode_apikey(r_info[["api_key"]])
  ckanr::ckanr_setup(url = "https://igenomed.stanford.edu/", key = key1)
  ckan_file<-ckanr::package_create(title=title_ckan, author = author, author_email = authemail, owner_org="test", name=name_ckan)
  ckanr::resource_create(package_id = ckan_file$id, upload = path2 , rcurl = paste("https://igenomed.stanford.edu/dataset/",name_ckan,sep=""), description = description_ckan, name = title_ckan2, format="STATE.RDA") ## CHANGE FOR SERVER ##
}

observeEvent(input$ckan_submit, {
  withProgress(message = "Sending to Server!", value = 1, {
    author <- input$ckan_author
    authemail <-input$ckan_authemail
    title_ckan <- input$ckan_title
    name_ckan<-input$ckan_name
    description_ckan<-input$ckan_des
    saveckan1(author, authemail, title_ckan, name_ckan,description_ckan)
  })
})
