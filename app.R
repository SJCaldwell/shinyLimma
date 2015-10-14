rm(list = ls())
library(shiny)
library(shinyBS)

campaigns_list <- letters[1:10]

ui =fluidPage(
  checkboxGroupInput("campaigns","Choose campaign(s):",campaigns_list),
  actionLink("selectall","Select All"),
  bsModal("modalExample", "Yes/No", "selectall", size = "small",wellPanel(
    actionButton("no_button", "Yes"),
    actionButton("yes_button", "No")
  ))
)
server = function(input, output, session) {
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"campaigns","Choose campaign(s):",choices=campaigns_list)      
    }
    else
    {
      updateCheckboxGroupInput(session,"campaigns","Choose campaign(s):",choices=campaigns_list,selected=campaigns_list)
    }
  })
 

}
runApp(list(ui = ui, server = server))