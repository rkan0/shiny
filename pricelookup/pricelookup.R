library(shiny)
library(tidyverse)
library(stringr)
library(shinyjs)


# In each rate programme (e.g. flexible, non-flexible prepaid, tour group, staff use, third party bookings) there are multiple rate conditions (e.g. including breakfast, for particular seasons). For each rate condition, there is maximum one member rate and one non-member rate per room type, each represented by a unique ratecode. Each ratecode is 2 digits for the rate programme + 2 letters for the rate condition + 1 digit for the room type. The rate code ever changes along the room type, but never across programme or condition or between member and non-member. If a guest adds benefits e.g. breakfast, full board that is considered a 'room' upsell. 

# For strategic (revenue) reasons, the price difference is ideally to be captured on the nightly room rate, so that the original rate and room category are both updated. When there is no existing rate code in the new category within a particular condition, only the price difference is added without change to the rate code. Third-party bookings, which are synced with external systems, are not to be amended -- the price difference is added to the folio as a manual charge. 


# create dfs
rmtype_list <- c("Standard", "Deluxe", "Premium", "Executive", "Suite", "Luxury", "Signature")

ratecond_list <- c("10flex_room", "10flex_bf", "10flex_long", "12nonflex_room", "12nonflex_bf", "12nonflex_festive", "30thirdparty", "40staff")
ratecond_df <- data.frame(
  ratecond = ratecond_list,
  ratecond_mem = c("RM", "RE", "LN", "PM", "BM", "XM", "", "EM"),
  ratecond_non = c("RR", "RB", "", "PP", "PB", "XS", "EX", "")
)
rmno_df <- data.frame(
  rmtype = rmtype_list,
  rmno = sample(paste0(rep(1:7, each = 8), sprintf("%02d",rep(1:8))), size = 56, replace = FALSE))

data <- data.frame(
  rmtype = rep(rmtype_list, times = 8),
  ratecond = rep(ratecond_list, each = 7)
)

data <- data %>%
  left_join(ratecond_df, by = "ratecond") %>%
  mutate(ratepgm = substr(ratecond, 1, 2)) 

data <- data %>%  
  mutate(ratecond_mem = ifelse((ratecond_mem %in% c("XM", "EM")) & rmtype %in% c("Luxury", "Signature"), "", as.character(ratecond_mem))) %>%
  mutate(ratecond_non = ifelse(ratecond_non == "XS" & rmtype %in% c("Luxury", "Signature"), "", as.character(ratecond_non))) %>%
  mutate(ratecond_mem = ifelse(ratecond_mem == "EM" & rmtype %in% c("Premium", "Executive", "Suite"), "", as.character(ratecond_mem))) 

data <- data %>%
  pivot_longer(cols = ratecond_mem:ratecond_non, names_to = "membership", values_to = "ratecode9") %>%
  filter(ratecode9 != "") %>%
  mutate(ratecode = paste0(ratepgm, ratecode9, as.numeric(rmtype))) %>%
  complete(ratecond, rmtype, membership, fill = list(membership = "ratecond_mem", ratecode = "N/A; overwrite rate only")) %>%
  mutate(ratepgm = substr(ratecond, 1, 2)) %>%
  mutate(ratecode = ifelse(ratecode == "N/A; overwrite rate only" & ratepgm == "30", "Use manual charge", as.character(ratecode))) %>%
  left_join(rmno_df, by = "rmtype")
  
diff <- data.frame(
  orig_roomcat = rep(rmtype_list, times = 7),
  target_roomcat = rep(rmtype_list, each = 7),
  a1 = rep(1:7, times = 7),
  a2 = rep(1:7, each = 7)
)
diff_long <- diff %>%
  mutate(aufpreis = ifelse(a1 == a2, "Same category", ifelse(a1>a2, "Downgrade", (a2-a1)*10)))
                                     
levels1 <- unique(data$ratecode) #as.factor doesn't allow keyboard input for selectize
levels1 <- levels1[! levels1 %in% c("N/A; overwrite rate only")]



#### UI####
ui <- fluidPage(
  id = "form",
  tags$head(HTML("<title>Upsell lookup</title>")),
  headerPanel(
    fluidRow("")),
  titlePanel(""),
  hr(),
  h4("Step 1: What is the current booking?"),
  fluidRow(
    column(4, 
           selectizeInput('ratecode', 'Rate code', 
                          c(Choose = '', levels1), options = list(
                            placeholder = 'Start typing',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
    ),
    column(4, 
           selectizeInput('rmno', 'Room number', 
                          c(Choose = '', levels(data$rmno)), options = list(
                            placeholder = 'Start typing',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
           ),
    column(4, 
           selectizeInput('rmtype', 'Room type', 
                          c(Choose = '', levels(data$rmtype)), options = list(
                            placeholder = 'Start typing',
                            onInitialize = I('function() { this.setValue(""); }')
                          ))
           )
    ),
  mainPanel(
    tabsetPanel(
      id = "tabset",
      type = "hidden",
      tabPanelBody("aclearpanel", 
                   selected = TRUE),
      tabPanelBody("ratecodepanel", 
                   textOutput("sel_ratecode"), #a1a
                   textOutput("return_rmtype_a1b"),
                   hr(),
                   h4("Step 2: What is the target room?"),
                   fluidRow(
                     column(6, 
                            selectInput('t_rmtype', 'Room type',c(Choose = '', levels(data$rmtype)), selectize = TRUE),
                            textOutput("tar_rmtype"),   #a2f 
                            textOutput("return_ratecode_a2g"),
                            textOutput("return_aufpreis_a2h")
                     ),
                     column(6, 
                            selectInput('t_rmno', 'Room number',c(Choose = '', levels(data$rmno)), selectize = TRUE),
                            textOutput("tar_rmno") , #a2a 
                            textOutput("return_rmtype_a2k"),
                            textOutput("return_ratecode_a2c"),
                            textOutput("return_aufpreis_a2d")
                            )
                     )
                   ),
      tabPanelBody("rmnopanel", 
                   textOutput("sel_rmno"), #b1a
                   textOutput("return_rmtype_b1b"),
                   hr(),
                   h4("Step 2: What is the target room?"),
                   fluidRow(
                     column(6, 
                            selectInput('t_rmtypeb', 'Room type',c(Choose = '', levels(data$rmtype)), selectize = TRUE),
                            textOutput("tar_rmtypeb"),
                            textOutput("return_b2g_aufpreis")
                     ),
                     column(6, 
                            selectInput('t_rmnob', 'Room number',c(Choose = '', levels(data$rmno)), selectize = TRUE),
                            textOutput("tar_rmnob"),
                            textOutput("return_rmtype_b2d"),
                            textOutput("return_b2e_aufpreis")
                     )
                   )
      ),
      tabPanelBody("rmtypanel", 
                   textOutput("sel_rmtype") , #c1a
                   hr(),
                   h4("Step 2: What is the target room?"),
                   fluidRow(
                     column(6,
                            selectInput('t_rmtypec', 'Room type',c(Choose = '', levels(data$rmtype)), selectize = TRUE),
                            textOutput("tar_rmtypec"), #c2e
                            textOutput("return_c2f_aufpreis")
                            ),
                     column(6,
                            selectInput('t_rmnoc', 'Room number',c(Choose = '', levels(data$rmno)), selectize = TRUE),
                            textOutput("tar_rmnoc"), #c2b
                            textOutput("return_rmtype_c2c"),
                            textOutput("return_c2d_aufpreis"))
                     )
                   )
      ),
    br(),hr(),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
    actionButton("refresh", "reset")
    )
  )


#### server ####

server <- function(input, output, session) {
  
  ## Select the tab according to in which selectinput the last input was made ##
  
  # reactive values to track selected values
  selected_values <- reactiveValues(ratecode = NULL, rmno = NULL, rmtype = NULL, clear = NULL)
  
  # observe changes in select inputs
  observe_select_changes <- function(input_id, tab_id) {
    observeEvent(input[[input_id]], {
      selected_values[[input_id]] <- input[[input_id]]
      updateTabsetPanel(session, "tabset", selected = tab_id)
    })
  }
  
  # observe changes in all select inputs
  lapply(list(c("ratecode", "ratecodepanel"), c("rmno", "rmnopanel"), c("rmtype", "rmtypanel"),c("clear","clearpanel")), function(x) {
    observe_select_changes(x[[1]], x[[2]])
  })
  
  # track interaction
  interaction_occurred <- reactiveVal(FALSE)  
  observe({
    if (!interaction_occurred()) {
      updateTabsetPanel(session, "tabset", selected = "aclearpanel")
    }
  })
  
  # clear all
  observeEvent(input$refresh, {
    shinyjs::js$refresh_page()
  })
  
 
  #panel A 
  
  #a1a + d1a
  output$sel_ratecoded <- output$sel_ratecode <- renderText({ 
    paste("Booked rate code:", input$ratecode)
  })
  
  return_rmtype_a1b <- reactive({
    data$rmtype[data$ratecode == input$ratecode][1] #return first output only 
  })
  output$return_rmtype_a1b <- renderText({
    paste("Booked room type:", return_rmtype_a1b())
  })
  
  #a2f 
  output$tar_rmtype <- renderText({ 
    paste("Target room type:", input$t_rmtype)
  })
  
  #a2g
  # also possible to match by ratecode_9, but since the rate codes may not always be designated logically-sequentially, here 'membership' is a stand-in for any grouping variable other than rate_cond
  return_ratecode_a2g <- reactive({
    data$ratecode[data$rmtype == input$t_rmtype & data$membership == data$membership[data$ratecode == input$ratecode] & data$ratecond == data$ratecond[data$ratecode == input$ratecode][1]][1]
  })
  output$return_ratecode_a2g <- renderText({
    paste("New rate code:", return_ratecode_a2g())
  })
  
  #a2h
  return_aufpreis_a2h <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == data$rmtype[data$ratecode == input$ratecode][1] & diff_long$target_roomcat == input$t_rmtype][1]
  })
  output$return_aufpreis_a2h <- renderText({
    paste("Price difference:", return_aufpreis_a2h())
  })
  
  #a2a 
  output$tar_rmnob <- output$tar_rmno <- renderText({ 
    paste("Target room number:", input$t_rmno)
  }) 
  
  
  return_rmtype_a2k <- reactive({
    data$rmtype[data$rmno == input$t_rmno][1] #return first output only 
  })
  output$return_rmtype_a2k <- renderText({
    paste("Target room type:", return_rmtype_a2k())
  })
  
  
  #a2c
  return_ratecode_a2c <- reactive({
    data$ratecode[data$rmno == input$t_rmno & data$membership == data$membership[data$ratecode == input$ratecode] & data$ratecond == data$ratecond[data$ratecode == input$ratecode][1]][1]
  })
  output$return_ratecode_a2c <- renderText({
    paste("New rate code:", return_ratecode_a2c())
  })
  
  #a2d
  return_aufpreis_a2d <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == data$rmtype[data$ratecode == input$ratecode][1] & diff_long$target_roomcat == data$rmtype[data$rmno == input$t_rmno][1]][1]
  })
  output$return_aufpreis_a2d <- renderText({
    paste("Price difference:", return_aufpreis_a2d())
  })

  #panel B
  
  #b1a
  output$sel_rmno <- renderText({ 
    paste("Booked room number:", input$rmno)
  }) 
  
  #b1b
  return_rmtype_b1b <- reactive({
    data$rmtype[data$rmno == input$rmno][1] 
  })
  output$return_rmtype_b1b <- renderText({
    paste("Booked room type:", return_rmtype_b1b())
  })
  
  #b2c 
  output$tar_rmnob <- renderText({ 
    paste("Target room number:", input$t_rmnob)
  }) 
  
  #b2d 
  return_rmtype_b2d <- reactive({
    data$rmtype[data$rmno == input$t_rmnob][1] #return first output only 
  })
  output$return_rmtype_b2d <- renderText({
    paste("Target room type:", return_rmtype_b2d())
  })
  
  #b2f
  output$tar_rmtypeb <- renderText({ 
    paste("Target room type:", input$t_rmtypeb)
  })
  
  #b2g
  return_b2g_aufpreis <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == data$rmtype[data$rmno == input$rmno][1] & diff_long$target_roomcat == input$t_rmtypeb][1]
  })
  output$return_b2g_aufpreis <- renderText({
    paste("Price difference:", return_b2g_aufpreis())
  })
  
  #B2e
  return_b2e_aufpreis <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == data$rmtype[data$rmno == input$rmno][1] & diff_long$target_roomcat == data$rmtype[data$rmno == input$t_rmnob][1]][1]
  })
  output$return_b2e_aufpreis <- renderText({
    paste("Price difference:", return_b2e_aufpreis())
  })
  
  #panel C
  
  #c1a
  output$sel_rmtype <- renderText({ 
    paste("Booked room type:", input$rmtype)
  }) 
  
  
  #c2b
  output$tar_rmnoc <- renderText({ 
    paste("Target room number:", input$t_rmnoc)
  }) 
  
  #c2c
  return_rmtype_c2c <- return_rmtype_a2b <- reactive({
    data$rmtype[data$rmno == input$t_rmnoc][1] #return first output only 
  })
  output$return_rmtype_c2c <- renderText({
    paste("Target room type:", return_rmtype_c2c())
  })
  
  #c2d
  return_c2d_aufpreis <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == input$rmtype & diff_long$target_roomcat == data$rmtype[data$rmno == input$t_rmnoc][1]][1]
  })
  output$return_c2d_aufpreis <- renderText({
    paste("Price difference:", return_c2d_aufpreis())
  })
  
  #c2e
  output$tar_rmtypec <- renderText({ 
    paste("Target room type:", input$t_rmtypec)
  })
  
  #c2f
  return_c2f_aufpreis <- reactive({
    diff_long$aufpreis[diff_long$orig_roomcat == input$rmtype & diff_long$target_roomcat == input$t_rmtypec][1]
  })
  output$return_c2f_aufpreis <- renderText({
    paste("Price difference:", return_c2f_aufpreis())
  })
  
}

shinyApp(ui = ui, server = server)