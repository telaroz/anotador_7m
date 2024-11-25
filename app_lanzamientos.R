library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyWidgets)
library(data.table)
library(shinyjs)

draw_goal <- function(color = "#b90d16") {
  goal_width <- 0.08
  contour_size <- 1.1
  coloured_dimensions <- data.frame(xmin = c(seq(from = -1.5, 
                                                 to = 1.5 - 0.2, by = 0.4), rep(-1.5 - goal_width, 5) + 
                                               0.004, rep(1.5, 5) + 0.004, c(-1.5 - goal_width + 0.004, 
                                                                             1.5)), xmax = c(seq(from = -1.5 + 0.2, to = 1.5, by = 0.4), 
                                                                                             rep(-1.5, 5) - 0.002, rep(1.5 + goal_width, 5) - 0.002, 
                                                                                             c(-1.5, 1.5 + goal_width - 0.002)), ymin = c(rep(1, 8) + 
                                                                                                                                            0.002, seq(from = -1 + 0.2, to = 1, by = 0.4), seq(from = -1 + 
                                                                                                                                                                                                 0.2, to = 1, by = 0.4), c(1, 1)), ymax = c(rep(1 + goal_width, 
                                                                                                                                                                                                                                                8) - 0.004, seq(from = -1 + 0.2 + 0.2, to = 1, by = 0.4), 
                                                                                                                                                                                                                                            seq(from = -1 + 0.2 + 0.2, to = 1, by = 0.4), c(1 + goal_width - 
                                                                                                                                                                                                                                                                                              0.004, 1 + goal_width - 0.004)))
  ggplot2::ggplot() + ggplot2::geom_rect(data = coloured_dimensions, 
                                         ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                                         fill = color) + ggplot2::geom_line(ggplot2::aes(x = c(-1.5, 
                                                                                               -1.5, 1.5, 1.5), y = c(-1, 1, 1, -1)), linewidth = contour_size) + 
    ggplot2::geom_line(aes(x = c(-1.5 - goal_width, -1.5 - 
                                   goal_width, 1.5 + goal_width, 1.5 + goal_width), 
                           y = c(-1, 1 + goal_width, 1 + goal_width, -1)), linewidth = contour_size) + 
    ggplot2::geom_segment(ggplot2::aes(x = -2, xend = 2, 
                                       y = -1, yend = -1), size = 2.5) + ggplot2::theme_void()
} # Taken from the handbaloner package https://github.com/telaroz/handbaloner

ui <- dashboardPage(
  
  dashboardHeader(title = "Anotador de Lanzamientos de 7m"),
  
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Equipo 1", tabName = "dashboard", icon = icon("people-carry")),
                     menuItem('Escogencia de Jugadores: Equipo 1', tabName = 'escogencia_M'),
                     menuItem("Equipo 2", tabName = "widgets", icon = icon("child")),
                     menuItem('Escogencia de Jugadores: Equipo 2', tabName = 'escogencia_J')
                   )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = 'escogencia_M',
              column(width = 3,
                     checkboxGroupButtons(inputId = "escogencia_mayor", "Escogencia Jugadores Equipo 1",
                                          c(paste0("Jugador ", 1:20), 
                                            c('Invitado 1', 'Invitado 2')), 
                                          selected = sort(paste0("Jugador ", 1:13)),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      lib = "glyphicon")))),
              
              
              column(width = 3,
                     checkboxGroupButtons(inputId = "escogencia_mayor_porteros", "Escogencia Porteros Equipo 1",
                                          c(c("Portero 1", "Portero 2", "Portero 3"), sort(c("Portero 4", "Portero 5", "Portero 6", "Portero 7")), 'Invitado1'), 
                                          selected = sort(c("Portero 1", "Portero 2", "Portero 3")),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      
                                            ))))),
      tabItem(tabName = 'escogencia_J',
              
              column(width = 3, checkboxGroupButtons(inputId = "escogencia_juvenil", "Escogencia Jugadores Equipo 2",
                                                     c(paste0("Jugador ", 1:20),
                                                       paste0("Jugador ", 1:20),
                                                       c('Invitado 1', 'Invitado 2')), 
                                                     selected = sort(paste0("Jugador ", 1:13)),
                                                     status = "primary", 
                                                     direction = 'vertical',
                                                     checkIcon = list(
                                                       yes = icon("ok", 
                                                                  lib = "glyphicon"),
                                                       no = icon("remove",
                                                                 lib = "glyphicon")))),
              
              column(width = 3,
                     checkboxGroupButtons(inputId = "escogencia_juvenil_porteros", "Escogencia Jugadores Equipo 2",
                                          c(sort(paste0("Portero ", 1:4)), 'Invitado1'), 
                                          selected = sort(paste0("Portero ", 1:4)),
                                          status = "primary", 
                                          direction = 'vertical',
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"),
                                            no = icon("remove",
                                                      lib = "glyphicon"))))
              
      ),
      # First tab content
      tabItem(tabName = "dashboard",
              
              
              fluidRow(
                shinyjs::useShinyjs(),
                column(width = 2, radioGroupButtons(
                  inputId = "tirador",
                  label = "Tirador ",
                  choices = paste0("Jugador ", 1:20),
                  # size = 'lg',
                  direction = 'vertical',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                )),
                column(width = 2, radioGroupButtons(
                  inputId = "portero",
                  label = "Portero ",
                  choices = c("Portero 1", "Portero 2", 'Portero 3'),
                  # size = 'lg',
                  direction = 'horizontal',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                ),radioGroupButtons(inputId = "color",label = "Gol", choices =  c("Gol", "No Gol"), size = 'lg'),
                sliderTextInput(
                  inputId = "fatiga",
                  label = "Cansancio", 
                  choices = seq(0L, 10L, 1L),
                  grid = TRUE
                ),
                checkboxGroupButtons(
                  inputId = "Adicionales", 
                  label = "Tipo de tiro adicional",
                  size = 'lg',
                  choices = c("Finta", 
                              "Pique", "Bañito", "Rosca"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ), 
                materialSwitch(
                  inputId = "material_finta",
                  label = "Tiro al mismo lugar de la finta", 
                  value = FALSE,
                  status = "primary"
                ),
                
                sliderInput("posicion_portero", "Salida del portero en metros",
                            min = 0, max = 4,
                            value = 0, step = 0.25),
                
                # sliderTextInput(
                #   inputId = "posicion_portero",
                #   label = "Salida del portero en metros", 
                #   choices = seq(0, 4, 0.25),
                #   grid = TRUE
                # ),
                
                checkboxGroupButtons(
                  inputId = "infraccion", 
                  label = "Infraccion",
                  size = 'lg', 
                  choices = c("Desliza/Maja", 
                              "Levanta Pie", 
                              "Tiempo",
                              "Se pasa"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ),   
                textInput("comentarios", "Comentario", "")), 
                column(width = 6,
                       h4("Click en el gráfico para agregar el tiro"),
                       actionButton("rem_point", "Quite el último dato"),
                       plotOutput("plot1", click = "plot_click")),
                column(width = 6,
                       h4("Tiros realizados"),
                       tableOutput("table")),    downloadButton("download", "Descargar Sesión", size = 'lg'))
              
              
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              fluidRow(
                shinyjs::useShinyjs(),
                column(width = 2, radioGroupButtons(
                  inputId = "tirador2",
                  label = "Tirador ",
                  choices = paste0("Jugador ", 1:20),
                  # size = 'lg',
                  direction = 'vertical',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                )),
                column(width = 2, radioGroupButtons(
                  inputId = "portero2",
                  label = "Portero ",
                  choices = c("Portero 1", "Portero 2", "Portero 3", "Portero 4"),
                  # size = 'lg',
                  direction = 'horizontal',
                  individual = FALSE,
                  status = 'primary', 
                  checkIcon = list(
                    yes = icon("ok", 
                               lib = "glyphicon"))
                ),radioGroupButtons(inputId = "color2",label = "Gol", choices =  c("Gol", "No Gol"), size = 'lg'),
                sliderTextInput(
                  inputId = "fatiga2",
                  label = "Cansancio", 
                  choices = seq(0L, 10L, 1L),
                  grid = TRUE
                ),
                checkboxGroupButtons(
                  inputId = "Adicionales2", 
                  label = "Tipo de tiro adicional",
                  size = 'lg',
                  choices = c("Finta", 
                              "Pique", "Bañito", "Rosca"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ), 
                materialSwitch(
                  inputId = "material_finta2",
                  label = "Tiro al mismo lugar de la finta", 
                  value = FALSE,
                  status = "primary"
                ),
                
                sliderInput("posicion_portero2", "Salida del portero en metros",
                            min = 0, max = 4,
                            value = 0, step = 0.25),
                
                # sliderTextInput(
                #   inputId = "posicion_portero",
                #   label = "Salida del portero en metros", 
                #   choices = seq(0, 4, 0.25),
                #   grid = TRUE
                # ),
                
                checkboxGroupButtons(
                  inputId = "infraccion2", 
                  label = "Infraccion",
                  size = 'lg', 
                  choices = c("Desliza/Maja", 
                              "Levanta Pie", 
                              "Tiempo",
                              "Se pasa"),
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o", 
                                style = "color: steelblue"))
                ),   
                textInput("comentarios2", "Comentario", "")), 
                column(width = 6,
                       h4("Click en el gráfico para agregar el tiro"),
                       actionButton("rem_point2", "Quite el último dato"),
                       plotOutput("plot2", click = "plot_click2")),
                column(width = 6,
                       h4("Tiros realizados"),
                       tableOutput("table2")),    downloadButton("download2", "Descargar Sesión", size = 'lg'))
      )
    )
  )
)

server <- function(input, output, session) {
  
  jugadores_mayor <- paste0("Jugador ", 1:20)
  
  jugadores_juvenil <- paste0("Jugador ", 1:20)
  
  jugadores <- unique(c(jugadores_mayor, jugadores_juvenil, 'Invitado1', 'Invitado2', 'Invitado3'))
  
  
  porteros_mayor <- paste0("Portero ", 1:4)
  porteros_juvenil <- paste0("Portero ", 1:4)
  porteros <- unique(c(porteros_mayor, porteros_juvenil, 'Invitado1'))
  
  
  observeEvent(input$escogencia_mayor, {
    jugadores_escogidos <- jugadores[jugadores %in% input$escogencia_mayor]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "tirador",
                            choices = jugadores_escogidos, selected = jugadores_mayor, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$escogencia_juvenil, {
    jugadores_escogidos <- jugadores[jugadores %in% input$escogencia_juvenil]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "tirador2",
                            choices = jugadores_escogidos, selected = jugadores_juvenil, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$escogencia_juvenil_porteros, {
    porteros_escogidos <- porteros[porteros %in% input$escogencia_juvenil_porteros]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "portero2",
                            choices = porteros_escogidos, selected = porteros_juvenil, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$escogencia_mayor_porteros, {
    porteros_escogidos <- porteros[porteros %in% input$escogencia_mayor_porteros]
    
    # Method 1
    updateRadioGroupButtons(session = session, inputId = "portero",
                            choices = porteros_escogidos, selected = porteros_mayor, status = 'primary', checkIcon = list(
                              yes = icon("ok", 
                                         lib = "glyphicon")))
    
    
  }, ignoreInit = TRUE)
  
  
  
  
  
  
  values <- reactiveValues()
  values$DT2 <- data.table(x = numeric(),
                           y = numeric(),
                           gol = character(),
                           tirador = character(),
                           portero = character(),
                           hora = character(),
                           adicional = character(),
                           infraccion = character(),
                           posicion_portero = numeric(),
                           tiro_igual_finta = logical(),
                           fatiga = integer(),
                           comentario = character())
  
  # Create a plot
  output$plot2 = renderPlot({
    draw_goal() + 
      geom_point(data = values$DT2, aes(x = x, y = y),
                 color = data.table::fifelse(values$DT2$gol == 'Gol', 'Green', 'Red'), size = 6) +
      lims(x = c(-3, 3), y = c(-1, 2)) +
      theme(legend.position = "none") +
      # include so that colors don't change as more colors are chosen
      scale_color_discrete(drop = FALSE) +
    #  theme_void() +
      coord_fixed()
  }, width = "auto", height = "auto")
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click2, {
    add_row <- data.table(x = input$plot_click2$x,
                          y = input$plot_click2$y,
                          gol = ifelse(input$color2 == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador2,
                          portero = input$portero2,
                          hora = as.character(Sys.time()),
                          adicional = paste0(sort(input$Adicionales2), collapse = '_'),
                          infraccion = paste0(sort(input$infraccion2), collapse = '_'),
                          posicion_portero = paste0(sort(input$posicion_portero2), collapse = '_'),
                          tiro_igual_finta = input$material_finta2,
                          fatiga = input$fatiga2,
                          comentario = input$comentarios2)
    values$DT2 <- rbind(values$DT2, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point2, {
    rem_row <- values$DT[-nrow(values$DT2), ]
    values$DT2 <- rem_row
  })
  
  # Render the table
  output$table2 <- renderTable({
    data <- data.table::setDT(tail(values$DT2, 10))
    a_mostrar <- nrow(data[!is.na(x)])
    data[min(10,a_mostrar):1]
  })
  
  # Add a download button
  output$download2 <- downloadHandler(
    filename = function() {
      paste0('penales_juvenil', Sys.Date(),'.csv')
    },
    content = function(file) {
      data.table::fwrite(values$DT2, file)
    }
  )
  
  observeEvent(input$plot_click2, {
    shinyjs::reset(id = 'comentarios2')
    shinyjs::reset(id = 'posicion_portero2')
  })
  
  
  
  
  
  
  
  
  # Ahora el de la mayor
  
  values$DT <- data.table(x = numeric(),
                          y = numeric(),
                          gol = character(),
                          tirador = character(),
                          portero = character(),
                          hora = character(),
                          adicional = character(),
                          infraccion = character(),
                          posicion_portero = numeric(),
                          tiro_igual_finta = logical(),
                          fatiga = integer(),
                          comentario = character())
  
  # Create a plot
  output$plot1 = renderPlot({
    draw_goal() +  
      geom_point(data = values$DT, aes(x = x, y = y), 
                 color = data.table::fifelse(values$DT$gol == 'Gol', 'Green', 'Red'), 
                 size = 6) +
      lims(x = c(-3, 3), y = c(-1, 2)) +
      theme(legend.position = "none") +
      # include so that colors don't change as more colors are chosen
      scale_color_discrete(drop = FALSE) +
      # theme_void() +
      coord_fixed()
  }, width = "auto", height = "auto")
  
  # Add a new row, as a reaction to a click
  observeEvent(input$plot_click, {
    add_row <- data.table(x = input$plot_click$x,
                          y = input$plot_click$y,
                          gol = ifelse(input$color == 'Gol', 'Gol', 'NO Gol'),
                          tirador = input$tirador,
                          portero = input$portero,
                          hora = as.character(Sys.time()),
                          adicional = paste0(sort(input$Adicionales), collapse = '_'),
                          infraccion = paste0(sort(input$infraccion), collapse = '_'),
                          posicion_portero = paste0(sort(input$posicion_portero), collapse = '_'),
                          tiro_igual_finta = input$material_finta,
                          fatiga = input$fatiga,
                          comentario = input$comentarios)
    values$DT <- rbind(values$DT, add_row)
  })
  
  # Action button in case a row should be removed.
  observeEvent(input$rem_point, {
    rem_row <- values$DT[-nrow(values$DT), ]
    values$DT <- rem_row
  })
  
  # Render the table
  output$table <- renderTable({
    data <- data.table::setDT(tail(values$DT, 10))
    a_mostrar <- nrow(data[!is.na(x)])
    data[min(10,a_mostrar):1]
  })
  
  # Add a download button
  output$download <- downloadHandler(
    filename = function() {
      paste0('penales_mayor', Sys.Date(),'.csv')
    },
    content = function(file) {
      data.table::fwrite(values$DT, file)
    }
  )
  
  observeEvent(input$plot_click, {
    shinyjs::reset(id = 'comentarios')
    shinyjs::reset(id = 'posicion_portero')
  })
  
}



shinyApp(ui, server)