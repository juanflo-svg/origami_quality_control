
#============================
# EJECUTAR 1 VEZ LO SGTE:
#============================
install.packages(c("shiny", "shinydashboard", "nortest", "readxl", "qcc"))

library(shiny)
library(shinydashboard)
library(nortest)
library(readxl)
library(qcc)

# =========================
# LEER EXCEL
# =========================

ruta <- "DatosRecogidos.xlsx"

med_diam <- as.numeric(read_excel(ruta, sheet = "DisenoDiametros")[[2]])
val_diam <- med_diam

med_sol <- as.numeric(read_excel(ruta, sheet = "DisenoSolapas")[[2]])
val_sol <- med_sol

med_conf <- as.numeric(read_excel(ruta, sheet = "DisenoConformidad")[[2]])
val_conf <- med_conf

datos_ajuste <- read_excel(ruta, sheet = "Ajuste")
aj_sol <- as.data.frame(lapply(datos_ajuste[, 2:4], as.numeric))

# =========================
# UI
# =========================
ui <- dashboardPage(
  dashboardHeader(title = "Cartas de Control"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Diseño", tabName = "diseno"),
      menuItem("Ajuste", tabName = "ajuste")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "diseno",
              tabsetPanel(type = "tabs",
                          
                          # -------- DIÁMETROS --------
                          tabPanel("Diámetros",
                                   
                                   h4("Estadísticas descriptivas"),
                                   fluidRow(
                                     box(width = 12, status = "primary", solidHeader = TRUE,
                                         title = "Resumen",
                                         verbatimTextOutput("summary_diam"))
                                   ),
                                   
                                   h4("Gráficas descriptivas"),
                                   fluidRow(
                                     box(width = 6, title = "Histograma", status = "primary", solidHeader = TRUE,
                                         plotOutput("hist_diam")),
                                     box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE,
                                         plotOutput("box_diam"))
                                   ),
                                   fluidRow(
                                     box(width = 12, title = "Gráfico de mediciones", status = "primary", solidHeader = TRUE,
                                         plotOutput("plot_diam"))
                                   ),
                                   
                                   h4("Análisis de normalidad"),
                                   fluidRow(
                                     box(width = 6, title = "QQ Plot", status = "primary", solidHeader = TRUE,
                                         plotOutput("qq_diam")),
                                     box(width = 6, title = "Pruebas de normalidad", status = "primary", solidHeader = TRUE,
                                         tableOutput("tabla_diam"))
                                   ),
                                   
                                   h4("Cartas de control"),
                                   fluidRow(
                                     box(width = 6, title = "Carta I", status = "primary", solidHeader = TRUE,
                                         plotOutput("r_diam")),
                                     box(width = 6, title = "Carta X̄", status = "primary", solidHeader = TRUE,
                                         plotOutput("x_diam"))
                                   )
                          ),
                          
                          # -------- SOLAPAS --------
                          tabPanel("Solapas",
                                   
                                   h4("Estadísticas descriptivas"),
                                   fluidRow(
                                     box(width = 12, status = "primary", solidHeader = TRUE,
                                         title = "Resumen",
                                         verbatimTextOutput("summary_sol"))
                                   ),
                                   
                                   h4("Gráficas descriptivas"),
                                   fluidRow(
                                     box(width = 6, title = "Histograma", status = "primary", solidHeader = TRUE,
                                         plotOutput("hist_sol")),
                                     box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE,
                                         plotOutput("box_sol"))
                                   ),
                                   fluidRow(
                                     box(width = 12, title = "Gráfico de mediciones", status = "primary", solidHeader = TRUE,
                                         plotOutput("plot_sol"))
                                   ),
                                   
                                   h4("Análisis de normalidad"),
                                   fluidRow(
                                     box(width = 6, title = "QQ Plot", status = "primary", solidHeader = TRUE,
                                         plotOutput("qq_sol")),
                                     box(width = 6, title = "Pruebas de normalidad", status = "primary", solidHeader = TRUE,
                                         tableOutput("tabla_sol"))
                                   ),
                                   
                                   h4("Cartas de control"),
                                   fluidRow(
                                     box(width = 6, title = "Carta I", status = "primary", solidHeader = TRUE,
                                         plotOutput("r_sol")),
                                     box(width = 6, title = "Carta X̄", status = "primary", solidHeader = TRUE,
                                         plotOutput("x_sol"))
                                   )
                          ),
                          
                          # -------- CONFORMIDAD --------
                          tabPanel("Conformidad",
                                   
                                   h4("Estadísticas descriptivas"),
                                   fluidRow(
                                     box(width = 12, status = "primary", solidHeader = TRUE,
                                         title = "Resumen",
                                         verbatimTextOutput("summary_conf"))
                                   ),
                                   
                                   h4("Gráficas descriptivas"),
                                   fluidRow(
                                     box(width = 6, title = "Histograma", status = "primary", solidHeader = TRUE,
                                         plotOutput("hist_conf")),
                                     box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE,
                                         plotOutput("box_conf"))
                                   ),
                                   fluidRow(
                                     box(width = 12, title = "Gráfico de mediciones", status = "primary", solidHeader = TRUE,
                                         plotOutput("plot_conf"))
                                   ),
                                   
                                   h4("Análisis de normalidad"),
                                   fluidRow(
                                     box(width = 6, title = "QQ Plot", status = "primary", solidHeader = TRUE,
                                         plotOutput("qq_conf")),
                                     box(width = 6, title = "Pruebas de normalidad", status = "primary", solidHeader = TRUE,
                                         tableOutput("tabla_conf"))
                                   ),
                                   
                                   h4("Cartas de control"),
                                   fluidRow(
                                     box(width = 6, title = "Carta I", status = "primary", solidHeader = TRUE,
                                         plotOutput("r_conf")),
                                     box(width = 6, title = "Carta X̄", status = "primary", solidHeader = TRUE,
                                         plotOutput("x_conf"))
                                   )
                          )
              )
      ),
      
      # -------- AJUSTE --------
      tabItem(tabName = "ajuste",
              selectInput("carta", "Seleccionar:",
                          choices = c("Solapas")),
              
              h4("Estadísticas descriptivas"),
              fluidRow(
                box(width = 12, title = "Resumen", status = "primary", solidHeader = TRUE,
                    verbatimTextOutput("summary_aj"))
              ),
              
              h4("Gráficas descriptivas"),
              fluidRow(
                box(width = 6, title = "Histograma", status = "primary", solidHeader = TRUE,
                    plotOutput("hist_aj")),
                box(width = 6, title = "Boxplot", status = "primary", solidHeader = TRUE,
                    plotOutput("box_aj"))
              ),
              fluidRow(
                box(width = 12, title = "Gráfico de mediciones", status = "primary", solidHeader = TRUE,
                    plotOutput("plot_aj"))
              ),
              
              h4("Análisis de normalidad"),
              fluidRow(
                box(width = 6, title = "QQ Plot", status = "primary", solidHeader = TRUE,
                    plotOutput("qq_aj")),
                box(width = 6, title = "Pruebas de normalidad", status = "primary", solidHeader = TRUE,
                    tableOutput("tabla_aj"))
              ),
              
              h4("Cartas de control"),
              fluidRow(
                box(width = 6, title = "Carta R", status = "primary", solidHeader = TRUE,
                    plotOutput("ajuste_r")),
                box(width = 6, title = "Carta X̄", status = "primary", solidHeader = TRUE,
                    plotOutput("ajuste_x"))
              )
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  tabla_norm <- function(vals) {
    p1 <- shapiro.test(vals)$p.value
    p2 <- lillie.test(vals)$p.value
    data.frame(
      Prueba    = c("Shapiro-Wilk", "Lilliefors"),
      p_valor   = round(c(p1, p2), 8),
      Resultado = ifelse(c(p1, p2) > 0.05, "Normal", "No normal")
    )
  }
  
  # -------- DIÁMETROS --------
  output$summary_diam <- renderPrint({
    cat("Media:   ", mean(val_diam), "\n")
    cat("Mediana: ", median(val_diam), "\n")
    cat("SD:      ", sd(val_diam), "\n")
    cat("Min:     ", min(val_diam), "\n")
    cat("Max:     ", max(val_diam), "\n")
  })
  output$hist_diam  <- renderPlot({ hist(val_diam, col = "purple", main = "Histograma", xlab = "Valores", ylab = "Frecuencia") })
  output$box_diam   <- renderPlot({ boxplot(val_diam, main = "Boxplot", ylab = "Valores") })
  output$plot_diam  <- renderPlot({ plot(val_diam, type = "b", main = "Gráfico de mediciones", xlab = "Observación", ylab = "Valor") })
  output$qq_diam    <- renderPlot({ qqnorm(val_diam); qqline(val_diam, col = "red") })
  output$tabla_diam <- renderTable({ tabla_norm(val_diam) })
  output$r_diam     <- renderPlot({ qcc(val_diam, type = "xbar.one") })
  output$x_diam     <- renderPlot({ plot(val_diam, type = "b", main = "Mediciones individuales", xlab = "Subgrupo", ylab = "Valor"); abline(h = mean(val_diam), col = "green", lwd = 2) })
  
  # -------- SOLAPAS --------
  output$summary_sol <- renderPrint({
    cat("Media:   ", mean(val_sol), "\n")
    cat("Mediana: ", median(val_sol), "\n")
    cat("SD:      ", sd(val_sol), "\n")
    cat("Min:     ", min(val_sol), "\n")
    cat("Max:     ", max(val_sol), "\n")
  })
  output$hist_sol  <- renderPlot({ hist(val_sol, col = "blue", main = "Histograma", xlab = "Valores", ylab = "Frecuencia") })
  output$box_sol   <- renderPlot({ boxplot(val_sol, main = "Boxplot", ylab = "Valores") })
  output$plot_sol  <- renderPlot({ plot(val_sol, type = "b", main = "Gráfico de mediciones", xlab = "Observación", ylab = "Valor") })
  output$qq_sol    <- renderPlot({ qqnorm(val_sol); qqline(val_sol, col = "red") })
  output$tabla_sol <- renderTable({ tabla_norm(val_sol) })
  output$r_sol     <- renderPlot({ qcc(val_sol, type = "xbar.one") })
  output$x_sol     <- renderPlot({ plot(val_sol, type = "b", main = "Mediciones individuales", xlab = "Subgrupo", ylab = "Valor"); abline(h = mean(val_sol), col = "green", lwd = 2) })
  
  # -------- CONFORMIDAD --------
  output$summary_conf <- renderPrint({
    cat("Media:   ", mean(val_conf), "\n")
    cat("Mediana: ", median(val_conf), "\n")
    cat("SD:      ", sd(val_conf), "\n")
    cat("Min:     ", min(val_conf), "\n")
    cat("Max:     ", max(val_conf), "\n")
  })
  output$hist_conf  <- renderPlot({ hist(val_conf, col = "green", main = "Histograma", xlab = "Valores", ylab = "Frecuencia") })
  output$box_conf   <- renderPlot({ boxplot(val_conf, main = "Boxplot", ylab = "Valores") })
  output$plot_conf  <- renderPlot({ plot(val_conf, type = "b", main = "Gráfico de mediciones", xlab = "Observación", ylab = "Valor") })
  output$qq_conf    <- renderPlot({ qqnorm(val_conf); qqline(val_conf, col = "red") })
  output$tabla_conf <- renderTable({ tabla_norm(val_conf) })
  output$r_conf     <- renderPlot({ qcc(val_conf, type = "xbar.one") })
  output$x_conf     <- renderPlot({ plot(val_conf, type = "b", main = "Mediciones individuales", xlab = "Subgrupo", ylab = "Valor"); abline(h = mean(val_conf), col = "green", lwd = 2) })
  
  # -------- AJUSTE --------
  val_aj <- c(as.matrix(aj_sol))
  
  output$summary_aj <- renderPrint({
    cat("Media:   ", mean(val_aj), "\n")
    cat("Mediana: ", median(val_aj), "\n")
    cat("SD:      ", sd(val_aj), "\n")
    cat("Min:     ", min(val_aj), "\n")
    cat("Max:     ", max(val_aj), "\n")
  })
  output$hist_aj  <- renderPlot({ hist(val_aj, col = "blue", main = "Histograma", xlab = "Valores", ylab = "Frecuencia") })
  output$box_aj   <- renderPlot({ boxplot(val_aj, main = "Boxplot", ylab = "Valores") })
  output$plot_aj  <- renderPlot({ plot(val_aj, type = "b", main = "Gráfico de mediciones", xlab = "Observación", ylab = "Valor") })
  output$qq_aj    <- renderPlot({ qqnorm(val_aj); qqline(val_aj, col = "red") })
  output$tabla_aj <- renderTable({ tabla_norm(val_aj) })
  output$ajuste_r <- renderPlot({
    rangos <- apply(aj_sol, 1, function(x) max(x) - min(x))
    qcc(rangos, type = "R", sizes = ncol(aj_sol), title = "Ajuste R — Solapas")
  })
  output$ajuste_x <- renderPlot({
    qcc(aj_sol, type = "xbar", title = "Ajuste X̄ — Solapas")
  })
}

shinyApp(ui, server)














