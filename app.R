paquetes <- c("shiny", "readxl", "ggplot2")
faltan_paquetes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]
if (length(faltan_paquetes) > 0) {
  stop(
    "Instalá los paquetes necesarios antes de iniciar la app: ",
    paste(faltan_paquetes, collapse = ", ")
  )
}

library(shiny)
library(readxl)
library(ggplot2)

archivos_posibles <- c("Rendimiento_decada.xlsx", "Rendimientos_decada.xlsx")
archivo <- archivos_posibles[file.exists(archivos_posibles)][1]
if (is.na(archivo)) {
  stop(
    "No se encontró Rendimiento_decada.xlsx ni Rendimientos_decada.xlsx ",
    "en la carpeta de la aplicación."
  )
}

datos <- read_excel(
  archivo,
  sheet = "rendimiento",
  na = c("", "NA", "SD", "sd", "Sd")
)

columnas_esperadas <- c(
  "Cultivo", "Campaña", "Sup. Sembrada", "Sup. Cosechada"
)
faltan_columnas <- setdiff(columnas_esperadas, names(datos))
if (length(faltan_columnas) > 0) {
  stop(
    "Faltan columnas necesarias en el Excel: ",
    paste(faltan_columnas, collapse = ", ")
  )
}

convertir_a_numero <- function(x, variable) {
  valores <- trimws(as.character(x))
  valores[!is.na(valores) & toupper(valores) %in% c("", "NA", "SD")] <-
    NA_character_

  es_numero <- grepl(
    "^[+-]?[0-9]+([.][0-9]*)?([eE][+-]?[0-9]+)?$",
    valores,
    perl = TRUE
  )
  invalidos <- !is.na(valores) & nzchar(valores) & !es_numero
  if (any(invalidos)) {
    stop(
      "Hay valores no numéricos inesperados en '", variable, "': ",
      paste(unique(valores[invalidos]), collapse = ", ")
    )
  }

  as.numeric(valores)
}

datos$`Sup. Sembrada` <- convertir_a_numero(
  datos$`Sup. Sembrada`,
  "Sup. Sembrada"
)
datos$`Sup. Cosechada` <- convertir_a_numero(
  datos$`Sup. Cosechada`,
  "Sup. Cosechada"
)

datos$Cultivo <- trimws(as.character(datos$Cultivo))
datos[["Campaña"]] <- trimws(as.character(datos[["Campaña"]]))
datos$Cultivo[!is.na(datos$Cultivo) & datos$Cultivo == ""] <- NA_character_
datos[["Campaña"]][
  !is.na(datos[["Campaña"]]) & datos[["Campaña"]] == ""
] <- NA_character_

cultivos <- sort(unique(stats::na.omit(datos$Cultivo)))
campanias <- unique(stats::na.omit(datos[["Campaña"]]))
if (length(cultivos) == 0 || length(campanias) == 0) {
  stop("El Excel no contiene cultivos o campañas válidas.")
}

datos$Campania <- factor(datos[["Campaña"]], levels = campanias)

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Cultivo",
        choices = cultivos,
        selected = cultivos[1]
      ),
      helpText(
        "Los valores SD y las celdas vacías se consideran faltantes. ",
        "Si falta algún dato de una campaña, no se calcula el total ",
        "de esa superficie para evitar mostrar una suma parcial."
      )
    ),
    mainPanel(
      textOutput("resumen"),
      plotOutput("grafico", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  datos_filtrados <- reactive({
    datos[!is.na(datos$Cultivo) & datos$Cultivo == input$cultivo, ]
  })

  totales_por_campania <- reactive({
    datos_actuales <- datos_filtrados()

    total_completo <- function(variable, campania) {
      valores <- datos_actuales[[variable]][
        !is.na(datos_actuales$Campania) &
          as.character(datos_actuales$Campania) == campania
      ]

      if (length(valores) == 0 || anyNA(valores)) {
        return(NA_real_)
      }

      sum(valores)
    }

    data.frame(
      Campania = factor(campanias, levels = campanias),
      Sembrada = vapply(
        campanias,
        function(campania) total_completo("Sup. Sembrada", campania),
        numeric(1)
      ),
      Cosechada = vapply(
        campanias,
        function(campania) total_completo("Sup. Cosechada", campania),
        numeric(1)
      )
    )
  })

  output$resumen <- renderText({
    datos_actuales <- datos_filtrados()
    totales <- totales_por_campania()

    paste0(
      "Cultivo: ", input$cultivo,
      " | Registros: ", nrow(datos_actuales),
      " | Valores faltantes: sembrada: ",
      sum(is.na(datos_actuales$`Sup. Sembrada`)),
      ", cosechada: ", sum(is.na(datos_actuales$`Sup. Cosechada`)),
      " | Registros sin campaña: ",
      sum(is.na(datos_actuales$Campania)),
      " | Campañas sin total completo: sembrada: ",
      sum(is.na(totales$Sembrada)),
      ", cosechada: ", sum(is.na(totales$Cosechada))
    )
  })

  output$grafico <- renderPlot({
    totales <- totales_por_campania()
    validate(
      need(
        any(!is.na(totales$Sembrada) | !is.na(totales$Cosechada)),
        "No hay superficies completas para mostrar con este filtro."
      )
    )

    serie <- rbind(
      data.frame(
        Campania = totales$Campania,
        Superficie = totales$Sembrada,
        Variable = "Sup. Sembrada"
      ),
      data.frame(
        Campania = totales$Campania,
        Superficie = totales$Cosechada,
        Variable = "Sup. Cosechada"
      )
    )
    serie$Variable <- factor(
      serie$Variable,
      levels = c("Sup. Sembrada", "Sup. Cosechada")
    )
    serie <- serie[order(serie$Variable, serie$Campania), ]
    serie$segmento <- ave(
      as.integer(is.na(serie$Superficie)),
      serie$Variable,
      FUN = cumsum
    )

    ggplot(
      serie,
      aes(
        x = Campania,
        y = Superficie,
        color = Variable,
        group = interaction(Variable, segmento)
      )
    ) +
      geom_line(na.rm = TRUE, size = 1) +
      geom_point(na.rm = TRUE, size = 2) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(
        labels = function(x) {
          format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
        }
      ) +
      labs(
        x = "Campaña",
        y = "Superficie",
        color = "Variable",
        title = paste(
          "Superficie sembrada y cosechada por campaña -",
          input$cultivo
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui = ui, server = server)
