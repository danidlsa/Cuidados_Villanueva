
library(shiny)
library(leaflet)
library(sf)
library(DT)
library(dplyr)
library(plotly)   




sf_children           <- readRDS("rds/sf_children.rds")
sf_elderly            <- readRDS("rds/sf_elderly.rds")
sf_disability         <- readRDS("rds/sf_disability.rds")
sf_oferta_children    <- readRDS("rds/sf_oferta_children.rds")
sf_oferta_elderly     <- readRDS("rds/sf_oferta_elderly.rds")
sf_oferta_disability  <- readRDS("rds/sf_oferta_disability.rds")
sf_desiertos_children <- readRDS("rds/sf_desiertos_children.rds")
sf_desiertos_elderly  <- readRDS("rds/sf_desiertos_elderly.rds")
sf_desiertos_disability <- readRDS("rds/sf_desiertos_disability.rds")




# UI
ui <- navbarPage(
  title = "Cuidados en Villanueva, Honduras",
  id = "nav",
  # Pestaña Cover
  
  tabPanel(
    "Intro",
    fluidPage(
      tags$head(
        tags$style(HTML("
        .cover-page ul, .cover-page ol {
          list-style-position: inside;
          text-align: center;
          padding-left: 0;
        }
      "))
      ),
      tags$div(class = "cover-page", style = "text-align:center; padding:30px;",
               tags$img(src = "logopnud.png", height = "150px"),
               h1("Georreferenciación de Cuidados en Villanueva, Honduras", style = "color:#0072B2;"),
               h4("Una herramienta del PNUD para mapear oferta, demanda y accesibilidad a servicios de cuidado y apoyo"),
               tags$hr(),
               p("Este explorador permite visualizar datos georreferenciados sobre:",
                 tags$ul(
                   tags$li(HTML("<b>Oferta de servicios de cuidados y apoyo</b>: centros gubernamentales, privados y comunitarios.")),
                   tags$li(HTML("<b>Demanda potencial de cuidados</b>: población que requiere o puede requerir cuidados o apoyos.")),
                   tags$li(HTML("<b>Brechas de accesibilidad</b> y <b>desiertos de cuidado</b>: zonas críticas donde la demanda no puede alcanzar la oferta.")),
                   tags$li(HTML("Resultados del <b>trabajo de campo</b> piloto de caracterización de centros de cuidado.")),
                   tags$li(HTML("<b>Datos climáticos en tiempo real</b>: capas de precipitación y viento que pueden cruzarse con los mapas de cuidados o visualizarse por separado para análisis de riesgos y resiliencia."))
                 )
               ),
               h4("¿Cómo usar la herramienta?"),
               tags$ol(
                 tags$li("Seleccione la pestaña correspondiente a la población de interés."),
                 tags$li("Active o desactive capas de Oferta, Demanda, Desiertos de cuidado y datos climáticos."),
                 tags$li("Use la tabla para explorar detalles de cada centro."),
                 tags$li("Descargue datos en formato CSV si lo necesita."),
                 tags$li("Explore los resultados del trabajo de campo en la pestaña correspondiente.")
               ),
               br(),
               tags$p(style="font-size:14px; color:#555;",
                      HTML("<i>Nota:</i> Las capas climáticas provienen de OpenWeather Maps API y Windy.com, por lo que no deben tratarse como datos oficiales.")
               ),
               br(),
               actionButton("goToMap", "Conoce la metodología", class = "btn-primary")
      )
    )
  ),
  




tabPanel(
  "¿Qué mapeamos?",
  fluidPage(
    tags$div(
      class = "que-mapeamos-page",
      style = "padding:30px; max-width:900px; margin:auto; font-size:16px; line-height:1.6; text-align:center;",
      
      # Logo centrado
      tags$img(src = "logopnud.png", height = "120px", style = "display:block; margin:auto;"),
      
      # Título
      h2("¿Qué mapeamos?", style = "color:#0072B2; margin-top:20px;"),
      
      tags$div(style="text-align:left;",
               p("La Herramienta de Georreferenciación de Cuidados permite identificar y caracterizar distintos tipos de servicios que sostienen la organización social del cuidado en los territorios. Estos servicios son fundamentales para garantizar el derecho al cuidado y reducir las desigualdades."),
               
               # Definición general de cuidados ONU
               h3("¿Qué entendemos por cuidados?"),
               tags$div(
                 style = "background-color:#f0f8ff; border-left:5px solid #0072B2; padding:15px; margin:15px 0; border-radius:8px;",
                 p("Los cuidados sustentan todas las formas de vida y son fundamentales para el bienestar de las personas y del planeta. Los cuidados pueden entenderse como una actividad que abarca todo lo que hacemos para mantener, perpetuar y reparar nuestro mundo."),
                 p("Esta definición amplia abarca el acto en que la persona cuida de sí misma, de los demás y del planeta, así como la prestación de apoyo y asistencia a quienes lo necesitan para que puedan participar en la sociedad con dignidad y autonomía."),
                 p("Todas las personas necesitan cuidados y apoyo en algún momento de la vida o en todo el transcurso de esta.")
               ),
               
               
               h3("Servicios de cuidado"),
               tags$div(
                 style = "background-color:#f9f9f9; border-left:5px solid #0072B2; 
          padding:15px; margin:15px 0; border-radius:8px;",
                 
                 p("Un servicio de cuidado se define como el conjunto de actividades para asistir a personas 
    (por ejemplo: niños y niñas, personas adultas mayores, personas con discapacidad) 
    que requieren apoyo en sus necesidades básicas y actividades diarias 
    (alimentación, higiene, movilidad, etc.). 
    Estos servicios buscan su bienestar físico, emocional y social, 
    y abarcan desde el cuidado personal directo hasta tareas de cuidado indirecto 
    (por ejemplo, tareas domésticas y de apoyo social como cocinar, limpiar o hacer compras)."),
                 
                 p("Algunos ejemplos incluyen:"),
                 
                 tags$ul(
                   tags$li("Centros de día para personas adultas mayores o personas con discapacidad: 
            ofrecen cuidados integrales de forma ambulatoria para personas con dependencia 
            leve o moderada, promoviendo su autonomía en su entorno habitual."),
                   
                   tags$li("Centros de cuidado infantil: servicios para niños y niñas, como guarderías 
            y jardines infantiles, destinados a apoyar a las familias y garantizar 
            un entorno seguro. También pueden incluir centros del sistema educativo 
            que atienden a niños y niñas menores de 5 años."),
                   
                   tags$li("Centros residenciales para personas adultas mayores, donde pernoctan 
            y reciben cuidados durante las 24 horas del día.")
                 )
               ),
               
               
               # Definición actualizada de Servicios de Apoyo
               h3("Servicios de apoyo"),
               tags$div(
                 style = "background-color:#e8f4ff; border-left:5px solid #0072B2; 
          padding:15px; margin:15px 0; border-radius:8px;",
                 
                 p("Los servicios de cuidado pueden abarcar, asimismo, servicios de apoyos específicos 
     para poblaciones en situación de dependencia. Se trata de servicios, acciones y recursos 
     que facilitan la autonomía y la independencia funcional de las personas, reduciendo la 
     necesidad de cuidados directos intensivos."),
                 
                 p("Incluyen adaptaciones del entorno, tecnologías de asistencia, transporte accesible, 
     rehabilitación, ayudas técnicas, orientación y acompañamiento para la inclusión social. 
     Estos servicios son clave para garantizar el ejercicio de derechos y la participación 
     plena en la comunidad.")
               ),
               
               
               # Fuente
               tags$p(style="font-size:14px; color:#555;", "Fuente: Naciones Unidas (2024), Transformar los sistemas de cuidado en el contexto de los Objetivos de Desarrollo Sostenible y nuestra agenda común."),
               
               
               h3("Mapas de cuidados y Gestión de Riesgos de Desastres"),
               tags$div(
                 style = "background-color:#f0f8ff; border-left:5px solid #0072B2; padding:15px; margin:15px 0; border-radius:8px; text-align:left;",
                 p("Los mapas de cuidados no solo permiten identificar brechas en la organización social del cuidado, sino que también tienen un alto potencial para fortalecer la resiliencia territorial frente a desastres."),
                 p("Al integrar datos de oferta, demanda y accesibilidad con información climática y modelos de riesgo, la herramienta facilita:"),
                 tags$ul(
                   tags$li("Identificación de centros de cuidado en zonas de riesgo y priorización de infraestructura crítica."),
                   tags$li("Localización rápida de población vulnerable (niños/as, personas adultas mayores, personas con discapacidad) para planes de evacuación inclusivos."),
                   tags$li("Diseño de rutas seguras y articulación con sistemas de alerta temprana."),
                   tags$li("Priorización de inversiones post-desastre en territorios con mayor brecha de accesibilidad."),
                   tags$li("Fortalecimiento de redes comunitarias de cuidado como parte de la respuesta y recuperación.")
                 ),
                 p("Este enfoque promueve la interoperabilidad entre sistemas de cuidado y plataformas de Gestión de Riesgos de Desastres (GRD), contribuyendo a políticas públicas más integrales y resilientes.")
               )
               
      )
    )
  )
)


,

  
  # Pestaña Niños y niñas (con panel lateral de gráficos interactivos)
  tabPanel(
    "Niños y niñas",
    fluidPage(
      # Mapa
      h3("Mapa de cuidados: Niñas y niños"),
      h5("Una vez cargado el mapa, cliquee el botón «Zoom a villanueva»"),
      actionButton("resetKids", "Zoom a Villanueva"),
      leafletOutput("mapKids", height = "500px"), 
      br(),
      h6("Nota: Las capas de OpenWeather mostradas en este mapa son generadas a partir de modelos meteorológicos globales y observaciones. 
         Estas imágenes representan condiciones actuales y pronósticos a corto plazo (nowcasting), actualizados aproximadamente cada 10 minutos (precipitaciones) o cada hora (viento). La visualización es indicativa y depende de la cobertura del servicio en la región."),
      # Tabla ocupando todo el ancho
      h3("Oferta mapeada de servicios de cuidado y apoyo"),
      DTOutput("tblKids"),
      br(),
      downloadButton("dlKids", "Descargar CSV"),
      br(), br(),
      # Estadísticas descriptivas (gráficos uno debajo del otro)
      wellPanel(
        h3("Estadísticas descriptivas"),
        fluidRow(
          column(
            width = 6,
            h4("% de niños/as (0–5) viviendo en desiertos de cuidado"),
            plotlyOutput("plotDesiertosKids", height = "250px")
          ),
          column(
            width = 6,
            h4("Distribución de centros por tipo de administración"),
            plotlyOutput("plotAdminKids", height = "250px")
          )
        )
      )
    )
  ),
  
  # Pestaña Personas adultas mayores
  tabPanel(
    "Personas adultas mayores",
    fluidPage(
      h3("Mapa de cuidados: Personas adultas mayores"),
      h5("Una vez cargado el mapa, cliquee el botón «Zoom a villanueva»"),
      actionButton("resetOlder", "Zoom a Villanueva"),
      leafletOutput("mapOlder", height = "400px"),
      br(),
      h6("Nota: Las capas de OpenWeather mostradas en este mapa son generadas a partir de modelos meteorológicos globales y observaciones. 
         Estas imágenes representan condiciones actuales y pronósticos a corto plazo (nowcasting), actualizados aproximadamente cada 10 minutos (precipitaciones) o cada hora (viento). La visualización es indicativa y depende de la cobertura del servicio en la región."),
      h4("Oferta mapeada de servicios de cuidado y apoyo"),
      DTOutput("tblOlder"),
      br(),
      downloadButton("dlOlder", "Descargar CSV"),
      br(), br(),
      # NUEVO: Estadísticas descriptivas (replicadas del tab Niños y niñas)
      wellPanel(
        h3("Estadísticas descriptivas"),
        fluidRow(
          column(
            width = 6,
            h4("% de personas adultas mayores viviendo en desiertos de cuidado"),
            plotlyOutput("plotDesiertosOlder", height = "250px")
          ),
          column(
            width = 6,
            h4("Distribución de centros por tipo de administración"),
            plotlyOutput("plotAdminOlder", height = "250px")
          )
        )
      )
    )
  ),
  
  # Pestaña Personas con discapacidad
  tabPanel(
    "Personas con discapacidad",
    fluidPage(
      h3("Mapa de cuidados: Personas con alguna discapacidad"),
      h5("Una vez cargado el mapa, cliquee el botón «Zoom a villanueva»"),
      actionButton("resetDisability", "Zoom a Villanueva"),
      leafletOutput("mapDisability", height = "400px"),
      br(),
      h6("Nota: Las capas de OpenWeather mostradas en este mapa son generadas a partir de modelos meteorológicos globales y observaciones. 
         Estas imágenes representan condiciones actuales y pronósticos a corto plazo (nowcasting), actualizados aproximadamente cada 10 minutos (precipitaciones) o cada hora (viento). La visualización es indicativa y depende de la cobertura del servicio en la región."),
      h4("Oferta mapeada de servicios de cuidado y apoyo"),
      DTOutput("tblDisability"),
      br(),
      downloadButton("dlDisability", "Descargar CSV"),
      br(), br(),
      # NUEVO: Estadísticas descriptivas (replicadas del tab Niños y niñas)
      wellPanel(
        h3("Estadísticas descriptivas"),
        fluidRow(
          column(
            width = 6,
            h4("% de personas con discapacidad viviendo en desiertos de cuidado"),
            plotlyOutput("plotDesiertosDisability", height = "250px")
          ),
          column(
            width = 6,
            h4("Distribución de centros por tipo de administración"),
            plotlyOutput("plotAdminDisability", height = "250px")
          )
        )
      )
    )
  ),
  
  # Pestaña Trabajo de campo (Power BI embebido)
  tabPanel(
    "Caracterización de centros",
    fluidPage(
      h3("Dashboard de resultados del trabajo de campo en Villanueva"),
      h5("Nota: El trabajo de campo fue realizado sobre una selección de centros*, con el objetivo de testear la metodología y los instrumentos de caracterización."),
      h5("Puede navegar interactivamente por los resultados sin salir de la pestaña."),
      tags$iframe(
        src = "https://app.powerbi.com/view?r=eyJrIjoiNTU3Mzc5NjQtMWUwMS00ZTY1LTlmZDQtODU0NjE4NTU4ZTZiIiwidCI6IjkxMzAwNmVlLTczNTItNDA2Ni1hYTIyLTE3ODBiZTBjMjQ4YSJ9&pageName=1702c1efc17823ec5679",
        width = "100%",
        height = "800px",
        style = "border:none;"
      ),
      h6("*Se caracterizan un total de 42 centros, identificados a partir de registros administrativos, web scraping y 250 encuesta aplicadas a mujeres mayores de 25 años habitantes del territorio.")
    )
  ),



# Pestaña Trabajo de campo (Power BI embebido)
tabPanel(
  "Avisos climatológicos",
  fluidPage(
    h3("Avisos climatológicos"),
    h5("Visualiza el clima actual y las previsiones meteorológicas en tiempo real."),
    
    tags$iframe(
      src = "https://embed.windy.com/embed.html?type=map&location=coordinates&metricRain=default&metricTemp=default&metricWind=default&zoom=10&overlay=capAlerts&product=capAlerts&level=surface&lat=15.292&lon=-87.946",
      width = "1000",
      height = "600",
      style = "border:none;"
    ),
    h6("Fuente: Windy.com")
  ) 
),







tabPanel(
  "Metodología",
  fluidPage(
    # CSS global para mejorar estética
    tags$head(
      tags$style(HTML("
        .metodologia-page h2 { font-size: 28px; font-weight: bold; color: #0072B2; }
        .metodologia-page h3 { font-size: 22px; color: #0072B2; margin-top: 25px; }
        .metodologia-page ul, .metodologia-page ol { font-size: 16px; line-height: 1.6; }
        .btn-primary {
          background-color: #0072B2; border-color: #0072B2; font-size: 16px; padding: 10px 20px;
        }
        .btn-primary:hover {
          background-color: #005a8c; border-color: #005a8c;
        }
      "))
    ),
    
    tags$div(
      class = "metodologia-page",
      style = "padding:30px; max-width:900px; margin:auto; font-size:16px; line-height:1.6; text-align:center;",
      
      # Logo centrado
      tags$img(src = "logopnud.png", height = "150px", style = "display:block; margin:auto;"),
      
      # Título
      h2("Metodología de la Herramienta de Georreferenciación de Cuidados"),
      
      # Introducción
      tags$div(style="text-align:left;",
               p("La Herramienta de Georreferenciación de Cuidados (HGC) es una solución digital desarrollada por el PNUD para fortalecer las capacidades estatales en el diseño de políticas de cuidado basadas en evidencia territorial. Surge como respuesta a las profundas desigualdades en la organización social del cuidado en América Latina y el Caribe, donde persisten brechas de género, socioeconómicas y territoriales que limitan el acceso equitativo a servicios esenciales."),
               
               p("La HGC permite mapear la oferta, la demanda y la accesibilidad de los servicios de cuidado en distintos territorios, integrando datos oficiales, fuentes alternativas (imágenes satelitales, web scraping) y mapeos colaborativos en campo. Esta combinación genera diagnósticos precisos que orientan la toma de decisiones, la planificación territorial y la gestión de riesgos de desastres."),
               
               # Objetivos
               h3("Objetivos principales"),
               tags$ul(
                 style = "list-style-position: inside; padding-left: 0;",
                 tags$li(HTML("<b>Visibilizar brechas territoriales</b> en la oferta y accesibilidad de cuidados.")),
                 tags$li(HTML("<b>Identificar desiertos de cuidado</b>: zonas donde la demanda es alta pero la oferta es inaccesible.")),
                 tags$li(HTML("<b>Fortalecer la infraestructura pública digital</b> construyendo capas de datos que pueden ser interoperables con sistemas de salud, educación y protección social.")),
                 tags$li(HTML("<b>Promover políticas inclusivas y sostenibles</b> basadas en evidencia territorializada."))
               ),
               
               # Etapas metodológicas con narrativa y diagrama
               h3("Etapas de la metodología"),
               # Imagen del diagrama centrada
               tags$img(src = "Diagrama 1.svg", height = "400px", style = "display:block; margin:auto; margin-top:20px;"),
               p("La metodología se basa en tres pilares que permiten construir diagnósticos territoriales precisos:"),
               
               tags$ol(
                 style = "text-align:left; padding-left:20px;",
                 tags$li(HTML("<b>Mapeo de oferta:</b> identificación y georreferenciación de centros que brindan cuidados o apoyos a niños/as, personas adultas mayores y personas con discapacidad. Para ello se integran:
            <ul>
              <li><b>Registros administrativos:</b> SEDUC, SEDESOL, SEMUJER, SENAF, alcaldías municipales.</li>
              <li><b>Web scraping:</b> extracción automatizada de datos públicos mediante la Google Places API.</li>
              <li><b>Trabajo de campo (identificación):</b> encuestas aplicadas personas habitantes del municipio, para identificar iniciativas comunitarias de cuidado, así como a empresas privadas y OSCs en colonias priorizadas.</li>
              <li><b>Trabajo de campo (caracterización):</b> encuestas aplicadas a una muestra de centros de cuidado y apoyo, con el objetivo de caracterizarlos en mayor profundidad y ampliar los datos disponibles.</li>

            </ul> Los datos se homologan, depuran y georreferencian para construir una base consolidada por grupo poblacional y tipo de administración.")),
                 
                 
                 
                 tags$li(
                   tags$b("Mapeo de demanda:"),
                   " estimación de población que podría requerir cuidados. Se consideran tres grupos:",
                   tags$ul(
                     style = "list-style-position: inside; padding-left: 0;",
                     tags$li(
                       tags$b("Niños/as (0-5 años) y personas adultas mayores (60+):"),
                       " estimaciones de alta resolución basadas en imágenes satelitales y proyecciones demográficas, disponibles en ",
                       tags$a(
                         href = "https://data.humdata.org/dataset/honduras-high-resolution-population-density-maps-demographic-estimates",
                         target = "_blank",
                         "Humanitarian Data Exchange"
                       ),
                       "(HDX)."
                     ),
                     tags$li(
                       tags$b("Personas con discapacidad:"),
                       " datos del Censo Nacional de Población y Vivienda 2013, procesados y facilitados por el Instituto Nacional de Estadística (INE)."
                     )
                   ),
                   " Las capas se presentan en formato de hexágonos (HDX) o segmentos censales (INE)."
                 ),
                 
                 
                 tags$li(HTML("<b>Análisis de accesibilidad:</b> cálculo de tiempos de viaje a pie desde cada unidad geográfica hasta el centro más cercano, usando:
            <ul>
              <li>API de ruteo de OpenStreetMaps (OSMR).</li>
              <li>Librería UrbanPy del BID (red vial, elevación y rutas reales).</li>
            </ul> Se aplican ajustes para evitar sobreestimaciones y se generan isócronas de tiempos de caminata.")
                 )
               ),
               
               # Recuadro destacado para Desiertos de Cuidado
               h3("Desiertos de Cuidado"),
               tags$div(
                 style = "background-color:#f0f8ff; border-left:5px solid #0072B2; padding:15px; margin:20px 0; border-radius:8px; text-align:left;",
                 tags$p("Se consideran desiertos de cuidado aquellas zonas donde:"),
                 tags$ul(
                   style = "list-style-position: inside; padding-left: 0;",
                   tags$li("La demanda es igual o superior a la media o mediana del área."),
                   tags$li("El centro de cuidado más cercano está localizado a más de 20 minutos caminando.")
                 ),
                 tags$p("Este análisis permite identificar zonas críticas para la intervención de políticas públicas, visibilizando aquellos territorios donde existe demanda potencial por servicios de cuidado y apoyo, pero no pueden alcanzarlos en un tiempo razonable.")
               ),
               
               
               h3("Integración con datos climáticos y Gestión de Riesgos"),
               tags$div(
                 style = "background-color:#f9f9f9; border-left:5px solid #0072B2; padding:15px; margin:20px 0; border-radius:8px; text-align:left;",
                 p("La Herramienta de Georreferenciación de Cuidados puede interoperar con datos climáticos en tiempo real para fortalecer la resiliencia territorial y la planificación ante emergencias."),
                 p("¿Cómo se logra esta integración?"),
                 tags$ul(
                   tags$li("Superposición de capas climáticas (precipitación, viento, nubosidad, presión) sobre los mapas de oferta, demanda y accesibilidad."),
                   tags$li("Cruce de información para identificar centros de cuidado en zonas de riesgo y población vulnerable expuesta."),
                   tags$li("Articulación con sistemas de alerta temprana y rutas seguras para planes de evacuación inclusivos."),
                   tags$li("Priorización de rehabilitación de infraestructura y focalización de recursos post-desastre.")
                 ),
                 p("En este visualizador, pueden observarse ejemplos sencillos de esta superposición, basados en APIs externas y no oficiales."),
                 h4("Interpretación de las capas climáticas"),
                 p("Las capas superpuestas en los mapas de cuidados provienen de la API Weather Maps de OpenWeather y representan condiciones actuales y pronósticos inmediatos (nowcasting):"),
                 tags$ul(
                   tags$li("Precipitación: actualizada cada ~10 minutos, basada en radar y modelos de corto plazo."),
                   tags$li("Viento: actualizado cada ~1 hora, basado en modelos globales."),
                   tags$li("Estas capas son indicativas y deben interpretarse como apoyo para la toma de decisiones, no como sustituto de sistemas oficiales de alerta.")
                 ),
                 p("Sumado a esto, en la pestaña «Avisos climatológicos» puede accederse a una aplicación más completa de nowcasting, que permite navegar por distintas capas de información."),
                 tags$p(style="font-size:14px; color:#555;",
                        HTML("<i>Nota:</i> La integración de datos climáticos permite anticipar impactos en la infraestructura de cuidados y planificar respuestas inclusivas ante huracanes, tormentas y otros eventos extremos.")
                 )
               ),
               
               
               # Valor agregado
               h3("¿Por qué es innovadora esta metodología?"),
               p("La HGC no solo produce mapas, sino que busca contribuir a la construcción de una infraestructura pública digital que articula datos abiertos, registros administrativos, fuentes de información innovadoras y gobernanza colaborativa. Su enfoque territorial, de género e interseccional permite diseñar políticas más equitativas, reducir la sobrecarga de cuidados que recae sobre las mujeres y fortalecer la resiliencia comunitaria frente a emergencias."),
               
               p("Además, la herramienta incorpora fuentes de datos no tradicionales como imágenes satelitales y modelos de aprendizaje automático, lo que mejora la precisión del diagnóstico territorial en contextos con baja disponibilidad de datos oficiales. También visibiliza la oferta comunitaria, muchas veces omitida en registros administrativos, y promueve su integración en sistemas nacionales de cuidado."),
               
               # Botón para descargar nota de política
               br(),
               tags$a(
                 href = "https://www.undp.org/es/latin-america/publicaciones/cartografias-del-cuidado-en-america-latina-y-el-caribe-herramientas-digitales-para-disenar-politicas-de-cuidado-basadas",
                 target = "_blank",
                 class = "btn btn-primary",
                 "Descargar Nota de Política"
               )
      )
    )
  )
)


)


# Server
server <- function(input, output, session) {
  
  
  observeEvent(input$goToMap, {
    updateTabsetPanel(session, "nav", selected = "Metodología")
  })
  

  # Paletas
  pal_children   <- colorNumeric("Blues",   domain = sf_children$children_2,  na.color = "#cccccc")
  pal_elderly    <- colorNumeric("Greens",  domain = sf_elderly$elderly_20,   na.color = "#cccccc")
  pal_disability <- colorNumeric("Purples", domain = sf_disability$Alguna.dis, na.color = "#cccccc")
  
  pal_oferta_children   <- colorFactor(c("blue", "orange"), domain = sf_oferta_children$administra)
  pal_oferta_elderly    <- colorFactor(c("blue", "orange"), domain = sf_oferta_elderly$administra)
  pal_oferta_disability <- colorFactor(c("blue", "orange"), domain = sf_oferta_disability$administra)
  
  # Flag de "Desierto de cuidados"
  desierto_flag <- function(sf_obj) {
    if ("Desierto.de.cuidados_q" %in% names(sf_obj)) {
      return(sf_obj[["Desierto.de.cuidados_q"]] == "Desierto de cuidados")
    } else {
      return(rep(NA, nrow(sf_obj)))
    }
  }
  flag_children   <- desierto_flag(sf_desiertos_children)
  flag_elderly    <- desierto_flag(sf_desiertos_elderly)
  flag_disability <- desierto_flag(sf_desiertos_disability)
  
  # Vista inicial
  initial_lng  <- -88.03
  initial_lat  <- 15.3
  initial_zoom <- 4
  
  # --- MAPAS ---
  output$mapKids <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addTiles(group = "Calles (OSM)") %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      # DEMANDA
      addPolygons(
        data = sf_children,
        fillColor = ~pal_children(children_2),
        color = "white", weight = 1, fillOpacity = 0.7,
        group = "Demanda de cuidados",
        popup = ~paste0(
          "<b>Nº niños y niñas de 0 a 5 años estimado:</b> ",
          ifelse(is.na(round(children_2)), 0, round(children_2)),
          "<br>Fuente: Estimaciones de alta definición disponibles en",
          "<br>Humanitarian Data Exchange"
        )
      ) %>%
      # OFERTA
      addCircleMarkers(
        data = sf_oferta_children,
        fillColor = ~pal_oferta_children(administra),
        color = ~pal_oferta_children(administra),
        radius = 4, stroke = TRUE, weight = 1, fillOpacity = 1,
        group = "Oferta de cuidados",
        layerId = ~nombre,
        popup = ~paste0(
          "<b>Centro:</b> ", nombre,
          "<br><b>Administración:</b> ", administra,
          "<br><b>Dirección:</b> ", direccion,
          "<br><b>Población atendida:</b> ", poblacion,
          "<br><b>Fuente:</b> ", fuente
        )
      ) %>%
      # DESIERTOS
      addPolygons(
        data = sf_desiertos_children,
        fill = FALSE, fillOpacity = 0,
        color = ~ifelse(flag_children, "red", NA),
        opacity = ~ifelse(flag_children, 1, 0),
        weight = ~ifelse(flag_children, 2, 0),
        group = "Desiertos de cuidado",
        popup = ~paste0("<b>Desierto de cuidados:</b> ", ifelse(flag_children, "Sí", "No"))
      ) %>%
      # Control
      addLayersControl(
        baseGroups = c("Satélite (Esri)", "Calles (OSM)"),
        overlayGroups = c("Demanda de cuidados", "Oferta de cuidados", "Desiertos de cuidado", "Precipitaciones (OpenWeather)",
                          "Viento (OpenWeather)"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      # Leyendas
      addLegend(pal = pal_children, values = sf_children$children_2,
                position = "topleft",
                title = "Nº niños y niñas (0 a 5 años)<br><small>Fuente: Humanitarian Data Exchange</small>",
                na.label = "0")  %>% # <-- reemplaza NA por 0) 
      addLegend(pal = pal_oferta_children, values = sf_oferta_children$administra,
                position = "topright", title = "Oferta de cuidados") %>%
      addLegend(position = "topleft",
                colors = "transparent",
                labels = HTML("<div style='display:inline-block;width:20px;height:20px;border:2px solid red;margin-right:6px;'></div> Desierto de cuidados<br><small>(alta demanda + baja accesibilidad a cuidados)</small>"),
                title = "Desiertos de cuidado")
  })
  
  owm_key <- ""  
  
  owm_precip <- paste0("https://tile.openweathermap.org/map/precipitation_new/{z}/{x}/{y}.png?appid=", owm_key)
  
  owm_wind <- paste0("https://tile.openweathermap.org/map/wind_new/{z}/{x}/{y}.png?appid=", owm_key)
  
  # Añadir capa cuando el mapa esté listo
  observeEvent(input$mapKids_bounds, {
    leafletProxy("mapKids") %>%
      addTiles(
        urlTemplate = owm_precip,
        options = tileOptions(opacity = 1, zIndex = 999),
        group = "Precipitaciones (OpenWeather)"
      )
  })
 
  observeEvent(input$mapKids_bounds, {
    leafletProxy("mapKids") %>%
      addTiles(
        urlTemplate = owm_wind,
        options = tileOptions(opacity = 0.8, zIndex = 999),
        group = "Viento (OpenWeather)"
      )
  }) 
  
  
  
  output$mapOlder <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satélite (Esri)") %>%
      addTiles(group = "Calles (OSM)") %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      addPolygons(
        data = sf_elderly,
        fillColor = ~pal_elderly(elderly_20),
        color = "white", weight = 1, fillOpacity = 0.7,
        group = "Demanda de cuidados",
        popup = ~paste0(
          "<b>Nº adultos mayores estimado:</b> ",
          ifelse(is.na(round(elderly_20)), 0, round(elderly_20)),
          "<br>Fuente: Estimaciones de alta definición disponibles en",
          "<br>Humanitarian Data Exchange"
        )
      ) %>%
      addCircleMarkers(
        data = sf_oferta_elderly,
        fillColor = ~pal_oferta_elderly(administra),
        color = ~pal_oferta_elderly(administra),
        radius = 4, stroke = TRUE, weight = 1, fillOpacity = 1,
        group = "Oferta de cuidados",
        layerId = ~nombre,
        popup = ~paste0(
          "<b>Centro:</b> ", nombre,
          "<br><b>Administración:</b> ", administra,
          "<br><b>Dirección:</b> ", direccion,
          "<br><b>Población atendida:</b> ", poblacion,
          "<br><b>Fuente:</b> ", fuente
        )
      ) %>%
      addPolygons(
        data = sf_desiertos_elderly,
        fill = FALSE, fillOpacity = 0,
        color = ~ifelse(flag_elderly, "red", NA),
        opacity = ~ifelse(flag_elderly, 1, 0),
        weight = ~ifelse(flag_elderly, 2, 0),
        group = "Desiertos de cuidado",
        popup = ~paste0("<b>Desierto de cuidados:</b> ", ifelse(flag_elderly, "Sí", "No"))
      ) %>%
      addLayersControl( baseGroups = c("Satélite (Esri)", "Calles (OSM)"),
        overlayGroups = c("Demanda de cuidados", "Oferta de cuidados", "Desiertos de cuidado", "Precipitaciones (OpenWeather)", "Viento (OpenWeather)"),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      addLegend(pal = pal_elderly, values = sf_elderly$elderly_20,
                position = "topleft",
                na.label = "0",
                title = "Nº personas adultas mayores (60+)<br><small>Fuente: Humanitarian Data Exchange</small>") %>%
      addLegend(pal = pal_oferta_elderly, values = sf_oferta_elderly$administra,
                position = "topright",
                title = "Oferta de servicios de <br>cuidado y apoyo") %>%
      addLegend(position = "topleft",
                colors = "transparent",
                labels = HTML("<div style='display:inline-block;width:20px;height:20px;border:2px solid red;margin-right:6px;'></div> Desierto de cuidados<br><small>(alta demanda + baja accesibilidad a cuidados)</small>"),
                title = "Desiertos de cuidado")
  })
  
  # Añadir capa cuando el mapa esté listo
  observeEvent(input$mapOlder_bounds, {
    leafletProxy("mapOlder") %>%
      addTiles(
        urlTemplate = owm_precip,
        options = tileOptions(opacity = 1, zIndex = 999),
        group = "Precipitaciones (OpenWeather)"
      )
  })
  
  observeEvent(input$mapOlder_bounds, {
    leafletProxy("mapOlder") %>%
      addTiles(
        urlTemplate = owm_wind,
        options = tileOptions(opacity = 0.8, zIndex = 999),
        group = "Viento (OpenWeather)"
      )
  }) 

  
  
  output$mapDisability <- renderLeaflet({
    leaflet() %>%
      # Capas base (cada una con su "group" para el selector)
      addProviderTiles("Esri.WorldImagery", group = "Satélite (Esri)") %>%
      #addProviderTiles("CartoDB.Positron", group = "Calles (CartoDB)") %>%
      # Si prefieres OSM clásico:
      addTiles(group = "Calles (OSM)") %>%
      
      # Define la vista inicial (la base que se vea primero será la última que agregues antes del setView)
      setView(lng = initial_lng, lat = initial_lat, zoom = initial_zoom) %>%
      
      # ---- Overlays (se mantienen igual) ----
    addPolygons(
      data = sf_disability,
      fillColor = ~pal_disability(Alguna.dis),
      color = "white", weight = 1, fillOpacity = 0.7,
      group = "Demanda de cuidados",
      popup = ~paste0(
        "<b>Nº personas con alguna limitación:</b> ",
        ifelse(is.na(round(Alguna.dis)), 0, round(Alguna.dis)),
        "<br>Fuente: Censo Nacional de Población y Vivienda 2013"
      )
    ) %>%
      addCircleMarkers(
        data = sf_oferta_disability,
        fillColor = ~pal_oferta_disability(administra),
        color = ~pal_oferta_disability(administra),
        radius = 4, stroke = TRUE, weight = 1, fillOpacity = 1,
        group = "Oferta de cuidados",
        layerId = ~nombre,
        popup = ~paste0(
          "<b>Centro:</b> ", nombre,
          "<br><b>Administración:</b> ", administra,
          "<br><b>Dirección:</b> ", direccion,
          "<br><b>Población atendida:</b> ", poblacion,
          "<br><b>Fuente:</b> ", fuente
        )
      ) %>%
      addPolygons(
        data = sf_desiertos_disability,
        fill = FALSE, fillOpacity = 0,
        color = ~ifelse(flag_disability, "red", NA),
        opacity = ~ifelse(flag_disability, 1, 0),
        weight = ~ifelse(flag_disability, 2, 0),
        group = "Desiertos de cuidado",
        popup = ~paste0("<b>Desierto de cuidados:</b> ", ifelse(flag_disability, "Sí", "No"))
      ) %>%
      
      # Control de capas: bases + overlays
      addLayersControl(
        # baseGroups = c("Satélite (Esri)", "Calles (CartoDB)"),
        # Si usas OSM en vez de CartoDB:
        baseGroups = c("Satélite (Esri)", "Calles (OSM)"),
        overlayGroups = c(
          "Demanda de cuidados",
          "Oferta de cuidados",
          "Desiertos de cuidado",
          "Precipitaciones (OpenWeather)",
          "Viento (OpenWeather)"
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "topright"
      ) %>%
      
      # Leyendas (se mantienen)
      addLegend(
        pal = pal_disability, values = sf_disability$Alguna.dis,
        position = "topleft",
        title = "N° personas con <br>alguna discapacidad<br><small>Fuente: Censo 2013</small>"
      ) %>%
      addLegend(
        pal = pal_oferta_disability, values = sf_oferta_disability$administra,
        position = "topright", title = "Oferta de servicios de cuidado y apoyo"
      ) %>%
      addLegend(
        position = "topleft",
        colors = "transparent",
        labels = HTML("<div style='display:inline-block;width:20px;height:20px;border:2px solid red;margin-right:6px;'></div> Desierto de cuidados<br><small>(alta demanda + baja accesibilidad a cuidados)</small>"),
        title = "Desiertos de cuidado"
      )
    
  })
  
  # Añadir capa cuando el mapa esté listo
  observeEvent(input$mapDisability_bounds, {
    leafletProxy("mapDisability") %>%
      addTiles(
        urlTemplate = owm_precip,
        options = tileOptions(opacity = 1, zIndex = 999),
        group = "Precipitaciones (OpenWeather)"
      )
  })
  
  observeEvent(input$mapDisability_bounds, {
    leafletProxy("mapDisability") %>%
      addTiles(
        urlTemplate = owm_wind,
        options = tileOptions(opacity = 0.8, zIndex = 999),
        group = "Viento (OpenWeather)"
      )
  }) 
  
  # --- Tablas con encabezados bonitos ---
  output$tblKids <- renderDT({
    datatable(
      sf_oferta_children %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente),
      colnames = c("Nombre", "Administración", "Dirección", "Población atendida", "Fuente"),
      options = list(pageLength = 5)
    )
  }, selection = "single")
  
  output$tblOlder <- renderDT({
    datatable(
      sf_oferta_elderly %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente),
      colnames = c("Nombre", "Administración", "Dirección", "Población atendida", "Fuente"),
      options = list(pageLength = 5)
    )
  }, selection = "single")
  
  output$tblDisability <- renderDT({
    datatable(
      sf_oferta_disability %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente),
      colnames = c("Nombre", "Administración", "Dirección", "Población atendida", "Fuente"),
      options = list(pageLength = 5)
    )
  }, selection = "single")
  
  # --- Reactividad: selección en tabla -> zoom + popup (sin marcador extra)
  observeEvent(input$tblKids_rows_selected, {
    row <- sf_oferta_children[input$tblKids_rows_selected, ]
    if (nrow(row) > 0) {
      content <- paste0(
        "<b>Centro:</b> ", row$nombre,
        "<br><b>Administración:</b> ", row$administra,
        "<br><b>Dirección:</b> ", row$direccion,
        "<br><b>Población atendida:</b> ", row$poblacion,
        "<br><b>Fuente:</b> ", row$fuente
      )
      leafletProxy("mapKids") %>%
        clearPopups() %>%
        setView(lng = row$lng, lat = row$lat, zoom = 16) %>%
        addPopups(lng = row$lng, lat = row$lat, popup = content)
    }
  })
  observeEvent(input$tblOlder_rows_selected, {
    row <- sf_oferta_elderly[input$tblOlder_rows_selected, ]
    if (nrow(row) > 0) {
      content <- paste0(
        "<b>Centro:</b> ", row$nombre,
        "<br><b>Administración:</b> ", row$administra,
        "<br><b>Dirección:</b> ", row$direccion,
        "<br><b>Población atendida:</b> ", row$poblacion,
        "<br><b>Fuente:</b> ", row$fuente
      )
      leafletProxy("mapOlder") %>%
        clearPopups() %>%
        setView(lng = row$lng, lat = row$lat, zoom = 16) %>%
        addPopups(lng = row$lng, lat = row$lat, popup = content)
    }
  })
  observeEvent(input$tblDisability_rows_selected, {
    row <- sf_oferta_disability[input$tblDisability_rows_selected, ]
    if (nrow(row) > 0) {
      content <- paste0(
        "<b>Centro:</b> ", row$nombre,
        "<br><b>Administración:</b> ", row$administra,
        "<br><b>Dirección:</b> ", row$direccion,
        "<br><b>Población atendida:</b> ", row$poblacion,
        "<br><b>Fuente:</b> ", row$fuente
      )
      leafletProxy("mapDisability") %>%
        clearPopups() %>%
        setView(lng = row$lng, lat = row$lat, zoom = 16) %>%
        addPopups(lng = row$lng, lat = row$lat, popup = content)
    }
  })
  
  # --- Botones: Resetear vista
  observeEvent(input$resetKids, {
    leafletProxy("mapKids") %>%
      clearPopups() %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = 11)
  })
  observeEvent(input$resetOlder, {
    leafletProxy("mapOlder") %>%
      clearPopups() %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = 11)
  })
  observeEvent(input$resetDisability, {
    leafletProxy("mapDisability") %>%
      clearPopups() %>%
      setView(lng = initial_lng, lat = initial_lat, zoom = 11)
  })
  
  # --- Descargas CSV
  output$dlKids <- downloadHandler(
    filename = function() sprintf("oferta_ninos_%s.csv", Sys.Date()),
    content = function(file) {
      sf_oferta_children %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  output$dlOlder <- downloadHandler(
    filename = function() sprintf("oferta_adultos_mayores_%s.csv", Sys.Date()),
    content = function(file) {
      sf_oferta_elderly %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  output$dlDisability <- downloadHandler(
    filename = function() sprintf("oferta_discapacidad_%s.csv", Sys.Date()),
    content = function(file) {
      sf_oferta_disability %>%
        sf::st_drop_geometry() %>%
        select(nombre, administra, direccion, poblacion, fuente) %>%
        write.csv(file, row.names = FALSE)
    }
  )
  
  # =========================
  # ESTADÍSTICAS (Niños y niñas) — Plotly
  # =========================
  
  # 1) Torta: % de centros por administración (valores originales)
  
  output$plotAdminKids <- renderPlotly({
    admin_tbl <- sf_oferta_children %>%
      sf::st_drop_geometry() %>%
      count(administra, name = "n")
    
    plot_ly(
      admin_tbl,
      labels = ~administra,
      values = ~n,
      type   = "pie",
      hole = 0.6,
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("#0072B2", "orange")) # Azul y verde
    ) %>%
      layout(
        showlegend = TRUE,
#        legend = list(orientation = "h", x = 0.3, y = -0.1),
        margin = list(t = 30, b = 30)
      )
  })
  
  

  
  output$plotDesiertosKids <- renderPlotly({
    desiertos_df <- sf_desiertos_children %>% sf::st_drop_geometry()
    children_df <- sf_children %>% sf::st_drop_geometry()
    
    hex_col_des <- if ("hex" %in% names(desiertos_df)) "hex" else names(desiertos_df)[1]
    hex_col_ch <- if ("hex" %in% names(children_df)) "hex" else names(children_df)[1]
    
    idx_desierto <- which(flag_children %in% TRUE)
    hex_desierto <- if (length(idx_desierto) > 0) desiertos_df[[hex_col_des]][idx_desierto] else character(0)
    
    total_personas <- sum(children_df$children_2, na.rm = TRUE)
    personas_desierto <- if (length(hex_desierto) > 0) {
      sum(children_df$children_2[children_df[[hex_col_ch]] %in% hex_desierto], na.rm = TRUE)
    } else 0
    
    porcentaje <- if (total_personas > 0) round((personas_desierto / total_personas) * 100, 1) else 0
    
    # Datos para donut
    donut_data <- data.frame(
      categoria = c("Reside en desierto de cuidados", "Reside fuera de desierto"),
      valor = c(personas_desierto, total_personas - personas_desierto)
    )
    
    plot_ly(
      donut_data,
      labels = ~categoria,
      values = ~valor,
      type = "pie",
      hole = 0.6,  # <-- donut
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("darkred", "#e6f5ff"))
    ) %>%
      layout(
        showlegend = TRUE,
#        legend = list(orientation = "h", x = 0.3, y = -0.1),
        margin = list(t = 30, b = 30),
        annotations = list(
          list(
            text = paste0(porcentaje, "%"),
            font = list(size = 28),
            showarrow = FALSE,
            x = 0.5, y = 0.5
          )
        )
      )
  })
  
  # 3) Torta: % de centros por administración (Adultos mayores)
  output$plotAdminOlder <- renderPlotly({
    admin_tbl <- sf_oferta_elderly %>%
      sf::st_drop_geometry() %>%
      count(administra, name = "n")
    
    plot_ly(
      admin_tbl,
      labels = ~administra,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("#0072B2", "orange"))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30)
      )
  })
  
  # 4) Donut: % de personas adultas mayores que viven en desiertos de cuidado
  output$plotDesiertosOlder <- renderPlotly({
    desiertos_df <- sf_desiertos_elderly %>% sf::st_drop_geometry()
    elderly_df   <- sf_elderly            %>% sf::st_drop_geometry()
    
    hex_col_des <- if ("hex" %in% names(desiertos_df)) "hex" else names(desiertos_df)[1]
    hex_col_el  <- if ("hex" %in% names(elderly_df))   "hex" else names(elderly_df)[1]
    
    idx_desierto <- which(flag_elderly %in% TRUE)
    hex_desierto <- if (length(idx_desierto) > 0) desiertos_df[[hex_col_des]][idx_desierto] else character(0)
    
    total_personas    <- sum(elderly_df$elderly_20, na.rm = TRUE)
    personas_desierto <- if (length(hex_desierto) > 0) {
      sum(elderly_df$elderly_20[elderly_df[[hex_col_el]] %in% hex_desierto], na.rm = TRUE)
    } else 0
    
    porcentaje <- if (total_personas > 0) round((personas_desierto / total_personas) * 100, 1) else 0
    
    donut_data <- data.frame(
      categoria = c("Reside en desierto de cuidados", "Reside fuera de desierto"),
      valor     = c(personas_desierto, total_personas - personas_desierto)
    )
    
    plot_ly(
      donut_data,
      labels = ~categoria,
      values = ~valor,
      type   = "pie",
      hole   = 0.6,
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("darkred", "#e6f5ff"))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30),
        annotations = list(
          list(
            text = paste0(porcentaje, "%"),
            font = list(size = 28),
            showarrow = FALSE, x = 0.5, y = 0.5
          )
        )
      )
  })
  
  
  # 5) Torta: % de centros por administración (Discapacidad)
  output$plotAdminDisability <- renderPlotly({
    admin_tbl <- sf_oferta_disability %>%
      sf::st_drop_geometry() %>%
      count(administra, name = "n")
    
    plot_ly(
      admin_tbl,
      labels = ~administra,
      values = ~n,
      type = "pie",
      hole = 0.6,
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("#0072B2", "orange"))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30)
      )
  })
  
  
  
  # Donut: % de personas con discapacidad que viven en desiertos de cuidado
  output$plotDesiertosDisability <- renderPlotly({
    # Quitar geometría
    desiertos_df <- sf_desiertos_disability %>% sf::st_drop_geometry()
    
    # Validar columnas
    if (!("Desierto.de.cuidados_q" %in% names(desiertos_df)) ||
        !("N..personas.con.alguna.discapacidad" %in% names(desiertos_df))) {
      return(NULL)
    }
    
    # Población total
    total_personas <- sum(desiertos_df[["N..personas.con.alguna.discapacidad"]], na.rm = TRUE)
    
    # Población en desiertos (filtrar por Desierto.de.cuidados_q == "Desierto de cuidados")
    personas_desierto <- sum(
      desiertos_df[["N..personas.con.alguna.discapacidad"]][desiertos_df[["Desierto.de.cuidados_q"]] == "Desierto de cuidados"],
      na.rm = TRUE
    )
    
    # Porcentaje
    porcentaje <- if (total_personas > 0) round((personas_desierto / total_personas) * 100, 1) else 0
    
    # Datos para donut
    donut_data <- data.frame(
      categoria = c("Reside en desierto de cuidados", "Reside fuera de desierto"),
      valor     = c(personas_desierto, max(total_personas - personas_desierto, 0))
    )
    
    plot_ly(
      donut_data,
      labels = ~categoria,
      values = ~valor,
      type   = "pie",
      hole   = 0.6,
      textinfo = "none",
      insidetextorientation = "radial",
      marker = list(colors = c("darkred", "#e6f5ff"))
    ) %>%
      layout(
        showlegend = TRUE,
        margin = list(t = 30, b = 30),
        annotations = list(
          list(
            text = paste0(porcentaje, "%"),
            font = list(size = 28),
            showarrow = FALSE, x = 0.5, y = 0.5
          )
        )
      )
  })
  
  
  
  
}

# Lanzar app
shinyApp(ui, server)





