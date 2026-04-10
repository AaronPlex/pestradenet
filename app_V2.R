library(shiny)
library(tidyverse)
library(igraph)
library(ggraph)
library(reshape2)
library(shinythemes)

# ── Helper: abbreviate long country names ──
abbreviate_countries <- function(df, col) {
  lookup <- c(
    "Antigua and Barbuda" = "ANTI",
    "Bahrain, Kingdom of" = "Bahrain",
    "Bolivia, Plurinational State of" = "Bolivia",
    "Bolivia (Plurinational State of)" = "Bolivia",
    "Bosnia and Herzegovina" = "BiH",
    "Central African Republic" = "CF",
    "China, mainland" = "China",
    "Democratic Republic of the Congo" = "Congo-Kinshasa",
    "Congo, Democratic Republic of the" = "Congo-Kinshasa",
    "Congo, Republic of the" = "Congo Republic",
    "Congo" = "Congo Republic",
    "Czech Republic" = "Czechia",
    "Dominican Republic" = "DOM",
    "Hong Kong, China" = "Hong Kong",
    "China, Hong Kong SAR" = "Hong Kong",
    "Iran (Islamic Republic of)" = "Iran",
    "C\u00f4te d'Ivoire" = "Ivory Coast",
    "Saint Kitts and Nevis" = "KN",
    "Kuwait, the State of" = "Kuwait",
    "Kyrgyzstan" = "Kyr",
    "Kyrgyz Republic" = "Kyr",
    "Lao People's Democratic Republic" = "Laos",
    "Lebanese Republic" = "Lebanon",
    "Macao, China" = "Macao",
    "China, Macao SAR" = "Macao",
    "Micronesia (Federated States of)" = "Micronesia",
    "Moldova, Republic of" = "Moldova",
    "Republic of Moldova" = "Moldova",
    "Netherlands (Kingdom of the)" = "Netherlands",
    "Democratic People's Republic of Korea" = "North Korea",
    "Papua New Guinea" = "PNG",
    "Russian Federation" = "Russia",
    "Saudi Arabia, Kingdom of" = "SB",
    "Saudi Arabia" = "SB",
    "Slovak Republic" = "Slovakia",
    "Korea, Republic of" = "South Korea",
    "Republic of Korea" = "South Korea",
    "Sao Tome and Principe" = "STD",
    "Syrian Arab Republic" = "Syria",
    "China, Taiwan Province of" = "Taiwan",
    "United Republic of Tanzania" = "Tanzania",
    "Trinidad and Tobago" = "TT",
    "T\u00fcrkiye" = "Turkey",
    "United Arab Emirates" = "UAE",
    "United Kingdom" = "UK",
    "United Kingdom of Great Britain and Northern Ireland" = "UK",
    "United States of America" = "USA",
    "United States" = "USA",
    "Saint Vincent and the Grenadines" = "VC",
    "Venezuela, Bolivarian Republic of" = "Venezuela",
    "Venezuela (Bolivarian Republic of)" = "Venezuela",
    "Viet Nam" = "Vietnam"
  )
  vals <- df[[col]]
  for (long in names(lookup)) {
    vals[vals == long] <- lookup[[long]]
  }
  df[[col]] <- vals
  df
}

# ── UI ──
ui <- navbarPage(
  title = "Pest Introduction Risk Tool",
  theme = shinytheme("flatly"),

  tabPanel("About",
    fluidRow(
      column(8, offset = 2,
        br(),
        h2("Pest Introduction Risk Based on International Trade"),
        hr(),
        p("This application estimates the risk of invasive pest introduction into
          countries through international trade networks. It combines three data
          sources to build a gravity-style risk model:"),
        tags$ul(
          tags$li(strong("Trade data"), " \u2014 bilateral commodity trade volumes
                  between countries (WTO format with columns: Reporting.Economy,
                  Partner.Economy, Year, Value)."),
          tags$li(strong("Invasive species data"), " \u2014 reported presence and
                  geographic extent of a target pest in each country (columns:
                  Region, Presence, Extent, First.record, USOrder)."),
          tags$li(strong("Host data"), " \u2014 harvested area of the host crop in
                  each country (FAOSTAT format with columns: Area, Year, Value).")
        ),
        p("The tool constructs a directed network where edge weights reflect the
          combined influence of trade volume, pest prevalence in the exporting
          country, host availability, pest establishment duration, and
          biosecurity measures. Several network centrality measures are then
          computed to rank countries by introduction risk."),
        h4("Centrality Measures"),
        tags$ul(
          tags$li(strong("Joint Probability"), " \u2014 the probability that at
                  least one trade pathway introduces the pest."),
          tags$li(strong("Node Strength"), " \u2014 total weighted in-degree from
                  all trading partners."),
          tags$li(strong("Betweenness Centrality"), " \u2014 how often a country
                  lies on the shortest risk path between other countries."),
          tags$li(strong("Eigenvector Centrality"), " \u2014 importance based on
                  the importance of trading partners."),
          tags$li(strong("PageRank Centrality"), " \u2014 random-walk measure of
                  arrival likelihood.")
        ),
        h4("How to Use"),
        tags$ol(
          tags$li("Go to the ", strong("Analysis"), " tab."),
          tags$li("Upload the three CSV files."),
          tags$li("Select one or more centrality measures."),
          tags$li("Choose Global or Regional Network view."),
          tags$li("Click ", strong("Run Analysis"), " to generate the plot.")
        ),
        hr(),
        p(em("Developed by Aaron Plex. Methodology based on geographic trade
             risk modelling for pest movement potential."))
      )
    )
  ),

  tabPanel("Analysis",
    sidebarLayout(
      sidebarPanel(width = 3,
        h4("Inputs"),
        hr(),
        fileInput("tradeFile", "Trade Data (CSV)", accept = ".csv",
                  placeholder = "WTO bilateral imports file"),
        fileInput("pestFile", "Invasive Species Data (CSV)", accept = ".csv",
                  placeholder = "Pest distribution file"),
        fileInput("hostFile", "Host Data (CSV)", accept = ".csv",
                  placeholder = "FAOSTAT harvested area file"),
        hr(),
        h4("Centrality Measures"),
        checkboxGroupInput("centrality", NULL,
          choices = c("Joint Probability"        = "joint.risk",
                      "Node Strength"            = "country.strength",
                      "Betweenness Centrality"   = "country.betweenness",
                      "Eigenvector Centrality"   = "country.eigenvector",
                      "PageRank Centrality"      = "country.PageRank"),
          selected = "joint.risk"
        ),
        hr(),
        h4("Output"),
        radioButtons("outputType", NULL,
          choices = c("Global Network"   = "global",
                      "Regional Network" = "regional"),
          selected = "global"
        ),
        conditionalPanel(
          condition = "input.outputType == 'regional'",
          textInput("focusedCountries",
                    "Focal countries (comma-separated)",
                    value = "USA")
        ),
        hr(),
        actionButton("run", "Run Analysis", class = "btn-primary btn-block")
      ),

      mainPanel(width = 9,
        plotOutput("networkPlot", height = "750px")
      )
    )
  )
)

# ── Server ──
server <- function(input, output, session) {

  results <- eventReactive(input$run, {
    req(input$tradeFile, input$pestFile, input$hostFile)

    # ── 1. Trade data (WTO format) ──
    tradeRaw <- read.csv(input$tradeFile$datapath, stringsAsFactors = FALSE)
    tradeRaw <- tradeRaw[, c("Reporting.Economy", "Partner.Economy", "Year", "Value")]
    colnames(tradeRaw) <- c("ImportCountry", "ExportCountry", "Year", "Value")
    tradeRaw$Value <- as.numeric(tradeRaw$Value)
    years <- length(unique(tradeRaw$Year))

    cropTradeWTO <- tradeRaw %>%
      group_by(ExportCountry, ImportCountry) %>%
      summarise(YearTradeMean = sum(Value, na.rm = TRUE) / years, .groups = "drop") %>%
      filter(YearTradeMean > 0) %>%
      mutate(YearTradeMean = (YearTradeMean / max(YearTradeMean))^(1/4))

    cropTradeWTO <- abbreviate_countries(data.frame(cropTradeWTO), "ExportCountry")
    cropTradeWTO <- abbreviate_countries(cropTradeWTO, "ImportCountry")

    # ── 2. Host distribution (FAOSTAT format) ──
    hostRaw <- read.csv(input$hostFile$datapath, stringsAsFactors = FALSE)
    hostRaw <- hostRaw[, c("Area", "Year", "Value")]
    colnames(hostRaw) <- c("ExportCountry", "Year", "HostArea")
    hostRaw$HostArea <- as.numeric(hostRaw$HostArea)

    host.distribution <- hostRaw %>%
      group_by(ExportCountry) %>%
      summarise(HostArea = mean(HostArea, na.rm = TRUE), .groups = "drop") %>%
      filter(HostArea > 0) %>%
      mutate(HostArea = log10(HostArea + 11/10))

    host.distribution <- abbreviate_countries(data.frame(host.distribution), "ExportCountry")

    # ── 3. Pest distribution ──
    pestRaw <- read.csv(input$pestFile$datapath, stringsAsFactors = FALSE)
    pestRaw <- pestRaw[, c("Region", "Presence", "Extent", "First.record", "USOrder")]
    colnames(pestRaw) <- c("ExportCountry", "Presence", "Extent", "First.record", "USOrder")

    # Remove sub-national entries
    subnations <- pestRaw$ExportCountry[grepl(" - ", pestRaw$ExportCountry)]
    PestDistribution <- pestRaw %>% filter(!ExportCountry %in% subnations)

    PestDistribution <- PestDistribution %>%
      mutate(Extent = ifelse(
        (Extent == "-" | Extent == "Extent not recorded") & Presence == "Present",
        "Present", Extent))
    PestDistribution$ExtentCat <- PestDistribution$Extent

    extent_map <- c("Widespread" = 5, "Native" = 5, "Present" = 4,
                    "Localized" = 3, "Few occurrences" = 2,
                    "Transient under surveillance" = 2,
                    "Transient under eradication" = 1)
    PestDistribution$Extent <- extent_map[PestDistribution$Extent]
    PestDistribution$Extent[is.na(PestDistribution$Extent)] <- 0

    PestDistribution$USOrder <- ifelse(
      !is.na(PestDistribution$USOrder) & PestDistribution$USOrder == "yes", "yes", "no")
    PestDistribution$Biosecurity <- ifelse(PestDistribution$USOrder == "yes", 10, 1)

    PestDistribution <- abbreviate_countries(data.frame(PestDistribution), "ExportCountry")

    # ── 4. Merge everything ──
    CountryNodes <- unique(c(cropTradeWTO$ExportCountry, cropTradeWTO$ImportCountry))

    PestHostdf <- merge(PestDistribution, host.distribution, by = "ExportCountry", all = TRUE)

    analysis.year <- 2024
    TradingCountries <- merge(PestHostdf,
                              data.frame(ExportCountry = CountryNodes),
                              by = "ExportCountry", all.y = TRUE)
    TradingCountries <- TradingCountries %>%
      mutate(First.record = as.numeric(First.record)) %>%
      distinct(ExportCountry, .keep_all = TRUE) %>%
      replace_na(list(Presence = "Unreported",
                      Extent = 0,
                      First.record = analysis.year,
                      ExtentCat = "Unreported",
                      HostArea = log10(11/10),
                      USOrder = "no",
                      Biosecurity = 1)) %>%
      mutate(pest.age = analysis.year + 1 - First.record,
             country.type = ifelse(HostArea == log10(11/10), "Intermediary", "Producer"))

    # ── 5. Build network & matrices ──
    TradeNet <- graph_from_data_frame(d = cropTradeWTO,
                                     vertices = TradingCountries,
                                     directed = TRUE)
    tradeMatrix <- as.matrix(as_adjacency_matrix(TradeNet, attr = "YearTradeMean"))

    cnames <- TradingCountries$ExportCountry
    n <- length(cnames)

    # Pest matrix
    pe <- TradingCountries$Extent
    pestmatrix <- (2^(matrix(pe, n, 1) %*% matrix(1/(pe + 1), 1, n))) / (2^5)
    colnames(pestmatrix) <- rownames(pestmatrix) <- cnames

    # Pest duration matrix
    pd <- TradingCountries$pest.age
    page.matrix <- matrix(pd, n, 1) %*% matrix(rep(1, n), 1, n)
    page.matrix <- page.matrix / max(page.matrix)
    colnames(page.matrix) <- rownames(page.matrix) <- cnames

    # Host matrix
    ho <- TradingCountries$HostArea / max(TradingCountries$HostArea)
    hostmatrix <- matrix(ho, n, 1) %*% matrix(ho, 1, n)
    colnames(hostmatrix) <- rownames(hostmatrix) <- cnames

    # Biosecurity matrix
    bi <- 1 / TradingCountries$Biosecurity
    biomatrix <- matrix(bi, n, 1) %*% matrix(bi, 1, n)
    colnames(biomatrix) <- rownames(biomatrix) <- cnames

    # Gravity model
    geoTradeRisk <- pestmatrix * tradeMatrix * hostmatrix * page.matrix * biomatrix
    geoTradeRisk[geoTradeRisk == 0] <- 0

    PestMoveLinks <- melt(geoTradeRisk) %>%
      rename(ExportCountry = Var1, ImportCountry = Var2, GeoTradeRisk = value) %>%
      filter(GeoTradeRisk > 0)

    pestMoveNet <- graph_from_data_frame(d = PestMoveLinks,
                                        vertices = TradingCountries,
                                        directed = TRUE)
    E(pestMoveNet)$weight <- E(pestMoveNet)$GeoTradeRisk

    # ── 6. Centralities ──
    No <- dim(geoTradeRisk)
    joint.risk <- sapply(1:No[1], function(i) 1 - prod(1 - geoTradeRisk[, i]))

    country.strength <- strength(pestMoveNet, mode = "in",
                                 weights = E(pestMoveNet)$weight)
    trans.weight <- 1.0001 * max(E(pestMoveNet)$weight) - E(pestMoveNet)$weight
    country.betweenness <- igraph::betweenness(pestMoveNet, directed = TRUE,
                                               weights = trans.weight,
                                               normalized = TRUE)
    country.eigenvector <- tryCatch(
      eigen_centrality(pestMoveNet, directed = TRUE,
                       weights = E(pestMoveNet)$weight, scale = TRUE)$vector,
      error = function(e) rep(0, vcount(pestMoveNet))
    )
    country.PageRank <- page_rank(pestMoveNet, directed = TRUE,
                                  weights = E(pestMoveNet)$weight)$vector

    cc <- data.frame(joint.risk, country.strength, country.betweenness,
                     country.eigenvector, country.PageRank)
    cc <- cc %>% mutate(across(everything(), ~ . / max(., na.rm = TRUE)))

    sel <- input$centrality
    if (length(sel) == 0) sel <- "joint.risk"
    PTRI <- rowSums(cc[, sel, drop = FALSE])
    PTRI <- PTRI / max(PTRI, na.rm = TRUE)

    list(pestMoveNet = pestMoveNet,
         TradingCountries = TradingCountries,
         PestMoveLinks = PestMoveLinks,
         PTRI = PTRI,
         cc = cc)
  })

  output$networkPlot <- renderPlot({
    res <- results()
    net <- res$pestMoveNet

    V(net)$ExtentCat <- factor(V(net)$ExtentCat,
      levels = c("Unreported", "Eradicated", "No longer present",
                 "Intercepted only", "Transient under eradication",
                 "Transient under surveillance", "Few occurrences",
                 "Exporting interceptions", "Localized", "Present", "Widespread"))

    fill_vals <- c("Unreported" = "#FCB519FF",
                   "Transient under eradication" = "#F98C0AFF",
                   "Transient under surveillance" = "#F98C0AFF",
                   "Few occurrences" = "#E35932FF",
                   "Localized" = "#BB3754FF",
                   "Present" = "#89226AFF",
                   "Widespread" = "#56106EFF")

    if (input$outputType == "regional") {
      fc <- trimws(unlist(strsplit(input$focusedCountries, ",")))
      flinks <- res$PestMoveLinks %>% filter(ImportCountry %in% fc)
      fnodes <- res$TradingCountries %>%
        filter(ExportCountry %in% unique(c(flinks$ExportCountry, flinks$ImportCountry)))
      fnet <- graph_from_data_frame(d = flinks, vertices = fnodes, directed = TRUE)
      V(fnet)$ExtentCat <- factor(V(fnet)$ExtentCat,
        levels = c("Unreported", "Eradicated", "No longer present",
                   "Intercepted only", "Transient under eradication",
                   "Transient under surveillance", "Few occurrences",
                   "Exporting interceptions", "Localized", "Present", "Widespread"))

      ggraph(fnet, layout = "stress") +
        geom_edge_link(aes(edge_width = GeoTradeRisk,
                           start_cap = circle(.3), end_cap = circle(.3)),
                       edge_colour = "darkgrey",
                       arrow = arrow(angle = 30, length = unit(0.15, "inches"))) +
        geom_node_point(aes(fill = ExtentCat, color = ExtentCat,
                            size = HostArea), shape = 21) +
        geom_node_text(aes(label = name), size = 5, repel = TRUE) +
        scale_edge_width_continuous(range = c(0.3, 2.5)) +
        scale_size_continuous(range = c(2, 12)) +
        scale_fill_manual(values = fill_vals, drop = FALSE, na.value = "grey50") +
        scale_color_manual(values = fill_vals, drop = FALSE, na.value = "grey50") +
        coord_fixed() + theme_graph() +
        theme(legend.position = "right", legend.key.size = unit(1, "cm")) +
        guides(
          fill = guide_legend(title = "Reported distribution",
                              override.aes = list(size = 10)),
          color = guide_legend(title = "Reported distribution",
                               override.aes = list(size = 10)),
          size = guide_legend(title = "Log-scaled cropland ha"),
          edge_width = guide_legend(title = "Pest trade\nmovement potential")
        )
    } else {
      ggraph(net, layout = "centrality", cent = res$PTRI^(1/2)) +
        geom_edge_link(aes(edge_width = GeoTradeRisk,
                           start_cap = circle(.3), end_cap = circle(.3)),
                       edge_colour = "darkgrey",
                       arrow = arrow(angle = 30, length = unit(0.15, "inches"))) +
        geom_node_point(aes(fill = ExtentCat, color = ExtentCat,
                            size = exp(HostArea), shape = country.type)) +
        geom_node_text(aes(label = name), size = 5, repel = TRUE) +
        scale_shape_manual(values = c("Intermediary" = 23, "Producer" = 21)) +
        scale_edge_width_continuous(range = c(0.05, 2)) +
        scale_size_continuous(range = c(2, 15)) +
        scale_fill_manual(values = fill_vals, drop = FALSE, na.value = "grey50") +
        scale_color_manual(values = fill_vals, drop = FALSE, na.value = "grey50") +
        coord_fixed() + theme_graph() +
        theme(legend.position = "right", legend.key.size = unit(1, "cm")) +
        guides(
          fill = guide_legend(title = "Reported distribution",
                              override.aes = list(size = 10)),
          color = guide_legend(title = "Reported distribution",
                               override.aes = list(size = 10)),
          shape = guide_legend(title = "Country category",
                               override.aes = list(size = 10)),
          size = guide_legend(title = "Log-scaled cropland ha"),
          edge_width = guide_legend(title = "Pest trade\nmovement potential")
        )
    }
  })
}

shinyApp(ui, server)
