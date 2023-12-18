landingPageText1 <- function() {
  mainPanel(
    h1("Hva er Hotspots-Appen?"),
    p("Ei heilskapleg kartlegging av naturverdier er avgjørende for en kunnskapsbasert naturforvaltning. Avveininger mellom 
      naturverdier og andre samfunnshensyn kan ikkje gjerast verken på internasjonal, nasjonal, regional eller kommunal skala
      utan at utbreiing av naturverdi er kjent. Dette krever kjennskap til utbreiing av artsmangfold på relevante romlige skalaer. "),
    p("Derfor utvikler vi Hotspots-prosjekt. Prosjekt bruker avansert formatering og modellering av apent artsdata og miljøvariabler
      til å skaper data på biomangfold hotspots i Norge, og tilgjengeliggjør transparent og troverdig utbreiingsdata for 
      miljøforvaltner i Norge. Data er tilgjengelig til sluttbrukere på en rekke skala og artsgrupper, som kan implementeres i 
      kartleggingsprosesser og andre forvaltnings sluttprodukter."),
    p("Dette Shiny app er et eksempel på en mulig implementering av denne pipeline. Det gir et kort oversikt av muligheter av 
      Hotspots prosjekt sitt data og resultater, og har hensikt å utdanne potensielle sluttbruker om sterke og grenser av 
      utbreiingsdata i Norge.")
  )
}

landingPageText2 <- function() {
  mainPanel(
    p("For en kort demonstrasjon av appen, se på video", strong(a(" i lenken her",
    href = "https://www.loom.com/share/20f951e43ab04ec389853e31198006f4",
    target = "_blank")),"(bare engelsk)."),
    p("Alle kode som ble brukt å produsere data i appen finnes ", 
      strong(a("på GitHub", href = "https://github.com/gjearevoll/BioDivMapping", target = "_blank")), "."),
    p("En omfattende guide til egen kjøring av arbeidsflyt finnes i side-tab under Instruksjoner (også for tiden bare på engelsk).")
  )
}

speciesIntensityText <- function() {
  mainPanel(
    p("Intensitet referer til sånnsynligheten for en artsforekomst. Når vi refererer til intensitet, refererer vi til 
      intensitetsfunksjonen til den underliggende punktprosessmodellen."),
    p("Sagt mindre enkelt, det den egentlig beskriver er tettheten av arter på et hvilket som helst sted på kartet av 
    interesse. Dette er modellert som en funksjon av miljøvariabler og et “Gaussian random field”, som brukes til å gjøre 
    rede for eventuelle umålte kovariater og potensiell romlig autokorrelasjon. Jo høyere verdien av intensitetsfunksjonen 
    er på et eller annet tidspunkt på kartet, desto mer sannsynlig er arten å finne der,"),
    p("Det som er ",strong("viktig å merke seg")," er at intensiteten til en enkelt art i kartleggingsverktøyet vårt for øyeblikket",
    strong("kun er i forhold til den arten"),". Det vil si at en artsintensitet på 0,5 på en ganske vanlig art og på 0,5 på en svært sjelden art 
    ikke innebærer samme sannsynlighet for forekomst.")
  )
}



environmentalCovariateText <- function() {
  mainPanel(
    p("Datamodellen funnet i R Shiny-appen omfatter for tiden fem kovariater fra en rekke forskjellige kilder.
     Alle data ble standardisert til et 1 km rutenett, med planer for høyere oppløsningsinterpolasjoner i fremtidige modeller."),
    p("For en fullstendig liste over datakilder (inkludert sitater), sjekk ", 
      strong(a("GitHub repo-mappen", href = "https://github.com/gjearevoll/BioDivMapping/tree/main/data/external/environmentalCovariates", 
               target = "_blank")))
)
}