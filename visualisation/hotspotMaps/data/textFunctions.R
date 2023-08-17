landingPageText <- function() {
  mainPanel(
    h1("What is the Open Data Biodiversity Mapper?"),
    p("Over the last decade, open data has exploded, and biologists from every part of the globe are reaping the benefits. 
      Data that once would have been hidden in a faculty corner office’s filing cabinet can now easily be standardised 
      and uploaded to databases like the Global Biodiversity Information Facility (GBIF). It’s a process which has opened 
      up a myriad of new possibilities for ecological modellers and statisticians."),
    p("But with such copious amounts of data come obvious challenges. Though every scientist that uploads data to GBIF 
      standardises their data, they’re still collecting said data in vastly different ways. And while scientists, natural 
      resource makers and environmental policy makers worldwide can benefit from said data, not all of these groups have the 
      necessary skills to download, process, integrate and then analyse this data."),
    p("This is why we’ve developed the Open Data Biodiversity Mapper. It allows scientists and natural resource managers in 
      Norway (and soon the rest of the world) to mobilise data at a variety of scales to map individual species occurrences, 
      and from there plot biodiversity hotspots for the purpose of setting conservation priorities."),
    h3("How does it work?"),
    p("The tool is currently an R Shiny app, which relies on a data pipeline which draws down data from open data sources, 
    currently GBIF and Norwegian database Arearepresentativ overvakning (ANO). A user can define their own taxa or species
    of choice depending on their need. Because a bit of detail can be lost during the standardisation to GBIF, the pipeline
    also digs into the source files to add that detail back in. What the user is left with is a list of datasets containing
    one or more of the species that they’re studying."),
    p("This is combined with a series of relevant environmental covariates, standardised to a 1km grid, with plans for higher 
      resolution in future models. The environmental data was sourced from a range of different databases, more detail on 
      which can be found in the ",a("GitHub repository.", 
                                    href = "https://github.com/gjearevoll/BioDivMapping/tree/main/data/external/environmentalCovariates")),
    p("This is then processed through an",
      strong("integrated species distribution model (ISDM)"), "to give a series of species intensity maps."),
    p("These intensity maps can then be combined to show biodiversity hotspots within a region, for all species involved 
      and for threatened species (those on the Norwegian Red List). The tool also allows the user to view the data behind 
      the estimations on a species basis. This can be separated into data type (presence-absence or presence-only), as well 
      as data source."),
    h3("What's an Integrated Species Distribution Model?"),
    p("If you’re going to use vastly different datasets, you have two basic choices. You can stack them all together, 
    thereby losing a lot of valuable detail and reducing the accuracy of the end product. Or you can find a way to integrate
    all the data types together, which was until recently a waking nightmare for the statistical ecologist (and most likely
    their computer’s CPU). As you may have guessed, the latter option is the whole point of an ISDM."),
    p("An ISDM assigns an observation model and statistical description of the data collection protocol to each dataset, 
    then connects them all to a common process model. This allows us to make the most of the individual quirks and 
    personalities present within each dataset, yet model the data together in a single statistical framework with shared 
    parameters across each."),
    p("Evidence suggests that these models outperform single-use datasets in almost all case scenarios. Combining high 
    quality structured data with the abundant citizen science data available allows us to reduce the effect of observer 
    biases that are typically found in the latter, improves the spatial and temporal scale of the study, and increases the 
    precision of model estimates. More so, the tools to help ecologists construct these models (for example: INLA, inlabru, 
    PointedSDMs) have become more prevalent and user-friendly, allowing us to build these models more conveniently than what 
    could be done previously."),
    h3("Who is the tool designed for?"),
    p("While the current version is a proof of concept that serves as a springboard for a larger project, the Biodiversity 
    Mapper is intended for end users involved in policy decisions which require knowledge of conservation priorities. 
    This could include:"),
    p("- Developers looking to assess sites of expansion of infrastructure"),
    p("- Government organisations creating new protected areas"),
    p("- Conservationists carrying out habitat restoration work"),
    p("- Field workers looking to assess regions which have historically been under surveyed"),
    h3("Future development"),
    p("While the open source code can be used to run the Biodiversity Mapping pipeline at small regional scales, we are 
      currently scaling the tool in order to produce maps for the whole of Norway. We are also focussing on adding more 
      tangible conservation visualisations, such as replacing species intensity with probability of occurrence. We are also 
      planning on comparing model accuracy to the expectations of domain experts to see whether their experience with 
      specific species lines up with the predictions produced by our species intensity models."),
    p("Lastly, we are planning on automating the data pipeline using the targets package in order to ensure we are always 
      using the most up-to-date datasets without constant manual updates. We will also be integrating temporal data into the 
      pipeline to reflect the regional changes that have and will affect species distribution.")
  )
}


speciesIntensityText <- function() {
  mainPanel(
    p("Put simply, intensity here refers to the probability of a species occurrence. When we refer to intensity, 
    we are referring to the intensity function of the underlying point-process model."),
    p("Put less simply, what it really describes is the density of species at any location across the map of interest. 
    This is modelled as a function of environmental covariates and a Gaussian random field, which is used to 
      account for any unmeasured covariates and potential spatial autocorrelation. The higher the value of the intensity 
      function at some point on the map, the more probable the species is to be found there."),
    p("What is ", strong("important to note"), "is that a single species' intensity in our mapping tool is currently ",
      strong("only relative to that species."), "That is to say that a species intensity of 0.5 on a fairly common species
      and of 0.5 on a very rare species do not imply the same likelihood of occurrence.")

  )
}