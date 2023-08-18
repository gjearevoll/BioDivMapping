landingPageText1 <- function() {
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
      and from there plot biodiversity hotspots for the purpose of setting conservation priorities.")
  )
}

landingPageText2 <- function() {
  mainPanel(
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
      as data source.")
  )
}

landingPageText3<- function() {
  mainPanel(
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
    could be done previously.")
  )
}

landingPageText4 <- function() {
  mainPanel(
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

instructionsText1 <- function() {
  mainPanel(
    h2("Can I create my own version of this?"),
  p("Absolutely! Below we've laid out a set of instructions so that you can create your own version of the 
  pipeline for your own species or taxa. Just remember that this tool is still only in the alpha stage,
    so best not to make any important conservation decisions using the tool just yet!"),
  p("Prerequisite knowledge for running this pipeline is not too intense. You’ll need to have a reasonable understanding of R, 
    and you’ll need to know a bit about GitHub and GBIF as well. We've attempted to answer questions about GBIF 
    and GitHub in the FAQ section below, let us know if there's something more we should add!")
  )
} 

instructionsText2 <- function() {
  mainPanel(
    p("There are a few things you need to define before you get started on running the pipeline."),
    p(),
    p("1. Which species you want to model"),
    p("2. Which region you want to look at"),
    p("3. Which datasets you want to use"),
    p(),
    p("Let’s go through them one by one."),
    h3("Species"),
    p("The species information should be inputted into the file marked focalSpecies.csv in data/external 
    there’s a template you can use in there already so just follow the case and structure there). The file requires:"),
    p(),
    p("- The species’ scientific name (make sure it’s the accepted scientific name on GBIF"),
    p("- The taxonomic group the species belongs to"),
    p("- Whether or not the species is red-listed"),
    p("- The species common name (not mandatory)"),
    p("- Their red-list status (also not mandatory)"),
    p(),
    p("Also note that the taxonomic group does not have to be at a specific level, it can be as simple as ‘animals’ 
    or as detailed as ‘members of the genus Arnica’."),
    h3("Region"),
    p("At the moment we have four different options available when it comes to regions. All of them are currently 
    on a Norwegian level (this will soon be expanded)."),
    p(),
    p("- You can define a rectangular box using latitudinal and longitudinal points as your vertices"),
    p("- You can select a Norwegian county"),
    p("- You can select a Norwegian municipality"),
    p("- Or you can simply select the whole of Norway."),
    p(),
    p("Keep in mind of course that the larger the region you select, the longer the model will take to run. 
    Two and four-number codes for Norwegian municipalities can be found ",
      a("at this link.", href = "https://kartverket.no/til-lands/kommunereform/tekniske-endringer-ved-sammenslaing-og-grensejustering/komendr2020")),
    h3("Datasets"),
    p("You need to define which datasets you’d like to use in the metadataSummary.csv file (there’s a template 
    for this too in that data/external folder). GBIF has a range of datasets for each species and you’ll need the 
    following information about those datasets:"),
      p(),
      p("- The dataset name"),
    p("- The dataset code as listed in GBIF"),
    p("- The type of data it is (PA - presence/absence, or PO - presence only)"),
    p("- Whether or not the dataset should be activated (TRUE/FALSE)"),
    p(),
    p("If you’re not too keen on sifting through every dataset right now, I’d recommend you simply use the Norwegian 
    Species Observation Service, iNaturalist Research Grade Observations, and the ANO (Arealrepresentativ naturovervåking) 
    datasets. The first two of these you can find in the metadataSummary.csv file, the ANO dataset can be turned on within 
    the master script.")
    
  )
} 

instructionsText3 <- function() {
  mainPanel(
    p("Once you’ve set the above up you’ll need to create a branch off from the main GitHub repo so you can 
    run your own code locally. You should then open the script masterScript.R found at the head of the 
    repository, and start running it. This is the only script that you should need to change any of the code 
    on, though you’re of course welcome to open other scripts and have a poke around inside!"),
    p(""),
    p("The last thing you need to do before you get started is to make sure you’ve got all relevant R packages 
    installed. There is a utility script at the start of the master script which helps out here, but of course 
    R isn’t always known for willingly installing packages. This utility script also installs INLA in order to 
    run the models - ", strong(a("more on that here.", href = "https://www.r-inla.org"))),
    p(""),
    p("The master script runs five different scripts, one after the other. Here’s a brief overview of the process 
    that occurs when you’re running the master script (a more detailed description can be found in the main 
    README file)."),
    p(""),
    p(strong("speciesImport.R")," - after defining our species, datasets and region, we import all species data, 
    as well as the species metadata and images of species direct from iNaturalist."),
    p(strong("environmentalImport.R")," - we then import environmental covariates for the region we’ve defined."),
    p(strong("speciesDataProcessing.R"), " - we dig into the metadata of our datasets to find out which ones might
    have extra information, and go right to the source to add that extra information back into our datasets."),
    p(strong("speciesModelRuns.R"), " - we construct and run our integrated species distribution models. Note that 
    this script is the one likely to take the longest."),
    p(strong("biodiveristyMetricEstimation.R"), " - we combine the results from our models to give us estimates of 
    total taxa biodiversity for all species and for only red-listed species."),
    p(""),
    p("When you start the pipeline, a ‘run’ folder is automatically created. It’s named after the date, and can be 
    found in data/run_YYYY-MM-DD. This will contain all species input and output data, images, and the 
    speciesMetadata.html file which gives you information about the species and datasets that were used in the model 
    run.")
    
    
  )
} 

instructionsText4 <- function() {
  mainPanel(
    p("To start your own app, you’ll need to open either the ui.R or server.R script found in the 
      visualisations/hotspotMap folder. From there you can hit ‘Run app’ in the top right corner of the script panel."),
    img(src = "runAppImage.png", width = 600)

  )
} 

faqText1 <- function() {
  mainPanel(
    h1("Frequently Asked Questions"),
    p("We've included a selection of FAQs below. If there's anything we've missed, get in touch!")
  )
} 

faqText2 <- function() {
  mainPanel(
    p("An integrated distribution model is a type of species distribution model using data from 
      a variety of different sources, with the aim of describing the differences in how each dataset 
      was generated – thereby retaining the strengths and personalities of each. This differs from 
      simple data pooling, where all the data are assumed to come from a common observation process, 
      ignoring any differences between them. ")
  )
} 

faqText3 <- function() {
  mainPanel(
    p("We integrate presence-only data and presence absence by assuming a state-space point-process 
      model, which was then fit in a hierarchical Bayesian framework using the integrated nested Laplace 
      approximation estimation method. This state-space model can be thought of as a combination of a 
      process model for the true underlying distribution of the species, and separate observation models 
      for each species, capturing how each of the datasets were generated.")
  )
} 

faqText4 <- function() {
  mainPanel(
    p("Once you’ve found the taxa you’re looking for by searching through GBIF, you should arrive
      on a page with an address that looks like something like this - gbif.org/species/xxxxxxxx. That
      number out the back is your taxa number.")
  )
} 

faqText5 <- function() {
  mainPanel(
    p("Once you’ve found the dataset you’re looking for by searching through GBIF, you should
      arrive on a page with an address that looks like something like this - gbif.org/dataset/xxxxxxxx. 
      That series of numbers and letters out the back is a UUID code, and it’s the GBIF dataset code.")
  )
} 

faqText6 <- function() {
  mainPanel(
    p("You’ll need to have GitHub installed on your computer, more on how to do that here. 
      If you’re using RStudio, you’ll want to turn on the GitHub functionality as well, which 
      you can do here. You’ll then want to start a new project locally, connecting it to 
      BioDivMapping using the url. Once you’ve done that, you’ll want to start a new branch 
      in GitHub, which you can do by clicking ‘main’ on the repo home page and typing in your 
      new branch’s name. Then you can click ‘pull’in RStudio and start working on your own 
      version of the tool!")
  )
} 

faqText7 <- function() {
  mainPanel(
    p("Thanks to the inherently messy nature of taxonomy, the scientific name you use for a 
      species might not match up with the scientific name that GBIF uses for a certain species. 
      Luckily, GBIF will most likely have your species’ name floating around, and attached to the 
      right species - but it will be listed as a synonym, and not the accepted scientific name. 
      Make sure you use the accepted scientific name in focalSpecies.csv, or the pipeline will 
      have trouble.")
  )
} 

faqText8 <- function() {
  mainPanel(
    p("ANy decent spatial model needs to have some method of accounting for spatial autocorrelation - 
    the phenomena whereby two regions have similar species occurrences not because of any similarities 
    in environmental variables, but just because they’re close together."),
    p("The intensity function in our INLA model uses a Gaussian random field to account for potential 
      spatial autocorrelation, as well as other unmeasured covariates in the model. For computational 
      efficiency, INLA uses the stochastic partial differential equation (SPDE) approach to model the 
      random field in the form of a zero-mean  Gaussian markov random field described by a Matérn 
      covariance structure. The INLA-SPDE approach evaluates the continuous random field as a discrete 
      random process through some spatial mesh defined by triangulating the study domain. Constructing 
      this mesh is made easy using the R-INLA package with the inla.mesh.2d() function.")
  )
}

faqText9 <- function() {
  mainPanel(
    p("Building the mesh is typically a matter of trial and error. Building a mesh that is too coarse
    will result in poor estimation from the model, and building a mesh that is too fine will result in
    a model which is computationally demanding."),
    p("The function contains numerous parameters which can be customized by the user (see ?INLA::inla.mesh.2d 
    in R to get a detailed explanation of these arguments). The main ones are max.edge, the cutoff, 
    and the offset."),
    p("‘max.edge’ needs to be a vector with two elements, the first of which needs to be set between 1/3rd - 
    1/10th of the range (the radius around an observation for which autocorrelation drops below 0.1), and the 
    second needs to be large enough such that no boundary effect (variance greater at the boundary) occurs.  
    The ‘cutoff’ argument of the function typically needs to be around 1/5th of the size of the first max.edge 
    value."),
    p("We’ve provided a test function in the master script so that you can play around a bit with different 
    mesh parameters. If the function takes too long to run, there’s a good chance you’ve made the mesh too 
    fine and need to increase your parameters. Here’s an example of a reasonable mesh for Trøndelag."),
    img(src = "exampleMesh.png")
  )
} 

contactText <- function() {
  mainPanel(
    h2("If you have any questions you'd like to ask about the project, feel free to get in touch!"),
    p("Sam Wenaas Perrin, PhD."),
    p("sam.perrin@ntnu.no"),
    p(a("Sam's profile at Ecology for the Masses", href = "https://ecologyforthemasses.com/sam-perrin-2/")),
    br(),
    p("Philip Stanley Mostert"),
    p("philip.s.mostert@ntnu.no")
  )
}