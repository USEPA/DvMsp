# DvMsp

### A Supplementary R Package to "A comparison of design-based and model-based approaches for spatial data"

##### Michael Dumelle<sup>1</sup>, Matthew Higham<sup>2</sup>, Jay Ver Hoef<sup>3</sup>, Anthony R. Olsen<sup>1</sup>, Lisa Madsen<sup>4</sup>

##### <sup>1</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>2</sup>St. Lawrence University Department of Math, Computer Science, and Statistics, Canton, NY, USA
##### <sup>3</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA
##### <sup>4</sup>Oregon State University Department of Statistics, Corvallis, OR, USA
##### For correspondence, please email Michael Dumelle at Dumelle.Michael@epa.gov

### Abstract
  This is the abstract.

### Package Overview

This supplementary R package contains all files used in the creation of this document. Next we discuss how to use the package to access these files.

### Installation

To install the supplementary R package, run
```r
install.packages("remotes") # if you don't already have remotes installed
remotes::install_github("michaeldumelle/DvMsp", ref = "main", dependencies = TRUE)
```

The package must be installed to view any of the files we discuss throughout this `README` on your machine. This package does not have to be installed to view any of these files if you want to look at them interactively using this GitHub repository.

### Scripts

After installing the package, the script used to perform the simulations presented in the manuscript is available in the "scripts" folder located on your machine at the file path found by running
```r
system.file("scripts", package = "DvMsp")
```

### Output

Summaries of the simulation output (all 36 trials) are available in the "output" folder located on your machine at the file path found by running
```r
system.file("output", package = "DvMsp")
```

Raw simulation output is not included when installing the supplementary package but is available [here](https://github.com/michaeldumelle/DvMsp/tree/main/inst/output/simulation_raw).

### Manuscript

The manuscript files are available in the "manuscript" folder located on your machine at the file path found by running
```r
system.file("manuscript", package = "DvMsp")
```

To view the manuscript, open `manuscript.pdf`. To view the supplementary material, open `supplementary.pdf`.

### Vignette

To view the vignette, run
```r
vignette("simulation-vignette", "DvMsp")
```
