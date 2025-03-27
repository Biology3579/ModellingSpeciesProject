# Modelling Species Distributions Project

## Project Overview
This research investigates the distribution of Phoenix dactylifera (Date Palm) and Rhynchophorus ferrugineus (Red Palm Weevil) across North Africa, the Middle East, and southern Europe. The study uses Species Distribution Models (SDMs) to predict how climate change might impact the spatial relationships between these species.

All the necessary code for the analysis can be found in the [modelling_species_script.qmd](modelling_species_script.qmd) file. The Quarto document contains the data preprocessing, plots and modelling performed during the project.

## Finished Reports

In my repo you will find a completed report - an interactive HTML version - summarising the analysis, results, and conclusions:

## Running the Analysis
If you would like to run the analysis and reproduce the results yourself, follow these steps to set up your local environment and execute the necessary code.

### Prerequisites
1. Install Git on your system.
2. Install R and RStudio.

### Installation
**1. Clone the repository:**

First, clone the repository to your local machine: 
1. Open Git Bash or the terminal application
2. Navigate to where you want to clone the repo to _(e.g. Documents)_:
```bash
cd ~/Documents
```
3. Clone the repo to the specificed location:
```bash
git clone https://github.com/Biology3579/ModellingSpeciesProject.git
```
This will create a local copy of the repository on your machine.

4. After cloning, navigate to the project directory:
```bash
cd ModellingSpeciesProject
```

**2. Open PenguinAnalysis.Rmd**

Find the `modelling_species_script.qmd` file in the main repo.
This is the main file for the analysis.

**3. Restore the R environment**

Within the file, the *Set Up* section, you will see the following command to command in R to restore the project's environment:
```r
renv::restore()
```
*This command will:*

 - Install the required R packages as specified in the renv.lock file.
 - Set up the project environment to match the one used when the project was initially created, ensuring that all dependencies are correctly installed.

*Note: this requires you to have renv previously installed.*
If you don't have renv already installed, install it by running the following command in your R console:
```r
install.packages("renv")
```
