# STAT4630

Group 13's Repository for STAT 4630: Statistical Machine Learning

------------------------------------------------------------------------

## Table of Contents

-   [Important Links](#important-links)

-   [Files & Directories](#files--directories)

-   [Setup Instructions](#setup-instructions)

    -   [Prerequisites](#prerequisites)

    -   [Clone Repository](#clone-repository)

    -   [Loading Packages](#loading-packages) 

    -   [Creating Local Changes](#creating-local-changes)

    -   [Pushing to GitHub](#pushing-to-github)

------------------------------------------------------------------------

## Important Links

-   [Google Drive Project Folder](https://drive.google.com/drive/folders/188y8UtK8QoEAMdiTjjkhtXyP0BsFcb4z)

-   [Schedule & Contact Info](https://docs.google.com/spreadsheets/d/12F8hKwLkV5tnXcsUE9qrD-ibVNVBkb0jPGl12eB_yXY)

-   [Kaggle Dataset](https://www.kaggle.com/datasets/mvieira101/global-cost-of-living)

-   [Data Dictionary](https://docs.google.com/spreadsheets/d/1kLACSfz_Ong4xYVKJl5fUlyfeKf7uE2nLWon1aV8aCE)

------------------------------------------------------------------------

## Files & Directories

### Scripts
| Name                                             | Description                                                                         |
|--------------------------------------------------|-------------------------------------------------------------------------------------|
| [`config.R`](Scripts/config.R)                   | Global variables that can be loaded with `source(file.path("Scripts", "config.R"))` |
| [`data_cleaning.R`](Scripts/data_cleaning.R)     | Splits and transforms data. Outputs: `train.rds`, `test.rds`, etc.                  |
| [`eda.R`](Scripts/eda.R)                         | Based off of Google Colab notebook used for Milestone 2 EDA                         |
| [`themes.R`](Scripts/themes.R)                   | Stores custom themes and related variables + functions                              |
| [`‎Milestone3EDA.Rmd`](Scripts/Milestone3EDA.Rmd) | Ada's EDA for Milestone 3                                                           |

### Data
| Name                                                  | Description                                                                                            |
|-------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| [`cost-of-living_v2.csv`](Data/cost-of-living_v2.csv) | [Kaggle Dataset](https://www.kaggle.com/datasets/mvieira101/global-cost-of-living)                     |
| [`data-dict.csv`](Data/data-dict.csv)                 | [Data Dictionary](https://docs.google.com/spreadsheets/d/1kLACSfz_Ong4xYVKJl5fUlyfeKf7uE2nLWon1aV8aCE) |
| [`train.rds`](Data/train.rds)                         | Transformed training data, created with `data_cleaning.R`                                              |
| [`test.rds`](Data/test.rds)                           | Transformed testing data, created with `data_cleaning.R`                                               |
| [`train-long.rds`](Data/train-long.rds)               | Long version of `train.rds`                                                                            |
| [`testlong.rds`](Data/test-long.rds)                  | Long version of `testing.rds`                                                                          |

### Project Root
| Name               | Description                                                                              |
|--------------------|------------------------------------------------------------------------------------------|
| `Data`(./Data/)       | Directory containing `.csv` and `.rds` data files                                        |
| `Scripts`(./Scripts/) | Directory containing `.R` and `.Rmd`scripts                                              |
| `renv`             | Directory for our "reproducible environment" (see [Loading Packages](#loading-packages)) |
| `.Rprofile`        | Configuration file for our R environment                                                 |
| `.gitignore`       | Tells GIT which files not to track within the project directory                          |
| `LICENSE`          | The MIT license gives express permission for users to reuse code for any purpose         |
| `README.md`        | The markdown file you are currently reading                                              |
| `STAT4630.Rproj`   | Contains basic configuration options for the project                                     |
| `renv.lock`        | Records a list of the dependencies (and versions) used in our project                    |

------------------------------------------------------------------------

## Setup Instructions

For more detailed instructions, follow [this tutorial](https://happygitwithr.com/rstudio-git-github.html).

### Prerequisites

-   Create a free [GitHub account](https://github.com/join).

-   Install/Update R and Rstudio.

-   Install [Git](https://git-scm.com/downloads).

### Clone Repository

In Rstudio, create a new project: *File \> New Project \> Version Control \> Git*.

-   Enter <https://github.com/camccaffrey/STAT4630> for "Repository URL."

-   Notice where the project will be saved under "Create project as subdirectory of."

-   It will be easiest if you leave "Project directory name" unchanged.

### Loading Packages

The [renv](https://posit.co/blog/renv-project-environments-for-r/) package is a dependency manager that makes R projects more isoloated, portable, and reproducible. Once you've cloned the repository, simply run `renv::restore()` to install all necessary packages (you may need to run `renv::activate()` beforehand). To update the list of dependencies, just run `renv:snapshot()`; this will capture all libraries used inside the project. If, for whatever reason, you need to exit this environment, navigate to *Project (STAT 4630)* > *Close Project*.

If you run into issues with `renv`, try using `load_requirements()` from `Scripts/config.R` instead.

### Creating Local Changes

*Note: to use version control, you must be inside of an R project. You can open the project by locating the `STAT4630.Rproj` file.*

#### Pulling

Prior to making any local changes, navigate to *Version Control (GIT)* \> *Pull Branches*. "Pulling" will ensure that your local version is up-to-date with the repository on GitHub.

#### Saving

To make changes, edit your files as you normally would, remembering to save frequently. However, know that "saving" *will only update the code in your local files on your computer's file system*.

#### Committing

To actually make updates to the version control system, you must "commit" your changes. When you commit code, you are creating a snapshot of your project at a specific point in time, along with a commit message that describes what changes were made. Commits provide a history of changes, making it possible to revert to previous states of your code, track who made what changes, and collaborate with others seamlessly.

To make a commit, navigate to *Version Control (GIT)* \> *Commit*.

1.  In the top left "Staged" box, select the changed files you would like to include in your commit.
    -   At the bottom, you will see changes for the selected file. Additions will appear green and deletions will appear red.
2.  Type a message in "Commit message", such as "Updated README" or "Added data cleaning script."
3.  Click "Commit."

### Pushing to GitHub

"Pushing" allows you to share your committed changes to a remote location; in this case, our [GitHub repository](https://github.com/camccaffrey/STAT4630). When you're ready, navigate to *Version Control (GIT)* \> *Push Branch*.
