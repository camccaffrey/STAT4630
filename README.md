# STAT4630

Group 13's Repository for STAT 4630: Statistical Machine Learning

------------------------------------------------------------------------

## Table of Contents

-   [Important Links](#important-links)

-   [File Descriptions](#file-descriptions)

-   [Setup Instructions](#setup-instructions)

    -   [Prerequisites](#prerequisites)

    -   [Clone Repository](#clone-repository)

    -   [Creating Local Changes](#creating-local-changes)

    -   [Pushing to GitHub](#pushing-to-github)

------------------------------------------------------------------------

## Important Links

-   [Google Drive Project Folder](https://drive.google.com/drive/folders/188y8UtK8QoEAMdiTjjkhtXyP0BsFcb4z?usp=drive_link)

-   [Schedule & Contact Info](https://docs.google.com/spreadsheets/d/12F8hKwLkV5tnXcsUE9qrD-ibVNVBkb0jPGl12eB_yXY/edit?usp=sharing)

-   [Kaggle Dataset](https://www.kaggle.com/datasets/mvieira101/global-cost-of-living)

-   [Data Dictionary](https://docs.google.com/spreadsheets/d/1kLACSfz_Ong4xYVKJl5fUlyfeKf7uE2nLWon1aV8aCE/edit?usp=sharing)

------------------------------------------------------------------------

## File Descriptions

| File Name      | Description                                                                       |
|----------------|-----------------------------------------------------------------------------------|
| .gitignore     | Tells GIT which files not to track within the project directory.                  |
| License        | The MIT license gives express permission for users to reuse code for any purpose. |
| README.md      | The markdown file you are currently reading.                                      |
| STAT4630.Rproj | Contains basic configuration options for the project.                             |

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

### Creating Local Changes

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
