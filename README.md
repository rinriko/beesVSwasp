Homework2

# Table of Contents
1. [Demo](#demo)
2. [Install Rstudio](#install-rstudio)
3. [Download project](#download-project)
4. [Set-up project](#set-up-project)
5. [Run project](#run-project)
6. [Dataset](#dataset)
7. [How to use](#howtouse)

## <a name="demo"></a> Demo
Go to this [link](https://rinriko.shinyapps.io/homework2/).

## <a name="install-rstudio"></a> Install Rstudio
1. Go to this [link](https://rstudio.com/products/rstudio/download/).
2. Download Rstudio 1.3.1093 for your Operating System (e.g., Mac OS X, Windows)
3. Install Rstudio 1.3.1093 by following this [link](https://courses.edx.org/courses/UTAustinX/UT.7.01x/3T2014/56c5437b88fa43cf828bff5371c6a924/).

## <a name="download-project"></a> Download project
### First method (clone repository)
1. Open Command Prompt or Terminal. [More information](https://www.groovypost.com/howto/open-command-window-terminal-window-specific-folder-windows-mac-linux/).
2. Change the current working directory to the location where you want to download the project.
3. Clone repository:

```sh
$ git clone https://github.com/rinriko/Homework2.git
```
Note: If Git is not installed, please complete the following [instructions](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git). Or follow the [second method](#second-method) instructions.

### <a name="second-method"></a> Second method (download project)
1. Go to https://github.com/rinriko/Homework2.git.
2. Above the list of files, click &#8595; Code.
3. Click "Download ZIP".
4. Save the zip file into a convenient location on your PC and start working on it.
5. Extract the ZIP file.


## <a name="set-up-project"></a> Set up project
### From the first method
1. Open [Rstudio](https://rstudio.com/).
2. Use the Console at the bottom left and type:
```sh
> install.packages(c('shiny','bs4Dash','DT','shinyWidgets','shinyjs','GGally','ggplot2','ggfortify'))
```
## <a name="run-project"></a> Run project
1. Open this project via [Rstudio](https://rstudio.com/).
2. Open file app.R of this project.
3. Click "Run App" button at the top right corner.

## <a name="dataset"></a> Dataset

- Dataset for PCA are provided in folder [/Dataset/pca](https://github.com/rinriko/Homework2/tree/main/Dataset/pca)

- Dataset for Image Classification with Eigenface are provided in folder [/Dataset/eigenfaces](https://github.com/rinriko/Homework2/tree/main/Dataset/eigenfaces)

## <a name="howtouse"></a> How to use
Start on the first (left-most) tab to select a feature between PCA and Image Classification with Eigenface. Then, chose the dataset or upload your CSV file, then click on each tab, in order from left to right, to see the results.

## <a name="documeent"></a> Documeent
Here is a [link for document](https://htmlpreview.github.io/?https://github.com/rinriko/Homework2/blob/main/R/ui/html/report.html)
