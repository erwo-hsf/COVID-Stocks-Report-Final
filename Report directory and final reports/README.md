# Template for writing a thesis / term paper according to the regulations of the Hochschule Fresenius applying APA 7 guidelines

This repository contains a Quarto template including a Quarto extension. 
This template was created by Prof. Dr. Stephan Huber and is based on the Quarto extension apaquarto [@Schneider2024quarto] and the \LaTeX\ package apa7 [@Weiss2022Formatting]. For the most part, it is designed in accordance with APA stlye (7th Edition) guidelines. However, some adjustments have been made to conform to the formatting requirements specified in the "Handbook of Academic Writing" [@Hildebrandt2019Handbook, Section 4.1.2]. More detailed explanations and examples of use can be found in the PDF “roadshow_apa_en.pdf”.

In order to use the template download the repo and prepare your PC as explained below. If you don't want to install anything on your PC, feel free to run everything in the cloud using [Posit Cloud](https://posit.cloud/). If you do so do not forget to install an up to date version of tinytex. If you struggle with anything, write me a note.


# Preparation of the PC

- Install R: [https://www.r-project.org/](https://cran.rstudio.com/)
- Install R Studio: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
- Install Quarto (version = 1.4 or higher): [https://quarto.org/docs/get-started/](https://quarto.org/docs/get-started/)
- Install the R package `quarto` and `tinytex` with
```{r}
install.packages("quarto")
install.packages("tinytex")
```
The `quarto` package in R is not Quarto itself, but it provides handy functions for interacting with Quarto in R.

# Download the repo

## Method 1: Using the mouse

### Method 1a: Clicking in GitHub

In GitHub, click on the green `<> Code v` button found in this repository. In the context menu that opens, you will find a button called `Download ZIP`. Click on it and you will have a zipped copy of the repo that you can unzip and use.

### Method 1b: Click on [this link](https://github.com/hubchev/temp_apa_en/zipball/HEAD)

You can download the repository in a zipped file using [this link](https://github.com/hubchev/temp_apa_en/zipball/HEAD). Now you can unzip it to a directory of your choice.

## Method 2: usethis::use_course()

Install and download the usethis package and download it with `use_course`:

```{r}
install.packages("usethis")
library("usethis")
usethis::use_course("hubchev/temp_apa_en", destdir = getwd())
```

By default, the repository is downloaded to a directory and stored in your working directory. Feel free to replace `getwd()` with "path/to/target", i.e. the desired location on your system where you want to download the repository.


## Method 3: Using the terminal
### Method 3a: Using Git

If you have Git installed on your system ([here](https://git-scm.com/downloads) you can download it if necessary), you can download this repository using the command line:

- Open the terminal in R Studio (In RStudio, the terminal is in a tab next to the console.) or the command prompt window of your operating system.
- Navigate to the directory where you want to download the repository using the cd command (e.g. `cd ~/documents`).
- Execute the following command to clone the repository:

```{bash}
git clone https://github.com/hubchev/temp_apa_en.git
```

### Method 3a: Using Quarto

Use the terminal (In RStudio, the terminal is located in a tab next to the console.) to navigate to the directory where you want to copy the template (e.g. cd path/to/my/folder). Make sure the directory is empty and execute this command:

```{bash}
quarto use template hubchev/temp_apa_en
```


# Using the template
Once you have downloaded the repository, navigate to the directory where you downloaded it and open the `temp_apa_en.qmd` file in RStudio or your favorite editor. Customize the template to create your own work!

To convert the Quarto file into the desired document (e.g. a PDF or HTML), "render" the `exam_template.qmd` file. You can do this in RStudio by opening the file and clicking on the "Render" button or by executing the following command in the R console:

```{r}
quarto_render("path/to/temp_apa_en.qmd")
```

Replace "path/to/temp_apa_en.qmd" with the actual path to the file.


# Feedback

If you have any questions or need help with the template, feel free to `pull request` changes via GitHub or contact me. 
