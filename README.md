# Welcome to the tp24-statistical-illusions project!

# 1. Overview

**Project Aim:**
The goal of this project is to create a Shiny App in R, showcasing various statistical illusions for use in university statistics lectures.

**Project Description:**
This Shiny App will provide interactive demonstrations of different statistical illusions, helping students understand and visualize complex statistical concepts. The app will include modules such as the Monty Hall Problem, Simpson's Paradox, St. Petersburg Paradox, and more.

# 2. Getting Started

**Prerequisites:**


**Installation:**

Clone the repository:

`git clone https://github.com/fgoepp/tp24-statistical-illusions.git`


Navigate to the project directory:

`cd tp24-statistical-illusions`


Install required R packages:


`install.packages(c("shiny", "ggplot2", "dplyr", "plotly", "shinyBS", "shinydashboard", "styler", "DT", "bslib"))`

**Running the App:**

Open app.R in RStudio.
Click "Run App" in the top-right corner of the RStudio script editor.

# 4. Adding New Statistical Illusions

https://github.com/fgoepp/tp24-statistical-illusions/wiki/Add-your-own-illusion

# Current file structure

```
.
├── app.R
├── data
│   ├── simpsons_paradox
│   │   ├── department_admissions.csv
│   │   ├── exercise_data.csv
│   │   └── total_admissions.csv
│   └── st-petersburg_paradox
│       ├── st-petersburg-history.html
│       ├── st-petersburg-intro.html
│       ├── st-petersburg-math.html
│       └── st-petersburg-references.html
├── modules
│   ├── app1_module.R
│   ├── app2_module.R
│   ├── app3_module.R
│   ├── app4_module.R
│   └── home_module.R
└── www
    ├── monty_hall
    │   └── desiciontree.png
    ├── simpsons_paradox
    │   └── vector_interpretation.png
    └── two_children
        ├── BB.png
        ├── Bg.png
        ├── GG-crossed.png
        ├── GG.png
        ├── Gb-crossed.png
        ├── Gb.png
        ├── male.png
        ├── man.png
        ├── small_man.png
        ├── small_woman.png
        ├── tuesdayboy.JPG
        ├── tuesdayboy.gif
        └── woman.png
