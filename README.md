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


`install.packages(c("shiny", "ggplot2", "dplyr"))`

**Running the App:**

Open app.R in RStudio.
Click "Run App" in the top-right corner of the RStudio script editor.

# 4. Adding New Statistical Illusions

To add your own statistical illusion to the Shiny App, follow these steps:
...

Current file structure
prototype
    ├── R
    │   ├── app1_module.R
    │   ├── app2_module.R
    │   ├── app3_module.R
    │   ├── app4_module.R
    │   ├── home_module.R
    │   ├── monty_hall_module.R
    │   └── two_child_module.R
    ├── app.R
    └── data
        ├── department_admissions.csv
        ├── exercise_data.csv
        └── total_admissions.csv
