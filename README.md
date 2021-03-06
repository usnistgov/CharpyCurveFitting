# CharpyCurveFitting

NIST has developed a software package that allows users to fit test results obtained from Charpy or toughness tests as a function of test temperature, thereby obtaining so-called transition curves. Non-linear fitting is available for five regression models, two symmetric and three asymmetric, selected from the literature. Example data sets are also included in the NIST software package. The NIST software is tested on three Charpy absorbed energy datasets. Two datasets correspond to actual impact test results performed on various pipeline steels, while the third is a simulated dataset generated from predetermined model parameters. These datasets allow a thorough demonstration of the NIST software. The software is freely available to the scientific community for research purposes. 

Along with the R Shiny application, two macro-enabled excel spreadsheets are provided:

1. [Macro-enabled Excel spreadsheet Establishment of initial values for Charpy regressions.xlsm](excel_files/Establishment_of_initial_values_for_Charpy_regressions.xlsm)
This spreadsheet can be used to determine reasonable initial values for the parameters of three Charpy regression models: AHT (asymmetric hyperbolic tangent), BUR (asymmetric Burr model), and KHT (asymmetric Kohout model). The effect of changing parameter values can be immediately appreciated in graphical form, by comparing the corresponding regression curve with the experimental data points.

2. [Macro-enabled Excel spreadsheet Charpy data regression.xlsm](excel_files/Charpy_data_regression.xlsm)
This spreadsheet can be employed by users that only need a basic-level analysis (fitting data with various models and picking the best-fitting), as an alternative to the advanced analysis offered by the Shiny App. The spreadsheet also provides values of characteristic temperatures and graphically compares the experimental data with the various analytical regression curves.


For more information, visit: https://www.nist.gov/publications/nist-software-package-obtaining-charpy-transition-curves.

## Running the Application Locally

There are two ways to run the Charpy application locally. 

### Option 1: Running from R on Windows

To run the application using R on your computer, you will first need to install R [here](https://www.r-project.org/). Then, download a zip file of the https://github.com/usnistgov/CharpyCurveFitting repository by clicking the green 'Code' button, and then 'Download ZIP'. After unzipping the file, click the run_app.bat file (if you are using Windows), and the app should run. Note that the first time the app runs, it may take some time, as the necessary packages will need to be installed if they aren't already. Afterwards, the app should run almost instantly.

### Option 2: Containerized using Docker

To run the Charpy application from within a Docker container you will first need to install Docker (https://docs.docker.com/get-docker/).

Once Docker is installed, download the CharpyCurveFitting repository (at github.com/usnistgov/CharpyCurveFitting, click 'Code' and then 'Download ZIP'), and navigate your terminal to the main directory of the project (the same level as Dockerfile). Then, run the following command to build the image:
```
docker build -t charpy .
```
To run the container, run the following command:
```
docker run -d -p 3838:3838 --name my_container charpy
```
(-d for 'detached', -p specifies the port mapping, '--name' gives a name to the running container, and 'charpy' tells docker which image to build the container from.) Then the app should be visible at localhost:3838 (accessed via your web browser).

To stop and remove the running container, run the following:
```
docker rm -f my_container
```

Alternatively, you can run, stop, and remove the container using the Docker Desktop application.
