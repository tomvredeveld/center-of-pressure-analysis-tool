# Introduction
This Github repository holds a Center of Pressure Analysis tools, which employs clinicians with a webbrowser-based app to calculate frequently used Center of Pressure parameters without the need to know about programming languages.

![Screenshot of the analysis tool](/images/screenshot-gui.png)
*Screenshot of the analysis tool with an uploaded force plate file, showing the center of pressure in mediolateral and anteroposterior directions*

With this tool you can select a segment from data obtained with a force plate (e.g., AMTI, Kistler, Wii Balance Board) to calculate Center of Pressure (COP) parameters such as standard deviation of displacement in a certain direction or total sway area. In day-to-day clinical practice, one may want to indicate postural stability by COP parameters, however, without the need to understand programming languages such as MATLAB, Python or R. 

This tool enables clinicians to upload a `.txt` or `.csv` file which is than plotted and a segment can be selected by choosing timestamps. Then, frequently used COP parameters can be calculated and a COP trajectory (sway) can be viewed.

The tool was developed using the `R` language and `Shiny for R`. More information on R for Shiny can be found at [rstudio.com](https://www.rstudio.com/products/shiny/)

## Getting started.
The app is developed to run from the shinyapps.io website (through a simple webbrowser) or by running it locally and download the code from this Github repository. Both methods are described below. 

### Method 1: Shinyapps.io
The app is published to the shinyapps.io server so it can be used online. You may find it at my personal shinyapps.io account [tomvredeveld.shinyapps.io](https://tomvredeveld.shinyapps.io/center-of-pressure-analysis-tool/) . Please note, due to a limited monthly use at the server, it may be possible that the app is not available. Wait till the next month, or download the app and use it locally on your computer. The steps for using the app are described below.

### Method 2: Local use
The single `app.R` file can be downloaded from this github page and can be run when using R and RStudio. You can install them here:
R: https://cran.rstudio.com
RStudio: https://posit.co/download/rstudio-desktop/

The app depends on a couple of functions from packages which are (freely) available from the CRAN package repository. You can install them in R by running the following single line code: 

`install.packages(c("shiny", "data.table", "signal", "plotly")`

Then open the  `app.R` file and press `Run App` from within RStudio. The steps to using the app are described below. 

![Screenshot of running it local with RStudio](/images/screenshot-using-r.png)
*Quick instructions to run the app locally on your computer by using R and Rstudio.*

## Using the app

### Step 1: uploading data
Upload a `.txt` or `.csv` file to the app, but make sure that the file consists of three columns, time, copx (medio-lateral position of the center of pressure) and copy (anterior-posterior position of the center of pressure). These should be the first three columns, yet names can be different and more columns may be present (will be ignored when uploading your file). 

### Step 2: looking at the data
At the tab `table` and `plotly` you may find a table displaying the first 6 rows of the data you uploaded. This shows you if the upload was succesful. The plotly tab provides 2 figures: above, center of pressure signal for the medio-lateral axis and below for the anterior-posterior axis. 

### Step 3: selecting a segment
Next, you might want to calculate COP parameters from a part of the signal (or complete signal, than use 0 and the highest value as timestamps). You can select a 'timestamp' by hovering your mouse over the plot. This exact value has to be written in the fields next to the figures. A second timestamp depicts the end of the segment. 

### Step 4: COP parameters & Sway area plots
Now, click the `Calculate COP Parameters` button. Automatically the COP parameters are calculated: including: 
- Standard deviation of displacement of the COP on the medio-lateral axis (COPx or ML)
- Standard deviation of displacement of the COP on the anterior-posterior axis (COPy or AP)
- Mean velocity of the COP on the medio-lateral axis,
- Mean velocity of the COP on the anterior-posterior axis
- Total COP pathlength
- 95% Predicted Ellipse Area, as described by P. Schuber and M. Kirchner in their [paper](http://dx.doi.org/10.1016/j.gaitpost.2013.09.001)

The 'Sway Area' tab shows two plots, one with COP coordinates and a drawn ellipse, while the second shows, with identical data, the COP path, or: COP trajectory,  viewed from a larger coordinate plane. You can interactively click the figures to drag, zoom, or identify specific data points. Click the 'house' symbol that appears in the right upper corner to return to the original plotted positions of the data.

## Privacy
Please be aware that uploading your data to the shinyapps.io website does not provide privacy! Be careful when using sensitive data and perhaps choose to run it local on your computer.

## Version
Current: 1.0.0 - First public version

Version management
0.1.0 - Personal beta version. 

### Issues
If anything is not working properly in this app, please let me know and add it as an issue or send me an e-mail t.vredeveld [at] hva [dot] nl 
