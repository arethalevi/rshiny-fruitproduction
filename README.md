# Fruit Production Dashboard

## Project Description
An interactive visualization dashboard includes map visualization, time plot, and barchart to see fruit production trend in Indonesia from 2015-2020 using data from Badan Pusat Statistik (BPS). The project is built using Rshiny and deployed Rshinyapps.

## Data Variables 
- <b>Type of fruit</b>: categorical data with 28 classes (27 type of fruits + overall fruit productions)
- <b>Location of Production</b>: categorical data with 35 classes (34 provinces in Indonesia + Indonesia overall)
- <b>Time of production</b> in year
- <b>Number of Production</b> in ton


## Dashboard
<img src="https://user-images.githubusercontent.com/72438807/208246926-6087feba-8f4c-41f9-9116-8f62c015eab6.png" width=80%>

Visualization consists of 3 major plot: 

- <b>Line Chart</b> where its X axis is the time in year and Y axis is number of fruit production. Plot will show overall production of fruits in Indonesia by default, but user can customize the type of fruit and location of production they want to see in dropdown.
- <b>Map Chart</b> where user can see production spread in Indonesia. Plot will show overall production of fruits in Indonesia by default, but user can customize the type of fruit and year of production in the dropdown.
- <b>Bar Chart</b> of 8 most and least produced fruits. User can customize the year of production and location of production. 

##
You can visit the project [here](https://arethalevi.shinyapps.io/dashboard-fp/)
