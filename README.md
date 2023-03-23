# PPR

PPR helps users pre-process, analyze, visualize, join and apply machine learning models on datasets without coding.

# Getting started

Try it online : https://kassis.shinyapps.io/pre-process/

# Available functionalities

## Pre-processing
- See details for each columns (data types, statistical informations and unique values)
- Drop NA using specific columns
- Replace NA of specific columns by median, mean, 0 or specific value
- Merge columns by specifying a name and a separator
- Encode columns (Numerical and One hot)
- Manual encoding
- Convert column's type (integer, double, character, factor and date)
- Split columns using separator
- Rename columns
- Reorder columns
- Delete columns
- Edit rows
- Delete rows
- Override current data
- Export dataset (copy, pdf, csv, excel and print)
- Filter table (filter table on multiple columns using regex syntax or by range)
- Reset (return initial dataset)


## Plots
### Available plots
- Scatter plot
- Bar chart
- Boxplot
- Violin plot
- Pie chart
- Correlogram
- Pair plot
### Plotting parameters
- x axis
- y axis
- label color
- facet wrap (up to 2 options)
- facet orientation (vertical or horizontal)
- position (only bar chart : dodge or identity)
- x label orientation
- vertical adjustement for x axis
- horizontal adjustement of x axis
- y label orientation
- vertical adjustement for y axis
- horizontal adjustement for y axis

## Join
Load two files and join them. PPR automatically detects columns with same name on both files.
- Inner join
- Left join
- Right join
- Full join
- Semi join
- Anti join

After joining, merged file can be either export or use as current dataset.

## ML models
- Linear regression
- SVM (Linear)
- SVM (Polynomial)
- Random Forest
- Linear discriminant Analysis


