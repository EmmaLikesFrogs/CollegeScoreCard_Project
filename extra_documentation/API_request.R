# Install packages
install.packages("httr")
install.packages("jsonlite")

# Import libraries
library(httr)
library(jsonlite)

# Create a GET response to call the API
url <- "https://api.data.gov/ed/collegescorecard/v1/schools"

api_key <- "S7BhOWWUPmlNh9Alf9n2DDlwDD93JQDoyq0RUhhT"

# Define parameters for the API request
params <- list(
  "api_key" = api_key,
  "school.state" = "CA",
  "school.name" = 'University of California'
)


# LIST OF POSSIBLE PARAMETERS:
# school.name: Filter by the name of the school.
# school.state: Filter by the state in which the school is located.
# school.carnegie_basic: Filter by the Carnegie Classification Basic classification.
# school.degrees_awarded.predominant: Filter by the predominant degree awarded by the school.
# fields: Specify the fields (data columns) you want to include in the response.
# per_page: Control the number of results per page (pagination).
# page: Retrieve a specific page of results when using pagination.
# sort: Sort the results by a specific field (e.g., 2018.student.size:desc to sort by student size in descending order).
# id: Retrieve data for a specific school by its unique identifier.
# zip: Filter by the ZIP code of the school's location.
# school.ownership: Filter by the ownership type of the school (public, private, etc.).
# school.under_investigation: Filter schools that are under investigation by the U.S. Department of Education.
# school.operating: Filter by the operational status of the school (active, closed, etc.).
# school.main_campus: Filter by whether the school is the main campus or not.
# latest.admissions.admission_rate.overall: Filter by the overall admission rate.
# latest.earnings.10_yrs_after_entry.median: Filter by median earnings 10 years after entry.
# latest.cost.attendance.academic_year: Filter by the cost of attendance for an academic year.
# latest.completion.completion_rate_4yr_150nt: Filter by the 4-year completion rate for first-time, full-time students.
# latest.repayment.3_yr_repayment.overall: Filter by the 3-year repayment rate for federal student loans.
# latest.repayment.repayment_cohort.3_year_declining_balance: Filter by the 3-year declining balance repayment cohort rate.
# latest.student.demographics: Filter by various demographic characteristics of students.


# Make the GET request to the API
response <- GET(url, query = params)

# Check if the request was successful
if (http_status(response)$category == "Success") {
  # Parse the JSON response
  data <- content(response, "parsed")

} else {
  cat("API request failed with status code: ", http_status(response)$status_code)
}


View(data)
data$results[[20]]$school$tuition_revenue_per_fte
