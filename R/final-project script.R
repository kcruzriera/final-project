library(gtsummary)
library(tidyverse)
library(readxl)
library(here)
library(janitor)
here::here()
getwd()
final_project<-read_excel(here::here("R","final-project.xlsx"))
final_projectx<-clean_names(final_project)

tbl_summary(
	final_projectx,
	by=reported_perpetrator_name,
	include=c(number_of_attacks_on_health_facilities_reporting_damaged,health_workers_killed,health_workers_injured, health_transportation_destroyed),
	label=list(
		number_of_attacks_on_health_facilities_reporting_damaged ~ "Health Facilities Damaged",
		health_workers_killed ~ "Health Workers Killed",
		health_workers_injured ~ "Health Workers Injured",
		health_transportation_destroyed ~ "Health Transportation Destroyed"),
missing_text = "Missing")|>
	add_p(test = list(all_continuous() ~ "t.test",
                    all_categorical() ~ "chisq.test")) |>
  add_overall(col_label = "**Total**") |>
  bold_labels() |>
  modify_footnote(update = everything() ~ NA) |>
  modify_header(label = "**Variable**", p.value = "**P**")

poisson_model <- glm(health_workers_killed ~ reported_perpetrator_name +
										 	number_of_attacks_on_health_facilities_reporting_damaged + health_transportation_destroyed,
										 data = final_projectx, family = poisson)

tbl_regression(
	poisson_model,
	exponentiate=TRUE)

daily_count<-function (n) {
	countee<-n
	return(table(final_project$`Health Workers Killed`== n))
}
daily_count(2)
daily_count(3)
