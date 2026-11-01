# Descriptive analysis of cinema-related variables

## Cinema attendance and profile of attendees

Attendance percentages and attendee characteristics in the main sample use the supplied survey weight (`KVZV2` in 2016; `KVZV` in 2018–2024). Counts are unweighted. Income is nominal monthly rubles. The additional samples are kept separate and their results are unweighted.

| Year | Cinema attendees, n | Weighted share among valid attendance answers, % | Weighted mean age | Women among attendees, weighted % | Tertiary education (codes 1–4), weighted % | Weighted median per-capita income, rubles/month | Weighted median household income, rubles/month | Regular cinema attendance among attendees, weighted % |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 2016 | 25,429 | 36.2 | 34.7 | 53.9 | 43.0 | 15,000 | 45,000 | 19.7 |
| 2018 | 35,979 | 39.5 | 35.7 | 53.7 | 42.7 | 17,500 | 55,000 | 21.4 |
| 2020 | 28,845 | 31.4 | 35.2 | 53.3 | 41.9 | 21,875 | 55,000 | 14.0 |
| 2022 | 31,839 | 36.6 | 35.4 | 53.9 | 39.7 | 22,500 | 67,500 | 18.1 |
| 2024 | 35,313 | 40.5 | 37.2 | 56.0 | 44.3 | 31,314 | 78,733 | 18.8 |

Across the main sample, the weighted share reporting cinema attendance was lowest in 2020 and highest in 2024. Attendees' weighted mean age was between 34.7 and 37.2 years. Women made up about 53–56% of attendees. Specialist/master's education was the largest single education category in every wave; the CSV contains the full weighted distribution across education levels.

Income amounts are nominal monthly rubles and are not adjusted for inflation. Weighted means across cinema attendees were:

| Year | Mean per-capita household income, rubles/month | Mean household income, rubles/month |
|---:|---:|---:|
| 2016 | 19,280 | 53,411 |
| 2018 | 21,728 | 60,031 |
| 2020 | 24,079 | 64,340 |
| 2022 | 26,456 | 71,546 |
| 2024 | 37,097 | 88,541 |

## Other selected variables

| Year | Child went to cinema: selected records, n | Film downloads: selected records, n | 2024 share among valid 0/1 responses: child cinema / film download, weighted % |
|---:|---:|---:|---:|
| 2016 | 2,065 | 24,832 | — |
| 2018 | 3,128 | 33,951 | — |
| 2020 | 3,165 | 35,847 | — |
| 2022 | 2,880 | 32,616 | — |
| 2024 | 2,746 | 30,491 | 54.9 / 38.8 |

For 2016–2022, the child-cinema and film-download fields are stored as selected response options: a positive code means the option was selected, while missing values do not mean “no.” Those rows are reported as counts only. The 2024 files use 0/1 coding, so percentages are calculated among records with a non-missing 0 or 1. The child-cinema question applies only to the relevant child module.

The cinema-frequency variable `I09_03_01_02` is present in **2024** as well as 2016–2022. Regular attendance is code 3; one or several visits are code 4. This corrects my earlier note that it was absent in 2024.

## Detailed tables and reproducibility

- [Attendance and frequency by year/sample](cinema_attendance_descriptives.csv)
- [Cinema-attendee profile, including full education distributions](cinema_goers_profile_descriptives.csv)
- [Child-cinema and film-download variables](cinema_related_variables_descriptives.csv)
- [R script used to generate the tables](../02_scripts/cinema_descriptive_analysis.R)

Main and additional samples are not pooled. Additional samples are shown separately without weights because the weight variable is incomplete there. This is descriptive analysis; it does not account for clustering or stratification and does not establish causal relationships.
