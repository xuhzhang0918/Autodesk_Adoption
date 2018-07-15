README
================
UC Davis - Autodesk Adoption team
2018-6-30

R Files:
--------

| index | file\_name                                                   | lines\_of\_code | description                                                  |
| ----: | :----------------------------------------------------------- | --------------: | :----------------------------------------------------------- |
|     1 | [codes/2017/adjusted\_basic\_summary.R](codes/2017/adjusted_basic_summary.R) |              21 | Summaries for the dataset                                    |
|     2 | [codes/2017/adoption\_and\_time.R](codes/2017/adoption_and_time.R) |              67 | How many products people use in each collection by subscription types(single or multiple) |
|     3 | [codes/2017/business\_size.R](codes/2017/business_size.R)    |              49 | Business size and number of seats                            |
|     4 | [codes/2017/countries.R](codes/2017/countries.R)             |              86 | Number of seats in each region of the world                  |
|     5 | [codes/2017/custom functions.R](codes/2017/custom%20functions.R) |              35 | Create function to read the .tsv file and convert columns into correct data type |
|     6 | [codes/2017/data\_preparation.R](codes/2017/data_preparation.R) |              41 | Convert columns to correct data type                         |
|     7 | [codes/2017/overlap\_all\_seats.R](codes/2017/overlap_all_seats.R) |              48 | How many seats use different versions of the same product during the same week |
|     8 | [codes/2017/overlap\_all\_sessions.R](codes/2017/overlap_all_sessions.R) |              46 | How many sessions use different versions of the same product during the same week |
|     9 | [codes/2017/time\_series\_and\_correalation.R](codes/2017/time_series_and_correalation.R) |              40 | Create line plot for how many people use a product  during a month |
|    10 | [codes/2017/top\_products.R](codes/2017/top_products.R)      |              70 | Top products in terms of seats/sessions                      |
|    11 | [codes/2018\_first\_week/product\_assosciations.R](codes/2018_first_week/product_assosciations.R) |             100 | Association rules between products                           |
|    12 | [codes/2018\_first\_week/product\_update\_revise.R](codes/2018_first_week/product_update_revise.R) |             227 | Create plot on how users update the products.                |
|    13 | [codes/2018\_first\_week/quantitles.R](codes/2018_first_week/quantitles.R) |              82 | Quantile on number of total seats and total sessions.        |
|    14 | [codes/aggregate/seats\_and\_sessions\_aggreagate.R](codes/aggregate/seats_and_sessions_aggreagate.R) |              24 | aggregate on number of seats on sessions with original dataset on weekly level. |
|    15 | [codes/geo/country\_mapping.R](codes/geo/country_mapping.R)  |              12 | map each country to corresponding regions                    |
|    16 | [codes/geo/geo\_collection\_association.R](codes/geo/geo_collection_association.R) |             213 | Association rules for products in different regions.         |
|    17 | [codes/geo/geo\_collection\_quantile\_final.R](codes/geo/geo_collection_quantile_final.R) |              62 | Quantile for number of seats and sessions in different collections and regions |
|    18 | [codes/geo/geo\_quantile\_final.R](codes/geo/geo_quantile_final.R) |              59 | Quantile for number of seats in different regions            |
|    19 | [codes/geo/geon\_quantile\_and\_association.R](codes/geo/geon_quantile_and_association.R) |              23 | Aggregate total number of seats and sessions on weekly level |
|    20 | [codes/geo/product\_associatons\_by\_geo.R](codes/geo/product_associatons_by_geo.R) |              92 | Association rules in different regions                       |
|    21 | [codes/product\_association.R](codes/product_association.R)  |              87 |                                                              |
|    22 | [codes/product\_dual\_usage.R](codes/product_dual_usage.R)   |             188 | How people use different versions of the same product.       |
|    23 | [codes/product\_update.R](codes/product_update.R)            |             274 | Create line plot on time representing how people update their products |
|    24 | [codes/redo\_association\_rules.R](codes/redo_association_rules.R) |              18 | Association rules for products.                              |

Jupyter Notebooks:
------------------

| index | file\_name                                                   | lines\_of\_code | description                                                  |
| ----: | :----------------------------------------------------------- | --------------: | :----------------------------------------------------------- |
|     1 | [notebooks/adption\_and\_times.ipynb](notebooks/adption_and_times.ipynb) |             547 | How many products people use in each collection by subscription types(single or multiple) |
|     2 | [notebooks/Draw\_plots\_for\_two\_slides.ipynb](notebooks/Draw_plots_for_two_slides.ipynb) |             542 | Number of seats in different business size and collections; Number of seats in different regions and collections. |
|     3 | [notebooks/Generate product lifecycle csv for each device id.ipynb](notebooks/Generate%20product%20lifecycle%20csv%20for%20each%20device%20id.ipynb) |            3097 | Generate table on what products users use in each week.      |
|     4 | [notebooks/product\_update.ipynb](notebooks/product_update.ipynb) |            2894 | Creates graphs on how long does it take for people to upgrade their products |
|     5 | [notebooks/redo\_association\_rule.ipynb](notebooks/redo_association_rule.ipynb) |             221 | Associations rules on products                               |
|     6 | [notebooks/sessions\_per\_seat.ipynb](notebooks/sessions_per_seat.ipynb) |             783 | Count of sessions per seat and distribution of it for each product/collection. |
|     7 | [notebooks/time\_series\_and\_correalation.ipynb](notebooks/time_series_and_correalation.ipynb) |             628 | Create graphs on how many sessions for a product in each month. |

Data Files
----------

| index | file\_name                                                   | lines\_of\_code | description                                                  |
| ----: | :----------------------------------------------------------- | --------------: | :----------------------------------------------------------- |
|     1 | [output/charts/dual\_usage\_all.csv](output/charts/dual_usage_all.csv) |              15 | How many people use different versions of the same product after update is available. |
|     2 | [output/charts/dual\_usage\_score.csv](output/charts/dual_usage_score.csv) |              18 | Calculated score evaluating how many people are using different versions of the same product. |
|     3 | [output/charts/dual\_usage\_score\_2\_versions.csv](output/charts/dual_usage_score_2_versions.csv) |              18 | Calculated score evaluating how many people are using different versions of the same product. |
|     4 | [output/charts/dual\_usage\_weeks.csv](output/charts/dual_usage_weeks.csv) |              94 | Calculated score evaluating how many people are using different versions of the same product. |
|     5 | [output/charts/new\_detvice\_dt.csv](output/charts/new_device_dt.csv) |              68 | How many devices are under the subscription model each week. |
|     6 | [output/charts/overlap\_seats.csv](output/charts/overlap_seats.csv) |             213 | How many seats are using the same products under different collections. |
|     7 | [output/charts/overlap\_sessions.csv](output/charts/overlap_sessions.csv) |             213 | How many sessions are using the same products under different collections. |
|     8 | [output/charts/product\_popularity.csv](output/charts/product_popularity.csv) |              45 | List of the most popular products by total sessions.         |
|     9 | [output/charts/product\_popularity\_all.csv](output/charts/product_popularity_all.csv) |              29 | List of the most popular products by total sessions.         |
|    10 | [output/charts/product\_wise\_dual\_usage.csv](output/charts/product_wise_dual_usage.csv) |              94 | How many devices are using different versions of the same product |
|    11 | [output/charts/quantitles\_top3\_prodtucts.csv](output/charts/quantitles_top3_products.csv) |              10 | Quantiles of total seats for top 3 products in different collections. |
|    12 | [output/charts/region\_data.csv](output/charts/region_data.csv) |              10 | Totals seats and ratio of seats in different regions.        |
