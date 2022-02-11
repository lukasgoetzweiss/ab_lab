# rctr

rctr is an open source web app to help data scientists orchestrate and report on targeted randomized experiments. rctr is built on R shiny and BigQuery and can be setup in under 30 minutes. The underlying experimentation model is highly general and can be applied to experiments in a wide variety of context. 

## Experimentation model

There are four terms to understand to begin running experiments in rctr:

1. **Unit of observation** - in most business contexts this will be a user or potential customer
2. **Audience** - a set of units of observation defined by some filtering logic
3. **Treatment** - a specific action, policy, or intervention put forward towards a unit of observation
4. **Experiment** - a set of treatments applied to an audience over a specified time period

## Setup

rctr is set up in three steps:

1. define data integration by writing two queries
2. setup config file
3. run docker image

### Data integration

Before starting, identify your unit of observation. In what follows, let's assume the unit of observation is a user.

Next, create a new BigQuery data set and create two views (or tables):

1. Define dimensions to use when targeting audiences
  
```
  create view <my_bq_data_set>.customers as (
    select user_id
        ,  visits_per_week
        ,  region
        ,  lifetime_purchases
      from user_dim
  );
```

Note that you can name your table anything (you'll specify the table name when setting up the config file)

2. Define time series metrics that you want to measure

```
  create view <my_bq_data_set>.customer_transactions as (
    select user_id
        ,  date
        ,  units_sold
        ,  total_sales
      from transactions
  );
```

Again, feel free to name this table whatever makes sense for your application. The only requirements on this table are that it can join to the dimension table above (in this case on `user_id`), and that it contains a date field.

### Setup config file

Next, create a file named `context.yml` anywhere on your machine using the template below:

```
bq_project: <your bq project name>
bq_user: <your bq username>
bq_dataset: <the bq dataset that contains the tables above>
bq_verif: <file name of GCP json verification file, store in the same directory as this yml file>
unit_pk: <primary key unique to each unit of observation, e.g., user_id>
segment_table: <name of the table used for audience targeting, e.g., customer>
timeseries_table: <name of table with time series metrics of interest, e.g., customer_transactions>
timeseries_timestamp: <name of datefield in timeseries_table, e.g., date>

```

### Run docker image

Make sure you have [docker](https://docs.docker.com/) installed on your machine and run the following command to build and run a local docker image

```
docker build -t test .

docker run --rm \
  -e GOOGLE_APPLICATION_CREDENTIALS \
  -p 80:80 \
  -v ~/ab_lab_mnt:/srv \
  test Rscript -e "shiny::runApp('/src/rctr/act_app/', port = 3838)"
```

You're all setup! You can now view the app at http://localhost:80

