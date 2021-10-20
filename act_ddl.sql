-- CLIENT SIDE

-- user_metric
-- user_variable
-- user_variable_value

-- APP GENERATED DATA

-- experiment
CREATE OR REPLACE TABLE
example_act.experiment (
    experiment_id INT64
  , name STRING
  , audience_id INT64
  , primary_impact_variable STRING
  , delivery_variable STRING
  , attrition_variable STRING
  , attrition_rate_prior FLOAT64
  , attrition_independence_prior STRING
  , spillover_prior STRING
  , start_datetime TIMESTAMP
  , end_datetime TIMESTAMP
  , create_datetime TIMESTAMP
  , modify_datetime TIMESTAMP
);

-- treatment
CREATE OR REPLACE TABLE
example_act.treatment (
    treatment_id              INT64
  , name                      STRING
  , create_datetime           TIMESTAMP
);

-- experiment_treatment
CREATE OR REPLACE TABLE
example_act.experiment_treatment (
    experiment_treatment_id   INT64
  , experiment_id             INT64
  , treatment_id              INT64
  , sample_weight             FLOAT64
  , create_datetime           TIMESTAMP
);

-- audience
CREATE OR REPLACE TABLE
example_act.audience (
    audience_id               INT64
  , name                      STRING
  , create_datetime           TIMESTAMP
);

-- audience_filter
CREATE OR REPLACE TABLE
example_act.audience_filter (
    audience_filter_id        INT64
  , audience_id               INT64
  , filter_on                 STRING
  , comparator_sql            STRING
  , comparator_params         STRING
  , create_datetime           TIMESTAMP
);

-- BACKEND DATA

-- audience

-- experiment_audience
CREATE OR REPLACE TABLE
example_act.experiment_audience (
    experiment_audience_id    INT64
  , experiment_id             INT64
  , user_id                   INT64
  , treatment_id              INT64
  , active_fg                 STRING
  , create_datetime           TIMESTAMP
  , modified_datetime         TIMESTAMP
);
