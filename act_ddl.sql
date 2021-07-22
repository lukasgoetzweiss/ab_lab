-- CLIENT SIDE

-- user_metric
-- user_variable
-- user_variable_value

-- APP GENERATED DATA

-- experiment
CREATE OR REPLACE TABLE
example_act.experiment (
    experiment_id             INT64
  , name                      STRING
  , start_datetime            TIMESTAMP
  , end_datetime              TIMESTAMP
  , create_datetime           TIMESTAMP
  , modified_datetime         TIMESTAMP
)
PARTITION BY
start_datetime;

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
  , create_datetime           TIMESTAMP
);

-- BACKEND DATA

-- audience

-- experiment_audience