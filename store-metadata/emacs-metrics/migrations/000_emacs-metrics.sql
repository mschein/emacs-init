-- Initial database creation for store emacs-metrics

-- Is it better to have one row per call?
CREATE TABLE function_metrics (
     id INTEGER PRIMARY KEY,
     name TEXT NOT NULL,
     count INTEGER NOT NULL
);
CREATE UNIQUE INDEX function_metrics_name_index ON function_metrics (name);
