## data

This folder holds project datasets and intermediate artifacts used throughout the workflow.

- **Suggested structure**:
  - `raw/`: original, immutable data as received.
  - `interim/`: partially processed data and checkpoints.
  - `processed/`: cleaned, analysis-ready datasets produced by `c01-data-transformation`.
- **Good practices**: avoid committing large, proprietary, or sensitive data to version control; document data sources and access instructions here if data must be obtained separately.

