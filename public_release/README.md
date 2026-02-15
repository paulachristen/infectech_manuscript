# Public Data Package for Manuscript

This folder is prepared for public sharing and citation.

## What to share publicly

- `data/survey_anonymized.csv`: de-identified survey data generated from internal data.
- `data/codebook_public.csv`: variable dictionary for the shared dataset.
- `scripts/create_public_dataset.R`: reproducible anonymization script.

## Generate anonymized data

From repository root:

```bash
Rscript public_release/scripts/create_public_dataset.R
```

The script reads:
- `data/processed/df_filtered.csv`

The script writes:
- `public_release/data/survey_anonymized.csv`
- `public_release/data/codebook_public.csv`
- `public_release/data/anonymization_report.txt`

## Confidential interview data

Interview-level data are confidential and should remain restricted.
If your journal requires some qualitative sharing, provide only aggregate outputs
(for example, theme counts by income group) with no quotes and no country-level cells < 5.

## GitHub repository setup (for citation)

1. Create a new public repo (suggested name: `infectech-forecasting-public-data`).
2. Upload only this `public_release/` folder content.
3. Create a versioned release (`v1.0.0`) on GitHub.
4. Connect GitHub to Zenodo and create a Zenodo archive from the release.
5. Use the Zenodo DOI in your manuscript Data Availability Statement.

GitHub link is useful for browsing, while DOI is preferred for formal citation stability.
