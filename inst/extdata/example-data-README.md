# Built-in NuSEDS example data

`metasalmon` ships two Fraser coho example tables so users can choose a tiny demo
or a fuller official slice.

## Included files

| File | Rows | Years | Intended use |
| --- | ---: | --- | --- |
| `nuseds-fraser-coho-sample.csv` | 30 | 1996–2024 | Smallest possible walkthroughs, fast examples, light semantic seeding demos |
| `nuseds-fraser-coho-2023-2024.csv` | 173 | 2023–2024 | More realistic package creation, testing, and documentation examples |

The tiny sample is retained unchanged for backwards compatibility.

## Provenance for the fuller official example

- Open Government Canada record: <https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6>
- Upstream resource used: <https://api-proxy.edh-cde.dfo-mpo.gc.ca/catalogue/records/c48669a3-045b-400d-b730-48aafe8c5ee6/attachments/Fraser%20and%20BC%20Interior%20NuSEDS_20251014.xlsx>
- Resource label: `Fraser and BC Interior NuSEDS_20251014.xlsx`
- Publisher: Fisheries and Oceans Canada
- Licence: Open Government Licence - Canada

## Reproducible derivation

The source repository includes `data-raw/nuseds_fraser_coho_examples.R`, which
recreates `nuseds-fraser-coho-2023-2024.csv` by:

1. downloading the official Fraser and BC Interior workbook,
2. filtering to `SPECIES == "Coho"`,
3. filtering to `ANALYSIS_YR %in% c(2023, 2024)`,
4. keeping a compact analysis-friendly subset of columns,
5. using `NATURAL_ADULT_SPAWNERS` because `NATURAL_SPAWNERS_TOTAL` is blank for
   this official two-year slice,
6. converting `START_DTT` and `END_DTT` to ISO dates, and
7. sorting by `ANALYSIS_YR`, `AREA`, `WATERBODY`, and `POP_ID`.

## Notes on the tiny demo

The legacy 30-row `nuseds-fraser-coho-sample.csv` file remains in place as the
fastest built-in demo. Its bundled example metadata continues to live in:

- `inst/extdata/dataset.csv`
- `inst/extdata/tables.csv`
- `inst/extdata/column_dictionary.csv`

Use the tiny sample when you want the quickest end-to-end walkthrough. Use the
173-row official slice when you want something closer to real Fraser coho data
without shipping the full NuSEDS workbook.
