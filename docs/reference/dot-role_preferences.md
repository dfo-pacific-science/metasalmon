# Load role-based ontology preferences

Returns the ranked allowlist of preferred ontologies per I-ADOPT role.
Based on dfo-salmon-ontology CONVENTIONS.md:

- unit: QUDT + NVS P06 preferred

- method: gcdfo: SKOS + SOSA/PROV patterns

- entity: gcdfo salmon domain + taxa resolvers (GBIF/WoRMS)

- property: STATO/OBA measurement ontologies

- Wikidata is alignment-only

## Usage

``` r
.role_preferences()
```

## Value

Tibble with role preferences and priority rankings
