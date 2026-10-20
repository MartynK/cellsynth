# cellsynth 0.1.0

* First release. Cell-wise rolling censor-and-redraw synthesis with CART
  leaf donors (raw strings copied), synthesised missingness, cell and row
  censor modes, lock groups for coupled columns, auxiliary predictors from
  other tables.
* Every sheet of a workbook is synthesised into one output workbook; empty
  and small sheets are copied, blanked or dropped (`small_sheets`).
* Identifier detection covers CDISC/EDC key names (`USUBJID`, `SUBJID`,
  `SITEID`, ...), multi-segment and zero-padded codes; identifiers and text
  columns can also be named or matched by pattern (`id`, `id_pattern`,
  `text`, `text_pattern`, or session-wide via `options(cellsynth.id = ...)`).
* Very wide sheets (9,000+ columns) are screened block-wise in bounded memory.
