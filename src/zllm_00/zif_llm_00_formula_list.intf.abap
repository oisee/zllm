INTERFACE zif_llm_00_formula_list
  PUBLIC .

  TYPES tt_file TYPE zif_llm_00_file_list=>tt_file .
  TYPES:
    BEGIN OF ts_formula,
      name    TYPE string,
      formula TYPE REF TO zif_llm_00_formula,
      pat_usr TYPE REF TO zif_llm_00_pat,
      pat_sys TYPE REF TO zif_llm_00_pat,
    END OF ts_formula .
  TYPES:
    tt_formula TYPE STANDARD TABLE OF ts_formula WITH KEY name .

  METHODS get
    RETURNING
      VALUE(rt_) TYPE tt_formula.
  METHODS filter
    IMPORTING
      !iv_       TYPE string
      !io_       TYPE REF TO zcl_llm_00_list OPTIONAL
    RETURNING
      VALUE(rt_) TYPE tt_formula .
  METHODS get_by_name
    IMPORTING
      !iv_       TYPE string
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_00_formula .
ENDINTERFACE.
