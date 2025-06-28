INTERFACE zif_llm_00_pat_list
  PUBLIC .


  TYPES tt_file TYPE zif_llm_00_file_list=>tt_file .
  TYPES:
    BEGIN OF ts_pat,
      name    TYPE string,
      pat     TYPE REF TO zif_llm_00_pat,
*     pat_usr TYPE REF TO zif_llm_00_pat,
*     pat_sys TYPE REF TO zif_llm_00_pat,
    END OF ts_pat .
  TYPES:
    tt_pat TYPE STANDARD TABLE OF ts_pat WITH KEY name .

  METHODS get
    RETURNING
      VALUE(rt_) TYPE tt_pat .
  METHODS filter
    IMPORTING
      !iv_       TYPE string
      !io_       TYPE REF TO zcl_llm_00_list OPTIONAL
    RETURNING
      VALUE(rt_) TYPE tt_pat .
  METHODS get_by_name
    IMPORTING
      !iv_       TYPE string
    RETURNING
      VALUE(ro_) TYPE REF TO zif_llm_00_pat .
ENDINTERFACE.
