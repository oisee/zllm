CLASS zcl_llm_00_json_mock DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES ts_ TYPE REF TO zif_llm_00_json .
    CLASS-METHODS new
      IMPORTING
        iv_        TYPE string
      RETURNING
        VALUE(ro_) TYPE REF TO zif_llm_00_json .
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS constructor
      IMPORTING iv_ TYPE string.

    DATA: mv_ TYPE string.
ENDCLASS.



CLASS ZCL_LLM_00_JSON_MOCK IMPLEMENTATION.


  METHOD constructor .
    mv_ = iv_.
  ENDMETHOD.


  METHOD new.
    ro_ ?= NEW zcl_llm_00_json_mock( iv_ ).
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = mv_.
  ENDMETHOD.
ENDCLASS.
