CLASS zcl_llm_00_array DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES: ts_ TYPE REF TO zif_llm_00_json.
    TYPES:
      tt_ TYPE STANDARD TABLE OF ts_ WITH DEFAULT KEY.

    CLASS-METHODS new
      IMPORTING
        !it_       TYPE tt_ OPTIONAL
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_array .
    METHODS append
      IMPORTING
        !io_       TYPE REF TO zif_llm_00_json
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_array .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_ TYPE tt_.
    METHODS constructor
      IMPORTING it_ TYPE tt_ OPTIONAL.

ENDCLASS.



CLASS ZCL_LLM_00_ARRAY IMPLEMENTATION.


  METHOD append.
    APPEND io_ TO mt_.
  ENDMETHOD.


  METHOD constructor .
    mt_ = it_.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( it_ ).
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = zcl_llm_00_json=>to_json( mt_ ).
  ENDMETHOD.
ENDCLASS.
