CLASS zcl_llm_00_kv DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES:
      BEGIN OF ts_,
        k TYPE string,
        v TYPE string,
      END OF ts_ .
    TYPES:
      tt_ TYPE HASHED TABLE OF ts_ WITH UNIQUE KEY k .

    CLASS-METHODS new
      IMPORTING it_        TYPE tt_ OPTIONAL
      RETURNING VALUE(ro_) TYPE REF TO zcl_llm_00_kv .
    METHODS append
      IMPORTING
        !is_       TYPE ts_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_kv .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_ TYPE tt_.
    METHODS constructor
      IMPORTING it_ TYPE tt_ OPTIONAL.

ENDCLASS.



CLASS ZCL_LLM_00_KV IMPLEMENTATION.


  METHOD append.
    IF line_exists( mt_[ k = is_-k ] ).
      RETURN.
    ENDIF.

    INSERT is_ INTO TABLE mt_.
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
