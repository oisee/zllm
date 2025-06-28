CLASS zcl_llm_00_parameters DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_llm_00_json .

    ALIASES to_json
      FOR zif_llm_00_json~to_json .

    TYPES ts_ TYPE zif_llm_00_types=>ts_parameter .
    TYPES ts_par TYPE zif_llm_00_function=>ts_par .

    CLASS-METHODS new
      IMPORTING
        !is_       TYPE ts_par
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_parameters
      RAISING
        zcx_s .
    METHODS convert_type
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rv_) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA gv_msg TYPE string .

    METHODS constructor
      IMPORTING
        !is_ TYPE ts_par
      RAISING
        zcx_s .
    DATA: ms_par TYPE ts_par.
    DATA: ms_ TYPE ts_.
ENDCLASS.



CLASS ZCL_LLM_00_PARAMETERS IMPLEMENTATION.


  METHOD constructor.
    ms_par = is_.
    ms_-type = 'object'.
    ms_-required = VALUE #( FOR ls_ IN ms_par-imp WHERE ( optional IS INITIAL )
     ( CONV #( ls_-parameter ) )
    ).

    ms_-properties = VALUE #( FOR ls_ IN ms_par-imp WHERE ( parameter IS NOT INITIAL )
     ( k = ls_-parameter
       v = zcl_llm_00_kv=>new( VALUE #( ( k = 'type'        v = me->convert_type( to_lower( ls_-typ ) ) )
                                        ( k = 'description' v = VALUE #( ms_par-doc[ parameter = ls_-parameter ]-stext OPTIONAL ) )
                                      )
                             )
      )
    ) .

  ENDMETHOD.


  METHOD convert_type.

    CASE iv_.
      WHEN 'i'.
        rv_ = 'integer'.
      WHEN 'string'.
        rv_ = iv_.
      WHEN 'sap_bool' OR 'abap_bool' OR 'boolean'.
        rv_ = 'boolean'.
      WHEN 'f'.
        rv_ = 'number'.
      WHEN OTHERS.
        "rv_ = 'not supported'.
        " throw exception
        RETURN.
    ENDCASE.

*ts_ => type: "object"
*tt_ => type: "array"
*date => type: "string", format: "date-time"
*string
*number
*boolean
*null/empty
*object
*array
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( is_ ).
  ENDMETHOD.


  METHOD zif_llm_00_json~to_json.
    rv_ = zcl_llm_00_json=>to_json( ms_ ).
  ENDMETHOD.
ENDCLASS.
