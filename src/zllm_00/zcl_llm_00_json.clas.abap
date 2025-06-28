CLASS zcl_llm_00_json DEFINITION
  PUBLIC
  INHERITING FROM /ui2/cl_json
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_kv,
        k TYPE string,
        v TYPE string,
      END OF ts_kv .
    TYPES:
      tt_kv TYPE STANDARD TABLE OF ts_kv WITH KEY k .

    CLASS-METHODS to_json
      IMPORTING
        !data             TYPE data
        !compress         TYPE bool DEFAULT 'X'
        !name             TYPE string OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-low_case
        !type_descr       TYPE REF TO cl_abap_typedescr OPTIONAL
        !assoc_arrays     TYPE bool DEFAULT 'X'
        !ts_as_iso8601    TYPE bool DEFAULT c_bool-false
        !expand_includes  TYPE bool DEFAULT c_bool-true
        !assoc_arrays_opt TYPE bool DEFAULT 'X'
        !numc_as_string   TYPE bool DEFAULT c_bool-false
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
        !format_output    TYPE bool DEFAULT c_bool-false
        !hex_as_base64    TYPE bool DEFAULT c_bool-true
      RETURNING
        VALUE(r_json)     TYPE json .
    CLASS-METHODS from_json
      IMPORTING
        !json             TYPE json OPTIONAL
        !jsonx            TYPE xstring OPTIONAL
        !pretty_name      TYPE pretty_name_mode DEFAULT pretty_mode-low_case
        !assoc_arrays     TYPE bool DEFAULT 'X'
        !assoc_arrays_opt TYPE bool DEFAULT 'X'
        !name_mappings    TYPE name_mappings OPTIONAL
        !conversion_exits TYPE bool DEFAULT c_bool-false
        !hex_as_base64    TYPE bool DEFAULT c_bool-true
      CHANGING
        !data             TYPE data .
    CLASS-METHODS is_structure
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS has_field
      IMPORTING
        !ir_       TYPE REF TO data
        !iv_field  TYPE string DEFAULT 'RESULT'
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS field_unpacked
      IMPORTING
        !ir_       TYPE REF TO data
        !iv_field  TYPE string DEFAULT 'RESULT'
      EXPORTING
        !er_       TYPE REF TO data
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS is_string
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS deref
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rr_) TYPE REF TO data .
    CLASS-METHODS is_eventually_table
      IMPORTING
        !ir_       TYPE REF TO data
      EXPORTING
        !er_       TYPE REF TO data
        !ev_type   TYPE abap_abstypename
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS is_table
      IMPORTING
        !ir_       TYPE REF TO data
      EXPORTING
        !er_       TYPE REF TO data
        !ev_type   TYPE abap_abstypename
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS is_reference
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS is_reference_to_table
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rv_) TYPE sap_bool .
    CLASS-METHODS flatten_dref
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rr_) TYPE REF TO data .
    CLASS-METHODS to_kv
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rt_) TYPE tt_kv .
    CLASS-METHODS js_to_dref
      IMPORTING
        !iv_       TYPE string
      RETURNING
        VALUE(rr_) TYPE REF TO data .
  PROTECTED SECTION.

    METHODS dump_int
        REDEFINITION .
  PRIVATE SECTION.

    CLASS-METHODS flatten_table
      IMPORTING
        !it_       TYPE ANY TABLE
      RETURNING
        VALUE(rr_) TYPE REF TO data .
    CLASS-METHODS _flatten_dref
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rr_) TYPE REF TO data .
    CLASS-METHODS _flatten_table
      IMPORTING
        !ir_       TYPE REF TO data
      RETURNING
        VALUE(rr_) TYPE REF TO data .
ENDCLASS.



CLASS ZCL_LLM_00_JSON IMPLEMENTATION.


  METHOD deref.
    rr_ = ir_.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
      WHEN cl_abap_typedescr=>kind_struct.
      WHEN cl_abap_typedescr=>kind_table.
      WHEN cl_abap_typedescr=>kind_ref.
        rr_ = deref( <fs_f> ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD dump_int.
    DATA:lo_obj_ref    TYPE REF TO object.

    IF type_descr->kind = cl_abap_typedescr=>kind_ref AND
       data IS NOT INITIAL AND
       type_descr->type_kind EQ cl_abap_typedescr=>typekind_oref.
      lo_obj_ref ?= data.
      IF lo_obj_ref IS INSTANCE OF zif_llm_00_json.
        DATA:lo_json TYPE REF TO zif_llm_00_json.
        lo_json ?= lo_obj_ref.
        r_json = lo_json->to_json( ).
        RETURN.
      ENDIF.
    ENDIF.
    r_json = super->dump_int(
      EXPORTING
        data       = data
        type_descr = type_descr
        convexit   = convexit
        level      = level
    ).

  ENDMETHOD.


  METHOD field_unpacked.
    IF NOT is_structure( ir_ ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_s> TYPE any. "structure
    FIELD-SYMBOLS: <fs_f> TYPE any. "field
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_s>.
    IF sy-subrc NE 0 OR <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT iv_field OF STRUCTURE <fs_s> TO <fs_f>.
    IF sy-subrc NE 0 OR <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    IF er_ IS REQUESTED.
      er_ = REF #( <fs_f> ).
    ENDIF.
    rv_ = abap_true.
  ENDMETHOD.


  METHOD flatten_dref.
    rr_ = ir_.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        IF is_eventually_table(
             EXPORTING ir_ = REF #( <fs_f> )
             IMPORTING er_ = rr_
         ).
          rr_ = _flatten_table( ir_ ).
        ENDIF.
      WHEN cl_abap_typedescr=>kind_ref.
        IF is_eventually_table(
             EXPORTING ir_ = <fs_f>
             IMPORTING er_ = rr_
         ).
          rr_ = _flatten_table( ir_ ).
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD flatten_table.
    rr_ = REF #( it_ ).
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF rr_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN rr_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        IF is_eventually_table(
             EXPORTING ir_ = REF #( <fs_f> )
             IMPORTING er_ = rr_
         ).
          DATA(lr_) = _flatten_table( rr_ ).
          rr_ = lr_.
        ENDIF.
      WHEN cl_abap_typedescr=>kind_ref.
        IF is_eventually_table(
             EXPORTING ir_ = <fs_f>
             IMPORTING er_ = rr_
         ).
          lr_ = _flatten_table( rr_ ).
          rr_ = lr_.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD from_json.

    DATA: lo_json  TYPE REF TO zcl_llm_00_json.


    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

      CREATE OBJECT lo_json
        EXPORTING
          pretty_name      = pretty_name
          name_mappings    = name_mappings
          assoc_arrays     = assoc_arrays
          conversion_exits = conversion_exits
          hex_as_base64    = hex_as_base64
          assoc_arrays_opt = assoc_arrays_opt.

      TRY .
          lo_json->deserialize_int( EXPORTING json = json jsonx = jsonx CHANGING data = data ).
        CATCH cx_sy_move_cast_error.                    "#EC NO_HANDLER
      ENDTRY.

    ENDIF.
  ENDMETHOD.


  METHOD has_field.
    IF NOT is_structure( ir_ ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_s> TYPE any. "structure
    FIELD-SYMBOLS: <fs_f> TYPE any. "field
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_s>.
    IF sy-subrc NE 0 OR <fs_s> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT iv_field OF STRUCTURE <fs_s> TO <fs_f>.
    IF sy-subrc NE 0 OR <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rv_ = abap_true.
  ENDMETHOD.


  METHOD is_eventually_table.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        IF er_ IS REQUESTED.
          er_ = REF #( <fs_f> ).
        ENDIF.
        IF ev_type IS REQUESTED.
          ev_type = lo_td->absolute_name.
        ENDIF.
        rv_ = abap_true.
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        DATA lr_     LIKE er_.
        DATA lv_type LIKE ev_type.
        rv_ = is_table(
                EXPORTING
                  ir_     = <fs_f>
                IMPORTING
                  er_     = lr_
                  ev_type = lv_type
              ).
        IF er_ IS REQUESTED.
          er_ = lr_.
        ENDIF.
        IF ev_type IS REQUESTED.
          ev_type = lv_type.
        ENDIF.
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD is_reference.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
      WHEN cl_abap_typedescr=>kind_struct.
      WHEN cl_abap_typedescr=>kind_table.
      WHEN cl_abap_typedescr=>kind_ref.
        "rv_ = is_reference( <fs_f> ).
        rv_ = abap_true.
        RETURN.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD is_reference_to_table.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
      WHEN cl_abap_typedescr=>kind_struct.
      WHEN cl_abap_typedescr=>kind_table.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = is_table( <fs_f> ).
        RETURN.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD is_string.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        IF lo_td->type_kind = cl_abap_typedescr=>typekind_char      OR
           lo_td->type_kind = cl_abap_typedescr=>typekind_clike     OR
           lo_td->type_kind = cl_abap_typedescr=>typekind_csequence OR
           lo_td->type_kind = cl_abap_typedescr=>typekind_string.
          rv_ = abap_true.
        ENDIF.
      WHEN cl_abap_typedescr=>kind_struct.
      WHEN cl_abap_typedescr=>kind_table.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = is_string( <fs_f> ).
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD is_structure.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        rv_ = abap_true.
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = is_structure( <fs_f> ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD is_table.
    FIELD-SYMBOLS: <fs_f> TYPE any.
    IF ir_ IS NOT BOUND.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    ASSIGN ir_->* TO <fs_f>.
    IF <fs_f> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA: lo_td TYPE REF TO cl_abap_typedescr.
    lo_td ?= cl_abap_typedescr=>describe_by_data( <fs_f> ).
    CASE lo_td->kind.
      WHEN cl_abap_typedescr=>kind_elem.
        RETURN.
      WHEN cl_abap_typedescr=>kind_struct.
        RETURN.
      WHEN cl_abap_typedescr=>kind_table.
        IF er_ IS REQUESTED.
          er_ = REF #( <fs_f> ).
        ENDIF.
        IF ev_type IS REQUESTED.
          ev_type = lo_td->absolute_name.
        ENDIF.
        rv_ = abap_true.
        RETURN.
      WHEN cl_abap_typedescr=>kind_ref.
        rv_ = is_table(
                EXPORTING
                  ir_     = <fs_f>
                IMPORTING
                  er_     = er_
                  ev_type = ev_type
              ).
        RETURN.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD js_to_dref.
    IF iv_ IS INITIAL.
      rr_ = REF #( iv_ ).
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    rr_ = generate( iv_ ).
    IF rr_ IS NOT BOUND.
      rr_ = REF #( iv_ ).
    ENDIF.

  ENDMETHOD.


  METHOD to_json.

    " **********************************************************************
    " Usage examples and documentation can be found on SCN:
    " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
    " **********************************************************************  "

    DATA: lo_json  TYPE REF TO zcl_llm_00_json.

    CREATE OBJECT lo_json
      EXPORTING
        compress         = compress
        pretty_name      = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = assoc_arrays
        assoc_arrays_opt = assoc_arrays_opt
        expand_includes  = expand_includes
        numc_as_string   = numc_as_string
        conversion_exits = conversion_exits
        format_output    = format_output
        hex_as_base64    = hex_as_base64
        ts_as_iso8601    = ts_as_iso8601.

    r_json = lo_json->serialize_int( name = name data = data type_descr = type_descr ).
  ENDMETHOD.


  METHOD to_kv.

    DATA(lr_) = flatten_dref( ir_ ).
*   DATA(lv_json) = to_json( ir_ ).
    DATA(lv_json) = to_json( lr_ ).
    from_json(
       EXPORTING
         json             = lv_json
*        jsonx            = jsonx
*        pretty_name      = pretty_name
*        assoc_arrays     = assoc_arrays
*        assoc_arrays_opt = assoc_arrays_opt
*        name_mappings    = name_mappings
*        conversion_exits = conversion_exits
*        hex_as_base64    = hex_as_base64
      CHANGING
        data             = rt_
    ).

  ENDMETHOD.


  METHOD _flatten_dref.
    rr_ = ir_.
    DATA: lr_a TYPE REF TO data.
    DATA: lr_b TYPE REF TO data.
    FIELD-SYMBOLS: <fs_a> TYPE any.
    FIELD-SYMBOLS: <fs_t1> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l1> TYPE any. "line
    FIELD-SYMBOLS: <fs_r1> TYPE REF TO data.
    FIELD-SYMBOLS: <fs_t2> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l2> TYPE any. "line
    FIELD-SYMBOLS: <fs_ta> TYPE STANDARD TABLE. "accumulator table for flattening
    FIELD-SYMBOLS: <fs_tb> TYPE STANDARD TABLE. "accumulator table for flattening
    ASSIGN ir_->* TO <fs_a>.
    IF <fs_a> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_a> ).
    CASE lo_type->kind.
      WHEN lo_type->kind_elem.
        RETURN.
      WHEN lo_type->kind_ref.
        RETURN.
      WHEN lo_type->kind_struct.
        RETURN.
      WHEN lo_type->kind_table.
        IF <fs_a> IS INITIAL.
          RETURN.
        ENDIF.
        ASSIGN <fs_a> TO <fs_t1>.
        READ TABLE <fs_t1> ASSIGNING <fs_l1> INDEX 1.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        DATA: lv_type_a TYPE abap_abstypename.
        DATA: lv_type_b TYPE abap_abstypename.
        IF zcl_llm_00_json=>is_eventually_table(
            EXPORTING ir_ = <fs_l1>
            IMPORTING er_ = lr_a ev_type = lv_type_a
          ).
          LOOP AT <fs_t1> ASSIGNING <fs_l1>.
            DATA(lv_tabix) = sy-tabix.
            IF lv_tabix = 1.
              ASSIGN lr_a->* TO <fs_ta>. "set accumulator table
            ELSE.
              zcl_llm_00_json=>is_eventually_table(
                EXPORTING ir_ = <fs_l1>
                IMPORTING er_ = lr_b ev_type = lv_type_b
              ).
              ASSIGN lr_b->* TO <fs_tb>. "set second table
              IF lv_type_a = lv_type_b AND
                 lr_a     NE lr_b.
                APPEND LINES OF <fs_tb> TO <fs_ta>.
                REFRESH <fs_tb>.
                FREE <fs_tb>.
                DELETE <fs_t1> INDEX lv_tabix.
              ELSE.
                lr_a = lr_b.
                ASSIGN lr_b->* TO <fs_ta>. "set accumulator table to the next table (of different type)
                lv_type_a = lv_type_b.
              ENDIF.
            ENDIF.
          ENDLOOP.
          rr_ = ir_. "_to_string( REF #( <fs_ta> ) ).
        ELSE.
          RETURN.
        ENDIF.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD _flatten_table.
    rr_ = ir_.
    DATA: lr_a TYPE REF TO data.
    DATA: lr_b TYPE REF TO data.
    FIELD-SYMBOLS: <fs_a> TYPE any.
    FIELD-SYMBOLS: <fs_t1> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l1> TYPE any. "line
    FIELD-SYMBOLS: <fs_r1> TYPE REF TO data.
    FIELD-SYMBOLS: <fs_t2> TYPE STANDARD TABLE. "line
    FIELD-SYMBOLS: <fs_l2> TYPE any. "line
    FIELD-SYMBOLS: <fs_ta> TYPE STANDARD TABLE. "accumulator table for flattening
    FIELD-SYMBOLS: <fs_la> TYPE any.
    FIELD-SYMBOLS: <fs_tb> TYPE STANDARD TABLE. "accumulator table for flattening
    ASSIGN ir_->* TO <fs_a>.
    IF <fs_a> IS NOT ASSIGNED.
      RETURN.
    ENDIF.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_a> ).
    CASE lo_type->kind.
      WHEN lo_type->kind_elem.
        RETURN.
      WHEN lo_type->kind_ref.
        RETURN.
      WHEN lo_type->kind_struct.
        RETURN.
      WHEN lo_type->kind_table.
        IF <fs_a> IS INITIAL.
          RETURN.
        ENDIF.
        ASSIGN <fs_a> TO <fs_t1>.
        READ TABLE <fs_t1> ASSIGNING <fs_l1> INDEX 1.
        IF sy-subrc NE 0.
          RETURN.
        ENDIF.
        DATA: lv_type_a TYPE abap_abstypename.
        DATA: lv_type_b TYPE abap_abstypename.
        IF zcl_llm_00_json=>is_eventually_table(
            EXPORTING ir_ = REF #( <fs_l1> )
            IMPORTING er_ = lr_a ev_type = lv_type_a
          ).
          LOOP AT <fs_t1> ASSIGNING <fs_l1>.
            DATA(lv_tabix) = sy-tabix.
            IF lv_tabix = 1.
              ASSIGN lr_a->* TO <fs_ta>. "set accumulator table
            ELSE.
              zcl_llm_00_json=>is_eventually_table(
                EXPORTING ir_ = REF #( <fs_l1> )
                IMPORTING er_ = lr_b ev_type = lv_type_b
              ).
              ASSIGN lr_b->* TO <fs_tb>. "set second table
              IF lv_type_a = lv_type_b AND
                 lr_a     NE lr_b.
                APPEND LINES OF <fs_tb> TO <fs_ta>.
                REFRESH <fs_tb>.
                FREE <fs_tb>.
                DELETE <fs_t1> INDEX lv_tabix.
              ELSE.
                lr_a = lr_b.
                ASSIGN lr_b->* TO <fs_ta>. "set accumulator table to the next table (of different type)
                lv_type_a = lv_type_b.
              ENDIF.
            ENDIF.
          ENDLOOP.
          rr_ = deref( ir_ ). "_to_string( REF #( <fs_ta> ) ).
          ASSIGN rr_->* TO <fs_ta>. "set accumulator table.
          IF sy-subrc NE 0.
            RETURN.
          ENDIF.
          IF lines( <fs_ta> ) = 1.
            READ TABLE <fs_ta> ASSIGNING <fs_la> INDEX 1.
            IF sy-subrc NE 0.
              RETURN.
            ENDIF.
            rr_ = deref( REF #( <fs_la> ) ).
          ENDIF.
        ELSE.
          RETURN.
        ENDIF.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
