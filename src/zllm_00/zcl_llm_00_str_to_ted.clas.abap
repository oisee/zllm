CLASS zcl_llm_00_str_to_ted DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ts_spl TYPE zcl_llm_00_spl=>ts_ .
    TYPES tt_spl TYPE zcl_llm_00_spl=>tt_ .
    TYPES:
      BEGIN OF ts_,
        id     TYPE string,
        name   TYPE string,
        editor TYPE REF TO cl_gui_textedit,
        gui    TYPE REF TO cl_gui_container,
        eh     TYPE REF TO zcl_llm_00_ted_event_handler,
        text   TYPE REF TO string,
      END OF ts_ .
    TYPES:
      tt_ TYPE STANDARD TABLE OF ts_ WITH DEFAULT KEY .

    CLASS-METHODS new
      IMPORTING
        !io_gui       TYPE REF TO cl_gui_container
        !ir_str       TYPE REF TO data
        !itr_         TYPE zcl_llm_00_tvarvc=>ttr_ OPTIONAL
        !iv_threshold TYPE i DEFAULT 4
        !iv_max       TYPE i DEFAULT 24
      RETURNING
        VALUE(ro_)    TYPE REF TO zcl_llm_00_str_to_ted
      RAISING
        zcx_s .
    METHODS get_ted
      IMPORTING
        !itr_      TYPE zcl_llm_00_tvarvc=>ttr_ OPTIONAL
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS update_from_ver
      IMPORTING
        !ir_str TYPE REF TO data
      RAISING
        zcx_s .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mo_gui TYPE REF TO cl_gui_container .
    DATA mr_str TYPE REF TO data .
    DATA mt_ TYPE tt_ .
    DATA mv_msg TYPE string .
    DATA mt_comp TYPE abap_compdescr_tab .
    DATA mt_spl TYPE tt_spl .
    DATA mv_dummy TYPE string .
    DATA mv_max   TYPE string .

ENDCLASS.



CLASS ZCL_LLM_00_STR_TO_TED IMPLEMENTATION.


  METHOD get_ted.
    rt_ = mt_.
    IF mt_ IS NOT INITIAL.
      DELETE rt_ WHERE name NOT IN itr_.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( ).
    ro_->mo_gui = io_gui.
    ro_->mr_str = ir_str.
    ro_->mv_max = iv_max.
*--------------------------------------------------------------------
    DATA: lo_ TYPE REF TO cl_abap_structdescr.
    lo_ ?= cl_abap_typedescr=>describe_by_data_ref( ro_->mr_str ). " describe_by_data( ls_t001 ).

    ro_->mt_comp = lo_->components.

    LOOP AT ro_->mt_comp REFERENCE INTO DATA(lr_comp) WHERE type_kind = 'g'. "string
      APPEND INITIAL LINE TO ro_->mt_ REFERENCE INTO DATA(lr_).
      lr_->name = lr_comp->name.
    ENDLOOP.

    DATA(lv_rows) = lines( ro_->mt_ ).
    IF lv_rows > ro_->mv_max.
      DATA: lt_ LIKE ro_->mt_.
      APPEND LINES OF ro_->mt_ FROM 1 TO 12 TO lt_.
      ro_->mt_ = lt_.
    ENDIF.

*--------------------------------------------------------------------
    IF ro_->mt_ IS INITIAL.
      MESSAGE e002(zcol_177) WITH lo_->absolute_name INTO ro_->mv_msg.
      zcx_s=>raise( sy ).
    ENDIF.
*--------------------------------------------------------------------
    DATA(lo_spl) = zcl_llm_00_spl=>new( ro_->mo_gui ).
    ro_->mt_spl = lo_spl->split( '@' ).

    DATA lt_event TYPE cntl_simple_events.
    lt_event  = VALUE #(
      ( eventid = cl_gui_textedit=>event_f1           appl_event = ' ' )
      ( eventid = cl_gui_textedit=>event_double_click appl_event = ' ' )
    ).

    DATA(lv_i) = 0.
    LOOP AT ro_->mt_ REFERENCE INTO lr_.
      ADD 1 TO lv_i.
      DATA(lr_spl) = REF #( ro_->mt_spl[ lv_i ] OPTIONAL ).
      IF lr_spl IS NOT BOUND.
        CONTINUE.
      ENDIF.

      DATA(lv_name) = lr_->name.
      lr_->* = CORRESPONDING #( lr_spl->* ).
      lr_->name = lv_name.

      lr_->id   = lr_spl->id.

      FIELD-SYMBOLS: <fs_str> TYPE any.
      FIELD-SYMBOLS: <fs_fld> TYPE any.
      ASSIGN ro_->mr_str->* TO <fs_str>.
      ASSIGN COMPONENT lr_->name OF STRUCTURE <fs_str> TO <fs_fld>.
      IF <fs_fld> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      lr_->text   = REF #( <fs_fld> ).
      IF lr_->name IN itr_.
        lr_->editor = NEW #( lr_->gui ).
        lr_->editor->set_registered_events( lt_event ).
        lr_->editor->set_toolbar_mode( 0 ).
        lr_->editor->set_status_text(
          EXPORTING
            status_text            = CONV text256( lr_->name ) " status text
          EXCEPTIONS
            error_cntl_call_method = 1           " Error while setting status text within TextEdit Control!
            OTHERS                 = 2
        ).
        IF sy-subrc <> 0.
          "zcl_cpu=>ok( ).
        ENDIF.

        IF lr_->text IS BOUND.
          lr_->editor->set_textstream( lr_->text->* ).
        ELSE.
          lr_->editor->set_textstream( ro_->mv_dummy ).
        ENDIF.

        lr_->eh = zcl_llm_00_ted_event_handler=>new( io_ted = lr_->editor ir_text = lr_->text ).
        SET HANDLER
          lr_->eh->handle_f1
          lr_->eh->handle_double_click
          FOR lr_->editor.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_from_ver.
    mr_str = ir_str.
*--------------------------------------------------------------------
    DATA: lo_ TYPE REF TO cl_abap_structdescr.
    lo_ ?= cl_abap_typedescr=>describe_by_data_ref( mr_str ). " describe_by_data( ls_t001 ).

*--------------------------------------------------------------------
    IF me->mt_ IS INITIAL.
      MESSAGE e002(zcol_177) WITH lo_->absolute_name INTO me->mv_msg.
      zcx_s=>raise( sy ).
    ENDIF.
*--------------------------------------------------------------------
*--------------------------------------------------------------------
    LOOP AT me->mt_ REFERENCE INTO DATA(lr_).
      DATA(lv_name) = lr_->name.
      FIELD-SYMBOLS: <fs_str> TYPE any.
      FIELD-SYMBOLS: <fs_fld> TYPE any.
      ASSIGN me->mr_str->* TO <fs_str>.
      ASSIGN COMPONENT lr_->name OF STRUCTURE <fs_str> TO <fs_fld>.
      IF <fs_fld> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      lr_->text   = REF #( <fs_fld> ).
      IF lr_->text IS BOUND.
        lr_->editor->set_textstream( lr_->text->* ).
      ELSE.
        lr_->editor->set_textstream( me->mv_dummy ).
      ENDIF.

      lr_->eh->update_from_ref( lr_->text ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
