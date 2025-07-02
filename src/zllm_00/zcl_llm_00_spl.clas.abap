CLASS zcl_llm_00_spl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_parent,
        id  TYPE string,
        gui TYPE REF TO cl_gui_container,
      END OF ts_parent .
    TYPES:
      BEGIN OF ts_,
        id           TYPE string,
        gui          TYPE REF TO cl_gui_container,
        parent       TYPE ts_parent,
        has_children TYPE sap_bool,
      END OF ts_ .
    TYPES:
      tt_ TYPE STANDARD TABLE OF ts_ WITH KEY id .
    TYPES:
      BEGIN OF ts_split,
        id     TYPE string,
        gui    TYPE REF TO cl_gui_splitter_container,
        parent TYPE ts_parent,
      END OF ts_split .
    TYPES:
      tt_split TYPE STANDARD TABLE OF ts_split WITH KEY id .

    CLASS-METHODS class_constructor .
    CLASS-METHODS new
      IMPORTING
        !io_       TYPE REF TO cl_gui_container DEFAULT cl_gui_container=>default_screen
        !iv_mode   TYPE string DEFAULT 'T'
          PREFERRED PARAMETER io_
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_llm_00_spl .
    METHODS split
      IMPORTING
        !iv_       TYPE string DEFAULT '-'
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS split_flex_into
      IMPORTING
        !iv_                     TYPE i
        !iv_split_vertical_first TYPE sap_bool DEFAULT 'X'
        !iv_threshold            TYPE i DEFAULT 5
      RETURNING
        VALUE(rt_)               TYPE tt_ .
    METHODS split_h_into
      IMPORTING
        !iv_       TYPE i
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS get_leafs
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS get_nodes
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS split_v_into
      IMPORTING
        !iv_       TYPE i
      RETURNING
        VALUE(rt_) TYPE tt_ .
    METHODS head_splitter
      RETURNING
        VALUE(rs_) TYPE ts_split .
  PROTECTED SECTION.
private section.

    "DATA ms_parent TYPE ts_ .
  data MS_ type TS_ .
  data MT_ type TT_ .
  data MT_SPLIT type TT_SPLIT .
  data MV_MODE type STRING .

  methods SPLIT_V
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_H_THIRD
    importing
      !IS_ type TS_
      !IV_HEIGHT type I default 33
    returning
      value(RT_) type TT_ .
  methods SPLIT_V_THIRD
    importing
      !IS_ type TS_
      !IV_WIDTH type I default 33
    returning
      value(RT_) type TT_ .
  methods SPLIT_H
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_1X3
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_2X2
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_3X3
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_5X4
    importing
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods SPLIT_H_N
    importing
      !IV_ type I
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
  methods HEAD
    importing
      !IT_ type TT_
    returning
      value(RS_) type TS_ .
  methods TAIL
    importing
      !IT_ type TT_
    returning
      value(RS_) type TS_ .
  methods ADJUST
    importing
      !IS_ type TS_
    changing
      !CT_ type TT_
    returning
      value(RS_) type TS_ .
  methods SPLIT_V_N
    importing
      !IV_ type I
      !IS_ type TS_
    returning
      value(RT_) type TT_ .
ENDCLASS.



CLASS ZCL_LLM_00_SPL IMPLEMENTATION.


  METHOD adjust.

    DATA(lr_parent) = REF #( mt_[ id = is_-id ] OPTIONAL ).
    IF lr_parent IS BOUND.
      lr_parent->has_children = 'X'.
    ENDIF.

    LOOP AT ct_ REFERENCE INTO DATA(lr_).
      lr_->parent = CORRESPONDING #( is_ ).
    ENDLOOP.

    IF mv_mode = 'T'.
      rs_ = me->tail( ct_ ).
    ELSE.
      rs_ = me->head( ct_ ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
  ENDMETHOD.


  METHOD get_leafs.

    rt_ = mt_.
    DELETE rt_ WHERE has_children = 'X'.

  ENDMETHOD.


  METHOD get_nodes.

    rt_ = mt_.
    DELETE rt_ WHERE has_children = ' '.

  ENDMETHOD.


  METHOD head.

    IF it_[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA lv_ TYPE i.
    WHILE 1 = 1.
      ADD 1 TO lv_.
      DATA(lr_) = REF #( it_[ lv_ ] OPTIONAL ).
      IF lr_ IS NOT BOUND.
        RETURN.
      ENDIF.

      rs_ = lr_->*.

      IF lr_->has_children IS INITIAL.
        RETURN.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD head_splitter.

    IF mt_split[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lr_) = REF #( mt_split[ 1 ] OPTIONAL ).
    IF lr_ IS NOT BOUND.
      RETURN.
    ENDIF.
    rs_ = lr_->*.

  ENDMETHOD.


  METHOD new.

    ro_ = NEW #( ).
    ro_->ms_-id = '0'.
    ro_->ms_-gui = io_.
    ro_->mv_mode = iv_mode.

  ENDMETHOD.


  METHOD split.
    IF iv_ IS INITIAL.
      rt_ = me->split( '-' ). "default: horizontal split
      RETURN.
    ENDIF.
*--------------------------------------------------------------------
    DATA(lv_) = to_upper( iv_ ).

    DATA:lv_i TYPE i.
    DATA(lv_len) = strlen( iv_ ).

    WHILE lv_i < lv_len.
      DATA(lv_c) = lv_+lv_i(1).
      CASE lv_c.
        WHEN '!'.
          APPEND LINES OF me->split_v_third( ms_ ) TO mt_.
        WHEN '"'.
          APPEND LINES OF me->split_v_n( is_ = ms_ iv_ = 3 ) TO mt_.
        WHEN '^'.
          APPEND LINES OF me->split_h_third( ms_ ) TO mt_.
        WHEN '~'.
          DATA(lv_percent_i) = 10.
          DATA(lv_next_offset)  = lv_i + 3.
          DATA(lv_next_percent) = lv_i + 1.
          IF lv_len >= lv_next_offset.
            DATA(lv_percent) = iv_+lv_next_percent(2).
            IF lv_percent CO '0123456789'.
              lv_percent_i = CONV i( lv_percent ).
              lv_i = lv_i + 2.
            ENDIF.
          ENDIF.
          lv_i = lv_i + 1.
          APPEND LINES OF me->split_h_third( is_ = ms_ iv_height = lv_percent_i ) TO mt_.
          CONTINUE.
        WHEN '%'.
          lv_percent_i = 10.
          lv_next_offset  = lv_i + 3.
          lv_next_percent = lv_i + 1.
          IF lv_len >= lv_next_offset.
            lv_percent = iv_+lv_next_percent(2).
            IF lv_percent CO '0123456789'.
              lv_percent_i = CONV i( lv_percent ).
              lv_i = lv_i + 2.
            ENDIF.
          ENDIF.
          lv_i = lv_i + 1.
          APPEND LINES OF me->split_v_third( is_ = ms_ iv_width  = lv_percent_i ) TO mt_.
          CONTINUE.
        WHEN '_'.
          APPEND LINES OF me->split_h_third( is_ = ms_ iv_height = '67' ) TO mt_.
        WHEN '-'.
          APPEND LINES OF me->split_h(       ms_ ) TO mt_.
        WHEN '|'.
          APPEND LINES OF me->split_v(       ms_ ) TO mt_.
        WHEN '='.
          APPEND LINES OF me->split_1x3(     ms_ ) TO mt_.
        WHEN '+'.
          APPEND LINES OF me->split_2x2(     ms_ ) TO mt_.
        WHEN '#'.
          APPEND LINES OF me->split_3x3(     ms_ ) TO mt_.
        WHEN '@'.
          APPEND LINES OF me->split_5x4(     ms_ ) TO mt_.
        WHEN OTHERS.
          IF lv_c CO '123456789'.
            DATA(lv_num) = CONV i( lv_c ).
            APPEND LINES OF me->split_h_n( iv_ = lv_num
                                           is_ = ms_ ) TO mt_.
          ELSEIF lv_c CO 'HT'.
            CASE lv_c.
              WHEN 'H'.
                ms_ = me->head( mt_ ).
              WHEN 'T'.
                ms_ = me->tail( mt_ ).
              WHEN OTHERS.
            ENDCASE.
          ENDIF.
      ENDCASE.
      ADD 1 TO lv_i.
    ENDWHILE.

    rt_ = get_leafs( ).
  ENDMETHOD.


  METHOD split_1x3.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 3
      columns = 1
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_1) = lo_->get_container(
        row       = 1
        column    = 1
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 2
        column    = 1
    ).

    DATA(lo_3) = lo_->get_container(
        row       = 3
        column    = 1
    ).

    APPEND VALUE #( id = |{ is_-id }.1T| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2M| gui = lo_2 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.3B| gui = lo_3 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_2x2.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 2
      columns = 2
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_1) = lo_->get_container(
        row       = 1
        column    = 1
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 1
        column    = 2
    ).

    DATA(lo_3) = lo_->get_container(
        row       = 2
        column    = 1
    ).

    DATA(lo_4) = lo_->get_container(
        row       = 2
        column    = 2
    ).

    APPEND VALUE #( id = |{ is_-id }.1TL| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2TR| gui = lo_2 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.3BL| gui = lo_3 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.4BR| gui = lo_4 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_3x3.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 3
      columns = 3
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_1) = lo_->get_container(
        row       = 1
        column    = 1
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 1
        column    = 2
    ).

    DATA(lo_3) = lo_->get_container(
        row       = 1
        column    = 3
    ).

    DATA(lo_4) = lo_->get_container(
        row       = 2
        column    = 1
    ).

    DATA(lo_5) = lo_->get_container(
        row       = 2
        column    = 2
    ).

    DATA(lo_6) = lo_->get_container(
        row       = 2
        column    = 3
    ).

    DATA(lo_7) = lo_->get_container(
        row       = 3
        column    = 1
    ).

    DATA(lo_8) = lo_->get_container(
        row       = 3
        column    = 2
    ).

    DATA(lo_9) = lo_->get_container(
        row       = 3
        column    = 3
    ).

    APPEND VALUE #( id = |{ is_-id }.1TL| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2TM| gui = lo_2 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.3TR| gui = lo_3 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.4ML| gui = lo_4 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.5MM| gui = lo_5 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.6MR| gui = lo_6 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.7BL| gui = lo_7 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.8BM| gui = lo_8 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.9BR| gui = lo_9 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_5x4.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows    = 5
      columns = 4
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_11) = lo_->get_container(
        row       = 1
        column    = 1
    ).

    DATA(lo_12) = lo_->get_container(
        row       = 1
        column    = 2
    ).

    DATA(lo_13) = lo_->get_container(
        row       = 1
        column    = 3
    ).

    DATA(lo_14) = lo_->get_container(
        row       = 1
        column    = 4
    ).

    DATA(lo_21) = lo_->get_container(
        row       = 2
        column    = 1
    ).

    DATA(lo_22) = lo_->get_container(
        row       = 2
        column    = 2
    ).

    DATA(lo_23) = lo_->get_container(
        row       = 2
        column    = 3
    ).

    DATA(lo_24) = lo_->get_container(
        row       = 2
        column    = 4
    ).

    DATA(lo_31) = lo_->get_container(
        row       = 3
        column    = 1
    ).

    DATA(lo_32) = lo_->get_container(
        row       = 3
        column    = 2
    ).

    DATA(lo_33) = lo_->get_container(
        row       = 3
        column    = 3
    ).

    DATA(lo_34) = lo_->get_container(
        row       = 3
        column    = 4
    ).

    DATA(lo_41) = lo_->get_container(
        row       = 4
        column    = 1
    ).

    DATA(lo_42) = lo_->get_container(
        row       = 4
        column    = 2
    ).

    DATA(lo_43) = lo_->get_container(
        row       = 4
        column    = 3
    ).

    DATA(lo_44) = lo_->get_container(
        row       = 4
        column    = 4
    ).

    DATA(lo_51) = lo_->get_container(
        row       = 5
        column    = 1
    ).

    DATA(lo_52) = lo_->get_container(
        row       = 5
        column    = 2
    ).

    DATA(lo_53) = lo_->get_container(
        row       = 5
        column    = 3
    ).

    DATA(lo_54) = lo_->get_container(
        row       = 5
        column    = 4
    ).

    APPEND VALUE #( id = |{ is_-id }.11| gui = lo_11 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.12| gui = lo_12 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.13| gui = lo_13 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.14| gui = lo_14 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.21| gui = lo_21 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.22| gui = lo_22 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.23| gui = lo_23 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.24| gui = lo_24 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.31| gui = lo_31 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.32| gui = lo_32 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.33| gui = lo_33 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.34| gui = lo_34 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.41| gui = lo_41 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.42| gui = lo_42 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.43| gui = lo_43 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.44| gui = lo_44 ) TO rt_.

    APPEND VALUE #( id = |{ is_-id }.51| gui = lo_51 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.52| gui = lo_52 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.53| gui = lo_53 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.54| gui = lo_54 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_flex_into.

    DATA(lv_max)      = iv_threshold - 2.
    DATA(lv_full_col) = round( val  = iv_ / lv_max
                               dec  = 0
                               mode = cl_abap_math=>round_down ).

    DATA:lv_last_col TYPE i.
    lv_last_col = iv_ - ( lv_max * lv_full_col ).
    DATA: lv_total_col TYPE i.

    IF lv_last_col = 0.
      lv_total_col = lv_full_col.
    ELSE.
      lv_total_col = lv_full_col + 1.
    ENDIF.

    DATA:lt_ LIKE mt_.
    DATA(lv_counter) = 0.

    CASE iv_split_vertical_first.
      WHEN 'X'.
        IF iv_ <= iv_threshold.
          APPEND LINES OF me->split_h_n( iv_ = iv_
                                         is_ = ms_ ) TO mt_.
          rt_ = get_leafs( ).
          RETURN.
        ENDIF.
*--------------------------------------------------------------------*
        APPEND LINES OF me->split_v_n( iv_ = ( lv_total_col )
                                       is_ = ms_ ) TO lt_.
        LOOP AT lt_ REFERENCE INTO DATA(lr_).
          ADD 1 TO lv_counter.
          IF  lv_counter <= lv_full_col OR lv_last_col = 0.
            APPEND LINES OF me->split_h_n( iv_ = lv_max
                                           is_ = lr_->* ) TO mt_.
          ELSE.
            APPEND LINES OF me->split_h_n( iv_ = lv_last_col
                                           is_ = lr_->* ) TO mt_.
          ENDIF.
        ENDLOOP.

        rt_ = get_leafs( ).
        RETURN.
      WHEN OTHERS.
        IF iv_ <= iv_threshold.
          APPEND LINES OF me->split_v_n( iv_ = iv_
                                         is_ = ms_ ) TO mt_.
          rt_ = get_leafs( ).
          RETURN.
        ENDIF.
*--------------------------------------------------------------------*
        APPEND LINES OF me->split_h_n( iv_ = ( lv_total_col )
                                       is_ = ms_ ) TO lt_.

        LOOP AT lt_ REFERENCE INTO lr_.
          ADD 1 TO lv_counter.
          IF  lv_counter <= lv_full_col OR lv_last_col = 0.
            APPEND LINES OF me->split_v_n( iv_ = lv_max
                                           is_ = lr_->* ) TO mt_.
          ELSE.
            APPEND LINES OF me->split_v_n( iv_ = lv_last_col
                                           is_ = lr_->* ) TO mt_.
          ENDIF.
        ENDLOOP.

        rt_ = get_leafs( ).
        RETURN.
    ENDCASE.

  ENDMETHOD.


  METHOD split_h.
    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 2
      columns = 1
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_1) = lo_->get_container(
        row       = 1       " Row
        column    = 1    " Column
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 2       " Row
        column    = 1    " Column
    ).

    APPEND VALUE #( id = |{ is_-id }.1T| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2B| gui = lo_2 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_h_into.

    APPEND LINES OF me->split_h_n( iv_ = iv_
                                   is_ = ms_ ) TO mt_.
    rt_ = get_leafs( ).

  ENDMETHOD.


  METHOD split_h_n.

    DATA(lo_) = NEW cl_gui_splitter_container(
      no_autodef_progid_dynnr = abap_true
      parent  = is_-gui
      rows    = iv_
      columns = 1
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA: lv_ TYPE i.
    DO iv_ TIMES.
      ADD 1 TO lv_.

      DATA(lo_1)  = lo_->get_container(
          row    = lv_
          column = 1
      ).

      APPEND VALUE #( id = |{ is_-id }.H{ lv_ }| gui = lo_1 ) TO rt_.
    ENDDO.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_h_third.
    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 2
      columns = 1
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    lo_->set_row_mode(
      EXPORTING
        mode              = lo_->mode_relative   " row Mode
      EXCEPTIONS
        cntl_error        = 1      " See CL_GUI_CONTROL
        cntl_system_error = 2      " See CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

    lo_->set_row_height( id     = 1
                         height = iv_height ).


    DATA(lo_1) = lo_->get_container(
        row       = 1       " Row
        column    = 1    " Column
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 2       " Row
        column    = 1    " Column
    ).

    APPEND VALUE #( id = |{ is_-id }.1T| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2B| gui = lo_2 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_v.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 1
      columns = 2
    ).
    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    DATA(lo_1) = lo_->get_container(
        row       = 1       " Row
        column    = 1    " Column
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 1       " Row
        column    = 2    " Column
    ).

    APPEND VALUE #( id = |{ is_-id }.1L| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2R| gui = lo_2 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_v_into.
    APPEND LINES OF me->split_v_n( iv_ = iv_
                                   is_ = ms_ ) TO mt_.
    rt_ = get_leafs( ).
  ENDMETHOD.


  METHOD split_v_n.

    DATA(lo_split) = NEW cl_gui_splitter_container(
      no_autodef_progid_dynnr = abap_true
      parent  = is_-gui
      rows    = 1
      columns = iv_
    ).

    DATA: lv_ TYPE i.
    DO iv_ TIMES.
      ADD 1 TO lv_.

      DATA(lo_)  = lo_split->get_container(
          row    = 1
          column = lv_
      ).

      APPEND VALUE #( id = |{ is_-id }.V{ lv_ }| gui = lo_ ) TO rt_.
    ENDDO.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD split_v_third.

    DATA(lo_) = NEW cl_gui_splitter_container(
      parent = is_-gui
      no_autodef_progid_dynnr = abap_true
      rows = 1
      columns = 2
    ).

    APPEND VALUE #( id = |{ is_-id }.S| gui = lo_ ) TO mt_split.

    lo_->set_column_mode(
      EXPORTING
        mode              = lo_->mode_relative   " Column Mode
      EXCEPTIONS
        cntl_error        = 1      " See CL_GUI_CONTROL
        cntl_system_error = 2      " See CL_GUI_CONTROL
        OTHERS            = 3
    ).
    IF sy-subrc <> 0.
      "zcl_cpu=>ok( ).
    ENDIF.

    lo_->set_column_width( id = 1
                           width = iv_width ).

    DATA(lo_1) = lo_->get_container(
        row       = 1       " Row
        column    = 1    " Column
    ).

    DATA(lo_2) = lo_->get_container(
        row       = 1       " Row
        column    = 2    " Column
    ).

    APPEND VALUE #( id = |{ is_-id }.1L| gui = lo_1 ) TO rt_.
    APPEND VALUE #( id = |{ is_-id }.2R| gui = lo_2 ) TO rt_.

    ms_ = me->adjust( EXPORTING is_ = is_
                      CHANGING  ct_ = rt_ ).

  ENDMETHOD.


  METHOD tail.

    IF it_[] IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_) = lines( it_ ).

    DATA(lr_) = REF #( it_[ lv_ ] OPTIONAL ).
    rs_ = lr_->*.

  ENDMETHOD.
ENDCLASS.
