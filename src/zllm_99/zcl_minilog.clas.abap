CLASS zcl_minilog DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.

    INTERFACES if_message .
    INTERFACES if_t100_message .
    INTERFACES zif_llm_log .
    INTERFACES zif_llm_logger .
    "   INTERFACES zif_tlog_id .

    ALIASES add FOR zif_llm_logger~add.
    ALIASES a FOR zif_llm_logger~a.
    ALIASES e FOR zif_llm_logger~e.
    ALIASES w FOR zif_llm_logger~w.
    ALIASES i FOR zif_llm_logger~i.
    ALIASES s FOR zif_llm_logger~s.
    ALIASES display FOR zif_llm_logger~display.
    ALIASES get_bapiret2_table FOR zif_llm_log~get_bapiret2_table .
    ALIASES get_flatten_table  FOR zif_llm_log~get_flatten_table .
    ALIASES get_message_table  FOR zif_llm_log~get_message_table .
    ALIASES get_converter      FOR zif_llm_log~get_converter .
    ALIASES get_analyser       FOR zif_llm_log~get_analyser .
*   ALIASES get_log_id         FOR zif_tlog_id~get_log_id.

    CLASS-METHODS new
      RETURNING
        VALUE(ro_) TYPE REF TO zcl_minilog .
  PROTECTED SECTION.

  PRIVATE SECTION.

    ALIASES gv_msg
      FOR zif_llm_log~gv_msg .
    ALIASES mt_msg
      FOR zif_llm_log~mt_msg .
    ALIASES ts_msg
      FOR zif_llm_log~ts_msg .
    ALIASES tt_msg
      FOR zif_llm_log~tt_msg .

*    METHODS add_bapiconf_like_structure
*      IMPORTING
*        !io_            TYPE any OPTIONAL
*        !iv_type        TYPE symsgty OPTIONAL
*        !is_context     TYPE simple OPTIONAL
*        !iv_callback_fm TYPE csequence OPTIONAL
*        !iv_importance  TYPE balprobcl OPTIONAL
*          PREFERRED PARAMETER io_ .
    METHODS add_bapiret2_like_structure
      IMPORTING
        !io_            TYPE any OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_bapireturn
      IMPORTING
        !io_            TYPE any OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_bapireturn1
      IMPORTING
        !io_            TYPE any OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_symsg_like_structure
      IMPORTING
        !io_            TYPE any OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_bcdmsgcoll
      IMPORTING
        !io_            TYPE bdcmsgcoll OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_exception
      IMPORTING
        !io_            TYPE REF TO cx_root OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
*    METHODS add_hrpad_message
*      IMPORTING
*        !io_            TYPE hrpad_message OPTIONAL
*        !iv_type        TYPE symsgty OPTIONAL
*        !is_context     TYPE simple OPTIONAL
*        !iv_callback_fm TYPE csequence OPTIONAL
*        !iv_importance  TYPE balprobcl OPTIONAL
*          PREFERRED PARAMETER io_ .
    METHODS add_syst
      IMPORTING
        !io_            TYPE syst OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_ts_msg
      IMPORTING
        !io_ TYPE zif_llm_log=>ts_msg OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_tt_msg
      IMPORTING
        !io_ TYPE zif_llm_log=>tt_msg OPTIONAL
          PREFERRED PARAMETER io_ .
    METHODS add_csequence
      IMPORTING
        !io_            TYPE csequence OPTIONAL
        !iv_type        TYPE symsgty OPTIONAL
        !is_context     TYPE simple OPTIONAL
        !iv_callback_fm TYPE csequence OPTIONAL
        !iv_importance  TYPE balprobcl OPTIONAL
          PREFERRED PARAMETER io_ .
ENDCLASS.



CLASS ZCL_MINILOG IMPLEMENTATION.


  METHOD add_bapiret2_like_structure.
    DATA: ls_ TYPE bapiret2.
    DATA ls_msg LIKE LINE OF mt_msg.
    MOVE-CORRESPONDING io_ TO ls_.
*--------------------------------------------------------------------*
    ls_msg-msgty = ls_-type.
    ls_msg-msgid = ls_-id.
    ls_msg-msgno = ls_-number.
    ls_msg-msgv1 = ls_-message_v1.
    ls_msg-msgv2 = ls_-message_v2.
    ls_msg-msgv3 = ls_-message_v3.
    ls_msg-msgv4 = ls_-message_v4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_bapireturn.
    DATA: ls_ TYPE bapireturn.
    DATA ls_msg LIKE LINE OF mt_msg.
    MOVE-CORRESPONDING io_ TO ls_.
*--------------------------------------------------------------------*
    ls_msg-msgty = ls_-type.
    ls_msg-msgid = ls_-code+0(2).
    ls_msg-msgno = ls_-code+2(3).
    ls_msg-msgv1 = ls_-message_v1.
    ls_msg-msgv2 = ls_-message_v2.
    ls_msg-msgv3 = ls_-message_v3.
    ls_msg-msgv4 = ls_-message_v4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_bapireturn1.
    DATA: ls_ TYPE bapireturn1.
    DATA ls_msg LIKE LINE OF mt_msg.
    MOVE-CORRESPONDING io_ TO ls_.
*--------------------------------------------------------------------*
    ls_msg-msgty = ls_-type.
    ls_msg-msgid = ls_-id.
    ls_msg-msgno = ls_-number.
    ls_msg-msgv1 = ls_-message_v1.
    ls_msg-msgv2 = ls_-message_v2.
    ls_msg-msgv3 = ls_-message_v3.
    ls_msg-msgv4 = ls_-message_v4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_bcdmsgcoll.
    DATA ls_msg LIKE LINE OF mt_msg.
*--------------------------------------------------------------------*
    ls_msg-msgty = io_-msgtyp.
    ls_msg-msgid = io_-msgid.
    ls_msg-msgno = io_-msgnr.
    ls_msg-msgv1 = io_-msgv1.
    ls_msg-msgv2 = io_-msgv2.
    ls_msg-msgv3 = io_-msgv3.
    ls_msg-msgv4 = io_-msgv4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_csequence.
    DATA: ls_msg LIKE LINE OF mt_msg.
*--------------------------------------------------------------------*
    ls_msg-free_text_msg = io_.
    ls_msg-msgty         = iv_type.
    ls_msg-callback_fm   = iv_callback_fm.
    ls_msg-importance    = iv_importance.
    APPEND ls_msg TO mt_msg.
  ENDMETHOD.


  METHOD add_exception.
    DATA ls_msg LIKE LINE OF mt_msg.
*--------------------------------------------------------------------*
    ls_msg-exception   = io_.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_symsg_like_structure.
    DATA: ls_ TYPE symsg.
    DATA ls_msg LIKE LINE OF mt_msg.
    MOVE-CORRESPONDING io_ TO ls_.
*--------------------------------------------------------------------*
    ls_msg-msgty = ls_-msgty.
    ls_msg-msgid = ls_-msgid.
    ls_msg-msgno = ls_-msgno.
    ls_msg-msgv1 = ls_-msgv1.
    ls_msg-msgv2 = ls_-msgv2.
    ls_msg-msgv3 = ls_-msgv3.
    ls_msg-msgv4 = ls_-msgv4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_syst.
    DATA: ls_msg LIKE LINE OF mt_msg.
    ls_msg-msgty = io_-msgty.
    ls_msg-msgid = io_-msgid.
    ls_msg-msgno = io_-msgno.
    ls_msg-msgv1 = io_-msgv1.
    ls_msg-msgv2 = io_-msgv2.
    ls_msg-msgv3 = io_-msgv3.
    ls_msg-msgv4 = io_-msgv4.
    ls_msg-callback_fm = iv_callback_fm.
    ls_msg-importance  = iv_importance.
*--------------------------------------------------------------------*
    IF iv_type IS NOT INITIAL.
      ls_msg-msgty = iv_type.
    ENDIF.
    APPEND ls_msg TO mt_msg.

  ENDMETHOD.


  METHOD add_ts_msg.
    APPEND io_ TO mt_msg.
  ENDMETHOD.


  METHOD add_tt_msg.
    APPEND LINES OF io_ TO mt_msg.
  ENDMETHOD.


  METHOD if_message~get_longtext.
    RETURN.
  ENDMETHOD.


  METHOD if_message~get_text.
    DATA: lv_ TYPE string.
    DATA: lr_ TYPE REF TO ts_msg.
    DATA(lt_msg) = zif_llm_log~get_flatten_table(  ).
    LOOP AT lt_msg REFERENCE INTO lr_.
      CLEAR: lv_.
      IF lr_->free_text_msg IS NOT INITIAL.
        lv_ = lr_->free_text_msg.
      ELSE.
        TRY .
            DATA: li_msg TYPE REF TO if_message.
            IF lr_->exception IS NOT INITIAL.
              li_msg = lr_->exception.
              "lv_ = |Exception:[ { li_msg->get_text( ) } ]|.
              lv_ = li_msg->get_text( ).
            ELSE.
              MESSAGE ID lr_->msgid TYPE lr_->msgty NUMBER lr_->msgno WITH lr_->msgv1 lr_->msgv2 lr_->msgv3 lr_->msgv4 INTO lv_.
            ENDIF.
          CATCH cx_sy_move_cast_error.
            DATA lo_msg_type TYPE REF TO cl_abap_typedescr.
            lo_msg_type = cl_abap_typedescr=>describe_by_data( lr_->exception ).
            MESSAGE e003(zcx_) WITH lo_msg_type->absolute_name INTO lv_.
        ENDTRY.
      ENDIF.

      IF result IS INITIAL.
        result = condense( lv_ ) .
      ELSE.
        result = result && ` | ` && condense( lv_ ).
        "result = condense( result && cl_abap_char_utilities=>cr_lf && condense( lv_ ) ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( ).
  ENDMETHOD.


  METHOD zif_llm_logger~a.
    ro_ = zif_llm_logger~add(
            io_            = io_
            iv_type        = iv_type
            is_context     = is_context
            iv_callback_fm = iv_callback_fm
            iv_importance  = iv_importance ).
  ENDMETHOD.


  METHOD zif_llm_logger~add.

    DATA: lo_msg_type TYPE REF TO cl_abap_typedescr.
    FIELD-SYMBOLS: <fs_any_t> TYPE ANY TABLE,
                   <fs_any>   TYPE any.

*--------------------------------------------------------------------*
    lo_msg_type = cl_abap_typedescr=>describe_by_data( io_ ).

    IF io_ IS INITIAL AND lo_msg_type->type_kind NE cl_abap_typedescr=>typekind_table.
      add_syst( io_            = sy
                iv_type        = iv_type
                is_context     = is_context
                iv_callback_fm = iv_callback_fm
                iv_importance  = iv_importance ).
    ELSEIF lo_msg_type->absolute_name = '\TYPE=SYST'.
      add_syst( io_            = io_
                iv_type        = iv_type
                is_context     = is_context
                iv_callback_fm = iv_callback_fm
                iv_importance  = iv_importance ).
    ELSEIF lo_msg_type->absolute_name = '\INTERFACE=ZIF_LOG\TYPE=TS_MSG'.
      add_ts_msg( io_ ).
    ELSEIF lo_msg_type->absolute_name = '\INTERFACE=ZIF_LOG\TYPE=TT_MSG'.
      add_tt_msg( io_ ).
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_oref.

      IF io_ IS INSTANCE OF zif_llm_log.
        DATA: li_log TYPE REF TO zif_llm_log.
        li_log ?= io_.
        "zif_logger~add( li_log->get_flatten_table( ) ).

        zif_llm_logger~add( li_log->get_message_table( ) ).

      ELSEIF io_ IS INSTANCE OF if_t100_message.
        DATA: li_t100_message TYPE REF TO if_t100_message.
        li_t100_message ?= io_.

        DATA(ls_msg) = VALUE zif_llm_log=>ts_msg(
            msgid = li_t100_message->t100key-msgid
            msgno = li_t100_message->t100key-msgno
            msgty = iv_type
            msgv1 = li_t100_message->t100key-attr1
            msgv2 = li_t100_message->t100key-attr2
            msgv3 = li_t100_message->t100key-attr3
            msgv4 = li_t100_message->t100key-attr4
        ).
        zif_llm_logger~add( ls_msg ).

      ELSEIF io_ IS INSTANCE OF if_message.
        DATA: li_message TYPE REF TO if_message.
        li_message ?= io_.
        zif_llm_logger~add( li_message->get_text( ) ).

      ELSEIF io_ IS INSTANCE OF cx_root.
        add_exception( io_            = io_
                       iv_type        = iv_type
                       is_context     = is_context
                       iv_callback_fm = iv_callback_fm
                       iv_importance  = iv_importance ).

      ELSE.
        MESSAGE e003(zcx_s) WITH lo_msg_type->absolute_name INTO gv_msg.
        add_syst( sy ).
      ENDIF.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN io_ TO <fs_any_t>.
      LOOP AT <fs_any_t> ASSIGNING <fs_any>.
        zif_llm_logger~add( io_ = <fs_any> ).
      ENDLOOP.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET1'         OR
           lo_msg_type->absolute_name = '\TYPE=BAPIRET2'         OR
           lo_msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN' OR
           lo_msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
      add_bapiret2_like_structure(
                         io_            = io_
                         iv_type        = iv_type
                         is_context     = is_context
                         iv_callback_fm = iv_callback_fm
                         iv_importance  = iv_importance  ).
*    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPI_CONF_RETURN'.
*      add_bapiconf_like_structure(
*                         io_            = io_
*                         iv_type        = iv_type
*                         is_context     = is_context
*                         iv_callback_fm = iv_callback_fm
*                         iv_importance  = iv_importance  ).
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRETURN'.
      add_bapireturn(
                         io_            = io_
                         iv_type        = iv_type
                         is_context     = is_context
                         iv_callback_fm = iv_callback_fm
                         iv_importance  = iv_importance  ).
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRETURN1'.
      add_bapireturn1(
                         io_            = io_
                         iv_type        = iv_type
                         is_context     = is_context
                         iv_callback_fm = iv_callback_fm
                         iv_importance  = iv_importance  ).
    ELSEIF lo_msg_type->absolute_name = '\TYPE=COCF_S_MESSAGE'.
      add_symsg_like_structure(
                         io_            = io_
                         iv_type        = iv_type
                         is_context     = is_context
                         iv_callback_fm = iv_callback_fm
                         iv_importance  = iv_importance  ).
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      add_bcdmsgcoll( io_            = io_
                      iv_type        = iv_type
                      is_context     = is_context
                      iv_callback_fm = iv_callback_fm
                      iv_importance  = iv_importance ).
*    ELSEIF lo_msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
*      add_hrpad_message( io_            = io_
*                         iv_type        = iv_type
*                         is_context     = is_context
*                         iv_callback_fm = iv_callback_fm
*                         iv_importance  = iv_importance  ).
    ELSEIF lo_msg_type->type_kind = lo_msg_type->typekind_char OR
           lo_msg_type->type_kind = lo_msg_type->typekind_clike OR
           lo_msg_type->absolute_name = '\TYPE=STRING'.
      add_csequence( io_            = io_
                     iv_type        = iv_type
                     is_context     = is_context
                     iv_callback_fm = iv_callback_fm
                     iv_importance  = iv_importance ).
    ELSEIF lo_msg_type->type_kind = lo_msg_type->typekind_csequence.
      add_csequence( io_            = io_
                     iv_type        = iv_type
                     is_context     = is_context
                     iv_callback_fm = iv_callback_fm
                     iv_importance  = iv_importance ).
    ELSE.
      MESSAGE e003(zcx_) WITH lo_msg_type->absolute_name INTO gv_msg.
      add_syst( sy ).
    ENDIF.

    "set_t100key( VALUE #( mt_msg[ lines( mt_msg ) ] OPTIONAL ) ).
    "set_t100key( ).
*--------------------------------------------------------------------*
    ro_ = me.

  ENDMETHOD.


  METHOD zif_llm_logger~display.
    "not supported
  ENDMETHOD.


  METHOD zif_llm_logger~e.
    ro_ = zif_llm_logger~add(
            io_            = io_
            iv_type        = iv_type
            is_context     = is_context
            iv_callback_fm = iv_callback_fm
            iv_importance  = iv_importance ).
  ENDMETHOD.


  METHOD zif_llm_logger~i.
    ro_ = zif_llm_logger~add(
            io_            = io_
            iv_type        = iv_type
            is_context     = is_context
            iv_callback_fm = iv_callback_fm
            iv_importance  = iv_importance ).
  ENDMETHOD.


  METHOD zif_llm_logger~s.
    ro_ = zif_llm_logger~add(
            io_            = io_
            iv_type        = iv_type
            is_context     = is_context
            iv_callback_fm = iv_callback_fm
            iv_importance  = iv_importance ).
  ENDMETHOD.


  METHOD zif_llm_logger~w.
    ro_ = zif_llm_logger~add(
            io_            = io_
            iv_type        = iv_type
            is_context     = is_context
            iv_callback_fm = iv_callback_fm
            iv_importance  = iv_importance ).
  ENDMETHOD.


  METHOD zif_llm_log~get_analyser.
    ro_ = zcl_llm_log_analyser=>new( me ).
  ENDMETHOD.


  METHOD zif_llm_log~get_bapiret2_table.
    DATA(lt_msg) = zif_llm_log~get_flatten_table( ).
    DATA: lr_ TYPE REF TO ts_msg.
    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<fs_msg>).
      APPEND INITIAL LINE TO rt_ ASSIGNING FIELD-SYMBOL(<fs_>).
      <fs_>-type       = <fs_msg>-msgty.
      <fs_>-id         = <fs_msg>-msgid.
      <fs_>-number     = <fs_msg>-msgno.
      <fs_>-message_v1 = <fs_msg>-msgv1.
      <fs_>-message_v2 = <fs_msg>-msgv2.
      <fs_>-message_v3 = <fs_msg>-msgv3.
      <fs_>-message_v4 = <fs_msg>-msgv4.
      IF <fs_msg>-free_text_msg IS NOT INITIAL.
        <fs_>-message = <fs_msg>-free_text_msg.
      ELSE.
        DATA(lv_msgty) = COND #( WHEN <fs_msg>-msgty CO 'AEIWSX' THEN <fs_msg>-msgty ELSE 'E' ).
        MESSAGE ID <fs_msg>-msgid
          TYPE      lv_msgty
          NUMBER   <fs_msg>-msgno
          WITH     <fs_msg>-msgv1 <fs_msg>-msgv2 <fs_msg>-msgv3 <fs_msg>-msgv4
          INTO     <fs_>-message.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_llm_log~get_converter.
    ro_ = zcl_llm_log_converter=>new( me ).
  ENDMETHOD.


  METHOD zif_llm_log~get_flatten_table.
    DATA: lr_ TYPE REF TO ts_msg.
    DATA: lt_msg TYPE tt_msg.
    lt_msg = get_message_table( ).
    LOOP AT lt_msg REFERENCE INTO lr_.
      IF lr_->exception IS NOT INITIAL.

        IF lr_->exception IS INSTANCE OF zif_llm_log.
          DATA: li_log TYPE REF TO zif_llm_log.
          li_log ?= lr_->exception.
          DATA: lt_ TYPE tt_msg.
          lt_ = li_log->get_flatten_table( ).
          APPEND LINES OF lt_ TO rt_.

        ELSEIF lr_->exception IS INSTANCE OF if_t100_message.
          DATA: li_t100_message TYPE REF TO if_t100_message.
          li_t100_message ?= lr_->exception.

          APPEND VALUE zif_llm_log=>ts_msg(
              msgid = li_t100_message->t100key-msgid
              msgno = li_t100_message->t100key-msgno
              msgty = lr_->msgty
              msgv1 = li_t100_message->t100key-attr1
              msgv2 = li_t100_message->t100key-attr2
              msgv3 = li_t100_message->t100key-attr3
              msgv4 = li_t100_message->t100key-attr4
          ) TO rt_.

        ELSEIF lr_->exception IS INSTANCE OF if_message.
          DATA: li_message TYPE REF TO if_message.
          li_message ?= lr_->exception.

          APPEND VALUE zif_llm_log=>ts_msg( free_text_msg = li_message->get_text( ) ) TO rt_ .

        ELSE.

          DATA(lo_msg_type) = cl_abap_typedescr=>describe_by_data( lr_->exception ).
          MESSAGE e003(zcx_) WITH lo_msg_type->absolute_name INTO gv_msg.
          APPEND VALUE zif_llm_log=>ts_msg(
              msgid = sy-msgid
              msgno = sy-msgno
              msgty = sy-msgty
              msgv1 = sy-msgv1
              msgv2 = sy-msgv1
              msgv3 = sy-msgv1
              msgv4 = sy-msgv1
          ) TO rt_.

        ENDIF.

      ELSE.
        APPEND lr_->* TO rt_.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_llm_log~get_message_table.
    rt_ = mt_msg.
  ENDMETHOD.
ENDCLASS.
