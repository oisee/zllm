*----------------------------------------------------------------------*
*       CLASS ZCL_BLOG DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_blog DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES zif_llm_log .
    INTERFACES zif_llm_logger .

    ALIASES a
      FOR zif_llm_logger~a .
    ALIASES add
      FOR zif_llm_logger~add .
    ALIASES display
      FOR zif_llm_logger~display .
    ALIASES e
      FOR zif_llm_logger~e .
    ALIASES get_analyser
      FOR zif_llm_log~get_analyser .
    ALIASES get_bapiret2_table
      FOR zif_llm_log~get_bapiret2_table .
    ALIASES get_converter
      FOR zif_llm_log~get_converter .
    ALIASES get_flatten_table
      FOR zif_llm_log~get_flatten_table .
    ALIASES get_message_table
      FOR zif_llm_log~get_message_table .
    ALIASES i
      FOR zif_llm_logger~i .
    ALIASES s
      FOR zif_llm_logger~s .
    ALIASES w
      FOR zif_llm_logger~w .

*"* public components of class ZCL_LOGGER
*"* do not include other source files here!!!
    DATA header TYPE bal_s_log READ-ONLY .
    DATA handle TYPE balloghndl READ-ONLY .
    DATA db_number TYPE balognr READ-ONLY .

    METHODS get_autocallback
      IMPORTING
        !is_       TYPE bal_s_msg
      RETURNING
        VALUE(rs_) TYPE bal_s_clbk .
    CLASS-METHODS new
      IMPORTING
        !object         TYPE csequence OPTIONAL
        !subobject      TYPE csequence OPTIONAL
        !desc           TYPE csequence OPTIONAL
        !context        TYPE simple OPTIONAL
        !auto_save      TYPE abap_bool OPTIONAL
        !second_db_conn TYPE abap_bool DEFAULT abap_true
        !auto_callback  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_log)    TYPE REF TO zcl_blog .
    CLASS-METHODS open
      IMPORTING
        !object                   TYPE csequence
        !subobject                TYPE csequence
        !desc                     TYPE csequence OPTIONAL
        !create_if_does_not_exist TYPE abap_bool DEFAULT abap_false
        !auto_save                TYPE abap_bool OPTIONAL
        !auto_callback            TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(r_log)              TYPE REF TO zcl_blog .
    METHODS popup .
    METHODS fullscreen .
    METHODS get_autosave
      RETURNING
        VALUE(auto_save) TYPE abap_bool .
    METHODS set_autosave
      IMPORTING
        !auto_save TYPE abap_bool .
    METHODS save .
    CLASS-METHODS fm_exists
      IMPORTING
        !iv_       TYPE rs38l_fnam
      RETURNING
        VALUE(rv_) TYPE abap_bool .
  PROTECTED SECTION.
*"* protected components of class ZCL_LOGGER
*"* do not include other source files here!!!
  PRIVATE SECTION.

    ALIASES gv_msg
      FOR zif_llm_log~gv_msg .
    ALIASES ts_msg
      FOR zif_llm_log~ts_msg .
    ALIASES tt_msg
      FOR zif_llm_log~tt_msg .

    TYPES:
* Local type for hrpad_message as it is not available in an ABAP Development System
      BEGIN OF hrpad_message_field_list_alike,
        scrrprfd TYPE scrrprfd.
    TYPES: END OF hrpad_message_field_list_alike .
    TYPES:
      BEGIN OF hrpad_message_alike,
        cause(32)    TYPE c,              "original: hrpad_message_cause
        detail_level TYPE ballevel.
        INCLUDE TYPE symsg .
      TYPES: field_list   TYPE STANDARD TABLE OF hrpad_message_field_list_alike
              WITH NON-UNIQUE KEY scrrprfd.
    TYPES: END OF hrpad_message_alike .

*"* private components of class ZCL_LOGGER
*"* do not include other source files here!!!
    DATA auto_save TYPE abap_bool .
    DATA sec_connection TYPE abap_bool .
    DATA sec_connect_commit TYPE abap_bool .
    DATA auto_callback TYPE abap_bool .
ENDCLASS.



CLASS ZCL_BLOG IMPLEMENTATION.


  METHOD fm_exists.

    SELECT SINGLE 'X' FROM tfdir INTO @rv_ WHERE funcname = @iv_ .

  ENDMETHOD.


  METHOD fullscreen.

    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    "APPEND me->handle TO lt_log_handles.
    INSERT me->handle INTO TABLE lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_SINGLE_LOG_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD get_autocallback.

    IF me->auto_callback NE abap_true.
      RETURN.
    ENDIF.
    IF is_-msgid IS INITIAL.
      RETURN.
    ENDIF.
*--------------------------------------------------------------------*
    DATA lv_callback_fm LIKE rs_-userexitf.

    lv_callback_fm = |{ is_-msgid }_LOG_{ is_-msgno }|. "tables params like spar: *" TABLES I_T_PARAMS STRUCTURE  SPAR
    IF me->fm_exists( lv_callback_fm ).
      rs_ = VALUE #( userexitt = 'F' userexitf = lv_callback_fm ).
      RETURN.
    ENDIF.
    lv_callback_fm = |{ is_-msgid }_LOG_|. "tables params like spar: *" TABLES I_T_PARAMS STRUCTURE  SPAR
    IF me->fm_exists( lv_callback_fm ).
      rs_ = VALUE #( userexitt = 'F' userexitf = lv_callback_fm ).
      RETURN.
    ENDIF.

    lv_callback_fm = |{ is_-msgid }LOG_{ is_-msgno }|. "tables params like spar: *" TABLES I_T_PARAMS STRUCTURE  SPAR
    IF me->fm_exists( lv_callback_fm ).
      rs_ = VALUE #( userexitt = 'F' userexitf = lv_callback_fm ).
      RETURN.
    ENDIF.
    lv_callback_fm = |{ is_-msgid }LOG_|. "tables params like spar: *" TABLES I_T_PARAMS STRUCTURE  SPAR
    IF me->fm_exists( lv_callback_fm ).
      rs_ = VALUE #( userexitt = 'F' userexitf = lv_callback_fm ).
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD get_autosave.

    auto_save = me->auto_save.

  ENDMETHOD.


  METHOD new.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    FIELD-SYMBOLS <context_val> TYPE c.

    CREATE OBJECT r_log.
    r_log->header-object = object.
    r_log->header-subobject = subobject.
    r_log->header-extnumber = desc.

    r_log->auto_save     = auto_save.
    r_log->auto_callback = auto_callback.

* Use secondary database connection to write data to database even if
* main program does a rollback (e. g. during a dump).
    IF second_db_conn = abap_true.
      r_log->sec_connection     = abap_true.
      r_log->sec_connect_commit = abap_true.
    ENDIF.

*--------------------------------------------------------------------*
    IF context IS SUPPLIED AND context IS NOT INITIAL.
      r_log->header-context-tabname =
        cl_abap_typedescr=>describe_by_data( context )->get_ddic_header( )-tabname.
      ASSIGN context TO <context_val> CASTING.
      r_log->header-context-value = <context_val>.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = r_log->header
      IMPORTING
        e_log_handle = r_log->handle.

* BAL_LOG_CREATE will fill in some additional header data.
* This FM updates our instance attribute to reflect that.
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = r_log->handle
      IMPORTING
        e_s_log      = r_log->header.

  ENDMETHOD.


  METHOD open.

*-- Added AUTO_SAVE as a parameter.  There are times when
*-- you do not want to save the log unless certain kinds
*-- of messages are put in the log.  By allowing the explicit
*-- setting of the AUTO_SAVE value, this can be done
*-- The SAVE method must be called at the end processing
*-- to save all of the log data

    DATA: filter             TYPE bal_s_lfil,
          desc_filter        TYPE bal_s_extn,
          obj_filter         TYPE bal_s_obj,
          subobj_filter      TYPE bal_s_sub,

          found_headers      TYPE balhdr_t,
          most_recent_header TYPE balhdr,
          handles_loaded     TYPE bal_t_logh.

    desc_filter-option = subobj_filter-option = obj_filter-option = 'EQ'.
    desc_filter-sign = subobj_filter-sign = obj_filter-sign = 'I'.

    obj_filter-low = object.
    APPEND obj_filter TO filter-object.
    subobj_filter-low = subobject.
    APPEND subobj_filter TO filter-subobject.
    IF desc IS SUPPLIED.
      desc_filter-low = desc.
      APPEND desc_filter TO filter-extnumber.
    ENDIF.

    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_s_log_filter = filter
      IMPORTING
        e_t_log_header = found_headers
      EXCEPTIONS
        log_not_found  = 1.

    IF sy-subrc = 1.
      IF create_if_does_not_exist = abap_true.
        r_log = zcl_blog=>new( object    = object
                               subobject = subobject
                               desc      = desc ).
      ENDIF.
      RETURN.
    ENDIF.

* Delete all but the last row.  Keep the found_headers table this way
* so we can pass it to BAL_DB_LOAD.
    IF lines( found_headers ) > 1.
      DELETE found_headers TO ( lines( found_headers ) - 1 ).
    ENDIF.
    READ TABLE found_headers INDEX 1 INTO most_recent_header.

    CREATE OBJECT r_log.
    r_log->auto_save = auto_save.
    r_log->auto_callback = auto_callback.

    r_log->db_number = most_recent_header-lognumber.
    r_log->handle = most_recent_header-log_handle.

    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = found_headers.

    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle = r_log->handle
      IMPORTING
        e_s_log      = r_log->header.

  ENDMETHOD.


  METHOD popup.
* See SBAL_DEMO_04_POPUP for ideas

    DATA:
      profile        TYPE bal_s_prof,
      lt_log_handles TYPE bal_t_logh.

    "APPEND me->handle TO lt_log_handles.
    INSERT me->handle INTO TABLE lt_log_handles.

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = profile.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = profile
        i_t_log_handle      = lt_log_handles.

  ENDMETHOD.


  METHOD save.
*--------------------------------------------------------------------*
* Method to save the log on demand.  Intended to be called at the    *
*  end of the log processing so that logs can be saved depending     *
*  on other criteria, like the existance of error messages.          *
*  If there are no error messages, it may not be desireable to save  *
*  a log                                                             *
*--------------------------------------------------------------------*


    DATA:
      lt_log_handles TYPE bal_t_logh,
      lt_log_numbers TYPE bal_t_lgnm,
      ls_log_number  TYPE bal_s_lgnm.

    CHECK auto_save = abap_false.

    "APPEND me->handle TO log_handles.
    INSERT me->handle INTO TABLE lt_log_handles.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle       = lt_log_handles
        i_2th_connection     = me->sec_connection
        i_2th_connect_commit = me->sec_connect_commit
      IMPORTING
        e_new_lognumbers     = lt_log_numbers.
    IF me->db_number IS INITIAL.
      READ TABLE lt_log_numbers INDEX 1 INTO ls_log_number.
      me->db_number = ls_log_number-lognumber.
    ENDIF.

  ENDMETHOD.


  METHOD set_autosave.

    me->auto_save = auto_save.

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
    DATA: lv_type LIKE iv_type.
    lv_type = iv_type.

    DATA: detailed_msg      TYPE bal_s_msg,
          free_text_msg     TYPE char200,
          lo_ctx_type       TYPE REF TO cl_abap_typedescr,
          ctx_ddic_header   TYPE x030l,
          lo_msg_type       TYPE REF TO cl_abap_typedescr,
          exception_data    TYPE bal_s_exc,
          log_numbers       TYPE bal_t_lgnm,
          log_handles       TYPE bal_t_logh,
          log_number        TYPE bal_s_lgnm,
          formatted_context TYPE bal_s_cont,
          formatted_params  TYPE bal_s_parm.

    FIELD-SYMBOLS: <table_of_messages> TYPE ANY TABLE,
                   <message_line>      TYPE any,
                   <fs_msg>            TYPE zif_llm_log=>ts_msg,
                   <fs_syst>           TYPE syst,
                   <bapi_msg1>         TYPE bapiret1,
                   <bapi_msg2>         TYPE bapiret2,
                   <bdc_msg>           TYPE bdcmsgcoll,
"                  <hrpad_msg>         TYPE hrpad_message, " hrpad_message_alike,
                   <context_val>       TYPE any.

    IF is_context IS NOT INITIAL.
      ASSIGN is_context TO <context_val>.
      formatted_context-value = <context_val>.
      lo_ctx_type = cl_abap_typedescr=>describe_by_data( is_context ).

      CALL METHOD lo_ctx_type->get_ddic_header
        RECEIVING
          p_header     = ctx_ddic_header
        EXCEPTIONS
          not_found    = 1
          no_ddic_type = 2
          OTHERS       = 3.
      IF sy-subrc = 0.
        formatted_context-tabname = ctx_ddic_header-tabname.
      ENDIF.
    ENDIF.

*    IF iv_callback_fm IS NOT INITIAL.
*      formatted_params-callback-userexitf = iv_callback_fm.
*      formatted_params-callback-userexitt = 'F'.
*    elseif me->auto_callback = abap_true.
*      formatted_params-callback-userexitf = me->get_autocallback( ).
*    ENDIF.

    lo_msg_type = cl_abap_typedescr=>describe_by_data( io_ ).

    IF io_ IS INITIAL AND lo_msg_type->type_kind NE cl_abap_typedescr=>typekind_table.
      detailed_msg-msgty = sy-msgty.
      detailed_msg-msgid = sy-msgid.
      detailed_msg-msgno = sy-msgno.
      detailed_msg-msgv1 = sy-msgv1.
      detailed_msg-msgv2 = sy-msgv2.
      detailed_msg-msgv3 = sy-msgv3.
      detailed_msg-msgv4 = sy-msgv4.
      IF lv_type IS NOT INITIAL.
        detailed_msg-msgty = lv_type.
      ENDIF.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=SYST'.
      ASSIGN io_ TO <fs_syst>.
      detailed_msg-msgty = <fs_syst>-msgty.
      detailed_msg-msgid = <fs_syst>-msgid.
      detailed_msg-msgno = <fs_syst>-msgno.
      detailed_msg-msgv1 = <fs_syst>-msgv1.
      detailed_msg-msgv2 = <fs_syst>-msgv2.
      detailed_msg-msgv3 = <fs_syst>-msgv3.
      detailed_msg-msgv4 = <fs_syst>-msgv4.
      IF lv_type IS NOT INITIAL.
        detailed_msg-msgty = lv_type.
      ENDIF.
    ELSEIF lo_msg_type->absolute_name = '\INTERFACE=ZIF_LOG\TYPE=TS_MSG'.
      ASSIGN io_ TO <fs_msg>.
      IF <fs_msg>-free_text_msg IS NOT INITIAL.
        free_text_msg = <fs_msg>-free_text_msg.
        lv_type       = <fs_msg>-msgty.
      ELSEIF <fs_msg>-exception IS NOT INITIAL.
        zif_llm_logger~add( <fs_msg>-exception ).
      ELSE.
        MOVE-CORRESPONDING <fs_msg> TO detailed_msg.
      ENDIF.
    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_oref.

      IF io_ IS INSTANCE OF zif_llm_log.
        DATA: li_log TYPE REF TO zif_llm_log.
        li_log ?= io_.
        zif_llm_logger~add( li_log->get_flatten_table( ) ).

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
        exception_data-exception = io_.
        IF iv_type IS NOT INITIAL.
          exception_data-msgty = iv_type.
        ELSE.
          exception_data-msgty = 'E'.
        ENDIF.
        exception_data-probclass = iv_importance.

      ELSE.
        MESSAGE e003(zcx_s) WITH lo_msg_type->absolute_name INTO gv_msg.
        zif_llm_logger~add( sy ).
      ENDIF.

    ELSEIF lo_msg_type->type_kind = cl_abap_typedescr=>typekind_table.
      ASSIGN io_ TO <table_of_messages>.
      LOOP AT <table_of_messages> ASSIGNING <message_line>.
        zif_llm_logger~add( <message_line> ).
      ENDLOOP.
      RETURN.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRET1' OR
           lo_msg_type->absolute_name = '\TYPE=BAPIRET2' OR
           lo_msg_type->absolute_name = '\TYPE=BAPI_CORU_RETURN' OR
           lo_msg_type->absolute_name = '\TYPE=BAPI_ORDER_RETURN'.
      DATA: ls_bapiret2 TYPE bapiret2.
      MOVE-CORRESPONDING io_ TO ls_bapiret2.
      detailed_msg-msgty = ls_bapiret2-type.
      detailed_msg-msgid = ls_bapiret2-id.
      detailed_msg-msgno = ls_bapiret2-number.
      detailed_msg-msgv1 = ls_bapiret2-message_v1.
      detailed_msg-msgv2 = ls_bapiret2-message_v2.
      detailed_msg-msgv3 = ls_bapiret2-message_v3.
      detailed_msg-msgv4 = ls_bapiret2-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPI_CONF_RETURN'.
*      DATA: ls_bapi_conf_return TYPE bapi_conf_return.
*      MOVE-CORRESPONDING io_ TO ls_bapi_conf_return.
*      detailed_msg-msgty = ls_bapi_conf_return-type.
*      detailed_msg-msgid = ls_bapi_conf_return-message_id.
*      detailed_msg-msgno = ls_bapi_conf_return-message_number.
*      detailed_msg-msgv1 = ls_bapi_conf_return-message_v1.
*      detailed_msg-msgv2 = ls_bapi_conf_return-message_v2.
*      detailed_msg-msgv3 = ls_bapi_conf_return-message_v3.
*      detailed_msg-msgv4 = ls_bapi_conf_return-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BAPIRETURN'.
      DATA: ls_bapireturn TYPE bapireturn.
      MOVE-CORRESPONDING io_ TO ls_bapireturn.
      detailed_msg-msgty = ls_bapireturn-type.
      detailed_msg-msgid = ls_bapireturn-code+0(2).
      detailed_msg-msgno = ls_bapireturn-code+2(3).
      detailed_msg-msgv1 = ls_bapireturn-message_v1.
      detailed_msg-msgv2 = ls_bapireturn-message_v2.
      detailed_msg-msgv3 = ls_bapireturn-message_v3.
      detailed_msg-msgv4 = ls_bapireturn-message_v4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=COCF_S_MESSAGE'.
      DATA: ls_symsg TYPE symsg.
      MOVE-CORRESPONDING io_ TO ls_symsg.
      detailed_msg-msgty = ls_symsg-msgty.
      detailed_msg-msgid = ls_symsg-msgid.
      detailed_msg-msgno = ls_symsg-msgno.
      detailed_msg-msgv1 = ls_symsg-msgv1.
      detailed_msg-msgv2 = ls_symsg-msgv2.
      detailed_msg-msgv3 = ls_symsg-msgv3.
      detailed_msg-msgv4 = ls_symsg-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=BDCMSGCOLL'.
      ASSIGN io_ TO <bdc_msg>.
      detailed_msg-msgty = <bdc_msg>-msgtyp.
      detailed_msg-msgid = <bdc_msg>-msgid.
      detailed_msg-msgno = <bdc_msg>-msgnr.
      detailed_msg-msgv1 = <bdc_msg>-msgv1.
      detailed_msg-msgv2 = <bdc_msg>-msgv2.
      detailed_msg-msgv3 = <bdc_msg>-msgv3.
      detailed_msg-msgv4 = <bdc_msg>-msgv4.
    ELSEIF lo_msg_type->absolute_name = '\TYPE=HRPAD_MESSAGE'.
*      ASSIGN io_ TO <hrpad_msg>.
*      detailed_msg-msgty = <hrpad_msg>-msgty.
*      detailed_msg-msgid = <hrpad_msg>-msgid.
*      detailed_msg-msgno = <hrpad_msg>-msgno.
*      detailed_msg-msgv1 = <hrpad_msg>-msgv1.
*      detailed_msg-msgv2 = <hrpad_msg>-msgv2.
*      detailed_msg-msgv3 = <hrpad_msg>-msgv3.
*      detailed_msg-msgv4 = <hrpad_msg>-msgv4.
    ELSEIF lo_msg_type->type_kind = lo_msg_type->typekind_char OR
           lo_msg_type->type_kind = lo_msg_type->typekind_clike OR
           lo_msg_type->absolute_name = '\TYPE=STRING'.
      free_text_msg = io_.
    ELSEIF lo_msg_type->type_kind = lo_msg_type->typekind_csequence.
      free_text_msg = io_.
    ELSE.
      MESSAGE e003(zcx_) WITH lo_msg_type->absolute_name INTO gv_msg.
      zif_llm_logger~add( sy ).
    ENDIF.

    IF iv_callback_fm IS NOT INITIAL.
      formatted_params-callback-userexitf = iv_callback_fm.
      formatted_params-callback-userexitt = 'F'.
    ELSE.
      formatted_params-callback = me->get_autocallback( detailed_msg ).
      "formatted_params-altext   = 'ALTTEXT'. "LSBAL_DETAILF02 line 57,    message i046(bl) with L_TEXT_OBJECT.
    ENDIF.

    IF free_text_msg IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle = me->handle
          i_msgty      = lv_type
          i_probclass  = iv_importance
          i_text       = free_text_msg
          i_s_context  = formatted_context
          i_s_params   = formatted_params.
    ELSEIF exception_data IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_exc      = exception_data.
    ELSEIF detailed_msg IS NOT INITIAL.
      detailed_msg-context = formatted_context.
      detailed_msg-params = formatted_params.
      detailed_msg-probclass = iv_importance.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = me->handle
          i_s_msg      = detailed_msg.
    ENDIF.

    IF auto_save = abap_true.
      "APPEND me->handle TO log_handles.
      INSERT me->handle INTO TABLE log_handles.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle       = log_handles
          i_2th_connection     = me->sec_connection
          i_2th_connect_commit = me->sec_connect_commit
        IMPORTING
          e_new_lognumbers     = log_numbers.
      IF me->db_number IS INITIAL.
        READ TABLE log_numbers INDEX 1 INTO log_number.
        me->db_number = log_number-lognumber.
      ENDIF.
    ENDIF.

    ro_ = me.
  ENDMETHOD.


  METHOD zif_llm_logger~display.
    CASE iv_mode.
      WHEN 'fullscreen'.
        fullscreen(  ).
      WHEN 'popup'.
        popup(  ).
      WHEN OTHERS.
        fullscreen(  ).
    ENDCASE.
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
    DATA: log_handle         TYPE bal_t_logh,
          lt_message_handles TYPE bal_t_msgh,
          ls_message         TYPE bal_s_msg,
          ls_bapiret2        TYPE bapiret2.

    FIELD-SYMBOLS <msg_handle> TYPE balmsghndl.

    INSERT handle INTO TABLE log_handle.

    CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
      EXPORTING
        i_t_log_handle = log_handle
      IMPORTING
        e_t_msg_handle = lt_message_handles
      EXCEPTIONS
        msg_not_found  = 1
        OTHERS         = 2.
    IF sy-subrc NE 0.
      MESSAGE e000(zcx_) INTO DATA(lv_msg).
    ENDIF.

    LOOP AT lt_message_handles ASSIGNING <msg_handle>.
      CLEAR: ls_message.
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <msg_handle>
        IMPORTING
          e_s_msg        = ls_message
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc   IS INITIAL OR
         ls_message IS NOT INITIAL.
        ls_bapiret2-type          = ls_message-msgty.
        ls_bapiret2-id            = ls_message-msgid.
        ls_bapiret2-number        = ls_message-msgno.
        ls_bapiret2-log_no        = <msg_handle>-log_handle. "last 2 chars missing!!
        ls_bapiret2-log_msg_no    = <msg_handle>-msgnumber.
        ls_bapiret2-message_v1    = ls_message-msgv1.
        ls_bapiret2-message_v2    = ls_message-msgv2.
        ls_bapiret2-message_v3    = ls_message-msgv3.
        ls_bapiret2-message_v4    = ls_message-msgv4.
        ls_bapiret2-system        = sy-sysid.

        DATA(lv_msgty) = COND #( WHEN ls_message-msgty CO 'AEIWSX' THEN ls_message-msgty ELSE 'E' ).
        MESSAGE ID     ls_message-msgid
                TYPE   lv_msgty "message-msgty
                NUMBER ls_message-msgno
                WITH   ls_message-msgv1 ls_message-msgv2 ls_message-msgv3 ls_message-msgv4
                INTO   ls_bapiret2-message.
        APPEND ls_bapiret2 TO rt_.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_llm_log~get_converter.
    ro_ = zcl_llm_log_converter=>new( me ).
  ENDMETHOD.


  METHOD zif_llm_log~get_flatten_table.
    rt_ = zif_llm_log~get_message_table(  ).
  ENDMETHOD.


  METHOD zif_llm_log~get_message_table.
    DATA: lt_msg TYPE tt_msg.
    DATA(lt_bapiret) = zif_llm_log~get_bapiret2_table(  ).
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).
      APPEND INITIAL LINE TO rt_ ASSIGNING FIELD-SYMBOL(<fs_>).
      <fs_>-msgty = <fs_bapiret>-type.
      <fs_>-msgid = <fs_bapiret>-id.
      <fs_>-msgno = <fs_bapiret>-number.
      <fs_>-msgv1 = <fs_bapiret>-message_v1.
      <fs_>-msgv2 = <fs_bapiret>-message_v2.
      <fs_>-msgv3 = <fs_bapiret>-message_v3.
      <fs_>-msgv4 = <fs_bapiret>-message_v4.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
