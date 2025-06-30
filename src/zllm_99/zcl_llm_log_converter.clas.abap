class ZCL_LLM_LOG_CONVERTER definition
  public
  create protected .

public section.

  interfaces zif_llm_log_converter .

  aliases GET_BAPIRET2_TABLE
    for zif_llm_log_converter~GET_BAPIRET2_TABLE .
  aliases GET_FLATTEN_TABLE
    for zif_llm_log_converter~GET_FLATTEN_TABLE .
  aliases GET_IDOC_STATUS_TABLE
    for zif_llm_log_converter~GET_IDOC_STATUS_TABLE .

*  aliases GET_MMPUR_T_MESSAGES_EXT
*    for ZIF_LOG_CONVERTER~GET_MMPUR_T_MESSAGES_EXT .
  class-methods NEW
    importing
      !IO_LOG type ref to zif_llm_log
    returning
      value(RO_) type ref to ZCL_LLM_LOG_CONVERTER .
  class-methods SY_TO_BAPIRET2_T
    importing
      !IS_ type SYST default SY
    returning
      value(RT_) type BAPIRET2_T .
  class-methods SY_TO_BAPIRET2
    importing
      !IS_ type SYST default SY
    returning
      value(RS_) type BAPIRET2 .
  class-methods BAPIRET2_TO_STRING
    importing
      !IS_ type BAPIRET2
    returning
      value(RV_) type STRING .
  class-methods BAPIRET2_TO_FIMSG
    importing
      !IS_ type BAPIRET2
    returning
      value(RS_) type FIMSG .
  class-methods BAPIRET2_T_TO_FIMSG_T
    importing
      !IT_ type BAPIRET2_T
    returning
      value(RT_) type FKK_TAB_CORR_FIMSG .
  class-methods SY_TO_FIMSG
    importing
      !IS_ type SYST default SY
    returning
      value(RS_) type FIMSG .
  class-methods SY_TO_FIMSG_T
    importing
      !IS_ type SYST default SY
    returning
      value(RT_) type FKK_TAB_CORR_FIMSG .
  PROTECTED SECTION.
    DATA: mo_log TYPE REF TO zif_llm_log.
    METHODS constructor IMPORTING io_log TYPE REF TO zif_llm_log.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_LLM_LOG_CONVERTER IMPLEMENTATION.


  METHOD bapiret2_to_fimsg.
    rs_-msgid  = is_-id.
    rs_-msgty  = is_-type.
    rs_-msgno  = is_-number.
    rs_-msgv1  = is_-message_v1.
    rs_-msgv2  = is_-message_v2.
    rs_-msgv3  = is_-message_v3.
    rs_-msgv4  = is_-message_v4.
  ENDMETHOD.


  METHOD bapiret2_to_string.
    IF is_-type IS NOT INITIAL.
      MESSAGE ID is_-id TYPE is_-type NUMBER is_-number WITH is_-message_v1 is_-message_v2 is_-message_v3 is_-message_v4 INTO rv_.
    ELSE.
      MESSAGE ID is_-id TYPE 'E' NUMBER is_-number WITH is_-message_v1 is_-message_v2 is_-message_v3 is_-message_v4 INTO rv_.
    ENDIF.
  ENDMETHOD.


  METHOD bapiret2_t_to_fimsg_t.
    LOOP AT it_ REFERENCE INTO DATA(lr_).
      APPEND bapiret2_to_fimsg( lr_->* ) TO rt_.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    mo_log = io_log.
  ENDMETHOD.


  METHOD new.
    ro_ = NEW #( io_log ).
  ENDMETHOD.


  METHOD sy_to_bapiret2.
    rs_-type       = is_-msgty.
    rs_-id         = is_-msgid.
    rs_-number     = is_-msgno.
    rs_-message_v1 = is_-msgv1.
    rs_-message_v2 = is_-msgv2.
    rs_-message_v3 = is_-msgv3.
    rs_-message_v4 = is_-msgv4.

    IF is_-msgty is NOT INITIAL.
      MESSAGE id is_-msgid TYPE is_-msgty NUMBER is_-msgno WITH is_-msgv1 is_-msgv2 is_-msgv3 is_-msgv4 INTO rs_-message.
    ELSE.
      MESSAGE id is_-msgid TYPE 'E' NUMBER is_-msgno WITH is_-msgv1 is_-msgv2 is_-msgv3 is_-msgv4 INTO rs_-message.
    ENDIF.

  ENDMETHOD.


  METHOD sy_to_bapiret2_t.
    APPEND INITIAL LINE TO rt_ ASSIGNING FIELD-SYMBOL(<fs_>).
    <fs_> = sy_to_bapiret2( is_ ).
  ENDMETHOD.


  METHOD sy_to_fimsg.
    rs_ = CORRESPONDING #( is_ ).
  ENDMETHOD.


  METHOD sy_to_fimsg_t.
    APPEND CORRESPONDING #( is_ ) TO rt_.
  ENDMETHOD.


  METHOD zif_llm_log_converter~get_bapiret2_table.
    rt_ = mo_log->get_bapiret2_table(  ).
  ENDMETHOD.


  METHOD zif_llm_log_converter~get_flatten_table.
    rt_ = mo_log->get_flatten_table(  ).
  ENDMETHOD.


  METHOD zif_llm_log_converter~get_idoc_status_table.
    DATA(lt_bapiret) = mo_log->get_bapiret2_table( ).
    LOOP AT lt_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).
      APPEND INITIAL LINE TO rt_ ASSIGNING FIELD-SYMBOL(<fs_>).
      <fs_>-status = iv_status.
      <fs_>-docnum = iv_docnum.
      <fs_>-appl_log = iv_appl_log.
      <fs_>-uname = iv_uname.
      <fs_>-msgty = <fs_bapiret>-type.
      <fs_>-msgid = <fs_bapiret>-id.
      <fs_>-msgno = <fs_bapiret>-number.
      <fs_>-msgv1 = <fs_bapiret>-message_v1.
      <fs_>-msgv2 = <fs_bapiret>-message_v2.
      <fs_>-msgv3 = <fs_bapiret>-message_v3.
      <fs_>-msgv4 = <fs_bapiret>-message_v4.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_llm_log_converter~get_iwbep_msg_container.

    DATA: lo_msg_container  TYPE REF TO /iwbep/if_message_container.
    lo_msg_container = /iwbep/cl_mgw_msg_container=>get_mgw_msg_container( ).

    lo_msg_container->add_messages_from_bapi(
      EXPORTING
         it_bapi_messages          = mo_log->get_bapiret2_table( )
         iv_error_category         = iv_error_category
         iv_determine_leading_msg  = iv_determine_leading_msg
         iv_entity_type            = iv_entity_type
         it_key_tab                = it_key_tab
         iv_add_to_response_header = iv_add_to_response_header
    ).

*TYPE BAPIRET2_T
*TYPE /IWBEP/IF_MESSAGE_CONTAINER=>TY_ERROR_CATEGORY  DEFAULT /IWBEP/IF_MESSAGE_CONTAINER=>GCS_ERROR_CATEGORY-PROCESSING
*TYPE /IWBEP/IF_MESSAGE_CONTAINER=>TY_LEADING_MSG_FLAG  DEFAULT /IWBEP/IF_MESSAGE_CONTAINER=>GCS_LEADING_MSG_SEARCH_OPTION-NONE
*TYPE STRING OPTIONAL
*TYPE /IWBEP/T_MGW_NAME_VALUE_PAIR OPTIONAL
*TYPE /IWBEP/SUP_MC_ADD_TO_RESPONSE  DEFAULT ABAP_FALSE


  ENDMETHOD.
ENDCLASS.
