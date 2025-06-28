class ZCL_LLM_00_LIST definition
  public
  final
  create private .

public section.

  types:
    tt_        TYPE TABLE OF string .
  types:
    ttr_       TYPE RANGE OF string .
  types:
    ttr_tvarvc TYPE RANGE OF tvarvc-low .

  class-methods NEW
    importing
      !IT_ type TT_
    returning
      value(RO_) type ref to ZCL_LLM_00_LIST .
  class-methods NEW_FROM_STRING
    importing
      !IV_ type STRING
      !IV_DELIMITER type STRING default ';'
    preferred parameter IV_
    returning
      value(RO_) type ref to ZCL_LLM_00_LIST .
  methods R
    returning
      value(RTR_) type TTR_ .
  methods R_T
    returning
      value(RTR_) type TTR_TVARVC .
  methods RU
    returning
      value(RTR_) type TTR_ .
  methods RU_T
    returning
      value(RTR_) type TTR_TVARVC .
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_        TYPE tt_.
    DATA: mtr_       TYPE ttr_.
    DATA: mtr_upper  TYPE ttr_.

    METHODS constructor
      IMPORTING it_ TYPE tt_.
ENDCLASS.



CLASS ZCL_LLM_00_LIST IMPLEMENTATION.


  METHOD constructor.
    mtr_ = VALUE #( FOR ls_ IN it_
      ( sign   = COND #( WHEN ( ls_ CA '!' AND ls_+0(1) = '!' ) THEN 'E'  ELSE 'I' )
        option = COND #( WHEN ls_ CA '*+' THEN 'CP' ELSE 'EQ' )
        low    = ls_
      )
    ).
    LOOP AT mtr_ REFERENCE INTO DATA(ltr_) WHERE sign = 'E'.
      DATA(lv_len) = strlen( ltr_->low ) - 1.
      IF lv_len = 0.
        CLEAR ltr_->low.
      ELSE.
        ltr_->low = ltr_->low+1(lv_len).
      ENDIF.
    ENDLOOP.
    mtr_upper = mtr_.
    LOOP AT mtr_upper REFERENCE INTO ltr_.
      ltr_->low = to_upper( ltr_->low ).
    ENDLOOP.

  ENDMETHOD.


  METHOD new.
    ro_ = NEW zcl_llm_00_list( it_ ).
  ENDMETHOD.


  METHOD new_from_string.
    DATA lt_ TYPE tt_.
    SPLIT iv_ AT iv_delimiter INTO TABLE lt_.
    ro_ = NEW zcl_llm_00_list( lt_ ).
  ENDMETHOD.


  METHOD r.
    rtr_ = mtr_.
  ENDMETHOD.


  METHOD RU.
    rtr_ = mtr_upper.
  ENDMETHOD.


  METHOD RU_T.
    rtr_ = CORRESPONDING #( mtr_upper ).
  ENDMETHOD.


  METHOD r_t.
    rtr_ = CORRESPONDING #( mtr_ ).
  ENDMETHOD.
ENDCLASS.
