*&---------------------------------------------------------------------*
*& Report ZLLM_00_SYNC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_00_sync.

"~ Report for synchronizing files from a specified folder to an SAP package.
"~ This ABAP report synchronizes files from a local folder to an SAP package,
"~ handling file listing, reading, and uploading. It utilizes a local class for operations,
"~ with methods for starting the process, getting file lists, and uploading to SAP. The
"~ REPORT is triggered via SELECTION-SCREEN PARAMETERS, allowing users to specify the folder
"~ and package FOR synchronization. key operations include file content conversion to binary,
"~ metadata preparation, and uploading files as sap mime objects.

"parameters: input folder, output package (smw0)

" Define parameters for the input folder and output package, masks for sys and usr
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.

  PARAMETERS: p_fold_i TYPE string LOWER CASE DEFAULT 'C:\TMP\@',
              p_pack_i TYPE string LOWER CASE DEFAULT '$ZLLM',
              p_ns_i   TYPE string DEFAULT 'Z',
              p_bin_i  TYPE zllm_00s_bin-bin LOWER CASE DEFAULT '$ZLLM'.

  PARAMETERS p_i1 RADIOBUTTON GROUP ri DEFAULT 'X' USER-COMMAND zi1.
  PARAMETERS p_i2 RADIOBUTTON GROUP ri.
  PARAMETERS p_i3 RADIOBUTTON GROUP ri.

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-b05.

  PARAMETERS: p_fold_o TYPE string LOWER CASE DEFAULT 'C:\TMP\@',
              p_pack_o TYPE string LOWER CASE DEFAULT '$ZLLM',
              p_ns_o   TYPE string DEFAULT 'Z',
              p_bin_o  TYPE zllm_00s_bin-bin LOWER CASE DEFAULT '$ZLLM'.

  PARAMETERS p_o1 RADIOBUTTON GROUP ro USER-COMMAND zo1.
  PARAMETERS p_o2 RADIOBUTTON GROUP ro.
  PARAMETERS p_o3 RADIOBUTTON GROUP ro DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b05.

PARAMETERS:p_codec AS CHECKBOX DEFAULT 'X'.

PARAMETERS: p_env   TYPE string LOWER CASE DEFAULT '*.env'.
PARAMETERS: p_sys   TYPE string LOWER CASE DEFAULT '*sys.md'.

" Define parameter for the user mask
PARAMETERS: p_usr   TYPE string LOWER CASE DEFAULT '*usr.md'.
PARAMETERS: p_pat   TYPE string LOWER CASE DEFAULT '*pat.md'.

SELECTION-SCREEN BEGIN OF BLOCK b15 WITH FRAME TITLE TEXT-b15.

  PARAMETERS p_d1 RADIOBUTTON GROUP rd USER-COMMAND zd1.
  PARAMETERS p_d2 RADIOBUTTON GROUP rd.
  PARAMETERS p_bin_d TYPE zllm_00s_bin-bin LOWER CASE DEFAULT ''.
  PARAMETERS p_mask  TYPE string LOWER CASE DEFAULT '*.ENV'.

SELECTION-SCREEN END OF BLOCK b15.

" define local classs

"$. region    {
" Define local class for handling file operations
CLASS lcl_ DEFINITION.
  "~ Local class definition for handling file operations, including listing and uploading files.
  PUBLIC SECTION.
    CLASS-METHODS: new RETURNING VALUE(ro_) TYPE REF TO lcl_.
    METHODS start.                                         "~ Method to initiate the file processing operation.
    METHODS upload_to_fl                                         "~ Method to upload files to a specified SAP package.
      IMPORTING
        io_in  TYPE REF TO zif_llm_00_file_list
        io_out TYPE REF TO zif_llm_00_file_list.

    METHODS get_fl_in  RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_file_list.
    METHODS get_fl_out RETURNING VALUE(ro_) TYPE REF TO zif_llm_00_file_list.
    DATA mo_codec TYPE REF TO zif_llm_00_codec.

    METHODS delete.
ENDCLASS.

CLASS lcl_ IMPLEMENTATION.
  "~ Implementation part of the local class for file operations.
  METHOD new.

    ro_ = NEW #( ).
    ro_->mo_codec = zcl_llm_00_codec=>new( ).

  ENDMETHOD.

  METHOD delete.
    IF p_bin_d IS INITIAL.
      RETURN.
    ENDIF.
    DATA(lo_mask) = zcl_llm_00_list=>new_from_string( p_mask ).
    DATA(ltr_r)  = lo_mask->r( ).
    DATA(ltr_ru) = lo_mask->ru( ).
    DELETE FROM zllm_00_bin WHERE bin = p_bin_d AND
              ( name IN ltr_r OR name IN ltr_ru ).
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD start.
    IF p_d2 = 'X'.
      me->delete( ).
      RETURN.
    ENDIF.


    DATA(lo_fl_in)  = me->get_fl_in( ).
    DATA(lo_fl_out) = me->get_fl_out( ).

    me->upload_to_fl(
      EXPORTING
        io_in  = lo_fl_in
        io_out = lo_fl_out
    ).

    DATA(lt_) = lo_fl_out->get( ).
    DATA(lv_json) = zcl_llm_00_json=>to_json(
      data          = lt_
      format_output = 'X'
    ).
    cl_demo_output=>display( lv_json ).
  ENDMETHOD.

  METHOD get_fl_in.
    IF p_codec = 'X'.
      DATA(lo_codec) = zcl_llm_00_codec=>new( ).
    ELSE.
      lo_codec = zcl_llm_00_codec_mock=>new( ).
    ENDIF.

    CASE 'X'.
      WHEN p_i1.
        ro_ = zcl_llm_00_file_list_local=>new_from_folder(
          iv_     = p_fold_i
          iv_mask = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
      WHEN p_i2.
        ro_ = zcl_llm_00_file_list_smw0=>new_from_package(
          iv_namespace = p_ns_i
          iv_          = p_pack_i
          io_codec     = lo_codec
          iv_mask      = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
      WHEN OTHERS.
        ro_ = zcl_llm_00_file_list_bin=>new_from_bin(
          iv_bin   = CONV #( p_bin_i )
          io_codec = lo_codec
          iv_mask  = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
    ENDCASE.

  ENDMETHOD.
  METHOD get_fl_out.
    IF p_codec = 'X'.
      DATA(lo_codec) = zcl_llm_00_codec=>new( ).
    ELSE.
      lo_codec = zcl_llm_00_codec_mock=>new( ).
    ENDIF.
    CASE 'X'.
      WHEN p_o1.
        ro_ = zcl_llm_00_file_list_local=>new_from_folder(
          iv_     = p_fold_o
          iv_mask = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
      WHEN p_o2.
        ro_ = zcl_llm_00_file_list_smw0=>new_from_package(
          iv_namespace = p_ns_o
          iv_          = p_pack_o
          io_codec     = lo_codec
          iv_mask      = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
      WHEN OTHERS.
        ro_ = zcl_llm_00_file_list_bin=>new_from_bin(
          iv_bin   = CONV #( p_bin_o )
          io_codec = lo_codec
          iv_mask  = p_sys && ';' && p_usr && ';' && p_pat && ';' && p_env
        ).
    ENDCASE.
  ENDMETHOD.

  METHOD upload_to_fl.
    "~ Implements the logic to upload files to the specified SAP package.
    DATA(lt_) = io_in->get( ).                               "~ Retrieves the list of files to be uploaded.
    LOOP AT lt_ REFERENCE INTO DATA(lr_).
      io_out->save( lr_->file ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
"$. endregion }1

INITIALIZATION.
  p_fold_i = p_fold_i && sy-sysid+0(3) && '\'.
  p_fold_o = p_fold_o && sy-sysid+0(3) && '\'.
  p_bin_o  = p_bin_o && '_' && sy-uname.
  DATA(go_) = lcl_=>new( ).                                "~ Instantiates the main class for file processing.

AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'ONLI'.
      go_->start( ).                                       "~ Triggers the start method of the main class on user command.
    WHEN OTHERS.
  ENDCASE.
  "$. endregion }

AT SELECTION-SCREEN OUTPUT.
  " Loop through the screen elements and hide or display the corresponding parameters
  " based on the selected radio button for input and output options
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_FOLD_I' .
        screen-input = COND #( WHEN p_i1 = abap_true THEN 1 ELSE 0 ).
      WHEN 'P_PACK_I' OR 'P_NS_I'.
        screen-input = COND #( WHEN p_i2 = abap_true THEN 1 ELSE 0 ).
      WHEN 'P_BIN_I'.
        screen-input = COND #( WHEN p_i3 = abap_true THEN 1 ELSE 0 ).
      WHEN 'P_FOLD_O' .
        screen-input = COND #( WHEN p_o1 = abap_true THEN 1 ELSE 0 ).
      WHEN 'P_PACK_O' OR 'P_NS_O'.
        screen-input = COND #( WHEN p_o2 = abap_true THEN 1 ELSE 0 ).
      WHEN 'P_BIN_O'.
        screen-input = COND #( WHEN p_o3 = abap_true THEN 1 ELSE 0 ).
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.
