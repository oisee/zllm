INTERFACE zif_llm
  PUBLIC .

  TYPES string_t TYPE TABLE OF string WITH DEFAULT KEY.
  CONSTANTS n TYPE abap_char1 VALUE cl_abap_char_utilities=>newline ##NO_TEXT.
  CONSTANTS cr_lf TYPE abap_cr_lf VALUE cl_abap_char_utilities=>cr_lf ##NO_TEXT.
ENDINTERFACE.
