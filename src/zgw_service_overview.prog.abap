*&---------------------------------------------------------------------*
*& Report ZGW_SERVICE_OVERVIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

INCLUDE zgw_service_overview_top. "Global Data
INCLUDE zgw_service_overview_class. "Screen handler

AT SELECTION-SCREEN OUTPUT.
  lcl_screen_handler=>handle_pbo( ).

AT SELECTION-SCREEN.
  lcl_screen_handler=>handle_pai( iv_ok_code = sy-ucomm ).
  CLEAR sy-ucomm.
