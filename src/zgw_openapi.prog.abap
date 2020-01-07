*&---------------------------------------------------------------------*
*& Report ZGW_OPENAPI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zgw_openapi.

INCLUDE zgw_openapi_top.
INCLUDE zgw_openapi_class.

AT SELECTION-SCREEN OUTPUT.
  lcl_screen_handler=>handle_pbo( ).

AT SELECTION-SCREEN.
  lcl_screen_handler=>handle_pai( iv_ok_code = sy-ucomm ).
  CLEAR sy-ucomm.
