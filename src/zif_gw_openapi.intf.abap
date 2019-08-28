INTERFACE zif_gw_openapi
  PUBLIC .


  METHODS get_json
    EXPORTING
      !ev_json        TYPE xstring
      !ev_json_string TYPE string.
ENDINTERFACE.
