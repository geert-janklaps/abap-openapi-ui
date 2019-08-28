INTERFACE zif_gw_openapi_metadata
  PUBLIC .


  METHODS get_metadata
    RETURNING
      VALUE(rv_metadata) TYPE xstring .
  METHODS get_json
    EXPORTING
      !ev_json        TYPE xstring
      !ev_json_string TYPE string .
ENDINTERFACE.
