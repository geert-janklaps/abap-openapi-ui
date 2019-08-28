interface ZIF_GW_OPENAPI_METADATA
  public .


  methods GET_METADATA
    returning
      value(RV_METADATA) type XSTRING .
  methods GET_JSON
    exporting
      !EV_JSON type XSTRING
      !EV_JSON_STRING type STRING .
endinterface.
