CLASS zcl_gw_openapi_meta_base DEFINITION
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
  PROTECTED SECTION.

    DATA mv_repository TYPE /iwbep/v4_med_repository_id .
    DATA mv_group_id TYPE /iwbep/v4_med_group_id .
    DATA mv_external_service TYPE /iwfnd/med_mdl_service_grp_id .
    DATA mv_version TYPE /iwfnd/med_mdl_version .
    DATA mv_base_url TYPE string .
    DATA mv_scheme TYPE string .
    DATA mv_host TYPE string .
    DATA mv_path TYPE string .
    DATA mv_description TYPE string .

    METHODS convert_odatav2_to_odatav4
      IMPORTING
        !iv_metadata_v2       TYPE xstring
      RETURNING
        VALUE(rv_metadata_v4) TYPE xstring .
    METHODS convert_odatav4_to_json
      IMPORTING
        !iv_metadata_v4 TYPE xstring
      RETURNING
        VALUE(rv_json)  TYPE xstring .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GW_OPENAPI_META_BASE IMPLEMENTATION.


  METHOD convert_odatav2_to_odatav4.

*   Convert OData V2 to V4 metadata document
    CALL TRANSFORMATION zgw_odatav2_to_v4
      SOURCE XML iv_metadata_v2
      RESULT XML rv_metadata_v4.

  ENDMETHOD.


  METHOD convert_odatav4_to_json.
    DATA: lt_parameters TYPE abap_trans_parmbind_tab.

*   Set transformation parameters
    DATA(lv_version) = me->mv_version.
    SHIFT lv_version LEFT DELETING LEADING '0'.
    lv_version = 'V' && lv_version.

    lt_parameters = VALUE #( ( name = 'openapi-version' value = '3.0.0' )
                             ( name = 'odata-version' value = '4.0' )
                             ( name = 'scheme' value = me->mv_scheme )
                             ( name = 'host' value = me->mv_host )
                             ( name = 'basePath' value = '/' && me->mv_path )
                             ( name = 'info-version' value = lv_version )
                             ( name = 'info-title' value = me->mv_external_service )
                             ( name = 'info-description' value = me->mv_description )
                             ( name = 'references' value = 'YES' )
                             ( name = 'diagram' value = 'YES' ) ).

*   Convert metadata document to openapi
    CALL TRANSFORMATION zgw_odatav4_to_openapi
      SOURCE XML iv_metadata_v4
      RESULT XML rv_json
      PARAMETERS (lt_parameters).


  ENDMETHOD.
ENDCLASS.
