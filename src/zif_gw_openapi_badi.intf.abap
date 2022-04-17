INTERFACE zif_gw_openapi_badi
  PUBLIC .


  INTERFACES if_badi_interface .

  METHODS procces_odata_v2_metadata
    IMPORTING
      !iv_namespace       TYPE namespace
      !iv_service_name    TYPE /iwfnd/med_mdl_service_grp_id
      !iv_service_version TYPE /iwfnd/med_mdl_version
      !iv_metadata        TYPE xstring
    RETURNING
      VALUE(rv_metadata)  TYPE xstring .
  METHODS procces_odata_v4_metadata
    IMPORTING
      !iv_group_id        TYPE /iwbep/v4_med_group_id
      !iv_service_id      TYPE /iwbep/v4_med_service_id
      !iv_repository_id   TYPE /iwbep/v4_med_repository_id
      !iv_service_version TYPE /iwbep/v4_med_service_version
      !iv_metadata        TYPE xstring
      !ii_request_info    TYPE REF TO /iwbep/if_v4_request_info
    RETURNING
      VALUE(rv_metadata)  TYPE xstring .
  METHODS enhance_openapi_json
    IMPORTING
      !iv_repository_id      TYPE /iwbep/v4_med_repository_id
      !iv_group_id           TYPE /iwbep/v4_med_group_id
      !iv_service_id         TYPE /iwfnd/med_mdl_service_grp_id
      !iv_service_version    TYPE /iwfnd/med_mdl_version
      !iv_openapi_json       TYPE xstring
    RETURNING
      VALUE(rv_openapi_json) TYPE xstring .
ENDINTERFACE.
