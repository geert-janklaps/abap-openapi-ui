INTERFACE zif_gw_openapi_badi
  PUBLIC .


  INTERFACES if_badi_interface .

  METHODS procces_odata_v2_metadata
    IMPORTING
      !iv_namespace       TYPE namespace
      !iv_service_name    TYPE /iwfnd/med_mdl_service_grp_id
      !iv_service_version TYPE /iwfnd/med_mdl_version
    CHANGING
      VALUE(cv_metadata)  TYPE xstring .
  METHODS procces_odata_v4_metadata
    IMPORTING
      !iv_group_id        TYPE /iwbep/v4_med_group_id
      !iv_service_id      TYPE /iwbep/v4_med_service_id
      !iv_repository_id   TYPE /iwbep/v4_med_repository_id
      !iv_service_version TYPE /iwbep/v4_med_service_version
      !ii_request_info    TYPE REF TO /iwbep/if_v4_request_info
    CHANGING
      VALUE(cv_metadata)  TYPE xstring .
  METHODS enhance_openapi_json
    IMPORTING
      !iv_repository_id      TYPE /iwbep/v4_med_repository_id
      !iv_group_id           TYPE /iwbep/v4_med_group_id
      !iv_service_id         TYPE /iwfnd/med_mdl_service_grp_id
      !iv_service_version    TYPE /iwfnd/med_mdl_version
    CHANGING
      VALUE(cv_openapi_json) TYPE xstring .
  METHODS update_openapi_info_attributes
    IMPORTING
      !iv_repository_id   TYPE /iwbep/v4_med_repository_id
      !iv_group_id        TYPE /iwbep/v4_med_group_id
      !iv_service_id      TYPE /iwfnd/med_mdl_service_grp_id
      !iv_service_version TYPE /iwfnd/med_mdl_version
    CHANGING
      !cv_title           TYPE string
      !cv_version         TYPE string
      !cv_description     TYPE string .
ENDINTERFACE.
