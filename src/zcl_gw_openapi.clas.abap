CLASS zcl_gw_openapi DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_gw_openapi .

    METHODS constructor
      IMPORTING
        !iv_repository TYPE /iwbep/v4_med_repository_id OPTIONAL
        !iv_group_id TYPE /iwbep/v4_med_group_id OPTIONAL
        !iv_service TYPE /iwfnd/med_mdl_service_grp_id
        !iv_version TYPE /iwbep/v4_med_service_version DEFAULT '0001'
        !iv_base_url TYPE string OPTIONAL .
    CLASS-METHODS launch_bsp
      IMPORTING
        !iv_external_service TYPE /iwfnd/med_mdl_service_grp_id
        !iv_version TYPE /iwfnd/med_mdl_version DEFAULT '0001'
        !iv_repository TYPE /iwbep/v4_med_repository_id OPTIONAL
        !iv_group_id TYPE /iwbep/v4_med_group_id OPTIONAL
        !iv_json TYPE xfeld OPTIONAL .
    CLASS-METHODS factory
      IMPORTING
        !iv_repository TYPE /iwbep/v4_med_repository_id OPTIONAL
        !iv_group_id TYPE /iwbep/v4_med_group_id OPTIONAL
        !iv_service TYPE /iwfnd/med_mdl_service_grp_id
        !iv_version TYPE /iwbep/v4_med_service_version DEFAULT '0001'
        !iv_base_url TYPE string OPTIONAL
      RETURNING
        VALUE(ri_openapi) TYPE REF TO zif_gw_openapi .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mi_metadata_handler TYPE REF TO zif_gw_openapi_metadata .
ENDCLASS.



CLASS zcl_gw_openapi IMPLEMENTATION.


  METHOD constructor.

*   If repository and group id are initial => OData V2 service
    IF iv_repository IS INITIAL AND iv_group_id IS INITIAL.
      me->mi_metadata_handler = zcl_gw_openapi_metadata_v2=>factory(
                                  iv_external_service = iv_service
                                  iv_version          = iv_version
                                  iv_base_url         = iv_base_url ).
    ELSE.
*     Repository and group id are not initial => OData V4 service
      me->mi_metadata_handler = zcl_gw_openapi_metadata_v4=>factory(
                                  iv_repository = iv_repository
                                  iv_group_id   = iv_group_id
                                  iv_service    = CONV /iwbep/v4_med_service_id( iv_service )
                                  iv_version    = iv_version
                                  iv_base_url   = iv_base_url ).
    ENDIF.

  ENDMETHOD.


  METHOD factory.

*   Return new instance of OpenAPI interface
    ri_openapi ?= NEW zcl_gw_openapi(
      iv_repository = iv_repository
      iv_group_id   = iv_group_id
      iv_service    = iv_service
      iv_version    = iv_version
      iv_base_url   = iv_base_url ).

  ENDMETHOD.


  METHOD launch_bsp.
    DATA: lv_url                  TYPE string,
          lv_url_1                TYPE agr_url2,
          lv_appl                 TYPE string,
          lv_page                 TYPE string,
          lt_params               TYPE tihttpnvp,
          lv_answer               TYPE string,
          lv_valueout             TYPE string,
          lv_is_syst_client_valid TYPE abap_bool VALUE abap_false.

    WHILE lv_is_syst_client_valid = abap_false.

      CLEAR: lv_answer, lv_valueout.

      CALL FUNCTION 'POPUP_TO_GET_VALUE'
        EXPORTING
          fieldname           = 'MANDT'
          tabname             = 'T000'
          titel               = 'Enter System Client'
          valuein             = sy-mandt
        IMPORTING
          answer              = lv_answer
          valueout            = lv_valueout
        EXCEPTIONS
          fieldname_not_found = 1
          OTHERS              = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF lv_answer = 'C'.
        RETURN.
      ENDIF.

      SELECT SINGLE @abap_true
        FROM t000
        INTO @lv_is_syst_client_valid
        WHERE mandt = @lv_valueout.

      IF lv_is_syst_client_valid = abap_false.
        MESSAGE `Client does not exist. Let's try again?!` TYPE 'I' DISPLAY LIKE 'I'.
      ENDIF.

    ENDWHILE.

*   Set parameters for BSP application
    lt_params = VALUE #( ( name = 'service' value = iv_external_service )
                         ( name = 'version' value = iv_version )
                         ( name = 'repository' value = iv_repository )
                         ( name = 'group' value = iv_group_id )
                         ( name = 'sap-client' value = lv_valueout )
                         ( name = 'sap-language' value = sy-langu ) ).

*   Set page
    IF iv_json = abap_false.
      lv_page = 'index.html'.
    ELSE.
      lv_page = 'openapi.json'.
    ENDIF.

*   Generate URL for BSP application
    cl_http_ext_webapp=>create_url_for_bsp_application(
      EXPORTING
        bsp_application      = 'ZGW_OPENAPI'
        bsp_start_page       = lv_page
        bsp_start_parameters = lt_params
      IMPORTING
        abs_url              = lv_url ).

*   Launch BSP application
    lv_url_1 = lv_url.

    CALL FUNCTION 'CALL_BROWSER'
      EXPORTING
        url                    = lv_url_1
*       BROWSER_TYPE           =
*       CONTEXTSTRING          =
      EXCEPTIONS
        frontend_not_supported = 1
        frontend_error         = 2
        prog_not_found         = 3
        no_batch               = 4
        unspecified_error      = 5
        OTHERS                 = 6.

  ENDMETHOD.


  METHOD zif_gw_openapi~get_json.

*   Get JSON data
    me->mi_metadata_handler->get_json(
      IMPORTING
        ev_json        = ev_json
        ev_json_string = ev_json_string ).

  ENDMETHOD.
ENDCLASS.