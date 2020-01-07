*&---------------------------------------------------------------------*
*& Include ZGW_SERVICE_OVERVIEW_TOP                 - Report ZGW_SERVICE_OVERVIEW
*&---------------------------------------------------------------------*


TABLES: /iwfnd/i_med_srh,
        /iwbep/i_v4_msrv,
        /iwfnd/c_v4_msgr,
        /iwbep/i_v4_msga.

SELECTION-SCREEN BEGIN OF BLOCK bl0 WITH FRAME TITLE TEXT-000.
PARAMETERS: p_v2 TYPE xfeld RADIOBUTTON GROUP rdb1 DEFAULT 'X' USER-COMMAND ent1,
            p_v4 TYPE xfeld RADIOBUTTON GROUP rdb1.
SELECTION-SCREEN END OF BLOCK bl0.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_name2 FOR /iwfnd/i_med_srh-service_name,
                s_vers2 FOR /iwfnd/i_med_srh-service_version.

SELECT-OPTIONS: s_name4 FOR /iwbep/i_v4_msrv-service_id,
                s_vers4 FOR /iwbep/i_v4_msrv-service_version,
                s_grp4 FOR /iwfnd/c_v4_msgr-group_id,
                s_rep4 FOR /iwbep/i_v4_msga-repository_id.
SELECTION-SCREEN END OF BLOCK bl1.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_ui   TYPE xfeld RADIOBUTTON GROUP rdb2 DEFAULT 'X' USER-COMMAND ent1,
            p_json TYPE xfeld RADIOBUTTON GROUP rdb2.
SELECTION-SCREEN END OF BLOCK bl2.
