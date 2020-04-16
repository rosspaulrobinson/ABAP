*&---------------------------------------------------------------------*
*& Report  YGTTC_CBID_PACK
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  ygttc_cbid_pack.

data l_while type rs_bool.

tables:
  sscrfields.

data:
  go_cbid_pack               type ref to ygttc_cl_cbid_pack.

data:
  g_alv                      type string.

data:
  gt_rsdri_range             type rsdri_t_range.

data begin of gs_rsdri_range_x.
data mark                    type rs_bool.
include                      type rsdri_s_range.
data end of gs_rsdri_range_x.

data:
  gt_rsdri_range_x           like table of gs_rsdri_range_x.

field-symbols:
  <gs_rsdri_range_x>     like gs_rsdri_range_x.

data begin of gs_cha_map_x.
data mark                    type rs_bool.
include                      type ygttc_cl_cbid_pack=>s_cha_map.
data end of gs_cha_map_x.

data:
  gt_cha_map_x               like table of gs_cha_map_x.

data begin of gs_val_map_x.
data mark                    type rs_bool.
include                      type ygttc_cl_cbid_pack=>s_val_map.
data end of gs_val_map_x.

data:
  gt_val_map_x               like table of gs_val_map_x.


data:
  l_iobjnm                   type rsiobjnm.

data:
  lo_rsd_dta                 type ref to cl_rsd_dta,
  l_infoprov                 type rsinfoprov,
  l_tablename                type rollname,
  ls_dta                     type rsd_s_dta,
  ls_dta_pro                 type rsd_s_dta_pro,
  lt_dta_pro                 type rsd_t_dta_pro.

types:
  begin of s_para,
    name type c length 30,
    val type string,
  end of s_para.

data:
  lt_para type table of s_para.

field-symbols:
  <ls_para> type s_para.

data:
  l_spool                    type string.

data:
  g_ss_group_begin           type rs_bool,
  g_ss_group_end             type rs_bool.

*&---------------------------------------------------------------------*
*&       Class LCL
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl definition.

  public section.
    class-methods get_para
      importing
        !i_name type char30
      exporting
        !i_val type any.

    class-methods set_para
      importing
        !i_name type char30
        !i_val type any.

    class-methods initialise.

    class-methods ucomm__b_flt_so.

    class-methods ucomm__b_flt_st.

    class-methods ucomm__b_chamap.

    class-methods ucomm__b_valmap.

endclass.               "LCL

start-of-selection.

  "Define one parameter to store complex parameter values.
  parameters:
    p_para                   type string no-display.

  "Choose the action, either 'Read & Export' or 'Import & Write'.
  selection-screen: begin of block b_action with frame title text-act, skip.

  selection-screen: begin of line.
  selection-screen comment (31) t_action.

  selection-screen comment (13) t_read.
  parameters p_read type rs_bool radiobutton group act default 'X' user-command action.

  selection-screen comment (14) t_write.
  parameters p_write type rs_bool radiobutton group act.
  selection-screen: end of line.

  selection-screen end of block b_action.

  "Set the parameters for the action.
  selection-screen: begin of block b_param with frame title text-prm, skip.
  parameters:
    p_ipsrc                    type rsinfoprov visible length 20.

  parameters:
    p_iptrg                    type rsinfoprov visible length 20 modif id w.

  selection-screen: skip.

  parameters begin_1 type rs_bool.

  "Define characteristic selection,
  parameters p_chanm type rsdri_s_range-chanm visible length 20 default '' no-display.
  parameters p_sign  type rsdri_s_range-sign default 'I' no-display.
  parameters p_compop type rsdri_s_range-compop default 'EQ' no-display.
  parameters p_low type rsdri_s_range-low visible length 20 no-display.
  parameters p_high type rsdri_s_range-high visible length 20 no-display.

  "By select-options.
  selection-screen: begin of line.
  selection-screen comment (31) t_chasel.
  selection-screen pushbutton 33(20) c_flt_so user-command b_flt_so visible length 20.
  "By selection table.
  selection-screen pushbutton 54(20) c_flt_st user-command b_flt_st visible length 20.
  selection-screen: end of line.

  "selection-screen: skip.
  selection-screen comment 1(1) text-skp.

  "Define characteristic mapping.
  parameters p_chamap type ygttc_cl_cbid_pack=>s_cha_map-chanm visible length 20 no-display.
  parameters p_chafr type ygttc_cl_cbid_pack=>s_cha_map-from visible length 20 no-display.
  parameters p_chato type ygttc_cl_cbid_pack=>s_cha_map-to visible length 20 no-display.

  selection-screen: begin of line.
  selection-screen comment (31) t_chamap.
  selection-screen pushbutton 33(20) c_chamap user-command b_chamap visible length 20.
  selection-screen: end of line.

  selection-screen comment 1(1) text-skp.

  "Define value mapping.
  parameters p_valmap type ygttc_cl_cbid_pack=>s_val_map-iobjnm visible length 20 no-display.
  parameters p_valfr type ygttc_cl_cbid_pack=>s_val_map-from visible length 20 no-display.
  parameters p_valto type ygttc_cl_cbid_pack=>s_val_map-to visible length 20 no-display.

  selection-screen: begin of line.
  selection-screen comment (31) t_valmap.
  selection-screen pushbutton 33(20) c_valmap user-command b_valmap visible length 20.
  selection-screen: end of line.

  selection-screen comment 1(1) text-skp.

  parameters end_1 type rs_bool.

  select-options s_maiobj for l_iobjnm no intervals.
  select-options s_mtiobj for l_iobjnm no intervals.

  selection-screen: skip.

  parameters p_direct type string default 'C:\CBID\' lower case.
  parameters p_flpfix type c length 4.
  parameters p_usegui type rs_bool default 'X'.
  parameters p_zip type rs_bool default 'X'.

  selection-screen: skip.

  parameters p_get_td type rs_bool.
  parameters p_get_ma type rs_bool.
  parameters p_get_mt type rs_bool.

  selection-screen: skip.

  selection-screen: begin of line.
  selection-screen comment (31) text-c09 for field p_valmap.
  selection-screen comment (4) text-csv for field p_csv.
  parameters p_csv type rs_bool radiobutton group fmat default 'X'.
  selection-screen comment (4) text-dat for field p_dat.
  parameters p_dat type rs_bool radiobutton group fmat.
  selection-screen: end of line.

  selection-screen: skip.

  parameters p_pksize type i default 10000.
  parameters p_pkmax type i visible length 4.
  parameters p_pkskip type i visible length 4.

  selection-screen: skip.

  selection-screen: begin of line.
  parameters p_applog type rs_bool.
  selection-screen comment (31) text-app for field p_applog.
  parameters: p_extnum type c length 30.
  selection-screen: end of line.

  selection-screen:
    function key 1.

  selection-screen end of block b_param.

initialization.

  "set icon and text for function button
  sscrfields-functxt_01 = text-log.

  t_action = 'Action'.
  t_read = 'Read & Export'.
  t_write = 'Import & Write'.
  t_chasel = 'Char. selection (Read/Exp)'.
  t_valmap = 'Value mapping (Read/Exp)'.
  t_chamap = 'Char. mapping (Read/Exp)'.

  c_flt_so = '@RU@ Select options'.
  c_flt_st = '@RS@ Selection table'.
  c_chamap = '@EY@ Define mapping'.
  c_valmap = '@EY@ Define mapping'.


  "Set default selection variant based on username.
  data:
    l_report                 type rsvar-report,
    l_variant                type rsvar-variant.

  l_report  = sy-cprog.
  l_variant = sy-uname.

  call function 'RS_SUPPORT_SELECTIONS'
    exporting
      report               = l_report
      variant              = l_variant
    exceptions
      variant_not_existent = 1
      variant_obsolete     = 2
      others               = 3.

  "Adjust the select-options definition.
  data:
    ls_sscr_restrict         type sscr_restrict,
    ls_sscr_opt_list         type sscr_opt_list,
    ls_sscr_ass              type sscr_ass.

  clear ls_sscr_opt_list.
  ls_sscr_opt_list-name       = 'JUST_EQ'.
  ls_sscr_opt_list-options-eq = 'X'.
  append ls_sscr_opt_list to ls_sscr_restrict-opt_list_tab.

  clear ls_sscr_ass.
  ls_sscr_ass-kind    = 'S'.
  ls_sscr_ass-name    = 'S_MAIOBJ'.
  ls_sscr_ass-sg_main = 'I'.
  ls_sscr_ass-sg_addy = ''.
  ls_sscr_ass-op_main = 'JUST_EQ'.
  append ls_sscr_ass to ls_sscr_restrict-ass_tab.

  ls_sscr_ass-name    = 'S_MTIOBJ'.
  append ls_sscr_ass to ls_sscr_restrict-ass_tab.

  call function 'SELECT_OPTIONS_RESTRICT'
    exporting
      restriction            = ls_sscr_restrict
    exceptions
      too_late               = 1
      repeated               = 2
      selopt_without_options = 3
      selopt_without_signs   = 4
      invalid_sign           = 5
      empty_option_list      = 6
      invalid_kind           = 7
      repeated_kind_a        = 8
      others                 = 9.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

at selection-screen.

  case sy-ucomm.

    when 'FC01'.

      if go_cbid_pack is not initial.
        call method go_cbid_pack->view_spool.
      else.
        message 'Initialise instance first' type 'S' display like 'W'.
      endif.

    when others.
      write sy-ucomm.
  endcase.

  case sscrfields-ucomm.
    when 'B_CHAMAP'.

      g_alv = 'CHAMAP'.
      lcl=>ucomm__b_chamap( ).
    when 'B_VALMAP'.

      g_alv = 'VALMAP'.
      lcl=>ucomm__b_valmap( ).
    when 'B_FLT_SO'.

      g_alv = 'FLT_SO'.
      lcl=>ucomm__b_flt_so( ).
    when 'B_FLT_ST'.

      g_alv = 'FLT_ST'.
      lcl=>ucomm__b_flt_st( ).
    when others.
  endcase.

at selection-screen output.

  data:
    l_active type i.

  loop at screen.

    if screen-name cs 'BEGIN_' or
      screen-name cs 'END_'.
      screen-active = 0.
      modify screen.
    endif.
  endloop.

  if p_write = 'X'.

    l_active = 1.

    loop at screen.

      if screen-name cs 'BEGIN_'.
        l_active = 0.
        continue.
      endif.

      if screen-name cs 'END_'.
        l_active = 1.
        continue.
      endif.

      screen-active = l_active.
      modify screen.
    endloop.
  endif.

  if p_read = 'X'.

    l_active = 1.

    loop at screen.

      if screen-group1 = 'W'.
        screen-active = 0.
        modify screen.
        continue.
      endif.

      screen-active = l_active.
      modify screen.
    endloop.
  endif.

  if go_cbid_pack is not initial.
    p_extnum = go_cbid_pack->gs_log-extnumber.
  endif.

at selection-screen on end of s_maiobj.

at selection-screen on end of s_mtiobj.

end-of-selection.

  "Initialise YGTTC_CL_CBID_PACK class.
  lcl=>initialise( ).

  "Read or Write based on selections.
  if p_read = 'X'.
    message 'Start of Read & Export' type 'S'.
    go_cbid_pack->read_export_all( ).
  else.
    message 'Start of Import & Write' type 'S'.
    go_cbid_pack->import_write_all( ).
  endif.
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
form set_pf_status using rt_extab.

  data:
    ls_extab                 type slis_extab,
    lt_extab                 type slis_t_extab.

  case g_alv.
    when 'FLT_ST'. set titlebar 'FLT_ST'.
    when 'CHAMAP'. set titlebar 'CHAMAP'.
    when 'VALMAP'. set titlebar 'VALMAP'.
  endcase.

  set pf-status 'ALV_GRID' excluding lt_extab.
endform.                    "set_pf_status
*----------------------------------------------------------*
*       FORM USER_COMMAND                                  *
*----------------------------------------------------------*
*       --> R_UCOMM                                        *
*       --> RS_SELFIELD                                    *
*----------------------------------------------------------*
form user_command using r_ucomm like sy-ucomm
                  rs_selfield type slis_selfield.

** Any Changes you have entered on screen will now be stored within
** the original internal table which the ALV was build from (lt_cbid_map)

  field-symbols:
  <l_data> type any,
  <ls_data> type any,
  <lt_data> type standard table.

  case r_ucomm.
    when
      '&IC1' or "Double-click, or Hotspot click.
      'ANLE' or "Append new line
      'DELE'. "Delete marked lines

      case g_alv .
        when 'FLT_ST'. assign gt_rsdri_range_x to <lt_data>.
        when 'CHAMAP'. assign gt_cha_map_x to <lt_data>.
        when 'VALMAP'. assign gt_val_map_x to <lt_data>.
      endcase.

    when others.

      if r_ucomm = 'HELP'.
        case g_alv .
          when 'FLT_ST'. message 'Example: 0COMP_CODE | I | EQ | Z000 | Z999 ' type 'S' display like 'I'.
          when 'CHAMAP'. message 'Example: 0MAT_UNIT | 0MATERIAL | 0MATERIAL' type 'S' display like 'I'.
          when 'VALMAP'. message 'Example: 0LOGSYS | MQ7DVR3152 | RD7DVR3152' type 'S' display like 'I'.
        endcase.
      else.
        message 'Unexpected input' type 'S'.
      endif.

  endcase.

* Check function code
  case r_ucomm.
    when '&IC1'. "Double-click.

      data:
        l_mark type rs_bool.

      if rs_selfield-sel_tab_field = '1-MARK'.

        if rs_selfield-tabindex = 0.

          check <lt_data> is assigned.

          read table <lt_data> transporting no fields
            with key
              ('MARK') = ''.
          if sy-subrc = 0.
            l_mark = 'X'.
          else.
            l_mark = ''.
          endif.

          loop at <lt_data> assigning <ls_data>.
            assign component 'MARK' of structure <ls_data> to <l_data>.
            if sy-subrc = 0.
              <l_data> = l_mark.
            endif.
          endloop.
        else.

          read table <lt_data> assigning <ls_data> index rs_selfield-tabindex.
          if sy-subrc = 0.

            assign component 'MARK' of structure <ls_data> to <l_data>.
            if sy-subrc = 0.
              if <l_data> = 'X'.
                <l_data> = ''.
              else.
                <l_data> = 'X'.
              endif.
            endif.
          endif.
        endif.
      endif.
      rs_selfield-refresh = 'X'.

    when 'PASTE'.

      types:
        begin of s_paste,
          text                     type c length 8022,
        end of   s_paste.

      data:
        l_length                   type i,
        ls_paste                   type s_paste,
        lt_paste                   type standard table of s_paste.

      data:
        l_str                      type string,
        lt_str                     type stringtab.

      data:
        lo_root                    type ref to cx_root.

      data:
        l_iobjnm                   type rsiobjnm,
        l_spool                    type string.

      call method cl_gui_frontend_services=>clipboard_import
        importing
          data                 = lt_paste
          length               = l_length
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          others               = 4.
      if sy-subrc <> 0.
        write: / `@5C@ Error in clipboard import`.
      endif.

      try.

          call method cl_abap_conv_in_ce=>uccp
            exporting
              uccp = '0009'
            receiving
              char = l_str.

        catch cx_sy_conversion_codepage into lo_root.
          raise exception lo_root.
        catch cx_parameter_invalid_type into lo_root.
          raise exception lo_root.
        catch cx_sy_codepage_converter_init into lo_root.
          raise exception lo_root.
      endtry.

      case g_alv.
        when 'FLT_ST'.

          loop at lt_paste into ls_paste.

            clear gs_rsdri_range_x.
            split ls_paste-text
              at l_str into
                gs_rsdri_range_x-chanm
                gs_rsdri_range_x-sign
                gs_rsdri_range_x-compop
                gs_rsdri_range_x-low
                gs_rsdri_range_x-high.
            append gs_rsdri_range_x to gt_rsdri_range_x.
          endloop.

        when 'CHAMAP'.

          loop at lt_paste into ls_paste.

            clear gs_cha_map_x.
            split ls_paste-text
              at l_str into
                gs_cha_map_x-chanm
                gs_cha_map_x-from
                gs_cha_map_x-to.
            append gs_cha_map_x to gt_cha_map_x.
          endloop.

        when 'VALMAP'.

          loop at lt_paste into ls_paste.

            clear gs_val_map_x.
            split ls_paste-text
              at l_str into
                gs_val_map_x-iobjnm
                gs_val_map_x-from
                gs_val_map_x-to.
            append gs_val_map_x to gt_val_map_x.
          endloop.

        when others.
      endcase.

      rs_selfield-refresh = 'X'.

    when 'ANLE'.

      append initial line to <lt_data>.
      rs_selfield-refresh = 'X'.

    when 'DELE'.

      delete <lt_data> where (`MARK = 'X'`).
      rs_selfield-refresh = 'X'.

    when others.
  endcase.
endform.                    "user_command
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl implementation.

  method get_para.

    data:
      l_xml                  type string.


    if p_para is not initial.

      call transformation id
        source xml p_para
        result para = lt_para.
    endif.

    read table lt_para assigning <ls_para>
      with key
        name = i_name.
    if sy-subrc <> 0.

      append initial line to lt_para assigning <ls_para>.
      <ls_para>-name = i_name.
    endif.

    l_xml = <ls_para>-val.

    if l_xml is not initial.

      call transformation id
        source xml l_xml
        result para = i_val.
    endif.

  endmethod.                    "get_para

  method initialise.

    data:
      lt_cbid_src            type ygttc_cl_cbid_pack=>t_prov,
      lt_cbid_trg            type ygttc_cl_cbid_pack=>t_prov,
      ls_cbid_range          type rsdri_s_range,
      lt_cbid_range          type rsdri_t_range,
      ls_cbid_cha_map        type ygttc_cl_cbid_pack=>s_cha_map,
      lt_cbid_cha_map        type ygttc_cl_cbid_pack=>t_cha_map,
      ls_cbid_val_map        type ygttc_cl_cbid_pack=>s_val_map,
      lt_cbid_val_map        type ygttc_cl_cbid_pack=>t_val_map,
      l_file_format          type ygttc_cl_cbid_pack=>file_format,
      lt_cbid_ma             type ygttc_cl_cbid_pack=>t_iobj,
      lt_cbid_mt             type ygttc_cl_cbid_pack=>t_iobj.

    data:
      ls__s_maiobj           like line of s_mtiobj[],
      ls__s_mtiobj           like line of s_mtiobj[].


    "Set the source and target data providers.
    if p_ipsrc is not initial.
      append p_ipsrc to lt_cbid_src.
    endif.

    if p_iptrg is not initial.
      append p_iptrg to lt_cbid_trg.
    endif.

    "Set the selection range.
    lcl=>get_para( exporting i_name = 'LT_RSDRI_RANGE' importing i_val = lt_cbid_range ).

    if p_chanm is not initial.

      ls_cbid_range-chanm = p_chanm.
      ls_cbid_range-sign = 'I'.
      if p_high is initial.
        ls_cbid_range-compop = 'EQ'.
      else.
        ls_cbid_range-compop = 'BT'.
      endif.
      ls_cbid_range-low = p_low.
      ls_cbid_range-high = p_high.
      append ls_cbid_range to lt_cbid_range.
    endif.

    "Set the characteristic mapping.
    lcl=>get_para( exporting i_name = 'LT_CHA_MAP' importing i_val = lt_cbid_cha_map ).

    if p_chamap is not initial.

      ls_cbid_cha_map-chanm = p_chamap.
      ls_cbid_cha_map-from = p_chafr.
      ls_cbid_cha_map-to = p_chato.
      append ls_cbid_cha_map to lt_cbid_cha_map.
    endif.

    "Set the value mapping.
    lcl=>get_para( exporting i_name = 'LT_VAL_MAP' importing i_val = lt_cbid_val_map ).

    if p_valmap is not initial.

      ls_cbid_val_map-iobjnm = p_valmap.
      ls_cbid_val_map-from = p_valfr.
      ls_cbid_val_map-to = p_valto.
      append ls_cbid_val_map to lt_cbid_val_map.
    endif.

    "Set the file format.
    if p_csv = 'X'.
      l_file_format = 'CSV'.
    endif.

    if p_dat = 'X'.
      l_file_format = 'DAT'.
    endif.

    "Set the Attributes for selection.
    loop at s_maiobj into ls__s_maiobj.

      l_iobjnm = ls__s_maiobj-low.
      append l_iobjnm to lt_cbid_ma.
    endloop.

    "Set the Texts for selection.
    loop at s_mtiobj into ls__s_mtiobj.

      l_iobjnm = ls__s_mtiobj-low.
      append l_iobjnm to lt_cbid_mt.
    endloop.

    "Initialise class.
    create object go_cbid_pack
      exporting
        i_t_prov_src = lt_cbid_src
        i_t_prov_trg = lt_cbid_trg
        i_t_range    = lt_cbid_range
        i_t_iobj_ma  = lt_cbid_ma
        i_t_iobj_mt  = lt_cbid_mt
        i_t_cha_map  = lt_cbid_cha_map
        i_t_val_map  = lt_cbid_val_map
        i_directory  = p_direct
        i_usegui     = p_usegui
        i_get_td     = p_get_td
        i_get_ma     = p_get_ma
        i_get_mt     = p_get_mt
        i_appl_log   = p_applog.

    "Set additional 'default' parameters.
    go_cbid_pack->g_file_format = l_file_format.
    go_cbid_pack->g_file_prefix = p_flpfix.
    go_cbid_pack->g_file_compress = p_zip.
    go_cbid_pack->g_package_size = p_pksize.
    go_cbid_pack->g_no_of_packages = p_pkmax.
    go_cbid_pack->g_skip_packages = p_pkskip.

    p_applog = go_cbid_pack->g_appl_log.

    sscrfields-functxt_02 = text-exp.
    sscrfields-functxt_03 = text-imp.
    sscrfields-functxt_04 = text-log.
  endmethod.                    "initialise

  method set_para.

    data:
      l_xml                  type string.


    if p_para is not initial.

      call transformation id
        source xml p_para
        result para = lt_para.
    endif.

    call transformation id
      source para = i_val
      result xml l_xml.

    read table lt_para assigning <ls_para>
      with key
        name = i_name.
    if sy-subrc <> 0.

      append initial line to lt_para assigning <ls_para>.
      <ls_para>-name = i_name.
    endif.

    <ls_para>-val = l_xml.

    call transformation id
      source para = lt_para
      result xml p_para.
  endmethod.                    "set_para

  method ucomm__b_flt_so.

    data:
      l_rsdri_range          type string,
      ls_rsdri_range         type rsdri_s_range,
      lt_rsdri_range         type rsdri_t_range.

    data:
      ls_selopt              type rsdsselopt,
      ls_frange              type rsds_frange,
      lt_frange              type rsds_frange_t,
      ls_field_ranges_int    type rsds_range,
      lt_field_ranges_int    type rsds_trange.

    data:
      ls_field_ranges        type rsds_range,
      lt_field_ranges        type rsds_trange.

    field-symbols:
      <ls_field_ranges_int>  type rsds_range,
      <ls_frange>            type rsds_frange.

    data:
      ls_fields_tab          type rsdsfields,
      lt_fields_tab          type table of rsdsfields.

    data:
      ls_tables_tab          type rsdstabs,
      lt_tables_tab          type table of rsdstabs.

    data :
      lt_expressions         type rsds_texpr,
      lt_where_clauses       type rsds_twhere,
      l_selection_id         type dynselid,
      l_number_of_active_fields  like sy-tfill,
      l_title                like sy-title.


    if p_ipsrc is not initial.

      call method cl_rsd_dta=>factory
        exporting
          i_infoprov = p_ipsrc
        receiving
          r_r_dta    = lo_rsd_dta
        exceptions
          not_found  = 1
          others     = 2.
      if sy-subrc <> 0.
        message 'Invalid source InfoProvider' type 'S' display like 'W'. return.
      endif.
    else.
      message 'Provide a source InfoProvider' type 'S' display like 'W'. return.
    endif.

    call method lo_rsd_dta->if_rsd_dta~dta_get_info
      exporting
        i_with_atr_nav  = rs_c_true
      importing
        e_s_dta         = ls_dta
        e_t_dta_pro     = lt_dta_pro
      exceptions
        dta_not_found   = 1
        iobj_not_found  = 2
        objvers_invalid = 3
        others          = 4.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    "Get the persistant data from the parameter string in XML format.
    lcl=>get_para( exporting i_name = 'LT_RSDRI_RANGE' importing i_val = lt_rsdri_range ).

    loop at lt_rsdri_range into ls_rsdri_range.

      read table lt_dta_pro into ls_dta_pro
        with key
          iobjnm = ls_rsdri_range-chanm.
      if sy-subrc <> 0.
        l_spool = ls_rsdri_range-chanm && ` is not in the source InfoProv and has been removed`.
        message l_spool type 'S' display like 'W'.
        continue.
      endif.

      ls_fields_tab-tablename = ls_dta-viewtotal.
      ls_fields_tab-fieldname = ls_dta_pro-fieldnm.
      collect ls_fields_tab into lt_fields_tab.

      read table lt_frange assigning <ls_frange>
        with key
          fieldname = ls_dta_pro-fieldnm.
      if sy-subrc <> 0.

        append initial line to lt_frange assigning <ls_frange>.
        <ls_frange>-fieldname = ls_dta_pro-fieldnm.
      endif.

      ls_selopt-sign   = ls_rsdri_range-sign.
      ls_selopt-option = ls_rsdri_range-compop.
      ls_selopt-low    = ls_rsdri_range-low.
      ls_selopt-high   = ls_rsdri_range-high.
      append ls_selopt to <ls_frange>-selopt_t.
    endloop.

    ls_field_ranges_int-tablename = ls_dta-viewtotal.
    ls_field_ranges_int-frange_t = lt_frange.
    append ls_field_ranges_int to lt_field_ranges_int.
    "      endif.

    "Define the tables from which the selection can be made.
    clear lt_tables_tab.
    ls_tables_tab-prim_tab = ls_dta-viewtotal.
    append ls_tables_tab to lt_tables_tab.

    do 2 times.

      call function 'FREE_SELECTIONS_INIT'
        exporting
          kind                     = 'T'
          expressions              = lt_expressions
          field_ranges_int         = lt_field_ranges_int
        importing
          selection_id             = l_selection_id
          number_of_active_fields  = l_number_of_active_fields
        tables
          tables_tab               = lt_tables_tab
          fields_tab               = lt_fields_tab
        exceptions
          fields_incomplete        = 01
          fields_no_join           = 02
          field_not_found          = 03
          no_tables                = 04
          table_not_found          = 05
          expression_not_supported = 06
          incorrect_expression     = 07
          illegal_kind             = 08
          area_not_found           = 09
          inconsistent_area        = 10
          kind_f_no_fields_left    = 11
          kind_f_no_fields         = 12
          too_many_fields          = 13.
      if sy-subrc = 0.
        exit.
      else.
        case sy-subrc.
          when 3.
            clear lt_fields_tab.
            message id 'YGTTC_CBID' type 'I' number 004.
          when others.
            message id 'YGTTC_CBID' type 'I' number 005.
            return.
        endcase.
      endif.
    enddo.

    call function 'FREE_SELECTIONS_DIALOG'
      exporting
        selection_id            = l_selection_id
        title                   = 'Char. selection (Read/Exp) define by selection options'
        tree_visible            = 'X'
      importing
        where_clauses           = lt_where_clauses
        expressions             = lt_expressions
        number_of_active_fields = l_number_of_active_fields
        field_ranges            = lt_field_ranges
      tables
        fields_tab              = lt_fields_tab
      exceptions
        internal_error          = 01
        no_action               = 02
        no_fields_selected      = 03
        no_tables_selected      = 04
        selid_not_found         = 05.
    if sy-subrc <> 0.
      return.
    endif.

    clear lt_rsdri_range.
    loop at lt_field_ranges into ls_field_ranges.

      loop at  ls_field_ranges-frange_t into ls_frange.

        loop at ls_frange-selopt_t into ls_selopt.

          read table lt_dta_pro into ls_dta_pro with key fieldnm = ls_frange-fieldname.
          if sy-subrc = 0.

            ls_rsdri_range-chanm  = ls_dta_pro-iobjnm.
            ls_rsdri_range-sign   = ls_selopt-sign.
            ls_rsdri_range-compop = ls_selopt-option.
            ls_rsdri_range-low    = ls_selopt-low.
            ls_rsdri_range-high   = ls_selopt-high.
            append ls_rsdri_range to lt_rsdri_range.
          endif.
        endloop.
      endloop.
    endloop.

    lcl=>set_para( exporting i_name = 'LT_RSDRI_RANGE' i_val = lt_rsdri_range ).
  endmethod.                    "ucomm__b_flt_so
  method ucomm__b_flt_st.

    data:
      ls_rsdri_range         type rsdri_s_range,
      lt_rsdri_range         type rsdri_t_range.

    data:
      l_mark                 type rs_bool.

    data:
      ls_fieldcatalog        type slis_fieldcat_alv,
      lt_fieldcatalog        type slis_t_fieldcat_alv,
      lt_fieldcat            type lvc_t_fcat,
      ls_fieldcat            type lvc_s_fcat.

    data:
      gd_tab_group           type slis_t_sp_group_alv,
      gd_layout              type lvc_s_layo,     "slis_layout_alv,
      gd_repid               like sy-repid.


    lcl=>get_para( exporting i_name = 'LT_RSDRI_RANGE' importing i_val = lt_rsdri_range ).

    clear gt_rsdri_range_x.
    loop at lt_rsdri_range into ls_rsdri_range.
      move-corresponding ls_rsdri_range to gs_rsdri_range_x.
      append gs_rsdri_range_x to gt_rsdri_range_x.
    endloop.

    ls_fieldcat-fieldname   = 'MARK'.
    ls_fieldcat-scrtext_m   = '@2X@'.
    ls_fieldcat-col_pos     = 1.
    ls_fieldcat-outputlen   = 3.
    ls_fieldcat-checkbox    = 'X'.
    ls_fieldcat-edit        = 'X'.
    ls_fieldcat-hotspot     = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'CHANM'.
    ls_fieldcat-scrtext_m   = 'Characteristic'.
    ls_fieldcat-col_pos     = 2.
    ls_fieldcat-outputlen   = 30.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.


    ls_fieldcat-fieldname   = 'SIGN'.
    ls_fieldcat-scrtext_m   = 'Sign'.
    ls_fieldcat-col_pos     = 3.
    ls_fieldcat-outputlen   = 4.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'COMPOP'.
    ls_fieldcat-scrtext_m   = 'Opt.'.
    ls_fieldcat-col_pos     = 4.
    ls_fieldcat-outputlen   = 4.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'LOW'.
    ls_fieldcat-scrtext_m   = 'From'.
    ls_fieldcat-col_pos     = 5.
    ls_fieldcat-outputlen   = 30.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'HIGH'.
    ls_fieldcat-scrtext_m   = 'To'.
    ls_fieldcat-col_pos     = 6.
    ls_fieldcat-outputlen   = 30.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    data:
      l_grid_title type lvc_title.

    "    l_grid_title = 'Ross.'.

    call function 'REUSE_ALV_GRID_DISPLAY_LVC'
      exporting
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_program       = 'YGTTC_CBID_PACK'
        i_callback_user_command  = 'USER_COMMAND'
                                                                                                                                    "        i_grid_title             = l_grid_title
        is_layout_lvc            = gd_layout
        it_fieldcat_lvc          = lt_fieldcat
        i_save                   = 'X'
      tables
        t_outtab                 = gt_rsdri_range_x
      exceptions
        program_error            = 1
        others                   = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    clear lt_rsdri_range.
    loop at gt_rsdri_range_x into gs_rsdri_range_x.
      move-corresponding gs_rsdri_range_x to ls_rsdri_range.
      append ls_rsdri_range to lt_rsdri_range.
    endloop.

    lcl=>set_para( exporting i_name = 'LT_RSDRI_RANGE' i_val = lt_rsdri_range ).

  endmethod.                    "ucomm__b_flt_st

  method ucomm__b_chamap.

    data:
      ls_cha_map             type ygttc_cl_cbid_pack=>s_cha_map,
      lt_cha_map             type ygttc_cl_cbid_pack=>t_cha_map.

    data:
      l_mark                 type rs_bool.

    data:
      ls_fieldcatalog        type slis_fieldcat_alv,
      lt_fieldcatalog        type slis_t_fieldcat_alv,
      lt_fieldcat            type lvc_t_fcat,
      ls_fieldcat            type lvc_s_fcat.

    data:
      gd_tab_group           type slis_t_sp_group_alv,
      gd_layout              type lvc_s_layo,     "slis_layout_alv,
      gd_repid               like sy-repid.


    lcl=>get_para( exporting i_name = 'LT_CHA_MAP' importing i_val = lt_cha_map ).

    clear gt_cha_map_x.
    loop at lt_cha_map into ls_cha_map.
      move-corresponding ls_cha_map to gs_cha_map_x.
      append gs_cha_map_x to gt_cha_map_x.
    endloop.

    ls_fieldcat-fieldname   = 'MARK'.
    ls_fieldcat-scrtext_m   = '@2X@'.
    ls_fieldcat-col_pos     = 1.
    ls_fieldcat-outputlen   = 3.
    ls_fieldcat-checkbox    = 'X'.
    ls_fieldcat-edit        = 'X'.
    ls_fieldcat-hotspot     = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'CHANM'.
    ls_fieldcat-scrtext_m   = 'Characteristic'.
    ls_fieldcat-col_pos     = 2.
    ls_fieldcat-outputlen   = 30.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.


    ls_fieldcat-fieldname   = 'FROM'.
    ls_fieldcat-scrtext_m   = 'From'.
    ls_fieldcat-col_pos     = 3.
    ls_fieldcat-outputlen   = 15.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'TO'.
    ls_fieldcat-scrtext_m   = 'To.'.
    ls_fieldcat-col_pos     = 4.
    ls_fieldcat-outputlen   = 15.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    data:
      l_grid_title type lvc_title.

    "    l_grid_title = 'Ross.'.

    call function 'REUSE_ALV_GRID_DISPLAY_LVC'
      exporting
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_program       = 'YGTTC_CBID_PACK'
        i_callback_user_command  = 'USER_COMMAND'
                                                                                                                                    "        i_grid_title             = l_grid_title
        is_layout_lvc            = gd_layout
        it_fieldcat_lvc          = lt_fieldcat
        i_save                   = 'X'
      tables
        t_outtab                 = gt_cha_map_x
      exceptions
        program_error            = 1
        others                   = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    clear lt_cha_map.
    loop at gt_cha_map_x into gs_cha_map_x.
      move-corresponding gs_cha_map_x to ls_cha_map.
      append ls_cha_map to lt_cha_map.
    endloop.

    lcl=>set_para( exporting i_name = 'LT_CHA_MAP' i_val = lt_cha_map ).

  endmethod.                    "ucomm__b_chamap

  method ucomm__b_valmap.

    data:
      ls_val_map             type ygttc_cl_cbid_pack=>s_val_map,
      lt_val_map             type ygttc_cl_cbid_pack=>t_val_map.

    data:
      l_mark                 type rs_bool.

    data:
      ls_fieldcatalog        type slis_fieldcat_alv,
      lt_fieldcatalog        type slis_t_fieldcat_alv,
      lt_fieldcat            type lvc_t_fcat,
      ls_fieldcat            type lvc_s_fcat.

    data:
      gd_tab_group           type slis_t_sp_group_alv,
      gd_layout              type lvc_s_layo,     "slis_layout_alv,
      gd_repid               like sy-repid.


    lcl=>get_para( exporting i_name = 'LT_VAL_MAP' importing i_val = lt_val_map ).

    clear gt_val_map_x.
    loop at lt_val_map into ls_val_map.
      move-corresponding ls_val_map to gs_val_map_x.
      append gs_val_map_x to gt_val_map_x.
    endloop.

    ls_fieldcat-fieldname   = 'MARK'.
    ls_fieldcat-scrtext_m   = '@2X@'.
    ls_fieldcat-col_pos     = 1.
    ls_fieldcat-outputlen   = 3.
    ls_fieldcat-checkbox    = 'X'.
    ls_fieldcat-edit        = 'X'.
    ls_fieldcat-hotspot     = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'IOBJNM'.
    ls_fieldcat-scrtext_m   = 'InfoObject'.
    ls_fieldcat-col_pos     = 2.
    ls_fieldcat-outputlen   = 30.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.


    ls_fieldcat-fieldname   = 'FROM'.
    ls_fieldcat-scrtext_m   = 'From'.
    ls_fieldcat-col_pos     = 3.
    ls_fieldcat-outputlen   = 15.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    ls_fieldcat-fieldname   = 'TO'.
    ls_fieldcat-scrtext_m   = 'To'.
    ls_fieldcat-col_pos     = 4.
    ls_fieldcat-outputlen   = 15.
    ls_fieldcat-edit        = 'X'.
    append ls_fieldcat to lt_fieldcat.
    clear  ls_fieldcat.

    data:
      l_grid_title type lvc_title.

    "    l_grid_title = 'Ross.'.

    call function 'REUSE_ALV_GRID_DISPLAY_LVC'
      exporting
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_program       = 'YGTTC_CBID_PACK'
        i_callback_user_command  = 'USER_COMMAND'
                                                                                                                                    "        i_grid_title             = l_grid_title
        is_layout_lvc            = gd_layout
        it_fieldcat_lvc          = lt_fieldcat
        i_save                   = 'X'
      tables
        t_outtab                 = gt_val_map_x
      exceptions
        program_error            = 1
        others                   = 2.
    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    clear lt_val_map.
    loop at gt_val_map_x into gs_val_map_x.
      move-corresponding gs_val_map_x to ls_val_map.
      append ls_val_map to lt_val_map.
    endloop.

    lcl=>set_para( exporting i_name = 'LT_VAL_MAP' i_val = lt_val_map ).

  endmethod.                    "ucomm__b_valmap
endclass.               "lcl
