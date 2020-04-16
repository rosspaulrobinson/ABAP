*&---------------------------------------------------------------------*
*& Report  YGTTC_PCMV_SCENARIO_1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  ygttc_pcmv_scenario_1.

tables:
  sscrfields.

data:
  lt_rsdri_range             type rsdri_t_range.

data:
  lo_container               type ref to cl_gui_docking_container,
  lo_alv_grid                type ref to cl_gui_alv_grid.

data:
  ls_f4                      type lvc_s_f4,
  lt_f4                      type lvc_t_f4.

data:
  ls_lvc_fcat                type lvc_s_fcat,
  lt_lvc_fcat                type lvc_t_fcat,
  ls_lvc_lay                 type lvc_s_layo.

field-symbols:
  <ls_lvc_fcat>              type lvc_s_fcat.

data:
  ls_alv_filter              type ygttc_cl_pcmv=>s_alv_filter,
  lt_alv_filter              type ygttc_cl_pcmv=>t_alv_filter.

field-symbols:
  <ls_alv_filter>             type any,
  <lt_alv_filter>             type standard table.

data:
  ls_dd03l                   type dd03l,
  lt_dd03l                   type table of dd03l.

data:
  lo_chain_main              type ref to cl_rspc_chain,
  lo_chain_local             type ref to cl_rspc_chain.

data:
  ls_rspcchain               type rspcchain,
  lt_rspcchain               type table of rspcchain.

data:
  lo_trigger                 type ref to cl_rspc_trigger.

data:
  lo_variant                 type ref to cl_rspc_variant.

data:
  l_exists                   type rs_bool.

data:
  l_new                      type rs_bool.

data:
  l_subrc                    type sysubrc.

data:
  lt_msg                     type rs_t_msg.

data:
  l_chain                    type rspc_chain,
  l_chain_last               type rspc_chain,
  lt_chain                   type table of rspc_chain,
  l_chain_text               type rstxtlg.

data:
  l_local_chain_num          type i.

data:
  l_trigger                  type rspc_variant.

data:
  l_load_typ                 type rspc_variant.

data:
  ls_rspcvariant             type rspcvariant,
  lt_rspcvariant             type rspc_t_variant,
  ls_rspcvariantt            type rspcvariantt.

data:
  ls_variant                 type rspc_s_variante,
  ls_variant_from            type rspc_s_variante,
  ls_variant_to              type rspc_s_variante,
  lt_variant                 type rspc_t_variante.

data:
  ls_start                   type rspc_s_chain,
  ls_end                     type rspc_s_chain,
  l_color                    type rspc_eventcolor,
  lt_conflicts               type rspc_t_conflicts.

data:
  l_variant                   type rspc_variant,
  l_variant_text             type rstxtlg,
  l_startspecs               type tbtcstrt,
  l_meta                     type rs_bool,
  l_no_transport             type rs_bool,
  l_modify                   type rs_bool.

data:
  l_tabix                    type sytabix.

types:
  begin of s_filter,
    group                    type p length 2,
    txtlg                    type rstxtlg,
  end of s_filter.

data:
  ls_filter                  type s_filter,
  lt_filter                  type table of s_filter.

data:
  l_selscr_valid             type i.

data:
  l_str                      type string.

data:
  l_xml                      type string.

data:
  l_object                   type sobj_name,
  l_object_class             type trobjtype,
  l_trkorr                   type trkorr.
*&---------------------------------------------------------------------*
*&       Class LCL
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class msg definition.

  public section.

    class-data:
      g_msgid_default        type symsgid value 'YGTBW',
      g_msgno_default        type symsgno value '200'.

    class-data:
      g_write                type rs_bool value 'X',
      g_message_flag         type rs_bool value '',
      g_spool_flag           type rs_bool value ''.

    class-methods add
      importing
        msg type string optional
        s_msg type rs_s_msg optional
        t_msg type rs_t_msg optional
      preferred parameter msg
      returning value(rt_msg) type rs_t_msg.

endclass.                    "msg DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl definition.

  public section.

    class-methods create_trigger
      importing
        i_variant type rspc_variant.

    class-methods create_chain
      importing
        i_chain type rspc_chain
        i_chain_text type rstxtlg
      exporting
        eo_chain type ref to cl_rspc_chain.

    class-methods add_pcmv
      changing
        co_chain type ref to cl_rspc_chain.

    class-methods add_ipdtp
      changing
        co_chain type ref to cl_rspc_chain.

    class-methods add_chain1
      changing
        co_chain type ref to cl_rspc_chain.

    class-methods add_chain2
      changing
        co_chain type ref to cl_rspc_chain.

    methods
      handle_onf4 for event onf4 of cl_gui_alv_grid
        importing
          er_event_data
          es_row_no
          et_bad_cells
          e_display
          e_fieldname
          e_fieldvalue.
endclass.               "LCL


initialization.

  data
    lo_lcl                   type ref to lcl.

  create object lo_lcl.

  selection-screen begin of screen 0100.
  parameters p_src type rsbkdtp-src.
  parameters p_tgt type rsbkdtp-tgt.
  selection-screen end of screen 0100.

  selection-screen begin of line.
  selection-screen comment (33) c_mchain for field p_mchain.
  parameters p_mchain type c length 19 default 'Y0FFAPUU_Y3FFAPUU_X'.
  selection-screen comment 57(7)  c_sfix.
  selection-screen end of line.

  c_mchain = text-mpc.
  c_sfix = text-sfx.

  parameters:
    p_mchtxt type char30 lower case default 'Y0 > Y3 segmented Full-repair'.

  parameters:
    p_applnm type rsapplnm default 'YGTTC_PCMV'.

  selection-screen skip 1.

  parameters:
    p_ipdtp type char30.

  parameters:
    p_chain1 type char30.

  parameters:
    p_chain2 type char30.

  selection-screen skip 1.

  parameters:
    p_devcls type devclass default 'YGGTBMBG' obligatory.

  parameters:
    p_trkorr type trkorr.

  selection-screen skip 1.

  parameters:
    p_test type rs_bool default 'X'.

  parameters:
    p_range type string no-display.

start-of-selection.

at selection-screen.

  "Validate main-chain description.
  if p_mchtxt is initial.
    p_mchtxt = p_mchain.

    message id 'YGTBW' type 'S' number '200' display like 'I'
               with 'Main chain description set to technical name'.
  endif.

  "Get the selections from the ALV grid.
  lo_alv_grid->check_changed_data( ).

  "Move the data from the range table into a string.
  call transformation id
    source para = lt_alv_filter
    result xml l_xml.

  p_range = l_xml.

at selection-screen on p_ipdtp.

  "Validate the DTP/IP entry and determine the loading type.
  select single logdpid
    into l_str
    from rsldpio
    where logdpid = p_ipdtp.
  if sy-subrc = 0.

    l_load_typ = 'LOADING'.
  else.

    select single dtp
      into l_str
      from rsbkdtp
      where dtp = p_ipdtp.
    if sy-subrc = 0.

      l_load_typ = 'DTP_LOAD'.
    else.
      message id 'YGTBW' type 'W' number '200'
                 with 'Invalid IP/DTP' p_ipdtp.

      add 1 to l_selscr_valid.
    endif.
  endif.

at selection-screen on p_chain1.

  if p_chain1 is not initial.

    select single chain_id
      into l_chain
      from rspcchain
      where chain_id = p_chain1 and
            objvers = 'A'.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'W' number '200'
                 with 'First optional sub-chain is not valid'.

      add 1 to l_selscr_valid.
    endif.
  endif.

  if p_chain1 is not initial and p_chain2 is not initial.
    if p_chain1 = p_chain2.

      message id 'YGTBW' type 'W' number '200'
                 with 'First and second optional sub-chains are identical'.

      add 1 to l_selscr_valid.
    endif.
  endif.

at selection-screen on p_chain2.

  if p_chain2 is not initial.

    select single chain_id
      into l_chain
      from rspcchain
      where chain_id = p_chain2 and
            objvers = 'A'.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'W' number '200'
                 with 'Second optional sub-chain is not valid'.

      add 1 to l_selscr_valid.
    endif.
  endif.

  if p_chain1 is not initial and p_chain2 is not initial.
    if p_chain1 = p_chain2.

      message id 'YGTBW' type 'W' number '200'
                 with 'First and second optional sub-chains are identical'.

      add 1 to l_selscr_valid.
    endif.
  endif.

at selection-screen on value-request for p_ipdtp.

  if p_ipdtp cp 'ZPAK*'.
    l_load_typ = 'IP'.
  elseif p_ipdtp cp 'DTP*'.
    l_load_typ = 'DTP'.
  else.

    data:
      l_sqlwhere           type string,
      l_answer             type c,
      lt_spopli            type table of spopli,
      ls_spopli            type spopli.

    ls_spopli-varoption = 'InfoPackage'.
    append ls_spopli to lt_spopli.

    ls_spopli-varoption = 'DTP'.
    append ls_spopli to lt_spopli.

    call function 'POPUP_TO_DECIDE_LIST'
      exporting
        start_col          = 70
        start_row          = 5
        textline1          = ''
        titel              = 'Load type'
      importing
        answer             = l_answer
      tables
        t_spopli           = lt_spopli
      exceptions
        not_enough_answers = 1
        too_much_answers   = 2
        too_much_marks     = 3
        others             = 4.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unexpected error.'.
    else.

      case l_answer.
        when '1'.
          l_load_typ = 'IP'.
        when '2'.
          l_load_typ = 'DTP'.
        when others.
          message id 'YGTBW' type 'S' number '200' display like 'W'
                     with 'F4 help unavailable.' 'Load type cannot be determined.'.
      endcase.
    endif.

    types:
      ty_c200                type c length 200.

    data:
      ls_value_tab           type ty_c200,
      lt_value_tab           type table of ty_c200,
      ls_field_tab           type dfies,
      lt_field_tab           type table of dfies,
      ls_return_tab          type ddshretval,
      lt_return_tab          type table of ddshretval.

    data:
      l_returncode           type c,
      ls_fields              type sval,
      lt_fields              type table of sval.

    data:
      l_src                  type rsbkdtp-src,
      l_tgt                  type rsbkdtp-tgt.

    data:
      l_sqlwhere_src         type string,
      l_sqlwhere_tgt         type string.

    if l_load_typ = 'IP'.

      data:
        ls_rsldpio           type rsldpio,
        lt_rsldpio           type table of rsldpio.

    endif.

    if l_load_typ = 'DTP'.

      data:
        ls_rsbkdtp           type rsbkdtp,
        lt_rsbkdtp           type table of rsbkdtp.

      ls_fields-tabname = 'RSBKDTP'.
      ls_fields-fieldname = 'SRC'.
      ls_fields-value = 'Y3%'.
      append ls_fields to lt_fields.

      ls_fields-tabname = 'RSBKDTP'.
      ls_fields-fieldname = 'TGT'.
      ls_fields-value = 'Y5%'.
      append ls_fields to lt_fields.

      call function 'POPUP_GET_VALUES'
        exporting
          popup_title     = 'Source and Target selection (Wildcard = `%`)'
          start_column    = '70'
          start_row       = '5'
        importing
          returncode      = l_returncode
        tables
          fields          = lt_fields
        exceptions
          error_in_fields = 1
          others          = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      if l_returncode = ''.

        "Get the SRC SQL where clause.
        read table lt_fields into ls_fields
          with key
            fieldname = 'SRC'.
        if sy-subrc = 0 and ls_fields-value is not initial.
          l_src = ls_fields-value.

          if l_src cs '%'.
            l_sqlwhere_src = |SRC LIKE '{ l_src }'|.
          else.
            l_sqlwhere_src = |SRC = '{ l_src }'|.
          endif.
        endif.

        "Get the TGT SQL where clause.
        read table lt_fields into ls_fields
          with key
            fieldname = 'TGT'.
        if sy-subrc = 0 and ls_fields-value is not initial.
          l_tgt = ls_fields-value.

          if l_tgt cs '%'.
            l_sqlwhere_tgt = |TGT LIKE '{ l_tgt }'|.
          else.
            l_sqlwhere_tgt = |TGT = '{ l_tgt }'|.
          endif.
        endif.

        "Combine the SQL where clauses.
        if l_sqlwhere_src <> ''.

          l_sqlwhere = l_sqlwhere_src.

          if l_sqlwhere_tgt <> ''.
            l_sqlwhere = l_sqlwhere && ` AND ` && l_sqlwhere_tgt.
          endif.
        else.
          if l_sqlwhere_tgt <> ''.
            l_sqlwhere = l_sqlwhere_tgt.
          endif.
        endif.
      endif.

      try.

          select dtp src tgt up to 500 rows
            into corresponding fields of table lt_rsbkdtp
            from rsbkdtp
            where (l_sqlwhere).
          if sy-subrc = 0.

            loop at lt_rsbkdtp into ls_rsbkdtp.

              ls_value_tab = ls_rsbkdtp-dtp.
              append ls_value_tab to lt_value_tab.

              ls_value_tab = ls_rsbkdtp-src.
              append ls_value_tab to lt_value_tab.

              ls_value_tab = ls_rsbkdtp-tgt.
              append ls_value_tab to lt_value_tab.
            endloop.

            ls_field_tab-tabname = 'RSBKDTP'.
            ls_field_tab-fieldname = 'DTP'.
            append ls_field_tab to lt_field_tab.

            ls_field_tab-tabname = 'RSBKDTP'.
            ls_field_tab-fieldname = 'SRC'.
            append ls_field_tab to lt_field_tab.

            ls_field_tab-tabname = 'RSBKDTP'.
            ls_field_tab-fieldname = 'TGT'.
            append ls_field_tab to lt_field_tab.
          endif.

        catch cx_sy_dynamic_osql_syntax.
      endtry.

      call function 'F4IF_INT_TABLE_VALUE_REQUEST'
        exporting
          retfield        = 'DTP'
        tables
          value_tab       = lt_value_tab
          field_tab       = lt_field_tab
          return_tab      = lt_return_tab
        exceptions
          parameter_error = 1
          no_values_found = 2
          others          = 3.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      read table lt_return_tab into ls_return_tab
        with key
          fieldname = 'DTP'.
      if sy-subrc = 0.
        p_ipdtp = ls_return_tab-fieldval.
      endif.
    endif.
  endif.

at selection-screen on value-request for p_trkorr.

  l_object = p_mchain && '_FR'.

  call function 'RS_CORR_INSERT'
    exporting
      object                   = l_object
      object_class             = 'RSPC'
      mode                     = 'INSERT'
      global_lock              = 'X'
      devclass                 = p_devcls
      master_language          = 'E'
      object_class_supports_ma = 'X'
    importing
      korrnum                  = p_trkorr
    exceptions
      cancelled                = 1
      permission_failure       = 2
      unknown_objectclass      = 3
      others                   = 4.

at selection-screen output.

  "Only show transport information in development.
  loop at screen.

    if screen-name cs 'P_DEVCLS' or screen-name cs 'P_TRKORR'.

      if sy-sysid = 'RD8' or sy-sysid = 'G13'.

        screen-active = 1.
        modify screen.
      else.

        screen-active = 0.
        modify screen.
      endif.
    endif.
  endloop.

  if lo_container is initial.

    "create object lo_lcl.

    create object lo_container
      exporting
        repid     = sy-repid
        ratio     = '60'
        dynnr     = sy-dynnr
        side      = lo_container->dock_at_bottom
        extension = 300.

    create object lo_alv_grid
      exporting
        i_parent          = lo_container
      exceptions
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        others            = 5.

    clear:
      ls_lvc_lay,
      lt_lvc_fcat.

    assign lt_alv_filter to <lt_alv_filter>.

    "Define the field catalogue.
    lt_lvc_fcat = ygttc_cl_pcmv=>_set_alv_field_catalog( 'GENERATE' ).

    "Set the display options.
    ls_lvc_lay-cwidth_opt = 'X'.

    call method lo_alv_grid->set_table_for_first_display
      exporting
        is_layout                     = ls_lvc_lay
        i_save                        = 'X'
      changing
        it_fieldcatalog               = lt_lvc_fcat
        it_outtab                     = <lt_alv_filter>
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3.

    ls_f4-fieldname = 'FIELDNM'.
    ls_f4-register  = 'X'.
    ls_f4-getbefore = 'X'.
    append ls_f4 to lt_f4.

    ls_f4-fieldname = 'VNAM'.
    ls_f4-register  = 'X'.
    ls_f4-getbefore = 'X'.
    append ls_f4 to lt_f4.

    lo_alv_grid->register_f4_for_fields( lt_f4 ).

    "Set event handling.
    set handler lo_lcl->handle_onf4 for lo_alv_grid.
  endif.

  if p_range is not initial.

    "Move the data from the string to the range table.
    call transformation id
      source xml p_range
      result para = lt_alv_filter.

    "Refresh ALV grid display.
    call method lo_alv_grid->refresh_table_display
      exceptions
        finished = 1
        others   = 2.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'ALV grid display cannot be refreshed.'.
    endif.

  endif.

end-of-selection.

  if l_selscr_valid <> 0.

    message id 'YGTBW' type 'S' number '200' display like 'W'
               with 'Selection screen values invalid'.

    clear l_selscr_valid.
    return.
  endif.

  "Get the selections from the ALV grid.
  lo_alv_grid->check_changed_data( ).

  loop at lt_alv_filter into ls_alv_filter.
    ls_filter-group = ls_alv_filter-group.
    ls_filter-txtlg = ls_alv_filter-txtlg.
    append ls_filter to lt_filter.
  endloop.

  sort lt_filter by group.
  delete adjacent duplicates from lt_filter comparing group.

  "Set the global variables.
  l_chain      = p_mchain && '_FR'.
  l_chain_text = p_mchtxt.

  sy-tvar0 = '@9Y@'. sy-tvar1 = 'Start of generation'. sy-tvar2 = ''. new-page with-title no-heading.

  "Create the start variant for the main-chain.
  lcl=>create_trigger( exporting i_variant = |{ l_chain }| ).

  "Create the main-chain.
  lcl=>create_chain( exporting i_chain = l_chain i_chain_text = l_chain_text importing eo_chain = lo_chain_main ).

  loop at lt_filter into ls_filter.

    l_local_chain_num = sy-tabix.

    "Set the global variant variable.
    l_chain        = |{ p_mchain }_FR_{ ls_filter-group align = right width = 2 pad = '0' }|.
    l_chain_text   = ls_filter-txtlg.
    l_variant      = l_chain.
    l_variant_text = l_chain_text.

    sy-tvar0 = '@9Y@'. sy-tvar1 = l_chain(19). sy-tvar2 = l_chain+19(6). new-page with-title no-heading.

    "Create the start variant for the sub-chain.
    lcl=>create_trigger( exporting i_variant = l_variant ).

    "Create the sub-chain.
    lcl=>create_chain( exporting i_chain = l_chain i_chain_text = l_chain_text importing eo_chain = lo_chain_local ).

    "Create, add and connect the Multi-variant step.
    lcl=>add_pcmv( changing co_chain = lo_chain_local ).

    "Add and connect the load variant (LOADING / DTP_LOAD variant).
    lcl=>add_ipdtp( changing co_chain = lo_chain_local ).

    "Add and connect the first optional sub-chain.
    lcl=>add_chain1( changing co_chain = lo_chain_local ).

    "Add and connect the second optional sub-chain.
    lcl=>add_chain2( changing co_chain = lo_chain_local ).

*/ Save and actiate the local chain.

    if p_test <> 'X'.

      "Name the local chain.
      call method lo_chain_local->rename
        exporting
          i_txtlg = l_chain_text.

      "Set the application component.
      if p_applnm is not initial.

        call method lo_chain_main->set_application
          exporting
            i_applnm        = p_applnm
          exceptions
            aborted_by_user = 1
            others          = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to set application' p_applnm.
        endif.
      endif.

      "Save the local chain.
      call method lo_chain_local->save
        exporting
          i_objvers    = 'M'
          i_activation = ' '
        exceptions
          failed       = 1
          others       = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to save chain' l_chain.
      endif.

      "Activate the local chain.
      call method lo_chain_local->activate
        exporting
          i_noplan      = 'X'
          i_nosave      = ' '
          i_periodic    = ' '
          i_gui         = 'N'
        importing
          e_t_conflicts = lt_conflicts
        exceptions
          errors        = 1
          warnings      = 2
          others        = 3.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to activate chain' l_chain.
      else.
        msg=>add( |<I>CHAIN      { l_chain width = 30 pad = space } saved and activated.| ).
      endif.
    else.
      msg=>add( |<I>CHAIN      { l_chain width = 30 pad = space } saved and activated.| ).
    endif.

    "Add the chain to the list of chains for transport.
    append l_chain to lt_chain.

    if p_test <> 'X'.

      "Add the step.
      call method lo_chain_main->add_process
        exporting
          i_type                 = 'CHAIN'
          i_variant              = |{ l_chain }|
          i_dont_connect_default = ' '
          i_dont_add_default     = ' '
          i_with_dialog          = ' '
        exceptions
          aborted_by_user        = 1
          others                 = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to add variant' ls_variant-type '/' ls_variant-variante.
      endif.

      if l_local_chain_num = 1.

        ls_start-chain_id = p_mchain && '_FR'.
        ls_start-type     = 'TRIGGER'.
        ls_start-variante = p_mchain && '_FR'.

        l_color = ' '.
      else.

        ls_start-chain_id = p_mchain && '_FR'.
        ls_start-type     = 'CHAIN'.
        ls_start-variante = l_chain_last.

        l_color = 'G'.
      endif.

      ls_end-chain_id   = p_mchain && '_FR'.
      ls_end-type       = 'CHAIN'.
      ls_end-variante   = l_chain.

      "Connect the step.
      call method lo_chain_main->connect
        exporting
          i_s_start    = ls_start
          i_s_end      = ls_end
          i_color      = l_color
        exceptions
          not_possible = 1
          others       = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to connect' ls_start-variante 'to'  ls_end-variante.
      else.
        msg=>add( |<I>CHAIN      { l_chain width = 30 pad = space } added and connected to chain { p_mchain }_FR.| ).
      endif.

      l_chain_last = l_chain.
    else.
      msg=>add( |<I>CHAIN      { l_chain width = 30 pad = space  } added and connected to chain { p_mchain }_FR.| ).
    endif.
  endloop.

  "Add the chain to the list of chains for transport.
  l_chain = p_mchain && '_FR'.
  append l_chain to lt_chain.

*/ Save and actiate the main chain.

  if p_test <> 'X'.

    "Set the chain description.
    call method lo_chain_main->rename
      exporting
        i_txtlg = |{ p_mchtxt }|.

    "Set the application component.
    if p_applnm is not initial.

      call method lo_chain_main->set_application
        exporting
          i_applnm        = p_applnm
        exceptions
          aborted_by_user = 1
          others          = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to set application' p_applnm.
      endif.
    endif.

    "Save the local sub-chain.
    call method lo_chain_main->save
      exporting
        i_objvers    = 'M'
        i_activation = ' '
      exceptions
        failed       = 1
        others       = 2.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to save chain' p_mchain.
    endif.

    "Activate the local sub-chain.
    call method lo_chain_main->activate
      exporting
        i_noplan      = 'X'
        i_nosave      = ' '
        i_periodic    = ' '
        i_gui         = 'N'
      importing
        e_t_conflicts = lt_conflicts
      exceptions
        errors        = 1
        warnings      = 2
        others        = 3.
    if sy-subrc <> 0.
      message id 'YGTBW' type 'E' number '200'
                 with 'Unable to activate chain' p_mchain.
    else.
      msg=>add( |<I>CHAIN      { p_mchain && '_FR' width = 30 pad = space } saved and activated.| ).
    endif.
  else.
    msg=>add( |<I>CHAIN      { p_mchain && '_FR' width = 30 pad = space } saved and activated.| ).
  endif.

  "Add the chain to the list of chains for transport.
  l_chain = p_mchain && '_FR'.
  append l_chain to lt_chain.

  if ( sy-sysid = 'RD8' or sy-sysid = 'G13' ) and p_trkorr is not initial.

    sy-tvar0 = '@K4@'. sy-tvar1 = 'Collection for trans'. sy-tvar2 = 'port'. new-page with-title no-heading.

    loop at lt_chain into l_chain.

      l_object = l_chain.

      if p_test <> 'X'.

        call function 'RS_CORR_INSERT'
          exporting
            object                   = l_object
            object_class             = 'RSPC'
            mode                     = 'INSERT'
            global_lock              = 'X'
            devclass                 = p_devcls
            korrnum                  = p_trkorr
            master_language          = 'E'
            object_class_supports_ma = 'X'
          importing
            korrnum                  = l_trkorr
          exceptions
            cancelled                = 1
            permission_failure       = 2
            unknown_objectclass      = 3
            others                   = 4.
        if sy-subrc <> 0.
          msg=>add( |<W>Unable to include { l_object } in transport { p_trkorr }| ).
        else.

          if l_trkorr <> p_trkorr.
            msg=>add( |<W>{ l_object } locked in transport { l_trkorr }| ).
          else.
            msg=>add( |<I>{ l_object } added to transport { l_trkorr }| ).
          endif.
        endif.
      else.
        msg=>add( |<I>{ l_object width = 40 pad = space } added to transport { p_trkorr }| ).
      endif.

      select *
        into table lt_rspcchain
        from rspcchain
        where chain_id = l_chain and
              objvers = 'A'.
      if sy-subrc = 0.

        loop at lt_rspcchain into ls_rspcchain.

          case ls_rspcchain-type.
            when 'TRIGGER'.
              l_object_class = 'RSPT'.
              l_object = ls_rspcchain-variante.
            when 'CHAIN'.
              l_object_class = 'RSPC'.
              l_object = ls_rspcchain-variante.
            when 'LOADING'.
              l_object_class = 'ISIP'.
              l_object = ls_rspcchain-variante.
            when 'DTP_LOAD'.
              l_object_class = 'DTPA'.
              l_object = ls_rspcchain-variante.
            when others.
              l_object_class = 'RSPV'.
              l_object = |{ ls_rspcchain-type width = 10 align = left pad = space }{ ls_rspcchain-variante }|.
          endcase.

          if p_test <> 'X'.

            call function 'RS_CORR_INSERT'
              exporting
                object                   = l_object
                object_class             = l_object_class
                mode                     = 'INSERT'
                global_lock              = 'X'
                devclass                 = p_devcls
                korrnum                  = p_trkorr
                master_language          = 'E'
                object_class_supports_ma = 'X'
              importing
                korrnum                  = l_trkorr
              exceptions
                cancelled                = 1
                permission_failure       = 2
                unknown_objectclass      = 3
                others                   = 4.
            if sy-subrc <> 0.
              msg=>add( |<W>Unable to include { l_object } in transport { p_trkorr }| ).
            else.

              if l_trkorr <> p_trkorr.
                msg=>add( |<W>{ l_object } locked in transport { l_trkorr }| ).
              else.
                msg=>add( |<I>{ l_object } added to transport { l_trkorr }| ).
              endif.
            endif.
          else.
            msg=>add( |<I>{ l_object width = 40 pad = space } added to transport { p_trkorr }| ).
          endif.

        endloop.
      endif.

    endloop.
  endif.
*----------------------------------------------------------------------*
*       CLASS msg IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class msg implementation.

  method add.

    data:
      ls_msg                 type rs_s_msg,
      lt_msg                 type rs_t_msg,
      l_msgv                 type c length 200.

    data:
      l_msg_text             type gsugi_pa-rcmessage.

    data:
      l_class                 type smesg-arbgb,
      l_type                 type smesg-msgty,
      l_number               type smesg-txtnr.

    if msg is not initial.

      "Example '<W>Hello world Welcome world'
      if strlen( msg ) >= 3 and msg(1) = '<' and msg+2(1) = '>'.

        ls_msg-msgid = g_msgid_default.
        ls_msg-msgno = g_msgno_default.

        ls_msg-msgty = msg+1(1).
        l_msgv = msg+3.

        ls_msg-msgv1 = l_msgv(50).
        ls_msg-msgv2 = l_msgv+50(50).
        ls_msg-msgv3 = l_msgv+100(50).
        ls_msg-msgv4 = l_msgv+150(50).
        append ls_msg to lt_msg.

        "Example '<W200>Hello world Welcome world'
      elseif strlen( msg ) >= 6 and msg(1) = '<' and msg+5(1) = '>'.

        ls_msg-msgid = g_msgid_default.
        ls_msg-msgno = msg+2(3).

        ls_msg-msgty = msg+1(1).
        l_msgv = msg+6.

        split l_msgv at space into ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
        append ls_msg to lt_msg.
      else.

        ls_msg-msgid = g_msgid_default.
        ls_msg-msgno = g_msgno_default.

        ls_msg-msgty = 'S'.
        l_msgv = msg.

        ls_msg-msgv1 = l_msgv(50).
        ls_msg-msgv2 = l_msgv+50(50).
        ls_msg-msgv3 = l_msgv+100(50).
        ls_msg-msgv4 = l_msgv+150(50).
        append ls_msg to lt_msg.
      endif.
    endif.

    if s_msg is not initial.
      append s_msg to lt_msg.
    endif.

    if t_msg is not initial.
      append lines of t_msg to lt_msg.
    endif.

    if g_write = 'X'.

      case ls_msg-msgty.
        when 'I'. l_msgv = `@08@ ` && l_msgv.
        when 'W'. l_msgv = `@09@ ` && l_msgv.
        when 'E'. l_msgv = `@0A@ ` && l_msgv.
      endcase.

      write: / l_msgv.

      return.
    endif.

    loop at lt_msg into ls_msg.

      if g_message_flag = 'X'.

        message id ls_msg-msgid type 'S' number ls_msg-msgno display like ls_msg-msgty
          with ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4.
      endif.

      if g_spool_flag = 'X'.

        call function 'SPDA_CONVERT_MESSAGE_TO_TEXT'
          exporting
            msg_class  = ls_msg-msgid
            msg_number = ls_msg-msgno
            msg_var1   = ls_msg-msgv1
            msg_var2   = ls_msg-msgv2
            msg_var3   = ls_msg-msgv3
            msg_var4   = ls_msg-msgv4
          importing
            msg_text   = l_msg_text.

        case ls_msg-msgty.
          when 'I'. l_msg_text = `@08@ ` && l_msg_text.
          when 'W'. l_msg_text = `@09@ ` && l_msg_text.
          when 'E'. l_msg_text = `@0A@ ` && l_msg_text.
        endcase.

        write: / l_msg_text.
      endif.
    endloop.

    rt_msg = lt_msg.
  endmethod.                    "add
endclass.                    "msg IMPLEMENTATION
*&---------------------------------------------------------------------*
*&       Class (Implementation)  lcl
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl implementation.

  method create_trigger.

    data:
      l_variant_text         type rstxtlg.


    call method cl_rspc_trigger=>if_rspc_get_variant~exists
      exporting
        i_variant = i_variant
        i_objvers = 'A'
      receiving
        r_exists  = l_exists.

    if l_exists = 'X'.
      l_modify = 'X'.
    else.
      l_modify = ' '.
    endif.

    "Add the scheduling parameters for the start variant.
    l_variant_text = |Start: { i_variant };|.

    if p_test <> 'X'.

      call function 'RSPC_TRIGGER_GENERATE'
        exporting
          i_variant      = i_variant
          i_variant_text = l_variant_text
          i_meta         = rs_c_true
          i_no_transport = rs_c_true
          i_modify       = l_modify
        exceptions
          exists         = 1
          failed         = 2
          others         = 3.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to configure trigger variant' l_variant.
      else.
        msg=>add( |<I>TRIGGER    { i_variant width = 30 pad = space } created/modified.| ).
      endif.
    else.
      msg=>add( |<I>TRIGGER    { i_variant width = 30 pad = space } created/modified.| ).
    endif.
  endmethod.                    "create_trigger

  method create_chain.

    call method cl_rspc_chain=>if_rspc_get_variant~exists
      exporting
        i_variant = |{ i_chain }|
        i_objvers = 'M'
      receiving
        r_exists  = l_exists.

    if l_exists = 'X'.

      if p_test <> 'X'.

        create object eo_chain
          exporting
            i_chain         = i_chain
            i_variant       = |{ i_chain }|
            i_no_transport  = rs_c_true
            i_with_dialog   = rs_c_false
          exceptions
            aborted_by_user = 1
            not_unique      = 2
            wrong_name      = 3
            display_only    = 4
            others          = 5.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to create chain' l_chain.
        endif.

        call method eo_chain->delete
          exceptions
            failed = 1
            others = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to delete chain' l_chain.
        else.
          msg=>add( |<W>CHAIN      { i_chain width = 30 pad = space } chain deleted.| ).
        endif.
      else.
        msg=>add( |<W>CHAIN      { i_chain width = 30 pad = space } chain deleted.| ).
      endif.

      l_new = 'X'.
    else.
      l_new = 'X'.
    endif.

    if p_test <> 'X'.

      create object eo_chain
        exporting
          i_chain         = i_chain
          i_variant       = |{ i_chain }|
          i_no_transport  = rs_c_true
          i_with_dialog   = rs_c_false
          i_new           = l_new
        exceptions
          aborted_by_user = 1
          not_unique      = 2
          wrong_name      = 3
          display_only    = 4
          others          = 5.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to create chain' l_chain.
      endif.
    else.
      msg=>add( |<I>CHAIN      { i_chain width = 30 pad = space } created.| ).
    endif.
  endmethod.                    "create_chain

  method add_pcmv.

    call method cl_rspc_variant=>exists
      exporting
        i_type    = 'YGTTC_PCMV'
        i_variant = l_variant
        i_objvers = 'A'
      receiving
        r_exists  = l_exists.

    if l_exists = 'X'.

      if p_test <> 'X'.

        "Create an instance of the Process chain Multi-variant step.
        call method cl_rspc_variant=>create
          exporting
            i_type         = 'YGTTC_PCMV'
            i_variant      = l_variant
            i_objvers      = 'A'
            i_no_transport = rs_c_true
          receiving
            r_r_variant    = lo_variant
          exceptions
            locked         = 1
            others         = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to create PCMV variant' l_chain.
        else.
          msg=>add( |<I>YGTTC_PCMV { l_variant width = 30 pad = space } variant created.| ).
        endif.

        "Delete the instance.
        call method lo_variant->delete
          exceptions
            failed = 1
            others = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to delete PCMV variant' l_chain.
        endif.
      else.
        msg=>add( |<W>YGTTC_PCMV { l_variant width = 30 pad = space } variant deleted.| ).
      endif.
    endif.

    if p_test <> 'X'.

      "Create an instance of the Process chain Multi-variant step.
      call method cl_rspc_variant=>create
        exporting
          i_type         = 'YGTTC_PCMV'
          i_variant      = l_variant
          i_objvers      = 'A'
          i_no_transport = rs_c_true
        receiving
          r_r_variant    = lo_variant
        exceptions
          locked         = 1
          others         = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to create PCMV variant' l_chain.
      endif.

      clear lt_rspcvariant.
      ls_rspcvariant-type = 'YGTTC_PCMV'.
      ls_rspcvariant-variante = l_chain.
      ls_rspcvariant-objvers = 'A'.

      ls_rspcvariant-lnr = 1.
      ls_rspcvariant-fnam = 'TASK'.
      ls_rspcvariant-sign = 'I'.
      ls_rspcvariant-opt = 'EQ'.
      ls_rspcvariant-low = 'FILTER'.
      ls_rspcvariant-high = ''.
      append ls_rspcvariant to lt_rspcvariant.

      ls_rspcvariant-lnr = 2.
      ls_rspcvariant-fnam = 'STOP'.
      ls_rspcvariant-sign = 'I'.
      ls_rspcvariant-opt = 'EQ'.
      ls_rspcvariant-low = ''.
      ls_rspcvariant-high = ''.
      append ls_rspcvariant to lt_rspcvariant.

      "Set each of the characteristic filters.
      loop at lt_alv_filter into ls_alv_filter where group = ls_filter-group.

        add 1 to ls_rspcvariant-lnr.

        ls_rspcvariant-fnam = 'RANGE_' && ls_alv_filter-fieldnm.

        if ls_alv_filter-vnam is not initial.
          ls_rspcvariant-sign = ''.
          ls_rspcvariant-opt  = 'VN'.
          ls_rspcvariant-low  = ls_alv_filter-vnam.
          ls_rspcvariant-high = ''.
        else.

          ls_rspcvariant-sign = ls_alv_filter-sign.
          ls_rspcvariant-opt  = ls_alv_filter-option.
          ls_rspcvariant-low  = ls_alv_filter-low.
          ls_rspcvariant-high = ls_alv_filter-high.
        endif.

        append ls_rspcvariant to lt_rspcvariant.
      endloop.

      "Set the long text.
      ls_rspcvariantt-langu = sy-langu.
      ls_rspcvariantt-type = 'YGTTC_PCMV'.
      ls_rspcvariantt-variante = l_chain.
      ls_rspcvariantt-objvers = 'A'.
      ls_rspcvariantt-txtlg = ls_filter-txtlg.

      "Save the Process chain Multi-variant step.
      call method lo_variant->save
        exporting
          i_t_rspcvariant  = lt_rspcvariant
          i_s_rspcvariantt = ls_rspcvariantt
        exceptions
          failed           = 1
          others           = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to save PCMV variant' l_chain.
      else.
        msg=>add( |<I>YGTTC_PCMV { l_variant width = 30 pad = space } created & saved.| ).
      endif.
    else.
      msg=>add( |<I>YGTTC_PCMV { l_variant width = 30 pad = space } created & saved.| ).
    endif.

    if p_test <> 'X'.

      "Connect the Process chain Multi-variant step.
      ls_variant-type = 'YGTTC_PCMV'.
      ls_variant-variante = l_chain.
      append ls_variant to lt_variant.

      "Add the step.
      call method co_chain->add_process
        exporting
          i_type                 = ls_variant-type
          i_variant              = ls_variant-variante
          i_dont_connect_default = 'X'
          i_dont_add_default     = 'X'
          i_with_dialog          = 'X'
        exceptions
          aborted_by_user        = 1
          others                 = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to add variant' ls_variant-type '/' ls_variant-variante.
      endif.

      ls_start-chain_id = l_chain.
      ls_start-type     = 'TRIGGER'.
      ls_start-variante = l_chain.

      ls_end-chain_id   = l_chain.
      ls_end-type       = 'YGTTC_PCMV'.
      ls_end-variante   = l_chain.

      "Connect the step.
      call method co_chain->connect
        exporting
          i_s_start    = ls_start
          i_s_end      = ls_end
        exceptions
          not_possible = 1
          others       = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to connect' ls_start-variante 'to'  ls_end-variante.
      else.
        msg=>add( |<I>YGTTC_PCMV { l_variant width = 30 pad = space } added and connected to chain { l_chain }.| ).
      endif.
    else.
      msg=>add( |<I>YGTTC_PCMV { l_variant width = 30 pad = space } added and connected to chain { l_chain }.| ).
    endif.
  endmethod.                    "add_pcmv

  method add_ipdtp.

    ls_variant-type = l_load_typ.
    ls_variant-variante = p_ipdtp.
    append ls_variant to lt_variant.

    if p_test <> 'X'.

      "Add the step.
      call method co_chain->add_process
        exporting
          i_type                 = ls_variant-type
          i_variant              = ls_variant-variante
          i_dont_connect_default = 'X'
          i_dont_add_default     = 'X'
          i_with_dialog          = 'X'
        exceptions
          aborted_by_user        = 1
          others                 = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to add variant' ls_variant-type '/' ls_variant-variante.
      endif.

      ls_start-chain_id = l_chain.
      ls_start-type     = 'YGTTC_PCMV'.
      ls_start-variante = l_chain.

      ls_end-chain_id   = l_chain.
      ls_end-type       = l_load_typ.
      ls_end-variante   = p_ipdtp.

      "Connect the step.
      call method co_chain->connect
        exporting
          i_s_start    = ls_start
          i_s_end      = ls_end
          i_color      = 'G'
        exceptions
          not_possible = 1
          others       = 2.
      if sy-subrc <> 0.
        message id 'YGTBW' type 'E' number '200'
                   with 'Unable to connect' ls_start-variante 'to'  ls_end-variante.
      else.
        msg=>add( |<I>{ ls_variant-type width = 10 pad = space } { l_variant width = 30 pad = space } added and connected to chain { l_chain }.| ).
      endif.
    else.
      msg=>add( |<I>{ ls_variant-type width = 10 pad = space } { l_variant width = 30 pad = space } added and connected to chain { l_chain }.| ).
    endif.
  endmethod.                    "add_ipdtp

  method add_chain1.

    if p_chain1 is not initial.

      ls_variant-type = 'CHAIN'.
      ls_variant-variante = p_chain1.
      append ls_variant to lt_variant.

      if p_test <> 'X'.

        "Add the step.
        call method co_chain->add_process
          exporting
            i_type                 = ls_variant-type
            i_variant              = ls_variant-variante
            i_dont_connect_default = 'X'
            i_dont_add_default     = 'X'
            i_with_dialog          = 'X'
          exceptions
            aborted_by_user        = 1
            others                 = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to add variant' ls_variant-type '/' ls_variant-variante.
        endif.

        ls_start-chain_id = l_chain.
        ls_start-type     = l_load_typ.
        ls_start-variante = p_ipdtp.

        ls_end-chain_id   = l_chain.
        ls_end-type       = 'CHAIN'.
        ls_end-variante   = p_chain1.

        "Connect the step.
        call method co_chain->connect
          exporting
            i_s_start    = ls_start
            i_s_end      = ls_end
            i_color      = 'G'
          exceptions
            not_possible = 1
            others       = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to connect' ls_start-variante 'to'  ls_end-variante.
        else.
          msg=>add( |<I>CHAIN      { p_chain1 width = 30 pad = space } added and connected to chain { l_chain }.| ).
        endif.
      else.
        msg=>add( |<I>CHAIN      { p_chain1 width = 30 pad = space } added and connected to chain { l_chain }.| ).
      endif.
    endif.
  endmethod.                    "add_chain1

  method add_chain2.

    if p_chain2 is not initial.

      ls_variant-type = 'CHAIN'.
      ls_variant-variante = p_chain2.
      append ls_variant to lt_variant.

      if p_test <> 'X'.
        "Add the step.
        call method co_chain->add_process
          exporting
            i_type                 = ls_variant-type
            i_variant              = ls_variant-variante
            i_dont_connect_default = 'X'
            i_dont_add_default     = 'X'
            i_with_dialog          = 'X'
          exceptions
            aborted_by_user        = 1
            others                 = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to add variant' ls_variant-type '/' ls_variant-variante.
        endif.

        if p_chain1 is initial.

          ls_start-chain_id = l_chain.
          ls_start-type     = l_load_typ.
          ls_start-variante = p_ipdtp.
        else.
          ls_start-chain_id = l_chain.
          ls_start-type     = 'CHAIN'.
          ls_start-variante = p_chain1.
        endif.

        ls_end-chain_id   = l_chain.
        ls_end-type       = 'CHAIN'.
        ls_end-variante   = p_chain2.

        "Connect the step.
        call method co_chain->connect
          exporting
            i_s_start    = ls_start
            i_s_end      = ls_end
            i_color      = 'G'
          exceptions
            not_possible = 1
            others       = 2.
        if sy-subrc <> 0.
          message id 'YGTBW' type 'E' number '200'
                     with 'Unable to connect' ls_start-variante 'to'  ls_end-variante.
        else.
          msg=>add( |<I>CHAIN      { p_chain2 width = 30 pad = space } added and connected to chain { l_chain }.| ).
        endif.
      else.
        msg=>add( |<I>CHAIN      { p_chain2 width = 30 pad = space } added and connected to chain { l_chain }.| ).
      endif.
    endif.
  endmethod.                    "add_chain2
  method handle_onf4.

    data:
      ls_details               type bapi6109,
      ls_scheduling_params     type bapi6109btch,
      ls_destinations          type bapi6109dest,
      ls_file_params           type bapi6109file,
      ls_hie_params            type bapi6109hie.

    data:
      ls_selections            type bapi6109sel,
      lt_selections            type table of bapi6109sel,
      lt_infocubes             type table of bapi6109ic,
      lt_third_party_params    type table of bapi6109tcp,
      lt_return                 type table of bapiret2.

    data:
      lo_rsbk_dtp              type ref to cl_rsbk_dtp,
      lo_rsbc_filter           type ref to cl_rsbc_filter.

    data:
      ls_varseltab             type mch_var_select,
      lt_varseltab_before      type mch_t_var_select,
      lt_varseltab_after       type mch_t_var_select,
      ls_seltab                type rsbk_s_select,
      lt_seltab_before         type mch_t_select,
      lt_seltab_after          type mch_t_select,
      ls_dtprule               type mch_s_sourcecode,
      lt_dtprule               type mch_t_sourcecode,
      ls_selfields             type mch_s_field,
      lt_selfields             type mch_t_field.


    types:
      begin of s_var,
        name                 type rszvnam,
        txtlg                type rstxtlg,
      end of s_var.

    data:
      ls_var                 type s_var,
      lt_var                 type table of s_var.

    types:
      s_c200                 type c length 200.

    data:
      ls_value_tab           type s_c200,
      lt_value_tab           type table of s_c200,
      ls_field_tab           type dfies,
      lt_field_tab           type table of dfies,
      ls_return_tab          type ddshretval,
      lt_return_tab          type table of ddshretval.

    data:
      l_infopackage          type rslogdpid,
      l_dtp                  type rsbkdtpnm.


    case e_fieldname.
      when 'FIELDNM'.

        if p_ipdtp is not initial.

          select single logdpid
            into l_infopackage
            from rsldpio
            where logdpid = p_ipdtp.
          if sy-subrc = 0.

            l_infopackage = p_ipdtp.

            call function 'BAPI_IPAK_GETDETAIL'
              exporting
                infopackage          = l_infopackage
                with_empty_selfields = 'X'
              importing
                details              = ls_details
                scheduling_params    = ls_scheduling_params
                destinations         = ls_destinations
                file_params          = ls_file_params
                hie_params           = ls_hie_params
              tables
                selections           = lt_selections
                infocubes            = lt_infocubes
                third_party_params   = lt_third_party_params
                return               = lt_return.

            loop at lt_selections into ls_selections.
              ls_value_tab = ls_selections-fieldname.
              append ls_value_tab to lt_value_tab.
            endloop.

            ls_field_tab-tabname = 'RSLDPSEL'.
            ls_field_tab-fieldname = 'FIELDNAME'.
            append ls_field_tab to lt_field_tab.

            call function 'F4IF_INT_TABLE_VALUE_REQUEST'
              exporting
                retfield        = 'FIELDNAME'
                display         = 'X'
              tables
                value_tab       = lt_value_tab
                field_tab       = lt_field_tab
                return_tab      = lt_return_tab
              exceptions
                parameter_error = 1
                no_values_found = 2
                others          = 3.
          else.

            select single dtp
              into l_dtp
              from rsbkdtp
              where dtp = p_ipdtp.
            if sy-subrc = 0.

              "Create an instance of the DTP.
              lo_rsbk_dtp = cl_rsbk_dtp=>factory( |{ l_dtp }| ).

              "Get reference to DTP’s filter object.
              lo_rsbc_filter = lo_rsbk_dtp->get_obj_ref_filter( ).

              "Get the filter definition.
              call method lo_rsbc_filter->get_all
                importing
                  e_t_varseltab = lt_varseltab_before
                  e_t_seltab    = lt_seltab_before
                  e_t_dtprule   = lt_dtprule
                  e_t_selfields = lt_selfields.

              loop at lt_selfields into ls_selfields.
                ls_value_tab = ls_selfields-field.
                append ls_value_tab to lt_value_tab.
              endloop.

              ls_field_tab-tabname = 'RSBKDTP'.
              ls_field_tab-fieldname = 'DTP'.
              append ls_field_tab to lt_field_tab.

              call function 'F4IF_INT_TABLE_VALUE_REQUEST'
                exporting
                  retfield        = 'DTP'
                  display         = 'X'
                tables
                  value_tab       = lt_value_tab
                  field_tab       = lt_field_tab
                  return_tab      = lt_return_tab
                exceptions
                  parameter_error = 1
                  no_values_found = 2
                  others          = 3.
            endif.
          endif.
        else.
          message 'Provide a valid InfoPackage or DTP.' type 'S'.
        endif.
      when 'VNAM'.

        select vnam as name
               txtlg as txtlg
          into corresponding fields of table lt_var
          from rszglobv as a
          join rszelttxt as b
            on a~varuniid = b~eltuid and
               a~objvers = b~objvers
          where vnam like '%PCMV%' and
               a~objvers = 'A' and
               langu = sy-langu.
        if sy-subrc = 0.

          loop at lt_var into ls_var.

            ls_value_tab = ls_var-name.
            append ls_value_tab to lt_value_tab.

            ls_value_tab = ls_var-txtlg.
            append ls_value_tab to lt_value_tab.
          endloop.

          ls_field_tab-tabname = 'RSZGLOBV'.
          ls_field_tab-fieldname = 'VNAM'.
          append ls_field_tab to lt_field_tab.

          ls_field_tab-tabname = 'RSZELTTXT'.
          ls_field_tab-fieldname = 'TXTLG'.
          append ls_field_tab to lt_field_tab.

          call function 'F4IF_INT_TABLE_VALUE_REQUEST'
            exporting
              retfield        = 'VNAM'
              display         = 'X'
            tables
              value_tab       = lt_value_tab
              field_tab       = lt_field_tab
              return_tab      = lt_return_tab
            exceptions
              parameter_error = 1
              no_values_found = 2
              others          = 3.
        endif.
    endcase.
  endmethod.                    "handle_onf4
endclass.               "lcl
