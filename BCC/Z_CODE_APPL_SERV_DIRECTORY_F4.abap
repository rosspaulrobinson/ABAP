*&---------------------------------------------------------------------*
*& Report  Z_CODE_APPL_SERV_DIRECTORY_F4
*&
*&---------------------------------------------------------------------*
*&
*&  This demo program will output the contents of a directory on the
*&  application server. It uses the same code that is executed when
*&  using transaction AL11. It cannot be used for production since
*&  it uses 'C' calls which are not released for customer use, as
*&  such it is just experimental.
*&
*&---------------------------------------------------------------------*

report z_code_appl_serv_directory_f4.

parameters p_dir type dirname_al11 lower case default '/usr/sap/trans/Temp/DEV/DRA'.

types:
  begin of ts_file,
     dirname     type dirname_al11, " name of directory
     name        type filename_al11," name of entry
     type(10)    type c,            " type of entry.
     len(8)      type p,            " length in bytes.
     owner       type fileowner_al11, " owner of the entry.
     mtime(6)    type p,            " last mod.date, sec since 1970
     mode(9)     type c,            " like "rwx-r-x--x": prot. mode
     useable(1)  type c,
     subrc(4)    type c,
     errno(3)    type c,
     errmsg(40)  type c,
     mod_date    type d,
     mod_time(8) type c,            " hh:mm:ss
     seen(1)     type c,
     changed(1)  type c,
   end of ts_file.

data:
  lwa_file                         type ts_file,
  lt_file_list                     type standard table of ts_file with header line,
  lv_dir_name                      type dirname_al11,
  lv_generic_name                  type filename_al11,
  lv_mtext                         type string,
  lv_serverfile                    type string.

at selection-screen on value-request for p_dir.

  call function '/SAPDMC/LSM_F4_SERVER_FILE'
    exporting
      directory        = '/usr/sap/trans/Temp/'
      filemask         = '.'
    importing
      serverfile       = lv_serverfile
    exceptions
      canceled_by_user = 1
      others           = 2.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  p_dir = lv_serverfile.


start-of-selection.

  lv_dir_name = p_dir.
  lv_generic_name = '*'.

  call 'C_DIR_READ_FINISH'
      id 'ERRNO'  field lt_file_list-errno
      id 'ERRMSG' field lt_file_list-errmsg.

  call 'C_DIR_READ_START'
    id 'DIR'    field lv_dir_name
    id 'FILE'   field lv_generic_name
    id 'ERRNO'  field lwa_file-errno
    id 'ERRMSG' field lwa_file-errmsg.

  do.

    clear lwa_file.
    call 'C_DIR_READ_NEXT'
      id 'TYPE'   field lwa_file-type
      id 'NAME'   field lwa_file-name
      id 'LEN'    field lwa_file-len
      id 'OWNER'  field lwa_file-owner
      id 'MTIME'  field lwa_file-mtime
      id 'MODE'   field lwa_file-mode
      id 'ERRNO'  field lwa_file-errno
      id 'ERRMSG' field lwa_file-errmsg.
    case sy-subrc.
      when 0.
      when others.
        exit.
    endcase.

    lwa_file-dirname = lv_dir_name.
    move sy-subrc to lwa_file-subrc.

    append lwa_file to lt_file_list.

  enddo.

  loop at lt_file_list into lwa_file.

    lv_mtext = |{ lwa_file-len width = 10 }{ lwa_file-type width = 12 }{ lwa_file-name }|.

    write: / lv_mtext.

  endloop.

*Text elements
*----------------------------------------------------------
* 001 Options


*Selection texts
*----------------------------------------------------------
* P_DIR         Directory
Extracted by Direct Download Enterprise version 1.3.1 - E.G.Mellodew. 1998-2005 UK. Sap Release 731
