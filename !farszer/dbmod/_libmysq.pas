unit _libmysq;

{This is a more complete unit to interface with MySQL Server from Delphi
This work was started by by Blestan Tabakov. I have included the rest of the
functionality in libmysql.dll.
Delphi 3 Interface to LibMYSQL.dll
June 1998
Bob Silva   bsilva@umesd.k12.or.us

To find out more on this get the C API docs from www.tcx.se (makers of MySQL)}

{TODO: Implement as a VCL I dont think I have the knowledge to make it use Delphis
data controls but Ill look into it, I dont think the component will be too hard to
implement in the near future}


interface
const
 mysql_errmsg_size=200;
 MYSQL_PORT=3306;
 LOCAL_HOST='localhost';
 MYSQL_UNIX_ADDR='/tmp/mysql.sock';
 NAME_LEN=64;
 PROTOCOL_VERSION=10;
 MYSQL_SERVER_VERSION='3.21.18-beta';
 FRM_VER=6;
 MYSQL_VERSION_ID=32115;
 field_type_decimal=0;
 field_type_tiny=1;
 field_type_short=2;
 field_type_long=3;
 field_type_float=4;
 field_type_double=5;
 field_type_null=6;
 field_type_timestamp=7;
 field_type_longlong=8;
 field_type_int24=9;
 field_type_date=10;
 field_type_time=11;
 field_type_datetime=12;
 field_type_enum=247;
 field_type_set=248;
 field_type_tiny_blob=249;
 field_type_medium_blob=250;
 field_type_long_blob=251;
 field_type_blob=252;
 field_type_var_string=253;
 field_type_string=254;

type
 enum_field_types=byte;
 my_bool= shortint;
 gptr= pchar;
 socket= word;

 mysql_status=(mysql_status_ready,mysql_status_get_result,mysql_status_use_result);

 pused_mem=^used_mem;
 used_mem=record
            next:pused_mem;
            left,size: Cardinal;
           end;

 err_proc=procedure;

 pmem_root=^mem_root;
 mem_root=record
           free,used:pused_mem;
           min_malloc,block_size: Cardinal;
           error_handler:err_proc;
          end;

 net=record
       fd: socket;
       fcntl: integer;
       buff,buff_end,write_pos: pchar;
       last_error:array[01..mysql_errmsg_size] of char;
       last_errno,max_packet,timeout,pkt_vnr: integer;
       error,return_errno: my_bool;
      end;

pmysql_field=^mysql_field;
mysql_field=record
             name,table,def: pchar;
             _type: enum_field_types;
             length,max_length,flags,decimals: Cardinal;
            end;
mysql_field_offset= Cardinal;

(*************HELP HERE IF YOU CAN*************)
{The C def for this is: typedef byte **MYSQL_ROW;
Blestan originally used the array struct below, but I could not get it to
work like that.
Blestan Code:  mysql_row=array[00..$ffff div sizeof(pchar)] of pchar;
My def of a smaller array seems to solve the problem, but if you were to return
a very large result set, Im sure it will AV. So if you can translate the C Code:
Earlier in code: typedef char       byte;
typedef byte **MYSQL_ROW; into an adequate Delphi type then please do and let
me know how you did it. I could get it to return rows using mysql_row: ^PChar; but
I couldnt figure out how to index into it like that.}



 pmysql_row=^mysql_row;
 mysql_row= array[00..$ff] of pchar;



(*********************************************)


 pmysql_rows=^mysql_rows;
 mysql_rows=record
            next: pmysql_rows;
            data: pmysql_row;
           end;
 mysql_row_offset= pmysql_rows;

 pmysql_data= ^mysql_data;
 mysql_data= record
             rows, fields: Cardinal;
             data: pmysql_rows;
             alloc: mem_root;
             end;

pmysql=^mysql;
mysql= record
        _net: net;
        host,user,passwd,
        unix_socket,
        server_version,
        host_info,
        info,db: pchar;
        port,client_flag,server_capabilities,
        protocol_version,field_count: Cardinal;
        thread_id, affected_rows, insert_id, extra_info:longint;
        status: mysql_status;
        fields: pmysql_field;
        field_alloc: mem_root;
        free_me, reconnect: my_bool;
       end;

 pmysql_res=^mysql_res;
 mysql_res= record
           row_count: longint;
           field_count, current_field: Word;
           fields: pmysql_field;
           data: pmysql_data;
           data_cursor:pmysql_rows;
           field_alloc:mem_root;
           row: mysql_row;
           current_row: pmysql_row;
           lengths: ^Word;
           handle: ^mysql;
           eof: my_bool;
           end;

const thelib='libmysql.dll';
(*********ALL functions are working unless stated otherwise*************)
{If you can figure why why one of the ones marked bad is not working, please
send me the code to implement it properly}

function mysql_connect( _mysql: pmysql; const host,user,passwd:pchar):pmysql;stdcall;
{Non-functional: dont know why yet: I think you can do without it for now}
function mysql_real_connect(_mysql: pmysql; const host, user, passwd, db:pchar;
port:Cardinal; unix_socket:PChar;clientflag:Cardinal):pmysql;stdcall;
procedure mysql_close( _mysql: pmysql); stdcall;
function mysql_select_db(_mysql:pmysql;const DB: pchar):integer; stdcall;
function mysql_query(_mysql:pmysql;const query: pchar):integer;stdcall;
{Real Query not implemented in current version of libmysql.dll (timestamp:353be33a)}
function mysql_real_query(_mysql:pmysql;const query: pchar;length: Cardinal):integer;stdcall;
function mysql_create_db(_mysql:pmysql; const DB:PChar):integer;stdcall;
function mysql_drop_db(_mysql:pmysql; const DB:PChar):integer;stdcall;
function mysql_shutdown(_mysql: pmysql): Integer; stdcall;
{Although undocumented, I believe this is to reload the permissions table;
documentation calls for a reload function which there is none in libmysql.dll,
so I think this may be a replacement, it takes a Cardinal for options but I cant find out
what those options are since its not documented}
function mysql_refresh(_mysql:pmysql; refresh_options: Cardinal):integer;stdcall;
function mysql_kill(_mysql:pmysql; pid:longint):integer;stdcall;
function mysql_stat(_mysql:pmysql):pchar; stdcall;
function mysql_get_server_info(_mysql:pmysql):PChar;stdcall;
function mysql_get_client_info:PChar;stdcall;
function mysql_get_host_info(_mysql:pmysql):PChar;stdcall;
function mysql_get_proto_info(_mysql:pmysql):Cardinal;stdcall;
function mysql_list_dbs(_mysql:pmysql;wild: pchar):pmysql_res; stdcall;
function mysql_list_tables(_mysql: pmysql; const wild: pchar):pmysql_res; stdcall;
function mysql_list_fields(_mysql: pmysql;const table,wild: pchar):pmysql_res; stdcall;
function mysql_list_processes(_mysql: pmysql):pmysql_res; stdcall;
function mysql_store_result(_mysql:pmysql):pmysql_res; stdcall;
function mysql_use_result(_mysql:pmysql):pmysql_res;stdcall;
procedure mysql_free_result(result:pmysql_res); stdcall;
procedure mysql_data_seek(result:pmysql_res; offset:Cardinal); stdcall;
{Row seek is probably functional if I knew how to test it;
I think its a matter of saving a particular row pointer in a mysql_row_offset struct;
then later make a call to row_seek to restore the data_cursor to that row;
then a fetch_row will return the next row after that???? I have no need for it so you tell me}
function mysql_row_seek(result:pmysql_res; row:mysql_row_offset):mysql_row_offset;stdcall;
{Field Seek is not implemented in current version of libmysql.dll(timestamp:353be33a)}
function mysql_field_seek(result:pmysql_res;offset:mysql_field_offset):mysql_field_offset;stdcall
function mysql_fetch_row(result:pmysql_res):pmysql_row;stdcall;
{I dont think fetch_length is working properly because of the way that I set up the MYSQL_ROW array}
function mysql_fetch_lengths(result:pmysql_res):Cardinal;stdcall;
function mysql_fetch_field(handle: pmysql_res):pmysql_field; stdcall;

implementation
function mysql_connect( _mysql: pmysql; const host,user,passwd:pchar):pmysql;stdcall;external thelib;
function mysql_real_connect(_mysql: pmysql; const host, user, passwd, db:pchar;
port:Cardinal; unix_socket:PChar;clientflag:Cardinal):pmysql;stdcall;external thelib;
procedure mysql_close( _mysql: pmysql); stdcall;external thelib;
function mysql_select_db(_mysql:pmysql;const DB: pchar):integer; stdcall;external thelib;
function mysql_query(_mysql:pmysql;const query: pchar):integer;stdcall;external thelib;
function mysql_real_query(_mysql:pmysql;const query: pchar;length: Cardinal):integer;stdcall;external thelib;
function mysql_create_db(_mysql:pmysql; const DB:PChar):integer;stdcall;external thelib;
function mysql_drop_db(_mysql:pmysql; const DB:PChar):integer;stdcall;external thelib;
function mysql_shutdown(_mysql: pmysql): Integer; stdcall;external thelib;
function mysql_refresh(_mysql:pmysql; refresh_options: Cardinal):integer;stdcall;external thelib;
function mysql_kill(_mysql:pmysql; pid:longint):integer;stdcall;external thelib;
function mysql_stat(_mysql:pmysql):pchar; stdcall;external thelib;
function mysql_get_server_info(_mysql:pmysql):PChar;stdcall;external thelib;
function mysql_get_client_info:PChar;stdcall;external thelib;
function mysql_get_host_info(_mysql:pmysql):PChar;stdcall;external thelib;
function mysql_get_proto_info(_mysql:pmysql):Cardinal;stdcall;external thelib;
function mysql_list_dbs(_mysql:pmysql;wild: pchar):pmysql_res; stdcall;external thelib;
function mysql_list_tables(_mysql: pmysql; const wild: pchar):pmysql_res; stdcall;external thelib;
function mysql_list_fields(_mysql: pmysql;const table,wild: pchar):pmysql_res; stdcall;external thelib;
function mysql_list_processes(_mysql: pmysql):pmysql_res; stdcall;external thelib;
function mysql_store_result(_mysql:pmysql):pmysql_res; stdcall;external thelib;
function mysql_use_result(_mysql:pmysql):pmysql_res;stdcall;external thelib;
procedure mysql_free_result(result:pmysql_res); stdcall;external thelib;
procedure mysql_data_seek(result:pmysql_res; offset:Cardinal); stdcall;external thelib;
function mysql_row_seek(result:pmysql_res; row:mysql_row_offset):mysql_row_offset;stdcall;external thelib;
function mysql_field_seek(result:pmysql_res;offset:mysql_field_offset):mysql_field_offset;stdcall;external thelib;
function mysql_fetch_row(result:pmysql_res):pmysql_row;stdcall;external thelib;
function mysql_fetch_lengths(result:pmysql_res):Cardinal;stdcall;external thelib;
function mysql_fetch_field(handle: pmysql_res):pmysql_field; stdcall;external thelib;

end.
