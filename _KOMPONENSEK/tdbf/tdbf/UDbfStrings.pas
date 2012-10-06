unit UDbfStrings;

interface
var
  STRING_FILE_NOT_FOUND:string;
  STRING_VERSION:string;
  STRING_ABOUT:string;
  STRING_FILTER:string;
  STRING_FIELD_TOO_LONG:string;
  STRING_INDEX_BASED_ON_UNKNOWN_FIELD:string;
  STRING_CANNOT_OPEN_INDEX:string;

implementation

initialization
  STRING_FILE_NOT_FOUND               :='Open: Table file not found : %s';
  STRING_VERSION                      :='TDbf V%d.%3.3d';
  STRING_ABOUT                        :='This program use ' + STRING_VERSION
    + ' to access its data.'#13#13'For more info, please visit the website : '
    + 'http://www.tdbf.net';
  STRING_FILTER:='Dbf table|*.dbf';
  STRING_FIELD_TOO_LONG               :='Value is too long : %d characters (it can''t be more than %d).';
  STRING_INDEX_BASED_ON_UNKNOWN_FIELD :='Index based on unknown field "%s"';
  STRING_CANNOT_OPEN_INDEX            :='Cannot open index : "%s"';
end.
