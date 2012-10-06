unit Ftptst2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, System.ComponentModel, Borland.Vcl.StdCtrls;

type
  TDirectoryForm = class(TForm)
    DirListBox: TListBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

{$R *.nfm}

end.
