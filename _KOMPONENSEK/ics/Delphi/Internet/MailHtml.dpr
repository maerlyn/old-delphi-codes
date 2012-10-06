program MailHtml;

uses
  Forms,
  MailHtm1 in 'MailHtm1.pas' {HtmlMailForm};

{$R *.res}

begin
  {$IFNDEF VER80}Application.Initialize;{$ENDIF}
  Application.CreateForm(THtmlMailForm, HtmlMailForm);
  Application.Run;
end.

