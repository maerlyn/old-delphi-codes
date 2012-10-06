program NewsHtml;

uses
  Forms,
  NewsHtm1 in 'NewsHtm1.pas' {HtmlNewsForm},
  MimeUtil in '..\VC32\MimeUtil.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THtmlNewsForm, HtmlNewsForm);
  Application.Run;
end.
