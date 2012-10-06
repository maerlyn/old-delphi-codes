{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE. Based on work given by Louis S. Berman from
              BrainTree Ltd, lsb@braintree.com
Description:  MD5 is an implmentation for the MD5 Message-Digest Algorithm
              as described in RFC-1321
Creation:     October 1997
Version:      1.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
                                           francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2005 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:
Sep 12, 1998 V1.01 Beautified source
Sep 08, 2004 V1.02 Unit MD5 renamed to IcsMD5


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit MD5Test1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IcsMD5, StdCtrls;

const
  MD5TestVersion     = 102;
  CopyRight : String = ' MD5Test (c) 1997-2005 Francois Piette  V1.02 ';

type
  TMD5TestForm = class(TForm)
    DataEdit: TEdit;
    MD5Button: TButton;
    MD5ResultLabel: TLabel;
    procedure MD5ButtonClick(Sender: TObject);
  private
    { Déclarations privées }
  end;

var
  MD5TestForm: TMD5TestForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMD5TestForm.MD5ButtonClick(Sender: TObject);
var
    Buffer : String;
begin
    Buffer := DataEdit.Text + #0;
    MD5ResultLabel.Caption := GetMD5(@Buffer[1], Length(Buffer) - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
