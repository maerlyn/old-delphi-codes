(******************************************************************)
(* Copyright 1997 - 1999                                          *)
(*                                                                *)
(* Translation by Dejan Maksimovic  maksa@cent.co.yu              *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

unit SFXstrings;

interface

const
     N                 = #13#10;
     STR_CANNOTOPEN    = 'Ne mogu da otvorim fajl ..';
     STR_MAINDLG       = 'GLAVNIDIALOG';
     STR_CANNOTCLOSE   = 'Ne mogu da zatvorim fajl';
     STR_E             = 'Greska..';
     STR_EXISTS        = '...vec postoji, kopirati preko ?';
     STR_SELDLG        = 'DLGBIRAJ';
     STR_NEWDLG        = 'DLGNOVI';
     BSL               = '\';
     STR_EARCHIVE      = 'Greska prilikom citanja arhive.';
     STR_INVALIDNAME   = 'Ime fajla nije validno.';
     STR_PDLG          = 'LOZINKA';
     STR_FILEEXISTS    = 'FajlPostoji';
     STR_EDIRECTORY    = 'Greska u direktorijumu ..';
     STR_PREXT         = 'Dekompresujem: ';
     STR_ETYPE         = 'Nepoznat tip kompresije';
     STR_ALLEXT        = 'Svi fajlovi su dekompresovani.';
     STR_OK            = 'Zavrseno.';
     STR_NOTSELEXT     = 'Selektovani fajlovi nisu mogli da se dekompresuju.';
     STR_APP           = 'Delfi Zip Self-Extractor';
     STR_EXPT          = 'Molim Vas, izaberite destinacioni direktorijum';
STR_RUN_PRE       = 'After extraction, >< : '; //?'Posle dekompresije, >< : ' // belongs to the 2 lines below
STR_RUN_RUN       = 'run';                     // after extraction, run : xxx.exe
STR_RUN_INST      = 'install';                 // after extraction, install : xxx.inf
     STR_BROWSECAP     = 'Dekompresuj : ';
     STR_BTNFOLD       = 'Novi...';
     STR_EINC_SIZE     = 'Neispravna velicina fajla, molim Vas, probajte ponovo.';
STR_ABOUT_CAP     = 'About '+STR_APP+'...';
     STR_ABOUT_MSG     = ' © 1997-1999 by C. Bunton, E. W. Engler, M. Stephany'+N+N+
                         'Autori : '+N+N+'- Carl Bunton (http://www.concentric.net/~twojags)'+N+
                         N+'- Eric W. Engler (http://www.geocities.com/SiliconValley/Network/2114)'+N+
                         N+'- Markus Stephany (http://home.t-online.de/home/MirBir.St)'+N+
                         N+'Pomogli su : '+N+
                         N+'- Thomas Hoelzer (thoelzer@cityweb.de)'+
                         N+'- Deepu Chandy Thomas (deepuct@hotmail.com)'+
                         N+'- Todd Fast (tfast@eden.com)'+
                         N+'- Didier Havelange (Didier.Havelange@ping.be)'+
		N+'- Dejan Maksimovic (maksa@cent.co.yu)';

implementation

end.
