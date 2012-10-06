{$M 65500,0,655360}
program telefonregiszter;
uses kgy1,kgy5,crt,dos;

type
    adattip=record
            nev,
            tel:string[20];
            varos:string[15];
            irsz:string[4];
            cim,
            megj:string[40]
    end;
    telhdr=record
           azon:array[1..13] of char;
           mennyi:byte;
           auto:boolean;
    end;
    indtip=array[1..100] of byte;


var
   adat:array[1..100] of adattip;    { Mindig az aktu lis lista }
   mennyi,                           { H ny szem‚lyt tartunk nyilv n }
   rend:byte;                        { Hogyan rendezett a lista:
                                       0=sehogy 1=n‚v szerint 2=v ros szerint
                                       3=ir ny¡t¢sz m szerint }
   auto,                             { Az automata rendez‚s  llapota }
   files:boolean;                   { file-t vagy adatot listazunk }
   dosszie:string[8];                { A nyitott dosszi‚ neve }

procedure loadprg;
{--------A program bet”lt‚se-------}
var
   header:telhdr;
   filein:file;
   dirtel:searchrec;
begin
     findfirst('*.tel',archive,dirtel);
     if doserror=0 then
     begin
          assign(filein,dirtel.name);
          {$I-}
          reset(filein,1);
          {$I+}
          if ioresult=0 then
          begin
               blockread(filein,header,sizeof(header));
               if header.azon='KGyTelDosszi‚' then
               begin
                    dosszie:=copy(dirtel.name,1,pos('.',dirtel.name)-1);
                    ir(67,10,dosszie);
                    mennyi:=header.mennyi;
                    gotoxy(60,12); write(mennyi);
                    auto:=header.auto;
                    if auto then rend:=1 else rend:=0;
                    if auto then ir(62,14,'Igen')
                    else ir(62,14,'Nem');
                    blockread(filein,adat,sizeof(adattip)*mennyi);
                    atalakit(1,mennyi);
               end;
               close(filein);
          end;
     end;
     if dosszie='' then
     begin
          dosszie:='TELEFON';
          mennyi:=0;
          auto:=true;
          rend:=1;
          ir(67,10,dosszie);
          ir(60,12,'0');
          ir(62,14,'Igen');
     end;
end;


procedure bovites;
{----------Adat b“v¡t‚se----------}
var
   x:byte;
   ok:boolean;
begin
                adat[mennyi].nev:=szoveg;
                adat[mennyi].tel:=szoveg;
                adat[mennyi].varos:=szoveg;
                adat[mennyi].irsz:=szoveg;
                adat[mennyi].cim:=szoveg;
                adat[mennyi].megj:=szoveg;
end;


procedure ment;
{------Dosszi‚ ment‚se az aktu lis n‚vvel-------}
var
   fejlec:telhdr;
   fileout:file;
begin
     comment('Ment‚s. . .');
     assign(fileout,dosszie+'.tel');
     {$I-}
     rewrite(fileout,1);
     {$I+}
     if ioresult<>0 then
     begin
          ablak(18,11,62,15,14,4,dupla,megjelenik,arnyekbe);
          ir(24,13,'Hiba a file elment‚se k”zben. . .');
          beep;
          varj;
          ablaktorles;
     end
     else
     begin
          fejlec.azon:='KGyTelDosszi‚';
          fejlec.mennyi:=mennyi;
          fejlec.auto:=auto;
          blockwrite(fileout,fejlec,sizeof(fejlec));
          blockwrite(fileout,adat,sizeof(adattip)*mennyi);
          valtozas(false);
          close(fileout);
     end;
end;

procedure tolt;
{------Egy file bet”lt‚se-------}
var
   regimennyi:byte;      { A mennyi ‚rt‚ke a file-ok sz ma lesz }
   sorrend:indtip;       { A file-ok ABC sorrendben }
   dirtel:searchrec;
   i,j,min:byte;
   header:telhdr;        { A dosszi‚ fejl‚ce }
   filein:file;

begin
               comment('T”lt‚s. . .');
               assign(filein,adat[sorrend[listfirst+listcurr-1]].nev+'.tel');
               {$I-}
               reset(filein,1);
               {$I+}
               if ioresult=0 then
               begin
                    blockread(filein,header,sizeof(header));
                    if header.azon='KGyTelDosszi‚' then
                    begin
                         valtozas(false);
                         dosszie:=adat[sorrend[listfirst+listcurr-1]].nev;
                         textbackground(1);
                         textcolor(14);
                         ir(67,10,'        ');
                         ir(67,10,dosszie);
                         mennyi:=header.mennyi;
                         gotoxy(60,12); write(mennyi,'  ');
                         auto:=header.auto;
                         if auto then rend:=1 else rend:=0;
                         if auto then ir(62,14,'Igen')
                         else ir(62,14,'Nem ');
                         blockread(filein,adat,sizeof(adattip)*mennyi);
                         atalakit(1,mennyi);
                    end
                    else
                    begin
                         comment('T”lt‚s - hiba');
                         ablak(18,9,62,13,14,4,dupla,megjelenik,arnyekbe);
                         ir(27,11,'Hib s form tum£ a file!');
                         beep;
                         varj;
                         ablaktorles;
                    end;
                    close(filein);
               end
               else
               {-----Nem megnyithat¢ file----}
               begin
                    comment('T”lt‚s - hiba');
                    ablak(18,9,62,13,14,4,dupla,megjelenik,arnyekbe);
                    ir(28,11,'A dosszi‚ nem megnyithat¢!');
                    beep;
                    varj;
                    ablaktorles;
                    visszair;
               end;
          end;
          ablaktorles;
     end;
end;

begin
end.
