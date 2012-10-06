This short program shows how easy it is to add password access to
a delphi app using ExeMod. The program will not allow access until
the user has created a pw. Once a pw has been created then the exe
cannot be run without the user entering the pw. The pw is stored at
the end of the exe... here is what is added to the exe if the user
selects 'qwerty' as her pw.  SO!#MYPWRD¶qwertyEO!#MYPWRD  of course
there is very little security here but you could encrypt the pw that
is added to the exe and prevent a user from simply looking at the end
of the exe to copy the cleartext pw.