Delphi Interface to MySQL Server


Written on July 12 1998 by
Bob Silva  :  bsilva@umesd.k12.or.us


Most of the functions have been implemented, only a few dont work
or arent in the libmysql.dll which this calls into.

I have included two very rough samples to show you the syntax
for connecting to the server and performing a simple query.
I hope this helps you.

Delphi + MySQL Server = VERY FAST DATA ACCESS.

If you have any questions or wish to contribute to this ( I am
by no means an experienced programmer) work please do. I have
not been able to figure out how to convert one C structure properly
so if you can please!!!! send me workable code.


typedef char byte;
typedef byte **MYSQL_ROW;

The problem has to do with being able to index into it from Delphi.


Hopefully I can turn this into a VCL which can use Delphis native Data Controls.
Cant wait and think you can do it? Please do.

Enjoy,
Bob Silva