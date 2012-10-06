DeathTruckTion v1.1 release
---------------------------


Introduction
------------

DeathTruckTion is a network multiplayer 3D game written
as a french school project (at EPITA - www.epita.fr) by:
	- Matthieu Avrillon <Spiritus>
	- Linda Chan Sun <VideoGirlAi>
	- Régis Lanson <Actarus>
	- Clem Vasseur <NitroGen>
Current maintainer is Dominique Louis ( Dominique@SavageSoftware.com.au )


Instructions
------------

There is very few error checking in the game, so please
follow these instructions step-by-step:
	- launch DTTD, the dedicated server
	- make sure that data.pak is in the same directory
	as the game executable (DTT.exe or DTT)
	- launch DTT
	- in the Options menu, make sur the IP of the
	server is correct, and change your name
	- you can choose between 2 maps
	- play the game
You'll join the arena... other players can do the same
to join the arena, with different trucks
	- the arrow keys move your truck
	- the TAB key fire a missile
	- the key to show the console is the same as
	in Quake ;)
	- type "quit" in the console to leave the arena
	- other recognized commands are: "info", "opengl"
	"stat" and "bsphere on/off"

NOTE : For the game to run you must have either fmod.dll ( under Windows ) or
libfmod-3.5.so ( under Linux ) installed.

Under Windows just copy the fmod.dll file
found in the Runtimes/Win32/ directory to your Windows System or System32 directory

Under Linux, make sure you are signed in as root and copy the libfmod-3.5.so
file from the Runtimes/Linux/ directory to your /usr/lib/ directory.
Then via a command line, navigate to the /usr/lib/ directory and create a
symbolic link between libfmod-3.5.so and libfmod.so. This is done with the
following command ( it assumed you are in /usr/lib/ )...
ln -s libfmod-3.5.so libfmod.so.


Source code
-----------

The source code for Delphi and Kylix is available. It makes
use of SDL, FMod, OpenGL, and Indy, and runs under Win9x
and Linux. There is no comments in the source files, but the
Object Pascal syntax is self-explanatory. If you want to use
part of it in your own production, don't hesitate to ask...
the TNT-3D engine is particularly independant from the game,
and can be reused easily.
However, I don't provide any support for this, I just don't
have the time to do so.

Contacts
--------

DTT website: http://dtt.spinet.org (french)
Send any comment to nitrogen@nrj.com
Don't send bug reports or ideas for improvement, the projet
is finished and I am not going to change anything.

Copyright
---------

Most of the graphics are taken from other games (Unreal Tournament
and Hexen II). The truck was designed by Johannes Rajala of Icarus
Productions (http://icarus.ticalc.org). The TNT-3D Engine is
copyright (C)2000-2001 Clem Vasseur of Icarus Productions.
Delphi/Kylix: www.borland.com
SDL: www.libsdl.org
FMod: www.fmod.org
OpenGL: www.opengl.org

Have fun!