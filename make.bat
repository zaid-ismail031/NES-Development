@echo off
mkdir build
pushd build
ca65 ..\source\hellomario.asm -o hellomario.o --debug-info
ld65 hellomario.o -o hellomario.nes -t nes --dbgfile hellomario.dbg
popd
