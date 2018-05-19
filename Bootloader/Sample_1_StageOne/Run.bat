nasm -f bin Boot1.asm -o Boot1.bin
qemu-system-x86_64 -fda boot1.bin